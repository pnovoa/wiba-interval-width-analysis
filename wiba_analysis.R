# --- Librerías ---
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(latex2exp)
library(xtable)

# --- Parámetros ---
n_alternatives <- 7
n_criteria <- 5
emin <- 1
emax <- 5

# --- Matriz de desempeño (casos teóricos + intermedios) ---
E <- matrix(c(
  1, 1, 1, 1, 1,   # constante baja → min amplitud
  3, 3, 3, 3, 3,
  5, 5, 5, 5, 5,   # constante alta → min amplitud
  1, 2, 3, 4, 5,   # creciente
  5, 4, 3, 2, 1,   # decreciente
  1, 5, 5, 5, 5,   # máx amplitud (-,+)
  5, 1, 1, 1, 1   # máx amplitud (+,-)
 
), nrow = n_alternatives, byrow = TRUE)

rownames(E) <- c(
  "Min. Width - Low Constant",
  "Min. Width - Middle Constant",
  "Min. Width - High Constant",
  "Criteria-reversed",
  "Criteria-aligned",
  "Max. Width (-,+)",
  "Max. Width (+,-)"
  )

# --- Función para promedios prefijos ---
prefix_averages <- function(e) {
  cumsum(e) / seq_along(e)
}

# --- Calcular s-, s+, amplitud ---
s_minus <- s_plus <- s_middle <- amplitude <- ejs <- numeric(n_alternatives)

for (i in 1:n_alternatives) {
  A <- prefix_averages(E[i, ])
  minA = min(A)
  maxA = max(A)
  s_minus[i] <- minA
  s_plus[i] <- maxA
  amplitude[i] <- maxA - minA
  s_middle[i] <- (maxA + minA)/2
  ejs[i] <- paste0("(", paste0(E[i,], collapse = ", "), ")")
}

alternatives_labels <- paste0("A", seq(1,n_alternatives))

df_case <- data.frame(
  Alternative = rownames(E),
  ShortName = alternatives_labels,
  Type = "Defined cases",
  s_minus = s_minus,
  s_plus = s_plus,
  s_middle = s_middle,
  amplitude = amplitude,
  ei = ejs
) %>% mutate(ShortName = factor(ShortName, levels = rev(alternatives_labels))) 

df_int_plot <- df_case %>%
  mutate(
    Description = Alternative
  ) %>%
  select(-c("Type", "Alternative")) %>%
  rename(
    "Alternative" = ShortName,
    "Min. s" = s_minus,
    "Max. s" = s_plus,
    "Mid. s" = s_middle,
    "Int. Width" = amplitude
  ) %>% select(
    Alternative,
    Description,
    ei,
    `Min. s`,
    `Mid. s`,
    `Max. s`,
    `Int. Width`
  )

df_points <- df_int_plot %>%
  tidyr::pivot_longer(
    cols = c(`Min. s`, `Mid. s`, `Max. s`),
    names_to = "PointType",
    values_to = "Score"
  ) %>%
  mutate(
    PointType = factor(PointType, levels=c("Min. s", "Mid. s", "Max. s"))
  )


# --- Gráfico 1: Intervalos horizontales (tipo WIBA) ---
p1 <- ggplot(df_int_plot) +
  geom_segment(aes(y = Alternative, yend = Alternative,
                   x = `Min. s`, xend = `Max. s`),
               color = "#648FFF", linewidth = 4, alpha=0.3) +
  geom_segment(aes(y = Alternative, yend = Alternative,
                   x = `Min. s`, xend = `Max. s`),
               color = "#648FFF", linewidth = 0.5) +
  # Puntos con leyenda
  geom_point(
    data = df_points,
    aes(x = Score, y = Alternative, shape = PointType),
    size = 3,
    color = "black",
    stroke = 1,
    show.legend = TRUE
  ) +
  
  # --- Escalas y leyenda ---
  scale_shape_manual(
    name = "Interval Points",
    values = c("Min. s" = 1, "Mid. s" = 3, "Max. s" = 4),
    labels = c(TeX(r"(Lower bound ($s^-$))"), 
               TeX(r"(Middle score ($\bar{s}$))"),
               TeX(r"(Upper bound ($s^+$))")
               )
  ) +
  
  # --- Estilo general ---
  ggthemes::theme_clean(base_size = 13) +
  labs(
    title = "a) Score intervals per alternative",
    x = "Score",
    y = "Alternative"
  ) +
  theme(
    plot.background = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 13),
    legend.text = element_text(size = 11),
    legend.position = "top",  # puedes usar "right", "bottom", etc.
    legend.title = element_text(face = "bold", size=11),
    legend.background = element_rect(linewidth = 0.5)
  )


print(p1)

print(xtable(df_int_plot), include.rownames = FALSE)

ggsave(filename = "intervals.pdf", plot=p1, width = 7, height = 4.5)


middle_width = mean(range(df_case$amplitude))
middle_s_middle = mean(range(df_case$s_middle))


# --- DEPENDENCIAS ---
# Instala si es necesario
library(lhs)
library(randtoolbox)

# --- PARÁMETROS ---
n_samples <- 50000

# --- MUESTREO SOBOL / LHS ---
set.seed(123)

sampler_used <- "LHS"

# Try Sobol first, fallback to LHS
if(sampler_used == "Sobol"){
  sobol_points <- sobol(n = n_samples, dim = n_criteria, seed = 123, normal = FALSE)
  samples <- sobol_points
} else{
  samples <- randomLHS(n_samples, n_criteria)
  sampler_used <- "LHS"
}

# Map to [emin, emax]
E <- emin + (emax - emin) * samples

# --- FUNCIONES AUXILIARES ---
prefix_averages <- function(e) {
  cumsum(e) / seq_along(e)
}

compute_bounds <- function(e) {
  A <- prefix_averages(e)
  s_minus <- min(A)
  s_plus  <- max(A)
  s_middle <- (s_plus + s_minus)/2
  amplitude <- s_plus - s_minus
  return(c(s_minus, s_plus, s_middle, amplitude))
}

# --- CÁLCULO DE S- Y AMPLITUD ---
results <- t(apply(E, 1, compute_bounds))
colnames(results) <- c("s_minus", "s_plus", "s_middle", "amplitude")
df <- as.data.frame(results)
df$Alternative <- paste0("S", seq(1, n_samples))

# --- PREPARACIÓN DE DATOS ---
df_plot <- df %>% mutate(
    Type = "Sampled cases"
  ) %>%
  mutate(ShortName = Alternative) %>%
  bind_rows(df_case %>% mutate(Type = "Defined cases"))

# --- GRÁFICO 2: NUBE DE PUNTOS ---
plot_performance_landscape <- function(
    df_plot,
    x_var = c("s_middle", "s_minus", "s_plus"),
    title_prefix = "Performance landscape"
) {
  
  x_var <- match.arg(x_var)
  
  # --- Referencias ---
  middle_x <- mean(range(df_plot[[x_var]]))
  middle_width <- mean(range(df_plot$amplitude))
  
  x_min <- min(df_plot[[x_var]])
  x_max <- max(df_plot[[x_var]])
  y_min <- min(df_plot$amplitude)
  y_max <- max(df_plot$amplitude)
  
  # --- Etiquetas ---
  x_label <- switch(
    x_var,
    "s_middle" = TeX(r"( Middle score $(\bar{s})$ )"),
    "s_minus"  = TeX(r"( Conservative score $(s^-)$ )"),
    "s_plus"   = TeX(r"( Optimistic score $(s^+)$ )")
  )
  
  plot_title <- paste0(
    title_prefix, " (",
    ifelse(x_var == "s_middle", "neutral",
           ifelse(x_var == "s_minus", "conservative", "optimistic")),
    " view)"
  )
  
  # --- Clasificación (Defined cases primero) ---
  df_plot2 <- df_plot %>%
    mutate(
      Type_dyn = case_when(
        Type == "Defined cases" ~ "Defined cases",
        amplitude < middle_width & .data[[x_var]] < middle_x ~ "Certain weak",
        amplitude > middle_width & .data[[x_var]] < middle_x ~ "Uncertain weak",
        amplitude < middle_width & .data[[x_var]] > middle_x ~ "Certain strong",
        amplitude > middle_width & .data[[x_var]] > middle_x ~ "Uncertain strong"
      )
    )
  
  # --- Gráfico ---
  ggplot(df_plot2, aes(x = .data[[x_var]], y = amplitude)) +
    
    geom_point(
      data = dplyr::filter(df_plot2, Type_dyn == "Uncertain weak"),
      size = 2, alpha = 0.2, color = "#648FFF"
    ) +
    geom_point(
      data = dplyr::filter(df_plot2, Type_dyn == "Certain weak"),
      size = 2, alpha = 0.2, color = "#785EF0"
    ) +
    geom_point(
      data = dplyr::filter(df_plot2, Type_dyn == "Certain strong"),
      size = 2, alpha = 0.2, color = "#DC267F"
    ) +
    geom_point(
      data = dplyr::filter(df_plot2, Type_dyn == "Uncertain strong"),
      size = 2, alpha = 0.2, color = "#FE6100"
    ) +
    
    # Líneas divisorias
    geom_vline(xintercept = middle_x, linetype = "dashed", linewidth = 0.5) +
    geom_hline(yintercept = middle_width, linetype = "dashed", linewidth = 0.5) +
    
    # Cuadrantes
    annotate("text", x = x_min + 0.1*(x_max-x_min), y = y_min + 0.1*(y_max-y_min),
             label = "Certain Weak", fontface = "bold") +
    annotate("text", x = x_max - 0.1*(x_max-x_min), y = y_min + 0.1*(y_max-y_min),
             label = "Certain Strong", fontface = "bold") +
    annotate("text", x = x_min + 0.1*(x_max-x_min), y = y_max - 0.1*(y_max-y_min),
             label = "Uncertain Weak", fontface = "bold") +
    annotate("text", x = x_max - 0.1*(x_max-x_min), y = y_max - 0.1*(y_max-y_min),
             label = "Uncertain Strong", fontface = "bold") +
    
    # --- Casos definidos (siempre visibles) ---
    geom_point(
      data = dplyr::filter(df_plot2, Type_dyn == "Defined cases"),
      shape = 21, size = 7,
      fill = "#FFE36C", color = "black", stroke = 0.6
    ) +
    geom_text(
      data = dplyr::filter(df_plot2, Type_dyn == "Defined cases"),
      aes(label = ShortName),
      fontface = "bold", size = 3
    ) +
    
    theme_clean(base_size = 13) +
    labs(
      title = plot_title,
      x = x_label,
      y = TeX(r"( Interval width $(\bf{\Delta I})$ )")
    ) +
    theme(
      plot.title = element_text(hjust = 0.5)
    )
}

p_middle <- plot_performance_landscape(df_plot, "s_middle")
p_minus  <- plot_performance_landscape(df_plot, "s_minus")
p_plus   <- plot_performance_landscape(df_plot, "s_plus")

library(patchwork)

p_all <- p1 / p_middle

ggsave(filename = "all.pdf", plot=p_all, width = 7, height = 8)


p_performances <- p_middle / p_minus / p_plus + 
  plot_layout(nrow = 3) +
  plot_annotation(
    title = "Performance landscapes from different perspectives",
    theme = theme(plot.title = element_text(hjust = 0.5, size=14))
  )

ggsave(filename = "performance_landscapes.pdf", plot=p_performances, width = 8, height = 14)