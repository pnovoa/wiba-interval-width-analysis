# --- Librerías ---
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)

# --- Parámetros ---
n_alternatives <- 7
n_criteria <- 5
emin <- 1
emax <- 5

# --- Matriz de desempeño (casos teóricos + intermedios) ---
E <- matrix(c(
  1, 1, 1, 1, 1,   # constante baja → min amplitud
  5, 5, 5, 5, 5,   # constante alta → min amplitud
  1, 2, 3, 4, 5,   # creciente
  5, 4, 3, 2, 1,   # decreciente
  1, 5, 5, 5, 5,   # máx amplitud (-,+)
  5, 1, 1, 1, 1,   # máx amplitud (+,-)
  3, 5, 5, 5, 5
), nrow = n_alternatives, byrow = TRUE)

rownames(E) <- c(
  "A1\nMin. Width - Low Constant\ne=(1,1,1,1,1)",
  "A2\nMin. Width - High Constant\ne=(5,5,5,5,5)",
  "A3\nCriteria-reversed\ne=(1,2,3,4,5)",
  "A4\nCriteria-aligned\ne=(5,4,3,2,1)",
  "A5 \nMax. Width (-,+)\ne=(1,5,5,5,5)",
  "A6\nMax. Width (+,-)\ne=(5,1,1,1,1)",
  "A7\nIntersection point (+,-)\ne=(3,5,5,5,5)"
  )

# --- Función para promedios prefijos ---
prefix_averages <- function(e) {
  cumsum(e) / seq_along(e)
}

# --- Calcular s-, s+, amplitud ---
s_minus <- s_plus <- amplitude <- numeric(n_alternatives)

for (i in 1:n_alternatives) {
  A <- prefix_averages(E[i, ])
  s_minus[i] <- min(A)
  s_plus[i] <- max(A)
  amplitude[i] <- max(A) - min(A)
}

df_case <- data.frame(
  Alternative = rownames(E),
  ShortName = paste0("A", seq(1,n_alternatives)),
  Type = "Defined cases",
  s_minus = s_minus,
  s_plus = s_plus,
  amplitude = amplitude
) %>% mutate(Alternative = factor(Alternative, levels = rev(rownames(E))))

# --- Gráfico 1: Intervalos horizontales (tipo WIBA) ---
p1 <- ggplot(df_case) +
  geom_segment(aes(y = Alternative, yend = Alternative,
                   x = s_minus, xend = s_plus),
               color = "black", linewidth = 0.5) +
  geom_point(aes(x = s_minus, y = Alternative), size = 2) +
  geom_point(aes(x = s_plus, y = Alternative), size = 2) +
  ggthemes::theme_clean(base_size = 13)+
  labs(
    title = "Score Intervals per Alternative",
    x = "Score",
    y = "Alternative"
  ) 


# --- Mostrar resultados y gráficos ---
print(df_case)
print(p1)


middle_width = mean(range(df_case$amplitude))
middle_s_min = mean(range(df_case$s_minus))


# --- DEPENDENCIAS ---
# Instala si es necesario
library(lhs)
library(randtoolbox)

# --- PARÁMETROS ---
n_samples <- 50000
n_criteria <- 5
emin <- 1
emax <- 5

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
  amplitude <- s_plus - s_minus
  return(c(s_minus, s_plus, amplitude))
}

# --- CÁLCULO DE S- Y AMPLITUD ---
results <- t(apply(E, 1, compute_bounds))
colnames(results) <- c("s_minus", "s_plus", "amplitude")
df <- as.data.frame(results)
df$Alternative <- paste0("S", seq(1, n_samples))

# --- PREPARACIÓN DE DATOS ---
df_plot <- df %>%
  mutate( Type = 
    case_when(
      amplitude < middle_width & s_minus < middle_s_min ~ "Certain weak",
      amplitude > middle_width & s_minus < middle_s_min ~ "Uncertain weak",
      amplitude < middle_width & s_minus > middle_s_min ~ "Certain strong",
      amplitude > middle_width & s_minus > middle_s_min ~ "Uncertain strong"
    )
  ) %>%
  mutate(ShortName = Alternative) %>%
  bind_rows(df_case)


x_min <- min(df_plot$s_minus)
x_max <- max(df_plot$s_minus)
y_min <- min(df_plot$amplitude)
y_max <- max(df_plot$amplitude)

# --- VISUALIZACIÓN ---
p <- ggplot(df_plot, aes(x = s_minus, y = amplitude)) +
  
 
  geom_point(
    data = subset(df_plot, Type == "Uncertain weak"),
    size = 2,
    alpha = 0.2,
    color = "#648FFF",
    stroke = 0,
    show.legend = TRUE
  ) +
  
  geom_point(
    data = subset(df_plot, Type == "Certain weak"),
    size = 2,
    alpha = 0.2,
    color = "#785EF0",
    stroke = 0,
    show.legend = TRUE
  ) +
  geom_point(
    data = subset(df_plot, Type == "Certain strong"),
    size = 2,
    alpha = 0.2,
    color = "#DC267F",
    stroke = 0,
    show.legend = TRUE
  ) +
  geom_point(
    data = subset(df_plot, Type == "Uncertain strong"),
    size = 2,
    alpha = 0.2,
    color = "#FE6100",
    stroke = 0,
    show.legend = TRUE
  ) +
  
  
  
  # --- LÍNEAS DIVISORIAS DE LOS CUADRANTES ---
  geom_vline(xintercept = middle_s_min, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = middle_width, linetype = "dashed", color = "black", linewidth = 0.5) +
  
  # --- ETIQUETAS DE CUADRANTES (geom_label con fondo blanco) ---
  annotate("text", x = x_min + 1, y = y_min + 0.8,
           label = "Certain Weak", color = "black", size = 4, fontface = "bold") +
  annotate("text", x = x_max - 1, y = y_min + 0.8,
           label = "Certain Strong", color = "black", size = 4, fontface = "bold") +
  annotate("text", x = x_min + 1, y = y_max - 0.8,
           label = "Uncertain Weak", color = "black", size = 4, fontface = "bold") +
  annotate("text", x = x_max - 1, y = y_max - 0.8,
           label = "Uncertain Strong", color = "black", size = 4, fontface = "bold") +
  
  
  # puntos destacados (casos definidos)
  geom_point(
    data = subset(df_plot, Type == "Defined cases"),
    shape = 21,
    size = 6,
    fill = "white",
    color = "black",
    stroke = 0.6,
    show.legend = FALSE
  ) +
  
  # etiquetas de los casos teóricos
  geom_text(
    data = subset(df_plot, Type == "Defined cases"),
    aes(label = ShortName),
    vjust = 0.5,
    size = 3,
    color = "black",
    fontface = "bold"
  ) +
  
  # estética general
  theme_clean(base_size = 13) +
  labs(
    title = "Performance Landscape",
    x = "Lower bound (s-)",
    y = "Interval Width (A)"
  ) +
  coord_cartesian(expand = TRUE)

print(p)
