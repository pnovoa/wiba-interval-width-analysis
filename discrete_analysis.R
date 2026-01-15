# ============================================================
# === DISCRETE PERFORMANCE LANDSCAPE (WIBA)
# === Generic version for arbitrary n_criteria
# ============================================================

# --- LIBRERÍAS ---
library(ggplot2)
library(dplyr)
library(ggthemes)
library(latex2exp)
library(ggrepel)

# ------------------------------------------------------------
# --- PARÁMETROS ---
# ------------------------------------------------------------

n_criteria <- 5      # <-- CAMBIAR AQUÍ
emin <- 1
emax <- 5

# --- UMBRALES (idénticos al caso continuo) ---
middle_width <- (n_criteria - 1) / n_criteria * (emax - emin) / 2
middle_s_middle <- (emin + emax) / 2

# ------------------------------------------------------------
# --- FUNCIONES AUXILIARES ---
# ------------------------------------------------------------

prefix_averages <- function(e) {
  cumsum(e) / seq_along(e)
}

compute_bounds <- function(e) {
  A <- prefix_averages(e)
  s_minus <- min(A)
  s_plus  <- max(A)
  s_middle <- (s_plus + s_minus) / 2
  amplitude <- s_plus - s_minus
  c(s_minus, s_plus, s_middle, amplitude)
}

# ------------------------------------------------------------
# --- GENERACIÓN DEL ESPACIO DISCRETO (GENÉRICO) ---
# ------------------------------------------------------------

levels_discrete <- emin:emax

# Lista de vectores (uno por criterio)
grid_list <- replicate(n_criteria, levels_discrete, simplify = FALSE)

# Nombres e1, e2, ..., en
names(grid_list) <- paste0("e", seq_len(n_criteria))

# Expand grid genérico
E_discrete <- expand.grid(grid_list)

n_discrete <- nrow(E_discrete)
message("Number of discrete configurations: ", n_discrete)

# ------------------------------------------------------------
# --- CÁLCULO DE ESTADÍSTICAS WIBA ---
# ------------------------------------------------------------

results_discrete <- t(apply(E_discrete, 1, compute_bounds))
colnames(results_discrete) <- c("s_minus", "s_plus", "s_middle", "amplitude")

df_discrete <- as.data.frame(results_discrete) %>%
  mutate(
    Alternative = paste0("D", seq_len(n())),
    Type = "Discrete grid"
  )

# ------------------------------------------------------------
# --- CLASIFICACIÓN EN CUADRANTES ---
# ------------------------------------------------------------

df_discrete <- df_discrete %>%
  mutate(
    Region =
      case_when(
        amplitude < middle_width & !near(amplitude, middle_width) &
          s_middle < middle_s_middle ~ "Certain weak",
        
        amplitude > middle_width & !near(amplitude, middle_width) &
          s_middle < middle_s_middle ~ "Uncertain weak",
        
        amplitude < middle_width & !near(amplitude, middle_width) &
          s_middle > middle_s_middle ~ "Certain strong",
        
        amplitude > middle_width & !near(amplitude, middle_width) &
          s_middle > middle_s_middle ~ "Uncertain strong",
        
        TRUE ~ "Boundary"
      )
  )

# ------------------------------------------------------------
# --- PUNTOS A ETIQUETAR (EXTREMOS Y FRONTERA) ---
# ------------------------------------------------------------

min_width <- min(df_discrete$amplitude)
max_width <- max(df_discrete$amplitude)

df_labels <- df_discrete %>%
  filter(
    amplitude == min_width |
      amplitude == max_width |
      is.na(Region)
  )

# ------------------------------------------------------------
# --- ESTADÍSTICAS DESCRIPTIVAS ---
# ------------------------------------------------------------

stats_discrete <- df_discrete %>%
  summarise(
    n = n(),
    min_width = min(amplitude),
    max_width = max(amplitude),
    mean_width = mean(amplitude),
    min_middle = min(s_middle),
    max_middle = max(s_middle)
  )

print(stats_discrete)

# ------------------------------------------------------------
# --- VISUALIZACIÓN: DISCRETE PERFORMANCE LANDSCAPE ---
# ------------------------------------------------------------

# ------------------------------------------------------------
# --- FUNCIÓN DE LANDSCAPE DISCRETO ---
# ------------------------------------------------------------
plot_discrete_landscape <- function(
    df,
    x_var = c("s_middle", "s_minus", "s_plus"),
    title_prefix = "Discrete performance landscape",
    show_labels = FALSE
) {
  x_var <- match.arg(x_var)
  
  # --- Valores de referencia ---
  middle_x <- (emin + emax) / 2
  middle_width <- (n_criteria - 1) / n_criteria * (emax - emin) / 2
  
  x_min <- min(df[[x_var]])
  x_max <- max(df[[x_var]])
  y_min <- min(df$amplitude)
  y_max <- max(df$amplitude)
  
  # --- Etiquetas de eje ---
  x_label <- switch(
    x_var,
    s_middle = TeX(r"( Middle score $(\bar{s})$ )"),
    s_minus  = TeX(r"( Conservative score $(s^-)$ )"),
    s_plus   = TeX(r"( Optimistic score $(s^+)$ )")
  )
  
  plot_title <- paste0(
    title_prefix, " (", 
    ifelse(x_var == "s_middle", "neutral",
           ifelse(x_var == "s_minus", "conservative", "optimistic")),
    " view)"
  )
  
  # --- Clasificación dinámica ---
  df <- df %>%
    mutate(
      Region_dyn = case_when(
        amplitude < middle_width & !near(amplitude, middle_width) &
          .data[[x_var]] < middle_x ~ "Certain weak",
        
        amplitude > middle_width & !near(amplitude, middle_width) &
          .data[[x_var]] < middle_x ~ "Uncertain weak",
        
        amplitude < middle_width & !near(amplitude, middle_width) &
          .data[[x_var]] > middle_x ~ "Certain strong",
        
        amplitude > middle_width & !near(amplitude, middle_width) &
          .data[[x_var]] > middle_x ~ "Uncertain strong",
        
        TRUE ~ "Boundary"
      )
    )
  
  # --- Identificar extremos para etiquetas ---
  df_labels <- df %>%
    filter(amplitude == min(amplitude) | amplitude == max(amplitude) | Region_dyn == "Boundary")
  
  # --- Gráfico ---
  p <- ggplot(df, aes(x = .data[[x_var]], y = amplitude)) +
    geom_point(aes(color = Region_dyn), size = 1.8, alpha = 0.75)
  
  if (show_labels) {
    p <- p +
    geom_text_repel(
      data = df_labels,
      aes(label = Alternative),
      size = 2.6, color = "black",
      box.padding = 0.35, point.padding = 0.25,
      max.overlaps = Inf,
      segment.color = "grey40",
      segment.size = 0.3,
      show.legend = FALSE
    )}
  
  p <- p +
    geom_vline(xintercept = middle_x, linetype = "dashed", linewidth = 0.4) +
    geom_hline(yintercept = middle_width, linetype = "dashed", linewidth = 0.4) +
    
    theme_clean(base_size = 13) +
    labs(
      title = plot_title,
      x = x_label,
      y = TeX(r"( Interval width $(\bf{\Alpha})$ )"),
      color = "Region"
    ) +
    theme(plot.title = element_text(hjust = 0.5))
}

# ------------------------------------------------------------
# --- GENERAR LOS TRES LANDSCAPES ---
# ------------------------------------------------------------
p_middle <- plot_discrete_landscape(df_discrete, "s_middle")
p_minus  <- plot_discrete_landscape(df_discrete, "s_minus")
p_plus   <- plot_discrete_landscape(df_discrete, "s_plus")

library(patchwork)

p_all <- p_middle / p_minus / p_plus +
  plot_annotation(
    title = "Discrete performance landscapes (different perspectives)",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 14))
  )

print(p_all)

ggsave(
  filename = paste0("discrete_performance_landscapes_all_n", n_criteria, ".pdf"),
  plot = p_all,
  width = 8, height = 17
)

# ------------------------------------------------------------
# --- AGREGACIÓN POR PUNTO (BURBUJAS) ---
# ------------------------------------------------------------

df_bubbles <- df_discrete %>%
  group_by(s_middle, amplitude, Region) %>%
  summarise(Count = n(), .groups = "drop")

# ------------------------------------------------------------
# --- BUBBLE PERFORMANCE LANDSCAPE ---
# ------------------------------------------------------------

# ------------------------------------------------------------
# --- FUNCIÓN DE LANDSCAPE BUBBLE DISCRETO ---
# ------------------------------------------------------------
plot_discrete_landscape_bubble <- function(
    df,
    x_var = c("s_middle", "s_minus", "s_plus"),
    title_prefix = "Discrete performance landscape (bubble)"
) {
  x_var <- match.arg(x_var)
  
  # --- Valores de referencia ---
  middle_x <- (emin + emax) / 2
  middle_width <- (n_criteria - 1) / n_criteria * (emax - emin) / 2
  
  # --- Clasificación dinámica ---
  df <- df %>%
    mutate(
      Region_dyn = case_when(
        amplitude < middle_width & !near(amplitude, middle_width) &
          .data[[x_var]] < middle_x ~ "Certain weak",
        
        amplitude > middle_width & !near(amplitude, middle_width) &
          .data[[x_var]] < middle_x ~ "Uncertain weak",
        
        amplitude < middle_width & !near(amplitude, middle_width) &
          .data[[x_var]] > middle_x ~ "Certain strong",
        
        amplitude > middle_width & !near(amplitude, middle_width) &
          .data[[x_var]] > middle_x ~ "Uncertain strong",
        
        TRUE ~ "Boundary"
      )
    )
  
  # --- Agregar por punto para el tamaño de burbuja ---
  df_bubble <- df %>%
    group_by(.data[[x_var]], amplitude, Region_dyn) %>%
    summarise(Count = n(), .groups = "drop")
  
  # --- Etiquetas eje ---
  x_label <- switch(
    x_var,
    s_middle = TeX(r"( Middle score $(\bar{s})$ )"),
    s_minus  = TeX(r"( Conservative score $(s^-)$ )"),
    s_plus   = TeX(r"( Optimistic score $(s^+)$ )")
  )
  
  plot_title <- paste0(
    title_prefix, " (", 
    ifelse(x_var == "s_middle", "neutral",
           ifelse(x_var == "s_minus", "conservative", "optimistic")),
    " view)"
  )
  
  # --- Gráfico bubble ---
  ggplot(df_bubble, aes(x = .data[[x_var]], y = amplitude)) +
    geom_point(
      aes(size = Count, fill = Region_dyn),
      shape = 21,
      color = "black",
      alpha = 0.7,
      stroke = 0.3
    ) +
    geom_text(
      aes(label = Count),
      size = 3,
      fontface = "bold"
    ) +
    geom_vline(xintercept = middle_x, linetype = "dashed", linewidth = 0.4) +
    geom_hline(yintercept = middle_width, linetype = "dashed", linewidth = 0.4) +
    scale_size_continuous(range = c(4, 20), guide = "none") +
    theme_clean(base_size = 13) +
    labs(
      title = plot_title,
      x = x_label,
      y = TeX(r"( Interval width $(\bf{\Alpha})$ )"),
      fill = "Region"
    ) +
    theme(plot.title = element_text(hjust = 0.5))
}

# ------------------------------------------------------------
# --- GENERAR LOS TRES LANDSCAPES BUBBLE ---
# ------------------------------------------------------------
p_middle_bubble <- plot_discrete_landscape_bubble(df_discrete, "s_middle")
p_minus_bubble  <- plot_discrete_landscape_bubble(df_discrete, "s_minus")
p_plus_bubble   <- plot_discrete_landscape_bubble(df_discrete, "s_plus")

library(patchwork)

p_all_bubble <- p_middle_bubble / p_minus_bubble / p_plus_bubble +
  plot_annotation(
    title = "Discrete performance landscapes (bubble) from different perspectives",
    theme = theme(plot.title = element_text(hjust = 0.5, size=14))
  )

print(p_all_bubble)

ggsave(
  filename = paste0("discrete_performance_landscapes_bubble_all_n", n_criteria, ".pdf"),
  plot = p_all_bubble,
  width = 8,
  height = 17
) 
# ============================================================
# === END OF SCRIPT
# ============================================================