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
        
        TRUE ~ NA_character_
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

p_discrete <- ggplot(df_discrete, aes(x = s_middle, y = amplitude)) +
  geom_point(aes(color = Region), size = 1.8, alpha = 0.75) +
  
  geom_text_repel(
    data = df_labels,
    aes(label = Alternative),
    size = 2.6,
    color = "black",
    box.padding = 0.35,
    point.padding = 0.25,
    max.overlaps = Inf,
    segment.color = "grey40",
    segment.size = 0.3,
    show.legend = FALSE
  ) +
  
  geom_vline(xintercept = middle_s_middle, linetype = "dashed", linewidth = 0.4) +
  geom_hline(yintercept = middle_width, linetype = "dashed", linewidth = 0.4) +
  
  theme_clean(base_size = 13) +
  labs(
    title = paste("Discrete performance landscape (n =", n_criteria, ")"),
    x = TeX(r"( Middle score $(\bar{s})$ )"),
    y = TeX(r"( Interval width $(\bf{\Alpha})$ )"),
    color = "Region"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

print(p_discrete)

# ------------------------------------------------------------
# --- AGREGACIÓN POR PUNTO (BURBUJAS) ---
# ------------------------------------------------------------

df_bubbles <- df_discrete %>%
  group_by(s_middle, amplitude, Region) %>%
  summarise(Count = n(), .groups = "drop")

# ------------------------------------------------------------
# --- BUBBLE PERFORMANCE LANDSCAPE ---
# ------------------------------------------------------------

p_bubble <- ggplot(df_bubbles, aes(x = s_middle, y = amplitude)) +
  geom_point(
    aes(size = Count, fill = Region),
    shape = 21,
    color = "black",
    alpha = 0.7,
    stroke = 0.3
  ) +
  
  # --- CANTIDAD EN EL CENTRO DE LA BURBUJA ---
  geom_text(
    aes(label = Count),
    size = 3,
    fontface = "bold"
  ) +
  
  # --- LÍNEAS DE REFERENCIA ---
  geom_vline(
    xintercept = middle_s_middle,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  geom_hline(
    yintercept = middle_width,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  
  # --- OCULTAR LEYENDA DE TAMAÑO ---
  scale_size_continuous(
    range = c(4, 20),
    guide = "none"
  ) + 
  
  theme_clean(base_size = 13) +
  labs(
    title = paste("Discrete performance landscape (bubble, n =", n_criteria, ")"),
    x = TeX(r"( Middle score $(\bar{s})$ )"),
    y = TeX(r"( Interval width $(\bf{\Alpha})$ )"),
    fill = "Region"
  ) + 
  theme(
    plot.title = element_text(hjust = 0.5)
  )

print(p_bubble)

# ============================================================
# === END OF SCRIPT
# ============================================================