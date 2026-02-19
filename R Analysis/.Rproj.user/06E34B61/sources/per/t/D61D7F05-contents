# ================================
# Figure 1 — Threat compositions + Motivations by bioregion
# Outputs:
#   A) stacked_no_x_no_legend        (5 panels, no x-axis, no legend)
#   B) stacked_global_with_axis      (4 panels; only global has x-axis + legend)
#   C) stacked_local_with_axis       (4 panels; only local has x-axis + legend)
#   D) stacked_with_motivation_legend(5 panels; no x-axis; legend from motivations)
#   E) stacked_with_motivation_counts(6 panels; includes counts panel aligned)
# ================================
library(readxl)
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)
library(ggplot2)
library(scales)
library(cowplot)

# ---------- File paths ----------
file_threats <- "C:/Users/Kris Jypson Esturas/OneDrive - Macquarie University/Documents/2025/00 CRR Gov/ArcGIS Projects/CRR 2025_June/Bioregions_Threats.xlsx"
file_mot     <- "C:/Users/Kris Jypson Esturas/OneDrive - Macquarie University/Documents/2025/00 CRR Gov/R Analysis/Data/[Original] 20250813 CRR Governance Database 2023-2025.xlsx"

# ---------- Palette & orders ----------
threat_colors <- c(
  "Low"        = "#FFFFB2",
  "Medium"     = "#FECC5C",
  "High"       = "#FD8D3C",
  "Very High"  = "#E31A1C",
  "Critical"   = "#800026"
)
threat_levels_all <- names(threat_colors)

bioregion_order <- c(
  "North Philippine Sea Bioregion",
  "West Philippine Sea Bioregion",
  "Visayan Sea Bioregion",
  "South Philippine Sea Bioregion",
  "Sulu Sea Bioregion",
  "Celebes Sea Bioregion"
)

motivation_order <- c(
  "Climate change mitigation",
  "Coral reef rehabilitation",
  "Increase biodiversity",
  "Tourism and education",
  "Post-disturbance recovery",
  "Coastal resource management",
  "Fisheries-oriented",
  "Mixed objectives",
  "Research and innovation",
  "Unknown"
)

mot_palette <- c(
  "Fisheries-oriented"          = "#1f78b4",
  "Increase biodiversity"       = "#33a02c",
  "Coral reef rehabilitation"   = "#b2df8a",
  "Tourism and education"       = "#1b9e77",
  "Research and innovation"     = "#6a3d9a",
  "Coastal resource management" = "#ff7f00",
  "Climate change mitigation"   = "#e31a1c",
  "Post-disturbance recovery"   = "#a6cee3",
  "Mixed objectives"            = "#fb9a99",
  "Unknown"                     = "#bdbdbd"
)

# ---------- Helper: prep threats ----------
prep_prop <- function(df, threat_levels) {
  df %>%
    mutate(Threat = factor(.data$THREAT_TXT, levels = threat_levels, ordered = TRUE)) %>%
    group_by(Bioregion, Threat) %>%
    summarise(area = sum(Shape_Area, na.rm = TRUE), .groups = "drop") %>%
    complete(Bioregion, Threat, fill = list(area = 0)) %>%
    group_by(Bioregion) %>%
    mutate(total = sum(area), prop = ifelse(total > 0, area / total, 0)) %>%
    ungroup() %>%
    select(-total) %>%
    mutate(Bioregion = factor(Bioregion, levels = rev(bioregion_order)))
}

# ---------- Generic THREAT panel ----------
make_panel <- function(prop_area,
                       show_x = TRUE,
                       legend = "none",
                       legend_levels = threat_levels_all) {
  base <- ggplot(prop_area, aes(x = prop, y = Bioregion, fill = Threat)) +
    geom_col(width = 0.85, color = "grey20", linewidth = 0.475) +
    scale_x_continuous(labels = percent_format(), breaks = seq(0, 1, 0.25)) +
    scale_y_discrete(position = "right") +
    scale_fill_manual(values = threat_colors, limits = legend_levels, drop = FALSE) +
    labs(x = NULL, y = NULL, fill = "Threat level") +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey60", linewidth = 0.4),
      panel.grid.minor.x = element_blank(),
      axis.text  = element_text(color = "black", size = 11),
      legend.position = legend,
      legend.title    = element_text(face = "bold", size = 11),
      legend.text     = element_text(size = 11),
      plot.margin     = margin(5, 10, 5, 10)
    )
  if (!show_x) {
    base <- base + theme(axis.text.x = element_blank(),
                         axis.ticks.x = element_blank(),
                         axis.title.x = element_blank())
  }
  base
}

# ---------- MOTIVATION proportion panel ----------
make_motivation_panel <- function(show_x = FALSE, legend = "none") {
  base <- ggplot(prop_mot, aes(x = prop, y = Bioregion, fill = Primary_motivation)) +
    geom_col(width = 0.85, color = "grey20", linewidth = 0.475) +
    scale_x_continuous(
      limits = c(0, 1),                     # <- hard limit at 100%
      breaks  = seq(0, 1, 0.25),
      labels  = percent_format(),
      expand  = expansion(mult = c(0, 0))   # <- no padding; gridline hits panel edge
    ) +
    scale_y_discrete(position = "right") +
    scale_fill_manual(values = mot_palette, drop = FALSE, name = "Primary motivation") +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE, reverse = TRUE)) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey60", linewidth = 0.4),
      panel.grid.minor.x = element_blank(),
      axis.text  = element_text(color = "black", size = 11),
      legend.position = legend,
      legend.title    = element_text(face = "bold", size = 11),
      legend.text     = element_text(size = 11),
      plot.margin     = margin(5, 10, 5, 10)
    )
  
  if (!show_x) {
    base <- base + theme(axis.text.x  = element_blank(),
                         axis.ticks.x = element_blank(),
                         axis.title.x = element_blank())
  }
  base
}

# ---------- MOTIVATION counts panel (NEW for E) ----------
make_motivation_counts_panel <- function(show_x = FALSE, legend = "none") {
  totals <- prop_mot %>% group_by(Bioregion) %>% summarise(N = sum(n), .groups = "drop")
  max_total <- max(totals$N, na.rm = TRUE)
  
  base <- ggplot(prop_mot, aes(x = n, y = Bioregion, fill = Primary_motivation)) +
    geom_col(width = 0.85, color = "grey20", linewidth = 0.475) +
    scale_y_discrete(position = "right") +
    scale_fill_manual(values = mot_palette, drop = FALSE, name = "Primary motivation") +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE, reverse = TRUE)) +
    # counts panel: right edge at 200, no padding
    scale_x_continuous(
      limits = c(0, 200),
      breaks  = c(0, 50, 100, 150, 200),
      expand  = expansion(mult = c(0, 0))   # <- important
    )+
    labs(x = NULL, y = NULL) +
 #   coord_cartesian(clip = "off") +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey60", linewidth = 0.4),
      panel.grid.minor.x = element_blank(),
      axis.text  = element_text(color = "black", size = 11),
      legend.position = legend,
      legend.title    = element_text(face = "bold", size = 11),
      legend.text     = element_text(size = 11),
      plot.margin     = margin(5, 10, 5, 10)
    )
  
  if (!show_x) {
    base <- base + theme(axis.text.x  = element_blank(),
                         axis.ticks.x = element_blank(),
                         axis.title.x = element_blank())
  }
  base
}

# =========================
# Threats data
# =========================
df_over   <- read_xlsx(file_threats, sheet = "Bioregions_Overfishing")
prop_over <- prep_prop(df_over, c("Low","Medium","High","Very High"))

df_past   <- read_xlsx(file_threats, sheet = "Bioregion_PastThermalStress")
prop_past <- prep_prop(df_past, c("Low","Medium","High","Very High"))

df_int    <- read_xlsx(file_threats, sheet = "Bioregions_IntThreats")
prop_int  <- prep_prop(df_int, c("Low","Medium","High","Very High"))

df_2030   <- read_xlsx(file_threats, sheet = "Bioregions_2030_Int")
prop_2030 <- prep_prop(df_2030, c("Medium","High","Very High","Critical"))

# =========================
# Motivations data
# =========================
data2 <- read_excel(file_mot, sheet = "CRR 2025")

prop_mot <- data2 %>%
  transmute(
    Bioregion = str_squish(as.character(Bioregion)),
    Primary_motivation = str_squish(as.character(Primary_motivation))
  ) %>%
  filter(!is.na(Bioregion), Bioregion != "",
         !is.na(Primary_motivation), Primary_motivation != "") %>%
  count(Bioregion, Primary_motivation, name = "n") %>%
  complete(Bioregion, Primary_motivation, fill = list(n = 0)) %>%
  group_by(Bioregion) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    Bioregion = factor(Bioregion, levels = rev(bioregion_order)),
    Primary_motivation = factor(Primary_motivation, levels = motivation_order)
  )

# =========================
# A) STACKED — with motivations (no x, no legend)
# =========================
p_over_A <- make_panel(prop_over, show_x = FALSE, legend = "none")
p_past_A <- make_panel(prop_past, show_x = FALSE, legend = "none")
p_int_A  <- make_panel(prop_int,  show_x = FALSE, legend = "none")
p_2030_A <- make_panel(prop_2030, show_x = FALSE, legend = "none")
p_mot_A  <- make_motivation_panel(show_x = FALSE, legend = "none")
p_motE_A <- make_motivation_counts_panel(show_x = FALSE, legend = "none")

tight_theme <- theme(plot.margin = margin(1, 10, 1, 10))
p_over_A <- p_over_A + tight_theme
p_past_A <- p_past_A + tight_theme
p_int_A  <- p_int_A  + tight_theme
p_2030_A <- p_2030_A + tight_theme
p_mot_A  <- p_mot_A  + tight_theme
p_motE_A <- p_motE_A + tight_theme

stacked_no_x_no_legend <- cowplot::plot_grid(
  p_over_A, p_past_A, p_int_A, p_2030_A, p_mot_A, p_motE_A,
  ncol = 1, align = "v",
  rel_heights = c(1,1,1,1,1,1)
)

stacked_no_x_no_legend
# =========================
# B) STACKED — only Integrated Global has x-axis + legend
# =========================
p_over_B <- make_panel(prop_over, show_x = FALSE, legend = "none")
p_past_B <- make_panel(prop_past, show_x = FALSE, legend = "none")
p_int_B  <- make_panel(prop_int,  show_x = FALSE, legend = "none")
p_2030_B <- make_panel(prop_2030, show_x = TRUE,  legend = "bottom",
                       legend_levels = c("Medium","High","Very High","Critical"))

stacked_global_with_axis <- cowplot::plot_grid(
  p_over_B, p_past_B, p_int_B, p_2030_B,
  ncol = 1, align = "v"
)

# =========================
# C) STACKED — only Integrated Local has x-axis + legend
# =========================
p_over_C <- make_panel(prop_over, show_x = FALSE, legend = "none")
p_past_C <- make_panel(prop_past, show_x = FALSE, legend = "none")
p_int_C  <- make_panel(prop_int,  show_x = TRUE,  legend = "bottom",
                       legend_levels = c("Low","Medium","High","Very High"))
p_2030_C <- make_panel(prop_2030, show_x = FALSE, legend = "none")

stacked_local_with_axis <- cowplot::plot_grid(
  p_over_C, p_past_C, p_int_C, p_2030_C,
  ncol = 1, align = "v"
)

# =========================
# D) STACKED — like A but with a MOTIVATION legend at the bottom
# =========================
p_mot_for_legend <- make_motivation_panel(show_x = FALSE, legend = "bottom")
legend_mot <- cowplot::get_legend(p_mot_for_legend)

stacked_like_A_with_mot_legend_only <- cowplot::plot_grid(
  cowplot::plot_grid(
    p_over_A, p_past_A, p_int_A, p_2030_A, p_mot_A,
    ncol = 1, align = "v", rel_heights = c(1,1,1,1,1)
  ),
  legend_mot,
  ncol = 1,
  rel_heights = c(1, 0.12)
)

stacked_like_A_with_mot_legend_only
# =========================
# E) STACKED — with motivations (no x, no legend)
# =========================
p_over_A <- make_panel(prop_over, show_x = FALSE, legend = "none")
p_past_A <- make_panel(prop_past, show_x = FALSE, legend = "none")
p_int_A  <- make_panel(prop_int,  show_x = FALSE, legend = "none")
p_2030_A <- make_panel(prop_2030, show_x = FALSE, legend = "none")
#p_mot_A  <- make_motivation_panel(show_x = FALSE, legend = "none")
p_motE_A <- make_motivation_counts_panel(show_x = TRUE, legend = "none")

tight_theme <- theme(plot.margin = margin(1, 10, 1, 10))
p_over_A <- p_over_A + tight_theme
p_past_A <- p_past_A + tight_theme
p_int_A  <- p_int_A  + tight_theme
p_2030_A <- p_2030_A + tight_theme
#p_mot_A  <- p_mot_A  + tight_theme
p_motE_A <- p_motE_A + tight_theme

stacked_no_x_no_legend <- cowplot::plot_grid(
  p_over_A, p_past_A, p_int_A, p_2030_A, p_motE_A,
  ncol = 1, align = "v",
  rel_heights = c(1,1,1,1,1,1)
)

stacked_no_x_no_legend
# =========================
# Save outputs
# =========================
# ggsave("fig1A_stacked_no_x_no_legend.png",       stacked_no_x_no_legend,              width = 7, height = 13, dpi = 300)
# ggsave("fig1B_stacked_global_with_axis.png",     stacked_global_with_axis,            width = 7, height = 9.5, dpi = 300)
# ggsave("fig1C_stacked_local_with_axis.png",      stacked_local_with_axis,             width = 7, height = 9.5, dpi = 300)
# ggsave("fig1D_stacked_with_mot_legend.png",      stacked_like_A_with_mot_legend_only, width = 7, height = 12, dpi = 300)

