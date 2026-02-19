### PROJECT DURATION — y-axis labels include record counts

library(tidyverse)
library(here)
library(readxl)

# ---- load data ----
file_mot <- here("Data", "[Original] 20250813 CRR Governance Database 2023-2025.xlsx")

data <- read_excel(
  path  = file_mot,
  sheet = "CRR 2025"
)

# ---- helper: shorten titles (first n words) ----
shorten_title <- function(title, n_words = 8) {
  words <- str_split(title, "\\s+")[[1]]
  if (length(words) > n_words) paste(c(words[1:n_words], "…"), collapse = " ") else title
}

# ---- helper: recode restoration methods into standard classes ----
recode_method <- function(s) {
  case_when(
    str_detect(s, "Artificial Reef")         ~ "Artificial Reef",
    str_detect(s, "Coral Gardening")         ~ "Coral Gardening",
    str_detect(s, "Direct Transplantation")  ~ "Direct Transplantation",
    str_detect(s, "Larval Enhancement")      ~ "Larval Enhancement",
    str_detect(s, "Mineral Accretion")       ~ "Mineral Accretion",
    str_detect(s, "Substrate Stabilisation") ~ "Substrate Stabilisation",
    str_detect(s, "Microfragmentation")      ~ "Microfragmentation",
    str_detect(s, "Algal Removal")           ~ "Algae Removal",
    str_detect(s, "Rubble Stabilisation")    ~ "Rubble Stabilisation",
    TRUE                                     ~ "Other/Unknown"
  )
}

# ---- 1) prepare: keep records with known start AND end years ----
df <- data %>%
  filter(Project_start != "Unknown", Project_end != "Unknown") %>%
  mutate(
    Project_start = as.integer(Project_start),
    Project_end   = as.integer(Project_end)
  )

# ---- 2) split multi-method cells (comma/semicolon separated) ----
df_cleaned <- df %>%
  mutate(Specific_method = str_replace_all(Specific_method, ",", ";")) %>%
  separate_rows(Specific_method, sep = ";\\s*") %>%
  mutate(Specific_method = str_to_title(str_trim(Specific_method)))

# ---- 3) recode methods ----
df_cleaned <- df_cleaned %>%
  filter(Specific_method != "Unknown") %>%
  mutate(Method_clean = recode_method(Specific_method))

# ---- 4) build y-axis labels: "short title (n)" ordered by earliest start ----
labels_df <- df_cleaned %>%
  count(Restoration_project_title, name = "n_records") %>%
  left_join(
    df_cleaned %>%
      group_by(Restoration_project_title) %>%
      summarise(Earliest_start = min(Project_start, na.rm = TRUE), .groups = "drop"),
    by = "Restoration_project_title"
  ) %>%
  mutate(
    Short_title = vapply(Restoration_project_title, shorten_title, FUN.VALUE = character(1)),
    Restoration_project_label = paste0(Short_title, " (", n_records, ")")
  ) %>%
  arrange(Earliest_start) %>%
  mutate(Restoration_project_label = factor(Restoration_project_label,
                                            levels = rev(unique(Restoration_project_label))))

df_labeled <- df_cleaned %>%
  filter(Restoration_project_title != "Unknown") %>%
  left_join(labels_df %>% select(Restoration_project_title, Restoration_project_label),
            by = "Restoration_project_title")

# ---- 5) palette ----
method_colors <- c(
  "Artificial Reef"         = "#1b9e77",
  "Coral Gardening"         = "#d95f02",
  "Direct Transplantation"  = "#7570b3",
  "Larval Enhancement"      = "#e6ab02",
  "Mineral Accretion"       = "#66a61e",
  "Substrate Stabilisation" = "#e7298a",
  "Microfragmentation"      = "#a6761d",
  "Algae Removal"           = "#666666",
  "Rubble Stabilisation"    = "#999999",
  "Other/Unknown"           = "#bdbdbd"
)

# =============================================================================
# PLOT A — Projects that have year
# =============================================================================
ggplot(df_labeled, aes(
  y     = Restoration_project_label,
  x     = Project_start,
  xend  = Project_end,
  color = Method_clean
)) +
  geom_segment(aes(yend = Restoration_project_label), size = 3.5, lineend = "round") +
  # projects that start=end (single-year)
  geom_point(
    data = df_labeled %>% filter(Project_start == Project_end),
    aes(x = Project_start, y = Restoration_project_label),
    size = 3
  ) +
  # reference years
  geom_vline(xintercept = c(2000, 2010, 2019), linetype = "dashed", color = "black") +
  scale_color_manual(values = method_colors, name = "Methodology",
                     guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_y_discrete(expand = expansion(mult = c(0.02, 0.02))) +
  scale_x_continuous(breaks = seq(1990, 2025, by = 10)) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 14, color = "black"),
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.2),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    plot.margin = margin(10, 20, 10, 10)
  ) +
  coord_cartesian(clip = "off")


# =============================================================================
# PLOT SUBFIGURE A — Pie charts per decade (Year_estab)
# =============================================================================

df_decade <- df_cleaned %>%
  filter(!is.na(Year_estab), Year_estab != "Unknown") %>%
  mutate(
    Year_estab    = as.integer(Year_estab),
    decade        = floor(Year_estab / 10) * 10,
    decade_label  = paste0(decade, "s")
  )

df_pies <- df_decade %>%
  count(decade_label, Method_clean) %>%
  group_by(decade_label) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

ggplot(df_pies, aes(x = "", y = prop, fill = Method_clean)) +
  geom_col(width = 1, color = "black") +
  coord_polar(theta = "y") +
  facet_wrap(~decade_label, nrow = 1) +
  scale_fill_manual(values = method_colors, name = "Methodology") +
  theme_void(base_size = 14) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13)
  )
# (Legend here is intended for the entire figure)


# =============================================================================
# EXTRA PLOT — Points only (no end year OR Year_estab-only)
# =============================================================================

# A) start year present, end year missing -> plot at start year
df_start_no_end <- data %>%
  filter(Project_start != "Unknown", Project_end == "Unknown") %>%
  mutate(
    Project_start = suppressWarnings(as.integer(Project_start)),
    Project_end   = NA_integer_,
    Year_estab    = suppressWarnings(as.integer(na_if(Year_estab, "Unknown")))
  ) %>%
  mutate(Specific_method = str_replace_all(Specific_method, ",", ";")) %>%
  separate_rows(Specific_method, sep = ";\\s*") %>%
  mutate(
    Specific_method = str_to_title(str_trim(Specific_method)),
    Method_clean    = recode_method(Specific_method),
    x_year          = Project_start
  )

# B) Year_estab present, start+end missing -> plot at Year_estab
df_estab_only <- data %>%
  filter((Project_start == "Unknown" | is.na(Project_start)),
         (Project_end   == "Unknown" | is.na(Project_end)),
         !is.na(Year_estab), Year_estab != "Unknown") %>%
  mutate(
    Year_estab    = suppressWarnings(as.integer(Year_estab)),
    Project_start = NA_integer_,
    Project_end   = NA_integer_
  ) %>%
  mutate(Specific_method = str_replace_all(Specific_method, ",", ";")) %>%
  separate_rows(Specific_method, sep = ";\\s*") %>%
  mutate(
    Specific_method = str_to_title(str_trim(Specific_method)),
    Method_clean    = recode_method(Specific_method),
    x_year          = Year_estab
  )

# combine, then label with counts and order by earliest x_year
df_points_all <- bind_rows(df_start_no_end, df_estab_only) %>%
  filter(Restoration_project_title != "Unknown")

labels_pts <- df_points_all %>%
  count(Restoration_project_title, name = "n_records") %>%
  mutate(
    Short_title = vapply(Restoration_project_title, shorten_title, FUN.VALUE = character(1)),
    Restoration_project_label = paste0(Short_title, " (", n_records, ")")
  ) %>%
  left_join(
    df_points_all %>%
      group_by(Restoration_project_title) %>%
      summarise(Earliest_x = min(x_year, na.rm = TRUE), .groups = "drop"),
    by = "Restoration_project_title"
  ) %>%
  arrange(Earliest_x) %>%
  mutate(Restoration_project_label = factor(Restoration_project_label,
                                            levels = rev(unique(Restoration_project_label))))

df_points_all <- df_points_all %>%
  left_join(labels_pts %>% select(Restoration_project_title, Restoration_project_label),
            by = "Restoration_project_title")

set.seed(42)

ggplot(df_points_all, aes(
  y    = Restoration_project_label,
  x    = x_year,
  fill = Method_clean
)) +
  geom_point(
    shape = 21, size = 3, stroke = 0.5, color = "black", alpha = 0.95,
    position = position_jitter(width = 0.25, height = 0.15)
  ) +
  geom_vline(xintercept = c(2000, 2010), linetype = "dashed", color = "black") +
  scale_fill_manual(values = method_colors, name = "Methodology",
                    guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_y_discrete(expand = expansion(mult = c(0.02, 0.02))) +
  scale_x_continuous(limits = c(1985, 2025),
                     breaks = seq(1990, 2025, by = 10)) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 9, color = "black"),
    axis.text.x = element_text(size = 14, color = "black"),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.2),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    plot.margin = margin(10, 20, 10, 10)
  ) +
  coord_cartesian(clip = "off")
