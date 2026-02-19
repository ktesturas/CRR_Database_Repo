### PROJECT DURATION — with y-axis labels showing counts
library(tidyverse)

# ---- load ----
data <- readxl::read_excel(
  "C:/Users/Kris Jypson Esturas/OneDrive - Macquarie University/Documents/2025/00 CRR Gov/R Analysis/Data/20250616 CRR Governance Database 2023-2025.xlsx",
  sheet = "CRR 2025"
)

# ---- 1) prepare ----
df <- data %>%
  filter(Project_start != "Unknown", Project_end != "Unknown") %>%
  mutate(
    Project_start = as.integer(Project_start),
    Project_end   = as.integer(Project_end)
  )

# ---- 2) split multi-method cells ----
df_cleaned <- df %>%
  mutate(Specific_method = str_replace_all(Specific_method, ",", ";")) %>%
  mutate(Specific_method = strsplit(Specific_method, ";\\s*")) %>%
  unnest(Specific_method) %>%
  mutate(Specific_method = str_to_title(str_trim(Specific_method)))

# ---- 3) recode methods ----
df_cleaned <- df_cleaned %>%
  filter(Specific_method != "Unknown") %>%
  mutate(Method_clean = case_when(
    str_detect(Specific_method, "Artificial Reef")        ~ "Artificial Reef",
    str_detect(Specific_method, "Coral Gardening")        ~ "Coral Gardening",
    str_detect(Specific_method, "Direct Transplantation") ~ "Direct Transplantation",
    str_detect(Specific_method, "Larval Enhancement")     ~ "Larval Enhancement",
    str_detect(Specific_method, "Mineral Accretion")      ~ "Mineral Accretion",
    str_detect(Specific_method, "Substrate Stabilisation")~ "Substrate Stabilisation",
    str_detect(Specific_method, "Microfragmentation")     ~ "Microfragmentation",
    str_detect(Specific_method, "Algal Removal")          ~ "Algae Removal",
    TRUE                                                  ~ "Other/Unknown"
  ))

# ---- helper: ellipsize long titles to first 10 words ----
shorten_title <- function(s, n_words = 8) {
  w <- str_split(s, "\\s+")[[1]]
  if (length(w) > n_words) paste(c(w[1:n_words], "…"), collapse = " ") else s
}

# ---- 4) build label with counts & ordering (earliest start) ----
# count rows per project *after* unnesting
counts_df <- df_cleaned %>%
  count(Restoration_project_title, name = "n_records")

# earliest start per project
order_df <- df_cleaned %>%
  group_by(Restoration_project_title) %>%
  summarise(Earliest_start = min(Project_start, na.rm = TRUE), .groups = "drop")

# assemble labels: "Short title (n)"
labels_df <- counts_df %>%
  left_join(order_df, by = "Restoration_project_title") %>%
  mutate(
    Short_title = vapply(Restoration_project_title, shorten_title, FUN.VALUE = character(1)),
    Restoration_project_label = paste0(Short_title, " (", n_records, ")")
  ) %>%
  arrange(Earliest_start) %>%
  mutate(
    Restoration_project_label = factor(Restoration_project_label,
                                       levels = rev(unique(Restoration_project_label)))
  )

# attach labels for plotting; drop literal "Unknown"
df_labeled <- df_cleaned %>%
  filter(Restoration_project_title != "Unknown") %>%
  left_join(select(labels_df, Restoration_project_title,
                   Restoration_project_label, Earliest_start),
            by = "Restoration_project_title")

# ---- 5) palette ----
method_colors <- c(
  "Artificial Reef" = "#1b9e77",
  "Coral Gardening" = "#d95f02",
  "Direct Transplantation" = "#7570b3",
  "Larval Enhancement" = "#e6ab02",
  "Mineral Accretion" = "#66a61e",
  "Substrate Stabilisation" = "#e7298a",
  "Microfragmentation" = "#a6761d",
  "Algae Removal" = "#666666",
  "Rubble Stabilisation" = "#999999"
)

# ---- 6) plot (duration bars) ----
ggplot(df_labeled, aes(
  y    = Restoration_project_label,   # <- label with counts
  x    = Project_start,
  xend = Project_end,
  color = Method_clean
)) +
  geom_segment(aes(yend = Restoration_project_label), size = 3.5, lineend = "round") +
  geom_point(
    data = df_labeled %>% dplyr::filter(Project_start == Project_end),
    aes(x = Project_start, y = Restoration_project_label),
    size = 3
  ) +
  annotate("segment", x = 2000, xend = 2000, y = -Inf, yend = Inf,
           linetype = "dashed", color = "black") +
  annotate("segment", x = 2010, xend = 2010, y = -Inf, yend = Inf,
           linetype = "dashed", color = "black") +
  annotate("segment", x = 2019, xend = 2019, y = -Inf, yend = Inf,
           linetype = "dashed", color = "black") +
  scale_color_manual(values = method_colors, name = "Methodology",
                     guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_y_discrete(expand = expansion(mult = c(0.02, 0.02))) +
  scale_x_continuous(
    breaks = seq(1990, 2025, by = 10),
    labels = seq(1990, 2025, by = 10)
  ) +
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




#********************************************************************************
#* PLOT SUBFIGURE A
# Pie charts per era
library(ggplot2)
library(dplyr)

# --- Use Year_estab for decade classification
df_decade <- df_cleaned %>%
  filter(!is.na(Year_estab), Year_estab != "Unknown") %>%
  mutate(
    Year_estab = as.integer(Year_estab),
    decade = floor(Year_estab / 10) * 10,
    decade_label = paste0(decade, "s")
  )

# --- Summarize counts per Method per decade
df_pies <- df_decade %>%
  count(decade_label, Method_clean) %>%
  group_by(decade_label) %>%
  mutate(
    total = sum(n),
    prop = n / total
  ) %>%
  ungroup()


# --- Plot as faceted pies WITH LEGEND ---
ggplot(df_pies, aes(x = "", y = prop, fill = Method_clean)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
  facet_wrap(~decade_label, nrow = 1) +
  scale_fill_manual(values = method_colors, name = "Methodology") +
  theme_void(base_size = 14) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom",   # <-- show legend under the plot
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13)
  )







#**********************
#
## ---- SECOND PLOT: points only (no end year OR year_estab-only), same styling ----

library(dplyr)
library(stringr)
library(tidyr)

recode_method <- function(s) {
  case_when(
    str_detect(s, "Artificial Reef") ~ "Artificial Reef",
    str_detect(s, "Coral Gardening") ~ "Coral Gardening",
    str_detect(s, "Direct Transplantation") ~ "Direct Transplantation",
    str_detect(s, "Larval Enhancement") ~ "Larval Enhancement",
    str_detect(s, "Mineral Accretion") ~ "Mineral Accretion",
    str_detect(s, "Substrate Stabilisation") ~ "Substrate Stabilisation",
    str_detect(s, "Microfragmentation") ~ "Microfragmentation",
    str_detect(s, "Algal Removal") ~ "Algae Removal",
    str_detect(s, "Rubble Stabilisation") ~ "Rubble Stabilisation",
    TRUE ~ "Other/Unknown"
  )
}

shorten_title <- function(title, n_words = 10) {
  words <- str_split(title, "\\s+")[[1]]
  if (length(words) > n_words) paste(c(words[1:n_words], "…"), collapse = " ") else title
}

## A) Start present, End unknown  ------------------------------------------
df_start_no_end <- data %>%
  filter(Project_start != "Unknown", Project_end == "Unknown") %>%
  mutate(
    Project_start = suppressWarnings(as.integer(Project_start)),
    # make types consistent for binding
    Project_end   = NA_integer_,
    Year_estab    = suppressWarnings(as.integer(ifelse(Year_estab == "Unknown", NA, Year_estab)))
  ) %>%
  mutate(
    Specific_method = str_replace_all(Specific_method, ",", ";"),
    Specific_method = strsplit(Specific_method, ";\\s*")
  ) %>%
  unnest(Specific_method) %>%
  mutate(
    Specific_method = str_to_title(str_trim(Specific_method)),
    Method_clean    = recode_method(Specific_method),
    x_year          = Project_start     # plot on start year
  )

## B) Year_estab present; BOTH start & end unknown  ------------------------
df_estab_only <- data %>%
  filter((Project_start == "Unknown" | is.na(Project_start)),
         (Project_end   == "Unknown" | is.na(Project_end)),
         !is.na(Year_estab), Year_estab != "Unknown") %>%
  mutate(
    Year_estab    = suppressWarnings(as.integer(Year_estab)),
    Project_start = NA_integer_,   # ensure same types for bind_rows
    Project_end   = NA_integer_
  ) %>%
  mutate(
    Specific_method = str_replace_all(Specific_method, ",", ";"),
    Specific_method = strsplit(Specific_method, ";\\s*")
  ) %>%
  unnest(Specific_method) %>%
  mutate(
    Specific_method = str_to_title(str_trim(Specific_method)),
    Method_clean    = recode_method(Specific_method),
    x_year          = Year_estab       # plot on year_estab
  )

## C) Combine & label with counts (ellipsis first, then counts) ------------
df_points_all <- bind_rows(df_start_no_end, df_estab_only)

counts_pts <- df_points_all %>%
  count(Restoration_project_title, name = "n_records_pts")

df_points_all <- df_points_all %>%
  left_join(counts_pts, by = "Restoration_project_title") %>%
  mutate(
    Short_title = vapply(Restoration_project_title, shorten_title, FUN.VALUE = character(1)),
    Restoration_project_label = paste0(Short_title, " (", n_records_pts, ")")
  ) %>%
  group_by(Restoration_project_label) %>%
  mutate(Earliest_x = min(x_year, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Earliest_x) %>%
  mutate(
    Restoration_project_label = factor(Restoration_project_label,
                                       levels = rev(unique(Restoration_project_label)))
  )

# ---- PLOT: identical style to your first plot, points with black outline + slight jitter ----
set.seed(42)

ggplot(df_points_all, aes(
  y = Restoration_project_label,
  x = x_year,
  fill = Method_clean
)) +
  geom_point(
    shape = 21, size = 3, stroke = 0.5, color = "black", alpha = 0.95,
    position = position_jitter(width = 0.25, height = 0.15)
  ) +
  # dashed reference lines (same as first plot)
  annotate("segment", x = 2000, xend = 2000, y = -Inf, yend = Inf,
           linetype = "dashed", color = "black") +
  annotate("segment", x = 2010, xend = 2010, y = -Inf, yend = Inf,
           linetype = "dashed", color = "black") +
  
  scale_fill_manual(
    values = method_colors,
    name = "Methodology",
    guide = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  scale_y_discrete(expand = expansion(mult = c(0.02, 0.02))) +
  # --- lock x-axis extent to match the first figure exactly ---
  scale_x_continuous(
    limits = c(1985, 2025),
    breaks  = seq(1990, 2025, by = 10),
    labels  = seq(1990, 2025, by = 10)
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 9, color = "black"),
    axis.text.x = element_text(size = 14, color = "black"),
    legend.position = "none",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.2),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    plot.margin = margin(10, 20, 10, 10)
  ) +
  coord_cartesian(clip = "off")



















#************************************************************************************
# EXTRA CODE


### PROJECT DURATION + YEAR_ESTAB POINTS
library(tidyverse)
library(readxl)

# --- Load ---
data <- read_excel(
  "C:/Users/Kris Jypson Esturas/OneDrive - Macquarie University/Documents/2025/00 CRR Gov/R Analysis/Data/20250616 CRR Governance Database 2023-2025.xlsx",
  sheet = "CRR 2025"
)

# --- Clean helpers ---
recode_method <- function(x) {
  x <- x %>%
    str_replace_all(",", ";") %>%
    strsplit(";\\s*") %>% unlist() %>%
    str_to_title() %>%
    str_trim()
  tibble(Specific_method = x)
}

method_to_clean <- function(s) {
  case_when(
    str_detect(s, "Artificial Reef")        ~ "Artificial Reef",
    str_detect(s, "Coral Gardening")        ~ "Coral Gardening",
    str_detect(s, "Direct Transplantation") ~ "Direct Transplantation",
    str_detect(s, "Larval Enhancement")     ~ "Larval Enhancement",
    str_detect(s, "Mineral Accretion")      ~ "Mineral Accretion",
    str_detect(s, "Substrate Stabilisation")~ "Substrate Stabilisation",
    str_detect(s, "Microfragmentation")     ~ "Microfragmentation",
    str_detect(s, "Algal Removal")          ~ "Algae Removal",
    TRUE ~ "Other/Unknown"
  )
}

method_colors <- c(
  "Artificial Reef" = "#1b9e77",
  "Coral Gardening" = "#d95f02",
  "Direct Transplantation" = "#7570b3",
  "Larval Enhancement" = "#e6ab02",
  "Mineral Accretion" = "#66a61e",
  "Substrate Stabilisation" = "#e7298a",
  "Microfragmentation" = "#a6761d",
  "Algae Removal" = "#666666",
  "Other/Unknown" = "#999999"
)

# --- A) Duration rows (with start & end) ---
df_duration <- data %>%
  filter(Project_start != "Unknown", Project_end != "Unknown") %>%
  mutate(
    Project_start = suppressWarnings(as.integer(Project_start)),
    Project_end   = suppressWarnings(as.integer(Project_end))
  ) %>%
  filter(!is.na(Project_start), !is.na(Project_end)) %>%
  rowwise() %>%
  do({
    base <- .
    # split methods
    recode_method(base$Specific_method) %>%
      mutate(Restoration_project_title = base$Restoration_project_title,
             Project_start = base$Project_start,
             Project_end   = base$Project_end,
             Year_estab    = suppressWarnings(as.integer(base$Year_estab)))
  }) %>%
  ungroup() %>%
  mutate(Method_clean = method_to_clean(Specific_method))

# order titles by earliest start
title_order_duration <- df_duration %>%
  group_by(Restoration_project_title) %>%
  summarise(Earliest_start = min(Project_start, na.rm = TRUE), .groups="drop") %>%
  arrange(Earliest_start) %>%
  pull(Restoration_project_title)

# optional: wrap very long titles to 10 words
wrap_title <- function(txt, n_words = 10) {
  words <- str_split(txt, "\\s+")[[1]]
  if (length(words) > n_words) paste(c(words[1:n_words], "…"), collapse = " ") else txt
}

df_duration <- df_duration %>%
  mutate(Restoration_project_title = vapply(Restoration_project_title, wrap_title, FUN.VALUE = character(1)))

# --- B) Named points with Year_estab only (no duration) ---
df_named_points <- data %>%
  filter((Project_start == "Unknown" | is.na(Project_start)) &
           (Project_end   == "Unknown" | is.na(Project_end))) %>%
  filter(!is.na(Year_estab), Year_estab != "Unknown") %>%
  mutate(Year_estab = suppressWarnings(as.integer(Year_estab))) %>%
  filter(!is.na(Year_estab), Restoration_project_title != "Unknown") %>%
  rowwise() %>%
  do({
    base <- .
    recode_method(base$Specific_method) %>%
      mutate(Restoration_project_title = base$Restoration_project_title,
             Year_estab = base$Year_estab)
  }) %>%
  ungroup() %>%
  mutate(Method_clean = method_to_clean(Specific_method),
         Restoration_project_title = vapply(Restoration_project_title, wrap_title, FUN.VALUE = character(1)))

# order these by Year_estab (oldest first)
title_order_named_pts <- df_named_points %>%
  group_by(Restoration_project_title) %>%
  summarise(Year_estab = min(Year_estab, na.rm = TRUE), .groups="drop") %>%
  arrange(Year_estab) %>%
  pull(Restoration_project_title)

# --- C) Unknown-titled points with Year_estab -> single shared row ---
unknown_row_label <- "Unknown (year only)"
df_unknown_points <- data %>%
  filter((Project_start == "Unknown" | is.na(Project_start)) &
           (Project_end   == "Unknown" | is.na(Project_end))) %>%
  filter(!is.na(Year_estab), Year_estab != "Unknown") %>%
  mutate(Year_estab = suppressWarnings(as.integer(Year_estab))) %>%
  filter(!is.na(Year_estab), Restoration_project_title == "Unknown") %>%
  rowwise() %>%
  do({
    base <- .
    recode_method(base$Specific_method) %>%
      mutate(Restoration_project_title = unknown_row_label,
             Year_estab = base$Year_estab)
  }) %>%
  ungroup() %>%
  mutate(Method_clean = method_to_clean(Specific_method))

# --- D) Build final y-order (duration titles, then named points, then unknown row) ---
all_titles <- c(title_order_duration, setdiff(title_order_named_pts, title_order_duration), unknown_row_label) %>% unique()

# factor levels applied to each df
df_duration       <- df_duration       %>% mutate(Restoration_project_title = factor(Restoration_project_title, levels = rev(all_titles)))
df_named_points   <- df_named_points   %>% mutate(Restoration_project_title = factor(Restoration_project_title, levels = rev(all_titles)))
df_unknown_points <- df_unknown_points %>% mutate(Restoration_project_title = factor(Restoration_project_title, levels = rev(all_titles)))

# --- Plot ---
ggplot() +
  # duration bars
  geom_segment(
    data = df_duration,
    aes(y = Restoration_project_title, yend = Restoration_project_title,
        x = Project_start, xend = Project_end, color = Method_clean),
    size = 3.5
  ) +
  # points where start == end (duration layer)
  geom_point(
    data = df_duration %>% filter(Project_start == Project_end),
    aes(x = Project_start, y = Restoration_project_title, color = Method_clean),
    size = 3
  ) +
  # named projects with only Year_estab -> circle w/ black outline & method fill
  geom_point(
    data = df_named_points,
    aes(x = Year_estab, y = Restoration_project_title, fill = Method_clean),
    shape = 21, size = 3.2, stroke = 0.8, color = "black"
  ) +
  # unknown-titled Year_estab -> all on one shared row
  geom_point(
    data = df_unknown_points,
    aes(x = Year_estab, y = Restoration_project_title, fill = Method_clean),
    shape = 21, size = 3.2, stroke = 0.8, color = "black"
  ) +
  # vertical dashed lines at 2000 & 2010
  annotate("segment", x = 2000, xend = 2000, y = -Inf, yend = Inf,
           linetype = "dashed", color = "black") +
  annotate("segment", x = 2010, xend = 2010, y = -Inf, yend = Inf,
           linetype = "dashed", color = "black") +
  # scales
  scale_color_manual(values = method_colors, name = "Methodology") +
  scale_fill_manual(values = method_colors, guide = "none") +  # suppress duplicate legend
  scale_y_discrete(expand = expansion(mult = c(0.02, 0.20))) +
  scale_x_continuous(breaks = seq(1985, 2025, by = 5), labels = seq(1985, 2025, by = 5)) +
  # labels/theme
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 9, color = "black"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 14, color = "black"),
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 20, 10, 10)
  ) +
  coord_cartesian(clip = "off")

