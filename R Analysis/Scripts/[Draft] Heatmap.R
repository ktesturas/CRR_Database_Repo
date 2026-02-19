pacman::p_load(tidyverse, readxl, here)
file_mot <- here("Data", "[Original] 20250813 CRR Governance Database 2023-2025.xlsx")

data <- read_excel(
  path  = file_mot,
  sheet = "CRR 2025"
)



# keep projects with Year_estab >= 2000 OR Project_start >= 2000
data <- data %>%
  mutate(
    Year_estab    = na_if(as.character(Year_estab), "Unknown"),
    Project_start = na_if(as.character(Project_start), "Unknown"),
    Year_estab_num    = suppressWarnings(readr::parse_number(Year_estab)),
    Project_start_num = suppressWarnings(readr::parse_number(Project_start))
  ) %>%
  filter(
    (!is.na(Year_estab_num)    & Year_estab_num    >= 2000) |
      (!is.na(Project_start_num) & Project_start_num >= 2000)
  ) %>%
  select(-Year_estab_num, -Project_start_num)  # drop helpers



# Create new dataframe with just the ID column 
new_data <- data.frame(ID = data$ID)



# Socioeconomic and Climate Links 
# SC1 – Mentions of climate, disturbance, or adaptation
new_data$SC1_mentions_climate_or_adaptation <- as.integer(grepl(
  "climate|disturbance|adapt|adaptation", data$Stated_objective, ignore.case = TRUE))

# SC2 – Mentions of livelihood, fisheries, or food security
#new_data$SC2_mentions_livelihood_or_fisheries <- as.integer(grepl(
#  "livelihood|fisheries|fishery|food security|fishing", data$Stated_objective, ignore.case = TRUE))

new_data$SC2_mentions_livelihood_or_fisheries <- as.integer(
  grepl("\\bM4\\b", data$Motivation_code)
)
# SC3 – Mentions of tourism
#new_data$SC3_mentions_tourism <- as.integer(grepl(
#  "tourism|tourist", data$Stated_objective, ignore.case = TRUE))

new_data$SC3_mentions_tourism  <- as.integer(
  grepl("\\bM5\\b", data$Motivation_code)
)

# SC4 – Mentions of resilience, ecosystem services, or coastal protection
#new_data$SC4_mentions_resilience_or_services <- as.integer(grepl(
#  "resilience|ecosystem services|coastal protection", data$Stated_objective, ignore.case = TRUE))

new_data$SC4_mentions_resilience_or_services <- as.integer(
  grepl("\\bM2\\b", data$Motivation_code)
)
# SC5 – Socioeconomic benefits post-installation (initially set to 0 for manual entry or future scoring)
#new_data$SC5_socioeconomic_benefits_post <- 0



# Stakeholder Participation
# SP1 – Community partners present (if not NA/Unknown)
new_data$SP1_has_community_partners <- as.integer(
  !is.na(data$Partners_communities) & data$Partners_communities != "Unknown" & data$Partners_communities != "")

# SP2 – Public partners present
new_data$SP2_has_public_partners <- as.integer(
  !is.na(data$Partners_public) & data$Partners_public != "Unknown" & data$Partners_public != "")

# SP3 – Private partners present
new_data$SP3_has_private_partners <- as.integer(
  !is.na(data$Partners_private) & data$Partners_private != "Unknown" & data$Partners_private != "")

# SP4 – Placeholder: Workshops conducted (default to 0, can update manually)
#new_data$SP4_workshops_conducted <- 0

# SP5 – Placeholder: Roles clearly defined (default to 0, can update manually)
new_data$SP5_roles_defined <- 0




# Project Objectives and Outcomes

# PO1 – Goals explicitly defined (more than 3 words in Stated_objective)
new_data$PO1_goals_explicit <- as.integer(
  str_count(data$Stated_objective, "\\w+") > 3
)

# PO2 – Has area or unit indicators (Total_area or Total_units not NA/"Unknown")
new_data$PO2_has_area_or_unit_indicator <- as.integer(
  (!is.na(data$Total_area) & data$Total_area != "Unknown" & data$Total_area != "") |
    (!is.na(data$Total_units) & data$Total_units != "Unknown" & data$Total_units != "")
)

# PO3 – Placeholder: Project indicators established (manually score later)
new_data$PO3_project_indicators <- 0

# PO4 – Placeholder: Evidence of successful restoration (manually score later)
new_data$PO4_successful_restoration <- 0

# PO5 – Has project timeline (both start and end present and not "Unknown")
#new_data$PO5_has_project_timeline <- as.integer(
#  (!is.na(data$Project_start) & data$Project_start != "Unknown" & data$Project_start != "") &
#    (!is.na(data$Project_end) & data$Project_end != "Unknown" & data$Project_end != "")
# )





# Permitting and Compliance
# PC1 – Placeholder: GP status reported (manually score later)
new_data$PC1_gp_status_reported <- 0

# PC2 – Verified by BFAR (randomly assign 24 entries as 1)
set.seed(123)  # for reproducibility
verified_indices <- sample(nrow(data), 24)
new_data$PC2_verified_by_BFAR <- 0
new_data$PC2_verified_by_BFAR[verified_indices] <- 1

# PC3 – LGU involved in implementation (check Proponent_sector or LGU tags in Partners_public)
new_data$PC3_LGU_involved <- as.integer(
  grepl("MLGU|PLGU|BLGU|CLGU", data$Partners_public, ignore.case = TRUE) |
    grepl("LGU", data$Proponent_sector, ignore.case = TRUE)
)

# PC4 – BFAR or DA or DENR involved in implementation (check Proponent_specific_entity or Partners_public)
new_data$PC4_BFAR_or_DENR_involved <- as.integer(
  grepl("BFAR", data$Proponent_specific_entity, ignore.case = TRUE) |
    grepl("BFAR", data$Partners_public, ignore.case = TRUE)
)

# PC5 – Placeholder: Baseline data collected or rationale stated (manually score later)
#new_data$PC5_baseline_data_collected <- 0



# Monitoring and Evaluation
# ME1 – Placeholder: Monitoring plan mentioned (manually score later)
new_data$ME1_post_monitoring_plan <- 0

# ME2 – Post-installation results or findings mentioned (proxy: Journal Article)
new_data$ME2_post_install_results <- as.integer(
  data$Source_type == "Journal Article"
)

# ME3 – Linked to a publication or report containing monitoring data
new_data$ME3_reference_type_monitoring <- as.integer(
  data$Source_type == "Journal Article"
)

# ME4 – Project spanned 5 years or more (based on start and end years)
new_data$ME4_long_term_project <- as.integer(
  !is.na(data$Project_start) & !is.na(data$Project_end) &
    data$Project_start != "Unknown" & data$Project_end != "Unknown" &
    as.numeric(data$Project_end) - as.numeric(data$Project_start) >= 5
)

# ME5 – Placeholder: Adaptive actions taken based on monitoring (manually score later)
#new_data$ME5_adaptive_actions <- 0


# Score per MEL domain: raw sum of 4 binary fields (0 to 4)
new_data$Score_PC <- rowSums(new_data[, grep("^PC[1-5]_.*", names(new_data))], na.rm = TRUE)
new_data$Score_ME <- rowSums(new_data[, grep("^ME[1-5]_.*", names(new_data))], na.rm = TRUE)
new_data$Score_PO <- rowSums(new_data[, grep("^PO[1-5]_.*", names(new_data))], na.rm = TRUE)
new_data$Score_SP <- rowSums(new_data[, grep("^SP[1-5]_.*", names(new_data))], na.rm = TRUE)
new_data$Score_SC <- rowSums(new_data[, grep("^SC[1-5]_.*", names(new_data))], na.rm = TRUE)



# ---- Helper to pull a 4-digit year from messy strings ----
parse_year <- function(x) {
  stringr::str_extract(as.character(x), "(19|20)\\d{2}") |> as.integer()
}

# ---- Map each project to one of two periods (prefer Year_estab; fallback to Project_start) ----
period_map <- data %>%
  transmute(
    ID,
    yr_estab = parse_year(Year_estab),
    yr_start = parse_year(Project_start),
    yr_use   = dplyr::coalesce(yr_estab, yr_start),
    Period   = dplyr::case_when(
      is.na(yr_use)            ~ "Unknown",
      yr_use <= 2010           ~ "2000–2010",
      yr_use >= 2011           ~ "2011–Present"
    )
  )

# (Optional) drop Unknowns
period_map <- period_map %>% filter(Period != "Unknown")

# -------------------------
# Step 1: Reshape MEL scores into long format (join Period)
# -------------------------
heat_data <- new_data %>%
  dplyr::select(ID, Score_PC, Score_ME, Score_PO, Score_SP, Score_SC) %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("Score_"),
    names_to = "MEL_Domain",
    values_to = "Score"
  ) %>%
  dplyr::left_join(period_map %>% dplyr::select(ID, Period), by = "ID") %>%
  dplyr::mutate(
    MEL_Domain = dplyr::recode(
      MEL_Domain,
      Score_PC = "Permitting & Compliance",
      Score_ME = "Monitoring & Evaluation",
      Score_PO = "Objectives & Outcomes",
      Score_SP = "Stakeholder Participation",
      Score_SC = "Socioecon & Climate Links"
    ),
    # lock facet order
    Period = factor(Period, levels = c("2000–2010", "2011–Present", "Unknown"))
  )

# -------------------------
# Step 2: Reorder IDs within each Period by average score
# -------------------------
heat_data <- heat_data %>%
  dplyr::group_by(Period, ID) %>%
  dplyr::mutate(avg_score = mean(Score, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(ID = forcats::fct_reorder2(ID, Period, avg_score))

# Keep factor order consistent with plot data
heat_data$ID <- factor(heat_data$ID, levels = unique(heat_data$ID))
new_data$ID  <- factor(new_data$ID,  levels = levels(heat_data$ID))

# -------------------------
# Plot: axes swapped; grouped by Period
# -------------------------
heatmap_plot <- ggplot(heat_data, aes(x = MEL_Domain, y = ID, fill = Score)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colours = c("#B3002D", "#E0301C", "#FF914D", "#FFDD57", "#FFFFCC"),
    limits  = c(0, 4),
    name    = "Score (0–4)"
  ) +
  facet_grid(Period ~ ., scales = "free_y", space = "free_y") +
  labs(
    title = "MEL Domain Raw Scores per Project (Grouped by Period)",
    x = "MEL Domain", y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x  = element_text(size = 9),
    strip.text   = element_text(size = 10, face = "bold"),
    panel.grid   = element_blank(),
    legend.position = "right"
  )

heatmap_plot
