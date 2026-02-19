# ---- packages ----
pacman::p_load(tidyverse, readxl, stringr, ggnewscale)

# ---- load ----
file_mot <- here("Data", "20250609 CRR Governance Database 2023-2025.xlsx")
data <- read_excel(path = file_mot, sheet = "CRR 2025")


# ---- helpers ----
parse_year <- function(x) {
  stringr::str_extract(as.character(x), "(19|20)\\d{2}") %>%
    readr::parse_number()
}

clean_val <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- stringr::str_replace_all(x, "\\s+", " ")
  stringr::str_trim(x)
}

get_chr <- function(df, nm) if (nm %in% names(df)) as.character(df[[nm]]) else rep(NA_character_, nrow(df))

# Normalizer for partner parsing
norm <- function(x) {
  x <- stringr::str_to_upper(x)
  x <- stringr::str_replace_all(x, "\\.", "")
  x <- stringr::str_replace_all(x, "\\s+", " ")
  stringr::str_trim(x)
}

# ---- clean "UNKNOWN" globally ----
data <- data %>%
  mutate(across(
    where(is.character),
    ~ {
      x <- str_trim(.x)
      x[str_to_upper(x) == "UNKNOWN"] <- NA_character_
      x
    }
  ))


# ---- prefer Year_estab; fallback Project_start; keep 1970–1999 only ----
data <- data %>%
  mutate(
    Year_estab_num    = parse_year(Year_estab),
    Project_start_num = parse_year(Project_start),
    Year_use          = dplyr::coalesce(Year_estab_num, Project_start_num)
  ) %>%
  filter(!is.na(Year_use) & Year_use >= 1970 & Year_use < 2000) %>%
  mutate(
    Decade = paste0((Year_use %/% 10) * 10, "s"),
    .after = Year_use
  )

# ================================
# Permitting & Compliance (PC) 
# ================================
PC1_score <- rep(0L, nrow(data)) # placeholder
PC1_comp  <- rep(0L, nrow(data))

funders    <- clean_val(get_chr(data, "Funder_specific_entity"))
proponents <- clean_val(get_chr(data, "Proponent_specific_entity"))
partners   <- clean_val(get_chr(data, "Partners_public"))

blob <- paste(funders, proponents, partners, sep = " | ")

# completeness flags
PC2_comp <- as.integer(funders != "" | proponents != "" | partners != "")
PC3_comp <- PC2_comp

# scores
PC2_score <- ifelse(
  str_detect(blob, "(?i)\\bDA\\b|\\bBFAR\\b|\\bDENR\\b|\\bDA[- ]?BFAR\\b"),
  1L, 0L
)
PC3_score <- ifelse(
  str_detect(blob, "(?i)\\b(?:M|P|B|C)LGU\\b"),
  1L, 0L
)

PC4_score <- rep(0L, nrow(data)) # placeholder
PC4_comp  <- rep(0L, nrow(data))

pc <- tibble(
  ID = data$ID, Decade = data$Decade,
  PC1_score, PC1_comp,
  PC2_score, PC2_comp,
  PC3_score, PC3_comp,
  PC4_score, PC4_comp
) %>%
  mutate(
    Score_PC_raw     = PC1_score + PC2_score + PC3_score + PC4_score,  # 0–4
    n_avail          = PC1_comp + PC2_comp + PC3_comp + PC4_comp,       # 0–4
    Completeness_PC  = n_avail / 4,                                     # 0–1
    #  Score_PC = if_else(n_avail > 0, Score_PC_raw, NA_real_)
    Score_PC        = if_else(n_avail > 0, (Score_PC_raw / n_avail) * 4, 0)           # RAW (no rescale, NA → 0)
  )

pc_decade <- pc %>%
  group_by(Decade) %>%
  summarise(
    mean_score   = mean(Score_PC, na.rm = TRUE),               # 0–4 RAW mean
    completeness = 100 * mean(Completeness_PC, na.rm = TRUE),  # %
    .groups = "drop"
  ) %>%
  mutate(MEL_Domain = "Permitting & Compliance")





# ================================
# Stakeholder Participation (SP) — RAW scoring w/ DA/BFAR/DENR/LGU exclusion
# ================================
# --- Step: Normalize partner fields (Unknown -> NA already applied above) ---
data <- data %>%
  mutate(
    Partners_communities = na_if(Partners_communities, ""),
    Partners_public      = na_if(Partners_public, ""),
    Partners_private     = na_if(Partners_private, "")
  )

# --- Step: If ANY partner field has a value, fill the others with "None" ---
#data <- data %>%
#  rowwise() %>%
#  mutate(
#    any_partner_val = any(!is.na(c_across(c(Partners_communities, Partners_public, Partners_private)))),
#    Partners_communities = ifelse(any_partner_val & is.na(Partners_communities), "None", Partners_communities),
#    Partners_public      = ifelse(any_partner_val & is.na(Partners_public), "None", Partners_public),
#    Partners_private     = ifelse(any_partner_val & is.na(Partners_private), "None", Partners_private)
#  ) %>%
#  ungroup() %>%
#  select(-any_partner_val)

comm_raw  <- clean_val(get_chr(data, "Partners_communities"))
pub_raw   <- clean_val(get_chr(data, "Partners_public"))
priv_raw  <- clean_val(get_chr(data, "Partners_private"))
roles_raw <- clean_val(get_chr(data, "roles_defined"))


# completeness (keep as-is: 1 if the field was answered at all, even "None")
SP1_comp <- as.integer(comm_raw != "")
SP2_comp <- as.integer(pub_raw  != "")
SP3_comp <- as.integer(priv_raw != "")
SP4_comp <- as.integer(roles_raw != "")




# score helpers
is_none <- function(x) stringr::str_detect(x, "(?i)^\\s*none\\s*$")

# SP1 — community partners present → 1 only if not "None"
SP1_score <- as.integer(SP1_comp == 1 & !is_none(comm_raw))

# SP2 — public partners present → 1 only if not "None"
SP2_score <- as.integer(SP2_comp == 1 & !is_none(pub_raw))

# SP3 — private partners present → 1 only if not "None"
SP3_score <- as.integer(SP3_comp == 1 & !is_none(priv_raw))

# SP4 — roles/contributions clearly defined (accepts 1/yes/true/defined)
SP4_score <- as.integer(stringr::str_detect(roles_raw, "(?i)^(1|yes|true|defined)$"))

# assemble & aggregate (RAW scores, no rescale)
sp <- tibble(
  ID = data$ID, Decade = data$Decade,
  SP1_score, SP1_comp,
  SP2_score, SP2_comp,
  SP3_score, SP3_comp,
  SP4_score, SP4_comp
) %>%
  mutate(
    Score_SP_raw    = SP1_score + SP2_score + SP3_score + SP4_score,  # 0–4
    n_avail         = SP1_comp + SP2_comp + SP3_comp + SP4_comp,      # 0–4
    Completeness_SP = n_avail / 4,                                    # 0–1
    # Score_SP = if_else(n_avail > 0, Score_SP_raw, NA_real_)
    
    Score_SP        = if_else(n_avail > 0, (Score_SP_raw / n_avail) * 4, 0)
  )


sp_decade <- sp %>%
  group_by(Decade) %>%
  summarise(
    mean_score   = mean(Score_SP, na.rm = TRUE),               # RAW 0–4
    completeness = 100 * mean(Completeness_SP, na.rm = TRUE),  # %
    .groups = "drop"
  ) %>%
  mutate(MEL_Domain = "Stakeholder Participation")





# ================================
# Socioeconomic & Climate Links (SC)
# ================================

# (safety) helper if not already defined elsewhere
if (!exists("get_chr")) {
  get_chr <- function(df, col) if (col %in% names(df)) df[[col]] else NA_character_
}

# Pull fields & clean
stated_obj        <- clean_val(get_chr(data, "Stated_objective"))
socioecon_benefit <- clean_val(get_chr(data, "Socioecon_benefits"))

# -------- Indicators --------
# SC1 — Socioeconomic benefits reported
SC1_comp  <- as.integer(socioecon_benefit != "")
SC1_score <- ifelse(SC1_comp == 1, 1L, 0L)   # "all NA (completeness) or 0 (score)"

# SC2 — Mentions climate links in motivation/objectives
SC2_comp  <- as.integer(stated_obj != "")
SC2_score <- ifelse(str_detect(stated_obj, regex("\\bclimate\\b", ignore_case = TRUE)), 1L, 0L)

# SC3 — Links to livelihood / fisheries / food security
SC3_comp  <- as.integer(stated_obj != "")
SC3_score <- ifelse(
  str_detect(stated_obj, regex("\\b(fisheries?|food|livelihood|fishing|production|catch|fish stock|fishery|productivity|fish populations|fish population)\\b", ignore_case = TRUE)),
  1L, 0L
)

# SC4 — Links to tourism
SC4_comp  <- as.integer(stated_obj != "")
SC4_score <- ifelse(str_detect(stated_obj, regex("\\btouris(t|m)\\b", ignore_case = TRUE)), 1L, 0L)

# -------- Assemble & rescale (0–4) --------
sc <- tibble(
  ID = data$ID, Decade = data$Decade,
  SC1_score, SC1_comp,
  SC2_score, SC2_comp,
  SC3_score, SC3_comp,
  SC4_score, SC4_comp
) %>%
  mutate(
    Score_SC_raw    = SC1_score + SC2_score + SC3_score + SC4_score,  # 0–4
    n_avail         = SC1_comp + SC2_comp + SC3_comp + SC4_comp,      # 0–4
    Completeness_SC = n_avail / 4,                                    # 0–1
    # Rescaled to 0–4 by availability; if nothing available, force 0
    # Score_SC = if_else(n_avail > 0, Score_SC_raw, NA_real_)
    
    Score_SC        = if_else(n_avail > 0, (Score_SC_raw / n_avail) * 4, 0)
  )

# -------- Aggregate by decade --------
sc_decade <- sc %>%
  group_by(Decade) %>%
  summarise(
    mean_score   = mean(Score_SC, na.rm = TRUE),               # 0–4 mean (rescaled)
    completeness = 100 * mean(Completeness_SC, na.rm = TRUE),  # %
    .groups = "drop"
  ) %>%
  mutate(MEL_Domain = "Socioeconomic & Climate Links")



# ================================
# Monitoring & Evaluation (ME)
# ================================

# (safety) helper if not already defined elsewhere
if (!exists("get_chr")) {
  get_chr <- function(df, col) if (col %in% names(df)) df[[col]] else NA_character_
}

# Pull fields & clean
monitoring_results <- clean_val(get_chr(data, "Monitoring_results"))
project_start      <- suppressWarnings(as.numeric(get_chr(data, "Project_start")))
project_end        <- suppressWarnings(as.numeric(get_chr(data, "Project_end")))
source_type        <- clean_val(get_chr(data, "Source_type"))
reference_text     <- clean_val(get_chr(data, "Reference"))
eco_indicators     <- clean_val(get_chr(data, "Ecological_indicators"))

# -------- Indicators --------
# ME1 — Initial recruitment monitoring info
ME1_comp  <- as.integer(monitoring_results != "")
ME1_score <- ifelse(ME1_comp == 1, 1L, 0L)   # "all NA (completeness) or 0 (score)"

# ME2 — Activity spanned multiple years (≥ 5 yrs)
ME2_comp  <- as.integer(!is.na(project_start) & !is.na(project_end))
ME2_score <- ifelse(ME2_comp == 1 & (project_end - project_start >= 5), 1L, 0L)


# ME3 — Linked to publication or official report
ME3_comp  <- as.integer(source_type != "" | reference_text != "")
ME3_score <- ifelse(
  str_detect(source_type, regex("Journal Article", ignore_case = TRUE)) |
    str_detect(reference_text, regex("(^|\\W)(annual|briefer|quarterly)(\\W|$)", ignore_case = TRUE)),
  1L, 0L
)


# ME4 — Ecological outcomes reported (growth, survival, biomass, cover, etc.)
ME4_comp  <- as.integer(eco_indicators != "")
ME4_score <- ifelse(ME4_comp == 1, 1L, 0L)

# -------- Assemble & rescale (0–4) --------
me <- tibble(
  ID = data$ID, Decade = data$Decade,
  ME1_score, ME1_comp,
  ME2_score, ME2_comp,
  ME3_score, ME3_comp,
  ME4_score, ME4_comp
) %>%
  mutate(
    Score_ME_raw    = ME1_score + ME2_score + ME3_score + ME4_score,  # 0–4
    n_avail         = ME1_comp + ME2_comp + ME3_comp + ME4_comp,      # 0–4
    Completeness_ME = n_avail / 4,                                    # 0–1
    Score_ME        = if_else(n_avail > 0, (Score_ME_raw / n_avail) * 4, 0)
  )

# -------- Aggregate by decade --------
me_decade <- me %>%
  group_by(Decade) %>%
  summarise(
    mean_score   = mean(Score_ME, na.rm = TRUE),               # 0–4 mean (rescaled)
    completeness = 100 * mean(Completeness_ME, na.rm = TRUE),  # %
    .groups = "drop"
  ) %>%
  mutate(MEL_Domain = "Monitoring & Evaluation")






# ================================
# Project Objectives & Outcomes (PO)
# ================================

# (safety) helper if not already defined elsewhere
if (!exists("get_chr")) {
  get_chr <- function(df, col) if (col %in% names(df)) df[[col]] else NA_character_
}

# ---- Pull fields & clean ----
research_results  <- clean_val(get_chr(data, "Research_results"))
evaluated_obj        <- clean_val(get_chr(data, "Evaluated_objective"))
stated_obj        <- clean_val(get_chr(data, "Stated_objective"))
reference_text    <- clean_val(get_chr(data, "Reference"))
monitoring_text   <- clean_val(get_chr(data, "Monitoring_results"))

total_area  <- suppressWarnings(readr::parse_number(get_chr(data, "Total_area")))
total_units <- suppressWarnings(readr::parse_number(get_chr(data, "Total_units")))



# ---- Indicators ----
# PO1 — Results reported to DA-BFAR (or equivalent)
PO1_comp  <- as.integer(research_results != "")
PO1_score <- ifelse(PO1_comp == 1, 1L, 0L)   # "all NA (completeness) or 0 (score)"

# PO2 — Outcomes evaluated against stated objectives
# Proxy: objectives present AND results/monitoring mention evaluation language
eval_regex <- regex(
  "(achiev|met|not met|part(ly)? met|success|effective|ineffective|increase|decrease|impact|evaluation|assessment)",
  ignore_case = TRUE
)

PO2_comp <- as.integer(evaluated_obj != "")
PO2_score <- ifelse(
  PO2_comp == 1 &
    (
      str_detect(research_results, eval_regex) |
        str_detect(monitoring_text, eval_regex)  |
        (!is.na(total_area)  | !is.na(total_units)) # numerical outcomes exist
    ),
  1L, 0L
)

# PO3 — Goals explicit / not generic (word count > 5)
PO3_comp  <- as.integer(stated_obj != "")
PO3_score <- ifelse(PO3_comp == 1 & stringr::str_count(stated_obj, "\\S+") > 5, 1L, 0L)

# PO4 — Scope defined (explicit units or area covered)
PO4_comp  <- as.integer(!is.na(total_area) | !is.na(total_units))
PO4_score <- ifelse((!is.na(total_area)  & total_area  > 1) |
                      (!is.na(total_units) & total_units > 1), 1L, 0L)




# ---- Assemble & rescale (0–4) ----
po <- tibble(
  ID = data$ID, Decade = data$Decade,
  PO1_score, PO1_comp,
  PO2_score, PO2_comp,
  PO3_score, PO3_comp,
  PO4_score, PO4_comp
) %>%
  mutate(
    Score_PO_raw    = PO1_score + PO2_score + PO3_score + PO4_score,  # 0–4
    n_avail         = PO1_comp + PO2_comp + PO3_comp + PO4_comp,      # 0–4
    Completeness_PO = n_avail / 4,                                    # 0–1
    Score_PO        = if_else(n_avail > 0, (Score_PO_raw / n_avail) * 4, 0)
    # Score_PO = if_else(n_avail > 0, Score_PO_raw, NA_real_)
  )

# ---- Aggregate by decade ----
po_decade <- po %>%
  dplyr::group_by(Decade) %>%
  dplyr::summarise(
    mean_score   = mean(Score_PO, na.rm = TRUE),               # 0–4 mean (rescaled)
    completeness = 100 * mean(Completeness_PO, na.rm = TRUE),  # %
    .groups = "drop"
  ) %>%
  dplyr::mutate(MEL_Domain = "Project Objectives & Outcomes")







# ================================
# Build split-cell polygons (PC + SP + SC + ME + PO)
# ================================
dec_levels  <- c("1970s","1980s","1990s")
dec_present <- dec_levels[dec_levels %in% unique(c(
  pc_decade$Decade, sp_decade$Decade, sc_decade$Decade, me_decade$Decade, po_decade$Decade
))]

mk_tri <- function(row, which = c("top","bottom"), col = 1L) {
  which <- match.arg(which)
  x0 <- col - 0.5; x1 <- col + 0.5
  y0 <- row - 0.5; y1 <- row + 0.5
  if (which == "bottom") tibble(x = c(x0, x1, x1), y = c(y0, y0, y1))
  else                   tibble(x = c(x0, x0, x1), y = c(y0, y1, y1))
}

build_poly <- function(df_decade, col_id, label, dec_order) {
  df_decade %>%
    mutate(Decade = factor(Decade, levels = dec_order)) %>%
    arrange(Decade) %>%
    mutate(
      row        = as.integer(Decade),
      MEL_Domain = label,
      col_id     = col_id
    ) %>%
    rowwise() %>%
    mutate(
      poly_bottom = list(mk_tri(row, "bottom", col_id)),
      poly_top    = list(mk_tri(row, "top",    col_id))
    ) %>%
    ungroup()
}

pc_poly <- build_poly(pc_decade, 1L, "Permitting & Compliance",           dec_present)
sp_poly <- build_poly(sp_decade, 2L, "Stakeholder Participation",         dec_present)
sc_poly <- build_poly(sc_decade, 3L, "Socioeconomic & Climate Links",     dec_present)
me_poly <- build_poly(me_decade, 4L, "Monitoring & Evaluation",           dec_present)
po_poly <- build_poly(po_decade, 5L, "Project Objectives & Outcomes",     dec_present)

poly_all <- bind_rows(pc_poly, sp_poly, sc_poly, me_poly, po_poly)




# ================================
# Plot — grayscale completeness (dark→light), black text everywhere
# ================================
library(dplyr); library(tidyr); library(ggplot2); library(ggnewscale); library(grid)

pillar_labs <- c(
  "Permitting\n& Compliance",
  "Stakeholder\nParticipation",
  "Socioeconomic\n& Climate Links",
  "Monitoring\n& Evaluation",
  "Project Objectives\n& Outcomes"
)

p <- ggplot() +
  # --- Bottom triangles: Completeness (%) — dark gray (0) → light gray (100)
  geom_polygon(
    data = poly_all %>%
      select(MEL_Domain, Decade, completeness, poly_bottom) %>% unnest(poly_bottom),
    aes(x, y, group = interaction(MEL_Domain, Decade), fill = completeness),
    color = "grey15", linewidth = 0.5
  ) +
  scale_fill_gradient(
    name   = "Completeness",
    limits = c(0, 100),
    low    = "#f2f2f2",   # dark gray at 0
    high   = "#5F5F5C",   # light gray at 100
    guide = guide_colorbar(
      title.position = "top",
      barheight = unit(110, "pt"),
      barwidth  = unit(10,  "pt"),
      ticks = FALSE,
      frame.colour = NA
    )
  ) +
  ggnewscale::new_scale_fill() +
  # --- Top triangles: Mean Raw Score (0–4) — YlOrRd (low=yellow, high=red)
  geom_polygon(
    data = poly_all %>%
      select(MEL_Domain, Decade, mean_score, poly_top) %>% unnest(poly_top),
    aes(x, y, group = interaction(MEL_Domain, Decade), fill = mean_score),
    color = "grey15", linewidth = 0.5
  ) +
  scale_fill_distiller(
    name     = "Mean score",
    palette  = "YlOrRd",
    direction = -1,                # low = yellow, high = red
    limits   = c(0, 4),
    breaks   = 0:4,
    oob      = scales::squish,
    guide = guide_colorbar(
      title.position = "top",
      barheight = unit(110, "pt"),
      barwidth  = unit(10,  "pt"),
      ticks = FALSE,
      frame.colour = NA
    )
  ) +
  # --- In-cell labels (all black text)
  geom_text(
    data = poly_all,
    aes(x = col_id - 0.18, y = row + 0.18, label = sprintf("%.1f", mean_score)),
    size = 5.2, color = "black"
  ) +
  geom_text(
    data = poly_all,
    aes(x = col_id + 0.18, y = row - 0.18, label = paste0(round(completeness), "%")),
    size = 4.8, color = "black"
  ) +
  # --- Axes & theme
  scale_x_continuous(breaks = 1:5, labels = pillar_labs, expand = expansion(mult = 0.015)) +
  scale_y_continuous(breaks = seq_along(dec_present), labels = dec_present, expand = expansion(add = 0.15)) +
  coord_fixed(clip = "off") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid   = element_blank(),
    axis.line    = element_blank(),
    axis.ticks   = element_blank(),
    axis.text.x  = element_text(size = 14, margin = margin(t = 2), vjust = 1, color = "black"),
    axis.text.y  = element_text(size = 13, margin = margin(r = 2), color = "black"),
    legend.position   = "right",
    legend.title      = element_text(color = "black"),
    legend.text       = element_text(color = "black"),
    legend.background = element_blank(),
    legend.key        = element_blank(),
    plot.margin       = margin(4, 12, 4, 10)
  )

print(p)

