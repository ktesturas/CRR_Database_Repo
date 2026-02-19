# Figure 1: Contextualising Restoration Records and Site Selection 
library(readxl)
library(dplyr)
library(forcats)
library(ggplot2)
library(scales)


file_path <- "C:/Users/Kris Jypson Esturas/OneDrive - Macquarie University/Documents/2025/00 CRR Gov/ArcGIS Projects/CRR 2025_June/Bioregions_Threats.xlsx"

threat_colors <- c(
  "Low" = "#FFFFB2",
  "Medium" = "#FECC5C",
  "High" = "#FD8D3C",
  "Very High" = "#E31A1C",
  "Critical" = "#800026"
)

#*******************************************************************************
# Integrated Local Threats
df <- read_xlsx(file_path, sheet = "Bioregions_IntThreats") 
str(df)

prop_area <- df %>%
  group_by(Bioregion, THREAT_TXT) %>%
  summarise(area = sum(Shape_Area, na.rm = TRUE), .groups = "drop_last") %>%
  mutate(prop = area / sum(area)) %>%
  ungroup() %>%
  group_by(Bioregion) %>%
  mutate(prop_vh = prop[THREAT_TXT == "Very High"]) %>%  # for sorting
  ungroup() %>%
  mutate(Bioregion = fct_reorder(Bioregion, prop_vh, .desc = TRUE))


###
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(scales)
library(cowplot)   # for placing the title on the right side

# 1) Threat levels (ordered)
df2 <- df %>%
  mutate(
    Threat = factor(THREAT_TXT,
                    levels = c("Low", "Medium", "High", "Very High"),
                    ordered = TRUE)
  )

# 2) Area-weighted composition per bioregion
prop_area <- df2 %>%
  group_by(Bioregion, Threat) %>%
  summarise(area = sum(Shape_Area, na.rm = TRUE), .groups = "drop") %>%
  complete(Bioregion, Threat, fill = list(area = 0)) %>%
  group_by(Bioregion) %>%
  mutate(
    prop    = area / sum(area),
    prop_vh = dplyr::coalesce(prop[Threat == "Very High"], 0)
  ) %>%
  ungroup()

# 3) FIXED Bioregion order (top-to-bottom in plot)
bioregion_order <- c(
  "North Philippine Sea Bioregion",
  "West Philippine Sea Bioregion",
  "Visayan Sea Bioregion",
  "South Philippine Sea Bioregion",
  "Sulu Sea Bioregion",
  "Celebes Sea Bioregion"
)


prop_area <- prop_area %>%
  mutate(Bioregion = factor(Bioregion, levels = rev(bioregion_order)))

# 4) Plot: horizontal stacked bars, legend bottom
library(cowplot)

# your core plot (legend bottom, horizontal bars, fixed order)
p_core <- ggplot(prop_area, aes(x = prop, y = Bioregion, fill = Threat)) +
  geom_col(width = 0.85) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_discrete(position = "right") +                     # 👉 move labels to right
  scale_fill_manual(values = threat_colors, drop = FALSE) +  # fixed palette
  labs(x = NULL, y = NULL, fill = "Threat level") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 10, 10, 10)  # breathing room
  )

p_core



#*******************************************************************************
# Coastal Development 
df <- read_xlsx(file_path, sheet = "Bioregions_CoastalDevt") 
str(df)

# Define the threat level categories explicitly
threat_levels <- c("Low", "Medium", "High", "Very High")

prop_area <- df %>%
  mutate(THREAT_TXT = factor(THREAT_TXT, levels = threat_levels, ordered = TRUE)) %>%
  group_by(Bioregion, THREAT_TXT) %>%
  summarise(area = sum(Shape_Area, na.rm = TRUE), .groups = "drop") %>%
  # ensure *every* bioregion has *every* threat level
  complete(Bioregion, THREAT_TXT, fill = list(area = 0)) %>%
  group_by(Bioregion) %>%
  mutate(
    total_area = sum(area),
    prop       = if_else(total_area > 0, area / total_area, 0),
    # sorter: share of "Very High" (0 if absent)
    prop_vh    = if_else("Very High" %in% THREAT_TXT,
                         prop[THREAT_TXT == "Very High"], 0)
  ) %>%
  ungroup() %>%
  mutate(Bioregion = fct_reorder(Bioregion, prop_vh, .desc = TRUE)) %>%
  select(-total_area)


###
library(dplyr)
library(ggplot2)
library(forcats)   # for fct_reorder
library(scales)    # for percent_format
library(tidyr)     # for complete()

# 1) Set an ordered factor for threat

###
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(scales)
library(cowplot)   # for placing the title on the right side

# 1) Threat levels (ordered)
df2 <- df %>%
  mutate(
    Threat = factor(THREAT_TXT,
                    levels = c("Low", "Medium", "High", "Very High"),
                    ordered = TRUE)
  )

# 2) Area-weighted composition per bioregion
prop_area <- df2 %>%
  group_by(Bioregion, Threat) %>%
  summarise(area = sum(Shape_Area, na.rm = TRUE), .groups = "drop") %>%
  complete(Bioregion, Threat, fill = list(area = 0)) %>%
  group_by(Bioregion) %>%
  mutate(
    prop    = area / sum(area),
    prop_vh = dplyr::coalesce(prop[Threat == "Very High"], 0)
  ) %>%
  ungroup()

# 3) FIXED Bioregion order (top-to-bottom in plot)
bioregion_order <- c(
  "North Philippine Sea Bioregion",
  "West Philippine Sea Bioregion",
  "Visayan Sea Bioregion",
  "South Philippine Sea Bioregion",
  "Sulu Sea Bioregion",
  "Celebes Sea Bioregion"
)


prop_area <- prop_area %>%
  mutate(Bioregion = factor(Bioregion, levels = rev(bioregion_order)))

# 4) Plot: horizontal stacked bars, legend bottom
library(cowplot)

# your core plot (legend bottom, horizontal bars, fixed order)
p_core <- ggplot(prop_area, aes(x = prop, y = Bioregion, fill = Threat)) +
  geom_col(width = 0.85) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_discrete(position = "right") +                     # 👉 move labels to right
  scale_fill_manual(values = threat_colors, drop = FALSE) +  # fixed palette
  labs(x = NULL, y = NULL, fill = "Threat level") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 10, 10, 10)  # breathing room
  )

p_core

#*******************************************************************************
# Marine Pollution
df <- read_xlsx(file_path, sheet = "Bioregions_MarinePollution") 
str(df)

# Define the threat level categories explicitly
threat_levels <- c("Low", "Medium", "High", "Very High")

prop_area <- df %>%
  mutate(THREAT_TXT = factor(THREAT_TXT, levels = threat_levels, ordered = TRUE)) %>%
  group_by(Bioregion, THREAT_TXT) %>%
  summarise(area = sum(Shape_Area, na.rm = TRUE), .groups = "drop") %>%
  # ensure *every* bioregion has *every* threat level
  complete(Bioregion, THREAT_TXT, fill = list(area = 0)) %>%
  group_by(Bioregion) %>%
  mutate(
    total_area = sum(area),
    prop       = if_else(total_area > 0, area / total_area, 0),
    # sorter: share of "Very High" (0 if absent)
    prop_vh    = if_else("Very High" %in% THREAT_TXT,
                         prop[THREAT_TXT == "Very High"], 0)
  ) %>%
  ungroup() %>%
  mutate(Bioregion = fct_reorder(Bioregion, prop_vh, .desc = TRUE)) %>%
  select(-total_area)


###

###
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(scales)
library(cowplot)   # for placing the title on the right side

# 1) Threat levels (ordered)
df2 <- df %>%
  mutate(
    Threat = factor(THREAT_TXT,
                    levels = c("Low", "Medium", "High", "Very High"),
                    ordered = TRUE)
  )

# 2) Area-weighted composition per bioregion
prop_area <- df2 %>%
  group_by(Bioregion, Threat) %>%
  summarise(area = sum(Shape_Area, na.rm = TRUE), .groups = "drop") %>%
  complete(Bioregion, Threat, fill = list(area = 0)) %>%
  group_by(Bioregion) %>%
  mutate(
    prop    = area / sum(area),
    prop_vh = dplyr::coalesce(prop[Threat == "Very High"], 0)
  ) %>%
  ungroup()

# 3) FIXED Bioregion order (top-to-bottom in plot)
bioregion_order <- c(
  "North Philippine Sea Bioregion",
  "West Philippine Sea Bioregion",
  "Visayan Sea Bioregion",
  "South Philippine Sea Bioregion",
  "Sulu Sea Bioregion",
  "Celebes Sea Bioregion"
)


prop_area <- prop_area %>%
  mutate(Bioregion = factor(Bioregion, levels = rev(bioregion_order)))

# 4) Plot: horizontal stacked bars, legend bottom
library(cowplot)

# your core plot (legend bottom, horizontal bars, fixed order)
p_core <- ggplot(prop_area, aes(x = prop, y = Bioregion, fill = Threat)) +
  geom_col(width = 0.85) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_discrete(position = "right") +                     # 👉 move labels to right
  scale_fill_manual(values = threat_colors, drop = FALSE) +  # fixed palette
  labs(x = NULL, y = NULL, fill = "Threat level") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 10, 10, 10)  # breathing room
  )

p_core




#*******************************************************************************
# Overfishing
df <- read_xlsx(file_path, sheet = "Bioregions_Overfishing") 
str(df)

# Define the threat level categories explicitly
threat_levels <- c("Low", "Medium", "High", "Very High")

prop_area <- df %>%
  mutate(THREAT_TXT = factor(THREAT_TXT, levels = threat_levels, ordered = TRUE)) %>%
  group_by(Bioregion, THREAT_TXT) %>%
  summarise(area = sum(Shape_Area, na.rm = TRUE), .groups = "drop") %>%
  # ensure *every* bioregion has *every* threat level
  complete(Bioregion, THREAT_TXT, fill = list(area = 0)) %>%
  group_by(Bioregion) %>%
  mutate(
    total_area = sum(area),
    prop       = if_else(total_area > 0, area / total_area, 0),
    # sorter: share of "Very High" (0 if absent)
    prop_vh    = if_else("Very High" %in% THREAT_TXT,
                         prop[THREAT_TXT == "Very High"], 0)
  ) %>%
  ungroup() %>%
  mutate(Bioregion = fct_reorder(Bioregion, prop_vh, .desc = TRUE)) %>%
  select(-total_area)


###
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(scales)
library(cowplot)   # for placing the title on the right side

# 1) Threat levels (ordered)
df2 <- df %>%
  mutate(
    Threat = factor(THREAT_TXT,
                    levels = c("Low", "Medium", "High", "Very High"),
                    ordered = TRUE)
  )

# 2) Area-weighted composition per bioregion
prop_area <- df2 %>%
  group_by(Bioregion, Threat) %>%
  summarise(area = sum(Shape_Area, na.rm = TRUE), .groups = "drop") %>%
  complete(Bioregion, Threat, fill = list(area = 0)) %>%
  group_by(Bioregion) %>%
  mutate(
    prop    = area / sum(area),
    prop_vh = dplyr::coalesce(prop[Threat == "Very High"], 0)
  ) %>%
  ungroup()

# 3) FIXED Bioregion order (top-to-bottom in plot)
bioregion_order <- c(
  "North Philippine Sea Bioregion",
  "West Philippine Sea Bioregion",
  "Visayan Sea Bioregion",
  "South Philippine Sea Bioregion",
  "Sulu Sea Bioregion",
  "Celebes Sea Bioregion"
)


prop_area <- prop_area %>%
  mutate(Bioregion = factor(Bioregion, levels = rev(bioregion_order)))

# 4) Plot: horizontal stacked bars, legend bottom
library(cowplot)

# your core plot (legend bottom, horizontal bars, fixed order)
p_core <- ggplot(prop_area, aes(x = prop, y = Bioregion, fill = Threat)) +
  geom_col(width = 0.85) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_discrete(position = "right") +                     # 👉 move labels to right
  scale_fill_manual(values = threat_colors, drop = FALSE) +  # fixed palette
  labs(x = NULL, y = NULL, fill = "Threat level") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 10, 10, 10)  # breathing room
  )

p_core





#*******************************************************************************
# Overfishing
df <- read_xlsx(file_path, sheet = "Bioregions_Overfishing") 
str(df)

# Define the threat level categories explicitly
threat_levels <- c("Low", "Medium", "High", "Very High")

prop_area <- df %>%
  mutate(THREAT_TXT = factor(THREAT_TXT, levels = threat_levels, ordered = TRUE)) %>%
  group_by(Bioregion, THREAT_TXT) %>%
  summarise(area = sum(Shape_Area, na.rm = TRUE), .groups = "drop") %>%
  # ensure *every* bioregion has *every* threat level
  complete(Bioregion, THREAT_TXT, fill = list(area = 0)) %>%
  group_by(Bioregion) %>%
  mutate(
    total_area = sum(area),
    prop       = if_else(total_area > 0, area / total_area, 0),
    # sorter: share of "Very High" (0 if absent)
    prop_vh    = if_else("Very High" %in% THREAT_TXT,
                         prop[THREAT_TXT == "Very High"], 0)
  ) %>%
  ungroup() %>%
  mutate(Bioregion = fct_reorder(Bioregion, prop_vh, .desc = TRUE)) %>%
  select(-total_area)


###
library(dplyr)
library(ggplot2)
library(forcats)   # for fct_reorder
library(scales)    # for percent_format
library(tidyr)     # for complete()

# 1) Set an ordered factor for threat
df2 <- df %>%
  mutate(
    Threat = factor(THREAT_TXT,
                    levels = c("Low", "Medium", "High", "Very High"),
                    ordered = TRUE)
  )

# 2) Area-weighted composition per bioregion
prop_area <- df2 %>%
  group_by(Bioregion, Threat) %>%
  summarise(area = sum(Shape_Area, na.rm = TRUE), .groups = "drop") %>%
  # ensure all threat levels appear for each bioregion (zeros if missing)
  complete(Bioregion, Threat, fill = list(area = 0)) %>%
  group_by(Bioregion) %>%
  mutate(
    prop    = area / sum(area),
    # safe sorter: 0 if a bioregion has no "Very High"
    prop_vh = dplyr::coalesce(prop[Threat == "Very High"], 0)
  ) %>%
  ungroup() %>%
  mutate(Bioregion = fct_reorder(Bioregion, prop_vh, .desc = TRUE))

# 3) Plot (100% stacked, sorted by 'Very High' share)
ggplot(prop_area, aes(x = Bioregion, y = prop, fill = Threat)) +
  geom_col(width = 0.85) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "YlOrRd", direction = 1) +
  labs(
    x = NULL, y = "Share of area", fill = "Threat level",
    title = "Overfishing"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#*******************************************************************************
# Bioregions_WatershedPollution
df <- read_xlsx(file_path, sheet = "Bioregions_WatershedPollution") 
str(df)

# Define the threat level categories explicitly
threat_levels <- c("Low", "Medium", "High", "Very High")

prop_area <- df %>%
  mutate(THREAT_TXT = factor(THREAT_TXT, levels = threat_levels, ordered = TRUE)) %>%
  group_by(Bioregion, THREAT_TXT) %>%
  summarise(area = sum(Shape_Area, na.rm = TRUE), .groups = "drop") %>%
  # ensure *every* bioregion has *every* threat level
  complete(Bioregion, THREAT_TXT, fill = list(area = 0)) %>%
  group_by(Bioregion) %>%
  mutate(
    total_area = sum(area),
    prop       = if_else(total_area > 0, area / total_area, 0),
    # sorter: share of "Very High" (0 if absent)
    prop_vh    = if_else("Very High" %in% THREAT_TXT,
                         prop[THREAT_TXT == "Very High"], 0)
  ) %>%
  ungroup() %>%
  mutate(Bioregion = fct_reorder(Bioregion, prop_vh, .desc = TRUE)) %>%
  select(-total_area)


###

###
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(scales)
library(cowplot)   # for placing the title on the right side

# 1) Threat levels (ordered)
df2 <- df %>%
  mutate(
    Threat = factor(THREAT_TXT,
                    levels = c("Low", "Medium", "High", "Very High"),
                    ordered = TRUE)
  )

# 2) Area-weighted composition per bioregion
prop_area <- df2 %>%
  group_by(Bioregion, Threat) %>%
  summarise(area = sum(Shape_Area, na.rm = TRUE), .groups = "drop") %>%
  complete(Bioregion, Threat, fill = list(area = 0)) %>%
  group_by(Bioregion) %>%
  mutate(
    prop    = area / sum(area),
    prop_vh = dplyr::coalesce(prop[Threat == "Very High"], 0)
  ) %>%
  ungroup()

# 3) FIXED Bioregion order (top-to-bottom in plot)
bioregion_order <- c(
  "North Philippine Sea Bioregion",
  "West Philippine Sea Bioregion",
  "Visayan Sea Bioregion",
  "South Philippine Sea Bioregion",
  "Sulu Sea Bioregion",
  "Celebes Sea Bioregion"
)


prop_area <- prop_area %>%
  mutate(Bioregion = factor(Bioregion, levels = rev(bioregion_order)))

# 4) Plot: horizontal stacked bars, legend bottom
library(cowplot)

# your core plot (legend bottom, horizontal bars, fixed order)
p_core <- ggplot(prop_area, aes(x = prop, y = Bioregion, fill = Threat)) +
  geom_col(width = 0.85) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_discrete(position = "right") +                     # 👉 move labels to right
  scale_fill_manual(values = threat_colors, drop = FALSE) +  # fixed palette
  labs(x = NULL, y = NULL, fill = "Threat level") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 10, 10, 10)  # breathing room
  )

p_core






#*******************************************************************************
# =========================
# Bioregions_2050_Int plot

df <- read_xlsx(file_path, sheet = "Bioregions_2050_Int") 
str(df)
# Define the threat level categories explicitly
threat_levels <- c("Medium", "High", "Very High", "Critical")  # no "Low" here

df2 <- df %>%
  mutate(
    Threat = factor(THREAT_TXT, levels = threat_levels, ordered = TRUE)
  )
# 2) Area-weighted composition per bioregion
prop_area <- df2 %>%
  group_by(Bioregion, Threat) %>%
  summarise(area = sum(Shape_Area, na.rm = TRUE), .groups = "drop") %>%
  complete(Bioregion, Threat, fill = list(area = 0)) %>%
  group_by(Bioregion) %>%
  mutate(
    prop    = area / sum(area),
    prop_vh = dplyr::coalesce(prop[Threat == "Critical"], 0)
  ) %>%
  ungroup()

# 3) FIXED Bioregion order (top-to-bottom in plot)
bioregion_order <- c(
  "North Philippine Sea Bioregion",
  "West Philippine Sea Bioregion",
  "Visayan Sea Bioregion",
  "South Philippine Sea Bioregion",
  "Sulu Sea Bioregion",
  "Celebes Sea Bioregion"
)


prop_area <- prop_area %>%
  mutate(Bioregion = factor(Bioregion, levels = rev(bioregion_order)))

# 4) Plot: horizontal stacked bars, legend bottom
library(cowplot)

# your core plot (legend bottom, horizontal bars, fixed order)
p_core <- ggplot(prop_area, aes(x = prop, y = Bioregion, fill = Threat)) +
  geom_col(width = 0.85) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_discrete(position = "right") +                     # 👉 move labels to right
  scale_fill_manual(values = threat_colors, drop = FALSE) +  # fixed palette
  labs(x = NULL, y = NULL, fill = "Threat level") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 10, 10, 10)  # breathing room
  )

p_core



#*******************************************************************************
# =========================
# Bioregions_2030_Int plot

df <- read_xlsx(file_path, sheet = "Bioregions_2030_Int") 
str(df)
# Define the threat level categories explicitly
threat_levels <- c("Medium", "High", "Very High", "Critical")  # no "Low" here

df2 <- df %>%
  mutate(
    Threat = factor(THREAT_TXT, levels = threat_levels, ordered = TRUE)
  )

# 2) Area-weighted composition per bioregion
prop_area <- df2 %>%
  group_by(Bioregion, Threat) %>%
  summarise(area = sum(Shape_Area, na.rm = TRUE), .groups = "drop") %>%
  complete(Bioregion, Threat, fill = list(area = 0)) %>%
  group_by(Bioregion) %>%
  mutate(
    prop    = area / sum(area),
    prop_vh = dplyr::coalesce(prop[Threat == "Critical"], 0)
  ) %>%
  ungroup()

# 3) FIXED Bioregion order (top-to-bottom in plot)
bioregion_order <- c(
  "North Philippine Sea Bioregion",
  "West Philippine Sea Bioregion",
  "Visayan Sea Bioregion",
  "South Philippine Sea Bioregion",
  "Sulu Sea Bioregion",
  "Celebes Sea Bioregion"
)


prop_area <- prop_area %>%
  mutate(Bioregion = factor(Bioregion, levels = rev(bioregion_order)))

# 4) Plot: horizontal stacked bars, legend bottom
library(cowplot)

# your core plot (legend bottom, horizontal bars, fixed order)
p_core <- ggplot(prop_area, aes(x = prop, y = Bioregion, fill = Threat)) +
  geom_col(width = 0.85) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_discrete(position = "right") +                     # 👉 move labels to right
  scale_fill_manual(values = threat_colors, drop = FALSE) +  # fixed palette
  labs(x = NULL, y = NULL, fill = "Threat level") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 10, 10, 10)  # breathing room
  )

p_core




#*******************************************************************************
# =========================
# Bioregions_Historical_Thermal_Stress

df <- read_xlsx(file_path, sheet = "Bioregion_PastThermalStress") 
str(df)
# Define the threat level categories explicitly
threat_levels <- c("Low", "Medium", "High", "Very High")  # no "Low" here

df2 <- df %>%
  mutate(
    Threat = factor(THREAT_TXT, levels = threat_levels, ordered = TRUE)
  )

# 2) Area-weighted composition per bioregion
prop_area <- df2 %>%
  group_by(Bioregion, Threat) %>%
  summarise(area = sum(Shape_Area, na.rm = TRUE), .groups = "drop") %>%
  complete(Bioregion, Threat, fill = list(area = 0)) %>%
  group_by(Bioregion) %>%
  mutate(
    prop    = area / sum(area),
    prop_vh = dplyr::coalesce(prop[Threat == "Very High"], 0)
  ) %>%
  ungroup()

# 3) FIXED Bioregion order (top-to-bottom in plot)
bioregion_order <- c(
  "North Philippine Sea Bioregion",
  "West Philippine Sea Bioregion",
  "Visayan Sea Bioregion",
  "South Philippine Sea Bioregion",
  "Sulu Sea Bioregion",
  "Celebes Sea Bioregion"
)


prop_area <- prop_area %>%
  mutate(Bioregion = factor(Bioregion, levels = rev(bioregion_order)))

# 4) Plot: horizontal stacked bars, legend bottom
library(cowplot)

# your core plot (legend bottom, horizontal bars, fixed order)
p_core <- ggplot(prop_area, aes(x = prop, y = Bioregion, fill = Threat)) +
  geom_col(width = 0.85) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_discrete(position = "right") +                     # 👉 move labels to right
  scale_fill_manual(values = threat_colors, drop = FALSE) +  # fixed palette
  labs(x = NULL, y = NULL, fill = "Threat level") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 10, 10, 10)  # breathing room
  )

p_core

