# CRR 2025 Data Visualization
# This R script is to visualize data summaries of the CRR Database for the 2025 Update

# Load prerequisites
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

# Load dataset
data2 <- read_excel("C:/Users/Kris Jypson Esturas/OneDrive - Macquarie University/Documents/2025/00 CRR Gov/R Analysis/Data/Municipalities_with_CRR_Projects_TableToExcel.xlsx")

# Step 1: Filter by FREQUENCY and clean/split Specific_method
data_cleaned_bio <- data2 %>%
  filter(!is.na(FREQUENCY), !is.na(Specific_method)) %>%
  mutate(Specific_method = trimws(Specific_method)) %>%
  separate_rows(Specific_method, sep = ",\\s*")  # Split multi-method entries

# Step 2: Count projects per Bioregion × Specific_method
pie_data_bio <- data_cleaned_bio %>%
  group_by(Bioregion, Specific_method) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(Bioregion, Specific_method, fill = list(count = 0)) %>%
  group_by(Bioregion) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()

# Count of total project entries per bioregion (including repeats across bioregions)
bio_totals <- data_cleaned_bio %>%
  group_by(Bioregion) %>%
  summarise(n = n(), .groups = "drop")


# Step 4: Merge totals and create label
pie_data_bio_labeled <- pie_data_bio %>%
  left_join(bio_totals, by = "Bioregion") %>%
  mutate(Bioregion_label = paste0(Bioregion, "\n(n = ", n, ")"))

# Step 5: Define consistent custom colours
custom_colors <- c(
  "Artificial Reef" = "#66c2a5",
  "Coral Gardening" = "#fc8d62",
  "Direct Transplantation" = "#8da0cb",
  "Mineral Accretion" = "#e78ac3",
  "Rubble Stabilisation" = "#a6d854",
  "Larval Enhancement" = "#ffd92f",
  "Microfragmentation" = "#e5c494",
  "Algal Removal" = "#b3b3b3",
  "Unknown" = "#f781bf"
)

# Step 6: Plot
ggplot(pie_data_bio_labeled, aes(x = "", y = prop, fill = Specific_method)) +
  geom_bar(stat = "identity", width = 1, color = "grey20") +
  coord_polar("y", start = 0) +
  facet_wrap(~ Bioregion_label, ncol = 4) +
  theme_void() +
  scale_fill_manual(values = custom_colors) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 10, face = "bold")
  ) +
  guides(fill = guide_legend(nrow = 9, byrow = TRUE)) +
  labs(title = "Restoration Techniques Used per Bioregion",
       fill = "Specific Method")

# Step 7: Save plot
ggsave("Output/Specific_Method_Bioregion.png", width = 6, height = 8, dpi = 720)

sort(unique(data2$Specific_method))
