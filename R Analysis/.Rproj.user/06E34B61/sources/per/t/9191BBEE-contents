# CRR 2025 Data Visualization
# This R script is to visualize data summaries of the CRR Database for the 2025 Update

# Load Prerequisites
pacman::p_load(tidyverse, readxl, here)
data <- readxl::read_excel("C:/Users/Kris Jypson Esturas/OneDrive - Macquarie University/Documents/2025/00 CRR Gov/R Analysis/Data/20250422 CRR Governance Database 2023-2025.xlsx",
                           sheet = "CRR 2025")

library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Clean data (in case there are any multi-method entries, but Gen_method looks clean)
data_cleaned_gen <- data %>%
  filter(!is.na(Gen_method)) %>%
  mutate(Gen_method = trimws(Gen_method))

# Step 2: Count projects per Region × Gen_method
pie_data_gen <- data_cleaned_gen %>%
  group_by(Region, Gen_method) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(Region, Gen_method, fill = list(count = 0)) %>%
  group_by(Region) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()

# Step 3: Total number of distinct projects per region
region_totals_gen <- data_cleaned_gen %>%
  group_by(Region) %>%
  summarise(n = n_distinct(Unique_key), .groups = "drop")

# Step 4: Merge totals for facet labels
pie_data_gen_labeled <- pie_data_gen %>%
  left_join(region_totals_gen, by = "Region") %>%
  mutate(Region_label = paste0(Region, "\n(n = ", n, ")"))

# Step 5: Optional custom colours
custom_colors_gen <- c(
  "Substrate Enhancement" = "#66c2a5",
  "Asexual Propagation" = "#fc8d62",
  "Sexual Propagation" = "#8da0cb",
  "Unknown" = "#b3b3b3"
)

# Step 6: Plot
ggplot(pie_data_gen_labeled, aes(x = "", y = prop, fill = Gen_method)) +
  geom_bar(stat = "identity", width = 1, color = "grey20") +  # black outlines
  coord_polar("y") +
  facet_wrap(~ Region_label, ncol = 4) +
  theme_void() +
  scale_fill_manual(values = custom_colors_gen) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    strip.text = element_text(size = 10, face = "bold")
  ) +
  labs(title = "Restoration Intervention Types by Region",
       fill = "General Method")

##############
library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Clean and split multiple methods into separate rows
data_cleaned <- data %>%
  filter(!is.na(Specific_method)) %>%
  mutate(Specific_method = trimws(Specific_method)) %>%
  separate_rows(Specific_method, sep = ",\\s*")  # Split by comma + optional space

# Step 2: Count projects per Region × Specific_method
pie_data <- data_cleaned %>%
  group_by(Region, Specific_method) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(Region, Specific_method, fill = list(count = 0)) %>%
  group_by(Region) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()

# Step 3: Add total number of distinct projects (not methods) per region
region_totals <- data_cleaned %>%
  group_by(Region) %>%
  summarise(n = n_distinct(Unique_key), .groups = "drop")

# Step 4: Merge totals and create a new Region label
pie_data_labeled <- pie_data %>%
  left_join(region_totals, by = "Region") %>%
  mutate(Region_label = paste0(Region, "\n(n = ", n, ")"))

# Step 5: Define a clean custom color palette
custom_colors <- c(
  "Artificial reef" = "#66c2a5",
  "Coral gardening" = "#fc8d62",
  "Direct transplantation" = "#8da0cb",
  "Mineral accretion" = "#e78ac3",
  "Substrate stabilisation" = "#a6d854",
  "Larval enhancement" = "#ffd92f",
  "Micro-fragmentation" = "#e5c494",
  "Algae Removal" = "#b3b3b3",
  "Unknown" = "#f781bf",
  "Coral relocation" = "#cab2d6",
  "Reef balls" = "#ff7f00",
  "Other" = "#999999"
)

# Step 6: Plot with outlines and labels


ggplot(pie_data_labeled, aes(x = "", y = prop, fill = Specific_method)) +
  geom_bar(stat = "identity", width = 1)+ #color = "white", linewidth = 0.1) + # Change width to 1
  coord_polar("y", start = 0) +
  facet_wrap(~ Region_label, ncol = 4) +
  theme_void() +
  scale_fill_manual(values = custom_colors) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 10, face = "bold")
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  labs(title = "Restoration Techniques Used per Region",
       fill = "Specific Method")

# Save Plot
ggsave("Output/Specific_Method_Region.png", width = 6, height = 8, dpi = 720)



################################################
# Dummy data: one full circle per region for the outer border
border_data <- pie_data_labeled %>%
  dplyr::distinct(Region_label) %>%
  dplyr::mutate(x = "", y = 1, fill = NA)  # one full slice per pie

ggplot() +
  # FULL BORDER CIRCLE per pie
  geom_bar(data = border_data, 
           aes(x = x, y = y), 
           stat = "identity", 
           width = 1, 
           fill = NA, 
           color = "black", 
           linewidth = 0.7) +
  
  # ACTUAL PIE
  geom_bar(data = pie_data_labeled, 
           aes(x = "", y = prop, fill = Specific_method), 
           stat = "identity", 
           width = 1, 
           color = "black", 
           linewidth = 0.2) +
  
  coord_polar("y") +
  facet_wrap(~ Region_label, ncol = 4) +
  theme_void() +
  scale_fill_manual(values = custom_colors) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  labs(title = "Restoration Techniques Used per Region",
       fill = "Specific Method")


#######################
# Top 10 Municipalities
library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Clean and split Gen_method entries
data_cleaned_gen <- data %>%
  filter(!is.na(Gen_method)) %>%
  mutate(Gen_method = trimws(Gen_method)) %>%
  separate_rows(Gen_method, sep = ",\\s*")

# Step 2: Get top 10 municipalities by total project count
top10_municipalities <- data_cleaned_gen %>%
  count(Municipality, name = "TotalProjects") %>%
  arrange(desc(TotalProjects)) %>%
  slice_head(n = 10)

# Step 3: Filter data for only those top municipalities
top_municipalities_stacked <- data_cleaned_gen %>%
  filter(Municipality %in% top10_municipalities$Municipality) %>%
  count(Municipality, Gen_method, name = "ProjectCount") %>%
  left_join(top10_municipalities, by = "Municipality")

# Step 3: Define consistent color palette for Gen_method
custom_colors_gen <- c(
  "Substrate Enhancement" = "#66c2a5",
  "Asexual Propagation" = "#fc8d62",
  "Sexual Propagation" = "#8da0cb",
  "Unknown" = "#b3b3b3"
)

# Step 4: Plot
ggplot(top_municipalities_stacked, aes(x = reorder(Municipality, TotalProjects), y = ProjectCount, fill = Gen_method)) +
  geom_col(color = "black", width = 0.8) +  # adds black outlines between segments
  coord_flip() +
  scale_fill_manual(values = custom_colors_gen) +
  labs(x = "Municipality",
       y = "Number of Projects",
       fill = "General Method",
       title = "Top 10 Most Active Municipalities in Coral Reef Restoration") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    axis.text.y = element_text(size = 11, face = "bold"),
    panel.grid.major.y = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2))

##############################################
library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Clean and split Specific_method entries
data_cleaned_specific <- data %>%
  filter(!is.na(Specific_method)) %>%
  mutate(Specific_method = trimws(Specific_method)) %>%
  separate_rows(Specific_method, sep = ",\\s*")

# Step 2: Identify top 10 municipalities by number of distinct projects
top10_municipalities <- data_cleaned_specific %>%
  count(Municipality, name = "TotalProjects") %>%
  arrange(desc(TotalProjects)) %>%
  slice_head(n = 10)

# Step 3: Filter and count by Specific_method
top_municipalities_stacked <- data_cleaned_specific %>%
  filter(Municipality %in% top10_municipalities$Municipality) %>%
  count(Municipality, Specific_method, name = "ProjectCount") %>%
  left_join(top10_municipalities, by = "Municipality")

# Step 4: Custom colour palette (expand as needed)
custom_colors_specific <- c(
  "Artificial reef" = "#66c2a5",
  "Coral gardening" = "#fc8d62",
  "Direct transplantation" = "#8da0cb",
  "Mineral accretion" = "#e78ac3",
  "Substrate stabilisation" = "#a6d854",
  "Larval enhancement" = "#ffd92f",
  "Micro-fragmentation" = "#e5c494",
  "Algae Removal" = "#b3b3b3",
  "Unknown" = "#f781bf",
  "Coral relocation" = "#cab2d6",
  "Reef balls" = "#ff7f00",
  "Other" = "#999999"
)

# Step 5: Plot
ggplot(top_municipalities_stacked, aes(x = reorder(Municipality, TotalProjects), y = ProjectCount, fill = Specific_method)) +
  geom_col(color = "black", width = 0.8) +
  coord_flip() +
  scale_fill_manual(values = custom_colors_specific) +
  labs(
    x = "Municipality",
    y = "Number of Projects",
    fill = "Specific Method",
    title = "Top 10 Most Active Municipalities"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 13),
    panel.grid.major.y = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 3))

ggsave("Output/Specific_Method_Top10.png", width = 10, height = 8, dpi = 720)






#############################################################################
# Specific Method with Time

library(tidyverse)

# 1. Clean and split multiple methods properly using separate_rows
data_cleaned <- data %>%
  mutate(Specific_method = str_replace_all(Specific_method, ",", ";")) %>%
  separate_rows(Specific_method, sep = ";\\s*")

# 2. Separate known and unknown year entries
data_known <- data_cleaned %>%
  filter(!is.na(Year_estab)) %>%
  mutate(Year_estab = as.numeric(Year_estab),
         Year_estab = as.character(Year_estab))  # Convert to character for plotting

data_unknown <- data_cleaned %>%
  filter(is.na(Year_estab)) %>%
  mutate(Year_estab = "Unknown")

# 3. Combine known and unknown data, using only actual year levels
all_years <- sort(unique(na.omit(as.numeric(data_known$Year_estab))))
combined_data <- bind_rows(data_known, data_unknown) %>%
  mutate(Year_estab = factor(Year_estab, levels = c(as.character(all_years), "Unknown")))

# 4. Count number of projects per year and method
project_counts <- combined_data %>%
  count(Year_estab, Specific_method, name = "Count")

# 5. Total count per method for legend
method_totals <- project_counts %>%
  group_by(Specific_method) %>%
  summarise(Total_Count = sum(Count), .groups = "drop")

# 6. Add formatted legend label and clean method name
project_counts <- project_counts %>%
  left_join(method_totals, by = "Specific_method") %>%
  mutate(
    Legend_Label = paste0(Specific_method, " (", Total_Count, ")"),
    Method_clean = Specific_method  # For custom color key
  )

# 7. Define your custom color scheme
custom_colors <- c(
  "Artificial reef" = "#66C2A5",
  "Coral gardening" = "#FC8D62",
  "Direct transplantation" = "#8DA0CB",
  "Larval enhancement" = "#FFD92F",
  "Mineral accretion" = "#E78AC3",
  "Algae Removal" = "#BEBEBE"
)

# 8. Plot: Stacked bar chart with custom fill
ggplot(project_counts, aes(x = Year_estab, y = Count, fill = Method_clean)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.2) +
  scale_fill_manual(values = custom_colors, name = "Specific Method") +
  scale_x_discrete(
    labels = function(x) {
      # Show only even years; keep "Unknown"
      ifelse(x == "Unknown", "Unknown",
             ifelse(as.numeric(x) %% 2 == 0, x, ""))
    }
  ) +
  labs(x = "Year", y = "Projects (count)") +
  theme_minimal(base_size = 10) +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )



# Save Plot


###################################################################
# Plots with Time but continuous
library(tidyverse)

# 1. Clean and split multiple methods
data_cleaned <- data %>%
  mutate(Specific_method = str_replace_all(Specific_method, ",", ";")) %>%
  mutate(Specific_method = strsplit(as.character(Specific_method), ";\\s*")) %>%
  unnest(Specific_method)

# 2. Separate known and unknown years
data_known <- data_cleaned %>%
  filter(!is.na(Year_estab)) %>%
  mutate(Year_estab = as.numeric(Year_estab),
         Year_estab = as.character(Year_estab))

data_unknown <- data_cleaned %>%
  filter(is.na(Year_estab)) %>%
  mutate(Year_estab = "Unknown")

# 3. Combine known + unknown
combined_data <- bind_rows(data_known, data_unknown) %>%
  mutate(Year_estab = factor(
    Year_estab,
    levels = c(as.character(seq(min(as.numeric(data_known$Year_estab)), 2025)), "Unknown")
  ))

# 4. Create full grid for complete year-method combinations
all_years <- as.character(seq(min(as.numeric(data_known$Year_estab)), 2025))
all_methods <- unique(data_cleaned$Specific_method)

full_grid <- expand_grid(
  Year_estab = all_years,
  Specific_method = all_methods
)

# 5. Count occurrences and calculate cumulative sums
counts_raw <- combined_data %>%
  count(Year_estab, Specific_method, name = "Count")

project_counts <- full_grid %>%
  left_join(counts_raw, by = c("Year_estab", "Specific_method")) %>%
  mutate(Count = replace_na(Count, 0),
         Year_order = as.numeric(Year_estab)) %>%
  arrange(Specific_method, Year_order) %>%
  group_by(Specific_method) %>%
  mutate(Cumulative_Count = cumsum(Count)) %>%
  ungroup()

# 6. Add unknown years back (if any)
unknown_counts <- combined_data %>%
  filter(Year_estab == "Unknown") %>%
  count(Year_estab, Specific_method, name = "Cumulative_Count")

project_counts <- bind_rows(project_counts, unknown_counts)

# 7. Compute total counts for legend
method_totals <- project_counts %>%
  group_by(Specific_method) %>%
  summarise(Total_Count = max(Cumulative_Count), .groups = "drop")

project_counts <- project_counts %>%
  left_join(method_totals, by = "Specific_method") %>%
  mutate(
    Legend_Label = paste0(Specific_method, " (", Total_Count, ")"),
    Method_clean = Specific_method  # this will match with custom colors
  )

# 8. Define custom colors for key methods only
custom_colors <- c(
  "Artificial reef" = "#66C2A5",
  "Coral gardening" = "#FC8D62",
  "Direct transplantation" = "#8DA0CB",
  "Larval enhancement" = "#FFD92F",
  "Mineral accretion" = "#E78AC3",
  "Algae Removal" = "#BEBEBE"
)

# 9. Plot with color mapped to cleaned method name
ggplot(project_counts, aes(x = Year_estab, y = Cumulative_Count, fill = Method_clean)) +
  geom_col(color = "black", linewidth = 0.2) +
  scale_fill_manual(values = custom_colors, name = "Specific Method") +
  labs(x = "Year", y = "Cumulative Projects (count)") +
  theme_minimal(base_size = 10) +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

# ggsave("Output/Cumulative_Counts_Specific_Method_2025.png", width = 10, height = 6, dpi = 720)

######################################################
# Pie charts per era

library(tidyverse)

# 1. Clean and split multiple methods
data_cleaned <- data %>%
  mutate(Specific_method = str_replace_all(Specific_method, ",", ";")) %>%
  mutate(Specific_method = strsplit(as.character(Specific_method), ";\\s*")) %>%
  unnest(Specific_method)

# 2. Filter only rows with known years
data_known <- data_cleaned %>%
  filter(!is.na(Year_estab)) %>%
  mutate(Year_estab = as.numeric(Year_estab))

# 3. Create 'Decade' or 'Era' variable
data_known <- data_known %>%
  mutate(Era = case_when(
    Year_estab < 1980 ~ "1970s",
    Year_estab < 1990 ~ "1980s",
    Year_estab < 2000 ~ "1990s",
    Year_estab < 2010 ~ "2000s",
    Year_estab < 2020 ~ "2010s",
    Year_estab >= 2020 ~ "2020s"
  )) %>%
  mutate(Era = factor(Era, levels = c("1970s", "1980s", "1990s", "2000s", "2010s", "2020s")))

# 4. Count projects per method per era
pie_data <- data_known %>%
  count(Era, Specific_method, name = "Count")

# 5. Compute proportions per pie
pie_data <- pie_data %>%
  group_by(Era) %>%
  mutate(prop = Count / sum(Count)) %>%
  ungroup()

# 6. Define custom colors
custom_colors <- c(
  "Artificial reef" = "#66C2A5",
  "Coral gardening" = "#FC8D62",
  "Direct transplantation" = "#8DA0CB",
  "Larval enhancement" = "#FFD92F",
  "Mineral accretion" = "#E78AC3",
  "Algae Removal" = "#BEBEBE"
)

# 7. Plot: Pie charts by era
pie_data_clean <- pie_data %>% filter(!is.na(Era))

ggplot(pie_data_clean, aes(x = "", y = prop, fill = Specific_method)) +
  geom_bar(stat = "identity", width = 1, color = "grey20") +
  coord_polar("y") +
  facet_wrap(~ Era, ncol = 6) +
  scale_fill_manual(values = custom_colors) +
  theme_void() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 16, face = "bold", family = "Arial"),   # Facet labels
    plot.title = element_text(size = 16, hjust = 0.5, family = "Arial"),     # Title (optional)
    text = element_text(size = 12, family = "Arial")                         # General text
  )




#########################################################
# Plot of % Records that use a particular type of restoration technology

data <- readxl::read_excel("C:/Users/Kris Jypson Esturas/OneDrive - Macquarie University/Documents/2025/00 CRR Gov/R Analysis/Data/20250609 CRR Governance Database 2023-2025.xlsx",
                           sheet = "CRR 2025")


library(tidyverse)

# 1. Replace "Not applicable" in AR_material_type with values based on Specific_method
data_material <- data %>%
  mutate(AR_material_type = case_when(
    AR_material_type == "Not applicable" & Specific_method == "Direct transplantation" ~ "Transplantation",
    AR_material_type == "Not applicable" & Specific_method == "Coral gardening" ~ "CNU",
    AR_material_type == "Not applicable" & Specific_method == "Larval enhancement" ~ "Larval Enhancement",
    TRUE ~ AR_material_type
  ))

# 2. Split multiple materials separated by comma
data_material <- data_material %>%
  filter(!is.na(AR_material_type)) %>%
  mutate(AR_material_type = str_replace_all(AR_material_type, ",", ";")) %>%
  mutate(AR_material_type = strsplit(AR_material_type, ";\\s*")) %>%
  unnest(AR_material_type)

# 3. Filter valid years and convert to character for plotting
data_material <- data_material %>%
  filter(!is.na(Year_estab)) %>%
  mutate(Year_estab = as.character(as.numeric(Year_estab)))

# 4. Count per year-material combination and calculate proportion
plot_data <- data_material %>%
  count(Year_estab, AR_material_type, name = "Count") %>%
  group_by(Year_estab) %>%
  mutate(prop = Count / sum(Count)) %>%
  ungroup()

# 5. Define color-blind friendly color palette
material_colors <- c(
  "CNU" = "#FDB863",
  "Concrete" = "#999999",              # GREY: changed for better contrast with Hybrid
  "Engineered/Advanced" = "#5E3C99",
  "Hybrid" = "#66C2A5",                # TEAL: very distinct from grey
  "Larval Enhancement" = "#1F78B4",
  "Metal/Wreck" = "#A6CEE3",
  "Natural" = "#8DD3C7",
  "Not applicable" = "#D3D3D3",
  "Recycled/Waste" = "#E31A1C",
  "Tires" = "#333333",
  "Transplantation" = "#FB8072",
  "Unknown" = "#BC80BD"
)

# 6. Plot
ggplot(plot_data, aes(x = Year_estab, y = prop, fill = AR_material_type)) +
  geom_bar(stat = "identity", position = "fill", color = "white", linewidth = 0.2) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = material_colors, name = "Material Type") +
  labs(x = "Year", y = "Records (%)") +
  theme_minimal(base_size = 10) +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

###########
# Pie charts per era per artificial reeflibrary(tidyverse)

# 1. Filter only Artificial Reef projects (including mixed methods)
data_reef <- data %>%
  filter(str_detect(Specific_method, "(?i)Artificial reef")) %>%  # (?i) makes it case-insensitive
  filter(!is.na(Year_estab)) %>%
  mutate(Year_estab = as.numeric(Year_estab))


# 2. Assign Era
data_reef <- data_reef %>%
  mutate(Era = case_when(
    Year_estab < 1980 ~ "1970s",
    Year_estab < 1990 ~ "1980s",
    Year_estab < 2000 ~ "1990s",
    Year_estab < 2010 ~ "2000s",
    Year_estab < 2020 ~ "2010s",
    Year_estab >= 2020 ~ "2020s"
  )) %>%
  mutate(Era = factor(Era, levels = c("1970s", "1980s", "1990s", "2000s", "2010s", "2020s")))

# 3. Clean and split AR materials (handle multiple values)
data_reef_materials <- data_reef %>%
  filter(!is.na(AR_material_type)) %>%
  mutate(AR_material_type = str_replace_all(AR_material_type, ",", ";")) %>%
  mutate(AR_material_type = strsplit(AR_material_type, ";\\s*")) %>%
  unnest(AR_material_type)

# 4. Count materials per era
pie_data <- data_reef_materials %>%
  count(Era, AR_material_type, name = "Count") %>%
  group_by(Era) %>%
  mutate(prop = Count / sum(Count)) %>%
  ungroup()

# 5. Define material colors (green tones or refined palette)
material_colors <- c(
  "Concrete" = "#E69F00",            # orange-yellow
  "Tires" = "#56B4E9",               # sky blue
  "Natural" = "#009E73",            # bluish green
  "Recycled/Waste" = "#0072B2",     # blue
  "Metal/Wreck" = "#F0E442",        # yellow
  "Hybrid" = "#D55E00",             # reddish orange
  "Engineered/Advanced" = "#CC79A7",# purple/magenta
  "Unknown" = "#999999",            # medium gray
  "Not applicable" = "#CCCCCC",     # light gray
  "Other" = "#666666"               # dark gray
)


ggplot(pie_data, aes(x = "", y = prop, fill = AR_material_type)) +
  geom_bar(stat = "identity", width = 1, color = "grey20") +
  coord_polar("y") +
  facet_wrap(~ Era, nrow = 1) +  # use nrow = 1 instead of ncol
  scale_fill_manual(values = material_colors) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 10, face = "bold")
  )



##########################################################################################
#### PIE CHARTS BY ERA PER AR_TYPE
library(tidyverse)

# 1. Filter rows where "Artificial Reef" is in Specific_method
data_ar <- data %>%
  filter(str_detect(Specific_method, "Artificial Reef")) %>%
  mutate(Specific_method = str_replace_all(Specific_method, ",", ";")) %>%
  mutate(Specific_method = strsplit(Specific_method, ";\\s*")) %>%
  unnest(Specific_method) %>%
  filter(Specific_method == "Artificial Reef")  # Keep only the AR part after splitting

# 2. Filter valid years and convert to numeric
data_ar <- data_ar %>%
  filter(!is.na(Year_estab)) %>%
  mutate(Year_estab = as.numeric(Year_estab))

# 3. Assign Era based on Year_estab
data_ar <- data_ar %>%
  mutate(Era = case_when(
    Year_estab < 1980 ~ "1970s",
    Year_estab < 1990 ~ "1980s",
    Year_estab < 2000 ~ "1990s",
    Year_estab < 2010 ~ "2000s",
    Year_estab < 2020 ~ "2010s",
    Year_estab >= 2020 ~ "2020s",
    TRUE ~ "NA"
  )) %>%
  mutate(Era = factor(Era, levels = c("1970s", "1980s", "1990s", "2000s", "2010s", "2020s", "NA")))

# 4. Split AR_material_type into multiple rows
data_ar_materials <- data_ar %>%
  filter(!is.na(AR_material_type)) %>%
  mutate(AR_material_type = str_replace_all(AR_material_type, ",", ";")) %>%
  mutate(AR_material_type = strsplit(AR_material_type, ";\\s*")) %>%
  unnest(AR_material_type)

# 5. Count materials per era and calculate proportions
pie_data <- data_ar_materials %>%
  count(Era, AR_material_type, name = "Count") %>%
  group_by(Era) %>%
  mutate(prop = Count / sum(Count)) %>%
  ungroup()

# 6. Define color-blind friendly palette (Okabe & Ito)
material_colors <- c(
  "Concrete" = "#E69F00",            # orange
  "Engineered/Advanced" = "#56B4E9", # sky blue
  "Hybrid" = "#009E73",              # green
  "Metal/Wreck" = "#F0E442",         # yellow
  "Natural" = "#0072B2",             # blue
  "Recycled/Waste" = "#D55E00",      # red-orange
  "Tires" = "#CC79A7",               # pink
  "Unknown" = "#999999"              # grey
)

# 7. Plot pie charts by era
ggplot(pie_data, aes(x = "", y = prop, fill = AR_material_type)) +
  geom_bar(stat = "identity", width = 1, color = "black", linewidth = 0.3) +
  coord_polar("y") +
  facet_wrap(~ Era, ncol = 6) +
  scale_fill_manual(values = material_colors, name = "AR Material Type") +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 11, face = "bold")
  )


############################################################################
### PROJECT DURATION
library(tidyverse)

data <- readxl::read_excel("C:/Users/Kris Jypson Esturas/OneDrive - Macquarie University/Documents/2025/00 CRR Gov/R Analysis/Data/20250616 CRR Governance Database 2023-2025.xlsx",
                           sheet = "CRR 2025")

# Step 1: Prepare your data
df <- data %>%
  filter(Project_start != "Unknown", Project_end != "Unknown") %>%
  mutate(
    Project_start = as.integer(Project_start),
    Project_end = as.integer(Project_end)
  )

# Step 2: Split multiple methods and clean them
df_cleaned <- df %>%
  mutate(Specific_method = str_replace_all(Specific_method, ",", ";")) %>%
  mutate(Specific_method = strsplit(Specific_method, ";\\s*")) %>%
  unnest(Specific_method) %>%
  mutate(Specific_method = str_to_title(str_trim(Specific_method)))  # "Direct Transplantation"

# Step 3: Recode into consistent clean categories
df_cleaned <- df_cleaned %>%
  mutate(Method_clean = case_when(
    str_detect(Specific_method, "Artificial Reef") ~ "Artificial Reef",
    str_detect(Specific_method, "Coral Gardening") ~ "Coral Gardening",
    str_detect(Specific_method, "Direct Transplantation") ~ "Direct Transplantation",
    str_detect(Specific_method, "Larval Enhancement") ~ "Larval Enhancement",
    str_detect(Specific_method, "Mineral Accretion") ~ "Mineral Accretion",
    str_detect(Specific_method, "Substrate Stabilisation") ~ "Substrate Stabilisation",
    str_detect(Specific_method, "Microfragmentation") ~ "Microfragmentation",
    str_detect(Specific_method, "Algal Removal") ~ "Algae Removal",
    TRUE ~ "Other/Unknown"
  ))

# Step 4: Order project titles by earliest start (reversed for top-down)
df_cleaned <- df_cleaned %>%
  group_by(Restoration_project_title) %>%
  mutate(Earliest_start = min(Project_start, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Earliest_start) %>%
  mutate(Restoration_project_title = factor(Restoration_project_title, levels = rev(unique(Restoration_project_title))))

# Step 5: Define colour-blind friendly palette
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

# Step 6: Plot timeline with colour by method
# Filter out 'Unknown' methods before plotting
df_filtered <- df_cleaned %>% 
  filter(Restoration_project_title != "Unknown")

library(dplyr)
library(stringr)


df_filtered <- df_filtered %>%
  rowwise() %>%
  mutate(
    Restoration_project_title = {
      words <- str_split(Restoration_project_title, "\\s+")[[1]]
      if (length(words) > 10) {
        paste(c(words[1:10], "…"), collapse = " ")
      } else {
        Restoration_project_title
      }
    }
  ) %>%
  ungroup() %>%
  arrange(Project_start) %>%  # sort by earliest start date
  mutate(
    Restoration_project_title = factor(Restoration_project_title, 
                                       levels = rev(unique(Restoration_project_title)))
  )
ggplot(df_filtered, aes(
  y = Restoration_project_title,
  x = Project_start,
  xend = Project_end,
  color = Method_clean
)) +
  geom_segment(aes(yend = Restoration_project_title), size = 3.5) +  # thicker bars
  geom_point(
    data = df_filtered %>% filter(Project_start == Project_end),
    aes(x = Project_start, y = Restoration_project_title),
    size = 3
  ) +
  scale_color_manual(values = method_colors, name = "Methodology") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 9, color = "black"),
    axis.text.x = element_text(size = 16, vjust = 0.5, color = "black"),
    legend.position = "bottom",
    legend.text = element_text(size = 14),      # Increase legend label size
    legend.title = element_text(size = 15),     # Optional: increase title size too
    panel.grid.minor = element_blank()
  )

# Save Plot
ggsave("Output/Project_Durations.png", width = 10, height = 8.5, dpi = 720)



######################################################################
# Source Material

# Load packages
pacman::p_load(readxl, dplyr, ggplot2, forcats)

# Read the Excel file
data <- read_excel("C:/Users/Kris Jypson Esturas/OneDrive - Macquarie University/Documents/2025/00 CRR Gov/R Analysis/Data/20250609 CRR Governance Database 2023-2025.xlsx",
                   sheet = "CRR 2025")

# Count exact Source_type values
source_summary <- data %>%
  count(Source_type) %>%
  arrange(n) %>%
  mutate(Source_type = fct_inorder(Source_type))  # maintain order

# Plot
ggplot(source_summary, aes(x = n, y = fct_reorder(Source_type, n), fill = Source_type)) +
  geom_col() +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(face = "bold"),
    axis.text.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )


########################
# === 2. Summarise projects and unique references ===
plot_data <- data %>%
  filter(!is.na(Source_type), !is.na(Literature_type), !is.na(Reference)) %>%
  group_by(Literature_type, Source_type) %>%
  summarise(
    Projects = n(),
    References = n_distinct(Reference),
    .groups = "drop"
  )

# === 3. Reorder Literature_type so Grey is at the top ===
plot_data$Literature_type <- factor(plot_data$Literature_type,
                                    levels = rev(c("Grey literature", "Peer-reviewed", "Survey")))

# === 4. Create color palette ===
n_colors <- length(unique(plot_data$Source_type))
palette <- RColorBrewer::brewer.pal(n = min(n_colors, 12), name = "Set3")

# === 5. Plot: Projects (solid stacked bar) ===
plot_projects <- ggplot(plot_data, aes(x = Projects, y = Literature_type, fill = Source_type)) +
  geom_col(position = "stack", colour = "grey20", linewidth = 0.3) +
  scale_fill_manual(values = palette) +
  labs(x = "Number of Sources", y = NULL, fill = "Source Type") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(face = "bold", size = 11),
    axis.text.x = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )


# Load packages
pacman::p_load(readxl, dplyr, ggplot2, forcats)


# 
# Count by Literature_type and Source_type
plot_data <- data %>%
  count(Literature_type, Source_type) %>%
  filter(!is.na(Literature_type) & !is.na(Source_type)) %>%
  mutate(
    Literature_type = fct_inorder(Literature_type),
    Source_type = fct_reorder(Source_type, n, .fun = sum)  # for legend ordering
  )

# Plot: Stacked horizontal bar chart
ggplot(plot_data, aes(x = n, y = Literature_type, fill = Source_type)) +
  geom_col(position = "stack") +
  labs(
    x = "Number of Sources",
    y = NULL,
    fill = "Source Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    panel.grid.major.y = element_blank()
  )


####
# === 2. Summarise projects and unique references ===
plot_data <- data %>%
  filter(!is.na(Source_type), !is.na(Literature_type), !is.na(Reference)) %>%
  group_by(Literature_type, Source_type) %>%
  summarise(
    Projects = n(),
    References = n_distinct(Reference),
    .groups = "drop"
  )

# === 3. Reorder Literature_type so Grey is at the top ===
plot_data$Literature_type <- factor(plot_data$Literature_type,
                                    levels = rev(c("Grey literature", "Peer-reviewed", "Survey")))

# === 4. Create color palette ===
n_colors <- length(unique(plot_data$Source_type))
palette <- RColorBrewer::brewer.pal(n = min(n_colors, 12), name = "Set3")

# === 5. Plot: Projects (solid stacked bar) ===
plot_projects <- ggplot(plot_data, aes(x = Projects, y = Literature_type, fill = Source_type)) +
  geom_col(position = "stack", colour = "grey20", linewidth = 0.3) +
  scale_fill_manual(values = palette) +
  labs(x = "Number of Sources", y = NULL, fill = "Source Type") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(face = "bold", size = 11),
    axis.text.x = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

plot_projects

# === 6. Plot: References (hatched stacked bar) ===
# === 5. Plot: References (with transparent fill for texture) ===
plot_references <- ggplot(plot_data, aes(x = References, y = Literature_type, fill = Source_type)) +
  geom_col(position = "stack", colour = "grey20", linewidth = 0.3, alpha = 0.4) +
  scale_fill_manual(values = palette) +
  labs(x = "Number of Sources", y = NULL, fill = "Source Type") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(face = "bold", size = 11),
    axis.text.x = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

plot_references

#####
# Funder and Project Proponents
# Load required packages
pacman::p_load(readxl, dplyr, ggplot2, forcats, tidyr, RColorBrewer)

# === 1. Read data ===
data <- read_excel("C:/Users/Kris Jypson Esturas/OneDrive - Macquarie University/Documents/2025/00 CRR Gov/R Analysis/Data/20250609 CRR Governance Database 2023-2025.xlsx",
                   sheet = "CRR 2025")

# === 2. Get unique Specific_methods and extend color palette ===
method_levels <- unique(data$Specific_method)
n_methods <- length(method_levels)

# Extend Set3 palette to cover all methods
palette <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(n_methods)
names(palette) <- method_levels

# === 3. Plot: Specific_method by Proponent_sector ===
prop_plot <- data %>%
  filter(!is.na(Specific_method), !is.na(Proponent_sector)) %>%
  count(Proponent_sector, Specific_method) %>%
  ggplot(aes(x = n, y = fct_reorder(Proponent_sector, n, sum), fill = Specific_method)) +
  geom_col(position = "stack", colour = "grey30", linewidth = 0.3) +
  scale_fill_manual(values = palette) +
  labs(
    x = "Number of Projects",
    y = NULL,
    fill = "Restoration Method",
    title = "Restoration Methods by Proponent Sector"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9)
  )

# === 4. Plot: Specific_method by Funder_sector ===
fund_plot <- data %>%
  filter(!is.na(Specific_method), !is.na(Funder_sector)) %>%
  count(Funder_sector, Specific_method) %>%
  ggplot(aes(x = n, y = fct_reorder(Funder_sector, n, sum), fill = Specific_method)) +
  geom_col(position = "stack", colour = "grey30", linewidth = 0.3) +
  scale_fill_manual(values = palette) +
  labs(
    x = "Number of Projects",
    y = NULL,
    fill = "Restoration Method",
    title = "Restoration Methods by Funder Sector"
  ) +
  theme_minimal(base_size = 13) +http://127.0.0.1:17671/graphics/plot_zoom_png?width=1672&height=756
  theme(
    axis.text.y = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9)
  )

# === 5. Show both plots ===
prop_plot
fund_plot


##############
# Partnership Networks 

# === Load libraries ===
library(tidyverse)
library(readxl)
library(networkD3)
library(htmlwidgets)

# === 1. Load data ===
data <- read_excel("C:/Users/Kris Jypson Esturas/OneDrive - Macquarie University/Documents/2025/00 CRR Gov/R Analysis/Data/20250609 CRR Governance Database 2023-2025.xlsx",
                   sheet = "CRR 2025")

# === 2. Split multiple values ===
sankey_data <- data %>%
  select(Funder_sector, Proponent_sector, Specific_method) %>%
  filter(!is.na(Funder_sector), !is.na(Proponent_sector), !is.na(Specific_method)) %>%
  mutate(across(everything(), ~ str_replace_all(.x, " and | /", ", "))) %>%
  separate_rows(Funder_sector, Proponent_sector, Specific_method, sep = ",\\s*")

# === 3. Create links (flows) ===
flow1 <- sankey_data %>%
  count(Funder_sector, Proponent_sector, name = "value") %>%
  mutate(source = paste0(Funder_sector, " (F)"),
         target = paste0(Proponent_sector, " (P)")) %>%
  select(source, target, value)

flow2 <- sankey_data %>%
  count(Proponent_sector, Specific_method, name = "value") %>%
  mutate(source = paste0(Proponent_sector, " (P)"),
         target = Specific_method) %>%
  select(source, target, value)

flows <- bind_rows(flow1, flow2)

# === 4. Create nodes ===
nodes <- tibble(name = unique(c(flows$source, flows$target))) %>%
  mutate(group = name)

# === 5. Add link indices and group
flows <- flows %>%
  mutate(
    IDsource = match(source, nodes$name) - 1,
    IDtarget = match(target, nodes$name) - 1,
    group = source
  )

# === 6. Manually assign distinct colors to key nodes ===
custom_colors <- c(
  # Funders
  "Government - NGA (F)" = "#A2AEBB",  # Cadet Gray
  "Donor Agency (F)"     = "#FFBA08",  # Selective Yellow
  "Government - LGU (F)" = "#3F88C5",  # Steel Blue
  "Unknown (F)"          = "#C8553D",  # Brick Red
  "Private (F)"          = "#B9848C",  # Plum
  "CSO (F)"              = "#7FB285",  # Moss Green
  "NGO (F)"              = "#8E9AAF",  # Blue Gray
  "Academe (F)"          = "#1C3144",  # Prussian Blue
  "Individual (F)"       = "#DF9AA3",  # Coral Pink
  "Government - RGA (F)" = "#99907D",  # Stone
  
  # Proponents
  "Government - NGA (P)" = "#A2AEBB",
  "Donor Agency (P)"     = "#FFBA08",
  "Government - LGU (P)" = "#3F88C5",
  "Unknown (P)"          = "#C8553D",
  "Private (P)"          = "#B9848C",
  "CSO (P)"              = "#7FB285",
  "NGO (P)"              = "#8E9AAF",
  "Academe (P)"          = "#1C3144",
  "Individual/Student Project (P)" = "#DF9AA3",
  "Government - RGA (P)" = "#99907D",
  
  # Methods (optional: soft neutrals or grayscale variants)
  "Artificial Reef" = "#E3D5CA",
  "Coral gardening" = "#ADC2A9",
  "Direct transplantation" = "#A44A3F",
  "Larval enhancement" = "#F1DCA7",
  "Larval Enhancement" = "#F1DCA7",
  "Direct Transplantation" = "#A44A3F",
  "Microfragmentation" = "#B4A7D6",
  "Rubble Stabilisation" = "#7A6C5D",
  "Substrate stabilisation" = "#5D737E",
  "Mineral accretion" = "#C4A287",
  "Algal Removal" = "#B5D8CC",
  "Unknown" = "#D6C6B9"
)

# === 7. Build the color scale ===
# Ensure all nodes get a color (fallback = gray if not manually specified)
color_palette <- custom_colors[nodes$name]
color_palette[is.na(color_palette)] <- "#cccccc"

color_scale <- paste0(
  'd3.scaleOrdinal().domain(["',
  paste(nodes$name, collapse = '", "'),
  '"]).range(["',
  paste(color_palette, collapse = '", "'),
  '"])'
)

# === 8. Plot Sankey ===
sankey <- sankeyNetwork(
  Links = flows,
  Nodes = nodes,
  Source = "IDsource",
  Target = "IDtarget",
  Value = "value",
  NodeID = "name",
  NodeGroup = "group",
  LinkGroup = "group",
  fontSize = 13,
  nodeWidth = 30,
  sinksRight = TRUE,
  colourScale = color_scale
)

# === 9. Save output ===
saveWidget(sankey, "CRR_Sankey_Final.html", selfcontained = TRUE)

# Optional: View in RStudio Viewer
sankey



############################################################################
library(tidyverse)
library(readxl)

# Load the data
data <- read_excel("C:/Users/Kris Jypson Esturas/OneDrive - Macquarie University/Documents/2025/00 CRR Gov/R Analysis/Data/20250616 CRR Governance Database 2023-2025.xlsx",
                   sheet = "CRR 2025")

# Calculate the percentage of well-defined timelines
well_defined_timeline <- data %>%
  filter(Project_start != "Unknown", Project_end != "Unknown") %>%
  summarise(Count = n())

total_records <- nrow(data)

percent_well_defined <- (well_defined_timeline$Count / total_records) * 100

# Print the result
cat("Percent of records with well-defined timelines:", round(percent_well_defined, 2), "%\n")

#############################################################################
library(tidyverse)
library(readxl)

# 1. Clean and split multiple methods
data_cleaned <- data %>%
  mutate(Specific_method = str_replace_all(Specific_method, ",", ";")) %>%
  mutate(Specific_method = strsplit(as.character(Specific_method), ";\\s*")) %>%
  unnest(Specific_method)

# 2. Filter only rows with known years
data_known <- data_cleaned %>%
  filter(!is.na(Year_estab)) %>%
  mutate(Year_estab = as.numeric(Year_estab))

# 3. Create 'Decade' or 'Era' variable
data_known <- data_known %>%
  mutate(Era = case_when(
    Year_estab < 1980 ~ "1970s",
    Year_estab < 1990 ~ "1980s",
    Year_estab < 2000 ~ "1990s",
    Year_estab < 2010 ~ "2000s",
    Year_estab < 2020 ~ "2010s",
    Year_estab >= 2020 ~ "2020s"
  )) %>%
  mutate(Era = factor(Era, levels = c("1970s", "1980s", "1990s", "2000s", "2010s", "2020s")))

# 4. Count projects per method per era
pie_data <- data_known %>%
  count(Era, Specific_method, name = "Count")

# 5. Compute proportions per pie
pie_data <- pie_data %>%
  group_by(Era) %>%
  mutate(prop = Count / sum(Count)) %>%
  ungroup()

# 6. Get sample size per era
era_counts <- pie_data %>%
  group_by(Era) %>%
  summarise(n = sum(Count)) %>%
  mutate(Era_label = paste0(Era, "\n(n = ", n, ")")) %>%
  select(Era, Era_label)

# 7. Join labels to pie_data
pie_data <- pie_data %>%
  left_join(era_counts, by = "Era")

# 8. Define custom colors
custom_colors <- c(
  "Artificial reef" = "#66C2A5",
  "Coral gardening" = "#FC8D62",
  "Direct transplantation" = "#8DA0CB",
  "Larval enhancement" = "#FFD92F",
  "Mineral accretion" = "#E78AC3",
  "Algae Removal" = "#BEBEBE"
)

# 9. Plot: Pie charts by era with sample sizes
pie_data_clean <- pie_data %>% filter(!is.na(Era))

ggplot(pie_data_clean, aes(x = "", y = prop, fill = Specific_method)) +
  geom_bar(stat = "identity", width = 1, color = "grey20") +
  coord_polar("y") +
  facet_wrap(~ Era_label, ncol = 6) +  # Use custom labels with n
  scale_fill_manual(values = custom_colors) +
  theme_void() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 16, face = "bold", family = "Arial"),   # Facet labels with n
    plot.title = element_text(size = 16, hjust = 0.5, family = "Arial"),
    text = element_text(size = 12, family = "Arial")
  )


