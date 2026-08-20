###############################################################################
###############################################################################

# 🌾️ Openland Assessment Project
# 🧪 Phase 1, Step 2 — Exploratory Data Analysis and Quality Assurance
#
# Supporting Analytical Workflow for:
# Michigan Department of Natural Resources
# Wildlife Division, Planning and Adaptation Section
# Technical Report
#
# Author: Steven M. Gurney
# Last updated: 18 AUGUST 2026


###############################################################################
###############################################################################

# PURPOSE
# -------
# This script reads the prepared MiFI working dataset created in Phase 1,
# Part 1 and conducts exploratory data analysis and quality assurance checks.
# Simply put, its purpose is to better understand the prepared MiFI working 
# dataset before formal analysis.
#
# This script is designed to:
#
#   1) Confirm that the prepared working dataset loaded correctly.
#   2) Summarize missing and placeholder values.
#   3) Check authority, management area, cover type, canopy closure, management
#      status, duplicate stand records, and acreage distributions.
#   4) Create simple QA figures that help understand data limitations.
#
#
# INPUTS
# ------
#   • Prepared, full working dataset:
#       stands_working_prepped.rds
#
#
# OUTPUTS
# -------
#   • EDA & QA summary table (potential appendix file):
#       appendix_eda_qc_summary_table.csv 
#
#   • QA and exploratory figures:
#       - management_area_top10_acres.png
#       - cover_type_top10_comparison.png
#       - canopy_closure_distribution.png
#       - management_status_distribution.png
#       - stand_size_distribution.png
#       - stand_size_log_distribution.png
#       - inventory_age_distribution.png
#       - inventory_age_class_distribution.png
#       - inventory_year_distribution.png
#       - date_field_comparison.png
#
#
# IMPORTANT INTERPRETATION NOTES
# ------------------------------
# This script is exploratory and diagnostic. Outputs document dataset structure, 
# completeness, temporal characteristics, and potential limitations that inform 
# downstream analyses. It does not estimate ecological change or produce final 
# analytical summaries. Its purpose is to better understand the prepared MiFI
# working dataset before formal analysis.
#
#
# 📋 Summary of EDA/QC Findings
# -----------------------------
#
# The summary below reflects the prepared Wildlife Division dataset generated
# from the MiFI snapshot used in this analysis.
#
# 1. The prepared working dataset contained 19,949 stand records representing
#    19,588 unique stands, approximately 406,252 mapped acres, 266
#    compartments, and 104 management areas. 
#
# 2. Core vegetation attributes were generally complete. Missing Level-3 and
#    Level-4 cover-type information occurred in approximately 2% of records,
#    while canopy closure information was missing in approximately 1% of
#    records. These records were retained in the working dataset and may be 
#    later addressed through analysis-specific filtering.
#
# 3. Management-status attributes contained substantial uncertainty. More than
#    half of records were coded as either "Unspecified" or missing, which may
#    reflect inventory methodology. Management-status summaries should be 
#    interpreted cautiously and may require additional information for more 
#    meaningful interpretation. 
#
# 4. Stand acreage distributions were strongly right-skewed. Median stand size
#    was approximately 9 acres, whereas mean stand size exceeded 20 acres.
#    A relatively small number of very large polygons exerted substantial
#    influence on acreage-based summaries.
#
# 5. Water and wetland cover types dominated the upper tail of the acreage
#    distribution. Eight of the ten largest polygons were classified as Water,
#    while the remaining two were classified as Emergent Wetland (Phragmites).
#    Consequently, acreage-based summaries may be disproportionately influenced
#    by a relatively small number of large aquatic or wetland features.
#
# 6. Water represented a disproportionately large share of mapped acreage.
#    Although Water accounted for only approximately 3% of stand records, it
#    represented approximately 11% of total mapped acreage. Individual
#    management areas contained up to 10,000 acres of mapped water, and water
#    accounted for as much as 66% of total acreage within some management areas. 
#    It may be valuable for water cover to be considered separately when 
#    interpreting terrestrial vegetation summaries.
#
# 7. Forested cover types dominated the inventory, with Lowland Deciduous
#    Forest, Mixed Upland Deciduous Forest, Oak Types, and Aspen representing
#    major components of both stand counts and mapped acreage.
#
# 8. Canopy-closure distributions reflected the broad diversity of cover types
#    represented in the inventory. Open, intermediate, and closed-canopy
#    conditions were all well represented. Sub-canopy cover was not carried
#    forward for analysis because approximately 99% of forested stand records
#    were missing sub-canopy information or contained zero values interpreted as
#    no data. However, it is likely that sub-canopy data may exist somewhere in a
#    different dataset.
#
# 9. Duplicate stand identifiers (fcskey values) were uncommon and may reflect
#    repeated inventory, stand updates, or database maintenance rather than 
#    data-entry errors. These repeated records provide the foundation for
#    developing rolling inventory snapshots.
#
# 10. Temporal evaluation identified legacy and placeholder dates within the
#     database. Records dated 1899 and 1900 were present in inventory and
#     creation-date fields and were interpreted as migration artifacts,
#     placeholder values, or legacy database behavior rather than true
#     inventory dates.
#
# 11. The created_date and last_edited_date fields appeared to reflect database
#     migration and administrative activity rather than ecological inventory
#     timing. In contrast, the date field most closely represented stand-level
#     inventory or assessment activity and was therefore selected for temporal
#     analyses.
#
# 12. Inventory records were concentrated between approximately 2010 and 2025,
#     further supporting that MiFI functions as a rolling inventory rather than 
#     a synchronized statewide inventory.
#
# 13. Based on the most recent inventory record associated with each unique
#     stand, stand data were an average of approximately 9 years old
#     (median = 9.5 years; IQR = 5–13 years). Approximately 16% of stands
#     had not been inventoried within the previous 15 years.
#
# 14. Because inventory dates span multiple decades and inventory effort has
#     varied through time, direct comparison of mapped cover-type acreage among
#     years would confound ecological change with changes in inventory
#     coverage, stand updates, and database growth. These findings informed 
#     development of the rolling-snapshot approach used for temporal analyses.


###############################################################################
# 📦 1. Load Required Packages
###############################################################################
# ⭐ Why this matters:
# This loads the packages needed to summarize, visualize, and document data
# quality patterns in the prepared WLD MiFI working dataset.

library(sf)       # Analyzing spatial data
library(dplyr)    # Data manipulation
library(ggplot2)  # Data visualization
library(scales)   # Scaling visuals
library(readr)    # Reading/writing data
library(tibble)   # Simple data frames
library(forcats)  # Visualization tool
library(tidyr)    # Reshaping data for plots

# Prevent scientific notation in printed tables.
options(
  scipen = 999
)

###############################################################################
# 🎨 2. Define Global Figure Style
###############################################################################
# ⭐ Why this matters:
# This keeps EDA/QC figures visually consistent with the rest of the Openland
# Assessment workflow.

# Figures are designed for insertion into standard Word documents and reports.

figure_width <- 7.5
figure_height <- 5
figure_dpi <- 300

# Standard color palette.
crop_color  <- "goldenrod3"
herb_color <- "springgreen4"
neutral_color     <- "grey50"

# Custom theme.
theme_grass <- function() {
  theme_classic(base_size = 12) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(
        face = "bold",
        size = 14
      ),
      plot.subtitle = element_text(
        size = 11
      ),
      axis.title = element_text(
        face = "bold"
      ),
      panel.grid.minor = element_blank(),
      legend.title = element_blank(),
      legend.position = "right"
    )
}


###############################################################################
# 📁 3. Read Prepared Working Dataset and Summarize
###############################################################################
# ⭐ Why this matters:
# This confirms the prepared WLD dataset was created successfully and documents
# the basic number of compartments, stand records, and inventoried acres.

# This file was created in Phase 1, Step 1 — Data Preparation.

# WLD-level data from output folder.
stands_working <- readRDS("phase1_step1_outputs/stands_working_prepped.rds")

# Inspect object structure.
glimpse(stands_working) # Quick summary.
names(stands_working) # Look at column names.

# Summarize the size of the prepared working dataset (which may include
# multiple records for a single stand).
working_summary <- data.frame(
  metric = c(
    "Compartments",
    "Stands",
    "Acres"
  ),
  
  working = c(
    n_distinct(stands_working$fc_key),
    nrow(stands_working),
    sum(stands_working$acres, na.rm = TRUE)
  )
)

working_summary


###############################################################################
# ⚠️ 4. Missing and Placeholder Values
###############################################################################
# ⭐ Why this matters:
# This identifies missing and unassigned values that could affect cover-type,
# canopy, management-status, and acreage summaries.

# NA values indicate missing data.
# Zero values generally indicate unassigned or non-informative classifications.
# Note: zeros may be normal for some fields depending on MiFI coding rules.

missing_summary <- data.frame(
  variable = c(
    "L3 Cover Type",
    "L4 Cover Type",
    "Canopy Closure",
    "Management Status",
    "Acres"
  ),
  
  missing_values = c(
    sum(is.na(stands_working$l3covertype)),
    sum(is.na(stands_working$l4covertype_full)),
    sum(is.na(stands_working$canopy_closure)),
    sum(is.na(stands_working$managed_site)),
    sum(is.na(stands_working$acres))
  ),
  
  zero_values = c(
    sum(stands_working$l3covertype == 0, na.rm = TRUE),
    sum(stands_working$l4covertype_full == 0, na.rm = TRUE),
    sum(stands_working$canopy_closure == 0, na.rm = TRUE),
    sum(stands_working$managed_site == 0, na.rm = TRUE),
    sum(stands_working$acres == 0, na.rm = TRUE)
  )
) %>%
  mutate(
    total_rows = nrow(stands_working),
    percent_missing = 100 * missing_values / total_rows,
    percent_zero = 100 * zero_values / total_rows
  )

missing_summary


###############################################################################
# 🗺️ 5. Authority and Management Area Distribution
###############################################################################
# ⭐ Question this helps answer:
# Which WLD management areas are represented in the working dataset, and how is
# the inventoried acreage distributed across them?

authority_summary <- stands_working %>%
  st_drop_geometry() %>%
  count(authority_key, sort = TRUE) %>%
  mutate(
    percent = n / sum(n),
    percent_label = percent(percent, accuracy = 0.1)
  )

authority_summary


# Count unique management areas.
stands_working %>%
  st_drop_geometry() %>%
  summarise(
    unique_units = n_distinct(unit_key)
  )


# Summarize management areas by stand count and acreage.
unit_summary <- stands_working %>%
  st_drop_geometry() %>%
  group_by(unit_key) %>%
  summarise(
    stands = n(),
    acres = sum(acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percent_stands = stands / sum(stands),
    percent_acres = acres / sum(acres)
  ) %>%
  arrange(desc(acres))

# Print summary.
unit_summary


# Plot top 10 management areas by acreage.
# The top cover types are selected by acreage because acreage is more relevant
# for resource allocation, workload, and management opportunity than stand count.
# Stand count is still compared because it helps show how mapped area is divided
# across stand records.
unit_top10 <- unit_summary %>%
  slice_max(order_by = acres, n = 10) %>%
  mutate(
    unit_key = factor(
      unit_key,
      levels = rev(unit_key)
    )
  )

unit_plot <- ggplot(
  unit_top10,
  aes(x = unit_key, y = percent_acres)
) +
  geom_col(fill = "grey50") +
  geom_text(
    aes(label = percent(percent_acres, accuracy = 0.1)),
    hjust = -0.1,
    size = 3.8
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    #title = "Top 10 Management Areas by Acreage",
    #subtitle = "Percent of inventoried acres in the working dataset",
    x = "WLD management area\n(top 10 by acres)",
    y = "Percent of inventoried WLD acres\n(in MiFI)"
  ) +
  theme_grass()

# Print plot.
unit_plot


###############################################################################
# 🌾 6A. Preliminary Agriculture-Branch Cover Summary
###############################################################################
# ⭐ Question this helps answer:
# Which Agriculture-branch cover types are present in the prepared WLD working
# dataset before the final study dataset is filtered?
#
# This is an exploratory summary of cover records in the prepared working
# dataset whose standardized Level-3 cover code begins with "2".
#
# In the IFMAP hierarchy, cover codes beginning with "2" fall under the broader
# Agriculture branch. This check is used for EDA/QC only and does not define the
# final study dataset. Final analysis-specific filtering is completed in the
# snapshot analysis script.

cover_200_summary <- stands_working %>%
  st_drop_geometry() %>%
  filter(
    stringr::str_starts(
      as.character(l3covertype),
      "2" # Any IFMAP cover code beginning with 2 is in the Agriculture branch.
    )
  ) %>%
  group_by(
    l3covertype,
    l3cover_key
  ) %>%
  summarise(
    stands = n(),
    acres = sum(acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percent_200_stands = stands / sum(stands),
    percent_200_acres = acres / sum(acres),
    
    percent_total_stands =
      stands / nrow(st_drop_geometry(stands_working)),
    
    percent_total_acres =
      acres / sum(stands_working$acres, na.rm = TRUE),
    
    percent_200_stands_label =
      percent(percent_200_stands, accuracy = 0.1),
    
    percent_200_acres_label =
      percent(percent_200_acres, accuracy = 0.1),
    
    percent_total_stands_label =
      percent(percent_total_stands, accuracy = 0.1),
    
    percent_total_acres_label =
      percent(percent_total_acres, accuracy = 0.1)
  ) %>%
  arrange(
    desc(acres)
  )

# Print summary.
cover_200_summary


###############################################################################
# 🌾 6B. Preliminary Agriculture-Branch Cover Summary by Management Area
###############################################################################
# ⭐ Question this helps answer:
# Which management areas contain Agriculture-branch records in the prepared WLD
# working dataset before final study filtering?
#
# This table summarizes the same preliminary Agriculture-branch records by
# management area. It helps identify where these records occur in the working
# dataset and supports later decisions about study-area filtering, QA, and
# interpretation.

cover_200_by_unit_summary <- stands_working %>%
  st_drop_geometry() %>%
  filter(
    stringr::str_starts(
      as.character(l3covertype),
      "2" # Any IFMAP cover code beginning with 2 is in the Agriculture branch.
    )
  ) %>%
  group_by(
    unit_key,
    l3covertype,
    l3cover_key
  ) %>%
  summarise(
    stands = n(),
    acres = sum(acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(
    unit_key
  ) %>%
  mutate(
    total_unit_200_stands = sum(stands, na.rm = TRUE),
    total_unit_200_acres = sum(acres, na.rm = TRUE),
    
    percent_unit_200_stands =
      stands / total_unit_200_stands,
    
    percent_unit_200_acres =
      acres / total_unit_200_acres,
    
    percent_unit_200_stands_label =
      percent(percent_unit_200_stands, accuracy = 0.1),
    
    percent_unit_200_acres_label =
      percent(percent_unit_200_acres, accuracy = 0.1)
  ) %>%
  ungroup() %>%
  arrange(
    unit_key,
    desc(acres)
  )

# Print summary.
cover_200_by_unit_summary


###############################################################################
# 🌲 6C. Level-3 Cover Type Distribution
###############################################################################
# ⭐ Question this helps answer:
# What are the dominant Level-3 cover types in the WLD MiFI inventory, and do
# stand counts and acreage tell the same story?

# Summarize cover-type composition by both stand count and acreage.

cover_summary <- stands_working %>%
  st_drop_geometry() %>%
  group_by(l3covertype, l3cover_key) %>%
  summarise(
    stands = n(),
    acres = sum(acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percent_stands = stands / sum(stands),
    percent_acres = acres / sum(acres),
    percent_stands_label = percent(percent_stands, accuracy = 0.1),
    percent_acres_label = percent(percent_acres, accuracy = 0.1)
  ) %>%
  arrange(desc(acres))

# Print summary.
cover_summary


# Compare percent of stands and percent of acres for top cover types.
cover_compare_df <- cover_summary %>%
  slice_max(order_by = acres, n = 10) %>% # Top 10 by acres.
  select(l3cover_key, percent_stands, percent_acres) %>%
  pivot_longer(
    cols = c(percent_stands, percent_acres),
    names_to = "metric",
    values_to = "percent"
  ) %>%
  mutate(
    metric = recode(
      metric,
      percent_stands = "Stands",
      percent_acres = "Acres"
    ),
    l3cover_key = factor(
      l3cover_key,
      levels = rev(unique(l3cover_key))
    )
  )

cover_compare_plot <- ggplot(
  cover_compare_df,
  aes(x = l3cover_key, y = percent, fill = metric)
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    #title = "Top Level-3 Cover Types by Stand Count and Acreage",
    #subtitle = "Comparing percent of stand records with percent of inventoried acreage",
    x = "Level-3 cover type\n(top 10 by acres)",
    y = "Percent of WLD inventory\n(in MiFI)",
    fill = NULL
  ) +
  theme_grass() +
  scale_fill_manual(
    values = c(
      "Stands" = "grey70",
      "Acres" = "grey40"
    )
  )

cover_compare_plot


# Summarize the relationship between mapped acreage and stand count by Level-3
# cover type.
#
# Acres per stand is used as a simple indicator of average mapped stand size.
# Lower values indicate that a cover type is represented by many smaller mapped
# stands. Higher values indicate that a cover type is represented by fewer,
# larger mapped stands.

cover_acres_per_stand_summary <- cover_summary %>%
  mutate(
    acres_per_stand = acres / stands
  ) %>%
  arrange(
    acres_per_stand
  ) %>%
  select(
    l3cover_key,
    stands,
    acres,
    acres_per_stand
  )

# Print summary.
cover_acres_per_stand_summary


###############################################################################
# 🌳 7. Canopy and Subcanopy Summaries
###############################################################################
# ⭐ Question this helps answer:
# How complete and useful are canopy and sub-canopy fields for interpreting
# openland, non-forested, and forested conditions?

# Summarize canopy closure classes. These are categorical classes, not precise
# continuous canopy-cover estimates.
#
# Missing values, text "NA" values, and any existing "Missing" values are pooled
# together as "Missing / NA" so they are treated consistently in summaries and
# plots.

# Define canopy-cover class order.
# Note: "NA" includes missing, text "NA", "Missing", and "Unspecified" values.
canopy_class_order <- c(
  "0-25",
  "25-50",
  "50-75",
  "75-100",
  "NA"
)

# Summarize canopy-cover classes by both stand count and acreage.
# This helps identify whether canopy-cover classes are represented similarly by
# number of stands and total acres. Differences between stand counts and acres
# may indicate that some canopy classes tend to occur in larger or smaller
# mapped stands.

# Define canopy-cover class order.
# Note: "NA" includes missing, text "NA", "Missing", and "Unspecified" values.
canopy_class_order <- c(
  "0–<25", # Non-forested <25% canopy closure.
  "25–<50",
  "50–<75",
  "75–100",
  "NA"
)

canopy_summary <- stands_working %>%
  st_drop_geometry() %>%
  mutate(
    canopy_key = case_when(
      canopy_key == "0-25"   ~ "0–<25",
      canopy_key == "25-50"  ~ "25–<50",
      canopy_key == "50-75"  ~ "50–<75",
      canopy_key == "75-100" ~ "75–100",
      is.na(canopy_key) ~ "NA",
      canopy_key == "NA" ~ "NA",
      canopy_key == "Missing" ~ "NA",
      canopy_key == "Missing / NA" ~ "NA",
      canopy_key == "Unspecified" ~ "NA",
      TRUE ~ canopy_key
    ),
    canopy_key = factor(
      canopy_key,
      levels = canopy_class_order
    )
  ) %>%
  group_by(
    canopy_key
  ) %>%
  summarise(
    stands = n(),
    acres = sum(acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percent_stands = 100 * stands / sum(stands),
    percent_acres = 100 * acres / sum(acres)
  ) %>%
  arrange(
    canopy_key
  )

# Print summary.
canopy_summary


# Plot canopy-cover composition using percent of acres.
#
# This figure shows how canopy-cover classes are represented across inventoried
# Wildlife Division acres. The "NA" class includes missing, text "NA",
# "Missing", "Missing / NA", and "Unspecified" canopy values.

canopy_plot <- ggplot(
  canopy_summary,
  aes(
    x = canopy_key,
    y = percent_acres
  )
) +
  geom_col(
    fill = "grey50"
  ) +
  geom_text(
    aes(
      label = paste0(
        round(percent_acres, 1),
        "%"
      )
    ),
    vjust = -0.3,
    size = 3.8
  ) +
  scale_y_continuous(
    labels = percent_format(scale = 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  scale_x_discrete(
    drop = FALSE
  ) +
  labs(
    #title = "Distribution of Canopy Closure Classes",
    #subtitle = "Percent of inventoried acres within each canopy closure category",
    x = "Canopy-closure class",
    y = "Percent of inventoried WLD acres\n(in MiFI)"
  ) +
  theme_grass()

# Print plot.
canopy_plot

# Compare percent of stands and percent of acres by canopy-cover class.
#
# This figure mirrors the Level-3 cover comparison plot concept by comparing
# representation in stand records versus mapped acreage.
#
# Canopy classes are shown in logical canopy-cover order rather than being
# reordered by abundance.

canopy_compare_df <- canopy_summary %>%
  select(
    canopy_key,
    percent_stands,
    percent_acres
  ) %>%
  pivot_longer(
    cols = c(
      percent_stands,
      percent_acres
    ),
    names_to = "metric",
    values_to = "percent"
  ) %>%
  mutate(
    metric = recode(
      metric,
      percent_stands = "Stands",
      percent_acres = "Acres"
    ),
    metric = factor(
      metric,
      levels = c(
        "Stands",
        "Acres"
      )
    ),
    canopy_key = factor(
      canopy_key,
      levels = canopy_class_order
    )
  )

# Print plotting table.
canopy_compare_df


canopy_compare_plot <- ggplot(
  canopy_compare_df,
  aes(
    x = canopy_key,
    y = percent,
    fill = metric
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  scale_y_continuous(
    labels = percent_format(scale = 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  scale_x_discrete(
    drop = FALSE
  ) +
  labs(
    #title = "Canopy Cover by Stand Count and Acreage",
    #subtitle = "Comparing percent of stand records with percent of inventoried acreage",
    x = "Canopy-closure class",
    y = "Percent of WLD inventory\n(in MiFI)",
    fill = NULL
  ) +
  theme_grass() +
  scale_fill_manual(
    values = c(
      "Stands" = "grey70",
      "Acres" = "grey40"
    )
  )

# Print plot.
canopy_compare_plot

# Summarize availability of sub_canopy_cover for forested records.
#
# For this check, forested records are defined as records with canopy_key values
# other than "0-25". The "0-25" canopy class is excluded because this summary is
# intended to evaluate sub-canopy information for forested conditions.
#
# Missing values and zero values are pooled together as no-data values because
# zero functions as a no-data value in this field.
#
# Because this check evaluates field completion, stand records are more
# informative than acres.

forested_canopy_tbl <- stands_working %>%
  st_drop_geometry() %>%
  filter(
    !is.na(canopy_key),
    canopy_key != "0-25"
  )

# Summarize availability of sub_canopy_cover for forested records.
sub_canopy_availability_summary <- forested_canopy_tbl %>%
  mutate(
    sub_canopy_data_status = case_when(
      is.na(sub_canopy_cover) ~ "Missing / No data",
      sub_canopy_cover == 0   ~ "Missing / No data",
      TRUE                    ~ "Available"
    )
  ) %>%
  group_by(
    sub_canopy_data_status
  ) %>%
  summarise(
    records = n(),
    unique_stands = n_distinct(fcskey),
    acres = sum(acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percent_records = 100 * records / sum(records),
    percent_unique_stands = 100 * unique_stands / sum(unique_stands),
    percent_acres = 100 * acres / sum(acres)
  ) %>%
  arrange(
    desc(records)
  )

# Print summary.
sub_canopy_availability_summary

# Pull no-data result for appendix summary table.
sub_canopy_no_data_summary <- sub_canopy_availability_summary %>%
  filter(
    sub_canopy_data_status == "Missing / No data"
  )

# Print summary.
sub_canopy_no_data_summary

# Summarize the composition of available sub_canopy_cover values for forested
# records.
#
# Missing values and zero values are excluded from this composition table because
# they were already summarized as "Missing / No data" in the availability
# summary above.
#
# This table is retained because even though sub-canopy data availability is low,
# the available records still represent mapped acreage that may be useful for
# interpretation or future QA.

sub_canopy_available_composition_summary <- forested_canopy_tbl %>%
  filter(
    !is.na(sub_canopy_cover),
    sub_canopy_cover != 0
  ) %>%
  mutate(
    sub_canopy_cover = as.character(sub_canopy_cover)
  ) %>%
  group_by(
    sub_canopy_cover
  ) %>%
  summarise(
    records = n(),
    unique_stands = n_distinct(fcskey),
    acres = sum(acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percent_available_records =
      100 * records / sum(records),
    
    percent_available_unique_stands =
      100 * unique_stands / sum(unique_stands),
    
    percent_available_acres =
      100 * acres / sum(acres)
  ) %>%
  arrange(
    desc(acres) # Sort by acres.
  )

# Print summary.
sub_canopy_available_composition_summary


###############################################################################
# 🌳 8. Canopy Closure by Level-3 Cover Type
###############################################################################
# ⭐ Question this helps answer:
# Which Level-3 cover types are represented within each canopy-closure class?

# This builds on the overall canopy-closure distribution above by identifying
# which Level-3 cover types make up each canopy-closure class.

canopy_by_cover_summary <- stands_working %>%
  st_drop_geometry() %>%
  mutate(
    canopy_key = if_else(
      is.na(canopy_key),
      "Missing / NA",
      canopy_key
    ),
    canopy_key = factor(
      canopy_key,
      levels = c(
        "0-25",
        "25-50",
        "50-75",
        "75-100",
        "Unspecified",
        "Missing / NA"
      )
    )
  ) %>%
  group_by(canopy_key, l3cover_key) %>%
  summarise(
    stands = n(),
    acres = sum(acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(canopy_key) %>%
  mutate(
    percent_within_canopy = stands / sum(stands)
  ) %>%
  ungroup() %>%
  arrange(canopy_key, desc(stands))

# Print summary.
canopy_by_cover_summary


###############################################################################
# 🛠️ 9. Management Status Distribution
###############################################################################
# ⭐ Question this helps answer:
# How consistently are WLD stand records classified by management status, and
# how much of the open-canopy inventory is identified as managed?

management_summary <- stands_working %>%
  st_drop_geometry() %>%
  group_by(managed_site, management_key) %>%
  summarise(
    stands = n(),
    acres = sum(acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    management_key = if_else(
      is.na(management_key),
      "Missing / NA",
      management_key
    ),
    percent_stands = stands / sum(stands),
    percent_acres = acres / sum(acres)
  ) %>%
  arrange(desc(acres))

# Print summary.
management_summary

# Summarize and compare stand count and acreage by management status.
management_compare_df <- management_summary %>%
  select(management_key, percent_stands, percent_acres) %>%
  pivot_longer(
    cols = c(percent_stands, percent_acres),
    names_to = "metric",
    values_to = "percent"
  ) %>%
  mutate(
    metric = recode(
      metric,
      percent_stands = "Stands",
      percent_acres = "Acres"
    ),
    management_key = factor(
      management_key,
      levels = rev(unique(management_key))
    )
  )

# Build plot.
management_plot <- ggplot(
  management_compare_df,
  aes(x = management_key, y = percent, fill = metric)
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    #title = "Management Status by Stand Count and Acreage",
    #subtitle = "Comparing percent of stand records with percent of mapped acreage",
    x = "Managed-site classification",
    y = "Percent of WLD inventory\n(in MiFI)",
    fill = NULL
  ) +
  theme_grass() +
  scale_fill_manual(
    values = c(
      "Stands" = "grey70",
      "Acres" = "grey40")
  )


# Plot results.
management_plot

# Summarize management status for stands in the open canopy-cover class.
#
# This subset helps evaluate how open-canopy stands are classified from a
# management perspective. This is especially relevant because the 0-<25 canopy
# class is most likely to include grassland-associated, openland, or other
# non-forested conditions.
#
# Percentages are calculated within the 0-<25 canopy subset, not across the full
# Wildlife Division inventory.

open_canopy_management_summary <- stands_working %>%
  st_drop_geometry() %>%
  filter(
    canopy_key == "0-25"
  ) %>%
  mutate(
    management_key = if_else(
      is.na(management_key),
      "Missing / NA",
      management_key
    )
  ) %>%
  group_by(
    managed_site,
    management_key
  ) %>%
  summarise(
    stands = n(),
    acres = sum(acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percent_stands = stands / sum(stands),
    percent_acres = acres / sum(acres)
  ) %>%
  arrange(
    desc(acres)
  )

# Print summary.
open_canopy_management_summary


# Summarize and compare stand count and acreage by management status for the
# 0-<25 canopy subset.

open_canopy_management_compare_df <- open_canopy_management_summary %>%
  select(
    management_key,
    percent_stands,
    percent_acres
  ) %>%
  pivot_longer(
    cols = c(
      percent_stands,
      percent_acres
    ),
    names_to = "metric",
    values_to = "percent"
  ) %>%
  mutate(
    metric = recode(
      metric,
      percent_stands = "Percent of Stands",
      percent_acres = "Percent of Acres"
    ),
    management_key = factor(
      management_key,
      levels = rev(unique(management_key))
    )
  )

# Build plot.
open_canopy_management_plot <- ggplot(
  open_canopy_management_compare_df,
  aes(
    x = management_key,
    y = percent,
    fill = metric
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    #title = "Management Status for 0-<25 Canopy Class",
    #subtitle = "Comparing percent of stand records with percent of mapped acreage",
    x = "Managed-site classification",
    y = "Percent of non-forested WLD inventory\n(0-<25 canopy closure)",
    fill = NULL
  ) +
  theme_grass() +
  scale_fill_manual(
    values = c(
      "Percent of Stands" = "grey70",
      "Percent of Acres" = "grey40"
    )
  )

# Plot results.
open_canopy_management_plot


###############################################################################
# 🔁 10. Duplicate Stand Record Check
###############################################################################
# ⭐ Why this matters:
# This checks whether stand identifiers occur more than once, which affects how
# rolling snapshots and latest-condition summaries should be built.

# Check whether any unique fcskey values occur more than once.

duplicate_stands <- stands_working %>%
  st_drop_geometry() %>%
  count(fcskey, sort = TRUE) %>%
  filter(n > 1)

# Print results.
duplicate_stands


###############################################################################
# 📊 11. Acreage Size-Class Distribution
###############################################################################
# ⭐ Question this helps answer:
# Are WLD inventoried stands mostly small polygons, large polygons, or a mix of
# stand sizes?

# Binning acres makes the stand-size distribution easier to interpret than a
# log-scale histogram.

# Build summary.
acre_size_df <- stands_working %>%
  st_drop_geometry() %>%
  filter(!is.na(acres), acres > 0) %>%
  mutate(
    size_class = case_when(
      acres < 5   ~ "<5 acres",
      acres < 20  ~ "5–20 acres",
      acres < 50  ~ "20–50 acres",
      acres < 100 ~ "50–100 acres",
      TRUE        ~ "100+ acres"
    ),
    size_class = factor(
      size_class,
      levels = c(
        "<5 acres",
        "5–20 acres",
        "20–50 acres",
        "50–100 acres",
        "100+ acres"
      )
    )
  ) %>%
  count(size_class) %>%
  mutate(
    percent = n / sum(n)
  )

# Print results.
acre_size_df

# Build plot.
acre_size_plot <- ggplot(
  acre_size_df,
  aes(x = size_class, y = percent)
) +
  geom_col(fill = "grey50") +
  geom_text(
    aes(label = percent(percent, accuracy = 0.1)),
    vjust = -0.3,
    size = 3.8
  ) +
  scale_y_continuous(
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    #title = "Distribution of Stand Sizes",
    #subtitle = "Binned acreage classes for simplified representation of distribution",
    x = "Stand size class",
    y = "Percent of inventoried WLD stands\n(in MiFI)"
  ) +
  theme_grass()

# Plot results.
acre_size_plot


###############################################################################
# 📊 12. Log-Scaled Acreage Distribution
###############################################################################
# ⭐ Why this matters:
# This provides a technical check of the right-skewed stand-size distribution
# and helps identify whether a few very large records influence acreage summaries.

log_acre_plot <- stands_working %>%
  st_drop_geometry() %>%
  filter(!is.na(acres), acres > 0) %>%
  ggplot(aes(x = acres)) +
  geom_histogram(
    bins = 50,
    fill = "grey50"
  ) +
  scale_x_log10() +
  labs(
    #title = "Log-Scaled Distribution of Stand Sizes",
    #subtitle = "Log scaling highlights the right-skewed stand-size distribution",
    x = "Stand acres (log scale)",
    y = "Number of inventoried WLD stands"
  ) +
  theme_grass()

log_acre_plot


###############################################################################
# 📏 13. Acreage Distribution and Largest Stand Records
###############################################################################
# ⭐ Why this matters:
# This identifies stand-size outliers and the largest mapped records that may
# strongly influence acreage-based summaries.

acre_summary <- stands_working %>%
  st_drop_geometry() %>%
  summarise(
    min_acres = min(acres, na.rm = TRUE),
    q1_acres = quantile(acres, 0.25, na.rm = TRUE),
    median_acres = median(acres, na.rm = TRUE),
    mean_acres = mean(acres, na.rm = TRUE),
    q3_acres = quantile(acres, 0.75, na.rm = TRUE),
    max_acres = max(acres, na.rm = TRUE),
    sd_acres = sd(acres, na.rm = TRUE),
    cv_percent = 100 * sd_acres / mean_acres
  )

# Print results.
acre_summary

# Large stand polygons can strongly influence acreage summaries. Reviewing the
# largest stands helps identify which cover types are driving the upper tail of
# the acreage distribution.
largest_stands <- stands_working %>%
  st_drop_geometry() %>%
  select(
    acres,
    l3covertype,
    l3cover_key,
    l4covertype_full,
    l4cover_key,
    fc_key,
    unit_key
  ) %>%
  arrange(desc(acres)) %>%
  slice_head(n = 10)

# Print results.
largest_stands


###############################################################################
# 🕒 14. Date Summaries
###############################################################################
# ⭐ Why this matters:
# This compares MiFI date fields so the analysis can choose the most appropriate
# date field for rolling inventory summaries.

# MiFI contains multiple date fields that may reflect different processes:
#
#   • date             = likely inventory or assessment date
#   • created_date     = likely database creation or migration date
#   • last_edited_date = likely administrative edit or update date
#
# Some records may contain legacy or migrated dates, so exploratory evaluation
# is important before interpreting temporal patterns.

# Summarize the oldest and newest values recorded in each temporal field.
date_summary <- stands_working %>%
  st_drop_geometry() %>%
  summarise(
    
    # From the "date" column.
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE),
    
    # From the "created_date" column.
    min_created_date = min(created_date, na.rm = TRUE),
    max_created_date = max(created_date, na.rm = TRUE),
    
    # From the "last_edited_date" column.
    min_last_edited_date = min(last_edited_date, na.rm = TRUE),
    max_last_edited_date = max(last_edited_date, na.rm = TRUE)
  )

# Print summary.
date_summary


###############################################################################
# 📅 15. Unfiltered Record Counts by Year
###############################################################################
# ⭐ Why this matters:
# This identifies legacy, placeholder, or migration-related dates before date
# filters are applied in later analyses.

date_year_counts_all <- stands_working %>%
  st_drop_geometry() %>%
  select(
    date,
    created_date,
    last_edited_date
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "date_field",
    values_to = "date_value"
  ) %>%
  mutate(
    year = lubridate::year(date_value)
  ) %>%
  count(
    date_field,
    year,
    sort = TRUE
  )

# Print summary.
date_year_counts_all

# Summarize records occurring before 1950 (arbitrary date). These dates may 
# represent legacy records, migration artifacts, or placeholder values rather than true
# inventory dates.
pre1950_summary <- date_year_counts_all %>%
  filter(year < 1950)

# Print summary.
pre1950_summary # There are a lot of records from 1899 - 1900.


###############################################################################
# 📊 16. Temporal Distribution of Date Fields
###############################################################################
# ⭐ Question this helps answer:
# Which MiFI date field best reflects inventory timing rather than database
# creation, migration, or administrative edits?

# Pre-1950 dates are removed for visualization because they likely represent
# migration artifacts or legacy placeholder values rather than true inventory
# records.

date_long <- stands_working %>%
  st_drop_geometry() %>%
  select(
    date,
    created_date,
    last_edited_date
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "date_field",
    values_to = "date_value"
  ) %>%
  mutate(
    year = lubridate::year(date_value)
  ) %>%
  filter(
    !is.na(year),
    year >= 1950,
    year <= lubridate::year(Sys.Date())
  )

# Print long-format date table.
date_long

# Summarize record counts by date field and year.
date_year_summary <- date_long %>%
  count(date_field, year)

# Print summary.
date_year_summary

# Plot yearly distributions for each temporal field.
date_field_plot <- ggplot(
  date_year_summary,
  aes(x = year, y = n)
) +
  geom_col(fill = "grey50") +
  facet_wrap(
    ~date_field,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = seq(2005, lubridate::year(Sys.Date()), 2)
  ) +
  scale_y_continuous(
    labels = comma_format()
  ) +
  labs(
    #title = "Temporal Distribution of MiFI Date Fields",
    #subtitle = "Comparison of record frequencies among date, created_date, and last_edited_date",
    x = "Inventory year",
    y = "Number of new stand records"
  ) +
  theme_grass()  +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

# Plot results.
date_field_plot


###############################################################################
# 📅 17. Inventory Date Distribution
###############################################################################
# ⭐ Why this matters:
# This summarizes the apparent timing of WLD stand inventory records using the
# date field selected for rolling snapshot analysis.

# This section summarizes the temporal structure of those records and helps
# identify the inventory periods represented in the working dataset.

inventory_year_summary <- stands_working %>%
  st_drop_geometry() %>%
  mutate(
    inventory_year = lubridate::year(date)
  ) %>%
  
  # Remove pre-1950 years likely associated with migration artifacts or
  # placeholder values.
  filter(
    !is.na(inventory_year),
    inventory_year >= 1950,
    inventory_year <= lubridate::year(Sys.Date())
  ) %>%
  count(
    inventory_year,
    sort = FALSE
  )

# Print summary.
inventory_year_summary


# Plot temporal distribution of inventory records.
inventory_year_plot <- ggplot(
  inventory_year_summary,
  aes(x = inventory_year, y = n)
) +
  geom_col(fill = "grey50") +
  scale_x_continuous(
    breaks = seq(2005, lubridate::year(Sys.Date()), 2)
  ) +
  scale_y_continuous(
    labels = comma_format()
  ) +
  labs(
    #title = "Temporal Distribution of New MiFI Inventory Records",
    #subtitle = "Stand records represented in the working dataset by inventory year",
    x = "Inventory year",
    y = "Number of new WLD records\n(MiFI stands)"
  ) +
  theme_grass()

inventory_year_plot


###############################################################################
# 📅 18. Stand Inventory Age Summary
###############################################################################
# ⭐ Question this helps answer:
# How current are the WLD MiFI stand records used to describe present inventory
# conditions?

# Some stands occur multiple times in the database because they have been
# revisited, updated, or re-inventoried through time.
#
# To estimate how well the database reflects current vegetation conditions,
# inventory age is summarized using the most recent valid inventory year
# associated with each unique stand (fcskey).
#
# Inventory age is calculated relative to the current system year.

# Current year used for age calculations.
current_year <- lubridate::year(Sys.Date())

# Print current year.
current_year


# Calculate stand inventory age.
stand_inventory_age <- stands_working %>%
  st_drop_geometry() %>%
  mutate(
    inventory_year = lubridate::year(date)
  ) %>%
  
  # Remove implausible years likely associated with migration artifacts or
  # placeholder values.
  filter(
    !is.na(inventory_year),
    inventory_year >= 1950,
    inventory_year <= current_year
  ) %>%
  
  # Keep the most recent inventory year for each unique stand.
  group_by(fcskey) %>%
  summarise(
    most_recent_inventory_year = max(inventory_year),
    .groups = "drop"
  ) %>%
  mutate(
    inventory_age = current_year - most_recent_inventory_year
  )

# Print table.
stand_inventory_age


# Summarize stand inventory age.
stand_inventory_age_summary <- stand_inventory_age %>%
  summarise(
    unique_stands = n(),
    min_age = min(inventory_age),
    q1_age = quantile(inventory_age, 0.25),
    median_age = median(inventory_age),
    mean_age = mean(inventory_age),
    q3_age = quantile(inventory_age, 0.75),
    max_age = max(inventory_age),
    sd_age = sd(inventory_age)
  )

# Print summary.
stand_inventory_age_summary


# Summarize stand inventory age by year since most recent inventory.
age_summary <- stand_inventory_age %>%
  count(inventory_age) %>%
  mutate(
    percent = n / sum(n)
  )

# Print summary.
age_summary


# Plot stand inventory age distribution as percent of stands.
stand_inventory_age_distribution_plot <- ggplot(
  age_summary,
  aes(x = inventory_age, y = percent)
) +
  geom_col(fill = "grey50") +
  scale_x_continuous(
    breaks = seq(
      min(age_summary$inventory_age, na.rm = TRUE),
      max(age_summary$inventory_age, na.rm = TRUE),
      by = 2
    )
  ) +
  scale_y_continuous(
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    #title = "Distribution of Stand Inventory Age",
    #subtitle = "Based on the most recent inventory record associated with each stand",
    x = "Age of inventory records",
    y = "Percent of inventoried WLD stands\n(in MiFI)"
  ) +
  theme_grass()

# Print plot.
stand_inventory_age_distribution_plot

# Summarize stand inventory age into broader classes.
age_summary <- stand_inventory_age %>%
  mutate(
    age_class = case_when(
      inventory_age < 5  ~ "0–<5 years",
      inventory_age < 10 ~ "5–<10 years",
      inventory_age < 15 ~ "10–<15 years",
      TRUE               ~ "15+ years"
    ),
    age_class = factor(
      age_class,
      levels = c(
        "0–<5 years",
        "5–<10 years",
        "10–<15 years",
        "15+ years"
      )
    )
  ) %>%
  count(age_class) %>%
  mutate(
    percent = n / sum(n)
  )

# Print summary.
age_summary

# Build plot.
stand_inventory_age_class_plot <- ggplot(
  age_summary,
  aes(x = age_class, y = percent)
) +
  geom_col(fill = "grey50") +
  geom_text(
    aes(label = percent(percent, accuracy = 0.1)),
    vjust = -0.3,
    size = 3.8
  ) +
  scale_y_continuous(
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    #title = "Distribution of Stand Inventory Age",
    #subtitle = "Based on the most recent inventory record associated with each stand",
    x = "Years since most recent inventory",
    y = "Percent of inventoried WLD stands"
  ) +
  theme_grass()

# Print plot.
stand_inventory_age_class_plot


###############################################################################
# 🌊 19. Water Summary
###############################################################################
# ⭐ Why this matters:
# This documents how much mapped Water is present in MiFI so terrestrial
# vegetation summaries can remove Water transparently.

# This summary is based on MiFI cover classifications and their associated
# boundaries. The true legal boundaries may differ, so interpret these results
# with caution as they simply reflect what has been inventoried in MiFI.

# This uses stands_working rather than an analysis-specific dataset so water is
# evaluated relative to the full prepared Wildlife Division working dataset.

water_summary <- stands_working %>%
  st_drop_geometry() %>%
  summarise(
    water_stands = sum(l3cover_key == "Water", na.rm = TRUE),
    water_acres = sum(acres[l3cover_key == "Water"], na.rm = TRUE),
    
    total_stands = n(),
    total_acres = sum(acres, na.rm = TRUE),
    
    percent_total_stands = 100 * water_stands / total_stands,
    percent_total_acres = 100 * water_acres / total_acres
  )

# Print summary.
water_summary

# Summarize management areas containing the greatest proportion of mapped water.
# Percent water is calculated relative to the total mapped acreage within each
# management area.

water_by_unit <- stands_working %>%
  st_drop_geometry() %>%
  group_by(unit_key) %>%
  summarise(
    total_acres = sum(acres, na.rm = TRUE),
    water_acres = sum(
      acres[l3cover_key == "Water"],
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    percent_water = 100 * water_acres / total_acres
  ) %>%
  
  # Optional: remove units with negligible water acreage.
  filter(water_acres > 1) %>%
  
  arrange(desc(percent_water))

# Look at it arranged by percent water.
print(water_by_unit)

# Now by acres.
water_by_unit %>% arrange(desc(water_acres))

# Plot the management areas with the greatest mapped water acreage.
#
# This figure identifies where mapped water contributes the greatest absolute
# acreage to the Wildlife Division inventory.

water_by_unit_acres_plot_df <- water_by_unit %>%
  slice_max(
    order_by = water_acres,
    n = 10
  ) %>%
  arrange(
    water_acres
  ) %>%
  mutate(
    unit_key = factor(
      unit_key,
      levels = unit_key
    )
  )

# Print plotting table.
water_by_unit_acres_plot_df


water_by_unit_acres_plot <- ggplot(
  water_by_unit_acres_plot_df,
  aes(
    x = unit_key,
    y = water_acres
  )
) +
  geom_col(
    fill = "grey50"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    #title = "Top Management Areas by Mapped Water Acres",
    #subtitle = "Management areas with the greatest mapped water acreage",
    x = "WLD management area\n(top 10 by water acres in MiFI)",
    y = "Acres classified as water"
  ) +
  theme_grass()

# Print plot.
water_by_unit_acres_plot

# Plot the management areas with the highest proportion of mapped water.
#
# Percent water is calculated as mapped water acres divided by total mapped acres
# within each management area. This figure identifies areas where water makes up
# a large share of the mapped inventory, even if total water acreage is not among
# the largest statewide.

water_by_unit_percent_plot_df <- water_by_unit %>%
  slice_max(
    order_by = percent_water,
    n = 10
  ) %>%
  arrange(
    percent_water
  ) %>%
  mutate(
    unit_key = factor(
      unit_key,
      levels = unit_key
    )
  )

# Print plotting table.
water_by_unit_percent_plot_df


water_by_unit_percent_plot <- ggplot(
  water_by_unit_percent_plot_df,
  aes(
    x = unit_key,
    y = percent_water
  )
) +
  geom_col(
    fill = "grey50"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(scale = 1),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    #title = "Top Management Areas by Percent Water",
    #subtitle = "Management areas with the highest proportion of mapped water",
    x = "WLD management area\n(top 10 by % water)",
    y = "Percent of WLD management area\nclassified as water"
  ) +
  theme_grass()

# Print plot.
water_by_unit_percent_plot


###############################################################################
# 📋 20. EDA and QC Summary Table
###############################################################################
# ⭐ Why this matters:
# This creates one appendix-ready table summarizing the major data quality
# findings that affect interpretation of the openland assessment.

appendix_summary_table <- tibble(
  metric = c(
    "Stand records",
    "Unique stands",
    "Compartments",
    "Management areas",
    "Total mapped acres",
    "Missing L3 cover records",
    "Missing canopy closure records",
    "Management status unspecified or missing",
    "Median stand size",
    "Mean stand size",
    "Maximum stand size",
    "Stands <20 acres",
    "Water stands",
    "Water acres",
    "Water percent of total acres",
    "Mean stand inventory age",
    "Median stand inventory age",
    "Stands inventoried <5 years ago",
    "Stands inventoried 15+ years ago",
    "Pre-1950 temporal records",
    "Largest cover type by acres",
    "Forested records missing sub-canopy cover"
  ),
  
  value = c(
    comma(nrow(stands_working)),
    comma(n_distinct(stand_inventory_age$fcskey)),
    comma(n_distinct(stands_working$fc_key)),
    comma(n_distinct(stands_working$unit_key)),
    comma(sum(stands_working$acres, na.rm = TRUE), accuracy = 1),
    
    paste0(
      comma(missing_summary$missing_values[missing_summary$variable == "L3 Cover Type"]),
      " (",
      number(missing_summary$percent_missing[missing_summary$variable == "L3 Cover Type"], accuracy = 0.1),
      "%)"
    ),
    
    paste0(
      comma(missing_summary$missing_values[missing_summary$variable == "Canopy Closure"]),
      " (",
      number(missing_summary$percent_missing[missing_summary$variable == "Canopy Closure"], accuracy = 0.1),
      "%)"
    ),
    
    paste0(
      comma(
        management_summary$stands[
          management_summary$management_key %in% c("Unspecified", "Missing / NA")
        ] %>% 
          sum()
      ),
      " (",
      percent(
        management_summary$percent_stands[
          management_summary$management_key %in% c("Unspecified", "Missing / NA")
        ] %>% 
          sum(),
        accuracy = 0.1
      ),
      ")"
    ),
    
    paste0(number(acre_summary$median_acres, accuracy = 0.1), " acres"),
    paste0(number(acre_summary$mean_acres, accuracy = 0.1), " acres"),
    paste0(comma(acre_summary$max_acres, accuracy = 1), " acres"),
    
    percent(
      acre_size_df$percent[
        acre_size_df$size_class %in% c("<5 acres", "5–20 acres")
      ] %>% 
        sum(),
      accuracy = 0.1
    ),
    
    comma(water_summary$water_stands),
    paste0(comma(water_summary$water_acres, accuracy = 1), " acres"),
    paste0(number(water_summary$percent_total_acres, accuracy = 0.1), "%"),
    
    paste0(number(stand_inventory_age_summary$mean_age, accuracy = 0.1), " years"),
    paste0(number(stand_inventory_age_summary$median_age, accuracy = 0.1), " years"),
    
    percent(
      age_summary$percent[age_summary$age_class == "0–<5 years"],
      accuracy = 0.1
    ),
    
    percent(
      age_summary$percent[age_summary$age_class == "15+ years"],
      accuracy = 0.1
    ),
    
    comma(sum(pre1950_summary$n, na.rm = TRUE)),
    
    cover_summary$l3cover_key[which.max(cover_summary$acres)],
    
    paste0(
      comma(sub_canopy_no_data_summary$records),
      " (",
      number(sub_canopy_no_data_summary$percent_records, accuracy = 0.1),
      "%)"
    )
  ),
  
  interpretation = c(
    "Total prepared stand records in the Wildlife Division working dataset.",
    "Unique stand identifiers after accounting for repeated fcskey records.",
    "Number of compartments represented in the working dataset.",
    "Number of Wildlife Division management areas represented.",
    "Total mapped acres associated with the working dataset.",
    "Cover-type completeness was high, supporting downstream cover analyses.",
    "Canopy closure was mostly complete but should still be filtered by analysis.",
    "Management status should be interpreted cautiously due to high unspecified/missing values.",
    "Median is more representative than mean because stand sizes are right-skewed.",
    "Mean is inflated by a small number of very large polygons.",
    "Largest records should be reviewed because they can strongly influence acreage summaries.",
    "Most stands are relatively small polygons.",
    "Water is a small share of stand records.",
    "Water is a large share of mapped acreage.",
    "Water should be considered separately in terrestrial vegetation summaries.",
    "Average recency of the most recent inventory record per stand.",
    "Median recency of the most recent inventory record per stand.",
    "Share of stands with relatively recent inventory information.",
    "Share of stands with older inventory information.",
    "Likely legacy, placeholder, or migration-related dates.",
    "Dominant Level-3 cover type by mapped acreage.",
    "Sub-canopy cover was not carried forward because nearly all forested records were missing or coded as zero/no data."
  )
)

# Print table.
appendix_summary_table


###############################################################################
# 💾 21. Save EDA/QC Outputs
###############################################################################
# ⭐ Why this matters:
# This saves the appendix summary table and key QA figures needed to document
# data limitations and support later snapshot analyses.

# Create folder for output.
dir.create(
  "phase1_step2_outputs",
  showWarnings = FALSE
)

write_csv(
  appendix_summary_table,
  "phase1_step2_outputs/appendix_eda_qc_summary_table.csv"
)

ggsave(
  "phase1_step2_outputs/management_area_top10_acres.png",
  unit_plot,
  width = figure_width,
  height = figure_height,
  dpi = figure_dpi
)

ggsave(
  "phase1_step2_outputs/cover_type_top10_comparison.png",
  cover_compare_plot,
  width = figure_width,
  height = figure_height,
  dpi = figure_dpi
)

ggsave(
  "phase1_step2_outputs/canopy_closure_distribution.png",
  canopy_plot,
  width = figure_width,
  height = figure_height,
  dpi = figure_dpi
)

ggsave(
  "phase1_step2_outputs/management_status_distribution.png",
  management_plot,
  width = figure_width,
  height = figure_height,
  dpi = figure_dpi
)

ggsave(
  "phase1_step2_outputs/stand_size_distribution.png",
  acre_size_plot,
  width = figure_width,
  height = figure_height,
  dpi = figure_dpi
)

ggsave(
  "phase1_step2_outputs/stand_size_log_distribution.png",
  log_acre_plot,
  width = figure_width,
  height = figure_height,
  dpi = figure_dpi
)

ggsave(
  "phase1_step2_outputs/inventory_year_distribution.png",
  inventory_year_plot,
  width = figure_width,
  height = figure_height,
  dpi = figure_dpi
)

ggsave(
  "phase1_step2_outputs/date_field_comparison.png",
  date_field_plot,
  width = 8.5,
  height = figure_height,
  dpi = figure_dpi
)

ggsave(
  "phase1_step2_outputs/inventory_age_distribution.png",
  stand_inventory_age_distribution_plot,
  width = figure_width,
  height = figure_height,
  dpi = figure_dpi
)

ggsave(
  "phase1_step2_outputs/inventory_age_class_distribution.png",
  stand_inventory_age_class_plot,
  width = figure_width,
  height = figure_height,
  dpi = figure_dpi
)


###############################################################################
# End of script
###############################################################################

