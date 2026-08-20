###############################################################################
###############################################################################

# 🌾 Openland Assessment Project
# 📊 Phase 1, Step 3 — Latest-Condition Snapshot Analysis
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
# This script builds rolling annual summaries from the prepared MiFI working
# datasets and creates latest-condition stand-level snapshots for analysis.
#
# This script is designed to answer the main Phase 1 questions:
#
#   1) How much Agriculture and Herbaceous Openland does WLD currently have
#      inventoried in MiFI?
#   2) How are Agriculture and Herbaceous Openland acres distributed across WLD
#      management areas?
#
# This script also provides Department-wide context for project-relevant cover
# types, but the primary analysis remains focused on WLD lands.
#
#
# INPUTS
# ------
#   • stands_working_prepped.rds
#       - Prepared WLD working dataset from Phase 1, Step 1.
#
#   • stands_department_working.rds
#       - Prepared Department-wide working dataset from Phase 1, Step 1.
#
#
# OUTPUTS
# -------
# Output folder:
#   • phase1_step3_outputs/
#
# Core tables:
#   • step3_report_summary.csv
#       - Single report-ready summary table focused on how much WLD has and
#         where project-relevant cover classes occur.
#
#   • department_authority_terrestrial_context.csv
#       - Department-wide table showing retained latest-condition terrestrial
#         MiFI snapshot acres by management authority.
#
#   • department_project_cover_authority_context.csv
#       - Department-wide table showing Agriculture and Herbaceous Openland
#         acres by authority, including each authority's contribution to the
#         Department-wide class total and the percent of each authority's
#         retained terrestrial MiFI land base represented by each class.
#
#   • department_retention_summary.csv
#       - Department-wide retention table showing how many records, unique
#         stands, and acres remain after cover-type screening, date screening,
#         latest-snapshot creation, and Water removal.
#
#   • stand_size_summary.csv
#       - Stand-size descriptive statistics for Agriculture and Herbaceous
#         Openland in the latest-condition terrestrial snapshot.
#
#   • wld_management_area_full_summary.csv
#       - Complete WLD management-area table showing inventoried terrestrial
#         acres, Agriculture acres, Herbaceous Openland acres, and percent of
#         each management area's terrestrial MiFI land base represented by those
#         cover classes.
#
# RDS objects:
#   • snapshot_latest.rds
#       - Latest-condition WLD snapshot, including all retained Level-3 cover
#         types.
#
#   • snapshot_terrestrial.rds
#       - Latest-condition WLD terrestrial snapshot, with Water removed.
#
# Figures:
#   • fig_department_total_acres.png
#       - Department-wide MiFI inventory accumulation through time.
#
#   • fig_total_acres.png
#       - WLD MiFI inventory accumulation through time.
#
#   • fig_project_cover_through_time.png
#       - Agriculture and Herbaceous Openland through time as a percent of
#         inventoried terrestrial WLD acres.
#
#   • fig_agriculture_composition.png
#       - Level-3 composition of Agriculture.
#
#   • fig_herbaceous_openland_composition.png
#       - Level-4 composition of Herbaceous Openland.
#
#   • fig_management_area_acres.png
#       - Top management areas by Agriculture and Herbaceous Openland acres.
#
#   • fig_management_area_proportion.png
#       - Management areas where Agriculture and Herbaceous Openland make up the
#         largest share of inventoried terrestrial acres.
#
#   • fig_acre_concentration_bar.png
#       - Percent of total cover class acres represented by the top 5, 10, and
#         20 management areas.
#
#   • fig_stand_size_distribution.png
#       - Distribution of Agriculture and Herbaceous Openland stands by
#         stand-size class.
#
#   • fig_mdnr_authority.png
#       - Distribution of Agriculture and Herbaceous Openland stands by
#         MDNR land-managing authority.
#
#
# IMPORTANT INTERPRETATION NOTES
# ------------------------------
# The working datasets intentionally retain records with missing values,
# unclassified cover types, unspecified canopy classes, and other data-quality
# issues identified during EDA.
#
# Analysis-specific datasets are created throughout this script as needed so
# exclusion criteria remain transparent and reproducible.
#
# MiFI is a rolling inventory database. Changes in mapped acres or stand counts
# through time may reflect inventory expansion, stand boundary updates,
# reclassification, database migration, or changes in data availability rather
# than true ecological change on the ground.
#
# For this reason, annual summaries in this script are used primarily to
# demonstrate inventory accumulation and justify using latest-condition
# snapshots for current-inventory summaries.
#
# Acreage values should be interpreted as inventoried acres in MiFI, not legal
# acreage or verified current ground condition.
#
# This script evaluates inventoried vegetative cover. It does not use MiFI
# management-status attributes to infer whether, when, or how recently
# management occurred.


###############################################################################
# 📦 1. Load Required Packages
###############################################################################
# ⭐ Why this matters:
# This loads the R packages needed to build rolling snapshots, summarize MiFI
# inventory data, create figures, and export report-ready outputs.

library(sf)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(purrr)
library(readr)
library(tibble)
library(tidytext)
library(tidyr)
library(forcats)
library(stringr)

# Prevent scientific notation in printed tables.
options(
  scipen = 999
)


###############################################################################
# 📁 2. Read Prepared Working Datasets
###############################################################################
# ⭐ Why this matters:
# This loads the prepared WLD and Department-wide datasets needed to build the
# rolling snapshots and current-inventory summaries.

# These files were created in Phase 1, Step 1 — Data Preparation.
# They should already include readable domain labels and the selected authority
# subset.

stands_working <- readRDS("phase1_step1_outputs/stands_working_prepped.rds")

stands_department_working <- readRDS("phase1_step1_outputs/stands_department_working.rds")

# Inspect object structure.
glimpse(stands_working)
names(stands_working)

glimpse(stands_department_working)
names(stands_department_working)

# Summarize the size of the prepared Department-wide dataset.
department_summary <- data.frame(
  metric = c(
    "Compartments",
    "Stands",
    "Acres"
  ),
  
  department = c(
    n_distinct(stands_department_working$fc_key),
    nrow(stands_department_working),
    sum(stands_department_working$acres, na.rm = TRUE)
  )
)

# Print summary.
department_summary


###############################################################################
# 🎨 3. Define Global Plot Theme
###############################################################################
# ⭐ Why this matters:
# This makes all figures visually consistent so outputs can be used together in
# reports, presentations, and appendices.

figure_width <- 7.5
figure_height <- 5
figure_dpi <- 300

# Standard color palette.
ag_color <- "goldenrod3"
herb_color <- "springgreen4"
neutral_color <- "grey50"

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
      legend.position = "right",
      
      strip.text = element_text(
        face = "bold"
      )
    )
}


###############################################################################
# 🔎 4A. QA Agriculture Cover-Code Rule
###############################################################################
# ⭐ Why this matters:
# This checks which MiFI cover codes are being classified as Agriculture before
# using the rule in summaries.

stands_working %>%
  st_drop_geometry() %>%
  mutate(
    cover_code_chr = as.character(l4covertype_full)
  ) %>%
  filter(
    stringr::str_starts(cover_code_chr, "2")
  ) %>%
  count(
    l4covertype_full,
    l3covertype,
    l3cover_key,
    l4cover_key,
    sort = TRUE
  )


###############################################################################
# 🏷️ 4B. Define Project Cover Classification Helper
###############################################################################
# ⭐ Why this matters:
# This creates a consistent rule for identifying the two project-relevant MiFI
# cover classes used throughout the analysis.

# Project-relevant cover classes:
#
#   • Agriculture
#       - MiFI Level-1 Agriculture.
#       - Identified using cover-type codes that begin with "2".
#
#   • Herbaceous Openland
#       - MiFI Level-3 Herbaceous Openland.
#
# Agriculture is identified using the original mixed-level MiFI/IFMAP cover
# code. Because the classification is hierarchical, any code beginning with "2"
# is treated as part of the Agriculture branch, regardless of code length.
#
# This classification is based on inventoried vegetative cover only. It does
# not use management-status attributes.


add_project_cover_class <- function(data) {
  data %>%
    mutate(
      cover_code_chr = as.character(l4covertype_full), # Original mixed-level code.
      
      project_cover_class = case_when(
        stringr::str_starts(cover_code_chr, "2") ~
          "Agriculture",
        
        l3cover_key == "Herbaceous Openland" ~
          "Herbaceous Openland",
        
        TRUE ~ NA_character_
      )
    )
}

###############################################################################
# ✂️ 5. Create Cover-Type Analysis Dataset
###############################################################################
# ⭐ Why this matters:
# This removes missing or unassigned Level-3 cover records so cover-type
# summaries are based on interpretable MiFI classifications.

# The working dataset retains missing and unassigned values so exclusions can be
# applied transparently for each analysis.
#
# For cover-type summaries and rolling snapshot analyses, records with missing
# or unassigned Level-3 cover-type classifications are removed.

stands_cover <- stands_working %>%
  filter(
    !is.na(l3cover_key),
    l3covertype != 0
  ) %>%
  add_project_cover_class()

# QA project cover classification after cover-type filtering.
project_cover_rule_qa <- stands_cover %>%
  st_drop_geometry() %>%
  filter(
    !is.na(project_cover_class)
  ) %>%
  count(
    project_cover_class,
    cover_code_chr,
    l3covertype,
    l3cover_key,
    l4cover_key,
    sort = TRUE
  )

# Print QA table.
project_cover_rule_qa

# Summarize the effect of cover-type filtering.
cover_filter_summary <- data.frame(
  dataset = c(
    "Working dataset",
    "Cover-type analysis dataset"
  ),
  
  rows = c(
    nrow(stands_working),
    nrow(stands_cover)
  ),
  
  unique_stands = c(
    n_distinct(stands_working$fcskey),
    n_distinct(stands_cover$fcskey)
  ),
  
  acres = c(
    sum(stands_working$acres, na.rm = TRUE),
    sum(stands_cover$acres, na.rm = TRUE)
  )
) %>%
  mutate(
    percent_rows_retained =
      100 * rows / rows[dataset == "Working dataset"],
    
    percent_stands_retained =
      100 * unique_stands /
      unique_stands[dataset == "Working dataset"],
    
    percent_acres_retained =
      100 * acres /
      acres[dataset == "Working dataset"]
  )

# Print summary.
cover_filter_summary


###############################################################################
# 📅 6. Prepare Inventory Dates for Rolling Snapshot
###############################################################################
# ⭐ Why this matters:
# This prepares the inventory year field used to build rolling annual snapshots
# and avoid treating MiFI as a fixed monitoring dataset.

# The "date" field appeared most representative of inventory timing during EDA.
# This script therefore uses "date" to construct annual rolling snapshots.
#
# Records with implausible or missing inventory years are removed for this
# analysis.

baseline_year <- 2015
current_year <- lubridate::year(Sys.Date())

stands_time_tbl <- stands_cover %>%
  st_drop_geometry() %>%
  mutate(
    inventory_year_raw = lubridate::year(date),
    
    inventory_year = if_else(
      inventory_year_raw <= baseline_year,
      baseline_year,
      inventory_year_raw
    )
  ) %>%
  filter(
    !is.na(inventory_year_raw),
    inventory_year_raw >= 1950,
    inventory_year_raw <= current_year
  )

# Summarize temporal coverage after filtering.
stands_time_summary <- stands_time_tbl %>%
  summarise(
    total_rows = n(),
    unique_stands = n_distinct(fcskey),
    min_inventory_year_raw = min(inventory_year_raw, na.rm = TRUE),
    max_inventory_year_raw = max(inventory_year_raw, na.rm = TRUE),
    min_analysis_year = min(inventory_year, na.rm = TRUE),
    max_analysis_year = max(inventory_year, na.rm = TRUE)
  )

# Print summary.
stands_time_summary

# Count records by analysis year.
stands_time_tbl %>%
  count(
    inventory_year,
    sort = FALSE
  )


###############################################################################
# 🧹 7. Deduplicate and QA Stand-Year Records
###############################################################################
# ⭐ Why this matters:
# This prevents duplicate stand-year records from inflating annual snapshot
# summaries.

# The rolling snapshot is built at an annual time step. Therefore, each stand
# should occur only once per inventory year.
#
# If the same stand appears more than once within the same inventory year, keep
# only one record for that stand-year. This prevents over-counting stands in
# annual summaries.

stands_time_tbl <- stands_time_tbl %>%
  arrange(
    fcskey,
    inventory_year_raw,
    desc(date)
  ) %>%
  distinct(
    fcskey,
    inventory_year_raw,
    .keep_all = TRUE
  )

stand_record_summary <- stands_time_tbl %>%
  summarise(
    rows_after_filtering = n(),
    unique_fcskey_years = n_distinct(paste(fcskey, inventory_year_raw)),
    unique_stands = n_distinct(fcskey),
    
    duplicate_stand_year_rows =
      rows_after_filtering - unique_fcskey_years,
    
    repeated_stand_rows =
      rows_after_filtering - unique_stands
  )

# Print summary.
stand_record_summary

# Optional QA table: identify stands that still occur in more than one year.
repeated_stands <- stands_time_tbl %>%
  count(
    fcskey,
    sort = TRUE
  ) %>%
  filter(
    n > 1
  )

# Print repeated stands.
repeated_stands


###############################################################################
# 📅 8. Create Analysis Year Sequence
###############################################################################
# ⭐ Why this matters:
# This defines the annual time steps used to show how the MiFI inventory
# accumulates through time.

analysis_years <- tibble(
  inventory_year = seq(
    from = min(stands_time_tbl$inventory_year, na.rm = TRUE),
    to   = max(stands_time_tbl$inventory_year, na.rm = TRUE),
    by   = 1
  )
)

# Print analysis years.
analysis_years


###############################################################################
# 🧠 9. Build Rolling Annual Snapshots by Level-3 Cover Type
###############################################################################
# ⭐ Why this matters:
# This builds the rolling inventory used to show how mapped cover-type acres
# change as additional MiFI records become available.

# For each analysis year:
#
#   • Keep all records where raw inventory year is <= that year.
#   • Within each stand, keep the newest available record.
#   • Summarize stands and acres by Level-3 cover type.
#
# This creates an all-cover summary first. Cover groups can be filtered later
# without rebuilding the rolling snapshot.

annual_cover_summary <- map_dfr(
  analysis_years$inventory_year,
  function(current_analysis_year) {
    
    snapshot_tbl <- stands_time_tbl %>%
      filter(
        inventory_year_raw <= current_analysis_year
      ) %>%
      arrange(
        fcskey,
        desc(inventory_year_raw),
        desc(date)
      ) %>%
      distinct(
        fcskey,
        .keep_all = TRUE
      )
    
    snapshot_tbl %>%
      group_by(
        inventory_year = current_analysis_year,
        l3covertype,
        l3cover_key,
        authority_key
      ) %>%
      summarise(
        stands_total = n(),
        acres_total = sum(acres, na.rm = TRUE),
        .groups = "drop"
      )
  }
) %>%
  group_by(
    inventory_year
  ) %>%
  mutate(
    total_stands_all_cover = sum(stands_total, na.rm = TRUE),
    total_acres_all_cover = sum(acres_total, na.rm = TRUE),
    percent_of_total_stands = 100 * stands_total / total_stands_all_cover,
    percent_of_total_acres = 100 * acres_total / total_acres_all_cover,
    
    year_label = if_else(
      inventory_year == baseline_year,
      paste0("\u2264", baseline_year),
      as.character(inventory_year)
    )
  ) %>%
  ungroup()

# Print summary.
annual_cover_summary


###############################################################################
# 🌄 10. Create Latest Available Snapshot Table
###############################################################################
# ⭐ Why this matters:
# This creates the latest-condition WLD snapshot used to answer the current
# inventory questions.

# This is the current best-known stand-level snapshot.
# It keeps both Level-3 and full-code labels for later filtering and QA.

snapshot_latest_tbl <- stands_time_tbl %>%
  arrange(
    fcskey,
    desc(inventory_year_raw),
    desc(date)
  ) %>%
  distinct(
    fcskey,
    .keep_all = TRUE
  ) %>%
  add_project_cover_class()

# Inspect latest snapshot.
glimpse(snapshot_latest_tbl)


###############################################################################
# 🏛️ 11A. Prepare Department-Wide Cover-Type Snapshot Dataset
###############################################################################
# ⭐ Why this matters:
# This prepares Department-wide data using the same snapshot rules so WLD acres
# can be interpreted within the broader Department MiFI inventory.

department_stands_cover <- stands_department_working %>%
  filter(
    !is.na(l3cover_key),
    l3covertype != 0
  ) %>%
  add_project_cover_class()

# QA Department-wide project cover classification after cover-type filtering.
department_project_cover_rule_qa <- department_stands_cover %>%
  filter(
    !is.na(project_cover_class)
  ) %>%
  count(
    project_cover_class,
    cover_code_chr,
    l3covertype,
    l3cover_key,
    l4cover_key,
    sort = TRUE
  )

# Print QA table.
department_project_cover_rule_qa

# Summarize the effect of Department-wide cover-type filtering.
department_cover_filter_summary <- data.frame(
  dataset = c(
    "Department working dataset",
    "Department cover-type analysis dataset"
  ),
  
  rows = c(
    nrow(stands_department_working),
    nrow(department_stands_cover)
  ),
  
  unique_stands = c(
    n_distinct(stands_department_working$fcskey),
    n_distinct(department_stands_cover$fcskey)
  ),
  
  acres = c(
    sum(stands_department_working$acres, na.rm = TRUE),
    sum(department_stands_cover$acres, na.rm = TRUE)
  )
) %>%
  mutate(
    percent_rows_retained =
      100 * rows / rows[dataset == "Department working dataset"],
    
    percent_stands_retained =
      100 * unique_stands /
      unique_stands[dataset == "Department working dataset"],
    
    percent_acres_retained =
      100 * acres /
      acres[dataset == "Department working dataset"]
  )

# Print summary.
department_cover_filter_summary

# Prepare Department-wide inventory dates using the same rules as the Wildlife
# Division rolling snapshot above.
department_stands_time_tbl <- department_stands_cover %>%
  mutate(
    inventory_year_raw = lubridate::year(date),
    
    inventory_year = if_else(
      inventory_year_raw <= baseline_year,
      baseline_year,
      inventory_year_raw
    )
  ) %>%
  filter(
    !is.na(inventory_year_raw),
    inventory_year_raw >= 1950,
    inventory_year_raw <= current_year
  )

# Summarize temporal coverage after filtering.
department_stands_time_summary <- department_stands_time_tbl %>%
  summarise(
    total_rows = n(),
    unique_stands = n_distinct(fcskey),
    min_inventory_year_raw = min(inventory_year_raw, na.rm = TRUE),
    max_inventory_year_raw = max(inventory_year_raw, na.rm = TRUE),
    min_analysis_year = min(inventory_year, na.rm = TRUE),
    max_analysis_year = max(inventory_year, na.rm = TRUE)
  )

# Print summary.
department_stands_time_summary

# Deduplicate Department-wide stand-year records.
department_stands_time_tbl <- department_stands_time_tbl %>%
  arrange(
    fcskey,
    inventory_year_raw,
    desc(date)
  ) %>%
  distinct(
    fcskey,
    inventory_year_raw,
    .keep_all = TRUE
  )

# Summarize Department-wide stand-year deduplication.
department_stand_record_summary <- department_stands_time_tbl %>%
  summarise(
    rows_after_filtering = n(),
    unique_fcskey_years = n_distinct(paste(fcskey, inventory_year_raw)),
    unique_stands = n_distinct(fcskey),
    
    duplicate_stand_year_rows =
      rows_after_filtering - unique_fcskey_years,
    
    repeated_stand_rows =
      rows_after_filtering - unique_stands
  )

# Print summary.
department_stand_record_summary


###############################################################################
# 🏛️ 11B. Department-Wide Rolling Annual Snapshots
###############################################################################
# ⭐ Why this matters:
# This shows how the Department-wide MiFI inventory accumulates through time and
# supports using a latest-condition snapshot for Department context.

department_analysis_years <- tibble(
  inventory_year = seq(
    from = min(department_stands_time_tbl$inventory_year, na.rm = TRUE),
    to   = max(department_stands_time_tbl$inventory_year, na.rm = TRUE),
    by   = 1
  )
)

# Print analysis years.
department_analysis_years

department_annual_cover_summary <- map_dfr(
  department_analysis_years$inventory_year,
  function(current_analysis_year) {
    
    snapshot_tbl <- department_stands_time_tbl %>%
      filter(
        inventory_year_raw <= current_analysis_year
      ) %>%
      arrange(
        fcskey,
        desc(inventory_year_raw),
        desc(date)
      ) %>%
      distinct(
        fcskey,
        .keep_all = TRUE
      )
    
    snapshot_tbl %>%
      group_by(
        inventory_year = current_analysis_year,
        l3covertype,
        l3cover_key,
        authority_key
      ) %>%
      summarise(
        stands_total = n(),
        acres_total = sum(acres, na.rm = TRUE),
        .groups = "drop"
      )
  }
) %>%
  group_by(
    inventory_year
  ) %>%
  mutate(
    total_stands_all_cover = sum(stands_total, na.rm = TRUE),
    total_acres_all_cover = sum(acres_total, na.rm = TRUE),
    percent_of_total_stands = 100 * stands_total / total_stands_all_cover,
    percent_of_total_acres = 100 * acres_total / total_acres_all_cover,
    
    year_label = if_else(
      inventory_year == baseline_year,
      paste0("\u2264", baseline_year),
      as.character(inventory_year)
    )
  ) %>%
  ungroup()

# Print summary.
department_annual_cover_summary

# Pull one row per year from the Department-wide annual cover summary.
department_annual_total_summary <- department_annual_cover_summary %>%
  distinct(
    inventory_year,
    year_label,
    total_stands_all_cover,
    total_acres_all_cover
  )

# Print summary.
department_annual_total_summary


###############################################################################
# 📈 11C. Plot Department-Wide Inventoried Acres Over Time
###############################################################################
# ⭐ Why this matters:
# This provides Department-wide context for MiFI as a rolling inventory rather
# than a fixed monitoring dataset.

p_department_total_acres <- ggplot(
  department_annual_total_summary,
  aes(
    x = inventory_year,
    y = total_acres_all_cover
  )
) +
  geom_line(
    linewidth = 1,
    color = neutral_color
  ) +
  geom_point(
    size = 2,
    color = neutral_color
  ) +
  scale_x_continuous(
    breaks = department_annual_total_summary$inventory_year,
    labels = department_annual_total_summary$year_label
  ) +
  scale_y_continuous(
    labels = comma_format()
  ) +
  labs(
    x = "Year",
    y = "Cumulative inventoried Department acres\n(in MiFI)"
  ) +
  theme_grass()

# Print plot.
p_department_total_acres


###############################################################################
# 🏛️ 11D. Create Department-Wide Latest Available Snapshot Table
###############################################################################
# ⭐ Why this matters:
# This creates the latest-condition Department-wide snapshot used for appendix
# context and authority-level comparisons.

department_snapshot_latest_tbl <- department_stands_time_tbl %>%
  arrange(
    fcskey,
    desc(inventory_year_raw),
    desc(date)
  ) %>%
  distinct(
    fcskey,
    .keep_all = TRUE
  ) %>%
  add_project_cover_class()

# Summarize Department-wide snapshot for QA.
department_snapshot_summary <- department_snapshot_latest_tbl %>%
  summarise(
    snapshot_stands = n(),
    snapshot_unique_stands = n_distinct(fcskey),
    snapshot_acres = sum(acres, na.rm = TRUE),
    min_inventory_year = min(inventory_year_raw, na.rm = TRUE),
    max_inventory_year = max(inventory_year_raw, na.rm = TRUE)
  )

# Print summaries.
department_snapshot_summary
glimpse(department_snapshot_latest_tbl)


###############################################################################
# 🏛️ 11E. Create Department-Wide Terrestrial Latest Snapshot
###############################################################################
# ⭐ Why this matters:
# This creates the Department-wide terrestrial snapshot used for Department
# context summaries.

# The Department-wide context is based on the retained latest-condition MiFI
# snapshot, using the same general screening logic as the Wildlife Division
# analysis.
#
# Water is removed from authority-level land-base denominators so Agriculture
# and Herbaceous Openland are compared against retained inventoried terrestrial
# acres.

department_snapshot_tbl <- department_snapshot_latest_tbl %>%
  mutate(
    l3cover_key = if_else(
      is.na(l3cover_key),
      "Missing / NA",
      l3cover_key
    ),
    
    authority_key = if_else(
      is.na(authority_key),
      "Missing / NA",
      authority_key
    )
  )

department_snapshot_terrestrial_tbl <- department_snapshot_tbl %>%
  filter(
    l3cover_key != "Water"
  )

# Summarize the effect of removing Water from the Department-wide snapshot.
department_terrestrial_filter_summary <- data.frame(
  dataset = c(
    "Department latest snapshot",
    "Department latest terrestrial snapshot"
  ),
  
  stands = c(
    nrow(department_snapshot_tbl),
    nrow(department_snapshot_terrestrial_tbl)
  ),
  
  acres = c(
    sum(department_snapshot_tbl$acres, na.rm = TRUE),
    sum(department_snapshot_terrestrial_tbl$acres, na.rm = TRUE)
  )
) %>%
  mutate(
    percent_stands_retained =
      100 * stands / stands[dataset == "Department latest snapshot"],
    
    percent_acres_retained =
      100 * acres / acres[dataset == "Department latest snapshot"]
  )

# Print summary.
department_terrestrial_filter_summary


###############################################################################
# 🏛️ 11F. Department-Wide Retention and Exclusion Summary
###############################################################################
# ⭐ Why this matters:
# This documents how many Department-wide records and acres are retained after
# cover-type screening, date screening, latest-snapshot creation, and Water
# removal.

# This table helps explain why the Department-wide context does not represent
# the full raw Department extract. It represents the retained latest-condition
# terrestrial MiFI snapshot that can be consistently assigned to cover type,
# inventory year, and administrative authority.

department_retention_summary <- tibble(
  step = c(
    "Department working dataset",
    "After cover-type screening",
    "After date screening",
    "Latest-condition snapshot",
    "Latest-condition terrestrial snapshot"
  ),
  
  rows_or_stands = c(
    nrow(stands_department_working),
    nrow(department_stands_cover),
    nrow(department_stands_time_tbl),
    nrow(department_snapshot_tbl),
    nrow(department_snapshot_terrestrial_tbl)
  ),
  
  unique_stands = c(
    n_distinct(stands_department_working$fcskey),
    n_distinct(department_stands_cover$fcskey),
    n_distinct(department_stands_time_tbl$fcskey),
    n_distinct(department_snapshot_tbl$fcskey),
    n_distinct(department_snapshot_terrestrial_tbl$fcskey)
  ),
  
  acres = c(
    sum(stands_department_working$acres, na.rm = TRUE),
    sum(department_stands_cover$acres, na.rm = TRUE),
    sum(department_stands_time_tbl$acres, na.rm = TRUE),
    sum(department_snapshot_tbl$acres, na.rm = TRUE),
    sum(department_snapshot_terrestrial_tbl$acres, na.rm = TRUE)
  )
) %>%
  mutate(
    percent_rows_or_stands_retained =
      100 * rows_or_stands / rows_or_stands[step == "Department working dataset"],
    
    percent_unique_stands_retained =
      100 * unique_stands / unique_stands[step == "Department working dataset"],
    
    percent_acres_retained =
      100 * acres / acres[step == "Department working dataset"]
  )

# Print summary.
department_retention_summary


###############################################################################
# ✅ 12. QA Annual Cover Summary
###############################################################################
# ⭐ Why this matters:
# This checks the rolling snapshot outputs before they are used for figures,
# summaries, or interpretation.

# Look at the front end of the results table.
annual_cover_summary %>%
  slice_head(
    n = 10
  )

# Look at the tail end of the results table.
annual_cover_summary %>%
  slice_tail(
    n = 10
  )

# Summarize ranges in annual cover summary.
annual_cover_summary %>%
  summarise(
    min_total_stands_all_cover = min(total_stands_all_cover, na.rm = TRUE),
    max_total_stands_all_cover = max(total_stands_all_cover, na.rm = TRUE),
    min_total_acres_all_cover = min(total_acres_all_cover, na.rm = TRUE),
    max_total_acres_all_cover = max(total_acres_all_cover, na.rm = TRUE),
    min_cover_acres = min(acres_total, na.rm = TRUE),
    max_cover_acres = max(acres_total, na.rm = TRUE)
  )

# Look at the number of cover types inventoried by year.
annual_cover_summary %>%
  count(
    inventory_year,
    sort = FALSE
  )


###############################################################################
# 📊 13. Create Annual Total Summary
###############################################################################
# ⭐ Why this matters:
# This creates the annual total acreage summary used to show inventory growth
# through time.

annual_total_summary <- annual_cover_summary %>%
  distinct(
    inventory_year,
    year_label,
    total_stands_all_cover,
    total_acres_all_cover
  )

# Print summary.
annual_total_summary


###############################################################################
# 📈 14. Plot Inventoried Acres Over Time
###############################################################################
# ⭐ Why this matters:
# This helps show that WLD MiFI acres accumulated through time and should be
# interpreted as a rolling inventory, not direct ecological change.

p_total_acres <- ggplot(
  annual_total_summary,
  aes(
    x = inventory_year,
    y = total_acres_all_cover
  )
) +
  geom_line(
    linewidth = 1,
    color = neutral_color
  ) +
  geom_point(
    size = 2,
    color = neutral_color
  ) +
  scale_x_continuous(
    breaks = annual_total_summary$inventory_year,
    labels = annual_total_summary$year_label
  ) +
  scale_y_continuous(
    labels = comma_format()
  ) +
  labs(
    x = "Year",
    y = "Cumulative inventoried WLD acres\n(in MiFI)"
  ) +
  theme_grass()

# Print plot.
p_total_acres


###############################################################################
# 📈 15. Plot Inventoried Stands Over Time
###############################################################################
# ⭐ Why this matters:
# This checks whether stand counts changed through time along with mapped acres.
# If this plot is not needed for reporting, it can remain internal or be removed.

p_total_stands <- ggplot(
  annual_total_summary,
  aes(
    x = inventory_year,
    y = total_stands_all_cover
  )
) +
  geom_line(
    linewidth = 1,
    color = neutral_color
  ) +
  geom_point(
    size = 2,
    color = neutral_color
  ) +
  scale_x_continuous(
    breaks = annual_total_summary$inventory_year,
    labels = annual_total_summary$year_label
  ) +
  scale_y_continuous(
    labels = comma_format()
  ) +
  labs(
    x = "Year",
    y = "Cumulative inventoried WLD stands\n(in MiFI)"
  ) +
  theme_grass()

# Print plot.
p_total_stands


###############################################################################
# 📊 16. Create Latest Acres by Level-3 Cover Type Table
###############################################################################
# ⭐ Why this matters:
# This identifies the largest current WLD Level-3 cover types and provides the
# basis for broader current-inventory context.

cover_latest_l3_table <- annual_cover_summary %>%
  filter(
    inventory_year == max(inventory_year)
  ) %>%
  arrange(
    desc(acres_total)
  ) %>%
  select(
    inventory_year,
    authority_key,
    l3covertype,
    l3cover_key,
    stands_total,
    acres_total,
    percent_of_total_stands,
    percent_of_total_acres
  )

# Print table.
cover_latest_l3_table

# QA latest snapshot totals.
snapshot_latest_tbl %>%
  summarise(
    latest_snapshot_stands = n(),
    latest_snapshot_acres = sum(acres, na.rm = TRUE)
  )


###############################################################################
# 📈 17. Plot Top Level-3 Cover Types Through Time
###############################################################################
# ⭐ Why this matters:
# This shows how the largest WLD Level-3 cover types accumulated through time.
# This is useful for QA/context but may not be needed as a final report figure.

top12_l3_cover <- cover_latest_l3_table %>%
  slice_head(
    n = 12
  ) %>%
  pull(
    l3cover_key
  )

annual_cover_top12 <- annual_cover_summary %>%
  filter(
    l3cover_key %in% top12_l3_cover
  ) %>%
  mutate(
    l3cover_key = factor(
      l3cover_key,
      levels = top12_l3_cover
    )
  )

# Plot with different y-axis scales.
p_l3_cover_acres <- ggplot(
  annual_cover_top12,
  aes(
    x = inventory_year,
    y = acres_total
  )
) +
  geom_line(
    linewidth = 1.1,
    color = neutral_color
  ) +
  geom_point(
    size = 1.6,
    color = neutral_color
  ) +
  facet_wrap(
    ~l3cover_key,
    scales = "free_y",
    ncol = 4
  ) +
  scale_x_continuous(
    breaks = annual_total_summary$inventory_year,
    labels = annual_total_summary$year_label
  ) +
  scale_y_continuous(
    labels = comma_format()
  ) +
  labs(
    x = "Year",
    y = "Cumulative inventoried WLD acres\n(top 12 by WLD acres in MiFI)"
  ) +
  theme_grass() +
  theme(
    strip.text = element_text(
      face = "bold",
      size = 10
    ),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

# Print plot.
p_l3_cover_acres


###############################################################################
# 🌱 18. Create Terrestrial Latest Snapshot
###############################################################################
# ⭐ Why this matters:
# This removes Water so the main summaries focus on terrestrial vegetation and
# land-cover classes.

snapshot_terrestrial_tbl <- snapshot_latest_tbl %>%
  filter(
    l3cover_key != "Water"
  )

# Summarize the effect of removing Water.
terrestrial_filter_summary <- data.frame(
  dataset = c(
    "Latest snapshot",
    "Latest terrestrial snapshot"
  ),
  
  stands = c(
    nrow(snapshot_latest_tbl),
    nrow(snapshot_terrestrial_tbl)
  ),
  
  acres = c(
    sum(snapshot_latest_tbl$acres, na.rm = TRUE),
    sum(snapshot_terrestrial_tbl$acres, na.rm = TRUE)
  )
) %>%
  mutate(
    percent_stands_retained =
      100 * stands / stands[dataset == "Latest snapshot"],
    
    percent_acres_retained =
      100 * acres / acres[dataset == "Latest snapshot"]
  )

# Print summary.
terrestrial_filter_summary


###############################################################################
# 🧪 18A. QA WLD Snapshot Consistency Across Workflows
###############################################################################
# ⭐ Why this matters:
# This checks whether the primary WLD latest-condition terrestrial snapshot
# matches the Wildlife subset of the Department-wide latest-condition
# terrestrial snapshot.

# The WLD workflow and Department-wide workflow should produce the same Wildlife
# terrestrial acreage if they use the same source records, cover-type screening,
# date screening, latest-record rule, and Water exclusion.

drop_geometry_if_present <- function(data) {
  if (inherits(data, "sf")) {
    sf::st_drop_geometry(data)
  } else {
    data
  }
}

# Create WLD comparison table.
wld_snapshot_compare <- snapshot_terrestrial_tbl %>%
  drop_geometry_if_present() %>%
  mutate(
    fcskey = as.character(fcskey),
    acres = as.numeric(acres)
  ) %>%
  select(
    fcskey,
    acres,
    l3cover_key,
    l4cover_key,
    project_cover_class,
    date,
    inventory_year_raw
  )

# Create Department-wide Wildlife comparison table.
department_wld_snapshot_compare <- department_snapshot_terrestrial_tbl %>%
  filter(
    authority_key == "Wildlife"
  ) %>%
  drop_geometry_if_present() %>%
  mutate(
    fcskey = as.character(fcskey),
    acres = as.numeric(acres)
  ) %>%
  select(
    fcskey,
    acres,
    l3cover_key,
    l4cover_key,
    project_cover_class,
    date,
    inventory_year_raw
  )

# Compare overall totals.
wld_snapshot_total_compare <- tibble(
  workflow = c(
    "WLD workflow",
    "Department workflow Wildlife subset"
  ),
  
  stands = c(
    nrow(wld_snapshot_compare),
    nrow(department_wld_snapshot_compare)
  ),
  
  unique_stands = c(
    n_distinct(wld_snapshot_compare$fcskey),
    n_distinct(department_wld_snapshot_compare$fcskey)
  ),
  
  acres = c(
    sum(wld_snapshot_compare$acres, na.rm = TRUE),
    sum(department_wld_snapshot_compare$acres, na.rm = TRUE)
  )
)

# Identify records present in one workflow but not the other.
wld_only_stands <- wld_snapshot_compare %>%
  anti_join(
    department_wld_snapshot_compare,
    by = "fcskey"
  )

department_wld_only_stands <- department_wld_snapshot_compare %>%
  anti_join(
    wld_snapshot_compare,
    by = "fcskey"
  )

# Compare matching records.
matching_wld_stands_compare <- wld_snapshot_compare %>%
  rename_with(
    ~ paste0(.x, "_wld"),
    -fcskey
  ) %>%
  inner_join(
    department_wld_snapshot_compare %>%
      rename_with(
        ~ paste0(.x, "_department"),
        -fcskey
      ),
    by = "fcskey"
  ) %>%
  mutate(
    acre_difference =
      acres_department - acres_wld,
    
    same_acres =
      dplyr::near(acres_department, acres_wld),
    
    same_l3cover =
      coalesce(l3cover_key_department, "Missing / NA") ==
      coalesce(l3cover_key_wld, "Missing / NA"),
    
    same_l4cover =
      coalesce(l4cover_key_department, "Missing / NA") ==
      coalesce(l4cover_key_wld, "Missing / NA"),
    
    same_project_cover_class =
      coalesce(project_cover_class_department, "Missing / NA") ==
      coalesce(project_cover_class_wld, "Missing / NA"),
    
    same_date =
      date_department == date_wld
  )

# Create compact QA summary.
wld_department_consistency_summary <- tibble(
  check = c(
    "WLD workflow terrestrial acres",
    "Department workflow Wildlife terrestrial acres",
    "Department minus WLD terrestrial acres",
    "WLD workflow terrestrial stands",
    "Department workflow Wildlife terrestrial stands",
    "Department minus WLD terrestrial stands",
    "Stands only in WLD workflow",
    "Stands only in Department workflow",
    "Matching stands with acre differences",
    "Net acre difference among matching stands",
    "Matching stands with L3 cover differences",
    "Matching stands with L4 cover differences",
    "Matching stands with project-class differences",
    "Matching stands with latest-date differences"
  ),
  
  value = c(
    sum(wld_snapshot_compare$acres, na.rm = TRUE),
    sum(department_wld_snapshot_compare$acres, na.rm = TRUE),
    sum(department_wld_snapshot_compare$acres, na.rm = TRUE) -
      sum(wld_snapshot_compare$acres, na.rm = TRUE),
    nrow(wld_snapshot_compare),
    nrow(department_wld_snapshot_compare),
    nrow(department_wld_snapshot_compare) -
      nrow(wld_snapshot_compare),
    nrow(wld_only_stands),
    nrow(department_wld_only_stands),
    sum(!matching_wld_stands_compare$same_acres, na.rm = TRUE),
    sum(matching_wld_stands_compare$acre_difference, na.rm = TRUE),
    sum(!matching_wld_stands_compare$same_l3cover, na.rm = TRUE),
    sum(!matching_wld_stands_compare$same_l4cover, na.rm = TRUE),
    sum(!matching_wld_stands_compare$same_project_cover_class, na.rm = TRUE),
    sum(!matching_wld_stands_compare$same_date, na.rm = TRUE)
  )
)

# Print QA summaries.
wld_snapshot_total_compare
wld_department_consistency_summary


###############################################################################
# 🌾 19. Current Agriculture and Herbaceous Openland Summary
###############################################################################
# ⭐ Question this helps answer:
# How much Agriculture and Herbaceous Openland does WLD currently have in MiFI?

# Create terrestrial totals.
terrestrial_total <- snapshot_terrestrial_tbl %>%
  summarise(
    total_terrestrial_stands = n(),
    total_terrestrial_acres = sum(acres, na.rm = TRUE)
  )

# Summarize project-relevant cover classes.
project_cover_summary <- snapshot_terrestrial_tbl %>%
  filter(
    !is.na(project_cover_class)
  ) %>%
  group_by(
    project_cover_class
  ) %>%
  summarise(
    stands = n(),
    acres = sum(acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percent_terrestrial_stands =
      100 * stands / terrestrial_total$total_terrestrial_stands,
    
    percent_terrestrial_acres =
      100 * acres / terrestrial_total$total_terrestrial_acres
  ) %>%
  arrange(
    desc(acres)
  )

# Print summary.
project_cover_summary


###############################################################################
# 📈 20. Agriculture and Herbaceous Openland Through Time
###############################################################################
# ⭐ Question this helps answer:
# Do Agriculture and Herbaceous Openland appear stable through time, or do their
# mapped acres change as the rolling inventory accumulates?

annual_project_cover_summary <- map_dfr(
  analysis_years$inventory_year,
  function(current_analysis_year) {
    
    snapshot_tbl <- stands_time_tbl %>%
      filter(
        inventory_year_raw <= current_analysis_year
      ) %>%
      arrange(
        fcskey,
        desc(inventory_year_raw),
        desc(date)
      ) %>%
      distinct(
        fcskey,
        .keep_all = TRUE
      ) %>%
      add_project_cover_class() %>%
      filter(
        l3cover_key != "Water"
      )
    
    total_terrestrial_acres <- sum(snapshot_tbl$acres, na.rm = TRUE)
    total_terrestrial_stands <- nrow(snapshot_tbl)
    
    snapshot_tbl %>%
      filter(
        !is.na(project_cover_class)
      ) %>%
      group_by(
        inventory_year = current_analysis_year,
        project_cover_class
      ) %>%
      summarise(
        stands = n(),
        acres = sum(acres, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        total_terrestrial_stands = total_terrestrial_stands,
        total_terrestrial_acres = total_terrestrial_acres,
        
        percent_terrestrial_stands =
          100 * stands / total_terrestrial_stands,
        
        percent_terrestrial_acres =
          100 * acres / total_terrestrial_acres,
        
        year_label = if_else(
          inventory_year == baseline_year,
          paste0("\u2264", baseline_year),
          as.character(inventory_year)
        )
      )
  }
)

# Print summary.
annual_project_cover_summary

project_cover_through_time_plot <- ggplot(
  annual_project_cover_summary,
  aes(
    x = inventory_year,
    y = percent_terrestrial_acres,
    color = project_cover_class
  )
) +
  geom_line(
    linewidth = 1.2
  ) +
  geom_point(
    size = 2
  ) +
  facet_wrap(
    ~project_cover_class,
    scales = "fixed"
  ) +
  scale_color_manual(
    values = c(
      "Agriculture" = ag_color,
      "Herbaceous Openland" = herb_color
    )
  ) +
  scale_x_continuous(
    breaks = annual_total_summary$inventory_year,
    labels = annual_total_summary$year_label
  ) +
  scale_y_continuous(
    labels = percent_format(scale = 1)
  ) +
  labs(
    x = "Year",
    y = "Percent of cumulative WLD inventory\n(terrestrial WLD acres in MiFI)"
  ) +
  theme_grass() +
  theme(
    legend.position = "none",
    
    strip.text = element_text(
      face = "bold"
    ),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

# Print plot.
project_cover_through_time_plot


###############################################################################
# 🌽 21. Agriculture Composition
###############################################################################
# ⭐ Question this helps answer:
# What MiFI Level-3 cover types make up mapped Agriculture?

# Summarize Agriculture by Level-3 cover type.
#
# Agriculture is evaluated at Level 1 for the main analysis, but this table
# documents the Level-3 cover types included within Agriculture.

agriculture_composition <- snapshot_terrestrial_tbl %>%
  filter(
    project_cover_class == "Agriculture"
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
    percent_of_agriculture_acres =
      100 * acres / sum(acres, na.rm = TRUE)
  ) %>%
  arrange(
    desc(acres)
  )

# Print summary.
agriculture_composition


###############################################################################
# 🌿 22. Herbaceous Openland Composition
###############################################################################
# ⭐ Question this helps answer:
# What Level-4 cover types make up mapped Herbaceous Openland?

# Summarize Herbaceous Openland by Level-4 cover type.

herbaceous_openland_composition <- snapshot_terrestrial_tbl %>%
  filter(
    project_cover_class == "Herbaceous Openland"
  ) %>%
  mutate(
    l4cover_key = if_else(
      is.na(l4cover_key),
      "Missing / NA",
      l4cover_key
    )
  ) %>%
  group_by(
    l4cover_key
  ) %>%
  summarise(
    stands = n(),
    acres = sum(acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percent_of_openland_acres =
      100 * acres / sum(acres, na.rm = TRUE)
  ) %>%
  arrange(
    desc(acres)
  )

# Print summary.
herbaceous_openland_composition


###############################################################################
# 📊 23. Plot Composition of Agriculture and Herbaceous Openland
###############################################################################
# ⭐ Question this helps answer:
# What finer-scale MiFI classifications make up inventoried Agriculture and
# Herbaceous Openland?
#
# Agriculture is summarized using Level-3 cover types, while Herbaceous Openland
# is summarized using Level-4 cover types. Percentages are calculated within
# each broader cover class so both panels can be displayed on a common 0–100%
# scale.
#
# Panel headings report the total inventoried WLD acreage represented by each
# broader cover class in the latest-condition terrestrial snapshot.
#
# Long cover-type labels are wrapped to improve readability when the figure is
# inserted into the report at a reduced size.
###############################################################################


###############################################################################
# 23A. Prepare Agriculture Composition
###############################################################################

agriculture_composition_plot_df <- agriculture_composition %>%
  transmute(
    project_cover_class = "Agriculture",
    cover_type = l3cover_key,
    acres = acres,
    percent_class_acres = percent_of_agriculture_acres
  )


###############################################################################
# 23B. Prepare Herbaceous Openland Composition
###############################################################################

herbaceous_composition_plot_df <- herbaceous_openland_composition %>%
  transmute(
    project_cover_class = "Herbaceous Openland",
    cover_type = l4cover_key,
    acres = acres,
    percent_class_acres = percent_of_openland_acres
  )


###############################################################################
# 23C. Combine and Format Plotting Data
###############################################################################

composition_plot_df <- bind_rows(
  agriculture_composition_plot_df,
  herbaceous_composition_plot_df
) %>%
  group_by(
    project_cover_class
  ) %>%
  mutate(
    
    # Total inventoried acreage within each broader cover class.
    total_class_acres = sum(
      acres,
      na.rm = TRUE
    ),
    
    # Create report-ready panel labels.
    # Put acreage on a second line so the facet headings remain readable
    # when the figure is reduced in size.
    facet_label = case_when(
      
      project_cover_class == "Agriculture" ~ paste0(
        "(a) Agriculture\n",
        comma(
          total_class_acres,
          accuracy = 1
        ),
        " ac"
      ),
      
      project_cover_class == "Herbaceous Openland" ~ paste0(
        "(b) Herbaceous Openland\n",
        comma(
          total_class_acres,
          accuracy = 1
        ),
        " ac"
      ),
      
      TRUE ~ project_cover_class
    ),
    
    # Wrap long cover-type labels so they do not consume excessive
    # horizontal space in the final report figure.
    #
    # Orchards/Vineyards/Nursery is handled separately because automatic
    # wrapping may treat the slash-separated label as a single word.
    cover_type_wrapped = case_when(
      
      cover_type == "Orchards/Vineyards/Nursery" ~
        "Orchards/Vineyards/\nNursery",
      
      TRUE ~ stringr::str_wrap(
        cover_type,
        width = 22
      )
    )
  ) %>%
  ungroup() %>%
  mutate(
    
    # Order finer-scale cover types independently within each panel.
    cover_type_plot = tidytext::reorder_within(
      cover_type_wrapped,
      percent_class_acres,
      facet_label
    )
  )


###############################################################################
# 23D. Build Combined Composition Figure
###############################################################################

cover_composition_plot <- ggplot(
  composition_plot_df,
  aes(
    x = percent_class_acres,
    y = cover_type_plot,
    fill = project_cover_class
  )
) +
  
  # Plot percentage of inventoried acreage within each broader cover class.
  geom_col() +
  
  # Add percentage labels to bars.
  geom_text(
    aes(
      label = paste0(
        round(
          percent_class_acres,
          1
        ),
        "%"
      )
    ),
    hjust = -0.1,
    size = 3.5
  ) +
  
  # Display Agriculture and Herbaceous Openland side by side.
  # Free y-scales allow each panel to display its own finer-scale
  # classifications while retaining a common x-axis percentage scale.
  facet_wrap(
    ~facet_label,
    nrow = 1,
    scales = "free_y"
  ) +
  
  # Preserve independent cover-type ordering within each panel.
  tidytext::scale_y_reordered() +
  
  # Apply project colors.
  scale_fill_manual(
    values = c(
      "Agriculture" = ag_color,
      "Herbaceous Openland" = herb_color
    )
  ) +
  
  # Use a common percentage scale for direct comparison of composition.
  scale_x_continuous(
    labels = percent_format(
      scale = 1
    ),
    limits = c(
      0,
      100
    ),
    expand = expansion(
      mult = c(
        0,
        0.08
      )
    )
  ) +
  
  # Axis labels.
  labs(
    x = "Percent of inventoried WLD acres within each cover class",
    y = NULL
  ) +
  
  # Apply the project theme.
  theme_grass() +
  
  # Fine-tune figure appearance for report readability.
  theme(
    
    # No legend is needed because each panel is directly labeled.
    legend.position = "none",
    
    # Increase facet-heading size and tighten spacing between the
    # cover-class name and acreage.
    strip.text = element_text(
      face = "bold",
      size = 13,
      lineheight = 1.0
    ),
    
    # Slightly reduce y-axis label size so wrapped labels remain readable
    # without forcing the panels to become too narrow.
    axis.text.y = element_text(
      size = 9
    )
  )

# Print plot
cover_composition_plot


###############################################################################
# 📏 24. Stand-Size Distribution and Summary Statistics
###############################################################################
# ⭐ Question this helps answer:
# What are the typical sizes of inventoried Agriculture and Herbaceous Openland
# stands, and how are stand sizes distributed?
#
# This analysis uses the latest-condition terrestrial snapshot so stand-size
# summaries correspond directly to the cover classes and acreage reported in
# the main analysis.
###############################################################################

# Create stand-size dataset for the two project cover classes.
project_stand_size <- snapshot_terrestrial_tbl %>%
  filter(
    !is.na(project_cover_class)
  )


###############################################################################
# 📋 24A. Stand-Size Summary Statistics
###############################################################################

stand_size_summary <- project_stand_size %>%
  group_by(
    project_cover_class
  ) %>%
  summarise(
    stands = n(),
    
    mean_acres = mean(
      acres,
      na.rm = TRUE
    ),
    
    median_acres = median(
      acres,
      na.rm = TRUE
    ),
    
    q1_acres = quantile(
      acres,
      0.25,
      na.rm = TRUE
    ),
    
    q3_acres = quantile(
      acres,
      0.75,
      na.rm = TRUE
    ),
    
    min_acres = min(
      acres,
      na.rm = TRUE
    ),
    
    max_acres = max(
      acres,
      na.rm = TRUE
    ),
    
    sd_acres = sd(
      acres,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

# Print summary.
stand_size_summary


###############################################################################
# 📊 24B. Stand-Size Distribution
###############################################################################
# Group stand sizes into intuitive acreage classes for communication.

size_distribution_summary <- project_stand_size %>%
  mutate(
    size_class = case_when(
      acres < 5   ~ "<5",
      acres < 20  ~ "5–19",
      acres < 50  ~ "20–49",
      acres < 100 ~ "50–99",
      TRUE        ~ "≥100"
    ),
    
    size_class = factor(
      size_class,
      levels = c(
        "<5",
        "5–19",
        "20–49",
        "50–99",
        "≥100"
      )
    )
  ) %>%
  count(
    project_cover_class,
    size_class
  ) %>%
  group_by(
    project_cover_class
  ) %>%
  mutate(
    percent_stands = 100 * n / sum(n)
  ) %>%
  ungroup()

# Print summary.
size_distribution_summary


# Build stand-size distribution figure.
size_distribution_plot <- ggplot(
  size_distribution_summary,
  aes(
    x = size_class,
    y = percent_stands,
    fill = project_cover_class
  )
) +
  geom_col() +
  geom_text(
    aes(
      label = paste0(
        round(percent_stands, 1),
        "%"
      )
    ),
    vjust = -0.3,
    size = 3.5
  ) +
  facet_wrap(
    ~project_cover_class,
    nrow = 1
  ) +
  scale_fill_manual(
    values = c(
      "Agriculture" = ag_color,
      "Herbaceous Openland" = herb_color
    )
  ) +
  scale_y_continuous(
    labels = percent_format(scale = 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    x = "Stand size (acres)",
    y = "Percent of inventoried WLD stands\nin each cover class"
  ) +
  theme_grass() +
  theme(
    legend.position = "none",
    strip.text = element_text(
      size = 14,
      face = "bold"
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )
# Print figure.
size_distribution_plot


###############################################################################
# 📍 25. Agriculture and Herbaceous Openland by Management Area
###############################################################################
# ⭐ Question this helps answer:
# Where does WLD have the most Agriculture and Herbaceous Openland acres?

management_area_summary <- snapshot_terrestrial_tbl %>%
  filter(
    !is.na(project_cover_class)
  ) %>%
  group_by(
    project_cover_class,
    unit_key
  ) %>%
  summarise(
    stands = n(),
    acres = sum(acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(
    project_cover_class
  ) %>%
  mutate(
    percent_category_acres =
      100 * acres / sum(acres, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(
    project_cover_class,
    desc(acres)
  )

# Print summary.
management_area_summary

# Identify the top 15 management areas within each category.
top_management_area_summary <- management_area_summary %>%
  group_by(
    project_cover_class
  ) %>%
  slice_max(
    order_by = acres,
    n = 15,
    with_ties = FALSE
  ) %>%
  ungroup()

# Print summary.
top_management_area_summary

# Plot top management areas by category.
management_area_plot <- ggplot(
  top_management_area_summary,
  aes(
    x = acres,
    y = reorder_within(
      unit_key,
      acres,
      project_cover_class
    ),
    fill = project_cover_class
  )
) +
  geom_col() +
  geom_text(
    aes(
      label = paste0(
        round(percent_category_acres, 1),
        "%"
      )
    ),
    hjust = -0.1,
    size = 3.2
  ) +
  facet_wrap(
    ~project_cover_class,
    scales = "free_y",
    labeller = as_labeller(
      c(
        "Agriculture" =
          "Top-15 management areas by\nAgriculture acres",
        
        "Herbaceous Openland" =
          "Top-15 management areas by\nHerbaceous Openland acres"
      )
    )
  ) +
  scale_y_reordered() +
  scale_fill_manual(
    values = c(
      "Agriculture" = ag_color,
      "Herbaceous Openland" = herb_color
    )
  ) +
  scale_x_continuous(
    labels = comma_format(),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    x = "Inventoried WLD acres\n(in MiFI)",
    y = NULL
  ) +
  theme_grass() +
  theme(
    legend.position = "none",
    
    strip.text = element_text(
      face = "bold"
    )
  )

# Print plot.
management_area_plot


###############################################################################
# 📍 26. Category Proportion Within Management Areas
###############################################################################
# ⭐ Question this helps answer:
# Where do Agriculture and Herbaceous Openland make up the largest share of a
# management area's inventoried acres?

management_area_proportion_summary <- snapshot_terrestrial_tbl %>%
  group_by(
    unit_key
  ) %>%
  summarise(
    total_unit_acres = sum(acres, na.rm = TRUE),
    
    agriculture_acres = sum(
      acres[project_cover_class == "Agriculture"],
      na.rm = TRUE
    ),
    
    agriculture_stands = sum(
      project_cover_class == "Agriculture",
      na.rm = TRUE
    ),
    
    herbaceous_openland_acres = sum(
      acres[project_cover_class == "Herbaceous Openland"],
      na.rm = TRUE
    ),
    
    herbaceous_openland_stands = sum(
      project_cover_class == "Herbaceous Openland",
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(
      agriculture_acres,
      herbaceous_openland_acres
    ),
    names_to = "project_cover_class_raw",
    values_to = "category_acres"
  ) %>%
  mutate(
    project_cover_class = case_when(
      project_cover_class_raw == "agriculture_acres" ~
        "Agriculture",
      
      project_cover_class_raw == "herbaceous_openland_acres" ~
        "Herbaceous Openland",
      
      TRUE ~ project_cover_class_raw
    ),
    
    category_stands = case_when(
      project_cover_class == "Agriculture" ~
        agriculture_stands,
      
      project_cover_class == "Herbaceous Openland" ~
        herbaceous_openland_stands,
      
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(
    category_acres > 0
  ) %>%
  mutate(
    percent_unit_acres =
      100 * category_acres / total_unit_acres
  ) %>%
  select(
    unit_key,
    project_cover_class,
    total_unit_acres,
    category_stands,
    category_acres,
    percent_unit_acres
  ) %>%
  arrange(
    project_cover_class,
    desc(percent_unit_acres)
  )

# Print summary.
management_area_proportion_summary

# Identify top 15 management areas by percent contribution within each category.
top_management_area_proportion <- management_area_proportion_summary %>%
  group_by(
    project_cover_class
  ) %>%
  slice_max(
    order_by = percent_unit_acres,
    n = 15,
    with_ties = FALSE
  ) %>%
  ungroup()

# Print summary.
top_management_area_proportion

# Build plot.
management_area_proportion_plot <- ggplot(
  top_management_area_proportion,
  aes(
    x = percent_unit_acres,
    y = tidytext::reorder_within(
      unit_key,
      percent_unit_acres,
      project_cover_class
    ),
    fill = project_cover_class
  )
) +
  geom_col() +
  geom_text(
    aes(
      label = paste0(
        round(category_acres, 0),
        " ac"
      )
    ),
    hjust = -0.1,
    size = 3.2
  ) +
  facet_wrap(
    ~project_cover_class,
    scales = "free_y",
    labeller = as_labeller(
      c(
        "Agriculture" =
          "Top-15 management areas by\npercent of inventoried acres in\nAgriculture",
        
        "Herbaceous Openland" =
          "Top-15 management areas by\npercent of inventoried acres in\nHerbaceous Openland"
      )
    )
  ) +
  tidytext::scale_y_reordered() +
  scale_fill_manual(
    values = c(
      "Agriculture" = ag_color,
      "Herbaceous Openland" = herb_color
    )
  ) +
  scale_x_continuous(
    labels = percent_format(scale = 1),
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    x = "Percent of each management area's\ninventoried WLD acres (in MiFI)",
    y = NULL
  ) +
  theme_grass() +
  theme(
    legend.position = "none"
  )

# Print plot.
management_area_proportion_plot


###############################################################################
# 📋 26A. Full WLD Management Area Summary Table
###############################################################################
# ⭐ Why this matters:
# This creates a complete management-area table for WLD, including total
# inventoried terrestrial acres, Agriculture acres, Herbaceous Openland acres,
# and percent of each management area's terrestrial acres in each cover class.

# This table includes all WLD management areas represented in the
# latest-condition terrestrial snapshot, not just areas with Agriculture or
# Herbaceous Openland.

management_area_full_summary <- snapshot_terrestrial_tbl %>%
  st_drop_geometry() %>%
  group_by(
    unit_key
  ) %>%
  summarise(
    total_terrestrial_stands = n(),
    total_terrestrial_acres = sum(acres, na.rm = TRUE),
    
    agriculture_stands = sum(
      project_cover_class == "Agriculture",
      na.rm = TRUE
    ),
    
    agriculture_acres = sum(
      acres[project_cover_class == "Agriculture"],
      na.rm = TRUE
    ),
    
    herbaceous_openland_stands = sum(
      project_cover_class == "Herbaceous Openland",
      na.rm = TRUE
    ),
    
    herbaceous_openland_acres = sum(
      acres[project_cover_class == "Herbaceous Openland"],
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    agriculture_percent_terrestrial_acres =
      100 * agriculture_acres / total_terrestrial_acres,
    
    herbaceous_openland_percent_terrestrial_acres =
      100 * herbaceous_openland_acres / total_terrestrial_acres,
    
    agriculture_and_herbaceous_openland_acres =
      agriculture_acres + herbaceous_openland_acres,
    
    agriculture_and_herbaceous_openland_percent_terrestrial_acres =
      100 * agriculture_and_herbaceous_openland_acres /
      total_terrestrial_acres
  ) %>%
  arrange(
    desc(agriculture_and_herbaceous_openland_acres),
    desc(total_terrestrial_acres)
  ) %>%
  mutate(
    management_area_rank_by_project_cover_acres = row_number()
  ) %>%
  select(
    management_area_rank_by_project_cover_acres,
    unit_key,
    total_terrestrial_acres,
    total_terrestrial_stands,
    agriculture_acres,
    agriculture_stands,
    agriculture_percent_terrestrial_acres,
    herbaceous_openland_acres,
    herbaceous_openland_stands,
    herbaceous_openland_percent_terrestrial_acres,
    agriculture_and_herbaceous_openland_acres,
    agriculture_and_herbaceous_openland_percent_terrestrial_acres
  )

# Print full management-area table.
management_area_full_summary


###############################################################################
# 📋 26B. Top-10 Management Areas for Report Table
###############################################################################
# ⭐ Why this matters:
# This creates the compact management-area table used in the main report.
# Management areas are ranked by combined inventoried Agriculture and
# Herbaceous Openland acreage.
#
# The table reports two different percentage metrics:
#
# 1. percent_of_wld_combined_project_cover_acres
#    = the percentage of all inventoried WLD Agriculture + Herbaceous Openland
#      acreage that occurs within each management area.
#
# 2. agriculture_and_herbaceous_openland_percent_terrestrial_acres
#    = the percentage of each management area's own inventoried terrestrial
#      acreage classified as Agriculture or Herbaceous Openland.
#
# These metrics answer different questions:
#   - How much does this management area contribute to the WLD total?
#   - How much of this management area is made up of the focal cover classes?
###############################################################################

# Calculate the total inventoried WLD acreage classified as Agriculture or
# Herbaceous Openland across all management areas.
wld_combined_project_cover_acres <- sum(
  management_area_full_summary$
    agriculture_and_herbaceous_openland_acres,
  na.rm = TRUE
)

# Create the compact top-10 table for the main report.
management_area_top10_report_table <- management_area_full_summary %>%
  mutate(
    percent_of_wld_combined_project_cover_acres =
      100 * agriculture_and_herbaceous_openland_acres /
      wld_combined_project_cover_acres
  ) %>%
  arrange(
    management_area_rank_by_project_cover_acres
  ) %>%
  slice_head(
    n = 15
  ) %>%
  select(
    management_area_rank_by_project_cover_acres,
    unit_key,
    agriculture_and_herbaceous_openland_acres,
    percent_of_wld_combined_project_cover_acres,
    agriculture_and_herbaceous_openland_percent_terrestrial_acres
  )

# Print report-ready top-15 table.
management_area_top15_report_table


###############################################################################
# 📊 27. Concentration of Acres
###############################################################################
# ⭐ Question this helps answer:
# Are Agriculture and Herbaceous Openland broadly distributed across WLD, or
# concentrated in a small number of management areas?

acre_concentration_summary <- management_area_summary %>%
  arrange(
    project_cover_class,
    desc(acres)
  ) %>%
  group_by(
    project_cover_class
  ) %>%
  mutate(
    rank = row_number(),
    cumulative_acres = cumsum(acres),
    total_category_acres = sum(acres, na.rm = TRUE),
    cumulative_percent = 100 * cumulative_acres / total_category_acres
  ) %>%
  ungroup()

# Summarize concentration within the largest management areas.
acre_concentration_table <- acre_concentration_summary %>%
  group_by(
    project_cover_class
  ) %>%
  summarise(
    top_5_percent =
      max(cumulative_percent[rank <= 5], na.rm = TRUE),
    
    top_10_percent =
      max(cumulative_percent[rank <= 10], na.rm = TRUE),
    
    top_20_percent =
      max(cumulative_percent[rank <= 20], na.rm = TRUE),
    
    total_management_areas = n(),
    
    .groups = "drop"
  )

# Print summary.
acre_concentration_table

# Plot.
acre_concentration_plot <- ggplot(
  acre_concentration_summary,
  aes(
    x = rank,
    y = cumulative_percent,
    color = project_cover_class
  )
) +
  geom_line(
    linewidth = 1.2
  ) +
  scale_color_manual(
    values = c(
      "Agriculture" = ag_color,
      "Herbaceous Openland" = herb_color
    )
  ) +
  scale_y_continuous(
    labels = percent_format(scale = 1)
  ) +
  labs(
    x = "Top WLD management areas included\n(rank 1 = most cover class acres)",
    y = "Cumulative percent of total\nWLD cover class acres (in MiFI)"
  ) +
  theme_grass() +
  theme(
    legend.position = c(0.70, 0.50),
    legend.background = element_blank()
  )

# Print plot.
acre_concentration_plot


###############################################################################
# 📊 28. Concentration Summary Bar Chart
###############################################################################
# ⭐ Question this helps answer:
# What percent of total Agriculture and Herbaceous Openland acres are contained
# within the top 5, 10, and 20 management areas?

acre_concentration_bar_df <- acre_concentration_table %>%
  tidyr::pivot_longer(
    cols = c(
      top_5_percent,
      top_10_percent,
      top_20_percent
    ),
    names_to = "largest_area_group",
    values_to = "percent_category_acres"
  ) %>%
  mutate(
    largest_area_group = case_when(
      largest_area_group == "top_5_percent"  ~ "Top 5",
      largest_area_group == "top_10_percent" ~ "Top 10",
      largest_area_group == "top_20_percent" ~ "Top 20",
      TRUE ~ largest_area_group
    ),
    
    largest_area_group = factor(
      largest_area_group,
      levels = c(
        "Top 5",
        "Top 10",
        "Top 20"
      )
    )
  )

# Print summary.
acre_concentration_bar_df

# Build plot.
acre_concentration_bar_plot <- ggplot(
  acre_concentration_bar_df,
  aes(
    x = largest_area_group,
    y = percent_category_acres,
    fill = project_cover_class
  )
) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65
  ) +
  geom_text(
    aes(
      label = paste0(
        round(percent_category_acres, 1),
        "%"
      )
    ),
    position = position_dodge(width = 0.75),
    vjust = -0.3,
    size = 3.5
  ) +
  scale_fill_manual(
    values = c(
      "Agriculture" = ag_color,
      "Herbaceous Openland" = herb_color
    )
  ) +
  scale_y_continuous(
    labels = percent_format(scale = 1),
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "WLD management areas with\nthe most cover class acres",
    y = "Percent of total inventoried\nWLD cover class acres (in MiFI)"
  ) +
  theme_grass() +
  theme(
    legend.position = "top",
    legend.background = element_blank()
  )

# Print plot.
acre_concentration_bar_plot


###############################################################################
# 📏 29. Stand-Size Summary Statistics
###############################################################################
# ⭐ Question this helps answer:
# What are the typical stand sizes for Agriculture and Herbaceous Openland?

stand_size_summary <- snapshot_terrestrial_tbl %>%
  filter(
    !is.na(project_cover_class)
  ) %>%
  group_by(
    project_cover_class
  ) %>%
  summarise(
    stands = n(),
    
    min_acres = min(acres, na.rm = TRUE),
    
    q1_acres = quantile(
      acres,
      0.25,
      na.rm = TRUE
    ),
    
    median_acres = median(
      acres,
      na.rm = TRUE
    ),
    
    mean_acres = mean(
      acres,
      na.rm = TRUE
    ),
    
    q3_acres = quantile(
      acres,
      0.75,
      na.rm = TRUE
    ),
    
    max_acres = max(
      acres,
      na.rm = TRUE
    ),
    
    sd_acres = sd(
      acres,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

# Print summary.
stand_size_summary


###############################################################################
# 📋 30. Step 3 Report Summary Table
###############################################################################
# ⭐ Why this matters:
# This creates one report-ready table that summarizes the main answers: how much
# WLD has, where it is, and how concentrated it is across management areas.

# Helper functions for report formatting.
format_acres <- function(x) {
  paste0(
    comma(x, accuracy = 1),
    " ac"
  )
}

format_count <- function(x) {
  comma(
    x,
    accuracy = 1
  )
}

format_percent_value <- function(x) {
  paste0(
    number(x, accuracy = 0.1),
    "%"
  )
}

# Pull top management area by total category acres.
top_management_area_by_acres <- management_area_summary %>%
  group_by(
    project_cover_class
  ) %>%
  slice_max(
    order_by = acres,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  select(
    project_cover_class,
    top_area_by_acres = unit_key,
    top_area_acres = acres,
    top_area_percent_category_acres = percent_category_acres
  )

# Pull top management area by percent of local management area acres.
top_management_area_by_percent <- management_area_proportion_summary %>%
  group_by(
    project_cover_class
  ) %>%
  slice_max(
    order_by = percent_unit_acres,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  select(
    project_cover_class,
    top_area_by_percent = unit_key,
    top_area_percent_unit_acres = percent_unit_acres,
    top_area_by_percent_acres = category_acres
  )

# Create category-level summary.
category_report_summary <- project_cover_summary %>%
  left_join(
    stand_size_summary %>%
      select(
        project_cover_class,
        mean_acres,
        median_acres
      ),
    by = "project_cover_class"
  ) %>%
  left_join(
    top_management_area_by_acres,
    by = "project_cover_class"
  ) %>%
  left_join(
    top_management_area_by_percent,
    by = "project_cover_class"
  ) %>%
  left_join(
    acre_concentration_table %>%
      select(
        project_cover_class,
        top_5_percent,
        top_10_percent,
        top_20_percent,
        total_management_areas
      ),
    by = "project_cover_class"
  )

# Create final report summary table.
step3_report_summary <- category_report_summary %>%
  mutate(
    acres = format_acres(acres),
    stands = format_count(stands),
    
    percent_terrestrial_stands =
      format_percent_value(percent_terrestrial_stands),
    
    percent_terrestrial_acres =
      format_percent_value(percent_terrestrial_acres),
    
    mean_acres = format_acres(mean_acres),
    median_acres = format_acres(median_acres),
    top_area_acres = format_acres(top_area_acres),
    
    top_area_percent_category_acres =
      format_percent_value(top_area_percent_category_acres),
    
    top_area_percent_unit_acres =
      format_percent_value(top_area_percent_unit_acres),
    
    top_area_by_percent_acres =
      format_acres(top_area_by_percent_acres),
    
    top_5_percent = format_percent_value(top_5_percent),
    top_10_percent = format_percent_value(top_10_percent),
    top_20_percent = format_percent_value(top_20_percent)
  ) %>%
  select(
    project_cover_class,
    acres,
    stands,
    percent_terrestrial_stands,
    percent_terrestrial_acres,
    mean_acres,
    median_acres,
    top_area_by_acres,
    top_area_acres,
    top_area_percent_category_acres,
    top_area_by_percent,
    top_area_percent_unit_acres,
    top_area_by_percent_acres,
    top_5_percent,
    top_10_percent,
    top_20_percent,
    total_management_areas
  )

# Print summary.
step3_report_summary


###############################################################################
# 🏛️ 31. Department-Wide Authority Context Tables
###############################################################################
# ⭐ Why this matters:
# This provides Department-wide context by showing how retained latest-condition
# terrestrial MiFI acres are distributed across management authorities overall
# and for the project-relevant cover classes.

# These tables help place Wildlife Division-administered lands in the broader
# Department MiFI inventory context.
#
# Table 1 answers:
#   "Of all retained Department-wide latest-condition terrestrial MiFI acres,
#    what percent is associated with each authority?"
#
# Table 2 answers:
#   "Of all retained Department-wide Agriculture or Herbaceous Openland acres,
#    what percent is associated with each authority?"
#
#   and:
#
#   "Within each authority's retained latest-condition terrestrial MiFI land
#    base, what percent is represented by Agriculture or Herbaceous Openland?"


###############################################################################
# 🏛️ 31A. Total Department-Wide Terrestrial Acres by Authority
###############################################################################

department_authority_context <- department_snapshot_terrestrial_tbl %>%
  group_by(
    authority_key
  ) %>%
  summarise(
    stands = n(),
    unique_stands = n_distinct(fcskey),
    terrestrial_acres = sum(acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percent_department_terrestrial_acres =
      100 * terrestrial_acres / sum(terrestrial_acres, na.rm = TRUE)
  ) %>%
  arrange(
    desc(terrestrial_acres)
  ) %>%
  mutate(
    authority_acre_rank = row_number(),
    terrestrial_acres = round(terrestrial_acres, 0),
    percent_department_terrestrial_acres =
      round(percent_department_terrestrial_acres, 1)
  ) %>%
  select(
    authority_acre_rank,
    authority_key,
    terrestrial_acres,
    percent_department_terrestrial_acres,
    stands,
    unique_stands
  )

# Print Department-wide authority context table.
department_authority_context


###############################################################################
# 🏛️ 31B. Department-Wide Agriculture and Herbaceous Openland by Authority
###############################################################################

# Create authority land-base totals from the retained Department-wide
# latest-condition terrestrial snapshot.
department_authority_terrestrial_landbase <- department_snapshot_terrestrial_tbl %>%
  group_by(
    authority_key
  ) %>%
  summarise(
    total_authority_terrestrial_acres =
      sum(acres, na.rm = TRUE),
    
    total_authority_terrestrial_stands =
      n(),
    
    .groups = "drop"
  )

# Summarize project-relevant cover classes by authority.
department_project_cover_authority_context <- department_snapshot_terrestrial_tbl %>%
  filter(
    !is.na(project_cover_class)
  ) %>%
  group_by(
    project_cover_class,
    authority_key
  ) %>%
  summarise(
    stands = n(),
    unique_stands = n_distinct(fcskey),
    acres = sum(acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(
    project_cover_class
  ) %>%
  mutate(
    total_department_class_acres =
      sum(acres, na.rm = TRUE),
    
    percent_department_class_acres =
      100 * acres / total_department_class_acres
  ) %>%
  ungroup() %>%
  left_join(
    department_authority_terrestrial_landbase,
    by = "authority_key"
  ) %>%
  mutate(
    percent_authority_terrestrial_landbase =
      100 * acres / total_authority_terrestrial_acres
  ) %>%
  arrange(
    project_cover_class,
    desc(acres)
  ) %>%
  mutate(
    acres = round(acres, 0),
    total_department_class_acres = round(total_department_class_acres, 0),
    percent_department_class_acres =
      round(percent_department_class_acres, 1),
    total_authority_terrestrial_acres =
      round(total_authority_terrestrial_acres, 0),
    percent_authority_terrestrial_landbase =
      round(percent_authority_terrestrial_landbase, 1)
  ) %>%
  select(
    project_cover_class,
    authority_key,
    acres,
    percent_department_class_acres,
    total_department_class_acres,
    percent_authority_terrestrial_landbase,
    total_authority_terrestrial_acres,
    stands,
    unique_stands
  )

# Print Department-wide project cover context table.
department_project_cover_authority_context


###############################################################################
# 📊 31C. MDNR Land-Managing Division Comparison
###############################################################################
# ⭐ Question this helps answer:
# How are inventoried terrestrial acreage, Agriculture, and Herbaceous Openland
# distributed among the three MDNR land-managing divisions represented in MiFI?
#
# This figure is intentionally limited to:
#   - State Forests
#   - Wildlife Division
#   - State Parks
#
# Good Neighbor Authority (USFS), Partner Lands, and Unspecified records remain
# in the broader MiFI analysis and Table 1 but are excluded from this figure so
# the visual comparison focuses specifically on MDNR land-managing divisions.
#
# IMPORTANT:
# Percentages are recalculated within the three selected MDNR divisions.
# Therefore, each bar sums to 100%.
###############################################################################


###############################################################################
# 31C-1. Define MDNR Land-Managing Divisions
###############################################################################

mdnr_land_divisions <- c(
  "State Forests",
  "Wildlife",
  "State Parks"
)


###############################################################################
# 31C-2. Prepare Inventoried Terrestrial Acreage
###############################################################################
# Each authority's total retained inventoried terrestrial acreage is already
# carried in department_project_cover_authority_context.
#
# Use distinct() because the same total terrestrial acreage is repeated for
# each focal cover class within an authority.

mdnr_terrestrial_plot_df <- department_project_cover_authority_context %>%
  filter(
    authority_key %in% mdnr_land_divisions
  ) %>%
  distinct(
    authority_key,
    total_authority_terrestrial_acres
  ) %>%
  transmute(
    category = "Inventoried terrestrial acreage",
    authority_key,
    acres = total_authority_terrestrial_acres
  ) %>%
  mutate(
    percent_acres = 100 * acres / sum(acres)
  )


###############################################################################
# 31C-3. Prepare Agriculture and Herbaceous Openland Acreage
###############################################################################
# Recalculate percentages after restricting the dataset to the three MDNR
# land-managing divisions. This ensures percentages describe how each focal
# cover class is distributed among these three divisions rather than across
# the full MiFI database.

mdnr_cover_plot_df <- department_project_cover_authority_context %>%
  filter(
    authority_key %in% mdnr_land_divisions,
    project_cover_class %in% c(
      "Agriculture",
      "Herbaceous Openland"
    )
  ) %>%
  transmute(
    category = case_when(
      project_cover_class == "Agriculture" ~
        "Inventoried Agriculture",
      
      project_cover_class == "Herbaceous Openland" ~
        "Inventoried Herbaceous Openland",
      
      TRUE ~ project_cover_class
    ),
    authority_key,
    acres
  ) %>%
  group_by(
    category
  ) %>%
  mutate(
    percent_acres = 100 * acres / sum(acres)
  ) %>%
  ungroup()


###############################################################################
# 31C-4. Combine Plotting Data
###############################################################################

mdnr_authority_plot_df <- bind_rows(
  mdnr_terrestrial_plot_df,
  mdnr_cover_plot_df
) %>%
  mutate(
    
    # Set category order from broad inventoried land base to focal cover
    # classes.
    category = factor(
      category,
      levels = c(
        "Inventoried terrestrial acreage",
        "Inventoried Agriculture",
        "Inventoried Herbaceous Openland"
      )
    ),
    
    # Set stacking order for administrative authorities.
    authority_key = factor(
      authority_key,
      levels = c(
        "State Forests",
        "Wildlife",
        "State Parks"
      )
    )
  )


###############################################################################
# 31C-5. QA Check
###############################################################################
# Each category should sum to approximately 100%.

mdnr_authority_plot_df %>%
  group_by(
    category
  ) %>%
  summarise(
    total_percent = sum(
      percent_acres,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


###############################################################################
# 31C-6. Build 100% Stacked-Bar Figure
###############################################################################
# Custom colors
authority_colors <- c(
  "State Forests" = "grey65",
  "Wildlife" = "grey45",
  "State Parks" = "grey90"
)

# Plot
mdnr_authority_plot <- ggplot(
  mdnr_authority_plot_df,
  aes(
    x = category,
    y = percent_acres,
    fill = authority_key
  )
) +
  
  geom_col(
    width = 0.7
  ) +
  
  geom_text(
    aes(
      label = paste0(
        round(
          percent_acres,
          1
        ),
        "%"
      )
    ),
    position = position_stack(
      vjust = 0.5
    ),
    size = 3.7
  ) +
  
  # Apply restrained administrative-authority colors.
  # Yellow and green are reserved elsewhere in the report for
  # Agriculture and Herbaceous Openland.
  scale_fill_manual(
    values = authority_colors
  ) +
  
  scale_y_continuous(
    labels = scales::percent_format(
      scale = 1
    ),
    limits = c(
      0,
      100
    ),
    expand = expansion(
      mult = c(
        0,
        0
      )
    )
  ) +
  
  labs(
    x = NULL,
    y = "Percent of inventoried acres\nin each category",
    fill = NULL
  ) +
  
  theme_grass() +
  
  theme(
    legend.position = "bottom",
    
    axis.text.x = element_text(
      size = 10
    )
  )


###############################################################################
# 31C-7. Print Figure
###############################################################################

mdnr_authority_plot


###############################################################################
# 💾 32. Export Summary Table, Appendix Table, RDS Objects, and Figures
###############################################################################
# ⭐ Why this matters:
# This exports only the core report-ready summary table, appendix table, final
# figures, and snapshot objects needed for communication and downstream use.

output_dir <- "phase1_step3_outputs"

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}


###############################################################################
# Export core report tables.
###############################################################################

write_csv(
  step3_report_summary,
  file.path(output_dir, "step3_report_summary.csv")
)

write_csv(
  department_authority_context,
  file.path(output_dir, "department_authority_terrestrial_context.csv")
)

write_csv(
  department_retention_summary,
  file.path(output_dir, "department_retention_summary.csv")
)

write_csv(
  department_project_cover_authority_context,
  file.path(output_dir, "department_project_cover_authority_context.csv")
)

write_csv(
  stand_size_summary,
  file.path(output_dir, "stand_size_summary.csv")
)

write_csv(
  management_area_full_summary,
  file.path(output_dir, "wld_management_area_full_summary.csv")
)

write_csv(
  management_area_top15_report_table,
  file.path(output_dir, "management_area_top15_report_table.csv")
)


###############################################################################
# Export RDS objects.
###############################################################################

saveRDS(
  snapshot_latest_tbl,
  file.path(output_dir, "snapshot_latest.rds")
)

saveRDS(
  snapshot_terrestrial_tbl,
  file.path(output_dir, "snapshot_terrestrial.rds")
)


###############################################################################
# Export figures.
###############################################################################

# Department-wide rolling inventory context.
ggsave(
  file.path(output_dir, "fig_department_total_acres.png"),
  p_department_total_acres,
  width = figure_width,
  height = figure_height,
  dpi = figure_dpi
)

# Wildlife Division rolling inventory context.
ggsave(
  file.path(output_dir, "fig_total_acres.png"),
  p_total_acres,
  width = figure_width,
  height = figure_height,
  dpi = figure_dpi
)

# Project cover classes through time.
ggsave(
  file.path(output_dir, "fig_project_cover_through_time.png"),
  project_cover_through_time_plot,
  width = figure_width,
  height = figure_height,
  dpi = figure_dpi
)

# Composition figures.
ggsave(
  file.path(output_dir, "fig_cover_class_composition.png"),
  cover_composition_plot,
  width = 11.5,
  height = 5,
  dpi = figure_dpi
)

# Management area distribution figures.
ggsave(
  file.path(output_dir, "fig_management_area_acres.png"),
  management_area_plot,
  width = 10.5,
  height = 7.5,
  dpi = figure_dpi
)

ggsave(
  file.path(output_dir, "fig_management_area_proportion.png"),
  management_area_proportion_plot,
  width = 8.5,
  height = 7,
  dpi = figure_dpi
)

# Acreage concentration figure.
ggsave(
  file.path(output_dir, "fig_acre_concentration_bar.png"),
  acre_concentration_bar_plot,
  width = figure_width,
  height = figure_height,
  dpi = figure_dpi
)

ggsave(
  file.path(output_dir, "fig_stand_size_distribution.png"),
  size_distribution_plot,
  width = figure_width,
  height = figure_height,
  dpi = figure_dpi
)

ggsave(
  file.path(output_dir, "fig_mdnr_authority.png"),
  mdnr_authority_plot,
  width = 7,
  height = 5,
  dpi = figure_dpi
)

# Export QA check.
list.files(output_dir)


###############################################################################
# End of script
###############################################################################
