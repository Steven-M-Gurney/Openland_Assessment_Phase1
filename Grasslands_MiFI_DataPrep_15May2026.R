###############################################################################
###############################################################################

# 🌾️ Grassland Assessment Project
# 🛠 Phase 1, Step 1 — Data Preparation
#
# Supporting Analytical Workflow for:
# Michigan Department of Natural Resources
# Wildlife Division, Planning and Adaption Section
# Special Publication No. 2026-02
#
# Author: Steven M. Gurney
# Last updated: 15 MAY 2026


###############################################################################
###############################################################################

# PURPOSE
# -------
# This script prepares a Wildlife Division subset of MiFI stand-level data for
# downstream explorations, quality assurance, and analyses.
#
# Specifically, this script:
#
#   1) Reads local snapshot copies of MiFI stand and compartment spatial data
#      from an ArcGIS Pro file geodatabase.
#   2) Reads and cleans coded domain lookup tables.
#   3) Joins human-readable labels onto coded stand and compartment fields.
#   4) Preserves original mixed-level IFMAP cover codes and creates a
#      standardized Level-3 cover type field.
#   5) Builds a Level-3 cover type lookup table, including manual crosswalk
#      additions for valid IFMAP parent classes missing from the exported domain.
#   6) Joins compartment attributes to stand records using the shared fc_key
#      compartment identifier.
#   7) Filters the prepared dataset to the selected management authority.
#
#
## INPUTS
# ------
#   • Local snapshot copy of the MiFI ArcGIS Pro file geodatabase
#     (Grasslands.gdb)
#
#   • Exported stand-level feature class:
#       Stands_ExportFeatures
#
#   • Exported compartment feature class:
#       Compartments_Copy
#
#   • Exported coded-domain lookup tables created using the ArcGIS Pro
#     "Domain To Table" geoprocessing tool:
#
#       - Canopy_Closure_Domain
#       - Cover_Type_Domain
#       - Management_Domain
#       - Authority_Domain
#       - UnitName_Domain
#
# The stand and compartment feature classes above were exported from MiFI to
# create static snapshot copies for reproducible analysis outside the enterprise
# geodatabase environment.
#
# Domain tables were exported separately because geodatabase coded-value domain
# descriptions are not automatically retained. These domain tables are later 
# used to rebuild human-readable labels from coded fields within the spatial
# datasets.
#
# Additional exported feature classes or domain tables may be required in the
# future depending on analysis objectives and associated MiFI attributes.
#
#
# OUTPUTS
# -------
#   • stands_working_prepped.rds
#
#
# IMPORTANT INTERPRETATION NOTES
# ------------------------------
# This script prepares database records for analysis. The resulting data reflect
# the MiFI records available in the local snapshot copy and should not be
# interpreted automatically as a current inventory or on-the-ground condition.
#
# MiFI was designed primarily as a forestry inventory tool. Some MiFI forestry 
# names, categories, and labels do not align perfectly with Wildlife Division
# terminology or management framing. The MiFI manual and its IFMAP
# classification hierarchy can be used to help interpret some content here.
#
# The original field named l4covertype contains mixed-level IFMAP codes, 
# including Level-3, Level-4, and Level-5 classifications. For consistency, this
# script creates a coarse l3covertype by retaining the first three digits of the
# original IFMAP code.
#
# Compartment attributes are joined to stands using fc_key (unique compartment
# identifier) rather than spatial clipping. This respects the database
# relationship between stand records and compartments and avoids
# geometry-derived complications (e.g., polygon splitting, boundary errors).


###############################################################################
# 📦 1. Load Required Packages
###############################################################################
# These packages will need to be installed if the user does not have them.
library(sf) # Analyzing spatial data
library(dplyr) # Data manipulation
library(janitor) # Data prep
library(lubridate) # Formatting dates
library(ggplot2) # Data visualization
library(scales) # Scaling visuals
library(purrr) # Clean coding
library(readr) # Reading data
library(tibble) # Simple data frames
library(forcats) # Visualization tool


###############################################################################
# 📁 2. Define ArcGIS Pro File Geodatabase
###############################################################################
# File pathway to where the local snapshot copy of the MiFI data was saved.
gdb <- "C:/Users/GurneyS5/OneDrive - State of Michigan DTMB/ArcGIS/Projects/Grasslands/Grasslands.gdb"


###############################################################################
# 🔎 3. Inspect Contents of the Geodatabase
###############################################################################
# Printed results here should include all the layers and tables needed later.
st_layers(gdb)$name


###############################################################################
# 🌲 4. Read Stand- and Compartment-Level Spatial Data
###############################################################################
# Read stand-level spatial data.
# This object is big and slow because it includes all MiFI spatial data.
stands <- st_read(
  dsn   = gdb, # Geodatabase defined above.
  layer = "Stands_ExportFeatures", # Exported snapshot copy.
  quiet = TRUE
) %>%
  clean_names() # Put column names in snake case (simplify).

# Read compartment spatial data, which provide management authority and 
# management area (unit) context.
# These attributes will eventually be spatially joined to the stands data.
compartments <- st_read(
  dsn   = gdb, # Geodatabase defined above.
  layer = "Compartments_Copy", # Exported snapshot copy.
  quiet = TRUE
) %>%
  clean_names() # Put column names in snake case (simplify).

# For reference, print column names from the attribute tables. 
names(stands) # Includes compartment identifier (fckey).
names(compartments) # Includes compartment identifier (fckey).

# For reference, inspect row count and data formats.
glimpse(stands)
glimpse(compartments)


###############################################################################
# 🧾 5. Read Domain Lookup Tables
###############################################################################
# Keeping domain tables together makes it easier to adjust later if needed.

# Put all domain lookup tables in a list.
domain_tables <- list(
  
  # Percent canopy cover, from the Stands layer.
  canopy_closure = st_read(
    dsn = gdb,
    layer = "Canopy_Closure_Domain", 
    quiet = TRUE
  ) %>%
    clean_names(), # Put column names in snake case (simplify).
  
  # L4 cover type classification, from the Stands layer.  
  cover_type = st_read(
    dsn = gdb,
    layer = "Cover_Type_Domain", 
    quiet = TRUE
  ) %>%
    clean_names(), # Put column names in snake case (simplify).
  
  # On-the-ground management type, from the Stands layer.
  management = st_read(
    dsn = gdb,
    layer = "Management_Domain", 
    quiet = TRUE
  ) %>%
    clean_names(), # Put column names in snake case (simplify).
  
  # Management authority, from the Compartments layer.
  authority = st_read(
    dsn = gdb,
    layer = "Authority_Domain", 
    quiet = TRUE
  ) %>%
    clean_names(), # Put column names in snake case (simplify).
  
  # Name of management area (e.g., game area), from the Compartments layer.
  unit_name = st_read(
    dsn = gdb,
    layer = "UnitName_Domain", 
    quiet = TRUE
  ) %>%
    clean_names() # Put column names in snake case (simplify).
  
)

# Inspect each item in the list created above.
domain_tables$canopy_closure # Grouped by ranges (not continuous data).
domain_tables$cover_type # Messy "code" descriptions and "code" length varies.
domain_tables$management # This is specific to non-forested stands. 
domain_tables$authority # Includes management authorities and partnerships.
domain_tables$unit_name # Description format varies.


###############################################################################
# 🧼 6. Clean Domain Lookup Tables
###############################################################################
# Each item is cleaned separately because the same numeric code can mean
# different things in different domains.

# Below, transmute() keeps only the fields needed for joins and renames code 
# columns to match the field names used in the spatial layers.

# Clean canopy-cover table.
canopy_lookup <- domain_tables$canopy_closure %>%
  transmute(
    canopy_closure = code,
    canopy_key = canopy_key
  )

# Clean cover-type table.
# Cover type labels need extra cleanup because some descriptions include
# leading numeric codes, dashes, and trailing "(OI)" text.
l4_lookup <- domain_tables$cover_type %>%
  transmute(
    l4covertype_full = code,
    
    # Remove leading numeric code and first dash separator, not meaningful
    # dashes that may occur in names.
    l4cover_key = sub("^[0-9]+\\s*-\\s*", "", l4cover_key) %>% 
      
    # Remove trailing " (OI)" if applicable.
    sub("\\s*\\(OI\\)$", "", .)  
  )

# Clean management-type table.
management_lookup <- domain_tables$management %>%
  transmute(
    managed_site = code,
    management_key = management_key
  )

# Clean authority-type table.
authority_lookup <- domain_tables$authority %>%
  transmute(
    management_type = code,
    authority_key = authority_key
  )

# Clean unit-type table.
unit_lookup <- domain_tables$unit_name %>%
  transmute(
    unit_name = code,
    unit_key = unit_key
  )

# Inspect the clean lookup tables.
canopy_lookup # Note MiFI uses <25% to classify stands as "non-forested."
l4_lookup 
management_lookup
authority_lookup
unit_lookup

# After this step, these lookup tables should be ready to join onto the stands 
# (spatial) data using their matching coded fields.


###############################################################################
# 🔗 7. Join Associated Domain Labels onto Stands
###############################################################################
# This step adds readable labels to coded fields in the stands data.

# It also preserves the original cover code and creates a broader Level-3 code
# for later summaries.

stands <- stands %>%
  mutate(
    
    # Preserve the original mixed-level cover code from MiFI.
    # This may include 3-, 4-, or 5-digit codes, despite the column being 
    # called "l4covertype."
    l4covertype_full = l4covertype,
    
    # Create a standardized Level-3 cover code by keeping the first 3 digits.
    # Example: 31021 -> 310; 42110 -> 421.
    l3covertype = as.integer(substr(as.character(l4covertype), 1, 3))
  ) %>%
  
  # Join readable canopy closure labels onto stands.
  # Matches stands$canopy_closure to canopy_lookup$canopy_closure.
  left_join(
    canopy_lookup,
    by = "canopy_closure"
  ) %>%
  
  # Join readable full cover-type labels onto stands.
  # Matches stands$l4covertype_full to l4_lookup$l4covertype_full.
  left_join(
    l4_lookup,
    by = "l4covertype_full"
  ) %>%
  
  # Join readable management-status labels onto stands.
  # Matches stands$managed_site to management_lookup$managed_site.
  left_join(
    management_lookup,
    by = "managed_site"
  ) %>%
  
  # Move key analysis fields to the front so they are easier to inspect.
  relocate(
    canopy_key,
    canopy_closure,
    l4cover_key,
    l4covertype_full,
    l3covertype,
    management_key,
    .before = everything()
  )


###############################################################################
# 🔍 8. Check Cover Type Code Hierarchy
###############################################################################
# MiFI/IFMAP cover codes are hierarchical. Despite the field name
# "l4covertype," this field can include Level-3, Level-4, and Level-5 codes.
#
# This check documents the classification depth represented in the raw stand
# records before custom Level-3 labels are finalized.

# Summarize how many stand records occur at each code length.
cover_level_counts <- stands %>%
  st_drop_geometry() %>%
  mutate(
    cover_code_digits = nchar(as.character(l4covertype_full)),
    cover_level = paste0("Level ", cover_code_digits)
  ) %>%
  count(
    cover_level,
    cover_code_digits,
    sort = TRUE
  )

# Take a look.
cover_level_counts


# Summarize raw cover-code frequencies.
# This helps identify dominant detailed classes and confirms the mixed-level
# structure of the l4covertype field.
cover_code_counts <- stands %>%
  st_drop_geometry() %>%
  count(
    l4covertype_full,
    l4cover_key,
    sort = TRUE
  )

# Take a look.
cover_code_counts


# Summarize how many unique full cover codes roll up under each derived
# Level-3 parent code.
cover_hierarchy_summary <- stands %>%
  st_drop_geometry() %>%
  group_by(l3covertype) %>%
  summarise(
    stand_records = n(),
    unique_full_cover_codes = n_distinct(l4covertype_full),
    .groups = "drop"
  ) %>%
  arrange(desc(stand_records))

# Take a look.
cover_hierarchy_summary


###############################################################################
# 🔍 9. Key Out Level-3 Cover
###############################################################################
# Key out derived Level-3 cover codes using the cleaned cover-type lookup table.
# This check is to see if all Level-3 parent codes have associated labels.

# Build table to see if Level-3 codes have cover-type labels.
l3_lookup_check <- stands %>%
  st_drop_geometry() %>%
  distinct(l3covertype) %>%
  left_join(
    l4_lookup %>%
      rename(
        l3covertype = l4covertype_full,
        l3cover_key_check = l4cover_key
      ),
    by = "l3covertype"
  ) %>%
  arrange(l3covertype)

# Take a look at the table.
l3_lookup_check

# Summarize Level-3 lookup coverage.
l3_lookup_coverage_summary <- l3_lookup_check %>%
  summarise(
    total_l3_codes = n(),
    l3_codes_with_description = sum(!is.na(l3cover_key_check)),
    l3_codes_missing_description = sum(is.na(l3cover_key_check))
  )

# Look at at summary.
l3_lookup_coverage_summary


# Print Level-3 codes missing descriptions.
l3_missing_descriptions <- l3_lookup_check %>%
  filter(is.na(l3cover_key_check))

# Look at the list of Level-3 cover codes that lack a cover-type description.
l3_missing_descriptions


###############################################################################
# 🔑 10. Build Level-3 Cover Type Lookup
###############################################################################
# Some Level-3 parent classes are valid IFMAP classes but do not have standalone
# descriptions in the exported cover-type domain table.
#
# This manual crosswalk fills those gaps so Level-3 summaries have readable
# labels.

# Assign Level-3 descriptions using the cover-type domain.
l3_lookup_from_domain <- l4_lookup %>%
  mutate(
    code_digits = nchar(as.character(l4covertype_full))
  ) %>%
  filter(code_digits == 3) %>%
  transmute(
    l3covertype = l4covertype_full,
    l3cover_key = l4cover_key,
    l3_key_source = "Cover_Type_Domain" # Assign data-source identifier.
  )

# Assign Level-3 descriptions manually (using IFMAP key for reference).
# Numeric codes below were identified in previous steps as missing descriptions.
l3_lookup_manual <- tribble(
  ~l3covertype, ~l3cover_key,
  419, "Mixed Upland Deciduous",
  421, "Planted Pines",
  422, "Natural Pines",
  423, "Other (Non-Pine) Upland Conifers",
  424, "Other Upland Conifers",
  431, "Upland Mixed Forest"
) %>%
  mutate(
    l3_key_source = "IFMAP manual crosswalk" # Assign data-source identifier.
  )

# Combine the domain-based and manual-made keys.
l3_lookup <- bind_rows(
  l3_lookup_from_domain,
  l3_lookup_manual
) %>%
  distinct(l3covertype, .keep_all = TRUE) %>%
  arrange(l3covertype)

# Take a look.
l3_lookup

# How many level-3 classes are there in the lookup table?
nrow(l3_lookup)

# Confirm that all derived Level-3 cover codes now have readable descriptions.
l3_lookup_check_final <- stands %>%
  st_drop_geometry() %>%
  distinct(l3covertype) %>%
  left_join(
    l3_lookup,
    by = "l3covertype"
  ) %>%
  arrange(l3covertype)

# Summarize remaining missing Level-3 descriptions.
l3_lookup_check_final %>%
  summarise(
    total_l3_codes = n(),
    l3_codes_with_description = sum(!is.na(l3cover_key)),
    l3_codes_missing_description = sum(is.na(l3cover_key))
  )

# Print any remaining unmatched Level-3 codes, which should only be NA and zero.
l3_lookup_check_final %>%
  filter(is.na(l3cover_key))


###############################################################################
# 🔗 11. Join Level-3 Cover Labels onto Stands
###############################################################################
# Join standardized Level-3 cover labels onto the stands data.
#
# This keeps:
#   • l4covertype_full = original mixed-level IFMAP code (L3, L4, L5)
#   • l4cover_key      = readable full-code description
#   • l3covertype      = standardized first-3-digit parent class
#   • l3cover_key      = readable Level-3 description

stands <- stands %>%
  
  # Join readable Level-3 labels onto stands.
  # Matches stands$l3covertype to l3_lookup$l3covertype.
  left_join(
    l3_lookup,
    by = "l3covertype"
  ) %>%
  
  # Move Level-3 fields near other cover-type fields.
  relocate(
    l3cover_key,
    l3covertype,
    .before = l4cover_key
  )

# Inspect joined Level-3 cover data and take note of row counts by cover type.
stands %>%
  st_drop_geometry() %>%
  count(l3covertype, l3cover_key, sort = TRUE)


###############################################################################
# 🔗 12. Join Associated Domain Labels onto Compartments
###############################################################################
# Add readable management authority and management area (unit) labels to coded compartment 
# fields before joining compartments with stands.

compartments <- compartments %>%
  
  # Join readable management authority labels onto compartments.
  # Matches compartments$management_type to compartments_lookup$management_type.
  left_join(
    authority_lookup,
    by = "management_type"
  ) %>%
  
  # Join readable management area (unit) labels onto compartments.
  # Matches compartments$unit_name to unit_lookup$unit_name.
  left_join(
    unit_lookup,
    by = "unit_name"
  ) %>%
  
  # Move key analysis fields to the front so they are easier to inspect.
  relocate(
    authority_key,
    management_type,
    unit_key,
    unit_name,
    .before = everything()
  )

# Take a look at the new labels.
glimpse(compartments)


###############################################################################
# 🧩 13. Join Compartment Attributes onto Stands
###############################################################################
# The "fc_key" is the unique compartment identifier.
# Many stand records can share the same fc_key, so this is a many-to-one join:
#
#   many stands → one compartment
#
# This adds compartment labels to each stand without clipping geometry.
# Spatial data dropped here because it would get repeated and make the stands
# dataset even bigger and slower. Also, the fc_key method respects the database 
# relationship and avoids geometry-derived complications.

# Select compartment attributes to join.
compartment_attributes <- compartments %>%
  st_drop_geometry() %>% # Drop compartment geometry completely.
  select(
    fc_key,
    authority_key,
    management_type,
    unit_key,
    unit_name
  ) %>%
  distinct(fc_key, .keep_all = TRUE)

# Join stand and compartment data to get a working data set.
stands_working <- stands %>%
  left_join(
    compartment_attributes,
    by = "fc_key"
  )

# This object (stands_working) is the full prepared dataset before any
# exploratory filtering or analysis-specific exclusions.


###############################################################################
# 🎯 14. Filter to Working Authority Dataset
###############################################################################
# Create filter based on management authority of interest. "Switches" (#) can be 
# flipped on or off here to select authority or groups of authorities.
working_authority <- c(
  "Wildlife" 
  # "State Forests",
  # "State Parks",
  # "Partner Lands",
  # "Good Neighbor Authority (USFS)"
)

# Apply filter to the stands working dataset. Note that the object's name stays
# the same.
stands_working <- stands_working %>%
  filter(authority_key %in% working_authority)


###############################################################################
# 💾 15. Save Prepared Output for Downstream Analysis
###############################################################################
# Save one prepared R object for the next script:
#
#   Phase 1, Step 2 — Exploratory Data Analysis and Quality Assurance
#
# RDS preserves the full R object, including spatial geometry, column classes,
# and joined labels. This is better than CSV for the main handoff file.

saveRDS(
  stands_working,
  "stands_working_prepped.rds"
)


###############################################################################
# End of script
###############################################################################

