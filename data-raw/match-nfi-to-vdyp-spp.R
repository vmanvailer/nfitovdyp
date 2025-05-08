library(data.table)
library(stringdist)

# Load data
# Copied from the Volume 1 - VDYP7 Overview [Revised February 2019]. Link below:
# https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/forest-inventory/growth-and-yield-modelling/variable-density-yield-projection-vdyp/publications-and-support
species_map <- fread("inst/extdata/nfi_to_vdyp_species_mapping.csv")
trees_list <- fread("inst/extdata/treesplistV45-added names - vini.csv", encoding = "Latin-1")

# Normalize to lowercase and trim whitespace
species_map[, Full_name_clean := tolower(trimws(`Full name`))]
trees_list[, COMMON_EN_clean := tolower(trimws(COMMON_EN))]

# Compute string distances (build distance matrix)
dist_matrix <- stringdistmatrix(species_map$Full_name_clean, trees_list$COMMON_EN_clean, method = "jw")

# Find closest match index and score
closest_idx <- apply(dist_matrix, 1, which.min)
closest_score <- apply(dist_matrix, 1, min)

# Add fuzzy match results
species_map[, fuzzy_match := trees_list$COMMON_EN_clean[closest_idx]]
species_map[, match_score := 1 - closest_score]  # Convert distance to similarity score

# Join CODE_GENU and CODE_SPEC from trees_list using the fuzzy_match
species_map <- merge(
  species_map,
  trees_list[, .(COMMON_EN_clean, CODE_GENU, CODE_SPEC)],
  by.x = "fuzzy_match",
  by.y = "COMMON_EN_clean",
  all.x = TRUE
)

# Fill nfi_tree_genus and nfi_tree_species
species_map[, nfi_tree_genus := CODE_GENU]
species_map[, nfi_tree_species := CODE_SPEC]

# Optional: reorder columns or drop helpers if desired
species_map[, c("CODE_GENU", "CODE_SPEC") := NULL]

# Write to CSV
fwrite(species_map, "nfi_to_vdyp_species_mapping_fuzzy_matched.csv")

# Required manual check and adjustments before adding as data.
species_map_corrected <- fread("inst/extdata/nfi_to_vdyp_species_mapping_fuzzy_matched_corrected.csv")
map_species <- species_map_corrected[, .(`SpcsCode`, nfi_tree_genus, nfi_tree_species)]
usethis::use_data(map_poly, map_layer, bec_zone_table, map_species, overwrite = TRUE)
