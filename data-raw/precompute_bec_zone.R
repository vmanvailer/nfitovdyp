# data-raw/precompute_bec_zone.R
library(data.table)
library(sf)
library(bcdata)
library(usethis)

nfi_folder <- file.path("development", "nfi_data")
output_file <- "data/bec_zone_lookup.rda"

# Read stand table
site_info_path <- list.files(nfi_folder, "all_gp_site_info.csv", recursive = TRUE, full.names = TRUE)[1]
site_info <- fread(site_info_path)[province == "BC",]

# Convert UTM to lat/lon
site_info <- convert_utm_to_latlon(site_info)

# Get BEC zones
# Pull BEC zone layer from bcdata
# data name from: https://catalogue.data.gov.bc.ca/dataset/bec-map/resource/3ec24cb4-f78d-48a9-87e7-516763f00cc8
bec <- bcdata::bcdc_get_data("WHSE_FOREST_VEGETATION.BEC_BIOGEOCLIMATIC_POLY")
bec_sf <- st_as_sf(bec)

# Convert site_info to sf
site_info_sf <- st_as_sf(site_info, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)

# Transform BEC to WGS84 (crs 4326)
bec_sf_latlon <- st_transform(bec_sf, sf::st_crs(site_info_sf))

# Spatial join to assign BEC zone
joined <- st_join(site_info_sf, bec_sf_latlon[, c("ZONE")], join = st_intersects)

# Extract and prepare output
bec_zone_table <- as.data.table(joined)[, .(nfi_plot, loc_id, BEC_ZONE_CODE = ZONE)]
bec_zone_table <- bec_zone_table[!duplicated(nfi_plot),]

# Optional: warn on unmatched points
unmatched <- bec_zone_table[is.na(BEC_ZONE_CODE)]
if (nrow(unmatched) > 0) {
  warning("Some points did not match a BEC zone: ", paste(unmatched$nfi_plot, unmatched$loc_id, collapse = "; "))
}

# Save as internal package data (.rda)
usethis::use_data(bec_zone_table, overwrite = TRUE)
