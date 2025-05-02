#-------------------------------------------------------------------------------
# Convert UTM easting and northing to Longitude (X) and Latitude (Y)
#
# The following code is used to read in the NFI plot location information
# and convert the easting and northing to columns labelled X (longitude)
# and Y (latitude).
#
#-------------------------------------------------------------------------------


library(sf)
library(dplyr)
library(data.table)

#-------------------------------------------------------------------------------
# Load the NFI plot data 
# - assumes that the loaded data includes:
#  1) Easting, 
#  2) Northing,
#  3) utm zone
#  4) FEATURE_ID (or nfi_plot) to link back to VRI polygon or NFI plot data
#-------------------------------------------------------------------------------

nfidatabc <- read.csv("Data/nfi_bc_siteinfo.csv")

#-------------------------------------------------------------------------------
# Calculate the crs value via the EPSG
#-------------------------------------------------------------------------------
get_epsg <- function(zone, north) {
  ifelse(north == "N", 32600 + zone, 32700 + zone)
}

nfidatabc <- nfidatabc %>%
  mutate(epsg = mapply(get_epsg, zone = utm_zone, north = "N"))

# Convert to sf object and transform to WGS84 (EPSG:4326)
convert_sf <- st_as_sf(nfidatabc, coords = c("utm_e", "utm_n"), crs = nfidatabc$epsg[1], remove = FALSE)

# Apply transformation for each unique EPSG
coord_transformed <- convert_sf %>%
  group_by(epsg) %>%
  mutate(geometry = st_transform(geometry, 4326))

# extract the lat and long values and bind back
coords <- st_coordinates(coord_transformed)
nfidatabc_transformed <- cbind(coord_transformed, coords)

# Remove the geometry column
nfidatabc_transformed <- nfidatabc_transformed[, !names(nfidatabc_transformed) %in% "geometry"]

# save the file as a csv 
fwrite(nfidatabc_transformed, "Data/nfidatabc_transformed.csv", na = "")