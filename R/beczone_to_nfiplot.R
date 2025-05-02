#-------------------------------------------------------------------------------
#
# Intersect the NFI plots with BEC Zone to get the
# proper BEC zone codes required by VDYP.
#
# The NFI plot data has 'prov_eco_type' listing BEC zone info. However, it is not
# in a format that is accepted by VDYP and is not consistent throughout for it
# to be easily converted to a common format. For these reasons, an intersect with
# BEC zone GIS layer is needed.
#
# The BEC Map is available from the BC Data Catalogue, through a custom download.
#
# BEC Map URL: https://catalogue.data.gov.bc.ca/dataset/f358a53b-ffde-4830-a325-a5a03ff672c3
#
# The code does the following:
# 1) Reads in the BEC map gis layer (BEC_BIOGEOCLIMATIC_POLY/BEC_POLY_polygon.shp)
# 2) Reads in the NFI plot location file (user defined)
# 3) Corrects geometries in the BEC layer
# 4) Converts the NFI plot location info to a spatial file
# 5) Intersects the NFI plots with the BEC map layer
# 6) Saves the resulting file
#
#-------------------------------------------------------------------------------

library(sf)
library(terra)
library(dplyr)

#-------------------------------------------------------------------------------
# 1) Read in BEC map gis layer
#-------------------------------------------------------------------------------

# path to bec zone shape file
bec_shapefile_path <- "~/<yourpath>/BEC_BIOGEOCLIMATIC_POLY/BEC_POLY_polygon.shp"

#-------------------------------------------------------------------------------
# 2) Read in the NFI plot location file
# - assumes that nfi_plot_latlon.csv contains (at least) the following columns:
#   1) FEATURE_ID (to link to nfi_plot)
#   2) X (Longitude)
#   3) Y (Latitude)
#-------------------------------------------------------------------------------

sample_data <- read.csv("Data/nfi_plot_latlon.csv")

#-------------------------------------------------------------------------------
# 3) Load the bec zone shapefiles and make corrections to geometries
#-------------------------------------------------------------------------------

# function to read the bec zones
read_bec_zone <- function(bec_shapefile_path) {
  bec_zones <- sf::st_read(bec_shapefile_path)
  return(bec_zones)
}

bec_zones <- read_bec_zone(bec_shapefile_path)

# Check the CRS of the BEC Zones
sf::st_crs(bec_zones)

# Check the validity of the geometries 
is_valid <- sf::st_is_valid(bec_zones)
# print(summary(!is_valid)) # See how many invalid geometries there are (# of TRUE = number of invalid geometries)

# If there are invalid geometries, fix them
bec_zones_fixed <- sf::st_make_valid(bec_zones)

# Check validity again after the fix
is_valid_after_fix <- sf::st_is_valid(bec_zones_fixed)
# print(summary(!is_valid_after_fix))

#-------------------------------------------------------------------------------
# 4) Convert the NFI plot location information to a spatial point file and correct projection
#-------------------------------------------------------------------------------

# The create_spatial_points function
create_spatial_points <- function(csv_data, lon_col = "Longitude", lat_col = "Latitude", crs_string = "EPSG:4326") {
  # Assuming your CSV data frame is named 'csv_data' and has 'Longitude' and 'Latitude' columns
  if (!all(c(lon_col, lat_col) %in% names(csv_data))) {
    stop("Longitude and/or Latitude columns not found in the CSV data.")
  }
  
  points_sf <- sf::st_as_sf(csv_data, coords = c(lon_col, lat_col), crs = crs_string)
  return(points_sf)
}

# Run the spatial point function
spatial_points_object <- create_spatial_points(sample_data, lon_col = "X", lat_col = "Y", crs_string = "EPSG:4326")
# plot(spatial_points_object)

# Reproject the point data to NAD83 (EPSG:4269)
spatial_points_nad83 <- sf::st_transform(spatial_points_object, crs = "EPSG:4269")
# Check the CRS of the reprojected point data
sf::st_crs(spatial_points_nad83)

#-------------------------------------------------------------------------------
# 5) Intersect the bec polygons with the NFI plots
#-------------------------------------------------------------------------------

# Function that intersect the bec polygons with the NFI plot locations
intersect_with_bec <- function(points_sf, bec_zones) {
  if (sf::st_crs(points_sf) != sf::st_crs(bec_zones)) {
    stop("Coordinate Reference Systems of the point data and BEC Zone data do not match.")
  }
  
  intersected <- sf::st_intersection(points_sf, bec_zones)
  return(intersected)
}

# Run the intersection function
intersected_data <- intersect_with_bec(spatial_points_nad83, bec_zones = bec_zones_fixed)

# Extract BEC Zone
if ("ZONE" %in% names(intersected_data)) {
  sample_data$BEC_Zone <- intersected_data$ZONE
} else {
  warning("BEC Zone information ('ZONE' column) not found in the intersection output.")
  sample_data$BEC_Zone <- NA
}

#-------------------------------------------------------------------------------
# 6) Output the results to a CSV file
#-------------------------------------------------------------------------------
write.csv(sample_data, file = 'Data/sample_data.csv', row.names = FALSE)
