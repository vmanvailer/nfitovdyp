#' Convert UTM coordinates to latitude/longitude
#'
#' Converts UTM easting, northing, and zone columns in site_info to latitude and longitude.
#'
#' @param site_info Data table with UTM coordinates and zone.
#' @import data.table
#' @return Data table with added LATITUDE and LONGITUDE columns.
#' @export
convert_utm_to_latlon <- function(site_info) {

  if(!is.data.table(site_info)) setDT(site_info)

  utm_to_longlat <- function(easting, northing, zone) {
    points <- data.frame(x = easting, y = northing)
    sp <- st_as_sf(points, coords = c("x", "y"), crs = paste0("+proj=utm +zone=", zone, " +datum=WGS84"))
    longlat <- st_transform(sp, crs = "+proj=longlat +datum=WGS84")
    coords <- st_coordinates(longlat)
    data.frame(lat = coords[, "Y"], lon = coords[, "X"])
  }

  # site_info[, `:=`(LATITUDE = NA_real_, LONGITUDE = NA_real_)]
  for (zone in unique(site_info$utm_zone)) {
    rows <- site_info$utm_zone == zone
    coords <- utm_to_longlat(site_info[rows, utm_e], site_info[rows, utm_n], zone)
    site_info[rows, `:=` (LATITUDE = coords$lat, LONGITUDE = coords$lon)]
  }

  return(site_info)
}
