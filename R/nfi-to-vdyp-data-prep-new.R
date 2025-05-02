#' Convert NFI data to VDYP input format
#'
#' This function reads National Forest Inventory (NFI) ground plot data,
#' transforms it into the input format required by \link[https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/forest-inventory/growth-and-yield-modelling/variable-density-yield-projection-vdyp/publications-and-support]{Variable Density Yield Projection (VDYP 7)},
#' and writes INPUT_POLY.csv and INPUT_LAYER.csv to an output directory.
#'
#' @param nfi_folder Path to folder containing NFI CSV files.
#' @param output_path Path to folder where VDYP input CSVs will be written. Default is current working directory + "nfi_to_vdyp_data".
#' @param remeasurement_number Optional measurement number to filter the data.
#'
#' @return No return value. Writes two CSV files to disk.
#' @export
#' @details
#' The function uses internal mapping tables (\code{map_polygon} and \code{map_layer}) to translate
#' National Forestry Inventory (NFI) data into the format required by VDYP.
#'
#' Key handling rules include:
#' \itemize{
#'   \item \strong{Direct mappings:} For most variables, NFI values are used as-is or default to the specified fallback if missing.
#'   \item \strong{Calculated fields:} Some variables require computation, e.g., percent standing dead trees is calculated as \code{plotvol_standdead / (plotvol_standlive + plotvol_standdead) * 100}.
#'   \item \strong{UTM conversions:} LATITUDE and LONGITUDE are computed from UTM coordinates.
#'   \item \strong{BEC zones:} BEC_ZONE_CODE is assigned using a GIS intersect between NFI plot coordinates and BEC zone polygons.
#'   \item \strong{Species composition:} For species codes and percentages, the function harmonizes small and large tree datasets, recalculates basal area per hectare for dbh >= 7.5cm (and dbh >= 12.5cm), computes relative percentages, and ranks species dominance.
#'   \item \strong{Height and volume metrics:} Metrics like LOREY_HEIGHT_75, BASAL_AREA_125, and WS_VOL_PER_HA_75/125 are computed using filtered live tree data.
#'   \item \strong{Post-processing or fillers:} Some fields are populated with placeholders (e.g., PLOT_NUMBER or FEATURE_ID) for downstream processing or tracing.
#' }
#'
#' The full description of each mapping rule is stored in the \code{Description} column of
#' the \code{map_polygon} and \code{map_layer} datasets included in the package.
#' @import data.table
#' @examples
#' nfi_to_vdyp("development/nfi_data")
nfi_to_vdyp <- function(nfi_folder,
                        output_path = file.path(getwd(), "nfi_to_vdyp_data"),
                        remeasurement_number = NULL) {

  library(data.table)
  library(sf)
  library(bcdata)

  # 1️⃣ Setup output folder
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

  # 3️⃣ Index NFI files
  file_list <- list.files(nfi_folder, "all_gp_.*\\.csv", recursive = TRUE, full.names = TRUE)
  # remove the duplicated all_gp_site_info.csv file paths. It is found on every folder.
  file_list <- file_list[!grepl(pattern = "(?<!nfi_data)/all_gp_site_info.csv", x = file_list, perl = TRUE)]

  # 4️⃣ Read stand table + convert UTM to lat/lon
  site_info_path <- grep(x = file_list, pattern =  file.path(nfi_folder, "all_gp_site_info.csv"), value = TRUE)
  site_info <- fread(site_info_path)[province == "BC",]
  site_info <- convert_utm_to_latlon(site_info)

  # 5 Get BEC zone lookup table
  # bec_zone_table <- get_bec_zone(site_info)

  # 6️⃣ Prepare INPUT_POLY
  message("Preparing INPUT_POLY...")
  input_poly <- prepare_input_poly(mapping = map_poly,
                                   file_list = file_list,
                                   site_info = site_info,
                                   bec_zone_table = bec_zone_table)

  # 7️⃣ Prepare INPUT_LAYER
  message("Preparing INPUT_LAYER...")
  input_layer <- prepare_input_layer(mapping = map_layer,
                                     file_list = file_list,
                                     site_info = site_info,
                                     remeasurement_number = remeasurement_number) |> suppressWarnings()

  # 8️⃣ Write CSV outputs
  fwrite(input_poly, file.path(output_path, "INPUT_POLY.csv"))
  fwrite(input_layer, file.path(output_path, "INPUT_LAYER.csv"))

  message("✅ VDYP input files saved to: ", output_path)
}

