#' Mapping table for INPUT_POLY variables
#'
#' This dataset contains the variable mapping between National Forestry Inventory (NFI) data
#' and VDYP INPUT_POLYGON variables. It includes the VDYP variable name, NFI source variables,
#' required data files, variable type, default values, and a short description of the procedure.
#'
#' @format A data.table with columns:
#' \describe{
#'   \item{VDYP Variable}{The target VDYP variable name}
#'   \item{NFI Variable (maps to)}{NFI source variable(s), separated by ";" if multiple}
#'   \item{NFI csv file}{The NFI CSV file(s) to read}
#'   \item{Type}{Expected data type (integer, numeric, character, etc.)}
#'   \item{Defaults To}{Default value if no data is available}
#'   \item{Description}{Description or notes (not used in calculation)}
#' }
#'
#' @source inst/extdata/nfitovdyp_mapping_polygon.csv
"map_poly"

#' Mapping table for INPUT_LAYER variables
#'
#' This dataset contains the variable mapping between National Forestry Inventory (NFI) data
#' and VDYP INPUT_LAYER variables. It specifies how to calculate or extract species composition,
#' height, basal area, and other tree-level attributes for VDYP input.
#'
#' @format A data.table with the same structure as \code{map_polygon}.
#'
#' @source inst/extdata/nfitovdyp_mapping_layer.csv
"map_layer"

#' BEC zone lookup table
#'
#' This dataset provides the Biogeoclimatic Ecosystem Classification (BEC) zone code
#' assigned to each NFI plot location based on latitude and longitude.
#'
#' @format A data.table with columns:
#' \describe{
#'   \item{nfi_plot}{Plot identifier}
#'   \item{loc_id}{Location identifier}
#'   \item{BEC_ZONE_CODE}{Assigned BEC zone code}
#' }
#'
#' @source Computed using \code{bcdata::bcdc_get_data()} and saved with \code{precompute_bec_zone()}.
"bec_zone_table"
