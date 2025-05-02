# NFI to VDYP data
#'
#' NFI to VDYP data
#'
#' This function extracts and properly formats Canada's National Forest Inventory (NFI) data to Variable Density Yield Projection 7 (VDYP7) input format.
#' It attempts to maximize the use of data but there are still some improvements that can be made to use the full extent of NFI data. Check 'Value' section for more details.
#'
#' It uses the current NFI data structure in csv files and is based on file naming patterns for ground plot data e.g. "all_gp_trees".
#' It maximizes the use of data by making several assumptions on which data should be included and how. Several of those assumptions can be changed by the user by modifying the function parameters e.g. calculate_ef_for_dbh_bin = TRUE.
#' For full details on how each OSM input variable is constructed and which assumptions are made consult the details.
#'
#' @param nfi_folder String. The path to the folder containing the NFI data.
#' @param scenario_name String.
#' @param remeasurement_number Integer.
#' @param include_small_trees Logical. Defaults to true to maximize data use. Will include all individual trees measured on the small plot data. See page 32 of \link[https://nfi.nfis.org/resources/groundplot/Gp_guidelines_v5.0.pdf]{Ground Plot Guidelines}.
#' @param dbh_filter Numeric. A field to filter out trees based on DBH. Will pick DBH higher than or equal to this value.
#' @param output_path String. Use to output the SQLite database that can be passed to OSM.
#' @return Returns a list with two data frames (data.tables) and a writes a database on specified output_path. The data frames should match the input data exactly as in \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{Open Stand Model}.
#' Multiple assumptions are made on the creation of each variable and they are described below.
#' \itemize{
#'    \item \strong{POLYGON DEFINITION TABLE}
#'          \describe{
#'            \item{FEATURE_ID}{Uame as `nfi_plot` in NFI data.}
#'            \item{INVENTORY_STANDARD_CODE}{Default to "V" for Vegetation Resources Inventory (VRI).}
#'            \item{BEC_ZONE_CODE}{The biogeoclimatic zone (BEC zone) extracted from \link[]{}}
#'            \item{CFS_ECOZONE}{The CFS from `meas_date` in \emph{all_gp_site_info.csv} NFI data.}
#'            \item{PHOTO_ESTIMATION_BASE_YEAR}{The stand age from `site_age` in \emph{all_gp_ltp_header.csv} NFI data. Uses arithmetic average age of trees deemed suitable for compiling site age. For more details see page 129 of \link[https://nfi.nfis.org/resources/groundplot/Gp_guidelines_v5.0.pdf]{Ground Plot Guidelines}.}
#'            \item{REFERENCE_YEAR}{Number of plots for a given stand. Defaults to 1 where each NFI plot will assumed to represent a stand.}
#'            \item{PCT_DEAD}{Ratio of total sample area (all plots) that supports productive tree growth. Calculated as described in `calculate_stockable`}
#'            }
#'    \item \strong{LAYER DEFINITION TABLE}
#'          \describe{
#'            \item{FEATURE_ID}{Same as `nfi_plot` in NFI data.}
#'            \item{EST_SITE_INDEX_SPECIES_CD}{Same as `tree_num` in \emph{all_gp_lgtree_plot.csv} and `smtree_num` in \emph{all_gp_smtree_plot.csv}}
#'            \item{ESTIMATED_SITE_INDEX}{NFI Species are mapped to OSM species based on the `Acadian_SpeciesList` table contained in the \link[https://forusresearch.com/downloads/osm/OSMv2.25.1/OSMv2.25.1_Demo.zip]{OSM demo package}.
#'                           NFI codes are mapped to the `GENUS` and `SPECIES` column of the `Acadian_SpeciesList` and the variable `OSM_AD_CmdKey` from that table is used.}
#'            \item{CROWN_CLOSURE}{DBH of individual trees or average DBH for each DBH Bin classes when `calculate_ef_for_dbh_bin = TRUE`.}
#'            \item{BASAL_AREA_75}{Height of individual trees or average height for each DBH Bin classes when `calculate_ef_for_dbh_bin = TRUE`.}
#'            \item{STEMS_PER_HA_75}{Trees per hectare this record represents. i.e., tree expansion factor. Calculated as described in the argument `calculate_ef_for_dbh_bin`.}
#'
#'            \item{SPECIES_CD_1}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}.}
#'            \item{SPECIES_PCT_1}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}.}
#'            \item{SPECIES_CD_2}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}. Could potentially be extracted from `height` of `stem_cond = "B"` while using `height_prj` as OSM `HT`.}
#'            \item{SPECIES_PCT_2}{Crown Ratio. Calculated for both large and small trees when, at minimum, `crown_cond` and `crown_base` data are avaialble but may also use `crow_top` if avaialble. Calculated as `(crown_top-crown_base)/HT` or `(crown_top-HT)/HT`.}
#'            \item{SPECIES_CD_3}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}.}
#'            \item{SPECIES_PCT_3}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}.}
#'            \item{SPECIES_CD_4}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}. May be partially inferred from \emph{plot_origin.csv} variables `veg_origin` and `regen_type`}
#'            \item{SPECIES_PCT_4}{Derived from `age_total` variable of \emph{all_gp_ltp_tree_age.csv}. For more details see psgr 27 of \link[https://nfi.nfis.org/resources/groundplot/GP_compilation_procedures_2.4.pdf]{Compilation proceedures}.}
#'            \item{SPECIES_CD_5}{Derived from `lgtree_status` (Large trees) or `smtree_status` (Small trees). If classified as DS = Dead Standing then it is assigned 9999 to be initialized in the snag pool. LS (Live Stading), LF (Live fallen) or M (Missing) are assumed to as live (set to 0).
#'            \item{SPECIES_PCT_5}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}.}
#'            \item{SPECIES_CD_6}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}.}
#'            \item{SPECIES_PCT_6}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}.}
#'
#'            \item{EST_AGE_SPP1}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}.}
#'            \item{EST_HEIGHT_SPP1}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}.}
#'            \item{EST_AGE_SPP2}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}. Could potentially be extracted from `height` of `stem_cond = "B"` while using `height_prj` as OSM `HT`.}
#'            \item{EST_HEIGHT_SPP2}{Crown Ratio. Calculated for both large and small trees when, at minimum, `crown_cond` and `crown_base` data are avaialble but may also use `crow_top` if avaialble. Calculated as `(crown_top-crown_base)/HT` or `(crown_top-HT)/HT`.}
#'            \item{LOREY_HEIGHT_75}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}.}
#'            \item{BASAL_AREA_125}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}.}
#'            \item{WS_VOL_PER_HA_75}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}. May be partially inferred from \emph{plot_origin.csv} variables `veg_origin` and `regen_type`}
#'            \item{WS_VOL_PER_HA_125}{Derived from `age_total` variable of \emph{all_gp_ltp_tree_age.csv}. For more details see psgr 27 of \link[https://nfi.nfis.org/resources/groundplot/GP_compilation_procedures_2.4.pdf]{Compilation proceedures}.}
#'            \item{SPECIES_CD_5}{Derived from `lgtree_status` (Large trees) or `smtree_status` (Small trees). If classified as DS = Dead Standing then it is assigned 9999 to be initialized in the snag pool. LS (Live Stading), LF (Live fallen) or M (Missing) are assumed to as live (set to 0).
#'            \item{SPECIES_PCT_5}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}.}
#'            \item{SPECIES_CD_6}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}.}
#'            \item{SPECIES_PCT_6}{Not extract from NFI data. Defaults to NaN in OSM. See OSM input page for \link[https://forusresearch.com/downloads/osm/help/OSM.HelpFiles/OSM.Input.htm]{details}.}
#'            }
#' }
#'
#' @import data.table
#' @import RSQLite
#' @importFrom tidyr separate
#' @import sf
#' @export
nfi_to_osm <- function(nfi_folder,
                       scenario_name = "nfi_simulation",
                       remeasurement_number = NULL,
                       include_small_trees = TRUE,
                       dbh_filter = 1,
                       output_path = getwd()
) {


  map_poly <- readxl::read_excel(path = file.path(getwd(), "development","mapped_variables_nfitovdyp.xlsx"), sheet = "INPUT_POLY")
  map_laye <- readxl::read_excel(path = file.path(getwd(), "development","mapped_variables_nfitovdyp.xlsx"), sheet = "INPUT_LAYER")


  <- unique(map_poly$`NFI csv file`)[1]



  if(!dir.exists(output_path)){
    stop("Specified directory below does not exist\n\n\t", output_path)
  }
  # nfi_folder <- file.path(getwd(), "development", "nfi_data")
  file_list <- list.files(nfi_folder,  "all_gp_.*\\.csv", recursive = TRUE, full.names = TRUE)

  # --- Stand Table ------------------------------------------------------------
  # Read site_info
  site_info_path <- grep(x = file_list, pattern =  file.path(nfi_folder, "all_gp_site_info.csv"), value = TRUE)
  site_info <- fread(site_info_path)

  ## Scenario ------------------------------------------------------------------
  site_info[, Scenario := scenario_name]

  # Select and rename columns for plot table
  stand_table <- site_info[, .(
    Scenario = Scenario,
    nfi_plot = nfi_plot,
    loc_id = loc_id, # filtering column
    meas_num = meas_num,# Filtering column
    meas_date = meas_date, # intermediate variable to calculate YEAR (SurveyYear)
    utm_n = utm_n, # intermediate variable to calculate LAT
    utm_e = utm_e, # intermediate variable to calculate LONG
    utm_zone = utm_zone, # intermediate variable to calculate LAT/LONG
    province = province
  )]

  # Filter to a given field campaign
  if(is.null(remeasurement_number)){
    message("No remeasurement number chosen. Will include all remeasurements. \n\nSurveyID will use the name pattern ID {nfi_plot} L {loc_id} M {meas_num}\n nfi_plot = NFI plot id\n loc_id = Location ID (used when plot change locations)\n meas_num = Remeasurement number\n\n")
  } else {
    stand_table <- stand_table[meas_num == remeasurement_number]
    if(nrow(stand_table) == 0){
      stop(paste0("There is no data for remeasurement number ", remeasurement_number, "\n\n"))
    }
  }

  # Location change (loc_id) may not impact model setup.
  # stand_table[loc_id == 0,]

  ## Survey Year --------------------------------------------------------------
  # Create year variable and remove intermediate variables used to calculate LAT and LONG
  stand_table[, SurveyYear := tidyr::separate(.SD, col = meas_date, into = "meas_yr", sep = "-", extra = "drop"), .SDcols = "meas_date"]
  stand_table[, SurveyYear := as.integer(SurveyYear)]

  ## Survey Age ---------------------------------------------------------------
  # Read ltp_tree_header
  ltp_header_path <- grep(x = file_list, pattern =  "ltp_header.csv", value = TRUE)
  ltp_header <- fread(ltp_header_path)

  stand_table <- merge(stand_table, ltp_header[,.(
    nfi_plot, loc_id, meas_date, meas_num, # Id fields
    site_age,                              # Age field
    meas_plot_size,
    nom_plot_size # size field (For "Stockable" variable)
    )], by = c("nfi_plot", "loc_id", "meas_date", "meas_num"), all.x = TRUE)

  data.table::setnames(stand_table, old = "site_age", new = "SurveyAge")

  ## Plots --------------------------------------------------------------------
  stand_table[,Plots := 1]

  ## Stockable ----------------------------------------------------------------
  # If measured is chosen
  if(calculate_stockable){
    message("Calculating 'Stockable' as measured plot size / nominal plot size (where both data exists).\n\n")
    } else {
    message("Setting 'Stockable' to 1.\n\n")
    }

  stand_table[, Stockable := fifelse(calculate_stockable & !is.na(meas_plot_size) & !is.na(nom_plot_size) & nom_plot_size > 0,
                                     meas_plot_size/nom_plot_size,
                                     1)
  ]

  ## Convert UTM to Lat Long --------------------------------------------------
  # First, create a function to convert UTM to lat/lon
  utm_to_longlat <- function(easting, northing, zone) {
    # Create an SF object
    points <- data.frame(x = easting, y = northing)
    sp <- st_as_sf(points, coords = c("x", "y"), crs = paste0("+proj=utm +zone=", zone, " +datum=WGS84"))

    # Transform to WGS84 (latitude/longitude)
    longlat <- st_transform(sp, crs = "+proj=longlat +datum=WGS84")

    # Extract coordinates
    coords <- st_coordinates(longlat)
    return(data.frame(lat = coords[, "Y"], lon = coords[, "X"]))
  }

  # Apply the UTM to lat/lon conversion *by zone*
  stand_table[, `:=`(Y = NA_real_, X = NA_real_)] # Initialize columns
  zones <- unique(stand_table$utm_zone) # Get unique UTM zones

  for (zone in zones) {
    rows <- stand_table$utm_zone == zone
    coords <- utm_to_longlat(stand_table[rows, utm_e], stand_table[rows, utm_n], zone)
    stand_table[rows, `:=`(Y = coords$lat, X = coords$lon)]
  }

  # --- Model variant ----------------------------------------------------------
  error_model_variant = "Please choose either 'Acadian' or 'Newfoundland and Labrador' as a model_variant."
  if(length(model_variant) != 1){
    stop(error_model_variant)
  }

  pt <- setNames(
    c("New Brunswick", "Prince Edward Island", "Nova Scotia", "Newfoundland and Labrador"),
    c("NB", "PE", "NS", "NL")
  )
  wrong_pt <- setdiff(provinces, pt)
  if(length(wrong_pt) > 0){
    stop(paste0("The following provinces are not part allowed or are mispelled:\n\t", paste(wrong_pt, collapse = "\n\t")))
  }

  pt_filter <- pt[pt %in% provinces] |> names()
  stand_table <- stand_table[province %in% pt_filter,]

  if(model_variant == "Acadian"){
    not_allowed_pt <- setdiff(provinces, c("New Brunswick", "Prince Edward Island", "Nova Scotia"))
    if(length(not_allowed_pt) > 0){
      message(paste0("Warning | '", not_allowed_pt, "' is not calibrated for the Acadian model.\n"))
    }

    if(nrow(stand_table) == 0){
      stop(paste0("There is no data for remeasurement number ", remeasurement_number, "for any of:\n", paste0(paste0("\t", provinces), collapse = "\n")))
    }

    # Zone
    stand_table[, Zone := province]

    column_order <- c("Scenario", "nfi_plot", "loc_id", "meas_num", "SurveyYear", "SurveyAge", "Plots", "Stockable", "X", "Y", "Zone", "Management", "BGI")

  } else if(model_variant == "Newfoundland and Labrador"){

    if(!"NL" %in% pt_filter){
      message("Warning | You removed the Newfoundland and Labrador from the 'provinces' argument but specified model_variant = 'Newfoundland and Labrador'.\nPerhaps you meant to use 'model_variannt = Acadian'?\n\nModel will default the 'District' column to 4 'NL'.\n")
      # District will default to 4 as per:
      # https://forusresearch.com/downloads/osm/help/OSM.Variants/OSM.Variants.NL/OSM.Variants.NL.HelpFiles/OSM.Variants.NL.InputTables.htm
    }
    column_order <- c("Scenario", "nfi_plot", "loc_id", "meas_num", "SurveyYear", "SurveyAge", "Plots", "Stockable", "X", "Y", "Management")

  } else {

    stop(error_model_variant)

  }

  # --- Management (Extra column) ---------------------------------------------
  treatment_path <- grep(x = file_list, pattern =  "all_gp_treatment.csv", value = TRUE)
  treatment <- fread(treatment_path, select = c("nfi_plot", "loc_id", "meas_date", "meas_num", "treat_type", "treat_yr"))

  stand_table <- merge(stand_table, treatment, all.x = TRUE)
  stand_table[, meas_yr := as.integer(substr(meas_date, 1, 4))]
  stand_table[, treat_last25_yt := fifelse(!is.na(meas_yr) &  meas_yr > 0 & !is.na(treat_yr) & treat_yr > 0, meas_yr-treat_yr <= 25, FALSE)]
  stand_table[, Management :=  fifelse(treat_type == "PC" & treat_last25_yt, "PartialCut",
                                      fifelse(treat_type == "CC" & treat_last25_yt, "Clearcut",
                                              fifelse(treat_type == "PT" & treat_last25_yt, "PCT", "None")))]


  # --- BGI -------------------------------------------------------------------
  if (model_variant == "Acadian"){
  stand_table <- merge(stand_table, bgi_values, by = c("nfi_plot", "loc_id", "meas_date", "meas_num", "province"), all.x = TRUE)
  }
  stand_table <- stand_table[,..column_order]

  # --- Tree Table ------------------------------------------------------------

  # Prepare species list found on the Acadian model to be merged with nfi data. We want the proper species code to use on OSM.
  acadian_splist_dt <- copy(acadian_species_list)
  setDT(acadian_splist_dt)
  acadian_gnsp_only <- acadian_splist_dt[, .(GENUS, SPECIES, OSM_AD_CmdKey)]
  acadian_gnsp_only[, `:=` (NFI_GENUS = toupper(substr(GENUS, 1, 4)),
                            NFI_SPECIES = toupper(substr(SPECIES, 1, 3))
  )]
  acadian_gnsp_only[, NFI_SPECIES := fifelse(SPECIES == "saccharum", "SAH", NFI_SPECIES)]


  ## --- Large Tree Table ------------------------------------------------------
  # Read ltp_tree
  ltp_tree_path <- grep(x = file_list, pattern =  "ltp_tree.csv", value = TRUE)
  ltp_tree <- fread(ltp_tree_path)

  # Merge the two tables
  large_tree_table <- merge(ltp_tree, ltp_header, by = c("nfi_plot", "loc_id", "meas_date", "meas_num"), all.x = TRUE)

  if(!is.null(remeasurement_number)){
    large_tree_table <- large_tree_table[meas_num == remeasurement_number]
  }

  # Filter only the nfi plots to be used.
  large_tree_table <- large_tree_table[nfi_plot %in% unique(stand_table$nfi_plot)]


  # Location change (loc_id) may not impact model setup.
  # large_tree_table[loc_id == 0,]

  # Select and rename columns for tree table
  large_tree_table <- large_tree_table[, .(
    nfi_plot = nfi_plot,
    loc_id = loc_id,
    meas_num = meas_num,
    meas_date = meas_date,
    meas_plot_size = meas_plot_size,
    nom_plot_size = nom_plot_size,
    tree_num = tree_num,
    tree_genus = lgtree_genus,
    tree_species = lgtree_species,
    DBH = dbh,
    stem_cond = stem_cond,
    HT = fifelse(stem_cond != "B" & height > 0, height, fifelse(stem_cond != "B" & height < 0 & !is.na(height_prj), height_prj, NA)),
    HTK = fifelse(stem_cond == "B" & !is.na(height_prj) & height_prj > 0, height_prj, NA),
    crown_cond = crown_cond,
    crown_base = crown_base,
    crown_top = crown_top,
    tree_status = fifelse(lgtree_status == "DS", "Dead", "Live"),
    tree_size = "Large"
  )]

  map_tree_species <- function(tree_table) {
  species_columns <- c("tree_genus", "tree_species")

  # Step 1: Add Softwood/Hardwood GROUP info
  tree_table <- merge(tree_table,
                      unique(nfi_species[, .(GROUP, CODE_GENU, CODE_SPEC)]),
                      by.x = species_columns,
                      by.y = c("CODE_GENU", "CODE_SPEC"),
                      all.x = TRUE)

  # Step 2: Full genus + species match to OSM species list
  tree_table <- merge(tree_table,
                      acadian_gnsp_only[NFI_SPECIES != "SPP", .(NFI_GENUS, NFI_SPECIES, OSM_AD_CmdKey)],
                      by.x = species_columns,
                      by.y = c("NFI_GENUS", "NFI_SPECIES"),
                      all.x = TRUE)

  # Step 3: Fallback match on genus only where species match failed
  unmatched <- is.na(tree_table$OSM_AD_CmdKey)

  if (any(unmatched)) {
    genus_only_lookup <- acadian_gnsp_only[NFI_SPECIES == "SPP", .(NFI_GENUS, OSM_AD_CmdKey)]

    tree_table[unmatched,
      OSM_AD_CmdKey := genus_only_lookup[.SD,
                                            on = .(NFI_GENUS = tree_genus),
                                            x.OSM_AD_CmdKey]]
  }

  # Step 4: Final fallback if no genus match either → OS (Softwood), OH (Hardwood), or XX
  tree_table[, OSM_AD_CmdKey := fifelse(is.na(OSM_AD_CmdKey) & GROUP == "Softwood", "OS",
                                 fifelse(is.na(OSM_AD_CmdKey) & GROUP == "Hardwood", "OH",
                                 fifelse(is.na(OSM_AD_CmdKey), "XX", OSM_AD_CmdKey)))]

  return(tree_table)
}

  tree_table <- map_tree_species(tree_table = large_tree_table)

  ## --- Small Tree Table ------------------------------------------------------
  if(include_small_trees){
    message("Including small trees.\n\n")

    # Read stp_tree and stp_header
    stp_header_path <- grep(x = file_list, pattern =  "stp_header.csv", value = TRUE)
    stp_tree_path <- grep(x = file_list, pattern =  "stp_tree.csv", value = TRUE)

    stp_header <- fread(stp_header_path)
    stp_tree <- fread(stp_tree_path)

    # Merge the two tables
    small_tree_table <- merge(stp_tree, stp_header, by = c("nfi_plot", "loc_id", "meas_date", "meas_num"), all.x = TRUE)

    # Filter for selected measurement number
    if(!is.null(remeasurement_number)){
      small_tree_table <- small_tree_table[meas_num == remeasurement_number]
    }

    # Filter only the nfi plots to be used.
    small_tree_table <- small_tree_table[nfi_plot %in% unique(stand_table$nfi_plot)]

    # Find max tree_num per plot and meas_num in large trees
    max_large_ids <- tree_table[, .(max_large_id = max(tree_num, na.rm = TRUE)),
                                by = .(nfi_plot, meas_num)]

    # Join with small tree table
    small_tree_table <- merge(small_tree_table, max_large_ids, by = c("nfi_plot", "meas_num"), all.x = TRUE)

    # Replace NAs (if no large trees in plot/meas_num) with 0
    small_tree_table[is.na(max_large_id), max_large_id := 0]

    # Renumber small tree IDs to continue from large tree IDs
    small_tree_table[, tree_num := max_large_id + .I, by = .(nfi_plot, meas_num)]

    # Drop helper column
    small_tree_table[, max_large_id := NULL]

    # Select and rename columns for small tree table
    small_tree_table <- small_tree_table[, .(
      nfi_plot = nfi_plot,
      loc_id = loc_id,
      meas_num = meas_num,
      meas_date = meas_date,
      meas_plot_size = meas_plot_size,
      nom_plot_size = nom_plot_size,
      tree_num = tree_num,
      tree_genus = smtree_genus,
      tree_species = smtree_species,
      DBH = smtree_dbh,
      HT = fifelse(stem_cond != "B" & smtree_ht > 0, smtree_ht, fifelse(stem_cond != "B" & smtree_ht < 0 & !is.na(smtree_ht_prj), smtree_ht_prj, NA)),
      HTK = fifelse(stem_cond == "B" & !is.na(smtree_ht_prj) & smtree_ht_prj > 0, smtree_ht_prj, NA),
      tree_status = fifelse(smtree_status == "DS", "Dead", "Live"),
      tree_size = "Small"
    )]

    small_tree_table <- map_tree_species(tree_table = small_tree_table)
    order_col <- intersect(names(tree_table), names(small_tree_table))

    small_tree_table <- small_tree_table[,..order_col]
    tree_table <- rbindlist(list(tree_table, small_tree_table), fill = TRUE)
  }


  ## --- Expansion Factor (Stems) ----------------------------------------------
  if(dbh_filter > 0) message("Including only trees with DBH >= ", dbh_filter, "\n\n")
  tree_table <- tree_table[DBH >= dbh_filter]

  message("Calculating expansion factor as 1/measured plot size\n\n")
  tree_table[,Stems := fifelse(!is.na(nom_plot_size) & nom_plot_size > 0, (1/nom_plot_size), 1/0.04)]

  # }

  ## ToCut = NaN --------------
  ## Weight = NaN -------------
  ## HTK = NaN ----------------

  ## --- Crown Ratio (CR) -------------------------------------------------------
  # Crown condition 1 all foliage, branch and twigs present. 2 is some or small foliage lost but branches and twigs present, 3+ major loss.
  # Crown base and top can have missing data.
  # Stem condition (I = Intact, B = Broken, M = Missing) Not considering stem condition.
  message("Calculating Crown Ratio ('CR') where data is available.\n\n")
  tree_table[,CR := fifelse(crown_cond > 0 & crown_cond <= 2.5 & !is.na(crown_base) & is.na(crown_top), (crown_top-crown_base)/HT,
                            fifelse(crown_cond > 0 & crown_cond <= 2.5 & !is.na(crown_base) & !is.na(HT), (HT-crown_base)/HT,
                                    NA))]
  ## DBHI = NaN --------
  # = possible but skip
  ## HTI = NaN ---------
  # = possible but skip

  ## --- Origin  ----------------------------------------------------------------
  # Some data possible from plot_origin.csv variables 'veg_origin' and 'regen_type'

  ## --- Born -------------------------------------------------------------------
  age_path <- grep(x = file_list, pattern = "tree_age.csv", value = TRUE)
  message("Getting 'Born' variable from tree age table.\n\n")
  age_table <- fread(age_path, select = c("nfi_plot", "loc_id", "meas_date", "meas_num", "tree_num", "age_total"))
  tree_table <- merge(tree_table, age_table, by = c("nfi_plot", "loc_id", "meas_date", "meas_num", "tree_num"), all.x = TRUE)
  data.table::setnames(tree_table, old = "tree_num", new = "TreeID")


  ## --- Died ------------------------------------------------------------------
  #  = estimate between measurements or 9999

  # DS = Dead fallen, LF = Live Fallen, LS = Live Stading, M = Missing.
  # Assumes all which are not dead to be live standing.
  message("Identifying dead trees. They will be assumed to have died 7 years ago as per OSM defaults.\n\n")
  tree_table[,Died := fifelse(tree_status == "Dead", 9999, 0)]

  ## Risk = 0 ------------------------------------------------------------------
  ## Grade = 0 -----------------------------------------------------------------
  ## Wrap-up -------------------------------------------------------------------

  # Create ids like in standplot
  if(is.null(remeasurement_number)){
    stand_table[ ,SurveyID := paste0("ID", nfi_plot, "L", loc_id, "M", meas_num)]
    stand_table[, `:=` (
      nfi_plot = NULL,
      loc_id = NULL,
      meas_num = NULL
      )]

    tree_table[ ,SurveyID := paste0("ID", nfi_plot, "L", loc_id, "M", meas_num)]
    tree_table[, nfi_plot := NULL]
    data.table::setnames(tree_table, old = c("OSM_AD_CmdKey", "age_total"), new = c("Species", "Born"))

  } else {
    data.table::setnames(stand_table, old = c("nfi_plot") , new = c("SurveyID"))
    data.table::setnames(tree_table, old = c("nfi_plot", "OSM_AD_CmdKey", "age_total"), new = c("SurveyID", "Species", "Born"))
  }

  # Select only final columns
  if(model_variant == "Acadian"){
    stand_table_column_order <- c("Scenario", "SurveyID", "SurveyYear", "SurveyAge", "Plots", "Stockable", "X", "Y", "Zone", "Management", "BGI")
  } else {
    stand_table_column_order <- c("Scenario", "SurveyID", "SurveyYear", "SurveyAge", "Plots", "Stockable", "X", "Y", "Management")
  }
  stand_table <- stand_table[,..stand_table_column_order]
  tree_table_column_order <- c("SurveyID", "TreeID", "Species", "DBH", "HT", "Stems", "CR", "Born", "Died")
  tree_table <- tree_table[,..tree_table_column_order]

  # Checking for errors or missed merges.
  in_tree_not_in_stand <- setdiff(unique(tree_table$SurveyID), unique(stand_table$SurveyID))
  in_stand_not_in_tree <- setdiff(unique(stand_table$SurveyID), unique(tree_table$SurveyID))

  unmatched_list <- paste(paste0("\t", in_stand_not_in_tree),  collapse = "\n")

  # Warnings and errors
  if(length(in_stand_not_in_tree)>0 & !include_small_trees){
    message(paste0("Warning | The NFI plots below have only small trees. Those will be removed since you have set 'include_small_trees = FALSE'\n", unmatched_list, "\n\n"))
  } else if (length(in_stand_not_in_tree)>0 & include_small_trees){
    message(paste0("Warning | NFI plots below could not be matched to tree data. Those will be removed from the Stand List Table\n",
                   "You can double check the source file at\n\t", ltp_tree_path, "\n\n", "for the nfi_plot ids\n", unmatched_list, "\n\n",
                   "If there is an issue, contact the developer.\n"))
  }

  if(length(in_tree_not_in_stand)>0){
    stop("Something is wrong with the code. There are tree data that does not match any stand data records. Contact developer.\n")
  }

  # Final data
  osm_input_data <- list(OSM_StandList = stand_table,
                         OSM_TreeList = tree_table)

  db_output_path <- file.path(output_path, paste0("nfitoosm_", scenario_name, ".sqlite"))

  # Write out SQLite
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = db_output_path)
  lapply(names(osm_input_data[1:2]), function(tbl_name){
    RSQLite::dbWriteTable(con, tbl_name, osm_input_data[[tbl_name]], overwrite = TRUE)
  }
  )
  RSQLite::dbDisconnect(con)

  message("OSM input table successfully created at:\n\n\t", db_output_path)

  #Return data
  return(osm_input_data)
}
