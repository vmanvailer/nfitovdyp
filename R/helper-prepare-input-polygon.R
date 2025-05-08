prepare_input_poly <- function(mapping, file_list, site_info, bec_zone_table) {
  result_dt <- copy(site_info[, .(nfi_plot, loc_id, meas_num)])

  for (i in seq_len(nrow(mapping))) {
  # for (i in 30) {
    # This loops through each line of the NFI to VDYP mapping file (call map_poly to look at it)
    # and processes variables based on their type.

    # First get individual values to be used.
    row <- mapping[i, ]
    vdyp_var <- row$`VDYP Variable`
    nfi_vars <- unlist(strsplit(row$`NFI Variable (maps to)`, ";"))
    file_names <- unlist(strsplit(row$`NFI csv file`, ";"))
    var_type <- row$Type
    default <- row$`Defaults To`

    # If they are
    if (vdyp_var %in% c("FEATURE_ID", "MAP_ID")) {
      result_dt[ , (vdyp_var) := paste0("ID", nfi_plot, "L", loc_id, "M", meas_num)]

    } else if (vdyp_var == "LATITUDE") {
    # If they are coordinates add them to the output file as is from site_info.
      result_dt[, LATITUDE := site_info$LATITUDE]

    } else if (vdyp_var == "LONGITUDE") {
      result_dt[, LONGITUDE := site_info$LONGITUDE]

    # If they are BC bec zone code merge the built in data (bec_zone_table).
    } else if (vdyp_var == "BEC_ZONE_CODE") {
      # Assumes bec_zone_table has nfi_plot + loc_id + BEC_ZONE_CODE
      result_dt <- merge(result_dt, bec_zone_table[, .(nfi_plot, loc_id, BEC_ZONE_CODE)],
                         by = c("nfi_plot", "loc_id"), all.x = TRUE)

    # For percentage of dead perform a calculation based on plotvol_standdead and plotvol_standlive
    # MAY NEED TO UPDATE THIS IN THE FUTURE AS ALL OTHER PROCESSED DATA IS DONE FOR 7.5+cm DBH.
    } else if (vdyp_var == "PCT_DEAD") {
      site_info_file <- grep(file_names, file_list, value = TRUE)
      site_info_dt <- fread(site_info_file)[nfi_plot %in% site_info$nfi_plot][, .(nfi_plot, loc_id, meas_num, plotvol_standdead, plotvol_standlive)]
      site_info_dt[, PCT_DEAD := (plotvol_standdead / (plotvol_standlive + plotvol_standdead)) * 100]
      result_dt <- merge(result_dt, site_info_dt[, .(nfi_plot, loc_id, meas_num, PCT_DEAD)],
                         by = c("nfi_plot", "loc_id", "meas_num"), all.x = TRUE)

    } else if (vdyp_var %in% c("PHOTO_ESTIMATION_BASE_YEAR", "REFERENCE_YEAR")) {
      # Create year variable and remove intermediate variables used to calculate LAT and LONG
      yr_value <- copy(site_info[,.(nfi_plot, loc_id, meas_num, meas_date)])
      yr_value[, meas_yr := tstrsplit(meas_date, "-", fixed = TRUE)[[1]]]
      yr_value[, (vdyp_var) := as.integer(meas_yr)]
      yr_value[, `:=` (meas_yr = NULL,
                       meas_date = NULL)
      ]  # clean up intermediate column
      result_dt <- merge(result_dt, yr_value, by = c("nfi_plot", "loc_id", "meas_num"), all.x = TRUE)

    } else {

      # If none of those, either use indicated variable as is or use the default if no variable is assiged.
      # This allows for future expansion of this package
      if (length(file_names) == 0) {
        value <- rep(default, nrow(result_dt))
        result_dt[, (vdyp_var) := value]
      } else {
        file_path <- grep(file_names[1], file_list, value = TRUE)
        dt <- fread(file_path)[nfi_plot %in% site_info$nfi_plot]
        if (length(nfi_vars)>1) {errorCondition(paste0(vdyp_var, "has more than one NFI variable found for it but is being processed as a direct match to NFI"))}
        if (nfi_vars[1] %in% names(dt)) {
          dt_sub <- dt[, .(nfi_plot, loc_id, meas_num, value = get(nfi_vars[1]))]
          dt_sub <- dt_sub[!duplicated(dt_sub),]
          result_dt <- merge(result_dt, dt_sub, by = c("nfi_plot", "loc_id", "meas_num"), all.x = TRUE)
          setnames(result_dt, "value", vdyp_var)
          result_dt[is.na(get(vdyp_var)), (vdyp_var) := default]
        } else {
          result_dt[, (vdyp_var) := default]
        }
      }

      # Type casting
      if (var_type == "integer") result_dt[, (vdyp_var) := as.integer(get(vdyp_var))]
      if (var_type == "numeric") result_dt[, (vdyp_var) := as.numeric(get(vdyp_var))]
      if (var_type %in% c("string", "character")) result_dt[, (vdyp_var) := as.character(get(vdyp_var))]
    }
  }

  return(result_dt)
}
