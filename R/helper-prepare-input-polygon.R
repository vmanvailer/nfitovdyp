prepare_input_poly <- function(mapping, file_list, site_info, bec_zone_table) {
  result_dt <- copy(site_info[, .(nfi_plot, loc_id, meas_num)])

  for (i in seq_len(nrow(mapping))) {
  # for (i in 30) {
    row <- mapping[i, ]
    vdyp_var <- row$`VDYP Variable`
    nfi_vars <- unlist(strsplit(row$`NFI Variable (maps to)`, ";"))
    file_names <- unlist(strsplit(row$`NFI csv file`, ";"))
    var_type <- row$Type
    default <- row$`Defaults To`

    if (vdyp_var == "LATITUDE") {
      result_dt[, LATITUDE := site_info$LATITUDE]

    } else if (vdyp_var == "LONGITUDE") {
      result_dt[, LONGITUDE := site_info$LONGITUDE]

    } else if (vdyp_var == "BEC_ZONE_CODE") {
      # Assumes bec_zone_table has nfi_plot + loc_id + BEC_ZONE_CODE
      result_dt <- merge(result_dt, bec_zone_table[, .(nfi_plot, loc_id, BEC_ZONE_CODE)],
                         by = c("nfi_plot", "loc_id"), all.x = TRUE)

    } else if (vdyp_var == "PCT_DEAD") {
      site_info_file <- grep(file_names, file_list, value = TRUE)
      site_info_dt <- fread(site_info_file)[nfi_plot %in% site_info$nfi_plot][, .(nfi_plot, loc_id, meas_num, plotvol_standdead, plotvol_standlive)]
      site_info_dt[, PCT_DEAD := (plotvol_standdead / (plotvol_standlive + plotvol_standdead)) * 100]
      result_dt <- merge(result_dt, site_info_dt[, .(nfi_plot, loc_id, meas_num, PCT_DEAD)],
                         by = c("nfi_plot", "loc_id", "meas_num"), all.x = TRUE)

    } else {

      # General case
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
