prepare_input_layer <- function(mapping, file_list, site_info, remeasurement_number = NULL) {
  # Load necessary tree data
  ltp_tree_path <- grep("all_gp_ltp_tree.csv", file_list, value = TRUE)
  stp_tree_path <- grep("all_gp_stp_tree.csv", file_list, value = TRUE)

  ltp_tree_h_path <- grep("all_gp_ltp_header.csv", file_list, value = TRUE)
  stp_tree_h_path <- grep("all_gp_stp_header.csv", file_list, value = TRUE)

  ltp <- fread(ltp_tree_path)[nfi_plot %in% site_info$nfi_plot,]
  stp <- fread(stp_tree_path)[nfi_plot %in% site_info$nfi_plot,]

  ltp_h <- fread(ltp_tree_h_path)[nfi_plot %in% site_info$nfi_plot,]
  stp_h <- fread(stp_tree_h_path)[nfi_plot %in% site_info$nfi_plot,]

  ltp_size <- ltp_h[,.(nfi_plot, loc_id, meas_num, meas_plot_size)]
  stp_size <- stp_h[,.(nfi_plot, loc_id, meas_num, meas_plot_size)]

  # Harmonize columns
  stp <- stp[, .(nfi_plot, loc_id, meas_num, meas_date,
                 tree_num = smtree_num, tree_genus = smtree_genus,
                 tree_species = smtree_species, tree_status = smtree_status,
                 dbh = smtree_dbh, height = smtree_ht, vol_total = smtree_vol_total)]

  ltp <- ltp[, .(nfi_plot, loc_id, meas_num, meas_date,
                 tree_num, tree_genus = lgtree_genus,
                 tree_species = lgtree_species, tree_status = lgtree_status,
                 dbh, height, vol_total)]

  ltp <- merge(ltp, ltp_size, all.x = TRUE, by = c("nfi_plot", "loc_id", "meas_num"))
  stp <- merge(stp, stp_size, all.x = TRUE, by = c("nfi_plot", "loc_id", "meas_num"))

  stp <- stp[dbh >= 7.5 & tree_status != "DS"]
  ltp <- ltp[tree_status != "DS"]

  stp[, tree_class := "small"]
  ltp[, tree_class := "large"]

  tree_dt <- rbindlist(list(stp, ltp), use.names = TRUE, fill = TRUE)

  # Calculate basal area per tree (m²)
  tree_dt[, basal_area := pi * (dbh^2) / 40000]

  # Calculate basal area per ha per species
  ba_species <- tree_dt[, .(sum_ba = sum(basal_area)),
                        by = .(nfi_plot, loc_id, meas_num, meas_date, tree_genus, tree_species, meas_plot_size)]
  ba_species[, ba_per_ha := sum_ba / meas_plot_size]

  # Calculate percent composition
  ba_species[, total_ba := sum(ba_per_ha), by = .(nfi_plot, loc_id, meas_num, meas_date)]
  ba_species[, pct_ba := (ba_per_ha / total_ba) * 100]

  # Rank species
  ba_species <- ba_species[order(-pct_ba), rank := seq_len(.N),
                           by = .(nfi_plot, loc_id, meas_num, meas_date)]

  # Filter by remeasurement if requested
  if (!is.null(remeasurement_number)) {
    ba_species <- ba_species[meas_num == remeasurement_number]
    tree_dt <- tree_dt[meas_num == remeasurement_number]
  }

  result_dt <- unique(tree_dt[, .(nfi_plot, loc_id, meas_num)])

  for (i in seq_len(nrow(mapping))) {
    row <- mapping[i, ]
    vdyp_var <- row$`VDYP Variable`
    var_type <- row$Type
    default <- row$`Defaults To`

    if (vdyp_var == "EST_SITE_INDEX_SPECIES_CD") {
      # Keep all original species columns (will handle later if needed)
      tree_sp <- ltp_h[,.(nfi_plot, loc_id, meas_num, site_index_genus, site_index_species)]
      result_dt <- merge(result_dt, tree_sp, by = c("nfi_plot", "loc_id", "meas_num"), all.x = TRUE)
      next

    } else if (grepl("SPECIES_CD_\\d", vdyp_var)) {
      rank_num <- as.integer(gsub("\\D", "", vdyp_var))
      species_cd <- ba_species[rank == rank_num,
                               .(nfi_plot, loc_id, meas_num, value = paste0(tree_genus, tree_species))]
      result_dt <- merge(result_dt, species_cd, by = c("nfi_plot", "loc_id", "meas_num"), all.x = TRUE)
      setnames(result_dt, "value", vdyp_var)

    } else if (grepl("SPECIES_PCT_\\d", vdyp_var)) {
      rank_num <- as.integer(gsub("\\D", "", vdyp_var))
      species_pct <- ba_species[rank == rank_num,
                                .(nfi_plot, loc_id, meas_num, value = pct_ba)]
      result_dt <- merge(result_dt, species_pct, by = c("nfi_plot", "loc_id", "meas_num"), all.x = TRUE)
      setnames(result_dt, "value", vdyp_var)

    } else if (vdyp_var == "LOREY_HEIGHT_75") {
      lorey <- tree_dt[, .(weighted_height = sum(height * basal_area) / sum(basal_area)),
                       by = .(nfi_plot, loc_id, meas_num)]
      result_dt <- merge(result_dt, lorey, by = c("nfi_plot", "loc_id", "meas_num"), all.x = TRUE)
      setnames(result_dt, "weighted_height", vdyp_var)

    } else if (vdyp_var == "BASAL_AREA_75") {
      ba_75 <- tree_dt[dbh > 7.5 & tree_status != "DS",
                        .(ba_75 = sum(basal_area)),
                        by = .(nfi_plot, loc_id, meas_num)]
      result_dt <- merge(result_dt, ba_75, by = c("nfi_plot", "loc_id", "meas_num"), all.x = TRUE)
      setnames(result_dt, "ba_75", vdyp_var)

    } else if (vdyp_var == "STEMS_PER_HA_75") {
      stemsha_75_psize <- tree_dt[dbh > 7.5 & tree_status != "DS",
                        .(stemsha_75_psize = .N/mean(meas_plot_size)),
                        keyby = .(nfi_plot, loc_id, meas_num, meas_plot_size)]
      stemsha_75 <- stemsha_75_psize[,
                        .(stemsha_75 = sum(stemsha_75_psize)),
                        keyby = .(nfi_plot, loc_id, meas_num)]
      result_dt <- merge(result_dt, stemsha_75, by = c("nfi_plot", "loc_id", "meas_num"), all.x = TRUE)
      setnames(result_dt, "stemsha_75", vdyp_var)

    } else if (vdyp_var == "BASAL_AREA_125") {
      ba_125 <- tree_dt[dbh > 12.5 & tree_status != "DS",
                        .(ba_125 = sum(basal_area)),
                        by = .(nfi_plot, loc_id, meas_num)]
      result_dt <- merge(result_dt, ba_125, by = c("nfi_plot", "loc_id", "meas_num"), all.x = TRUE)
      setnames(result_dt, "ba_125", vdyp_var)

    } else if (vdyp_var == "WS_VOL_PER_HA_75") {
      vol_75 <- tree_dt[, .(vol_per_ha = sum(vol_total) / unique(meas_plot_size)),
                        by = .(nfi_plot, loc_id, meas_num)]
      result_dt <- merge(result_dt, vol_75, by = c("nfi_plot", "loc_id", "meas_num"), all.x = TRUE)
      setnames(result_dt, "vol_per_ha", vdyp_var)

    } else if (vdyp_var == "WS_VOL_PER_HA_125") {
      vol_125 <- tree_dt[dbh > 12.5,
                         .(vol_per_ha = sum(vol_total) / unique(meas_plot_size)),
                         by = .(nfi_plot, loc_id, meas_num)]
      result_dt <- merge(result_dt, vol_125, by = c("nfi_plot", "loc_id", "meas_num"), all.x = TRUE)
      setnames(result_dt, "vol_per_ha", vdyp_var)

    } else {

      # General variables: pull or use default
      file_name <- row$`NFI csv file`
      nfi_var <- row$`NFI Variable (maps to)`

      if (length(file_name) == 0 | file_name == "") {
        result_dt[, (vdyp_var) := default]
      } else {
        file_path <- grep(file_name, file_list, value = TRUE)
        dt <- fread(file_path)[nfi_plot %in% site_info$nfi_plot]
        dt_sub <- dt[, .(nfi_plot, loc_id, meas_num, value = get(nfi_var))]
        result_dt <- merge(result_dt, dt_sub, by = c("nfi_plot", "loc_id", "meas_num"), all.x = TRUE)
        setnames(result_dt, "value", vdyp_var)
        result_dt[is.na(get(vdyp_var)), (vdyp_var) := default]
      }
    }

    # Type casting
    if (var_type == "integer") result_dt[, (vdyp_var) := as.integer(get(vdyp_var))]
    if (var_type == "numeric") result_dt[, (vdyp_var) := as.numeric(get(vdyp_var))]
    if (var_type %in% c("string", "character")) result_dt[, (vdyp_var) := as.character(get(vdyp_var))]
  }

  return(result_dt)
}
