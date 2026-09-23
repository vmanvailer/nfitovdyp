# Declare data.table column names used via non-standard evaluation so
# R CMD check does not flag them as undefined global variables.
utils::globalVariables(c(
  ":=", ".", ".N", ".SD",
  # NFI plot identifiers / site info
  "province", "nfi_plot", "loc_id", "meas_num", "meas_date", "meas_yr",
  "utm_zone", "utm_e", "utm_n", "LATITUDE", "LONGITUDE",
  # polygon fields
  "BEC_ZONE_CODE", "plotvol_standdead", "plotvol_standlive", "PCT_DEAD",
  # tree / layer fields
  "meas_plot_size",
  "smtree_num", "smtree_genus", "smtree_species", "smtree_status",
  "smtree_dbh", "smtree_ht", "smtree_vol_total",
  "tree_num", "tree_genus", "tree_species", "tree_status",
  "lgtree_genus", "lgtree_species", "lgtree_status",
  "dbh", "height", "vol_total", "tree_class", "basal_area",
  "SpcsCode", "sum_ba", "ba_per_ha", "total_ba", "pct_ba", "rank",
  "site_index_genus", "site_index_species", "weighted_height",
  "ba_75", "stemsha_75_psize", "stemsha_75", "ba_125", "vol_per_ha"
))
