# data-raw/prepare_mapping_tables.R

library(data.table)
library(usethis)

# 1️⃣ Read raw CSVs from inst/extdata
map_poly <- fread("inst/extdata/nfitovdyp_mapping_polygon.csv")
map_layer <- fread("inst/extdata/nfitovdyp_mapping_layer.csv")

# 2️⃣ Save as internal package data (.rda in /data)
usethis::use_data(map_poly, map_layer, bec_zone_table, map_species, overwrite = TRUE)

message("✅ Saved map_poly and map_layer as internal package data in /data")
