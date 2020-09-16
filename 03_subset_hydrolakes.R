# This script subsets the HydroLAKES V1.0 shapefile to include only data
# needed to produce the India figure (Fig 3). This is to reduce the file
# size being worked with in the India figure script.


# Load necessary packages -------------------------------------------------

library(tidyverse)
library(raster)


# 1. Load, subset, and re-export HydroLAKES -------------------------------

# Pull in the full hydrolakes shapefile
hydro <- shapefile(x = "../data/inputs/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10.shp")

# Convert the HydroLAKES shapefile to a data frame format, then reduce it to
# only the columns needed for use in India map
hydro_reduced <- hydro %>%
  as_tibble() %>%
  dplyr::select(Hylak_id, Lake_type)

# Export new version as a .csv file
write.csv(x = hydro_reduced,
          file = "../data/derived_products/hydrolakes_id_and_laketype.csv",
          row.names = FALSE)