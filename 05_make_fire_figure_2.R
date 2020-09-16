# A script to create figure 2 (coupling forest fire acreage data with basin
# and lake locations) for the L&O Bulletin manuscript

# The script can be divided into two main sections
# 1. Load and prep the data
#   1.1 Fire data
#   1.2 Subset PNW lake from GLCP
#   1.3 Bring in basins and join with lakes
# 2. Make the map


# Load necessary packages -------------------------------------------------

library(sf)
library(USAboundaries)
library(spdplyr)
library(tidyr)
library(ggplot2)
library(sp)
library(rgdal)
library(maptools)
library(RColorBrewer)
library(ggpubr)
library(data.table)
library(raster)
library(cowplot)
library(tidyverse)


# 1. Load and prep data ---------------------------------------------------


# 1.1 Fire data -----------------------------------------------------------

# Load MTBS database fire metadata
fire_metadata <- read.csv("../data/derived_products/combined_fire_metadata.csv")

# Add year grouping to fire data
fire_metadata <- fire_metadata %>%
  mutate(year_group = case_when(
    year %in% c(1995, 1996, 1997, 1998, 1999) ~ "1995 - 1999",
    year %in% c(2000, 2001, 2002, 2003, 2004) ~ "2000 - 2004",
    year %in% c(2005, 2006, 2007, 2008, 2009) ~ "2005 - 2009",
    year %in% c(2010, 2011, 2012, 2013, 2014, 2015) ~ "2010 - 2015"),
    fire_acres_t = fire_acres / 1000)


# 1.2 Subset PNW lakes from GLCP ------------------------------------------

# Load GLCP and pull lake locations
glcp <- fread("../data/inputs/glcp.csv",
              integer64 = "character")

# Get just lake location data
lake_locs <- glcp %>%
  dplyr::select(Hylak_id, centr_lat, centr_lon) %>%
  unique()

# Convert lake locations to sf format and set CRS
lake_metadata <- st_as_sf(lake_locs, coords = c("centr_lon", "centr_lat"))

lake_metadata <- st_set_crs(lake_metadata, "+proj=longlat +datum=WGS84 +no_defs")

# Get spatial data for PNW
pnw <- us_states(states = c("Idaho", "Washington", "Oregon"))

# Spatially subset lakes in the PNW
fire_hybas_pnw <- as_Spatial(st_join(x = lake_metadata,
                                     y = pnw,
                                     left = FALSE))

# Pull PNW lakes from GLCP and classify data into year groupings
lake_pnw_subset <- glcp %>%
  dplyr::select(year, Hylak_id, seasonal_km2, centr_lat, centr_lon) %>%
  filter(Hylak_id %in% fire_hybas_pnw$Hylak_id) %>%
  group_by(Hylak_id) %>%
  mutate(year_group = case_when(
    year %in% c(1995, 1996, 1997, 1998, 1999) ~ "1995 - 1999",
    year %in% c(2000, 2001, 2002, 2003, 2004) ~ "2000 - 2004",
    year %in% c(2005, 2006, 2007, 2008, 2009) ~ "2005 - 2009",
    year %in% c(2010, 2011, 2012, 2013, 2014, 2015) ~ "2010 - 2015"))

# Aggregate at each lake within the time frame
lake_pnw_subset_summary <- lake_pnw_subset %>%
  group_by(Hylak_id, year_group) %>%
  summarise(mean_seas_km2 = mean(seasonal_km2)) %>%
  left_join(x = .,
            y = lake_locs,
            by = c("Hylak_id")) %>%
  group_by(Hylak_id) %>%
  mutate(mean_seas_scored = scale(mean_seas_km2))


# 1.3 Bring in basins and join with lakes ---------------------------------

# Pull HydroBASINS lvl 5 basins in and subset to PNW states
basins <- read_sf("../data/inputs/hydrobasins_world/hybas_lev05_merge.shp")

basins_pnw <- st_join(x = basins,
                      y = pnw,
                      left = FALSE)

# Check to make sure things look right
ggplot() +
  geom_sf(data = basins_pnw) +
  geom_sf(data = pnw, fill = NA, color = "salmon")

# Take a look
colnames(basins_pnw)

# Set fire_metadata to NAD83
fire_sf <- st_as_sf(x = na.omit(fire_metadata),
                    coords = c("centr_lon", "centr_lat"),
                    crs = 4269)

# Gather lakes falling within PNW basins
basin_lake_join <- st_join(x = basins_pnw,
                           y = lake_metadata,
                           left = TRUE)

# Generate lake counts per basin
basin_lake_counts <- basin_lake_join %>%
  as.data.frame() %>%
  dplyr::select(HYBAS_ID, Hylak_id) %>%
  unique() %>%
  group_by(HYBAS_ID) %>%
  summarize(lake_count = n())

# Join the counts to the main basin dataset now
basins_with_counts <- left_join(x = basins_pnw,
                                y = basin_lake_counts,
                                by = c("HYBAS_ID"))

# Make the map
temporal_basin_fire_plot <- ggplot() +
  # Basins with fill for lake count
  geom_sf(data = basins_with_counts, aes(fill = lake_count),
          alpha = 0.8) +
  # Change fill color
  scale_fill_gradient(low = "white", high = "black", name = "Lake count") +
  # Add state overlays
  geom_sf(data = pnw, colour = "black", size = 0.8, fill = NA) +
  # Add fire acreage layer
  geom_sf(data = fire_sf,
          aes(color = fire_acres_t, size = fire_acres_t),
          alpha = 1, pch = 19) +
  # Change fire color
  scale_color_viridis_c(option = "inferno", name = "Fire acreage (thousands)") +
  # Break out by year group
  facet_wrap(. ~ year_group) +
  # Remove legend for fire acreage
  scale_size(guide = FALSE) +
  # Edit plot layout
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "bottom",
        strip.text.x = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 14, angle = 15),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  # Edit legends
  guides(fill = guide_colourbar(barwidth = 16, barheight = 2, order = 1),
         color = guide_colorbar(barwidth = 16, barheight = 2, order = 2)) +
  xlab("Longitude") +
  ylab("Latitude")


# 2. Make the map ---------------------------------------------------------

# Export the map
ggsave(filename = "../figures/temporal_fire_basin_chloropleth_plot.png",
       plot = temporal_basin_fire_plot, device = "png",
       height = 12, width = 12, units = "in",
       dpi = 300)
