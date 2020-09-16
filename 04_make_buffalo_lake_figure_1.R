# A script to create figure 1 ("Buffalo Lake comparative analysis & time series)
# for the L&O Bulletin manuscript

# The script has 3 main steps:
# 1. Load necessary data and set defaults
# 2. Makes figures 1a and 1b separately
# 3. Combined figures 1a and 1b into a signle figure

# Load necessary packages -------------------------------------------------

library(data.table)
library(tidyverse)
library(raster)
library(rgeos)
library(sf)
library(viridis)
library(ggpubr)


# 1. Load data and set defaults -------------------------------------------

# Shared color scheme:
colors <- magma(70)

# Read the full GLCP dataset so it can be joined with local subsets
lakes <- fread(input = "../data/inputs/glcp.csv",
               integer64 = "character", stringsAsFactors = F)



# Bring in US state data
us_state <- getData(name = "GADM", country = "us", level = 1)

# Subset US to state
wa <- subset(us_state, NAME_1 == "Washington")
plot(wa)

# Coord sys = WGS84
wa_wgs <- spTransform(x = wa, CRSobj = CRS("+proj=longlat +datum=WGS84"))


# Prep for spatial clip of lake locations with polygons
# Make them an sf object
lake_points <- st_as_sf(x = lakes %>%
                          dplyr::select(Hylak_id, centr_lat, centr_lon) %>%
                          unique(),
                        coords = c("centr_lon", "centr_lat"),
                        crs = "+proj=longlat +datum=WGS84")

# Make them spatial object
lake_points <- as(lake_points, "Spatial")

# Match coord system formatting with polygon
lake_points <- spTransform(lake_points, crs(wa_wgs))

# Perform a spatial intersection
wa_lake_points <- raster::intersect(x = lake_points, y = wa_wgs)


# Map out the lake centroids as a check
plot(wa_wgs)
points(wa_lake_points)


# 2. Make figure ----------------------------------------------------------


# 2.1 Make figure 1a ------------------------------------------------------

# State-level plot

# Get the lake centroids and join them to full GLCP
wa_lakes_df <- as.data.frame(wa_lake_points)

wa_lakes_join <- inner_join(x = lakes,
                            y = dplyr::select(wa_lakes_df, -c(GID_0, NAME_0)),
                            by = c("Hylak_id"))

# Check: Does the number of (WA GLCP rows / number of years) match the number
# of lake points?
(nrow(wa_lakes_join) / 21) == nrow(wa_lakes_df)

# Get the average surface area per lake
wa_lakes_agg <- wa_lakes_join %>%
  group_by(Hylak_id) %>%
  summarise(mean_sa = mean(total_km2))

# Grab Buffalo Lake value (convert to log10 + 1)
# Note that Buffalo Lake is ID 104321
# Users wishing to find a Hylak_id for their particular
# lake of interest could reference the HydroLAKES dataset
# which provides shapefiles for all lake contained in the 
# GLCP. Alternatively, users could find their lake by filtering
# with centr_lat and centr_lon columns to identify a particular
# lake based off the centroid's coordinates. 
buffalo <- log10(
  as.numeric(
    filter(wa_lakes_agg, Hylak_id == 104321)[, "mean_sa"]
  ) + 1
)

# Make a histogram of log10(mean SA + 1) for WA state, noting Buffalo Lake
wa_lakes_fig <- wa_lakes_agg %>%
  ggplot() + 
  geom_histogram(aes(x = log10(mean_sa + 1)),
                 color = "black", fill = "grey", binwidth = 0.05) +
  geom_vline(aes(xintercept = buffalo), color = colors[3], linetype = 5,
             size = 1) +
  xlab("log10(Mean Surface Area + 1)") +
  ylab("Count") +
  theme_classic() +
  theme(text = element_text(size = 25)) +
  scale_y_continuous(expand = c(0, 0.1)) +
  scale_x_continuous(expand = c(0.01, 0))


# 2.2 Make figure 1b ------------------------------------------------------

# Get the time series for Buffalo Lake
buffalo_ts <- wa_lakes_join %>%
  filter(Hylak_id == 104321)

# Plot time series of total, permanent, and seasonal water for Buffalo Lake
time_series_plot <- ggplot(data = buffalo_ts) +
  geom_point(aes(x = year, y = total_km2, color = colors[3]) , size = 5) +
  geom_point(aes(x = year, y = permanent_km2, color = colors[30]), size = 5) +
  geom_point(aes(x = year, y = seasonal_km2, color = colors[45]), size = 5) +
  ylab(bquote('Surface Area (km'^2*')')) +
  scale_colour_manual(name = "", values = colors[c(3, 30, 45)],
                      labels = c("Total Water",  "Permanent Water",
                                 "Seasonal Water")) +
  xlab("Year") +
  theme_classic() +
  theme(text = element_text(size = 25),
        legend.position = "right")


# 3. Combine the figures --------------------------------------------------

# Add '(a)' annotation to figure
wa_lakes_fig_a <- annotate_figure(p = wa_lakes_fig, fig.lab = "(a)",
                                  fig.lab.pos = "top.right",
                                  fig.lab.size = 25, fig.lab.face = "bold")

# Add '(b)' annotation to figure
time_series_plot_b <- annotate_figure(p = time_series_plot, fig.lab = "(b)",
                                      fig.lab.pos = "top.right",
                                      fig.lab.size = 25, fig.lab.face = "bold")

# Combine into a single panel
combined_figs <- ggarrange(wa_lakes_fig_a, time_series_plot_b, ncol = 2, nrow = 1,
                           widths = c(4, 7))

# Export final figure
ggsave(filename = "../figures/buffalo_lake_figure.png", plot = combined_figs,
       dpi = 300, width = 17, height = 5, units = "in")

