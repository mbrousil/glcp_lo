# A script to create figure 3 ("Slopes from regressions of z-scored seasonal
# water surface areas in India from 1995 to 2015") for the L&O Bulletin
# manuscript

# The workflow consists of 3 main steps:
# 1. Load and z-score the necessary data
# 2. 2. Build seasonal & total water area models & extract coeffs
# 3. Create map figure 

# Load necessary packages -------------------------------------------------

library(tidyverse)
library(data.table)
library(maps)
library(RColorBrewer)
library(sf)
library(ggrepel)


# 1. Load data ------------------------------------------------------------

# GLCP dataset
glcp <- fread("../data/inputs/glcp.csv", header = TRUE,
              integer64 = "character")

# Bring in a subset of the hydrolakes data (smaller file size)
hydrolakes <- read.csv(file = "../data/derived_products/hydrolakes_id_and_laketype.csv",
                       stringsAsFactors = FALSE)

# Pull lakes that have a laketype in hydrolakes and are in India. Z-score their
# total and seasonal areas
glcp_hydrolakes_india <- inner_join(x = glcp, y = hydrolakes,
                                    by = c("Hylak_id")) %>%
  filter(country == "India") %>%
  select(Hylak_id, year, seasonal_km2, total_km2, Lake_type) %>%
  group_by(Hylak_id) %>%
  mutate(seasonal_zscore = scale(seasonal_km2),
         total_zscore = scale(total_km2))





# 2. Build seasonal & total water area models & extract coeffs ------------

# Get a list of unique lakes and create filler data for for model outputs
unique_lakes <- unique(glcp_hydrolakes_india$Hylak_id)
beta_seasonal <- rep(0, length(unique_lakes))
beta_total <- rep(0, length(unique_lakes))


# Get coefficients for z-scored lake area models
# In the event of an error, we included tryCatch statements that enable 
# the for loop to continue with the next unique_lake element without 
# stopping the iteration
for(i in 1:length(unique_lakes)){
  
  lakes_filtered <- filter(glcp_hydrolakes_india, Hylak_id == unique_lakes[i])
  
  tryCatch({
    lakes_filtered_seasonal_model <- lm(seasonal_zscore ~ year, data = lakes_filtered)
  }, error = function(e) {beta_seasonal[i] <- NA})
  
  if(!is.na(beta_seasonal[i])){
    beta_seasonal[i] <- coef(lakes_filtered_seasonal_model)[[2]]
  }
  
  tryCatch({
    lakes_filtered_total_model <- lm(total_zscore ~ year, data = lakes_filtered)
  }, error = function(e) {beta_total[i] <- NA})
  
  if(!is.na(beta_total[i])){
    beta_total[i] <- coef(lakes_filtered_total_model)[[2]]
  }
  
}

# Condense coefficients into dataframe
combined_coeffs <- data.frame(cbind(unique_lakes, beta_seasonal, beta_total))
colnames(combined_coeffs)[1] <- "Hylak_id"

# Join coefficients to GLCP data
coeffs_glcp <- inner_join(x = combined_coeffs, y = glcp, by = c("Hylak_id")) %>%
  select(Hylak_id:centr_lon)

# Join coeffs and GLCP to hydro lake type
coeffs_glcp_hydro <- inner_join(x = coeffs_glcp,
                                y = hydrolakes,
                                by = c("Hylak_id" = "Hylak_id")) %>%
  select(Hylak_id:centr_lon, Lake_type) %>%
  mutate(Lake_type_factor = ifelse(test = Lake_type == 1,
                                   yes = "Lake", no = "Reservoir"))

# Create a spatial object from the coordinates
coeffs_as_sf <- st_as_sf(x = coeffs_glcp_hydro,
                         coords = c("centr_lon", "centr_lat"),
                         crs = "+proj=longlat +datum=WGS84 +no_defs")


# 3. Create map figure ----------------------------------------------------

# Get India map data and turn it into a subsettable polygon layer
india_map <- st_as_sf(maps::map(database = "world",
                                regions = "india",
                                plot = FALSE,
                                fill = TRUE)) %>%
  st_cast(., "POLYGON")

# Subset to main land area
india_main <- india_map[14, ] 

# Confirm correct polygon
plot(india_main)

# Get dataset of lake slopes matched to the mainland
lakes_on_mainland <- st_intersection(x = india_main,
                                     y = coeffs_as_sf)

# Coords for city of Chennai
chennai <- tibble(longitude = c(80.266667),
                  latitude = c(13.083333),
                  label = c("Chennai"))

# Plot total surface area lake slopes
map_total <- lakes_on_mainland %>%
  filter(beta_total != 0) %>%
  ggplot() +
  geom_sf(data = india_main,
          fill = "white", colour = "#7f7f7f", size = 0.5) +
  geom_sf(aes(color = beta_total),
          alpha = 0.01, size = 4) +
  geom_text_repel(data = chennai,
                  aes(x = longitude, y = latitude, label = label),
                  size = 6, segment.color = "grey50", nudge_x = 2) +
  geom_point(data = chennai, aes(x = longitude, y = latitude),
             shape = 16) +
  scale_color_distiller(palette = "BrBG", name = "Slope", direction = 1) +
  ylab("") +
  xlab("") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.key.width = unit(0.5, "in"),
        legend.key.height = unit(1, "in"),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 20),
        plot.title = element_text(size = 28),
        axis.text = element_text(size = 18))

# Export the figure
ggsave(filename = "../figures/total_india_map.png", plot = map_total, 
       device = "png", width = 18, height = 12, units = "in")

