# This script extracts relevant fire metadata from the Monitoring Trends in
# Burn Severity database.

# Load necessary packages -------------------------------------------------

library(stringr)
library(tidyr)
library(readr)
library(data.table)

# Read in all the fire metadata files
metadata_list <- list.files(path = "../data/derived_products/mtbs/",
                            pattern = "metadata.txt",
                            full.names = TRUE)

# For each metadata text file, retrieve some data:
for (m in 1:length(metadata_list)) {
  f <- readLines(metadata_list[m])

  # First get fire ID
  cline <- grep("MTBS Fire ID:", f,
                value = TRUE)
  fire_id <- as.character(str_extract(cline, "[a-z]+[0-9]+$"))

  # Second get date of fire
  cline <- grep("Date of Fire: ", f, value = TRUE)
  cline_sep <- strsplit(cline, " ")
  year <- as.numeric(unlist(cline_sep)[[6]])

  # Third, get the acreage burned
  cline <- grep("Acres within Fire Perimeter:", f,
                value = TRUE)
  fire_acres <- as.numeric(str_extract(cline, "[0-9]+$"))

  # Fourth, get the coordinates
  # West
  cline <- grep("West_Bounding_Coordinate: ", f,
                value = TRUE)
  west <- as.numeric(str_extract(cline, "\\-*\\d+\\.*\\d*"))

  # East
  cline <- grep("East_Bounding_Coordinate: ", f,
                value = TRUE)
  east <- as.numeric(str_extract(cline, "\\-*\\d+\\.*\\d*"))

  # North
  cline <- grep("North_Bounding_Coordinate: ", f,
                value = TRUE)
  north <- as.numeric(str_extract(cline, "\\-*\\d+\\.*\\d*"))

  # South
  cline <- grep("South_Bounding_Coordinate: ", f,
                value = TRUE)
  south <- as.numeric(str_extract(cline, "\\-*\\d+\\.*\\d*"))

  # Average the coordinate values to create a center point
  centr_lat <- mean(c(north, south))
  centr_lon <- mean(c(east, west))

  # Lastly, create a data frame of the retrieved values
  if (m == 1) {

    dat_combined <- data.frame(fire_id, year, fire_acres, centr_lat, centr_lon)

  } else {

    temp_dat <- data.frame(fire_id, year, fire_acres, centr_lat, centr_lon)

    dat_combined <- rbind(dat_combined, temp_dat)

  }
}

# Export the new dataset
fwrite(x = dat_combined,
       file = paste("../data/derived_products/combined_fire_metadata.csv"))