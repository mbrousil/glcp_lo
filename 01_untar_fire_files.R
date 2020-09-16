# This script opens the tar files containing data from the Monitoring Trends in
# Burn Severity dataset.

# Store years for use in directory names
y <- 1995:2015

# For each year, extract files corresponding to that year into the deposit_dir
# location
for (i in 1:length(y)) {
  extract_dir <- paste0("../data/inputs/mtbs/",
                        y[i], "/fire_level_tar_files/")
  tar_files <- list.files(path = extract_dir,
                          pattern = "tar.gz")
  deposit_dir <- paste0("../data/derived_products/mtbs")

  for (f in 1:length(tar_files)) {
    untar(tarfile = paste(extract_dir, tar_files[f], sep = "/"),
          exdir = deposit_dir)
  }
}
