####Satellite imagery extraction - Dead creeks####
library(terra)
library(tidyverse)
library(raster)
library(purrr)

##Use QGIS to find points
#stack satellite images for extraction
# Set your folder path (use forward slashes on Mac)
raster_folder <- "Data/sat_deadcreeks/"

# List all raster files (e.g., GeoTIFFs)
raster_files <- list.files(raster_folder, pattern = "\\.tif$", full.names = TRUE)

# Stack (SpatRaster) — automatically aligns by extent/resolution
sat_stack <- rast(raster_files)

# Check
sat_stack

#Create base names from filenames
base_names <- tools::file_path_sans_ext(basename(raster_files))

# Each raster has 4 layers
n_layers_per_raster <- 4

# Expand the names to match total number of layers
new_names <- unlist(lapply(base_names, function(x) {
  paste0(x, "_band", 1:n_layers_per_raster)
}))

# Apply to the stack
names(sat_stack) <- new_names

points <- vect("Data/Dead_creek_extraction.shp")
ex_vals <- extract(sat_stack, points)

write.csv(ex_vals, file = 'Data/dead_creek_extracted.csv')

df <- ex_vals

# Drop the unnamed index column if present
df <- df %>% select(-starts_with("Unnamed"))

# Identify the columns that contain raster values
value_cols <- names(df)[!names(df) %in% "ID"]

# Extract the unique raster identifiers (everything before "_band")
raster_ids <- unique(str_extract(value_cols, "^[^_]+_[^_]+_[^_]+_[^_]+"))

# Create a named list where each entry is one raster (4-band subset)
raster_list <- lapply(raster_ids, function(rid) {
  # Select ID and all bands for that raster
  sub_df <- df %>%
    select(ID, starts_with(rid))
  
  # Optionally, rename band columns more cleanly (band1–band4)
  names(sub_df) <- c("ID", paste0("band", 1:4))
  
  return(sub_df)
})

# Name each list entry by its raster ID (the file/date pattern)
names(raster_list) <- raster_ids

#clean up names
names(raster_list) <- format(
  as.Date(str_extract(names(raster_list), "^\\d{8}"), "%Y%m%d"),
  "%Y_%m"
)
#Use red/blue ratio as suggested by other studies. Can also try red edge/blue
CDOM_redblue <- map_dfr(
  names(raster_list),
  function(r_name) {
    df <- raster_list[[r_name]]
    
    # Calculate band3/band1 ratio per point and average it
    mean_ratio <- mean(df$band3 / df$band1, na.rm = TRUE)
    
    # Return as a one-row tibble
    tibble(
      raster = r_name,
      mean_ratio = mean_ratio
    )
  }
)

CDOM_nirblue <- map_dfr(
  names(raster_list),
  function(r_name) {
    df <- raster_list[[r_name]]
    
    # Calculate band3/band1 ratio per point and average it
    mean_ratio <- mean(df$band4 / df$band1, na.rm = TRUE)
    
    # Return as a one-row tibble
    tibble(
      raster = r_name,
      mean_ratio = mean_ratio
    )
  }
)
