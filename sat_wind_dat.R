##Extraction of areas and centroid for algal bloom data from USF and working up wind data
##Libraries
library(terra)
library(sf)
library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(circular)

tif_files <- list.files('Data/geotiff', pattern = "\\.tif$", full.names = TRUE)

# Initialize lists to store results
results_list <- list()
centroid_summary_list <- list()

for (i in seq_along(tif_files)) {
  
  # Load raster
  r <- rast(tif_files[i])
  
  # Threshold: keep only values >= 0.75
  r_bin <- r >= 0.75
  
  # Label patches (connected groups of cells where r >= 0.75)
  patches_raster <- patches(r_bin, directions = 8)
  
  # Mask: keep only areas where r >= 0.75
  patches_masked <- mask(patches_raster, r_bin, maskvalue = 0)
  
  # Convert patches to polygons (will include a field like 'layer' or 'patches')
  patch_polygons <- as.polygons(patches_masked, dissolve = TRUE, na.rm = TRUE) |> 
    st_as_sf()
  
  # Identify the name of the patch ID column (it’s often "layer" or the raster name)
  id_col <- names(patch_polygons)[1]
  
  # Filter out rows where the patch ID is NA
  patch_polygons <- patch_polygons %>%  filter(!is.na(.data[[id_col]]))
  
  # Skip if no valid polygons
  if (nrow(patch_polygons) == 0) next
  
  # Calculate area and centroid
  patch_polygons <- patch_polygons %>% 
    mutate(
      area_m2 = as.numeric(st_area(geometry)),
      centroid = st_centroid(geometry),
      source_file = basename(tif_files[i])
    )
  
  # Extract centroid coordinates and area
  coords <- st_coordinates(patch_polygons$centroid)
  areas  <- patch_polygons$area_m2
  
  # Weighted average of X and Y using area
  avg_x <- weighted.mean(coords[, 1], w = areas)
  avg_y <- weighted.mean(coords[, 2], w = areas)
  
  # Create sf point for weighted centroid
  avg_centroid <- st_sfc(st_point(c(avg_x, avg_y)), crs = st_crs(patch_polygons))
  
  # Store summary info for this raster
  summary_df <- st_sf(
    source_file = basename(tif_files[i]),
    geometry = avg_centroid
  )
  
  # Save to lists
  results_list[[i]] <- patch_polygons
  centroid_summary_list[[i]] <- summary_df
}

# Combine outputs
all_areas <- do.call(rbind, results_list)
avg_centroids_all <- do.call(rbind, centroid_summary_list)

# Summarize area info per file
area_summary <- all_areas %>% 
  st_drop_geometry() %>%   # remove geometry for summary stats
  group_by(source_file) %>% 
  summarise(
    total_area_m2 = sum(area_m2),
    n_patches = n()
  )

# Combine with avg_centroids_all
area_summary <- left_join(avg_centroids_all, area_summary, by = "source_file")

# Step 1: Transform to UTM Zone 17N (EPSG:26917 for NAD83)
summary_utm <- st_transform(area_summary, crs = 26917)

# Step 2: Extract Easting (X) and Northing (Y) from geometry
area_summary <- summary_utm %>% 
  mutate(
    easting  = st_coordinates(geometry)[, 1],
    northing = st_coordinates(geometry)[, 2]
  ) %>% st_drop_geometry()

write.csv(area_summary, file = 'Data/Clean/sat_dat_usf.csv')
##Now wind data
txt_files <- list.files('Data/Buoy_kw', pattern = "\\.txt$", full.names = TRUE)

# Function to read one file with header in first row
read_with_header_row <- function(file) {
  # Read all lines
  lines <- readLines(file)
  
  # First line = header
  header <- str_split(trimws(lines[1]), "\\s+", simplify = TRUE)
  
  # Read table starting from second line
  df <- read_table(file, skip = 1, col_names = header, col_types = cols(.default = "c"))
  
  # Rename "#YY" to "YY"
  names(df)[names(df) == "#YY"] <- "YY"
  
  # Clean and convert date columns safely
  df <- df %>%
    mutate(
      YY = as.integer(YY),
      MM = str_pad(MM, 2, pad = "0"),
      DD = str_pad(DD, 2, pad = "0"),
      year = ifelse(YY < 100, YY + 2000, YY),
      date_string = paste(year, MM, DD, sep = "-"),
      date = suppressWarnings(ymd(date_string)),
      WSPD = as.numeric(WSPD),
      WSPD = if_else(WSPD == 99.0, NA, WSPD),
      WDIR = as.numeric(WDIR),
      WDIR = if_else(WDIR == 999, NA, WDIR)# robust and forgiving
    ) %>%
    select(-date_string)  # optional cleanup
  
  return(df)
}
# Load all files into a list
data_list <- lapply(txt_files, read_with_header_row)
# Remove the first row of each dataframe in the list
data_list_clean <- lapply(data_list, function(df) {
  df[-1, ]  # drop the first row
})

combined_df <- bind_rows(data_list_clean)
max(combined_df$WDIR, na.rm = T)

# Clean numeric conversion: make WDIR and WSPD numeric
combined_df_clean <- combined_df %>%
  mutate(
    WSPD = suppressWarnings(as.numeric(WSPD)),
    WDIR = suppressWarnings(as.numeric(WDIR))
  ) %>%
  filter(!is.na(date), !is.na(WSPD), !is.na(WDIR))  # drop bad rows

monthly_summary_wind <- combined_df_clean %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(
    mean_WSPD = mean(WSPD, na.rm = TRUE),
    mean_WDIR = as.numeric(mean(circular(WDIR, units = "degrees", template = "geographics"), na.rm = TRUE)),
    n_obs = n()
  )

monthly_summary_wind <- monthly_summary_wind %>% 
  mutate(mean_WDIR = if_else(mean_WDIR < 0, 360 + mean_WDIR, mean_WDIR))

str(monthly_summary_wind)

er <- data.frame(month = as.Date('2022-12-01'), mean_WSPD = (3.599033 + 3.905019)/2, mean_WDIR = (57.12297 + 106.92765)/2)

msw <- monthly_summary_wind %>% select(-n_obs)
msw_full <- rbind(msw, er)

write.csv(msw_full, file = 'Data/Clean/monthly_wind_kw.csv')
