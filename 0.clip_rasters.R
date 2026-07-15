#####################################################################
## Clip rasters to BuMo project area
#####################################################################

library(terra)
library(sf)

# Read AOI
aoi <- st_read(
  "//sfp.idir.bcgov/S140/S40203/Ecosystems/Conservation Science/Species/Mesocarnivores/Projects/FLM/2. Data/2.1 GIS/BuMo/BuMo_50kmbuff.shp",
  quiet = TRUE
)

# Convert sf to terra vector
aoi <- vect(aoi)

# List raster files
ras_files <- list.files(
  "//sfp.idir.bcgov/home/perpetual/GIS",
  pattern = "\\.tif$",
  full.names = TRUE
)

# Create output folder if needed
dir.create(
  "//sfp.idir.bcgov/home/perpetual/GIS/clipped",
  showWarnings = FALSE
)

# Clip rasters
for (f in ras_files) {
  
  r <- rast(f)
  
  # Project AOI to raster CRS
  aoi_proj <- project(aoi, crs(r))
  
  # Crop and mask
  r_clip <- mask(crop(r, aoi_proj), aoi_proj)
  
  # Save output
  out_file <- file.path(
    "//sfp.idir.bcgov/home/perpetual/GIS/clipped",
    paste0(tools::file_path_sans_ext(basename(f)), "_clip.tif")
  )
  
  writeRaster(r_clip, out_file, overwrite = TRUE)
}



# r <- rast("//sfp.idir.bcgov/home/perpetual/GIS/clipped/open2023_clip.tif")
# plot(r)

