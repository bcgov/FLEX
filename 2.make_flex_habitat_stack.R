#####################################################################
#####################################################################
R_version <- paste0("R-",version$major,".",version$minor)
.libPaths(paste0("C:/Program Files/R/",R_version,"/library")) # to ensure reading/writing libraries from C drive
#####################################################################
#####################################################################
## Combining habitat to run FLEX
############################################################

library(tidyverse)
library(terra)
library(sf)

# --------------------------------------------------
# 1. Read habitat rasters (from your habitat script)
# --------------------------------------------------

year <- 2055

den  <- rast(paste0("fisher_hab_data/denning_",year,".tif"))
rust <- rast(paste0("fisher_hab_data/rest_rust_",year,".tif"))
cav  <- rast(paste0("fisher_hab_data/rest_cavity_",year,".tif"))
cwd  <- rast(paste0("fisher_hab_data/rest_cwd_",year,".tif"))
mov  <- rast(paste0("fisher_hab_data/movement_",year,".tif"))
opn  <- rast(paste0("fisher_hab_data/open_",year,".tif"))

# --------------------------------------------------
# 2. Pixel ID raster (required)
# --------------------------------------------------

pixelid <- den
pixelid[] <- seq_len(ncell(pixelid))
names(pixelid) <- "pixelid"

# --------------------------------------------------
# 3. Fisher population vector → raster
# --------------------------------------------------

fisher_pop_vec <- st_read("fisher_hab_data/FHE_two_pops.shp", quiet = TRUE)
fisher_pop_vec <- st_transform(fisher_pop_vec, crs(den))

plot(
  fisher_pop_vec["Hab_zone"],
  main = "Habitat Zones"
)

fisher_pop_vec$Hab_code <- recode(
  fisher_pop_vec$Hab_zone,
  "Boreal"           = 1L,
  "Sub-Boreal moist" = 2L,
  "Sub-Boreal dry"   = 3L,
  "Dry Forest 2"     = 4L,
  .default = NA_integer_
)

fisher_pop <- rasterize(
  vect(fisher_pop_vec),
  den,
  "Hab_code",
  background = NA
)

names(fisher_pop)
plot(fisher_pop)

# 1 = boreal
# 2 = sbs-wet
# 3 = sbs-dry
# 4 = dry

names(fisher_pop) <- "ras_fisher_pop"

# --------------------------------------------------
# 4. Rename habitat layers
# --------------------------------------------------
# Use "init" if this represents the starting condition

names(den)  <- "ras_fisher_denning_init"
names(rust) <- "ras_fisher_rust_init"
names(cav)  <- "ras_fisher_cavity_init"
names(cwd)  <- "ras_fisher_cwd_init"
names(mov)  <- "ras_fisher_movement_init"
names(opn)  <- "ras_fisher_open_init"

# --------------------------------------------------
# 5. Mask habitat to fisher range (IMPORTANT)
# --------------------------------------------------

mask_r <- !is.na(fisher_pop)

den  <- den  * mask_r
rust <- rust * mask_r
cav  <- cav  * mask_r
cwd  <- cwd  * mask_r
mov  <- mov  * mask_r
opn  <- opn  * mask_r

# --------------------------------------------------
# 6. Stack and save for FLEX
# --------------------------------------------------

hab_stack <- c(
  pixelid,
  fisher_pop,
  den, rust, cav, cwd, mov, opn
)

writeRaster(
  hab_stack,
  paste0("fisher_hab_data/FLEX_",year,"_init.tif"),
  overwrite = TRUE
)

plot(hab_stack)
