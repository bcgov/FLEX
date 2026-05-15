#####################################################################
#####################################################################
R_version <- paste0("R-",version$major,".",version$minor)
.libPaths(paste0("C:/Program Files/R/",R_version,"/library")) # to ensure reading/writing libraries from C drive
#####################################################################
#####################################################################
## Fisher Habitat Conversion
## TSA: Lakes TSA
## Output Year: 0, 10, 20 or 30 into the future (scenarios)
############################################################

## ---- 0. Libraries ----
library(sf)
library(tidyverse)
library(data.table)
library(raster)
library(fasterize)
library(exactextractr)
library(bcdata)
library(janitor)

## ---- 1. User Inputs ----
year <- 2055 
ssp <- 6 # snap_shot_period = 6 or 30 years into the future

# Local inputs
poly_path <- "./lakes_resultant.shp"
attrib_path <- "lakes_resultant.csv"

# Output directory
out_dir <- "fisher_hab_data"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

## ---- 2. Read Polygon Geometry ----
polys <- st_read(poly_path, quiet = TRUE) |>
  clean_names()

## ---- 3. Download Lakes TSA (bcdata) ----
# bcdc_search("bec", res_format = "wms")
tsa <- bcdc_get_data(
  "dab3daa4-d502-4b95-b39e-5f746a1041ed"  # Timber Supply Areas
) |>
  clean_names() |>
  filter(tsa_number_description == "Lakes TSA") |>
  st_transform(st_crs(polys))


## ---- 4. Clip Polygons to TSA ----
# plot data to see best way to clip
ggplot() +
  geom_sf(data = polys) +
  geom_sf(data = tsa)

# looks like the two areas overlap perfectly so no need to do a clip
  

polys <- st_transform(polys, st_crs(tsa)) # already did this in the step #3
# polys <- polys[st_intersects(polys, tsa, sparse = FALSE), ]
polys <- st_filter(polys, tsa)

## ---- 5. Download + Clip BEC Zones by TSA ----
bec <- bcdc_get_data(
  "f358a53b-ffde-4830-a325-a5a03ff672c3"  # BEC Zones (from BEC Map)
) |>
  clean_names() |>
  st_transform(st_crs(tsa)) |>
  st_intersection(tsa) |>
  select(zone, subzone, variant)

## ---- 6. Attach BEC to Polygons ----
polys <- st_join(polys, bec, left = TRUE)
names(polys)
polys <- polys %>% rename(polygon_id = res30_tag)

## ---- 7. Read Attribute CSV + Join ----
attrib <- read_csv(attrib_path) |>
  clean_names() |>
  filter(snap_shot_period == ssp)
# join only year that we want - note that attrib contains >1 scenario

polys <- polys |>
  left_join(attrib, by = "polygon_id") |>
  filter(!is.na(age))

## ---- 8. Provincial Raster Template (BC Albers) ----
# prov_rast <- raster(
#   nrows = 15744,
#   ncols = 17216,
#   xmn = 159587.5,
#   xmx = 1881187.5,
#   ymn = 173787.5,
#   ymx = 1748187.5,
#   crs = st_crs(3005)$proj4string,  # BC Albers
#   resolution = 100,
#   vals = 0
# )

## ---- 8. Raster Template (BC Albers) to match aoi----

# ensure AOI is in BC Albers
aoi_sf <- st_transform(polys, 3005)

# create raster template from AOI extent
aoi_rast <- raster(
  extent(aoi_sf),
  resolution = 100,
  crs = st_crs(aoi_sf)$proj4string
)

# optionally initialize values
values(aoi_rast) <- 0


## ========================================================
##  HABITAT DEFINITIONS (STRUCTURAL – SQL‑FREE)
## ========================================================

## ---- 9. Denning Habitat ----
names(polys)
denning <- polys |>
  filter(
    age >= 125,
    crown_closure >= 30,
    dbhq >= 28.5,
    basal_area >= 29.75,
    zone %in% c("SBS", "IDF", "MS", "SBPS", "BWBS")
  ) |>
  mutate(denning = 1)

denning_ras <- fasterize(denning, aoi_rast, "denning")
denning_ras[is.na(denning_ras[])] <- 0
writeRaster(denning_ras,
            file.path(out_dir, paste0("denning_", year, ".tif")),
            overwrite = TRUE)

plot(denning_ras)
## ---- 10. Resting – Rust ----
rest_rust <- polys |>
  filter(
    age >= 72,
    crown_closure >= 25,
    dbhq >= 19.6,
    basal_area >= 32
  ) |>
  mutate(rust = 1)

rest_rust_ras <- fasterize(rest_rust, aoi_rast, "rust")
rest_rust_ras[is.na(rest_rust_ras[])] <- 0
writeRaster(rest_rust_ras,
            file.path(out_dir, paste0("rest_rust_", year, ".tif")),
            overwrite = TRUE)

## ---- 11. Resting – Cavity ----
rest_cavity <- polys |>
  filter(
    crown_closure >= 25,
    dbhq >= 30,
    basal_area >= 32,
    height >= 35
  ) |>
  mutate(cavity = 1)

rest_cavity_ras <- fasterize(rest_cavity, aoi_rast, "cavity")
rest_cavity_ras[is.na(rest_cavity_ras[])] <- 0
writeRaster(rest_cavity_ras,
            file.path(out_dir, paste0("rest_cavity_", year, ".tif")),
            overwrite = TRUE)

## ---- 12. Resting – CWD (Proxy) ----
rest_cwd <- polys |>
  filter(
    age >= 100,
    dbhq >= 18,
    height >= 19
  ) |>
  mutate(cwd = 1)

rest_cwd_ras <- fasterize(rest_cwd, aoi_rast, "cwd")
rest_cwd_ras[is.na(rest_cwd_ras[])] <- 0
writeRaster(rest_cwd_ras,
            file.path(out_dir, paste0("rest_cwd_", year, ".tif")),
            overwrite = TRUE)

## ---- 13. Movement Habitat ----
movement <- polys |>
  filter(
    age > 1,
    crown_closure >= 25
  ) |>
  mutate(movement = 1)

movement_ras <- fasterize(movement, aoi_rast, "movement")
movement_ras[is.na(movement_ras[])] <- 0
writeRaster(movement_ras,
            file.path(out_dir, paste0("movement_", year, ".tif")),
            overwrite = TRUE)

## ---- 14. Open Habitat ----
open <- polys |>
  filter(
    is.na(crown_closure) |
      crown_closure <= 10 |
      age <= 1
  ) |>
  mutate(open = 1)

open_ras <- fasterize(open, aoi_rast, "open")
open_ras[is.na(open_ras[])] <- 0
writeRaster(open_ras,
            file.path(out_dir, paste0("open_", year, ".tif")),
            overwrite = TRUE)

## ========================================================
##  EXTRACT HABITAT TO POLYGONS
## ========================================================

polys$hab_den  <- exact_extract(denning_ras, polys, "sum")
polys$hab_rust <- exact_extract(rest_rust_ras, polys, "sum")
polys$hab_cav  <- exact_extract(rest_cavity_ras, polys, "sum")
polys$hab_cwd  <- exact_extract(rest_cwd_ras, polys, "sum")
polys$hab_mov  <- exact_extract(movement_ras, polys, "sum")
polys$hab_opn  <- exact_extract(open_ras, polys, "sum")

## ---- 15. Save Final Spatial Output ----
st_write(
  polys,
  file.path(out_dir, paste0("fisher_habitat_", year, "_lakes_tsa.shp")),
  overwrite = TRUE, append = FALSE
)

############################################################
## END OF SCRIPT
############################################################

