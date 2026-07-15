#####################################################################
R_version <- paste0("R-",version$major,".",version$minor)
.libPaths(paste0("C:/Program Files/R/",R_version,"/library")) # to ensure reading/writing libraries from C drive
#####################################################################
## FLEX – Standalone runner (no SpaDES)
#####################################################################

## ---- Libraries ----------------------------------------------------
library(data.table)
library(terra)
library(truncnorm)
library(sampling)
library(BalancedSampling)
library(SpaDES.tools)  # for spread2
library(here)

## ---- Source core model -------------------------------------------
source("FLEX_core.R", echo = TRUE)

## ---- Inputs -------------------------------------------------------

## Habitat raster
habitat_raster <- here("fisher_hab_data/FLEX_BuMo2023_init.tif")

# ---- Load raster ----
rast_stack <- rast(habitat_raster)

# ---- Ensure pixelid exists ----
if (!"pixelid" %in% names(rast_stack)) {
  pix_rast <- rast_stack[[1]]
  values(pix_rast) <- 1:ncell(pix_rast)
} else {
  pix_rast <- rast_stack[[grep("pixelid", names(rast_stack))]]
}

# ---- Convert to table ----
habitat_dt <- as.data.table(rast_stack[])

# ---- Rename columns (user-controlled) ----
setnames(habitat_dt,
         old = c(
           "ras_fisher_pop",
           "ras_fisher_denning_init",
           "ras_fisher_rust_init",
           "ras_fisher_cavity_init",
           "ras_fisher_cwd_init",
           "ras_fisher_movement_init",
           "ras_fisher_open_init"
         ),
         new = c(
           "fisher_pop",
           "denning",
           "rust",
           "cavity",
           "cwd",
           "movement",
           "open"
         ),
         skip_absent = TRUE
)

# ---- Filter habitat ----
habitat_dt <- habitat_dt[fisher_pop > 0]

## ---- Run model ----------------------------------------------------

res <- run_flex_model(
  pix_rast            = pix_rast,
  habitat_dt          = habitat_dt,
  female_hr_table     = female_hr_table,
  survival_rate_table = survival_rate_table,
  repro_rate_table    = repro_rate_table,
  project_years       = 5,
  burn_in             = 5
)

## ---- Outputs ------------------------------------------------------

dir.create("outputs", showWarnings = FALSE)

fwrite(res$report, "outputs/fisher_population.csv")
saveRDS(res$agents, "outputs/final_agents.rds")
