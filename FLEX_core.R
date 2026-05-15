#####################################################################
#####################################################################
## source data for run_FLEX.R
############################################################

library(data.table)
library(terra)
library(sf)
library(truncnorm)
library(sampling)
library(BalancedSampling)
library(SpaDES.tools)  # for spread2
library(stats)


# ---- Debug helper ----
dbg <- function(...) {
  cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n")
}


# ============================================================
# Data tables (HR, repro, survival)
# ============================================================

# Fisher populations 
# 1 = boreal
# 2 = sbs-wet
# 3 = sbs-dry
# 4 = dry

## Female home range parameters
female_hr_table <- data.table (fisher_pop = c (1:4), 
                               # hr_mean = c (3000, 3000, 3000, 3000),
                               hr_mean = c (2880, 2920, 4340, 4530), # actual mean
                               # hr_sd = c (500, 500, 500, 500))
                               hr_sd = c (482, 460, 1120, 571)) # actual SE

## Survival rates
survival_rate_table<-rbindlist(list(
  data.table (fisher_pop = c (1,1,1,1, 2,2,2,2, 3,3,3,3, 4,4,4,4),
              type = "Established",
              cohort = c ("Adult", "Juvenile", "Senior", "Old", "Adult", "Juvenile", "Senior", "Old", "Adult", "Juvenile", "Senior", "Old", "Adult", "Juvenile", "Senior", "Old"),
              # Mean = c (0.86, 0.50, 0.8, 0.2,  0.79, 0.41, 0.7, 0.2,  0.79, 0.41, 0.7, 0.2,  0.79, 0.41, 0.7, 0.2), #Lofroth 2022
              # Mean = c (0.95, 0.55, 0.88, 0.22,
              # # 0.87, 0.45, 0.77, 0.22,  0.87, 0.45, 0.77, 0.22,  0.87, 0.45, 0.77, 0.22), #Fogarty 2022 - no trap
              # Mean = c (0.95, 0.55, 0.88, 0.22,
              # 0.95, 0.50, 0.85, 0.24,  0.95, 0.50, 0.85, 0.24,  0.95, 0.50, 0.85, 0.24), #Fogarty 2022 - no trap + 10% for Columbian
              Mean = c (0.95, 0.55, 0.88, 0.22,
                        0.95, 0.80, 0.85, 0.24,  0.95, 0.80, 0.85, 0.24,  0.95, 0.80, 0.85, 0.24), # unrealistically high
              SD = c (0.1,0.2,0.1,0.1, 0.1,0.2,0.1,0.1, 0.1,0.2,0.1,0.1, 0.1,0.2,0.1,0.1)), 
  data.table (fisher_pop = c (1,1,1,1, 2,2,2,2, 3,3,3,3, 4,4,4,4),
              type = "Disperser",
              cohort = c ("Adult", "Juvenile", "Senior", "Old", "Adult", "Juvenile", "Senior", "Old", "Adult", "Juvenile", "Senior", "Old", "Adult", "Juvenile", "Senior", "Old"),
              Mean = c (0.76, 0.40, 0.7, 0.1,   0.69, 0.31, 0.6, 0.1, 0.69, 0.31, 0.6, 0.1, 0.69, 0.31, 0.6, 0.1),
              SD = c (0.1,0.2,0.1,0.1, 0.1,0.2,0.1,0.1, 0.1,0.2,0.1,0.1, 0.1,0.2,0.1,0.1)) 
))


## Reproduction rates 
repro_rate_table <- data.table (Fpop = c(1,1,2,2,3,3,4,4),
                                Param = c("DR", "LS","DR", "LS","DR", "LS","DR", "LS"),
                                # Mean = c(0.75,2.6, 0.54,1.7, 0.54,1.7, 0.54,1.7), # Lofroth 2022
                                # Mean = c(0.75,2.6, 0.59,1.9, 0.59,1.9, 0.59,1.9), # Fograty 2022 (+10% for Columbian)
                                # Mean = c(0.75,2.6, 0.65,2.0, 0.65,2.0, 0.65,2.0), # Fograty 2022 (20% for Columbian)
                                Mean = c(0.75,2.6, 0.75,2.6, 0.75,2.6, 0.75,2.6), # Fograty 2022 (unrealistically high for Columbian)
                                SD = c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1))

# ============================================================
# Helper: create spread raster
# ============================================================
make_spread_raster <- function(pix_rast, habitat_dt, mask_pixels = NULL) {
  
  out <- pix_rast
  out[] <- 0
  
  habitat_dt[
    denning == 1 | rust == 1 | cavity == 1 | cwd == 1 | movement == 1,
    spreadprob := 1
  ]
  habitat_dt[open == 1, spreadprob := 0.09]
  habitat_dt[is.na(spreadprob), spreadprob := 0.18]
  
  out[habitat_dt$pixelid] <- habitat_dt$spreadprob
  
  if (!is.null(mask_pixels)) {
    out[mask_pixels] <- 0
  }
  
  out
}

# ============================================================
# Initialize population
# ============================================================
initialize_fishers <- function(habitat_dt, pix_rast, female_hr_table, initial_n = NULL){
  
  den_pixels <- habitat_dt[denning == 1, pixelid]
  
  if (is.null(initial_n)) {
    initial_n <- length(unique(den_pixels)) %/% 50
  }
  
  coords <- as.data.table(xyFromCell(pix_rast, den_pixels))
  pi <- inclusionprobabilities(rep(1 / nrow(coords), nrow(coords)), initial_n)
  sel <- lpm(pi, coords)
  
  starts <- den_pixels[sel]
  
  agents <- data.table(
    individual_id = seq_along(starts),
    sex = "F",
    age = sample(3:12, length(starts), replace = TRUE),
    initialPixels = starts
  )
  
  agents <- merge(
    agents,
    habitat_dt[, .(pixelid, fisher_pop)],
    by.x = "initialPixels",
    by.y = "pixelid",
    all.x = TRUE
  )
  
  agents <- merge(
    agents,
    female_hr_table,
    by = "fisher_pop",
    all.x = TRUE
  )
  
  agents[, hr_size := round(rnorm(.N, hr_mean, hr_sd))]
  agents[, size_achieved := NA_real_]
  agents[, d2_score := NA_real_]
  
  agents[, currentPixel := initialPixels]
  
  agents
}

# ============================================================
# Establish territories
# ============================================================
establish_territories <- function(agents, pix_rast, habitat_dt) {
  
  spread_rast <- make_spread_raster(pix_rast, habitat_dt)
  
  
  if (any(is.na(agents$currentPixel))) {
    dbg("WARNING: removing agents with NA currentPixel")
    agents <- agents[!is.na(currentPixel)]
  }
  
  spread_out <- spread2(
    spread_rast,
    start = agents$currentPixel,
    spreadProb = spread_rast,
    exactSize = agents$hr_size,
    allowOverlap = FALSE,
    asRaster = FALSE
  )
  
  terr_sizes <- spread_out[, .N, by = initialPixels]
  
  # ensure no duplicate column
  # Keep only necessary columns before merge (prevents accumulation)
  agents <- agents[, .(
    individual_id,
    fisher_pop,
    age,
    sex,
    initialPixels,
    currentPixel,
    hr_size
  )]
  
  # Merge territory sizes
  agents <- merge(
    agents,
    terr_sizes,
    by = "initialPixels",
    all.x = TRUE
  )
  
  agents[, size_achieved := N]
  agents[, N := NULL]
  
  
  
  territories <- spread_out
  
  list(agents = agents, territories = territories)
}

# ============================================================
# Survival
# ============================================================
survive_fishers <- function(agents, dispersers, survival_rate_table) {
  
  agents[, cohort :=
           fifelse(age <= 1, "Juvenile",
                   fifelse(age <= 5, "Adult",
                           fifelse(age <= 8, "Senior", "Old")))]
  
  agents <- merge(
    agents,
    survival_rate_table[type == "Established"],
    by = c("fisher_pop", "cohort"),
    all.x = TRUE
  )
  
  agents[, survive :=
           rbinom(.N, 1,
                  rtruncnorm(1, 0, 1, Mean, SD)
           )
  ]
  
  agents <- agents[survive == 1]
  agents[, c("Mean", "SD", "survive", "cohort") := NULL]
  
  list(
    agents = agents,
    dispersers = dispersers
  )
}

# ============================================================
# Reproduction
# ============================================================
reproduce_fishers <- function(agents, repro_rate_table, next_id) {
  
  # Only adult females reproduce
  breeders <- agents[age >= 2,
                     .(individual_id, fisher_pop, age, currentPixel)]
  
  
  if (nrow(breeders) == 0) {
    return(list(agents = agents, dispersers = data.table(),next_id = next_id))
  }
  
  # Join once per parameter
  params <- dcast(
    repro_rate_table,
    Fpop ~ Param,
    value.var = "Mean"
  )
  setnames(params, "Fpop", "fisher_pop")
  
  breeders[, c("type", "Mean", "SD") := NULL]
  breeders <- merge(breeders, params, by = "fisher_pop", all.x = TRUE)
  setnames(breeders, c("DR", "LS"), c("DR_mean", "LS_mean"))
  
  # safety check
  if (nrow(breeders) != nrow(unique(breeders, by = "individual_id"))) {
    stop("Duplicate breeders created during reproduction join")
  }
  
  # Bernoulli: does female reproduce?
  breeders[, reproduce :=
             rbinom(.N, 1, pmin(pmax(DR_mean, 0), 1))
  ]
  
  reproducers <- breeders[reproduce == 1]
  
  if (nrow(reproducers) == 0) {
    return(list(agents = agents, dispersers = data.table()))
  }
  
  # Litter size
  reproducers[, litter_size :=
                pmax(0, round(rnorm(.N, LS_mean, 0.5)))
  ]
  
  # Create offspring
  offspring <- reproducers[
    , .(n_offspring = litter_size),
    by = .(individual_id)
  ][
    , .(offspring_id = seq_len(sum(n_offspring))),
    by = individual_id
  ]
  
  # Build dispersers (juveniles)
  
  new_ids <- next_id + seq_len(nrow(offspring))
  
  dispersers <- data.table(
    individual_id = new_ids,
    sex = "F",
    age = 0,
    fisher_pop = agents$fisher_pop[match(offspring$individual_id, agents$individual_id)]
  )
  
  
  list(
    agents = agents,
    dispersers = dispersers,
    next_id = max(new_ids)
  )
  
}


# ============================================================
# Dispersal
# ============================================================
disperse_fishers <- function(dispersers, pix_rast, habitat_dt) {
  
  if (nrow(dispersers) == 0) return(dispersers)
  
  # Probability surface
  spread_rast <- make_spread_raster(pix_rast, habitat_dt)
  
  # Random starting points (for now)
  start_cells <- sample(
    habitat_dt$pixelid,
    nrow(dispersers),
    replace = TRUE
  )
  
  spread <- spread2(
    spread_rast,
    start = start_cells,
    spreadProb = spread_rast,
    maxSize = 200,
    allowOverlap = TRUE,
    asRaster = FALSE
  )
  
  end_locations <- spread[, .SD[.N], by = initialPixels]
  
  dispersers[, currentPixel := end_locations$pixels]
  
  dispersers <- dispersers[!is.na(currentPixel)]
  
  dispersers
}

settle_dispersers <- function(dispersers, agents, pix_rast, habitat_dt, female_hr_table) {
  
  if (nrow(dispersers) == 0) {
    return(list(agents = agents, dispersers = dispersers))
  }
  
  # # Add HR parameters
  # dispersers <- merge(
  #   dispersers,
  #   female_hr_table,
  #   by = "fisher_pop",
  #   all.x = TRUE
  # )
  # 
  # # Assign HR sizes
  # dispersers[, hr_size := round(rnorm(.N, hr_mean, hr_sd))]
  
  
  # Attempt to establish territories
  spread_rast <- make_spread_raster(pix_rast, habitat_dt)
  
  # Clean bad locations
  dispersers <- dispersers[!is.na(currentPixel)]
  
  # Remove duplicates (one per pixel)
  dispersers_unique <- dispersers[!duplicated(currentPixel)]
  
  # Add HR parameters
  dispersers_unique <- merge(
    dispersers_unique,
    female_hr_table,
    by = "fisher_pop",
    all.x = TRUE
  )
  
  # Check
  dbg("missing hr_mean:", sum(is.na(dispersers_unique$hr_mean)))
      
      # Assign HR sizes
      dispersers_unique[, hr_size := round(rnorm(.N, hr_mean, hr_sd))]
  
  territories <- spread2(
    spread_rast,
    start = dispersers_unique$currentPixel,
    spreadProb = spread_rast,
    exactSize = dispersers_unique$hr_size,
    allowOverlap = FALSE,
    asRaster = FALSE
  )
  
  
  success_ids <- unique(territories$initialPixels)
  
  settled <- dispersers_unique[currentPixel %in% success_ids]
  
  # everyone else (including duplicates and failures)
  unsettled <- dispersers[!dispersers$individual_id %in% settled$individual_id]
  
  
  # Convert to agents
  new_agents <- settled[
    , .(
      individual_id,
      sex,
      age,
      initialPixels = currentPixel,
      fisher_pop,
      hr_size
    )
  ]
  
  agents <- rbind(agents, new_agents, fill = TRUE)
  
  list(
    agents = agents,
    dispersers = unsettled
  )
}


# ============================================================
# Aging
# ============================================================
age_fishers <- function(agents, dispersers) {
  agents[, age := age + 1]
  dispersers[, age := age + 1]
  list(agents = agents, dispersers = dispersers)
}

# ============================================================
# Yearly Model Step
# ============================================================

step_flex_year <- function(
    agents,
    dispersers,
    pix_rast,
    habitat_dt,
    survival_rate_table,
    repro_rate_table,
    female_hr_table,
    next_id,
    yr = NA
) {
  
  dbg("YEAR STEP START", yr, "| agents:", nrow(agents), "| dispersers:", nrow(dispersers))
  
  
  if (nrow(agents) == 0) {
    dbg("EXTINCTION: no agents remaining at year", yr)
    return(list(
      agents = agents,
      dispersers = dispersers,
      territories = NULL,
      next_id = next_id,
      extinct = TRUE
    ))
  }
  
  
  # Territories
  dbg("  -> establishing territories")
  terr <- establish_territories(
    agents = agents,
    pix_rast = pix_rast,
    habitat_dt = habitat_dt
  )
  agents <- terr$agents
  dbg("     territories done | mean size:", round(mean(agents$size_achieved, na.rm = TRUE)))
  
  # Survival
  dbg("  -> survival")
  s <- survive_fishers(
    agents = agents,
    dispersers = dispersers,
    survival_rate_table = survival_rate_table
  )
  agents <- s$agents
  dispersers <- s$dispersers
  dbg("     survival complete | agents:", nrow(agents))
  
  # Reproduction
  dbg("  -> reproduction")
  
  r <- reproduce_fishers(
    agents = agents,
    repro_rate_table = repro_rate_table,
    next_id = next_id
  )
  next_id <- r$next_id
  
  dbg("     new dispersers:", nrow(r$dispersers))
  dispersers <- rbind(dispersers, r$dispersers, fill = TRUE)
  
  # Dispersal
  dbg("  -> dispersal")
  dispersers <- disperse_fishers(
    dispersers,
    pix_rast,
    habitat_dt
  )
  dbg("     dispersers after movement:", nrow(dispersers))
  
  # Settlement
  dbg("  -> settlement")
  s2 <- settle_dispersers(
    dispersers,
    agents,
    pix_rast,
    habitat_dt,
    female_hr_table
  )
  agents <- s2$agents
  dispersers <- s2$dispersers
  dbg("     post-settlement | agents:", nrow(agents), "| dispersers:", nrow(dispersers))
  
  # Aging
  dbg("  -> aging")
  a <- age_fishers(agents, dispersers)
  
  dbg("YEAR STEP END", yr, "| agents:", nrow(a$agents), "| dispersers:", nrow(a$dispersers))
  
  list(
    agents = a$agents,
    dispersers = a$dispersers,
    territories = terr$territories,
    next_id = next_id     # ✅ ADD THIS
  )
  
}


# ============================================================
# Main model runner
# ============================================================

run_flex_model <- function(
  pix_rast,
  habitat_dt,
  female_hr_table,
  survival_rate_table,
  repro_rate_table,
  burn_in = 5,
  project_years = 0
)
 {
  

  ## --- Initialize population ---
  agents <- initialize_fishers(
    habitat_dt = habitat_dt,
    pix_rast = pix_rast,
    female_hr_table = female_hr_table
  )
  
  dispersers <- data.table()
  
  next_id <- max(agents$individual_id)
  
  # --- Burn-in ---
  for (yr in seq_len(burn_in)) {
    
    dbg("---- Burn-in year", yr)
    
    step <- step_flex_year(
      agents = agents,
      dispersers = dispersers,
      pix_rast = pix_rast,
      habitat_dt = habitat_dt,
      survival_rate_table = survival_rate_table,
      repro_rate_table = repro_rate_table,
      female_hr_table = female_hr_table,
      next_id = next_id,
      yr = paste0("burn_", yr)
    )
    
    agents <- step$agents
    dispersers <- step$dispersers
    
    
    next_id <- step$next_id
    
    if (!is.null(step$extinct) && step$extinct) {
      dbg("Stopping burn-in: population extinct")
      break
    }
    
  }
  
  
  # --- Projection ---
  report <- list()
  
  if (project_years > 0) {
    for (yr in seq_len(project_years)) {
      
      dbg("---- Projection year", yr)
      
      step <- step_flex_year(
        agents = agents,
        dispersers = dispersers,
        pix_rast = pix_rast,
        habitat_dt = habitat_dt,
        survival_rate_table = survival_rate_table,
        repro_rate_table = repro_rate_table,
        female_hr_table = female_hr_table,
        next_id = next_id,
        yr = yr
      )
      
      agents <- step$agents
      dispersers <- step$dispersers
      
      report[[yr]] <- data.table(
        year = yr,
        n_agents = nrow(agents)
      )
      
      if (!is.null(step$extinct) && step$extinct) {
        dbg("Stopping projection: population extinct at year", yr)
        break
      }
    }
  }
  
  
  list(
    agents = agents,
    dispersers = dispersers,
    report = rbindlist(report, fill = TRUE)
  )
}