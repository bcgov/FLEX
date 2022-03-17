## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "FLEX",
  description = "The FLEX tool will use BC government warehouse publicly available layers to build the landbase and empirical and expert data to specify 'suitable' fisher habitat (i.e., the relative probability of occupancy) as per the BC Fisher Habitat Working Group habitat retention tools guidance (https://www.bcfisherhabitat.ca/habitat-tools/) and the Weir and Corbould (2010) predictive variable for 'openness'. This input layer may change but regardless of the underlying data, the important piece is that whatever data is used will produce a single 'suitable' value for the female fisher territory sized cell to be used in the fisher population model (i.e., Individual Based Model; IBM). The initial simulations used a binary value to differentiate suitable (1) from unsuitable (0) habitat. Once an actual landbase is connected to the IBM, this will be a threshold value, written as an argument function, with the ability of the user to specify. For the first beta version of the R shiny app, the assumption is that the habitat quality is static once the tool starts (i.e., not dynamically changing during the scenarios) while the fisher population will be predicted for 20 years.",
  keywords = c("Fisher", "planning tool", "landscape simulation", "agent based model"),
  authors = structure(list(list(given = "Tati", family = "Micheletti", 
                                role = "aut", email = "tati.micheletti@gmail.com", 
                                comment = NULL),
                           list(given = "Joanna", family = "Burgar", 
                                role = c("aut", "cre"), email = "Joanna.Burgar@gov.bc.ca", 
                                comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(FLEX = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "FLEX.Rmd"), ## same file
  reqdPkgs = list("SpaDES.core (>=1.0.10)", "ggplot2", "NetLogoR",
                  "magrittr", "raster", "dplyr", "Cairo", "stringr",
                  "tidyr", "data.table", "qs","PNWColors", "sf"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "logical", TRUE, NA, NA,
                    "Should the simulation save output plots?"),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter("iterations", "numeric", 100, NA, NA,
                    "How many iterations or replicates should be run?"),
    defineParameter("yrs.to.run", "numeric", 10, NA, NA,
                    "How many years should the simulation run for?"),
    defineParameter("nFemales", "numeric", 10, NA, NA,
                    "What is the initial number of femlaes to be used?"),
    defineParameter("maxAgeFemale", "numeric", 9, NA, NA,
                    "What is the maximum age a female can have?"),
    defineParameter("dist_mov", "numeric", 1.0, NA, NA,
                    "Distance of movement across landscape per time step"),
    defineParameter("sim_order", "numeric", 2, NA, NA,
                    ""),
    defineParameter("TS", "numeric", 12, NA, NA,
                    ""),
    defineParameter("name_out", "character", "Pex2", NA, NA,
                    "")
    ),
  inputObjects = bindrows( #TODO: JB to complete
    expectsInput(objectName = "repro.CI", objectClass = "data.table", 
                 desc = paste0("Table with the following hearders: ",
                               "Param: mean, sd, L95CI, U95CI",
                               "dr: XXXX",
                               "ls: XXXX", 
                               "Pop: Population the data belongs to",
                               " This table is the reproduction table for Fisher",
                               " published in XXXXX (20XX)"),  
                 sourceURL = NA), #TODO: Eventually it would be good to have these files in the cloud (i.e., GDrive)
    expectsInput(objectName = "rf_surv_estimates", objectClass = "data.table", 
                 desc = paste0("Table with the following hearders: ",
                               "Surv: mean female survival (0-1)",
                               "L95CI: lower confidence interval for survival (0-1)",
                               "U95CI: upper confidence interval for survival (0-1)",
                               "Cohort: Which cohort does the data belong to? ",
                               "(Uppercase letters)",
                               "Taken from Rory's updated survival, trapping",
                               " mortality excluded"), 
                 sourceURL = NA),
    expectsInput(objectName = "IBM_aoi", objectClass = "list", 
                 desc = paste0("Named list containing two objects: aoi, raoi",
                               "aoi: sf multipolygon indicating population, ",
                               "grid and habitat",
                               "raoi: raster version of aoi"),  
                 sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "Fpop", objectClass = "character", 
                  desc = "Describes which population the simulation is running for"),
    createsOutput(objectName = "w1", objectClass = "", 
                  desc = ""),
    createsOutput(objectName = "B.w1_real.FEMALE", objectClass = "list", 
                  desc = "")
  )
))

## event types

doEvent.FLEX = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
      sim$w1 <- set_up_REAL_world_FEMALE(nFemales = P(sim)$nFemales,
                                         maxAgeFemale = P(sim)$maxAgeFemale,
                                         raoi = sim$IBM_aoi$raoi)
      if (P(sim)$.plots) sim$w1
      
      sim$Fpop <- unique(substr(sim$IBM_aoi$aoi$Fpop,1,1))
        
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "FLEX", "runSimulation")
      sim <- scheduleEvent(sim, time(sim), "FLEX", "generateOutputs")
    },
    runSimulation = {
      
      B.w1_real.FEMALE.sim100 <- vector('list', P(sim)$iterations)
      for(i in 1:P(sim)$iterations){
        B.w1_real.FEMALE.sim100[[i]] <- FEMALE_IBM_simulation_same_world(land = sim$w1$land, 
                                                                         t0 = sim$w1$t0,
                                                                         repro_estimates = sim$repro.CI, 
                                                                         Fpop = sim$Fpop,
                                                                         surv_estimates = sim$rf_surv_estimates,
                                                                         maxAgeFemale = P(sim)$maxAgeFemale,
                                                                         dist_mov = P(sim)$dist_mov,
                                                                         yrs.to.run = P(sim)$yrs.to.run)
      }
      
      sim$B.w1_real.FEMALE <- list(sim$w1, 
                                   B.w1_real.FEMALE.sim100)
      
      # Schedule next event
      sim <- scheduleEvent(sim, time(sim) + 1, "FLEX", "runSimulation")
    },
    generateOutputs = {
      sim$B.w1_real <- ABM_fig_1sim(sim_out = sim$B.w1_real.FEMALE, 
                                    numsims = P(sim)$iterations, 
                                    yrs_sim = P(sim)$yrs.to.run, 
                                    Fpop = sim$Fpop)
      
      if (P(sim)$.plots) sim$B.w1_real
      
      sim$B.w1_real_heatmap <- heatmap_output(sim_out = sim$B.w1_real.FEMALE, 
                                              sim_order = P(sim)$sim_order, 
                                              numsims = P(sim)$iterations, 
                                              yrs_sim = P(sim)$yrs.to.run, 
                                              TS = P(sim)$TS, 
                                              name_out = P(sim)$name_out)

      if (P(sim)$.plots){
        
        Cairo(file = file.path(Paths$outputPath, "IBM_MeanSE_Pex2.PNG"),
              type = "png", width = 3000, height = 2200, 
              pointsize = 15, bg = "white", dpi = 300)
        sim$B.w1_real$sim.TS.plot_se
        dev.off()
        
        # plot of initial starting points for adult female fishers
        Cairo(file = file.path(Paths$outputPath, "IBM_Saoi_Pex2.PNG"),
              type = "png", width = 3000, height = 2200, pointsize = 15,
              bg = "white", dpi = 300)
        raster::plot(sim$B.w1_real.FEMALE[[1]]$land, 
             legend = FALSE, 
             main = "Simulated Fisher Established Territories within Area of Interest")
        points(sim$B.w1_real.FEMALE[[1]]$t0, 
               pch = sim$B.w1_real.FEMALE[[1]]$t0$shape, 
               col = of(agents = sim$B.w1_real.FEMALE[[1]]$t0, 
                        var = "color"))
        dev.off()
        
        raster::plot(sim$B.w1_real_heatmap$raster)
        
      }
      
      # Schedule next event
      sim <- scheduleEvent(sim, time(sim) + 1, "FLEX", "generateOutputs")
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }
  
  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  if (!suppliedElsewhere(object = "repro.CI", sim = sim)){
    sim$repro.CI <- fread(file.path(Paths[["modulePath"]], 
                                    currentModule(sim), 
                                    "data/repro.CI.csv"))
  }
  
  if (!suppliedElsewhere(object = "IBM_aoi", sim = sim)){
    sim$IBM_aoi <- qread(file.path(Paths[["modulePath"]],
                                     currentModule(sim), 
                                     "data/IBM_aoi_Pex2.qs"))
  }
  
  if (!suppliedElsewhere(object = "rf_surv_estimates", sim = sim)){
    sim$rf_surv_estimates <- fread(file.path(Paths[["modulePath"]],
                                               currentModule(sim), 
                                               "data/rf_surv_estimates.csv"))
    }
  
  return(invisible(sim))
}
