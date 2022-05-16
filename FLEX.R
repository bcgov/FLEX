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
  version = list(FLEX = "0.0.1.0"),
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
    # defineParameter("iterations", "numeric", 100, NA, NA,               # hard coding it in for 100 simulations - makes it easier for output
    #                 "How many iterations or replicates should be run?"),
    defineParameter("yrs.to.run", "numeric", 10, NA, NA,
                    "How many years should the simulation run for?"),
    defineParameter("propFemales", "numeric", 0.3, NA, NA,
                    "What is the initial proportion of femlaes to suitable FETAs to start?"),
    defineParameter("maxAgeFemale", "numeric", 9, NA, NA,
                    "What is the maximum age a female can have?"),
    # defineParameter("dist_mov", "numeric", 1.0, NA, NA,
    #                 "Distance of movement across landscape per time step"),
    defineParameter("sim_order", "numeric", 2, NA, NA,
                    ""),
    defineParameter("TS", "numeric", 10, NA, NA,
                    "Year (timestep) to use when creating heatmap"),
    defineParameter("name_out", "character", "Cariboo", NA, NA,
                    "")
    ),
  inputObjects = bindrows( #TODO: JB to complete
    expectsInput(objectName = "repro_estimates", objectClass = "data.table", 
                 desc = paste0("Table with the following hearders: ",
                               "Param: mean, sd, L95CI, U95CI",
                               "dr: XXXX",
                               "ls: XXXX", 
                               "Pop: Population the data belongs to",
                               " This table is the reproduction table for Fisher",
                               " published in XXXXX (20XX)"),  
                 sourceURL = NA), #TODO: Eventually it would be good to have these files in the cloud (i.e., GDrive)
    expectsInput(objectName = "surv_estimates", objectClass = "data.table", 
                 desc = paste0("Table with the following hearders: ",
                               "Surv: mean female survival (0-1)",
                               "L95CI: lower confidence interval for survival (0-1)",
                               "U95CI: upper confidence interval for survival (0-1)",
                               "Cohort: Which cohort does the data belong to? ",
                               "(Uppercase letters)",
                               "Taken from Rory's updated survival, trapping",
                               " mortality excluded"), 
                 sourceURL = NA),
    expectsInput(objectName = "mahal_metric", objectClass = "data.table", 
                 desc = paste0("Table with the following hearders: ",
                               "FHE_zone: Fisher Habitat Extension Zone Name",
                               "FHE_zone_num: numeric value for each zone (1= Boreal, 2=Sub-Boreal moist, 3=Sub-Boreal dry, 4=Dry Forest",
                               "Mean: XXXX",
                               "SD: XXXX",
                               "Max: XXXX",
                               "Taken from Rich Weir's Mahalanobis distance analysis"), 
                 sourceURL = NA),
    expectsInput(objectName = "IBM_aoi", objectClass = "list", 
                 desc = paste0("list containing two raster stacks: dynamic, static",
                               "dynamic (raster stacks): Mahalanobis distance values; movement values",
                               "raster stacks are clusObjects, updated every  5 years",
                               "static: Fisher population (1=Boreal, 2=Columbian); Fisher Habitat Zone (1:4, as above)"),  
                 sourceURL = NA),
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "num.land.updates", objectClass = "numeric", 
                  desc = "The number of times the underlying land needs to update (yrs.to.run / clus_years"),
    createsOutput(objectName = "Fpop", objectClass = "character", 
                  desc = "Describes which population the simulation is running for"),
    createsOutput(objectName = "fishers", objectClass = "agentMatrix object", 
                  desc = "Describes the fishers (agents) on the land"),
    createsOutput(objectName = "fishers_index", objectClass = "list", 
                  desc = "A list of length iterations describing the fishers (agents) on the land"),
    createsOutput(objectName = "Mahal_land", objectClass = "list", 
                  desc = " list of worldMatrix objects that describe the underlying landscape, 0=unsuitable habitat, 1=suitable FETA"),
    createsOutput(objectName = "FEMALE_IBM_dynamic", objectClass = "list", 
                  desc = "list of worldArray objects that describe the female fisher population using updated landscapes")
  )
))

## event types

doEvent.FLEX = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
      
      sim$num.land.updates <- P(sim)$yrs.to.run / P(sim)$clus_yrs # clus obj updates every 5 years (default)
      
      # create underlying landscape for each round of land (i.e., clus objects) updates
      sim$Mahal_land <- vector('list', sim$num.land.updates)
      
      for(i in 1:sim$num.land.updates){ # the number of Mahalanobis land layers from the clus obj
        
        sim$Mahal_land[[i]] <- create_MAHAL_land(rFHzone = sim$IBM_aoi$r_static$layer.2,
                                      rMahal = sim$IBM_aoi$r_dynamic[[i]],
                                      mahal_metric = sim$mahal_metric,
                                      D2_param = P(sim)$D2_param)
      }
      
      
      # Mahal_land <- vector('list', 2)
      # 
      # for(i in 1:2){ # the number of Mahalanobis land layers from the clus obj
      # 
      #   Mahal_land[[i]] <- create_MAHAL_land(rFHzone = IBM_aoi$r_static$layer.2,
      #                                            rMahal = IBM_aoi$r_dynamic[[i]],
      #                                            mahal_metric = fread(file.path(paste0(getwd(),"/modules/FLEX/"),"data/mahal_metric.csv")),
      #                                            D2_param = c("Max","SD"))
      # }
      
      
      # create fishers for start of simulation
      sim$Fpop <- extract_Fpop(rFpop=sim$IBM_aoi$r_static$layer.1)
      
      sim$fishers <- set_up_REAL_world_FEMALE(propFemales = P(sim)$propFemales,
                                         maxAgeFemale = P(sim)$maxAgeFemale,
                                         land = sim$Mahal_land[[1]],
                                         Fpop=sim$Fpop,
                                         repro_estimates = sim$repro_estimates)
      
      sim$fishers_index <- vector('list', P(sim)$iterations)
      
      # duplicate to have the same starting point for all iterations
      for(i in 1:P(sim)$iterations){
      sim$fishers_index[[i]]<- sim$fishers
      }
      
      # fishers <- set_up_REAL_world_FEMALE(propFemales = 0.3,
      #                                     maxAgeFemale = 9,
      #                                     land = Mahal_land[[1]],
      #                                     Fpop="C",
      #                                     repro_estimates = fread(file.path(paste0(getwd(),"/modules/FLEX/"),"data/repro_CI.csv")))
      # 
      
      if (P(sim)$.plots) sim$land # not sure what this is for...
      
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "FLEX", "dynamicSimulation")
      sim <- scheduleEvent(sim, time(sim), "FLEX", "generateOutputs")
    },
   
    dynamicSimulation = {
      
      # land, rMove and fishers will all need to be indexed to update as per yrs.to.run argument (yrs.to.run/clus_yrs = times to run sim)
      # sim$num.land.updates <- P(sim)$yrs.to.run / P(sim)$clus_yrs # clus obj updates every 5 years (default)
      
      sim$FEMALE_IBM_dynamic <- vector('list', sim$num.land.updates)
      
      for(dy in 1:sim$num.land.updates){
        for(i in 1:P(sim)$iterations){
          sim$FEMALE_IBM_dynamic[[dy]][[i]] <- FEMALE_IBM_simulation_same_world(land=sim$Mahal_land[[dy]], 
                                                                                rMove=sim$IBM_aoi$r_dynamic[[(dim(sim$IBM_aoi$r_dynamic)[3]/2+1):(dim(sim$IBM_aoi$r_dynamic)[3])]][[dy]],
                                                                                fishers=sim$fishers_index[[i]],
                                                                                repro_estimates=sim$repro_estimates,
                                                                                Fpop=sim$Fpop,
                                                                                surv_estimates=sim$surv_estimates,
                                                                                maxAgeFemale=P(sim)$maxAgeFemale,
                                                                                clus_yrs=P(sim)$clus_yrs)
          
        }
      }
      
      # Schedule next event
      sim <- scheduleEvent(sim, time(sim) + 1, "FLEX", "dynamicSimulation")
    },
    
    generateOutputs = {
      
      # will need to add the two simulations together...
      sim$FEMALE_IBM_initial
      
      sim$FEMALE_output <- ABM_fig_1sim(sim_out = sim$EX_real.FEMALE, 
                                    numsims = P(sim)$iterations, 
                                    yrs_sim = P(sim)$yrs.to.run, 
                                    Fpop = sim$Fpop)
      
      if (P(sim)$.plots) sim$FEMALE_output
      
      sim$EX_real_heatmap <- heatmap_output(sim_out = sim$FEMALE_output, 
                                              sim_order = P(sim)$sim_order, 
                                              numsims = P(sim)$iterations, 
                                              yrs_sim = P(sim)$yrs.to.run, 
                                              TS = P(sim)$TS,
                                              rextent = sim$rFpop,
                                              name_out = P(sim)$name_out)

      if (P(sim)$.plots){
        
        Cairo(file = file.path(Paths$outputPath, "IBM_MeanSE.PNG"),
              type = "png", width = 3000, height = 2200, 
              pointsize = 15, bg = "white", dpi = 300)
        sim$EX_real$sim.TS.plot_se
        dev.off()
        
        # plot of initial starting points for adult female fishers
        Cairo(file = file.path(Paths$outputPath, "IBM_Saoi.PNG"),
              type = "png", width = 3000, height = 2200, pointsize = 15,
              bg = "white", dpi = 300)
        raster::plot(sim$EX_real.FEMALE[[1]]$land, 
             legend = FALSE, 
             main = "Simulated Fisher Established Territories within Area of Interest")
        points(sim$EX_real.FEMALE[[1]]$t0, 
               pch = sim$EX_real.FEMALE[[1]]$t0$shape, 
               col = of(agents = sim$EX_real.FEMALE[[1]]$t0, 
                        var = "color"))
        dev.off()
        
        raster::plot(sim$EX_real_heatmap$raster)
        
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
  
  if (!suppliedElsewhere(object = "repro_estimates", sim = sim)){
    sim$repro_estimates <- fread(file.path(Paths[["modulePath"]], 
                                    currentModule(sim), 
                                    "data/repro_CI.csv"))
  }
  
  if (!suppliedElsewhere(object = "mahal_metric", sim = sim)){
    sim$mahal_metric <- fread(file.path(Paths[["modulePath"]],
                                   currentModule(sim), 
                                   "data/mahal_metric.csv"))
  }
  
  if (!suppliedElsewhere(object = "IBM_aoi", sim = sim)){
    sim$IBM_aoi <- qread(file.path(Paths[["modulePath"]],
                                     currentModule(sim), 
                                     "data/EX_Cariboo_IBM_aoi.qs"))
  }
  
  if (!suppliedElsewhere(object = "surv_estimates", sim = sim)){
    sim$surv_estimates <- fread(file.path(Paths[["modulePath"]],
                                               currentModule(sim), 
                                               "data/surv_estimates.csv"))
    }
  
  return(invisible(sim))
}
