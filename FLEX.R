# Copyright 2021 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
#===========================================================================================#
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.
#===========================================================================================#

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
                  "tidyr", "data.table", "qs","PNWColors", "sf","foreach","doParallel"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "logical", TRUE, NA, NA,
                    "Should the simulation save output plots?"),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter("simulations", "numeric", 5, NA, NA,               # keep small for testing, update to 100 simulations when functional
                    "How many simulations or replicates should be run?"),
    # defineParameter("yrs.to.run", "numeric", 10, NA, NA,
    #                 "How many years should the simulation run for?"),
    defineParameter("propFemales", "numeric", 0.3, NA, NA,
                    "What is the initial proportion of femlaes to suitable FETAs to start?"),
    defineParameter("maxAgeFemale", "numeric", 9, NA, NA,
                    "What is the maximum age a female can have?"),
    defineParameter("calculateInterval", "numeric", 1, NA, NA,
                    "What is the interval to run each dynamic simulation?"),
    defineParameter("D2_param", "character", "Max", NA, NA,
                    "Which Mahalanobis distance metric (Max, Mean, SD, or combination) to use as the threshold for suitable habitat?")
    ),
  inputObjects = bindrows( 
    expectsInput(objectName = "repro_estimates", objectClass = "data.table", 
                 desc = paste0("Table with the following hearders: ",
                               "Param: mean, sd, L95CI, U95CI",
                               "dr: 0.75, 0.39, 0.58, 0.92 [B]; 0.54, 0.41, 0.40, 0.68 [C]",
                               "ls: 2.6, 0.70, 2.25, 2.95 [B]; 1.7, 0.73, 1.28, 2.12 [C]", 
                               "Pop: Population the data belongs to",
                               "Taken from Lofroth (2022) vital rates paper"),  
                 sourceURL = NA),
    expectsInput(objectName = "surv_estimates", objectClass = "data.table", 
                 desc = paste0("Table with the following hearders: ",
                               "Surv: mean female survival (0-1); not explicitly used in current module",
                               "L95CI: lower confidence interval for survival (0-1); not used in current module",
                               "U95CI: upper confidence interval for survival (0-1); not used in current module",
                               "SurvLSE: Surv - Standard Error; used in current module",
                               "SurvHSE: Surv + Standard Erorr; used in current module",
                               "Cohort: Which cohort (uppercase letters) does the data belong to?",
                               "Taken from Lofroth (2022) vital rates paper, adjusted by Rory Fogarty to exclude trapping mortality"), 
                 sourceURL = NA),
    expectsInput(objectName = "mahal_metric", objectClass = "data.table", 
                 desc = paste0("Table with the following hearders: ",
                               "FHE_zone: Fisher Habitat Extension Zone Name",
                               "FHE_zone_num: numeric value for each zone (1= Boreal, 2=Sub-Boreal moist, 3=Sub-Boreal dry, 4=Dry Forest",
                               "Mean: 3.8, 4.4, 4.4, 3.6",
                               "SD: 2.71, 1.09, 2.33, 1.62",
                               "Max: 9.88, 6.01, 6.63, 7.50",
                               "Taken from Rich Weir's Mahalanobis distance analysis"), 
                 sourceURL = NA),
    expectsInput(objectName = "flexRasWorld", objectClass = "list", 
                 desc = paste0("list containing 3 rasters: rFHzone, rMahal, rMove",
                               "rFHzone: Fisher Habitat Zone (1:4, as above); static (used to determine the fisher population: Fpop)",
                               "rMahal (raster stacks): Mahalanobis distance values; updated annually every clus_yrs",
                               "rMove (raster stacks): movement values; updated annually every clus_yrs"),  
                 sourceURL = NA),
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "Fpop", objectClass = "character", 
                  desc = "Describes which population the simulation is running for"),
    createsOutput(objectName = "clus_yrs", objectClass = "numeric", 
                  desc = "The number of annual land updates provided by flexRasWorld"),
    createsOutput(objectName = "fishers", objectClass = "agentMatrix object", 
                  desc = "Describes the fishers (agents) on the land at the start of the simulation"),
    createsOutput(objectName = "fisher_output", objectClass = "list", 
                  desc = "A list of length simulations describing the fishers (agents) on the land at the end of each year"),
    createsOutput(objectName = "FLEX_output", objectClass = "list", 
                  desc = "A list of length clus_yrs containing the list of fisher_outputs"),
    createsOutput(objectName = "Mahal_land", objectClass = "list", 
                  desc = " list of worldMatrix objects that describe the underlying landscape, 0=unsuitable habitat, 1=suitable FETA"),
    createsOutput(objectName = "FLEX_setup", objectClass = "list",
                  desc = paste0("A list containing a raster and summarized info",
                  "r_start = raster of the inital set up of adults female fishers with established territories and remaining suitable FETAs",
                  "pop_info = values for number FETAS of suitable habitat, total habitat, percentage habitat, and number of adult females with established territories"))
  )
))


## event types

doEvent.FLEX = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
      
      
      # create Fpop as character value based on fisher habitat zone (Boreal = Boreal all others = Columbian)
      if(modal(values(sim$flexRasWorld[[1]]), ties='lowest', na.rm=TRUE, freq=FALSE) == 1){
        sim$Fpop <- 'B'
      }else{sim$Fpop <- 'C'}
      
      
      # create the starting world using, using the first layer from flexRasWorld and initial fisher parameters
      sim$fishers <- set_up_REAL_world_FEMALE(propFemales = P(sim)$propFemales,
                                         maxAgeFemale = P(sim)$maxAgeFemale,
                                         rFHzone = sim$flexRasWorld[[1]],
                                         rMahal = sim$flexRasWorld[[2]][[1]],
                                         mahal_metric = sim$mahal_metric,
                                         D2_param = P(sim)$D2_param,
                                         Fpop=sim$Fpop,
                                         repro_estimates = sim$repro_estimates)

      
      
      
      # duplicate to have the same starting point for all simulations
      sim$fisher_output <- vector('list', P(sim)$simulations+1)
      
      for(i in 1:P(sim)$simulations){
        sim$fisher_output[[i]]<- sim$fishers
      }
      
      
      # if (P(sim)$.plots) sim$land # not sure what this is for...
      
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$calculateInterval, "FLEX", "dynamicSimulation")
      # sim <- scheduleEvent(sim, time(sim), "FLEX", "generateOutputs")
      sim <- scheduleEvent(sim, end(sim), "FLEX", "generateOutputs") # wait until the end to generate outputs
      
    },
   
    dynamicSimulation = {
      
      
      # for each simulation
      # 1) start with same base / scenario start
      # 2) run for 100 fisher objects
      # 3) create output as FLEX object
      # 4) have the FLEX object be the same object that can then be exported back to CLUS
      
      sim$clus_yrs <- as.numeric(dim(sim$flexRasWorld[[2]])[3]) # number of years with updated landscape in flexRasWorld
      
      sim$Mahal_land <- vector('list', sim$clus_yrs)
      
      for(i in 1:sim$clus_yrs){
        sim$Mahal_land[[i]] <- create_MAHAL_land(rFHzone = sim$flexRasWorld[[1]],
                                                 rMahal = sim$flexRasWorld[[2]][[i]],
                                                 mahal_metric = sim$mahal_metric,
                                                 D2_param = P(sim)$D2_param)
      }
      
      
      # sim$FLEX_output <- FEMALE_IBM_simulation_same_world(land=sim$Mahal_land[[1]],
      #                                                          rMove=sim$flexRasWorld[[3]][[1]],
      #                                                          fishers=sim$fishers,
      #                                                          repro_estimates=sim$repro_estimates,
      #                                                          Fpop=sim$Fpop,
      #                                                          surv_estimates=sim$surv_estimates,
      #                                                          maxAgeFemale=P(sim)$maxAgeFemale)
      # 
      # 
      
      
      # Run IBM for each iteration of fishers (i.e., num of simulations)
      # For each fisher iteration/simulation run IBM with the landscape changing every year
      
      # bit for parallel processing; run on 2 cores - speed things up a bit
      # cl <- makeCluster(2)
      # registerDoParallel(cl)
      # 
      #  foreach (i = 1:P(sim)$simulations, 
      #           .export=c('FEMALE_IBM_simulation_same_world','repro_FEMALE','disperse_FEMALE','survive_FEMALE'),
      #           .packages=c('NetLogoR','tidyverse')) %dopar% {     # will need to update to %dopar% and add in parameter on how many cores to run
      #     for(t in 1:sim$clus_yrs){
      #       sim$FLEX_output[[i]] <- FEMALE_IBM_simulation_same_world(land=sim$Mahal_land[[t]],
      #                                        rMove=sim$flexRasWorld[[3]][[t]],
      #                                        fishers=sim$FLEX_output[[i]],
      #                                        repro_estimates=sim$repro_estimates,
      #                                        Fpop=sim$Fpop,
      #                                        surv_estimates=sim$surv_estimates,
      #                                        maxAgeFemale=P(sim)$maxAgeFemale)
      #     }
      #   } #end of foreach
      # 
      #  stopCluster(cl)
       
      sim$FLEX_output <- list()
      
          for(i in 1:P(sim)$simulations){
            for(t in 1:sim$clus_yrs){
              sim$fisher_output[[t]] <- FEMALE_IBM_simulation_same_world(land=sim$Mahal_land[[t]],
                                                                        rMove=sim$flexRasWorld[[3]][[t]],
                                                                        fishers=sim$fisher_output[i],
                                                                        repro_estimates=sim$repro_estimates,
                                                                        Fpop=sim$Fpop,
                                                                        surv_estimates=sim$surv_estimates,
                                                                        maxAgeFemale=P(sim)$maxAgeFemale)
              sim$FLEX_output[[i]] <- sim$fisher_output
        }
        }
     

      # Schedule next event
      sim <- scheduleEvent(sim, time(sim) + P(sim)$calculateInterval, "FLEX", "dynamicSimulation")
    },
    
    generateOutputs = {
      
      # generate output of initial world
      sim$FLEX_setup <- setup_raster(land=sim$Mahal_land[[1]],
                                     fishers=sim$fishers)
      
      # will need to add the two simulations together...

      sim$FLEX_agg_output <- sim_output(sim_out = sim$FLEX_output, 
                                      simulations = P(sim)$simulations, 
                                      clus_yrs = sim$clus_yrs)
      
      
      # FLEX_agg_output <- sim_output(sim_out = FLEX_output,
      #                             simulations = 10,
      #                             clus_yrs = 5)

      
      sim$FLEX_heatmap <- heatmap_output(sim_out = sim$FLEX_output,
                                         simulations = P(sim)$simulations,
                                         clus_yrs = sim$clus_yrs,
                                         propFemales = P(sim)$propFemales,
                                         rextent = sim$Mahal_land[[1]])
      
      # FLEX_heatmap <- heatmap_output(sim_out = FLEX_output,
      #                                    simulations = 10,
      #                                    clus_yrs = 5,
      #                                    propFemales = 0.3,
      #                                    rextent = Mahal_land[[1]])

     
      
      
      
      
      if (P(sim)$.plots){
        
        raster::plot(sim$FLEX_heatmap$raster)
        
      }
      
      # Schedule next event
      sim <- scheduleEvent(sim, time(sim) + P(sim)$calculateInterval, "FLEX", "generateOutputs")
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
  
  if (!suppliedElsewhere(object = "flexRasWorld", sim = sim)){
    sim$flexRasWorld <- readRDS(file.path(Paths[["modulePath"]],
                                     currentModule(sim),
                                     "data/flexRasWorld.rds")) # "data/flexRasWorld.rds" when new example comes
  }
  
  if (!suppliedElsewhere(object = "surv_estimates", sim = sim)){
    sim$surv_estimates <- fread(file.path(Paths[["modulePath"]],
                                               currentModule(sim), 
                                               "data/surv_estimates.csv"))
    }
  
  return(invisible(sim))
}
