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

# create function to loop through functions, allow sub-function specification
# now that the function is using the cohort survival data, have the survival run on an annual basis, not per time step
FEMALE_IBM_simulation_same_world <- function(land,
                                             fishers,               # import world
                                             rMove,
                                             repro_estimates,
                                             Fpop,           
                                             surv_estimates, 
                                             clus_yrs,
                                             # yrs.to.run,
                                             maxAgeFemale){ # run for as many years between updated clus objects (default is 5)

  # deals with inconsistencies between potential fisher inputs
  if(class(fishers)=="list"){
    last.iteration <- length(fishers)
    tApr <- fishers[[last.iteration]]
  } else {tApr <- fishers}

  FEMALE_same_world <- vector('list', clus_yrs)
  
  for(tcount in 1:clus_yrs){
    # tApril	April	Establish / maintain territory & reproduce & scent territory
    # 	Females ≥ 0.5 years without established territories disperse (up to 30 FETAs / 6 months of movement)
    # 	Females > 2 years give birth and with established territories give birth; number of kits born conditional on denning rate and litter size
    # 	Age 0.5 years
    if(NLcount(tApr)!=0){
      
      tApr <- repro_FEMALE(fishers=tApr, repro_estimates=repro_estimates, Fpop=Fpop)
      
      for(i in 1:30){
        tApr <- disperse_FEMALE(land=land, rMove=rMove, fishers=tApr)
      }
      
     age.val <- of(agents=tApr, var=c("age"))+0.5
      tApr <- NLset(turtles = tApr, agents=turtle(tApr, who=tApr$who),var="age", val=age.val)
      
      # plot(land)
      # points(tApr, pch = tApr$shape, col = of(agents = tApr, var = "color"))
    }
    
    tOct <- tApr
    
    # tOctober	October	Establish / maintain territory & survive
    # 	Females ≥ 0.5 years without established territories disperse
    # 	Fishers subject to mortality; number who die is conditional on survival estimates, maximum lifespan (for adults and dispersing juveniles)
    # 	Age 0.5 years
    
    if(NLcount(tOct)!=0){
      
      for(i in 1:30){
        tOct <- disperse_FEMALE(land=land, rMove=rMove, fishers=tOct)
      }
      
      age.val <- of(agents=tOct, var=c("age"))+0.5
      tOct <- NLset(turtles = tOct, agents=turtle(tOct, who=tOct$who),var="age", val=age.val)
      
      breed.val <- as.data.frame(of(agents=tOct, var=c("breed","age")))
      breed.val$breed <- case_when(breed.val$age>2 ~ "adult",
                                   TRUE ~ as.character(breed.val$breed))
      
      tOct <- NLset(turtles = tOct, agents=turtle(tOct, who=tOct$who),var="breed", val=breed.val$breed)
      
      tOct <- survive_FEMALE(fishers=tOct, surv_estimates=surv_estimates, Fpop=Fpop, maxAgeFemale=maxAgeFemale)
      
    }
    
    print(NLcount(tOct))
    FEMALE_same_world[[tcount]] <- tOct

    tApr <- tOct
    }
  
  return(FEMALE_same_world=FEMALE_same_world)
}


# rMove <- IBM_aoi$r_dynamic[[(dim(IBM_aoi$r_dynamic)[3]/2+1):(dim(IBM_aoi$r_dynamic)[3])]]
# fishers <- FEMALE_IBM_simulation_same_world(land=land ,rMove=rMove, fishers=fishers, repro_estimates = repro_estimates, Fpop=Fpop,
#                                             surv_estimates = surv_estimates, maxAgeFemale = 9,clus_yrs=5)
