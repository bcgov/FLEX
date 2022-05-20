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

###--- SURVIVE
# have the fisher survive one time step depending on their age and cohort
# use the survival function output from Eric Lofroth's latest survival analysis
# adjust the columbian population to no trapping mortalities (as per Rory Fogarty pers comm)
# the wide confidence intervals are unrealistically wide and run into issues of population crash
# adjusted to mean +/- SE as per Rory Fogarty analysis
# cohorts are broken down by population (Boreal / Columbian), sex (M/F), and ageclass (A/J)
# create a function that runs each year to determine the probability of a fisher surviving to the next year
# also need to kill off any fishers that are over the max age for females (default = 9)

survive_FEMALE <- function(fishers, 
                           surv_estimates,
                           Fpop,
                           maxAgeFemale){
  
  survFishers <- of(agents = fishers, var = c("who","breed","disperse","age")) # "who" of the fishers at start of survival
  survFishers$Cohort <- toupper(paste0(rep(Fpop,times=nrow(survFishers)),rep("F",times=nrow(survFishers)),survFishers$sex,substr(survFishers$breed,1,1)))
  
  survFishers <- as.data.frame(left_join(survFishers,surv_estimates,by=c("Cohort")))
  
  survFishers[is.na(survFishers)] <- 0
  survFishers$live <- NA
  
  for(i in 1:nrow(survFishers)){
    if(survFishers[i,]$age!=0){ # can't kill off juveniles that haven't reached 6 months
      survFishers[i,]$live <- rbinom(n=1, size=1, prob=survFishers[i,]$SurvLSE:survFishers[i,]$SurvHSE)
    }
  }
  
  dieWho <- survFishers %>% filter(live==0) # "who" of fishers which die, based on probability
  oldF <- survFishers %>% filter(age>maxAgeFemale) # "who" of female fishers who die of 'old age' (i.e., > 9 yrs)
  dispersing <- survFishers %>% filter(disperse=="D" & age>2) # "who" of dispersing fishers over 2
  
  fishers <- die(fishers, who=c(dieWho$who, oldF$who, dispersing$who))
  return(fishers)
}