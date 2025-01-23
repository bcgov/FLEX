###--- SURVIVE
# Have the fisher survive one time step depending on their age and cohort
# Use the survival function output from Eric's latest survival analysis
# Cohorts are broken down by population (Boreal / Central Interior), sex (M/F), and ageclass (A/J)
# create a function that runs each time step to determine the probability of a fisher surviving to the next time step
# also need to kill off any fishers that are over 8 years (female) and 4 years (male)
# *** UPDATE - not enough fishers were surviving when using age survival probabilities, changed to cohort level probabilities
# need to consider the revised vital rates from Eric - can't have such huge CIs or will die within a couple generations - how to rectify?

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
    # i=1; rm(i)
    if(survFishers[i,]$age!=0){ # can't kill off juveniles that haven't reached 6 months
      # survFishers[i,]$live <- as.integer(rbernoulli(n=1, p=c(survFishers[i,]$L95CL:survFishers[i,]$U95CL)))
      survFishers[i,]$live <- rbinom(n=1, size=1, prob=survFishers[i,]$SurvLSE:survFishers[i,]$SurvHSE)
    }
  }
  
  dieWho <- survFishers %>% filter(live==0) # "who" of fishers which die, based on probability
  oldF <- survFishers %>% filter(age>maxAgeFemale) # "who" of female fishers who die of 'old age' (i.e., > 8 yrs)
  dispersing <- survFishers %>% filter(disperse=="D" & age>2) # "who" of dispersing fishers over 2
  
  fishers <- die(fishers, who=c(dieWho$who, oldF$who, dispersing$who))
  return(fishers)
}