# create function to loop through functions, allow sub-function specification
# now that the function is using the cohort survival data, have the survival run on an annual basis, not per time step
FEMALE_IBM_simulation_same_world <- function(land, 
                                             fishers,               # import world
                                             rMove,
                                             repro_estimates, 
                                             Fpop,             # reproduction
                                             surv_estimates,   # survive
                                             maxAgeFemale,     # survive
                                             dist_mov,         # disperse
                                             out = TRUE,       # disperse
                                             torus = TRUE){     # disperse
                                             # yrs.to.run = 1){ # number of years to run simulations ()

  tApr <- fishers
  
  # tApril	April	Establish / maintain territory & reproduce & scent territory
  # 	Females ≥ 0.5 years without established territories disperse (up to 30 FETAs / 6 months of movement)
  # 	Females > 2 years give birth and with established territories give birth; number of kits born conditional on denning rate and litter size
  # 	Age 0.5 years
  
  if(NLcount(tApr)!=0){
    
    tApr <- repro_FEMALE(fishers=tApr, repro_estimates=repro_estimates, Fpop=Fpop)
    
    for(i in 1:30){
      tApr <- disperse_FEMALE(land=land, rMove=rMove, fishers=tApr, dist_mov=dist_mov, out=out, torus=torus)
    }
    
    # tmp <- patchHere(land, tApr[tApr$disperse=="E"])
    # duplicated(tmp)
    
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
      tOct <- disperse_FEMALE(land=land, rMove=rMove, fishers=tOct, dist_mov=dist_mov, out=out, torus=torus)
    }
    
    age.val <- of(agents=tOct, var=c("age"))+0.5
    tOct <- NLset(turtles = tOct, agents=turtle(tOct, who=tOct$who),var="age", val=age.val)
    
    breed.val <- as.data.frame(of(agents=tOct, var=c("breed","age")))
    breed.val$breed <- case_when(breed.val$age>2 ~ "adult",
                                 TRUE ~ as.character(breed.val$breed))
    
    tOct <- NLset(turtles = tOct, agents=turtle(tOct, who=tOct$who),var="breed", val=breed.val$breed)
    

    # survive_FEMALE(fishers=fishers, surv_estimates=surv_estimates, Fpop="B", maxAgeFemale=9)
    tOct <- survive_FEMALE(fishers=tOct, surv_estimates=surv_estimates, Fpop=Fpop, maxAgeFemale=maxAgeFemale)
    
    }
    
  fishers <- tOct
  
  return(fishers)
}


# rMove <- IBM_aoi$r_dynamic[[(dim(IBM_aoi$r_dynamic)[3]/2+1):(dim(IBM_aoi$r_dynamic)[3])]]
# fishers <- FEMALE_IBM_simulation_same_world(land=land ,rMove=rMove[[1]], fishers=fishers, repro_estimates = repro_estimates, Fpop=Fpop,
#                                             surv_estimates = surv_estimates, maxAgeFemale = 9, dist_mov = 1.0, out = TRUE, torus = TRUE)
# NLcount(fishers)
# 
# plot(land)
# points(fishers, pch = fishers$shape, col = "black")
