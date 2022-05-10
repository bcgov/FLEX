# create function to loop through functions, allow sub-function specification
# now that the function is using the cohort survival data, have the survival run on an annual basis, not per time step
FEMALE_IBM_simulation_same_world <- function(land, 
                                             t0,                                # import world
                                             repro_estimates, 
                                             Fpop,       # reproduction
                                             surv_estimates,   # survive
                                             maxAgeFemale,     # survive
                                             dist_mov=1.0,     # disperse
                                             out = TRUE,       # disperse
                                             torus = TRUE,     # disperse
                                             yrs.to.run = 10){ # number of years to run simulations ()
  
  IBM.sim.out <- vector('list', yrs.to.run)
  
  # Fpop=extract_Fpop(r_static=r_list[[2]])
  # class(Fpop) # character
  # mahal_metric <- read.csv("./modules/FLEX/data/mahal_metric.csv")
  # class(mahal_metric) # data.frame
  # land = create_MAHAL_land(r_static=r_list[[2]], r_dynamic=r_list[[1]], mahal_metric=mahal_metric, D2_param="Max")
  # class(land) #worldMatrix object 
  # land = land
  # t0 = w1$t0
  # repro_estimates = read.csv("./modules/FLEX/data/repro.CI.csv")
  # Fpop = substr(Fpop,1,1)
  
  # t0	April	Kits are born
  # 	Number of kits born conditional on denning rate and litter size
  # print(NLcount(t0))
  
  t0 <- repro_FEMALE(fishers=t0, repro_estimates=repro_estimates, Fpop=Fpop)
  
  # t1	October	Kits are kicked out of natal territory
  # 	Age 1 year (t0 and t1)
  
  age.val <- of(agents=t0, var=c("age"))+1
  t1 <- NLset(turtles = t0, agents=turtle(t0, who=t0$who),var="age", val=age.val)
  
  # plot(land)
  # points(t1, pch = t1$shape, col = of(agents = t1, var = "color"))
  
  print(NLcount(t1))
  IBM.sim.out[[1]] <- t1 # time step ends at April; gives the number of fishers at the start
  
  # tmp <- patchHere(land, t1[t1$disperse=="E"])
  # duplicated(tmp)
  
  tApr <- t1
  
  for(tcount in 2:(yrs.to.run+1)){
    
    # tApril	April	Establish / maintain territory & reproduce & scent territory
    # 	Females ≥ 0.5 years without established territories disperse (up to 30 FETAs / 6 months of movement)
    # 	Females > 2 years give birth and with established territories give birth; number of kits born conditional on denning rate and litter size
    # 	Age 0.5 years
    
    if(NLcount(tApr)!=0){
      
      tApr <- repro_FEMALE(fishers=tApr, repro_estimates=repro_estimates, Fpop=Fpop)
      
      for(i in 1:30){
        tApr <- disperse_FEMALE(land=land, fishers=tApr, dist_mov=dist_mov, out=out, torus=torus)
      }
      
      # tmp <- patchHere(land, tApr[tApr$disperse=="E"])
      # duplicated(tmp)
      
      age.val <- of(agents=tApr, var=c("age"))+0.5
      tApr <- NLset(turtles = tApr, agents=turtle(tApr, who=tApr$who),var="age", val=age.val)
      
      # plot(land)
      # points(tApr, pch = tApr$shape, col = of(agents = tApr, var = "color"))
    }
    
    # tOctober	October	Establish / maintain territory & survive
    # 	Females ≥ 0.5 years without established territories disperse
    # 	Fishers subject to mortality; number who die is conditional on survival estimates, maximum lifespan (for adults and dispersing juveniles)
    # 	Age 0.5 years
    tOct <- tApr
    
    if(NLcount(tOct)!=0){
      
      for(i in 1:30){
        tOct <- disperse_FEMALE(land=land, fishers=tOct, dist_mov=dist_mov, out=out, torus=torus)
      }
      
      # tmp <- patchHere(land, tOct)
      # duplicated(tmp)
      
      age.val <- of(agents=tOct, var=c("age"))+0.5
      tOct <- NLset(turtles = tOct, agents=turtle(tOct, who=tOct$who),var="age", val=age.val)
      
      breed.val <- as.data.frame(of(agents=tOct, var=c("breed","age")))
      breed.val$breed <- case_when(breed.val$age>2 ~ "adult",
                                   TRUE ~ as.character(breed.val$breed))
      
      tOct <- NLset(turtles = tOct, agents=turtle(tOct, who=tOct$who),var="breed", val=breed.val$breed)
      
      # surv_estimates = read.csv("./modules/FLEX/data/surv_estimates.csv")
      
      tOct <- survive_FEMALE(fishers=tOct, surv_estimates=surv_estimates, Fpop=Fpop, maxAgeFemale=maxAgeFemale)
      
      IBM.sim.out[[tcount]] <- tOct
      # plot(land)
      # points(tOct, pch = tOct$shape, col = of(agents = tOct, var = "color"))
      
    } else {
      IBM.sim.out[[tcount]] <- 0 }
    
    print(NLcount(tOct))
    tApr <- tOct
  }
  return(IBM.sim.out)
}


