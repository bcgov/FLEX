# create function to loop through functions, allow sub-function specification
# now that the function is using the cohort survival data, have the survival run on an annual basis, not per time step
FEMALE_IBM_simulation_same_world <- function(land, 
                                             t0,                                # import world
                                             repro_estimates, 
                                             Fpop = "C",       # reproduction
                                             surv_estimates,   # survive
                                             maxAgeFemale,     # survive
                                             dist_mov=1.0,     # disperse
                                             out = TRUE,       # disperse
                                             torus = TRUE,     # disperse
                                             yrs.to.run = 10){ # number of years to run simulations ()
  
  # 2 times steps per year so yrs.to.run*2 plus the initial 3 time steps (start in Apr=t0, Oct=t1, Apr=t2)
  IBM.sim.out <- vector('list', yrs.to.run+2)
  
  # *** Step 1. START ***
  # The assumption is that there is 100% survival during the first year (i.e., the set up), at the first time step no fishers die
  # •	t0 = October to April = kits are born; need to run through the reproduce functions
  # i.	t0 <- repro(fishers=t0, repro_estimates=repro.CI, Fpop="C")
  # t0=tmp$t0; land=tmp$land
  t0 <- repro_FEMALE(fishers = t0, 
                     repro_estimates = repro_estimates, 
                     Fpop = Fpop)
  
  print(NLcount(t0))
  IBM.sim.out[[1]] <- t0 # time step ends at April
  
  # *** Step 2. AGE ***
  # •	The assumption is that there is 100% survival during the first year (i.e., the set up), at the second time step no fishers die
  # •	t1 = April to October = kits kicked out of natal territory
  # •	all fishers age 1 year (to make up fro the Oct to Apr to Oct from start of t0)
  
  age.val <- of(agents=t0, var=c("age"))+1
  t1 <- NLset(turtles = t0, agents=turtle(t0, who=t0$who),var="age", val=age.val)
  
  # plot(land)
  # points(t1, pch = t1$shape, col = of(agents = t1, var = "color"))
  
  # print(NLcount(t1))
  # IBM.sim.out[[2]] <- t1 # time step ends at October
  
  # *** Step 3. ESTABLISH / MAINTAIN TERRITORY & SCENT TERRITORY (MATE) & SURVIVE ***
  # •	 t2 = October to April = females with established territory find mate
  # •	 3a = the first step is for individuals without territories to disperse; run through the disperse function up to 30 times to allow 6 months of movement
  # i.	t2 <- disperse(land=land, fishers=t2, dist_mov=dist_mov, out=FALSE)
  # •	all fishers age 0.5 years
  # •	at the end of this time step, all fishers subject to mortality; run through the survive function
  # i.	t2 <- survive(fishers=t2, surv_estimates=rf_surv_estimates, Fpop=Fpop, maxAgeFemale=maxAgeFemale)
  
  t2 <- t1
  for(i in 1:30){
    t2 <- disperse_FEMALE(land=land, fishers=t2, dist_mov=dist_mov, out=out, torus=torus)
  }
  
  # patchHere(land, t2)
  # plot(land)
  # points(t2, pch = t2$shape, col = of(agents = t2, var = "color"))
  
  age.val <- of(agents=t2, var=c("age"))+0.5
  t2 <- NLset(turtles = t2, agents=turtle(t2, who=t2$who),var="age", val=age.val)
  
  t2 <- survive_FEMALE(t2, surv_estimates=surv_estimates, Fpop=Fpop, maxAgeFemale=maxAgeFemale)
  
  # plot(land)
  # points(t2, pch = t2$shape, col = of(agents = t2, var = "color"))
  
  print(NLcount(t2))
  IBM.sim.out[[2]] <- t2 # time step ends at April
  
  
  ################################################################################
  
  tOct <- t2
  
  for(tcount in 3:(yrs.to.run+2)){
    
    #    # *** Step 4.  ESTABLISH / MAINTAIN TERRITORY ***
    # •	t3 = April to October = keep surviving
    # •	4a = the first step is for individuals without territories to disperse; run through the disperse function up to 30 times to allow 6 months of movement
    # i.	TOct <- disperse(land=land, fishers=tOct, dist_mov=dist_mov, out=FALSE)
    # •	all fishers age 0.5 years
    # •	update the fisher table to change juveniles to adults as they age out of (i.e., age > 2)
    
    if(NLcount(tOct)!=0){
      
      for(i in 1:30){
        tOct <- disperse_FEMALE(land=land, fishers=tOct, dist_mov=dist_mov, out=out, torus=torus)
      }
      
      age.val <- of(agents=tOct, var=c("age"))+0.5
      tOct <- NLset(turtles = tOct, agents=turtle(tOct, who=tOct$who),var="age", val=age.val)
      
      breed.val <- as.data.frame(of(agents=tOct, var=c("breed","age")))
      breed.val$breed <- case_when(breed.val$age>2 ~ "adult",
                                   TRUE ~ as.character(breed.val$breed))
      
      tOct <- NLset(turtles = tOct, agents=turtle(tOct, who=tOct$who),var="breed", val=breed.val$breed)
      # print(NLcount(tOct))
      #   IBM.sim.out[[tcount]] <- tOct
      # } else {
      #   IBM.sim.out[[tcount]] <- 0
    }
    
    # plot(land)
    # points(tOct, pch = tOct$shape, col = of(agents = tOct, var = "color"))
    
    # *** Step 5. ESTABLISH / MAINTAIN TERRITORY & REPRODUCE & SCENT TERRITORY (MATE) & SURVIVE ***
    # •	t4 = October to April = females with established territory produce kits
    # •	5a = the first step is for pregnant female fishers to reproduce; run through the reproduce denning and kits_produced functions
    # i.	tApr <- denning(fishers=tOct, denLCI=denLCI, denUCI=denUCI)
    # ii.	tApr <- kits_produced(fishers=tApr, ltrM=ltrM, ltrSD=ltrSD)
    # •	5b = the second step is for juvenile fishers without established territories to move; loop through the disperse function up to 30 times
    # i.	tApr <- disperse(land=land, fishers=tApr, dist_mov=dist_mov, out=out)
    # •	all fishers age 0.5 years
    # •	at the end of this time step, all fishers subject to mortality; run through the survive function
    # i.	tApr <- survive(fishers=tApr, surv_estimates=rf_surv_estimates, Fpop=Fpop, maxAgeFemale=maxAgeFemale)
    # *** Step 4.  ESTABLISH / MAINTAIN TERRITORY ***
    # t3 = April to October = keep surviving
    # 4a. function DISPERSE - run through DISPERSE function for individuals without territories, up to 30 times to allow 6 months of movement
    
    tApr <- tOct
    
    if(NLcount(tApr)!=0){
      
      tApr <- repro_FEMALE(fishers=tApr, repro_estimates=repro_estimates, Fpop=Fpop)
      
      for(i in 1:30){
        tApr <- disperse_FEMALE(land=land, fishers=tApr, dist_mov=dist_mov, out=out, torus=torus)
      }
      
      age.val <- of(agents=tApr, var=c("age"))+0.5
      tApr <- NLset(turtles = tApr, agents=turtle(tApr, who=tApr$who),var="age", val=age.val)
      
      tApr <- survive_FEMALE(fishers=tApr, surv_estimates=surv_estimates, Fpop=Fpop, maxAgeFemale=maxAgeFemale)
      
      # patchHere(land, tApr)
      # plot(land)
      # points(tApr, pch = tApr$shape, col = of(agents = tApr, var = "color"))
      
      print(NLcount(tApr))
      
      IBM.sim.out[[tcount]] <- tApr
      
    } else {
      IBM.sim.out[[tcount]] <- 0 }
    
    tOct <- tApr
    
  }
  return(IBM.sim.out)
}
