###--- REPRODUCE
repro_FEMALE <- function(fishers, 
                         repro_estimates, 
                         Fpop) {
  
  # Random (binomial) selection for which adult females reproduce, based on denning rates confidence intervals
  # Fpop="C"; fishers
  # repro_estimates <- as_tibble(repro_estimates)
  
  whoFishers <- of(agents = fishers, var = c("who","breed")) # "who" of the fishers before they reproduce
  whoAFFishers <- whoFishers[whoFishers$breed=="adult",]$who
  
  denCI <- repro_estimates %>% dplyr::filter(str_detect(Pop,Fpop)) %>% dplyr::filter(str_detect(Param,"CI")) %>% dplyr::select(dr)
  repro <- rbinom(n = length(whoAFFishers), size=1, prob=min(denCI):max(denCI)) # prob can be a range - use confidence intervals
  fishers <- NLset(turtles = fishers, agents = turtle(fishers, who=whoAFFishers), var = "repro", val = repro)
  
  # Random selection for which adult females reproduce, based on denning mean and SD (Central Interior)
  whoFishers <- as.data.frame(of(agents = fishers, var = c("who","repro"))) # "who" of the fishers before they reproduce
  reproWho <- whoFishers[whoFishers$repro==1,]$who # "who" of fishers which reproduce
  
  ltrM=repro_estimates[repro_estimates$Pop==Fpop & repro_estimates$Param=="mean",]$ls
  ltrSD=repro_estimates[repro_estimates$Pop==Fpop & repro_estimates$Param=="sd",]$ls
  
  # if there is at least one fisher reproducing
  # have those fishers have offspring, based on the mean and sd of empirical data
  if (length(reproWho) > 0) {
    fishers <- hatch(turtles = fishers, who = reproWho, n=round(rnorm(n=1, mean=ltrM, sd=ltrSD)/2),breed="juvenile") # litter size based on empirical data (divided by 2 for female only model)
    
    # assign all of the offsprig as dispersing, change repro and age values to reflect newborn kits rather than their moms
    allFishers <- of(agents=fishers, var="who")
    offspring <- allFishers[!(allFishers %in% whoFishers$who)]
    
    fishers <- NLset(turtles = fishers, agents = turtle(fishers, who=offspring), var = "disperse", val = "D")
    fishers <- NLset(turtles = fishers, agents = turtle(fishers, who=offspring), var = "age", val = 0) # just born so time step 0
    fishers <- NLset(turtles = fishers, agents = turtle(fishers, who=offspring), var = "repro", val = 0) # just born not yet reproductive
  }
  
  return(fishers)
}
