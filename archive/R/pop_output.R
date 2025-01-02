# grab output from all sets of simulations
pop_output <- function(sim_out, 
                       sim_order = c(4:6), 
                       numsims, 
                       yrs_sim){
  # sim_out=Boreal_escape_FEMALE_binom
  # sim_order=c(4:6)
  num.runs <- yrs_sim + 1
  ABM.df <- as.data.frame(array(NA,c(num.runs*numsims*length(sim_order),4)))
  colnames(ABM.df) <- c("Run","TimeStep","Count","Sim")
  
  a=1
  b=num.runs*numsims
  for(i in 4:6){
    ABM.df[a:b,] <- sim_output(sim_out=sim_out, sim=i, numsims=numsims, yrs_sim=yrs_sim)
    a=a+(num.runs*numsims)
    b=b+(num.runs*numsims)
  }
  return(ABM.df)
}
