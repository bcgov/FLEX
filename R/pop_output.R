# grab output from all sets of simulations
pop_output <- function(sim_out, 
                       iterations, 
                       clus_yrs){
  
  ABM.df <- as.data.frame(array(NA,c(clus_yrs*iterations,3)))
  colnames(ABM.df) <- c("Run","TimeStep","Count")
  
  ABM.df <- sim_output(sim_out=sim_out, iterations=iterations, clus_yrs=clus_yrs)
  
  return(ABM.df)
}
