# grab output from one set of 100 simulations
sim_output <- function(sim_out,
                       # simul, # Previously sim, but sim is a special name in SpaDES
                       iterations, 
                       clus_yrs){
  # sim_out=fishers_output; iterations=10; clus_yrs=5
  
  ABM.df <- as.data.frame(array(NA,c(iterations,clus_yrs)))
  colnames(ABM.df) <- paste0("TimeStep_",str_pad(seq_len(clus_yrs),2,pad="0"))
  
  # Rows = replicates (n = 100)
  # Columns = time steps (n = 5)
  
  Reps <- 1:iterations
  timeSteps <- 1:clus_yrs # Name = paste0("TimeStep_", 1:12)
  
  ABM.df <- rbindlist(lapply(Reps, function(rps){
    ABM.df_ts <- rbindlist(lapply(timeSteps, function(ts){
      DT <- as.array(sim_out[[rps]][[ts]])
      
      if (length(DT) > 1){
        nAdults = as.numeric(table(DT[DT$disperse=="E"]$breed)["adult"])
        nJuvenile = as.numeric(table(DT[DT$disperse=="E"]$breed)["juvenile"])
      } else {
        nAdults = 0
        nJuvenile = 0
      }
      tb <- data.table(Run = rps,
                       TimeStep = paste0("TimeStep_",str_pad(ts,2, pad="0")),
                       Count = nAdults)
      return(tb)
    }))
  }))
  
}