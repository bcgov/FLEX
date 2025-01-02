# grab output from one set of 100 simulations
sim_output <- function(sim_out,
                       simul, # Previously sim, but sim is a special name in SpaDES
                       numsims, 
                       yrs_sim){
  # sim_out=IBM; simul=2; numsims=100; yrs_sim=10
  
  num.runs <- yrs_sim + 1
  
  ABM.df <- as.data.frame(array(NA,c(numsims,num.runs)))
  colnames(ABM.df) <- paste0("TimeStep_",str_pad(seq_len(num.runs),2,pad="0"))
  
  # Suggesting using data.table and lapply
  # Rows = replicates (n = 100)
  # Columns = time steps (n = 12)
  
  Reps <- 1:numsims
  timeSteps <- 1:num.runs # Name = paste0("TimeStep_", 1:12)
  
  ABM.df <- rbindlist(lapply(Reps, function(rps){
    ABM.df_ts <- rbindlist(lapply(timeSteps, function(ts){
      # Structure => simulation::replicates::timeStep
      # Only simulation 4 has the data of interest?
      DT <- as.array(sim_out[[simul]][[rps]][[ts]])
      
      if (length(DT) > 1){
        nAdults = as.numeric(table(DT[DT$disperse=="E"]$breed)["adult"])
        nJuvenile = as.numeric(table(DT[DT$disperse=="E"]$breed)["juvenile"])
      } else {
        nAdults = 0
        nJuvenile = 0
      }
      tb <- data.table(Sim = paste0("Sim",str_pad(simul,2,pad="0")),
                       Run = rps,
                       TimeStep = paste0("TimeStep_",str_pad(ts,
                                                             2, pad="0")),
                       Count = nAdults)
      return(tb)
    }))
  }))
  
}