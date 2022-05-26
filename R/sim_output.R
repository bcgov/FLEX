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

# grab output from one set of simulations
sim_output <- function(sim_out,
                       simulations, 
                       clus_yrs){
  # sim_out=mySim$FLEX_output; simulations=2; clus_yrs=5
  
  ABM.df <- as.data.frame(array(NA,c(simulations,clus_yrs)))
  colnames(ABM.df) <- paste0("Year_",str_pad(seq_len(clus_yrs),2,pad="0"))
  
  # Rows = replicates (n = simulations)
  # Columns = years (n = clus_yrs)
  
  Reps <- 1:simulations
  timeSteps <- 1:clus_yrs
  
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
                       Year = paste0("Year_",str_pad(ts,2, pad="0")),
                       Count = nAdults)
      
      tb[is.na(tb$Count)]$Count <- 0 # converts NAs to 0 (NA arises when only juvenile remaining)
      
      return(tb)
    }))
  }))
}