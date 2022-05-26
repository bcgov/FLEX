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

### Create heatmaps for the outputs
# keep in mind that WGS84 lat/long espg = 4326; BC Albers espg = 3005; NAD83 / UTM zone 10N espg = 26910

# find coordinates for each fisher at 11 year mark (initial year + yrs+sim)
# create a heat map based on number of times fisher territory is selected
# uses presence/absence for female fishers on pixel (either 1 or 0) for each run
# then uses sum to count how many times each pixel is selected (out of numsims run, i.e., 100)
# the mean and se relate to the number of pixels (i.e., territories) selected per simulation

heatmap_output <- function(sim_out, 
                           simulations, 
                           clus_yrs, 
                           propFemales,
                           rextent){
 # sim_out = FLEX_output; simulations = 10; clus_yrs = 5; propFemales = 0.3; rextent = Mahal_land[[1]]
  rw <- world2raster(rextent)
  
  TS_full=paste0("Year_",str_pad(clus_yrs,2,pad="0"))
  
  # find out how many runs had at least one female adult fisher with an established territory alive at end)
  tmp <- sim_output(sim_out=sim_out, simulations=simulations, clus_yrs=clus_yrs) # simulations = num of simulations; # clus_yrs = num years run
  
  Nozero.runs <- tmp %>% filter(Year==TS_full) %>%
    filter(Count!=0)
  
  tmp2 <- Nozero.runs %>% dplyr::select(Run)
  nozerosims <- tmp2$Run
  
  
  # for simulations where at least one fisher survived
  if(length(nozerosims)>0){
    
    # set extent of raster the same as extent of initial world
    r <- raster()
    r <- setExtent(r, rw, keepres=TRUE)
    r_list=list()
    
    
    # for simulations where at least one fisher survived
    for(i in 1:length(nozerosims)){
      # i=1
      ftmp1 <- sim_out[[nozerosims[i]]][[clus_yrs]]
      whoEAF <- ftmp1[ftmp1$breed=="adult" & ftmp1$disperse=="E",]$who
      EAFind <- turtle(ftmp1, who = whoEAF) # fishers who are dispersing (i.e., kits)
      
      ftmp <- as.data.frame(patchHere(rextent, EAFind))
      ftmp$Fisher <- 1
      ftmp.sf <- st_as_sf(ftmp, coords = c("pxcor", "pycor"))
      ftmp.sfp <- st_buffer(ftmp.sf, dist=.1)
      
      r_list[[i]] <- rasterize(ftmp.sfp, r, field="Fisher", background=0) # interim work around until terra and new raster package uploaded
    }
  }
  
  
  if(length(nozerosims)!=simulations){
    r_zeroes <- raster()
    r_zeroes <- setExtent(r_zeroes, rw, keepres=TRUE)
    values(r_zeroes) <- 0
    
    r_zeroes_list=list()
    for(i in 1:(simulations-length(nozerosims))){
      r_zeroes_list[[i]] <- r_zeroes
    }
  }
  
  
  if(length(nozerosims)==simulations){
    r_stack = stack(r_list)
  } else { if(sum(nozerosims)==0){
    r_stack = stack(r_zeroes_list)
  } else {   r_stack = stack(r_list, r_zeroes_list) 
  }
 }

  r_stackApply <- stackApply(r_stack, indices=1, fun=sum)
  extent(r_stackApply) <- extent(rextent)

  return(r_stackApply)
  
}
