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

###--- SET-UP WORLD bringin in land aoi created previously - for female only IBM
set_up_REAL_world_FEMALE <- function(propFemales, 
                                     maxAgeFemale,
                                     land,
                                     repro_estimates,
                                     Fpop){

  # land = Mahal_land[[1]]; maxAgeFemale=9
  # Raster with mahalanobis distance (D2) values
  # Use mahal_metric to determine 
  cells.good.habitat <- sum(land)
  # total.cells <- dim(land)[1]*dim(land)[2]
  # actual.prop.hab <- cells.good.habitat / total.cells
  
  # for some reason NetLogoR world matrices are set up differently from rasters
  # need to flip, change coordinates (-1), and keep in mind that NL worlds are col by row
  habM <- as.matrix(land@.Data)
  habMflipped <- habM[nrow(habM):1,]
  
  # recall that Suitable Habitat for territories has a raster value of 1
  mHabitat <- which(habMflipped==1, arr.ind=TRUE)
  tmpMatrix <- matrix(1, nrow=nrow(mHabitat), ncol=ncol(mHabitat))
  NLmHabitat <- mHabitat - tmpMatrix
  
  fishers_start <- as.data.frame(NLmHabitat)
  colnames(fishers_start) <- c("pycor", "pxcor")
  fishers_start$rank <- rank(round(runif(cells.good.habitat, min=10000, max=999999)))
  
  # have the number of fishers be 30% of the good habitat cells
  nFemales <- round(cells.good.habitat*propFemales)
  nfishers = nFemales
  
  fishers_start <- fishers_start %>% filter(rank <= nFemales) %>% dplyr::select(-rank)
  fishers_start <- fishers_start[c("pxcor","pycor")]
  
  fishers_start <- as.matrix(fishers_start)
  # Start with a landscape of adult females and males, all on "good" habitat
  t0 <- createTurtles(n = nfishers, coords=fishers_start, breed="adult")
  
  # create values and assign as adult (females) with established territories
  t0 <- turtlesOwn(turtles = t0, tVar = c("shape"), tVal =16) # females are circles, males are squares
  t0 <- turtlesOwn(turtles = t0, tVar = c("disperse"), tVal = c(rep("E", each=nfishers)))
  t0 <- turtlesOwn(turtles = t0, tVar = c("repro"), tVal = 0)
  
  # have fishers randomly assigned a year between 2.5 and 1 year less than max life span
  yrs.adult <- (sample(5:((maxAgeFemale-1)*2), nfishers, replace=TRUE))/2
  t0 <- turtlesOwn(turtles=t0, tVar = c("age"), tVal = yrs.adult)

  # Visualize the turtles on the landscape with their respective color
  plot(land)
  points(t0, pch = t0$shape, col = of(agents = t0, var = "color"))
  
  # have females reproduce - set up kits and age everyone 1 year, ready to start 
  t0 <- repro_FEMALE(fishers=t0, repro_estimates=repro_estimates, Fpop=Fpop)
  
  # t1	October	Kits are kicked out of natal territory
  # 	Age 1 year (t0 and t1)
  
  age.val <- of(agents=t0, var=c("age"))+1
  fishers <- NLset(turtles = t0, agents=turtle(t0, who=t0$who),var="age", val=age.val)
  
  return(fishers)
}


# fishers <- set_up_REAL_world_FEMALE(propFemales = 0.3,
#                                     maxAgeFemale = 9,
#                                     land = land,
#                                     repro_estimates = repro_estimates,
#                                     Fpop = "C")

