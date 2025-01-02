###--- SET-UP WORLD for female only IBM
set_up_world_FEMALE <- function(nFemales=nFemales, maxAgeFemale=maxAgeFemale,xlim=xlim, ylim=ylim, prophab=prophab){
  # nFemales = 20
  # maxAgeFemale = 9
  # xlim = ylim = c(1,10)
  # prophab = 0.7
  # There are two types of 'agents' in NetLogoR: patches and turtles
  # Patches cannot move (i.e., landbase) while turtles can (i.e., fishers)
  
  # Create a landscape with coordinates equal to xlim and ylim (default is 20*20 square landscape)
  # Each cell is assumed to be the same size as one fisher territory
  # Cell values are randomly chosen either 0 or 1
  # Assume 0 = habitat unsuitable for fisher territory; 1 = suitable fisher habitat
  # with the proportion of suitable "good" habitat (1) given in function (default = 0.5)
  # Create the patches
  numcells <- (xlim[2]-xlim[1]+1) * (ylim[2]-ylim[1]+1)
  
  land <- createWorld(minPxcor = xlim[1], maxPxcor = xlim[2],
                      minPycor = ylim[1], maxPycor = ylim[2],
                      rbinom(numcells, 1, prophab))
  
  # randomly select "good" habitat cells for each fishers
  rtmp <- world2raster(land)
  numhabitatcells <- sum(rtmp@data@values) # number of "good" habitat cells
  actual.prop.hab <- numhabitatcells/numcells
  
  nfishers = nFemales
  fishers_start <- as.data.frame(sampleStratified(rtmp, size=nfishers, xy=TRUE, )) %>% dplyr::filter(layer==1)
  fishers_start <- as.matrix(fishers_start[c("x","y")])
  
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
  
  return(list("land"=land, "t0"=t0, "actual.prop.hab"=actual.prop.hab))
  
}