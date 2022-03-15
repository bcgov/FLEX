###--- SET-UP WORLD with actual aoi - for female only IBM
set_up_REAL_world_FEMALE <- function(nFemales=nFemales, maxAgeFemale=maxAgeFemale,raoi=raoi){
  # nFemales = 10
  # maxAgeFemale = 9
  
  nfishers = nFemales
  
  # Each cell is assumed to be the same size as one fisher territory
  # Cell values are either 0 or 1
  # Assume 0 = habitat unsuitable for fisher territory; 1 = suitable fisher habitat
  # Upload the raster (binary for habitat)
  cells.good.habitat <- sum(raoi@data@values)
  total.cells <- raoi@ncols * raoi@nrows
  actual.prop.hab <- cells.good.habitat / total.cells
  
  land <- raster2world(raoi)
  as.matrix(land@pCoords)
  
  # for some reason NetLogoR world matrices are set up differently from rasters
  # need to flip, change coordinates (-1), and keep in mind that NL worlds are col by row
  habM <- as.matrix(land@.Data)
  habMflipped <- habM[nrow(habM):1,]
  
  mHabitat <- which(habMflipped==1, arr.ind=TRUE)
  tmpMatrix <- matrix(1, nrow=nrow(mHabitat), ncol=ncol(mHabitat))
  NLmHabitat <- mHabitat - tmpMatrix
  
  fishers_start <- as.data.frame(NLmHabitat)
  colnames(fishers_start) <- c("pycor", "pxcor")
  fishers_start$rank <- rank(round(runif(cells.good.habitat, min=100, max=999)))
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
  
  return(list("land"=land, "t0"=t0, "actual.prop.hab"=actual.prop.hab))
  
}
