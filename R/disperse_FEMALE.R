###--- DISPERSE
disperse_FEMALE <- function(land=land, fishers=fishers, dist_mov=1.0, out=TRUE, torus=TRUE) {
  # Only want fishers without established territories to move
  # Assume female fisher can move ~35 km in a month, and that each pixel is 5.5 km in length or 7.8 km in diameter
  # For ease of calculations, assume a dist_mov of 1.0 is one pixel
  # This means that a female fisher can move between 5-6 pixels per month or 30-36 pixels in each time step
  # dist_mov relates to the number of cells (not quite right if fisher moving diagonally across a cell but works for our purposes)
  
  # fishers=tmp2
  # land=tmp$land
  whoDFishers <- fishers[fishers$disperse=="D" & fishers$age>0,]$who
  disperseInd <- turtle(fishers, who = whoDFishers) # fishers who are dispersing (i.e., kits)
  
  # Have each fisher move 1 step in random heading
  # The landscape is not wrapped (torus = FALSE); or try with torus wrapped and OUT=TRUE
  # and the fishers can disperse outside of the landscape (out=TRUE)
  disperseInd <- right(disperseInd, angle = runif(n = NLcount(disperseInd), min = 0, max = 360))
  # patchHere(land, disperseInd)
  disperseInd <- fd(turtle(disperseInd, who=whoDFishers),dist=dist_mov, land, torus=torus, out=out) # all kits move 1 cell # this doesn't allow for dispersing fishers
  # patchHere(land, disperseInd)
  
  # if any dispersing fishers have exited the worlds extent, remove them from the simulation
  fisher.location <- as.data.frame(patchHere(land, disperseInd))
  fisher.location$who <- disperseInd$who
  out.of.bounds.fisher <- fisher.location[is.na(fisher.location$pxcor),]$who
  
  disperseInd <- die(disperseInd, who=out.of.bounds.fisher) # remove fishers who traveled outside worlds extent from dispersing object
  fishers <- die(fishers, who=out.of.bounds.fisher) # remove fishers who traveled outside worlds extent from main object
  
  ###--- for dispersing FEMALES
  # determine patch information for dispersing females
  # only run the loop if there are dispersing females
  if(NLcount(disperseInd)!=0){
    disperseIndF <- turtle(disperseInd, who = disperseInd$who) # identify those dispersing (i.e., female kits)
    disperseHabitatF <- of(land, agents=patchHere(land, disperseIndF)) # identify habitat quality of current location
    disperseHabitatF[is.na(disperseHabitatF)] <- 0 # any NA habitat (i.e., outside of world is NOT suitable)
    dispersePatchF <- patchHere(land, disperseIndF) # the coordinates for the cells
    dispersePatchF[is.na(dispersePatchF)] <- 0
    
    # run loop to determine if females are dispersing
    # if the female kit finds a good quality cell (1) that is NOT occupied by another female (occupancy==1) can stay,
    # otherwise (if habitat = 0 OR occupancy>1) kit keeps moving
    # "D" = disperse; "E" = establish territory
    
    for(k in 1:NLcount(disperseIndF)){
      # check how many fishers are currently on the cell
      # k=1
      disperseInd.patch.occ <- turtlesOn(world = land, turtles = disperseIndF[k],
                                         agents = patch(land, dispersePatchF[k,1], dispersePatchF[k,2]))
      
      if(disperseHabitatF[k]==1 & NLcount(disperseInd.patch.occ)==1){ # if the habitat is good and there is only one turtle on the patch
        disperseIndF <- NLset(turtles = disperseIndF, agents = turtle(disperseIndF, who = disperseIndF[k]$who), var = "disperse", val = "E") # then establish
      } else {
        disperseIndF <- NLset(turtles = disperseIndF, agents = turtle(disperseIndF, who = disperseIndF[k]$who), var = "disperse", val = "D") # otherwise keep dispersing
      }
    }
    
    # now have updated object with kits dispersing or establishing
    # add the new values to the existing fishers 'turtle' object
    valdisperseIndF <- of(agents=disperseIndF, var=c("heading","xcor","ycor", "prevX","prevY","disperse"))
    fishers <- NLset(turtles = fishers, agents=turtle(fishers, who=disperseIndF$who),var=c("heading","xcor","ycor","prevX","prevY","disperse"), val=valdisperseIndF)
  }
  
  
  return(fishers)
  
}
