### Create heatmaps for the outputs
# keep in mind that WGS84 lat/long espg = 4326; BC Albers espg = 3005; NAD83 / UTM zone 10N espg = 26910

# find coordinates for each fisher at 11 year mark (initial year + yrs+sim)
# create a heat map based on number of times fisher territory is selected
# uses presence/absence for female fishers on pixel (either 1 or 0) for each run
# then uses sum to count how many times each pixel is selected (out of numsims run, i.e., 100)
# the mean and se relate to the number of pixels (i.e., territories) selected per simulation

heatmap_output <- function(sim_out, 
                           iterations, 
                           clus_yrs, 
                           TS, 
                           propFemales,
                           rextent){
  # sim_out = fishers_output;iterations = 10;clus_yrs = 5; TS = 5; rextent = Mahal_land[[1]]; propFemales=0.3

  TS_full=paste0("TimeStep_",str_pad(TS,2,pad="0"))
  
  # find out how many runs had at least one female adult fisher alive at end)
  tmp <- sim_output(sim_out=sim_out, iterations=iterations, clus_yrs=clus_yrs)
  
  # fishers_to_start <- round(tmp %>% filter(TimeStep=="TimeStep_01") %>% summarise(numAF=mean(Count))) # average fishers at 1 year
  
  Nozero.runs <- tmp %>% filter(TimeStep==TS_full) %>%
    filter(Count!=0)
  
  tmp2 <- Nozero.runs %>% dplyr::select(Run)
  nozerosims <- tmp2$Run
  
  # set extent of raster the same as extent of initial world
  rw <- world2raster(rextent)
  
  r <- raster()
  r <- setExtent(r, rw, keepres=TRUE)
  
  r_list=list()
  
  # for simulations where at least one fisher survived
  for(i in 1:length(nozerosims)){
    # i=1
    ftmp1 <- sim_out[[nozerosims[i]]][[TS]]
    whoEAF <- ftmp1[ftmp1$breed=="adult" & ftmp1$disperse=="E",]$who
    EAFind <- turtle(ftmp1, who = whoEAF) # fishers who are dispersing (i.e., kits)
    
    ftmp <- as.data.frame(patchHere(rextent, EAFind))
    ftmp$Fisher <- 1
    ftmp.sf <- st_as_sf(ftmp, coords = c("pxcor", "pycor"))
    ftmp.sfp <- st_buffer(ftmp.sf, dist=.1)
    
    r_list[[i]] <- rasterize(ftmp.sfp, r, field="Fisher", background=0) # interim work around until terra and new raster package uploaded
  }
  
  r_zeroes <- raster()
  r_zeroes <- setExtent(r_zeroes, rw, keepres=TRUE)
  values(r_zeroes) <- 0
  
  r_zeroes_list=list()
  
  if(length(nozerosims)!=100){
    for(i in 1:(100-length(nozerosims))){
      r_zeroes_list[[i]] <- r_zeroes
    }
  }
  
  r_stack = stack(r_list, r_zeroes_list)
  r_stackApply <- stackApply(r_stack, indices=1, fun=sum)
  
  extent(r_stackApply) <- extent(rextent)
  # writeRaster(r_stackApply, file=paste0("out/",dir_name,"/rSim_",name_out,"_",round(sim_out[[sim_order-3]]$actual.prop.hab*100),"hab.tif"), bylayer=TRUE, overwrite=TRUE)
  
  # Fisher_Nmean <- mean(r_stackApply@data@values[r_stackApply@data@values>1])
  Fisher_Nmean <- mean(r_stackApply@data@values)
  Fpredicted <- round(sum(r_stackApply@data@values/100))
  # Fisher_Nse <- se(r_stackApply@data@values[r_stackApply@data@values>1])
  Fisher_Nse <- se(r_stackApply@data@values)
  
  suitable_habitat <- sum(rextent)
  round(suitable_habitat*0.3) # number of fisher started with
  total_habitat <- dim(rextent)[1]*dim(rextent)[2]
  
  mtext_left <- floor(r_stackApply@extent@xmin) # to get legend to display at left extent of map
  
  Cairo(file=paste0("out/rHeatmap_Year",TS,"_hab.PNG"), type="png", width=2200, height=2000,pointsize=15,bg="white",dpi=300)
  
  plot(r_stackApply, oma=c(2, 3, 5, 2))
  mytitle = paste0("Estimated Fisher Territories over ",iterations," Simulations")
  mysubtitle1 = paste0("Starting with ",fishers_to_start$numAF," fishers and ",round(suitable_habitat/total_habitat*100),"% habitat")
  # mysubtitle2 = paste0("predicted ",round(Fisher_Nmean)," \u00B1 ",round(Fisher_Nse)," (mean \u00B1 1 SE) established fisher territories after ",yrs_sim," years.")
  mysubtitle2 = paste0("predicted ",Fpredicted," established fisher female territories after ",yrs_sim," years.")
  mtext(side=3, line=3, at=mtext_left, adj=0, cex=1, mytitle)
  mtext(side=3, line=2, at=mtext_left, adj=0, cex=0.8, mysubtitle1)
  mtext(side=3, line=1, at=mtext_left, adj=0, cex=0.8, mysubtitle2)
  dev.off()
  
  return(list(raster=r_stackApply, Fisher_Nmean=Fisher_Nmean, Fisher_Nse=Fisher_Nse, Fpredicted=Fpredicted, nozerosims=nozerosims))
  
}
