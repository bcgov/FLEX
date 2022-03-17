### Create heatmaps for the outputs
# keep in mind that WGS84 lat/long espg = 4326; BC Albers espg = 3005; NAD83 / UTM zone 10N espg = 26910

# find coordinates for each fisher at 12 year mark
# create a heat map based on number of times fisher territory is selected
# uses presence/absence for female fishers on pixel (either 1 or 0) for each run
# then uses sum to count how many times each pixel is selected (out of numsims run, i.e., 100)
# the mean and se relate to the number of pixels (i.e., territories) selected per simulation

heatmap_output <- function(sim_out, 
                           sim_order, 
                           numsims, 
                           yrs_sim, 
                           TS, 
                           name_out){
  # sim_out=C.w1_real.FEMALE; sim_order=2; numsims=100; yrs_sim=10; TS=12; name_out="QTSA_ex2"
  
  TS_full=paste0("TimeStep_",TS)
  
  # find out how many runs had at least one female adult fisher alive at end)
  tmp <- sim_output(sim_out=sim_out, sim=sim_order, numsims=numsims, yrs_sim=yrs_sim)
  
  fishers_to_start <- tmp %>% filter(TimeStep=="TimeStep_01") %>% summarise(numAF=mean(Count))
  
  Nozero.runs <- tmp %>% filter(TimeStep==TS_full) %>%
    group_by(Sim) %>%
    filter(Count!=0)
  
  tmp2 <- Nozero.runs %>% dplyr::select(Run)
  nozerosims <- tmp2$Run
  
  # set extent of raster the same as extent of initial world
  rw <- world2raster(sim_out[[1]]$land)
  
  r <- raster()
  r <- setExtent(r, rw, keepres=TRUE)
  
  r_list=list()
  
  # for simulations where at least one fisher survived
  for(i in 1:length(nozerosims)){
    ftmp <- as.data.frame(patchHere(sim_out[[1]]$land, sim_out[[sim_order]][[nozerosims[i]]][[TS]]))
    ftmp$Fisher <- 1
    ftmp.sf <- st_as_sf(ftmp, coords = c("pxcor", "pycor"))
    ftmp.sfp <- st_buffer(ftmp.sf, dist=.1)
    
    # r_list[[i]] <- rasterize(ftmp.sfp, r, field="Fisher", fun=rFun, background=0) # interim work around until terra and new raster package uploaded
    r_list[[i]] <- rasterize(ftmp.sfp, r, field="Fisher", background=0) # interim work around until terra and new raster package uploaded
  }
  
  r_zeroes <- raster()
  r_zeroes <- setExtent(r_zeroes, rw, keepres=TRUE)
  values(r_zeroes) <- 0
  
  r_zeroes_list <- list()
  
  if(length(nozerosims) != 100){
    for(i in 1:(100 - length(nozerosims))){
      r_zeroes_list[[i]] <- r_zeroes
    }
  }
  
  r_stack = stack(r_list, r_zeroes_list)
  r_stackApply <- stackApply(r_stack, indices = 1, fun = sum)

  writeRaster(r_stackApply, file = paste0(Paths$outputPath, "/rSim_",name_out,"_",
                                          round(sim_out[[sim_order-3]]$actual.prop.hab*100),
                                          "hab.tif"), bylayer = TRUE, 
              overwrite = TRUE)
  
  Fisher_Nmean <- mean(r_stackApply@data@values)
  Fisher_Nse <- se(r_stackApply@data@values)
  
  Cairo(file = paste0(Paths$outputPath, "/rHeatmap_", name_out, "_", 
                      round(sim_out[[sim_order-3]]$actual.prop.hab*100),
                      "hab.PNG"), type = "png", width = 2200, height = 2000,
        pointsize = 15, bg = "white", dpi = 300)
  raster::plot(r_stackApply, oma = c(2, 3, 5, 2))
  mytitle = paste0("Estimated Fisher Territories over ", numsims," Simulations")
  mysubtitle1 = paste0("Starting with ", fishers_to_start$numAF," fishers and ",
                       round(sim_out[[1]]$actual.prop.hab*100), "% habitat")
  mysubtitle2 = paste0("predicted ", round(Fisher_Nmean)," \u00B1 ",
                       round(Fisher_Nse), paste0(" (mean \u00B1 1 SE) ",
                                                 "established fisher ",
                                                 "territories after "), 
                       yrs_sim, " years.")
  mtext(side = 3, line = 3, at = -0.07, adj = 0, cex = 1, mytitle)
  mtext(side = 3, line = 2, at = -0.07, adj = 0, cex = 0.8, mysubtitle1)
  mtext(side = 3, line = 1, at = -0.07, adj = 0, cex = 0.8, mysubtitle2)
  dev.off()
  
  return(list(raster = r_stackApply, Fisher_Nmean = Fisher_Nmean, 
              Fisher_Nse = Fisher_Nse, nozerosims = nozerosims))
  
}
