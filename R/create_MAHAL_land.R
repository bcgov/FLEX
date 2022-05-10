#######--- convert Mahal FETA to suitable habitat based on D2, underlying FHEzone and threshold value (based on Rich's analysis = Mean, Max, SD or combination)
create_MAHAL_land <- function(rFHzone,
                              rMahal,
                              mahal_metric,
                              D2_param){
  
  # Raster with mahalanobis distance (D2) values
  # Use mahal_metric to determine which value is our cap for suitable habitat

  mahal_tmp <- mahal_metric %>% select("FHE_zone_num", D2_param)
  if (ncol(mahal_tmp) >2 ) {
  mahal_tmp$CapD2 <- rowSums(mahal_tmp[,2:ncol(mahal_tmp)])
  } else {mahal_tmp$CapD2 <- mahal_tmp[,2]}


  rMahal <- rMahal[[1]] # use the first (current) Mahal landbase

  FHzones <- unique(rFHzone@data@values)
  FHzones <- FHzones[!is.na(FHzones)]

  rMahal_list <- list()
  for(i in 1:length(FHzones)){
    # i=1
    rFHzone_tmp <- rFHzone==FHzones[[i]]
    rFHzone_tmp[rFHzone_tmp<1] <- NA
    rMahal_list[[i]] <- raster::mask(rMahal <= mahal_tmp[i,c("CapD2")], rFHzone_tmp)
  }

  # sum(rMahal_list[[1]]@data@values, na.rm=TRUE)+sum(rMahal_list[[2]]@data@values, na.rm=TRUE)+sum(rMahal_list[[3]]@data@values, na.rm=TRUE)

  rMahal_brick <- brick(rMahal_list)
  rMahal_ST <- calc(rMahal_brick, sum, na.rm=TRUE)
  # plot(rMahal_ST)
  # sum(rMahal_ST@data@values)

  land <- raster2world(rMahal_ST)

  return("land"=land)

}

land <- create_MAHAL_land(rFHzone = IBM_aoi$r_static[[2]], # 1=Boreal, 2=Sub-boreal moist, 3=Sub-boreal dry, 4= Dry forest
                          rMahal = IBM_aoi$r_dynamic[[1]], # Mahalanobis distances for NetLogo world, subsequent rasters are 5 at year intervals
                          mahal_metric = fread(file.path(paste0(getwd(),"/modules/FLEX/"),"data/mahal_metric.csv"), select=c(1:5)),
                          D2_param = "Max")


