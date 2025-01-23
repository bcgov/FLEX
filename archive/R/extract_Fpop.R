#######--- EXTRACT Fpop character from raster value
extract_Fpop <- function(rFpop=rFpop){
  
  Fpop_num <- median(rFpop@data@values, na.rm=T)
  Fpop <- ifelse(Fpop_num==1, "B","C")

  return("Fpop"=Fpop)
}

# Fpop <- extract_Fpop(rFpop = IBM_aoi$r_static$layer.1)
