# initial world plot
setup_plot <- function(sim_out, 
                       name_out,
                       pathOut){
  ###--- plot the simulated landbases
  Cairo(file=paste0("out/",name_out,"_",round(sim_out[[1]]$actual.prop.hab*100),"hab_setup.PNG"),type="png",width=2200,height=2000,pointsize=12,bg="white",dpi=300)
  plot(sim_out[[1]]$land, main=c(paste0("Simulated Landbase"),paste0(round(sim_out[[1]]$actual.prop.hab*100),"% Suitable Habitat")))
  points(sim_out[[1]]$t0, pch = sim_out[[1]]$t0$shape, col = of(agents = sim_out[[1]]$t0, var = "color"))
  dev.off()
  
  Cairo(file=paste0("out/",name_out,"_",round(sim_out[[1]]$actual.prop.hab*100),"hab_setup_nofishers.PNG"),type="png",width=2200,height=2000,pointsize=12,bg="white",dpi=300)
  plot(sim_out[[1]]$land, main=c(paste0("Simulated Landbase"),paste0(round(sim_out[[1]]$actual.prop.hab*100),"% Suitable Habitat")))
  dev.off()
}
