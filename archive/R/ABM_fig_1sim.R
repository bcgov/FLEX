ABM_fig_1sim <- function(sim_out, 
                         numsims, 
                         yrs_sim, 
                         Fpop){
  # sim_out=B.w1_real.FEMALE; numsims=100; yrs_sim=10; Fpop="B"
  # sim_out=scenario1; numsims=100; yrs_sim=10; Fpop="B"
  ABM.df <- sim_output(sim_out=sim_out, sim=2, numsims=numsims, yrs_sim=yrs_sim)
  ABM.df$Pop <- rep(Fpop, each=dim(ABM.df)[1])
  
  ABM.df$Pcnthab <- sim_out[[1]]$actual.prop.hab
  ABM.TS.mean <- ABM.df %>% dplyr::select(-Run) %>% pivot_wider(names_from=TimeStep, values_from=Count, values_fn=mean)
  ABM.TS.mean$Param <- "Mean"
  
  ABM.TS.se <- ABM.df %>% dplyr::select(-Run) %>% pivot_wider(names_from=TimeStep, values_from=Count, values_fn=se)
  ABM.TS.se$Param <- "SE"
 
  ABM.TS <- rbind(ABM.TS.mean, ABM.TS.se)
  
  ABM.TS.df <- ABM.TS %>% pivot_longer(cols = 4:(3+yrs_sim+1),names_to = "TimeStep",values_to = "Value" )
  ABM.TS.df <- ABM.TS.df %>% pivot_wider(names_from = Param, values_from = Value)
  
  ABM.TS.use <- ABM.TS.df %>% filter(!TimeStep %in% c("TimeStep_01"))
  
  ABM.TS.use$TimeStepNum <- as.numeric(substr(ABM.TS.use$TimeStep,10,11))
  
  pal_col <- pnw_palette(name="Starfish",n=7,type="discrete")
  
  Fpop_name <- ifelse(Fpop=="C","Columbian","Boreal")
  fishers_to_start <- nrow(sim_out[[1]]$t0)

  sim.TS.plot_se <- ggplot(data = ABM.TS.use) +
    theme_bw() + theme(strip.background = element_rect(fill = "white", colour = "white")) +
    theme(panel.grid = element_blank())+
    geom_vline(xintercept = "TimeStep_06", col="grey", lty=4) +
    geom_hline(yintercept = 0, col="grey", lty=4) +
    geom_point(aes(x = TimeStep, y = Mean), size=2) +
    geom_errorbar(aes(x = TimeStep, y = Mean, ymin=Mean-SE, ymax= Mean+SE),
                  width=.2, position=position_dodge(0.05)) +
    theme(axis.text.x = element_blank()) +
    xlab(expression(paste("Annual Predictions Starting at T"[0]))) +
    ylab("Number of Adult Female Fishers (Mean \u00B1 1 SE)")+ # \u00B1 is ± in unicode
    ggtitle(paste0("Simulating ",yrs_sim," Years of ",Fpop_name," Fisher Populations,\nStarting with ",
                   round(min(ABM.TS.df$Pcnthab*100)),"% Suitable Habitat and ",
                   fishers_to_start," Adult Female Fishers"))

  return(list(ABM.TS.df=ABM.TS.df, sim.TS.plot_se=sim.TS.plot_se))
}
