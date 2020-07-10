#########Beeswarm plot for mass########
source("R/load_filter_data.R")
source("R/ggplot_prep.R")
library(ggbeeswarm) 

ggplot(master_fidd,
       aes(Site, Crab_Mass))+
  geom_quasirandom(aes(color = Site))+
  ylab('Crab Mass(g)')+
  scale_color_manual(aesthetics = "Site", 
                     values=c("NAN"="orangered2",
                              "PIE"="darkblue"))+
  My_theme 


