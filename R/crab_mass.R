#########Beeswarm plot for mass########
source("R/load_filter_data.R")
source("R/ggplot_prep.R")
library(ggbeeswarm) 

# Convert Crab_Treatment_Num to a character to make it more flexible when faceting
master_fidd$Crab_Treatment_Num <- as.character(master_fidd$Crab_Treatment_Num)

# Rename Crab_Treatment_Num to `Crab Density` for clarity and aesthetics
master_fidd <- master_fidd %>%
  dplyr::rename(`Crab Density` = Crab_Treatment_Num) 

# Plot the beeswarm plot with Site on the x, Crab Mass on the y, and faceted
# by Crab Density
ggplot(master_fidd,
       aes(Site, Crab_Mass))+
  geom_quasirandom(aes(color = Site))+
  ylab('Crab Mass(g)')+
  scale_color_manual(aesthetics = "Site", 
                     values=c("NAN"="orangered2",
                              "PIE"="darkblue")) +
  facet_wrap(vars(`Crab Density`), labeller = label_both) +
  My_theme +
  theme(strip.text.x = element_text(size = 16,
                                    face = "bold"))


