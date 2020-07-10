######SS before and after#####
source("R/load_filter_data.R")
source("R/ggplot_prep.R")
library(visreg) 
library(DHARMa)

before_and_after <- glm(Soil_Strength_After ~ Soil_Strength_Before * 
                          Site + Crab_Mass,
                        data = master_nozero)
summary(before_and_after)

visreg(before_and_after,
       'Soil_Strength_Before',
       scale = 'response',
       by = 'Site',
       gg = TRUE,
       overlay = TRUE,
       ylab = 'Soil Strength After (psi)',
       xlab = 'Soil Strength Before (psi)') +
  scale_x_continuous(breaks = c(0,5,10,15,20,25)) +
  scale_y_continuous(breaks = c(0,5,10,15,20,25)) +
  scale_color_manual("Site", 
                     values=c("NAN"="orangered2",
                              "PIE"="darkblue")) +
  My_theme
