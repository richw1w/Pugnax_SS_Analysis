########Probability of Creating a Burrow Comparison####
#prep
source("R/load_filter_data.R")
source("R/ggplot_prep.R")
library(visreg) 
library(DHARMa)

#one crab
Compaction_vs_Dig1 <- glm(Dig~Soil_Strength_Before * 
                            Site + Crab_Mass,
                          data = master1,
                          family = binomial)
plot(simulateResiduals(Compaction_vs_Dig1))
summary(Compaction_vs_Dig1)
car::Anova(Compaction_vs_Dig1)

#fit plot with visreg
visreg(Compaction_vs_Dig1,
       'Soil_Strength_Before',
       scale = 'response',
       by = 'Site',
       gg = TRUE,
       overlay = TRUE,
       ylab = 'Probability of Creating a Burrow',
       xlab = 'Soil Strength (psi)') +
  scale_x_continuous(breaks = c(0,5,10,15,20,25)) +
  scale_y_continuous(breaks = c(0,1)) +
  scale_color_manual("Site", 
                     values=c("NAN"="orangered2",
                              "PIE"="darkblue")) +
  My_theme

#three crab
Compaction_vs_Dig3 <- glm(Dig~Soil_Strength_Before * 
                            Site + Crab_Mass,
                          data = master3,
                          family = binomial)
plot(simulateResiduals(Compaction_vs_Dig3))
summary(Compaction_vs_Dig3)
car::Anova(Compaction_vs_Dig3)

#fit the curve
visreg(Compaction_vs_Dig3,
       'Soil_Strength_Before',
       scale = 'response',
       by = 'Site',
       gg = TRUE,
       overlay = TRUE,
       ylab = 'Probability of Creating a Burrow',
       xlab = 'Soil Strength (psi)') +
  scale_x_continuous(breaks = c(0,5,10,15,20,25))+
  scale_y_continuous(breaks = c(0,1)) +
  scale_color_manual("Site", 
                     values=c("NAN"="orangered2",
                              "PIE"="darkblue")) +
  My_theme
