#####SS vs vol Comparison#####

#prep
source("R/load_filter_data.R")
source("R/ggplot_prep.R")
library(visreg) 
library(DHARMa)

#one crab
#interaction between compaction and site with mass as covariable
Compaction_vs_vol_nonzero1 <-glm(Burrow_Volume ~ Soil_Strength_Before * 
                                   Site + Crab_Mass, 
                                 data = nozero1)

res <- simulateResiduals(Compaction_vs_vol_nonzero1)
plotQQunif(res)

car::Anova(Compaction_vs_vol_nonzero1)
summary(Compaction_vs_vol_nonzero1)

#fitting the curve
visreg(Compaction_vs_vol_nonzero1,
       'Soil_Strength_Before',
       scale = 'response',
       by = 'Site',
       gg = TRUE,
       overlay = TRUE,
       ylab = 'Burrow Volume (mL)',
       xlab = 'Soil Strength (psi)') +
  scale_x_continuous(breaks = c(0,5,10,15,20,25))+
  scale_color_manual("Site", 
                     values=c("NAN"="orangered2",
                              "PIE"="darkblue")) +
  My_theme +
  geom_point(aes(color = Site)) 

#three crab
#interaction between compaction and site with mass as covariable
Compaction_vs_vol_nonzero3 <- glm(Burrow_Volume ~ Soil_Strength_Before * 
                                    Site + Crab_Mass, 
                                  data = nozero3,
                                  family = Gamma(link = "log"))
plot(simulateResiduals(Compaction_vs_vol_nonzero3))
summary(Compaction_vs_vol_nonzero3)
car::Anova(Compaction_vs_vol_nonzero3)

#fit the curve
visreg(Compaction_vs_vol_nonzero3,
       'Soil_Strength_Before',
       scale = 'response',
       by = 'Site',
       gg = TRUE,
       overlay = TRUE,
       ylab = 'Burrow Volume (mL)',
       xlab = 'Soil Strength (psi)') +
  scale_x_continuous(breaks = c(0,5,10,15,20,25))+
  scale_color_manual("Site", values=c("NAN"="orangered2",
                                      "PIE"="darkblue")) +
  My_theme +
  geom_point(aes(color = Site))
