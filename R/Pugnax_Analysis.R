##########Installing Packages################
library(readr) #installing the package to read excel files
library(ggthemes)# install.packages('ggthemes')
library(ggplot2)# install.packages('ggplot2')
library(dplyr)
library(DHARMa)
library(Rmisc)
library(ggbeeswarm) #install.packages('ggbeeswarm')
library(visreg) #install.packages('visreg')

#####Creating a theme#####
My_theme = theme_bw() +
  theme(axis.title.x = element_text(size = 22, face = "bold"),
        axis.text.x = element_text(size = 19),
        axis.title.y = element_text(size = 22, face = "bold"),
        axis.text.y = element_text(size = 19),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20))

My_color = scale_color_manual("Site", values=c("NAN"="orangered2",
                                             "PIE"="darkblue"))

#####Data#####
master_fidd <- read.csv("data/Complete_mud_fidd_data.csv")

#Treatments
master3 <- master_fidd %>%
  filter (Crab_Treatment_Num > 2)

master1 <- master_fidd %>%
  filter (Crab_Treatment_Num < 2)

#filter out zeros
master_nozero <- master_fidd %>% 
  filter(Burrow_Volume > 0)

#Treatments
nozero3 <- master_nozero %>%
  filter (Crab_Treatment_Num > 2)

nozero1 <- master_nozero %>%
  filter (Crab_Treatment_Num < 2)

#####NAN Data#####
NAN_master_fidd <- dplyr::filter(master_fidd, Site %in% "NAN")
str(NAN_master_fidd)

#Treatments
NAN_master3 <- NAN_master_fidd %>%
  filter (Crab_Treatment_Num > 2)

NAN_master1 <- NAN_master_fidd %>%
  filter (Crab_Treatment_Num < 2)

#Filter Out Zeros
NAN_nozero <- NAN_master_fidd %>% 
  filter(Burrow_Volume > 0)

#Treatments
NAN_3crab <- NAN_nozero %>%
  filter (Crab_Treatment_Num > 2)

NAN_1crab <- NAN_nozero %>%
  filter (Crab_Treatment_Num < 2)

#####PIE Data#####
PIE_master_fidd <- dplyr::filter(master_fidd, Site %in% "PIE")
str(PIE_master_fidd)

#treatments
PIE_master3 <- PIE_master_fidd %>%
  filter (Crab_Treatment_Num > 2)

PIE_master1 <- PIE_master_fidd %>%
  filter (Crab_Treatment_Num < 2)

#Filter Out Zeros
PIE_nozero <- PIE_master_fidd %>% 
  filter(Burrow_Volume > 0)

#Treatments
PIE_3crab <- PIE_nozero %>%
  filter (Crab_Treatment_Num > 2)

PIE_1crab <- PIE_nozero %>%
  filter (Crab_Treatment_Num < 2)

#####SS vs vol Comparison#####
#one crab
#interaction between compaction and site with mass as covariable
Compaction_vs_vol_nonzero1 <-glm(Burrow_Volume ~ Soil_Strength_Before * 
                                   Site + Crab_Mass, 
                         data = nozero1)

plot(simulateResiduals(Compaction_vs_vol_nonzero1))
summary(Compaction_vs_vol_nonzero1)
car::Anova(Compaction_vs_vol_nonzero1)

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
  My_theme

#three crab
#interaction between compaction and site with mass as covariable
Compaction_vs_vol_nonzero3 <- glm(Burrow_Volume ~ Soil_Strength_Before * 
                                    Site + Crab_Mass, 
                                  data = nozero3)
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
  My_theme


########Probability of Creating a Burrow Comparison####
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
  My_theme +
  ggsave("figures/burrow_plot_fit_one_crab.jpeg")

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
  My_theme  +
  ggsave("figures/burrow_plot_fit_three_crab.jpeg")

######SS before and after#####
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

#########Beeswarm plot for mass########
ggplot(master_fidd,
       aes(Site, Crab_Mass))+
  geom_quasirandom(aes(color = Site))+
  ylab('Crab Mass(g)')+
  scale_color_manual(aesthetics = "Site", 
                     values=c("NAN"="orangered2",
                              "PIE"="darkblue"))+
  My_theme 




