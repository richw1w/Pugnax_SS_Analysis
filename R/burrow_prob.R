########Probability of Creating a Burrow Comparison####
#prep
source("R/load_filter_data.R")
source("R/ggplot_prep.R")
source("R/maketabs.R")
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
p1 <- visreg(Compaction_vs_Dig1,
             rug = FALSE,
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
  My_theme + geom_point(data = master1, alpha = 0.8,
                          position = position_jitter(height = 0.03, width = 0.4),
                          aes(y = Dig, x = Soil_Strength_Before, color = Site)) 


p1 

#three crab
Compaction_vs_Dig3 <- glm(Dig~Soil_Strength_Before * 
                            Site + Crab_Mass,
                          data = master3,
                          family = binomial)
plot(simulateResiduals(Compaction_vs_Dig3))
summary(Compaction_vs_Dig3)
car::Anova(Compaction_vs_Dig3)

#fit the curve
p3 <- visreg(Compaction_vs_Dig3,
             rug = FALSE,
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
  My_theme + geom_point(data = master3, alpha = 0.8,
                      position = position_jitter(height = 0.03, width = 0.4),
                      aes(y = Dig, x = Soil_Strength_Before, color = Site))

p3

#bringing it all together with patchwork
svg("figures/probability_plot.svg", width = 15, height = 10)
(p1 + labs(title = "One Crab")) + 
  (p3 + guides(color = "none", fill = "none") + ylab("") + labs(title = "Three Crabs")) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(guides = 'collect') +
  ggsave("figures/combined_prob.jpeg",  width = 12, height = 7) # Uncomment out when needing to save again
dev.off()


#Tables ####
fileConn<-file("output.txt")

bind_rows(anovatab(Compaction_vs_Dig1, "One Crab"),
          anovatab(Compaction_vs_Dig3, "Three Crabs")) %>%
  table_styling %>%
  writeLines("tables/probability_anodev.html")

close(fileConn)

#coefs
fileConn<-file("output.txt")

bind_rows(coeftab(Compaction_vs_Dig1, "One Crab"),
          coeftab(Compaction_vs_Dig3, "Three Crabs")) %>%
  table_styling %>%
  writeLines("tables/probability_coefs.html")

close(fileConn)


#r2

fileConn<-file("output.txt")

bind_rows(rsq_tabl(Compaction_vs_Dig1, "One Crab"),
          rsq_tabl(Compaction_vs_Dig3, "Three Crabs")) %>%
  table_styling %>%
  writeLines("tables/probability_r2.html")

close(fileConn)

