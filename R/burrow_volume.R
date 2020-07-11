#####SS vs vol Comparison#####

#prep
source("R/load_filter_data.R")
source("R/maketabs.R")
source("R/ggplot_prep.R")
library(visreg) 
library(DHARMa)

#one crab
#interaction between compaction and site with mass as covariable
Compaction_vs_vol_nonzero1 <-glm(Burrow_Volume ~ Soil_Strength_Before * 
                                   Site + Crab_Mass, 
                                 data = nozero1,
                                 family = Gamma(link = "log"))

res <- simulateResiduals(Compaction_vs_vol_nonzero1)
plotQQunif(res)

car::Anova(Compaction_vs_vol_nonzero1)
summary(Compaction_vs_vol_nonzero1)


#fitting the curve
b1 <- visreg(Compaction_vs_vol_nonzero1,
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

b1 <- b1 + scale_y_log10() + ylab('Log10 Burrow Volume (mL)')
b1


#tables
fileConn<-file("tables/burrow_1_anova.html")
print(xtable(car::Anova(Compaction_vs_vol_nonzero1)), type = "html") %>%
  writeLines(fileConn)
close(fileConn)


  

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
b3 <- visreg(Compaction_vs_vol_nonzero3,
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

b3 <- b3 + scale_y_log10() + ylab('Log10 Burrow Volume (mL)')
b3

####
#bringing it all together with patchwork
svg("figures/burrow_plot_fit.svg", width = 10, height = 5)
(b1 + labs(title = "One Crab")) + 
  (b3 + guides(color = "none", fill = "none") + ylab("") + labs(title = "Three Crabs")) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(guides = 'collect')
dev.off()


####
#bringing it all together with patchwork and no fits

f <-  function(x) x + geom_point() +
  scale_x_continuous(breaks = c(0,5,10,15,20,25))+
  scale_color_manual("Site", values=c("NAN"="orangered2",
                                      "PIE"="darkblue")) +
  labs(y = 'Burrow Volume (mL)',
       x = 'Soil Strength (psi)')  +
  My_theme 

### the plot
svg("figures/burrow_plot.svg", width = 10, height = 5)
f(ggplot(nozero1, aes(y = Burrow_Volume, 
                      x = Soil_Strength_Before,
                      color = Site))+ labs(title = "One Crab"))  +

f(ggplot(nozero3, aes(y = Burrow_Volume, 
                    x = Soil_Strength_Before,
                    color = Site)) + 
    guides(color = "none", fill = "none") + 
    ylab("") + labs(title = "Three Crabs")) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(guides = 'collect')
  
dev.off()


#Tables
fileConn<-file("output.txt")

bind_rows(anovatab(Compaction_vs_vol_nonzero1, "One Crab"),
          anovatab(Compaction_vs_vol_nonzero3, "Three Crabs")) %>%
  knitr::kable("html") %>%
  writeLines("tables/burrow_anova.html")

close(fileConn)

#coefs
fileConn<-file("output.txt")

bind_rows(coeftab(Compaction_vs_vol_nonzero1, "One Crab"),
          coeftab(Compaction_vs_vol_nonzero3, "Three Crabs")) %>%
  knitr::kable("html") %>%
  writeLines("tables/burrow_coefs.html")

close(fileConn)
  