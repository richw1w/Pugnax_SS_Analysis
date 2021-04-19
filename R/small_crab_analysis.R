######### Analysis with largeset crabs removed ########
# Load in the Pugnax_Analysis script
source("R/Pugnax_Analysis.R")

##
# Filter out only PIE and remove crabs larger than 9g from
# the three crab treatment (the treatment with larger crabs)
small_PIE <- master3 %>%
  filter(Site == "PIE") %>%
  filter(Crab_Mass < 9)

# Filter our only NAN
small_NAN <- master3 %>%
  dplyr::filter(Site == "NAN")

##
# Randomly remove 6 rows from NAN, which is the number
# of crabs larger than 9g from PIE to even the sample
# size
small_NAN <- (small_NAN[-as.integer(runif(6, 1, 48)),])

##
# Join the PIE and NAN small datasets together
master_small <- full_join(small_NAN, small_PIE)

# Stats ####
# Rerun the analysis with three crab treatment
Compaction_small <- glm(Dig~Soil_Strength_Before * 
                            Site + Crab_Mass,
                          data = master_small,
                          family = binomial)
plot(simulateResiduals(Compaction_vs_Dig3))
summary(Compaction_small)

# As before, the only significant result was in Site
# Removing the biggest fellas didn't change the analysis

# Scraps ####
# master_small %>%
#   filter(Site == "NAN") %>%
#   nrow()
# 
# small_NAN <- master_small %>%
#   dplyr::filter(Site == "NAN")# %>%
# dplyr::group_by(Site) %>%
# dplyr::mutate(numbering = row_number()) 
# small_NAN %>%
#   dplyr::filter(numbering != as.integer(runif(6, 1, 48))) #%>%
#   !is.na()
# max(master1$Crab_Mass)
# max(master_fidd$Crab_Mass)
# 
# summary(arsenal::comparedf(master1, master_fidd))
# 
# df <- data.frame(a = 1:46)
# 
# df[-rnorm(1:48, 6),]
# df[-as.integer(runif(6, 1, 48)),]

   