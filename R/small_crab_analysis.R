######### Analysis with largeset crabs removed ########
# Load in the Pugnax_Analysis script
source("R/Pugnax_Analysis.R")

##
# Filter out only PIE and remove crabs larger than 9g from
# the three crab treatment (the treatment with larger crabs)
small_PIE <- master3 %>%
  filter(Site == "PIE") %>%
  filter(Crab_Mass <= 7.5)

# Filter our only NAN
small_NAN_first <- master3 %>%
  dplyr::filter(Site == "NAN")

##
# Randomly remove 6 rows from NAN, which is the number
# of crabs larger than 9g from PIE to even the sample
# size
small_NAN <- (small_NAN_first[-as.integer(runif(c(nrow(small_NAN_first) - nrow(small_PIE)), 1, 48)),])

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

# OK, let's just test whether there are still significant differences 
# in crab mass between sites
summary(lm(data = master_small, Crab_Mass ~ Site))

# Nope, how about for the one crab treatment
summary(lm(data = master1, Crab_Mass ~ Site))

# Nope. So, as before, the only significant result was in Site
# Removing the biggest fellas didn't change the analysis

# One last analysis, check to see if mass differed between sites 
# in the three crab treatment
# Nope, how about for the one crab treatment
summary(lm(data = master3, Crab_Mass ~ Site))

# Yes it did, which validates our culling rationale
