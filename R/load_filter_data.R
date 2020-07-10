#####Data#####
library(readr)
library(dplyr)
library(tidyr)

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
#str(NAN_master_fidd)

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
#str(PIE_master_fidd)

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