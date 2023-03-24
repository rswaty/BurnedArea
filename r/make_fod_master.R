#Bring in the FPA-FOD (Short) table and create a master table for anlysis.
#The input table was created in Arc Pro by clipping the FOD to CONUS to match LF Conus extent.

library(tidyverse)


### BRING IN DATA for 2011-2020
         
fod1 <- read_csv("in/fod_conus.csv") %>%
  subset(FIRE_YEAR > 2010) 


### TIDY
fod <- fod1 %>%
  select(FIRE_YEAR, FIRE_SIZE) %>%
  group_by(FIRE_YEAR) %>%
  transmute(hectares = ((sum(FIRE_SIZE)) * 0.40468564)) %>% #1ac = 0.40468564ha
  distinct() %>%
  rename(Year = FIRE_YEAR) 


### EXPORT MASTER TABLE
write.csv(fod, "in/fod_master.csv")  
