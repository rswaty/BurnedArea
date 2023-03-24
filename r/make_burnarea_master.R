#Bring in the combined raster tables for LBA and LF tables and create a master table for analysis.
#The input tables were created in Arc Pro by combining LBA and LF annually from 2011-2020. 

library(tidyverse)

### BRING IN DATA
#lf/lba combined raster attribute tables for every year (2011-2020)
list_csv = list.files(path="in/combine_csv_files", full.names=TRUE)
all_csv <- map(list_csv, read_csv)


### MAKE A DF + TIDY
burnarea <- reduce(all_csv, rbind) %>%
  select(-Value, -VALUE_1) %>% #remove unneeded columns
  filter(LF >= 0) %>% #remove values outside US (-9999, -1111) %>%
  mutate(DIST_TYPE = ifelse(LF == 0, "No Disturbance", DIST_TYPE)) %>% #for LF = 0 change DIST_TYPE to No Disturbance
  filter(DIST_TYPE != "Water") #remove water


### MAKE A X-WALK TABLE
#only needed to do this one time, now the table exists and can be brought in at the next step
# lfdist <- burnarea %>%
#   select(DIST_TYPE) %>%
#   distinct() %>%
#   write_csv("in/lookup_lfdist.csv")
#added a DIST_GROUP and DIST_FIRE fields to group values in excel


### JOIN X-WALK TO BURN AREA DF + CALC HECTARES
lookup_lfdist <- read_csv("in/lookup_lfdist.csv") 

burnarea <- left_join(burnarea, lookup_lfdist) %>%
  mutate(hectares = Count * .09) #(30m × 30m = 900m2 = .09 ha)


### EXPORT MASTER TABLE
write.csv(burnarea, "in/burnarea_master.csv")
