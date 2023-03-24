library(tidyverse)
library(scales)
#library(plyr)

### BRING IN DATA

#LF attributes
lf_vat <- read_csv("in/lfdist_attributes.csv")

#lf/lba combined raster attribute tables for every year (2011-2020)
list_csv = list.files(path="in/combine_csv_files", full.names=TRUE)
all_csv <- map(list_csv, read_csv)

#fod            ************************CHECK IN PIVOT
fod <- read_csv("in/fod_conus.csv") %>%
  select(FIRE_YEAR, FIRE_SIZE) %>%
  group_by(FIRE_YEAR) %>%
  transmute(area = sum(FIRE_SIZE)) %>%
  distinct() %>%
  subset(FIRE_YEAR > 2010) %>%
  mutate(fod = area * 0.40468564) %>% #1ac = 0.40468564ha  ###check Short data in Acres and I convert to HA
  select(-area) %>%
  rename(Year = FIRE_YEAR)


### PRE PROCESS DATA TABLES

#put all years in one DF
burnarea <- reduce(all_csv, rbind) 

#remove unneeded columns, calculate ha from cell count (30m × 30m = 900m2 = .09 ha)
burnarea <- burnarea %>% 
  select(-OID_, -Value) %>%
  mutate(hectares = Count*.09)

#join lf attributes
burnarea <- left_join(burnarea, lf_vat, by = c("LF" = "VALUE"))


### BURNED AREA/YEAR

#area burned per year by dataset
burnarea_lf <- burnarea %>%
  group_by(Year) %>%
  subset(DIST_GROUP_FIRE == "Fire") %>%
  transmute(landfire = sum(hectares)) %>%
  distinct()

burnarea_lba <- burnarea %>%
  group_by(Year) %>%
  subset(LBA == "1") %>%
  transmute(burnedarea = sum(hectares)) %>%
  distinct()

#join, pivot long, order datasets for plotting
burnarea_sum <- full_join(burnarea_lba, burnarea_lf)
burnarea_sum <- full_join(burnarea_sum, fod)
burnarea_sum <- burnarea_sum %>%
  pivot_longer(c(landfire, burnedarea, fod), names_to = "dataset", values_to = "hectares") %>%
  mutate(dataset = factor(dataset, levels = c("burnedarea", "landfire", "fod")))

#plot
ggplot(burnarea_sum, aes(x = Year, y = hectares, fill = dataset)) +
  geom_col(position = "dodge") + 
  scale_x_continuous(limits = c(2010, 2021), breaks = 2011:2020) +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  theme_minimal()


### PRESCRIBED V WILDFIRE

# lf
#make a xwalk to group the fire disturbances into wildfire and rx fire
fire_type_lkup <- tibble(DIST_TYPE = c("Wildland Fire", "Wildfire", "Fire", "Prescribed Fire", "Wildland Fire Use"), 
                         FireType = c("Wildfire", "Wildfire", "Wildfire", " Prescribed Fire", "Wildfire"))
#pull out fire disturbances and sum hectares
firetype_lf <- burnarea %>%
  group_by(Year, DIST_TYPE) %>%
  subset(DIST_GROUP_FIRE == "Fire") %>%
  transmute(lf_ha = sum(hectares)) %>%
  distinct()
#join in the fire type xwalk
firetype_lf <- firetype_lf %>%
  left_join(., fire_type_lkup, by = "DIST_TYPE") 
#calculate acres by fire type, add dataset column
firetype_lf <- firetype_lf %>%
  group_by(Year, FireType) %>%
  transmute(hectares = sum(lf_ha)) %>% 
  distinct() %>%
  mutate(dataset = "landfire")

# fod
firetype_fod <- fod %>%
  rename("Wildfire" = "fod") %>%
  mutate("Prescribed Fire" = 0) %>%
  pivot_longer(c("Wildfire", "Prescribed Fire"), names_to = "FireType", values_to = "hectares") %>%
  mutate(dataset = "fod")

#put lf and fod in one table, order for plotting
firetype <- rbind(firetype_lf, firetype_fod) %>%
  mutate(dataset = factor(dataset, levels = c("landfire", "fod")))

#plot
ggplot(data = firetype) +
  aes(x = dataset, y = hectares, fill = FireType) + 
  geom_col() +
  facet_grid(.~Year) +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) 


### CROSS TABS BA V LF 2011-2020
#https://stackoverflow.com/questions/44770902/how-to-convert-data-frame-to-contingency-table-in-r
#use rowwise to calc percents?

# count the hectares or number of cells???????????
#add percent agreement columns????????????


options(scipen=999)
#prettyNum(input_number, big.mark = “,”, scientific = FALSE)

burnarea_xtab_tally <- burnarea %>%
  group_by(LBA, DIST_TYPE) %>%
  tally() %>%
  spread(LBA, n)

#add row/column totals and % agreement**************
#CT1<-addmargins(table(df1$x1,df1$x2),c(1,2)) 

#xtabs: fire types
#look up these values to explain them, e.g. fill vs. no data****************
burnarea_xtab_tally_ha_fire <- burnarea %>%
  group_by(LBA, DIST_GROUP_FIRE) %>%
  tally(hectares) %>%
  spread(LBA, n) %>%
  rename(LBA_burned = "1") %>%
  rename(LBA_notburned = "9999") %>%
  arrange(desc(LBA_burned))

#xtabs: lf dist type for LF = notfire and lba = fire
burnarea_xtab_tally_ha_notfire <- burnarea %>%
  subset(DIST_GROUP_FIRE == "Not Fire") %>%
  group_by(LBA, DIST_GROUP) %>%
  tally(hectares) %>%
  spread(LBA, n) %>%
  rename(LBA_burned = "1") %>%
  rename(LBA_notburned = "9999") %>%
  replace(is.na(.), 0) %>%
  arrange(desc(LBA_burned))
