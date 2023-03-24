library(tidyverse)
library(scales)

### BRING IN DATA
fod <- read_csv("in/fod_master.csv") 
burnarea <- read_csv("in/burnarea_master.csv") %>%
  rename(Year = DIST_YEAR) 


### BURNED AREA/YEAR

#area burned per year by dataset
burnarea_lf <- burnarea %>%
  group_by(Year) %>%
  subset(DIST_FIRE == "Disturbance-Fire") %>%
  transmute(landfire = sum(hectares)) %>%
  distinct()

burnarea_lba <- burnarea %>%
  group_by(Year) %>%
  subset(LBA == "1") %>%
  transmute(burnedarea = sum(hectares)) %>%
  distinct()

burnarea_fod <- fod %>%
  select(Year, hectares) %>%
  rename(fod = hectares)

#join, pivot long, order datasets for plotting
burnarea_sum <- full_join(burnarea_lba, burnarea_lf)
burnarea_sum <- full_join(burnarea_sum, burnarea_fod)
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
firetype_lf <- burnarea %>%
  subset(DIST_FIRE == "Disturbance-Fire") %>%
  group_by(Year, DIST_GROUP) %>%
  transmute(hectares = sum(hectares)) %>%
  distinct() %>%
  mutate(dataset = "landfire") %>%
  rename(FireType = DIST_GROUP)

# fod
firetype_fod <- burnarea_fod %>%
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
#add row/column totals and % agreement**************
#CT1<-addmargins(table(df1$x1,df1$x2),c(1,2)) 
options(scipen=999)
#prettyNum(input_number, big.mark = “,”, scientific = FALSE)



#xtabs: fire types

#tidy LF data
xtab_fire <- burnarea %>%
  mutate(x_lf = ifelse(DIST_FIRE == "Disturbance-Fire", "LF_Fire", "LF_NoFire")) %>% #reclass LF values
  mutate(x_lba = ifelse(LBA == 1, "LBA_Fire", "LBA_NoFire")) %>% #reclass LBA values 
  select(Count, x_lf, x_lba) %>%
  group_by(x_lf, x_lba) %>%
  tally(Count) %>%
  spread(x_lf, n) 
#row totals
xtab_fire3 <- xtab_fire %>% 
  rowwise() %>% 
  mutate(row_total = sum(c(LF_Fire, LF_NoFire)))
#column totals ############################STILL WORKING ON THIS##################
xtab_fire3 <- xtab_fire3["column_total",]<-colSums(xtab_fire3)

#add margins #should work but dosent


# converting the dataframe to table
my_table <- table(xtab_fire) 
xtab_fire4 <- addmargins(my_table, FUN = sum)     


  columnwise()
  mutate(column_total = sum(c(LBA_Fire, LBA_NoFire)))


xtab_fire2 <- xtab_fire %>%  
  mutate(row_total = ifelse(x_lba == "Fire", sum(Fire), sum("No Fire")))

ifelse(LBA == 1, "Fire", "No Fire"

  mutate(row_total = sum(c("Fire", "No Fire")))

rm(xtab_fire3)
  
rowwise() %>%
  mutate(row_total = sum(c(x_lab$Fire)))
  

mutate(column_totals = sum) %>%
  mutate(column_agreement)
  
  df %>% rowwise() %>% mutate(m = mean(c(x, y, z)))

rowSums



#install.packages("ggcorrplot")                   
library("ggcorrplot")     
ggcorrplot(cor(xtab_fire))   
corrplot(cor(xtab_fire), method = "circle") 



#viz: https://statisticsglobe.com/correlation-matrix-in-r



plot(xtab_fire)
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
