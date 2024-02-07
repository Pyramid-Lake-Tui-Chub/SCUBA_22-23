## EXPLORING SCUBA DATA FOR HYDROACOUSTICS PROCESSING AND SUBSTRATE RECLASSIFICATION FROM 
## WENTWORTH TO CMECS

### INSTALL PACKAGES ----
library(ggplot2)
library(GGally)
library(dplyr)
library(gridExtra)
library(lubridate)

# DATA MANIPULATION
# convert numerical values from character values (what they come in as)
scuba_master <- scuba_master %>% mutate_at(c("bedrock", "boulder", "cobble", "gravel","small_gravel",
                                             "sand", "silt", "hard_clay", "veg_cover", "veg_1", "veg_2",
                                             "veg_3", "veg_4", "veg_5"), as.numeric)

scuba_2022 <- scuba_2022 %>% mutate_at(c("bedrock", "boulder", "cobble", "gravel","small_gravel",
                                           "sand", "silt", "hard_clay", "veg_cover"), as.numeric)

scuba_2023 <- scuba_2023 %>% mutate_at(c("bedrock", "boulder", "cobble", "gravel","small_gravel",
                                           "sand", "silt", "hard_clay", "veg_cover", "veg_1", "veg_2",
                                           "veg_3", "veg_4", "veg_5"), as.numeric)
#add averages by vegetation height
scuba_2023 <- scuba_2023 %>% mutate(veg_avg_height = rowMeans(cbind(veg_1, veg_2, veg_3, veg_4, veg_5)))
  
# convert date to date
scuba_master <- scuba_master %>% 
                mutate(date = lubridate::ymd(date))

# PAIRWISE COMPARISON OF SUBSTRATE
substrate_compare <- ggpairs(scuba_master[,c(12:19)]) + theme_bw()
substrate_compare

# VEGETATION COVER BY DEPTH

# 2022 
veg_cover_2022 <- ggplot(scuba_2022,
                          mapping = aes(x = deep_m, y = veg_cover)) + 
  scale_x_continuous(n.breaks=20) +
  geom_point(position="jitter") +
  labs( x= "Depth", y= "Vegetation Cover") +
  theme_bw()
veg_cover_2022

## nothing interesting here, basically veg at 10 m and less and not at 20 m

# 2023 
veg_cover_2023 <- ggplot(scuba_2023,
                          mapping = aes(x = shallow_m, y = veg_cover)) + 
  geom_point() +
  scale_x_continuous(n.breaks=20) +
  labs( x= "Depth", y= "Vegetation Cover") +
  theme_bw()
veg_cover_2023

# vegetation starts at 14.6 meters at the earliest

# VEGETATION HEIGHT BY DEPTH
veg_height_2023 <- ggplot(scuba_2023,
                          mapping = aes(x = deep_m, y = veg_avg_height)) + 
  geom_point() +
  scale_x_continuous(n.breaks=20) +
  labs( x= "Depth", y= "Average Vegetation Height") +
  theme_bw()
veg_height_2023



