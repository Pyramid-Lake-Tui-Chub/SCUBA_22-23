## EXPLORING SCUBA DATA FOR HYDROACOUSTICS PROCESSING AND SUBSTRATE RECLASSIFICATION FROM 
## WENTWORTH TO CMECS

### INSTALL PACKAGES ----
library(ggplot2)
library(GGally)
library(dplyr)
library(gridExtra)
library(lubridate)
library(cowplot)
library(tidyr)
library(ggridges)
library(tidyverse)
library(hrbrthemes)
library(viridis)

theme_set(theme_cowplot(font_size = 12))
### LOAD DATA ----
setwd("C:\\RCreations\\RProjects\\2023\\SCUBA_22&23")
scuba_master <- read.csv("scuba_master.csv")
scuba_2022 <- read.csv("scuba_2022.csv")
scuba_2023 <-read.csv("scuba_2023.csv")

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

scuba_2023 <- scuba_2023 %>% 
  mutate(date = lubridate::mdy(date))

# PAIRWISE COMPARISON OF SUBSTRATE
substrate_compare <- ggpairs(scuba_master[,c(12:19)]) + theme_bw()
substrate_compare

## VEGETATION COVER BY DEPTH

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
# make depth a factor to use in boxplot
scuba_2023$deep_m_int <- as.integer(scuba_2023$deep_m)
scuba_2023 <- scuba_2023 %>% mutate(depth_bin = case_when (deep_m_int == 0 ~ "0-2 m",
                                                             deep_m_int == 1 ~ "0-2 m",
                                                             deep_m_int == 2 ~ "0-2 m",
                                                             deep_m_int == 3 ~ "3-5 m",
                                                             deep_m_int == 4 ~ "3-5 m",
                                                             deep_m_int == 5 ~ "3-5 m",
                                                             deep_m_int == 6 ~ "6-8 m",
                                                             deep_m_int == 7 ~ "6-8 m",
                                                             deep_m_int == 8 ~ "6-8 m",
                                                             deep_m_int == 9 ~ "9-11 m",
                                                             deep_m_int == 10 ~ "9-11 m",
                                                             deep_m_int == 11 ~ "9-11 m",
                                                             deep_m_int == 12 ~ "12-14 m",
                                                             deep_m_int == 13 ~ "12-14 m",
                                                             deep_m_int == 14 ~ "12-14 m",
                                                             deep_m_int == 15 ~ "15-20 m",
                                                             deep_m_int == 16 ~ "15-20 m",
                                                             deep_m_int == 17 ~ "15-20 m",
                                                             deep_m_int == 18 ~ "15-20 m",
                                                             deep_m_int == 19 ~ "15-20 m",
                                                             deep_m_int == 20 ~ "15-20 m",
                                                             deep_m_int == 21 ~ "15-20 m"))
scuba_2023$depth_bin <- factor(scuba_2023$depth_bin, 
                               levels=c("0-2 m", "3-5 m", "6-8 m", "9-11 m", "12-14 m", "15-20 m"))

veg_cover_2023 <- ggplot(scuba_2023,
                         mapping = aes(x = shallow_m, y = veg_cover)) + 
  scale_x_continuous(n.breaks=18, limits=c(1,20)) +
  geom_point(position="jitter", size = 0.7, alpha = 0.9) +
  labs( x= "Depth (m)", y= "Vegetation Cover (%)") +
  theme_bw()
veg_cover_2023

veg_coverbox_23 <- ggplot(scuba_2023, aes(x=depth_bin, y=veg_cover)) +
  geom_boxplot(width=0.5, fill = "darkslategray4", alpha = 0.5) +
  geom_jitter(size= 0.7, alpha= 0.8, width = 0.25) +
  xlab("")+
  ylab("Vegetation Cover (%)")
veg_coverbox_23

# vegetation starts at 14.6 meters at the earliest

## VEGETATION HEIGHT BY DEPTH
veg_height_2023 <- ggplot(scuba_2023,
                          mapping = aes(x = deep_m, y = veg_avg_height)) + 
  geom_point() +
  scale_x_continuous(n.breaks=20) +
  labs( x= "Depth", y= "Average Vegetation Height") +
  theme_bw()
veg_height_2023

## SUBSTRATE BY DEPTH
# make the categories from CMECS
scuba_2023 <- scuba_2023 %>% mutate(Gravel=bedrock + boulder + cobble + gravel + small_gravel) 
scuba_2023 <- scuba_2023 %>% mutate(SiltClay=silt + hard_clay) 
scuba_2023 <- scuba_2023 %>% mutate(Sand=sand) 

# make a subset dataset so you can do pivot_longer
sub_scub_23 <- subset(scuba_2023, select = c("deep_m", "Gravel", "Sand", "SiltClay"))

# pivot_longer
sub_scub_23 <- sub_scub_23 %>% pivot_longer(!deep_m, names_to = "SedimentClass", values_to = "count")

# depth as integer
sub_scub_23$deep_m_int <- as.integer(sub_scub_23$deep_m)
sub_scub_23 <- sub_scub_23 %>% mutate(depth_bin = case_when (deep_m_int == 0 ~ "0-5 m",
                                                              deep_m_int == 1 ~ "0-5 m",
                                                              deep_m_int == 2 ~ "0-5 m",
                                                              deep_m_int == 3 ~ "0-5 m",
                                                              deep_m_int == 4 ~ "0-5 m",
                                                              deep_m_int == 5 ~ "0-5 m",
                                                              deep_m_int == 6 ~ "6-10 m",
                                                              deep_m_int == 7 ~ "6-10 m",
                                                              deep_m_int == 8 ~ "6-10 m",
                                                              deep_m_int == 9 ~ "6-10 m",
                                                              deep_m_int == 10 ~ "6-10 m",
                                                              deep_m_int == 11 ~ "11-15 m",
                                                              deep_m_int == 12 ~ "11-15 m",
                                                              deep_m_int == 13 ~ "11-15 m",
                                                              deep_m_int == 14 ~ "11-15 m",
                                                              deep_m_int == 15 ~ "11-15 m",
                                                              deep_m_int == 16 ~ "16-20 m",
                                                              deep_m_int == 17 ~ "16-20 m",
                                                              deep_m_int == 18 ~ "16-20 m",
                                                              deep_m_int == 19 ~ "16-20 m",
                                                              deep_m_int == 20 ~ "16-20 m",
                                                             deep_m_int == 21 ~ "16-20 m"))

# order factors
sub_scub_23$depth_bin <- factor(sub_scub_23$depth_bin, levels=c("0-5 m", "6-10 m", "11-15 m", "16-20 m"))
sub_scub_23$SedimentClass <- factor(sub_scub_23$SedimentClass, levels=c("Gravel", "Sand", "SiltClay"))

sub_comp_23 <- ggplot(sub_scub_23, aes(x=depth_bin, y=count, fill=SedimentClass)) +
  geom_boxplot(width=0.5) +
  scale_fill_viridis(discrete = TRUE, alpha=0.4, option = "G") +
  theme(
    legend.position="bottom"
  ) +
  xlab("Depth Range (m)")+
  ylab("Sediment Size Class (%)")
sub_comp_23

#### FIGURE EXPORT ----
# export
setwd("C:\\RCreations\\ROutput\\Vegetation")
png(filename = "ScubeVegByDepth.png", units = "in", width = 8, height = 6, res=600)
veg_coverbox_23
dev.off()

# export
setwd("C:\\RCreations\\ROutput\\Substrate")

png(filename = "ScubaSubstrateByDepth.png", units = "in", width = 8, height = 6, res=600)
sub_comp_23
dev.off()

# combined
setwd("C:\\RCreations\\ROutput\\SCUBA")

png(filename = "ScubaByDepth.png", units = "in", width = 8, height = 6, res=600)
grid.arrange(veg_coverbox_23, sub_comp_23, ncol = 1)
dev.off()



