library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)
library(ggmap)
library(rworldmap)
library(arules)
library(ggpubr)
library(car)
library(caret)
library(zoo)
library(ggfortify)

#Import Data
# GTdata <- read_csv("C:/Users/Vincy/Desktop/Data Science/project/globalterrorismdb_0718dist.csv")
GTdata <- read.csv("D:\\Pitt_2\\Data Science\\gtd\\globalterrorismdb_0718dist.csv")
View(GTdata)

# GDPdata <- read_csv("C:/Users/Vincy/Desktop/Data Science/project/GDP.csv")
GDPdata <- read.csv("D:\\Pitt_2\\Data Science\\API_NY.GDP.MKTP.CD_DS2_en_csv_v2_713242\\GDP.csv")
View(GDPdata)

# MilEXPdata <- read_csv("C:/Users/Vincy/Desktop/Data Science/project/MLE.csv")
MilEXPdata <- read.csv("D:\\Pitt_2\\Data Science\\API_MS.MIL.XPND.CD_DS2_en_csv_v2_716872\\MLE.csv")
View(MilExpData)

# MilEGdata <- read_csv("C:/Users/Vincy/Desktop/Data Science/project/MLE_G.csv")
MilEGdata <- read.csv("D:\\Pitt_2\\Data Science\\API_MS.MIL.XPND.GD.ZS_DS2_en_csv_v2_712990\\MLE_G.csv")
View(MilEGdata)

glimpse(GTdata)

names(GTdata)
names(GDPdata)
names(MilEXPdata)
names(MilEGdata)

GDPdata <- GDPdata %>%
  rename(
    Country_Name = ï..Country.Name,
    Country_Code = Country.Code,
    Indicator_Name = Indicator.Name,
    Indicator_Code = Indicator.Code
  )

MilEXPdata <- MilEXPdata %>%
  rename(
    Country_Name = ï..Country.Name,
    Country_Code = Country.Code,
    Indicator_Name = Indicator.Name,
    Indicator_Code = Indicator.Code
  )

MilEGdata <- MilEGdata %>%
  rename(
    Country_Name = ï..Country.Name,
    Country_Code = Country.Code,
    Indicator_Name = Indicator.Name,
    Indicator_Code = Indicator.Code
  )

## Data Cleaning: removing unnecessary variables & renaming some variables. We are going to focus for the sake of keeping
## our analysis clear on a subset of variables

GTdata_sub <- GTdata %>%
  select(iyear, imonth, iday, country_txt, region_txt, city, latitude, longitude, summary, multiple, attacktype1_txt, 
         targtype1_txt, targsubtype1_txt, gname, weaptype1_txt, nkill, nwound, nkillter)

GTdata_sub <- GTdata_sub %>% 
  rename(year = iyear, month = imonth, day = iday, country = country_txt, 
         region = region_txt, multiple_attack = multiple, attacktype = attacktype1_txt, target_type = targtype1_txt, 
         target_sub_type = targsubtype1_txt, group_name = gname, weapon_type = weaptype1_txt)

GTdata_sub <- GTdata_sub %>%
  mutate(decade = 
           ifelse(year<1980, '70s', 
                  ifelse(year < 1990, '80s', 
                         ifelse(year < 2000, '90s', 
                                ifelse( year < 2010, '2000s', '2010s')))))

GTdata_sub$decade <- factor(GTdata_sub$decade, levels=c("70s", "80s", "90s", "2000s", "2010s"))

## Data Overview of Global Terrorism Database

# Number of Terrorist Attacks
ggplot(data = GTdata_sub, aes(x = year)) + 
  geom_histogram(stat = 'count') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = 'Terrorism attacks over time')

GTdata_sub %>% 
  summarise(nr_of_attacks = n())

# Attack type Distribution
ggplot(data = GTdata_sub, aes(x = attacktype)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_histogram(stat = 'count') + 
  labs(title = 'Terrorism attack type distribution')

# Target distribution
ggplot(data = GTdata_sub, aes(x = target_type, fill=decade)) +
  geom_histogram(stat='count') +
  theme(axis.text.x= element_text(angle=45, hjust=1)) + 
  labs(title='Target distribution of terrorism over time')

GTdata_sub %>%
  group_by(target_type) %>%
  summarise(nr_of_attacks = n()) %>%
  arrange(desc(nr_of_attacks)) %>%
  head(n=10)

# location of terrorism
GTdata_sub_2000 <- GTdata_sub %>% 
  filter(year > 2000)

world <- borders("world", colour = "gray50", fill = "gray50")
worldmap <- ggplot() + world + scale_y_continuous(limits=c(-55, 90))
worldmap + 
  geom_point(aes(x = GTdata_sub_2000$longitude[GTdata_sub$nkill<51], y = GTdata_sub_2000$latitude[GTdata_sub$nkill<51]), col='blue', alpha= 0.2) +
  geom_point(aes(x = GTdata_sub_2000$longitude[GTdata_sub$nkill>50], y = GTdata_sub_2000$latitude[GTdata_sub$nkill>50]), col='red', size=2) +
  labs(title='Location of terrorist attacks by severity')

GTdata_sub %>%
  group_by(region) %>%
  summarise(nr_of_attacks = n()) %>%
  arrange(desc(nr_of_attacks)) %>%
  head(n=10)
GTdata_sub %>%
  group_by(country) %>%
  summarise(nr_of_attacks = n()) %>%
  arrange(desc(nr_of_attacks)) %>%
  head(n=10)
GTdata_sub %>%
  filter(city != 'Unknown') %>%
  group_by(city) %>%
  summarise(nr_of_attacks = n()) %>%
  arrange(desc(nr_of_attacks)) %>%
  head(n=10)

## Check the distribution of terrorist groups over the world
## Reduce the number of data: group by decade and only take the top 10000 points of each decade. 
## For visibility the set is further filtered based on having more than 300 attacks.

GTdata_sub_500 <- GTdata_sub %>% 
  select(decade, latitude, longitude, group_name) %>% 
  group_by(decade) %>%
  slice(1:10000)
GTdata_sub_500 <- GTdata_sub_500 %>% 
  group_by(group_name) %>% 
  filter(n() >= 300 & group_name != 'unknown')

worldmap + 
  geom_point(aes(x = GTdata_sub_500$longitude, y = GTdata_sub_500$latitude, col = GTdata_sub_500$group_name), size=2, position = 'jitter') +
  labs(title='Location of terrorist attacks by group') +
  theme(legend.position=c(0.5, -0.5))

## Distribution of terrorist groups
top10_groups <- GTdata_sub %>%
  filter(group_name != "Unknown") %>%
  group_by(group_name) %>%
  summarise(nr_of_attacks = n()) %>%
  arrange(desc(nr_of_attacks)) %>%
  head(n=10)

ggplot(data=top10_groups) +
  stat_summary(aes(x=group_name, y=nr_of_attacks), geom="bar") +
  theme(axis.text.x= element_text(angle=45, hjust=1)) +
  labs(title='Terrorist attacks per group')

### Trends in Terrorism
# Terrorism growth
GTdata_sub %>% 
  group_by(decade) %>% 
  summarise(nr_of_attacks = n()) %>% 
  arrange(desc(nr_of_attacks))
# visualization
ggplot(data = GTdata_sub, aes(x = year, fill = decade)) + 
  geom_histogram(stat = 'count') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = 'Terrorism growth over time')

## Process GDP, Military Expenditure 
GDPdata <- select(GDPdata, -c(5:14, 64))
MilEXPdata <- select(MilEXPdata, -c(5:14, 64))
MilEGdata <- select(MilEGdata, -c(5:14, 64))

yearGDP <- c()
for (i in 1:49) {
  yearGDP[i] <- sum(na.omit(GDPdata[,4+i]))
}

for (i in 3:8) {
  GTdata_pairs[, i] <- as.numeric(as.character(GTdata_pairs[, i], na.rm = T))
}
