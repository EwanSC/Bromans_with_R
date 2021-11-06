# Ewan Trying out R Code for Cleaning Epigraphic Data
# Created by: EC 2021/11/05
# Last Edit: EC 2021/11/05

# first, lets load packages

library(tidyverse) #installing tidyvery

library(RColorBrewer) #installing colour package for plot, used by DAACS

library(ggplot2)

AllDalmatiaEpig <- read.csv('whole_dalmatia_epigr_scrape.csv') # Importing our first dataframe (df)

str(AllDalmatiaEpig) # lets have a look at the structure of the data

head(AllDalmatiaEpig) # what are the first 6 rows

AllDalmatiaByDate <- AllDalmatiaEpig %>% #makes a df arranged by date
  arrange(dating.from, dating.to)

AllDalmatiaPlace <- AllDalmatiaEpig %>% #makes a df that provides counts of distinct places
  group_by(place) %>%
  count(place) %>%
  arrange(desc(n)) 

# make a sample df that only has places with 30+ inscriptions
DPSampleThirty <- AllDalmatiaPlace[which(AllDalmatiaPlace$n >= 30),]

# bar plot attempt with angled x-axis names for the list of plces with 30+ inscriptions
ggplot(DPSampleThirty, aes(x=place, y=n, fill=place)) +
  geom_bar(stat="identity", show.legend=F, width = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# thats still a lot of values so lets try 50+
DPSampleFifty <- AllDalmatiaPlace[which(AllDalmatiaPlace$n >= 50),]

# bar plot attempt with angled x-axis names for the list of plces with 50+ inscriptions
ggplot(DPSampleFifty, aes(x=place, y=n, fill=place)) +
  geom_bar(stat="identity", show.legend=F, width = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# lets make a bar plot that removes the noise of Salona (5000ish inscriptions gets in the way)
# to do so I will make a df that has a range between 30-1000 and then make this into a barplot
DPSampleThirtyToThousand <- AllDalmatiaPlace %>%
  filter(n %in% (30:1000))

theme_set(theme_bw(base_size = 10))
ggplot(DPSampleThirtyToThousand, aes(x=place, y=n, fill=place)) +
  geom_bar(stat="identity", show.legend=F, width = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
        