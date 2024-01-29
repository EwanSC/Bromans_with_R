#following https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html
# date: 29/01/2024

library(datplot)
data(Beazley)

#get mean
Beazley$DAT_mean <- (Beazley$DAT_max + Beazley$DAT_min) / 2

library(ggplot2)

ggplot(Beazley, aes(x = DAT_mean, fill = Technique)) +
  geom_histogram(binwidth = 25, position = "dodge")

# now to create dating steps 
system.time(result <- datsteps(Beazley, stepsize = 25))[3]

system.time(result <- datsteps(Beazley, stepsize = 1))[3]

ggplot(result, aes(x = DAT_step, fill = variable)) +
  geom_histogram(binwidth = 25, position = "dodge")

result <- datsteps(Beazley, stepsize = 25)

dens <- result
dens <- scaleweight(result, var = "all")
dens <- density(x = dens$DAT_step, weights = dens$weight)
plot(dens)

# now to plot the two pottery types separately
result <- scaleweight(result, var = 2)

ggplot(data = result, aes(x = DAT_step,
                          fill = variable,
                          weight = weight)) +
  geom_density(alpha = 0.5) +
  xlab("Dating")

#Now inscriptions:
data("Inscr_Bithynia")

Inscr_Bithynia <- na.omit(Inscr_Bithynia[, c(1, 3, 8, 9)])

result_bith <- scaleweight(datsteps(Inscr_Bithynia, stepsize = "auto"),
                           var = "all")
#step vs weight
#step
ggplot(result_bith, aes(x = DAT_step))+
  geom_density(alpha = 0.5, fill = "grey30") + xlab("Dating")
#weight
ggplot(result_bith, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "grey30") + xlab("Dating")

#adding histograms
histogramscale <- get.histogramscale(result)

ggplot(result, aes(x = DAT_step, fill = variable)) +
  stat_density(alpha = 0.5, position = "dodge",
               aes(y = (after_stat(density) * histogramscale), weight = weight)) +
  geom_histogram(alpha = 0.5, binwidth = attributes(result)$stepsize,
                 position = "dodge") +
  labs(y = "maximum number of objects per year", x = "Dating")


## now try with all Dalmatia inscriptions
#run script #1. first
library(datplot)
library(ggplot2)
library(tidyverse)

Dalmatia <- load_clean_place("data/2022-08-29-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json")

# need to change class of columns for dates as they are currently characters no integer
# following https://statisticsglobe.com/change-classes-data-frame-columns-automatically-r

Inscr_Dalmatia <- na.omit(Dalmatia %>%
                            select(`EDCS-ID`,cleaned_place,dating_to,dating_from))

Inscr_Dalmatia <- type.convert(Inscr_Dalmatia, as.is = TRUE)    # Modify column classes
Inscr_Dalmatia                                        # Print updated data frame

Inscr_Dalmatia %>% 
 rename(
   ID = `EDCS-ID`,
   DAT_min = dating_from,
   DAT_max = dating_to
 )

# run again to remove NAs

Inscr_Dalmatia <- na.omit(Inscr_Dalmatia %>%
                            select(`EDCS-ID`,cleaned_place,dating_to,dating_from))

Inscr_Dalmatia <- type.convert(Inscr_Dalmatia, as.is = TRUE)    # Modify column classes
Inscr_Dalmatia                                        # Print updated data frame

Inscr_Dalmatia %>% 
  rename(
    ID = `EDCS-ID`,
    DAT_min = dating_from,
    DAT_max = dating_to
  )

# if want to change vectors
# sapply(Inscr_Dalmatia, class)
# factor(Inscr_Dalmatia$cleaned_place)

#Inscr_dalm <- is.na(Inscr_dalm)

#Inscr_dalm <- complete.cases(Inscr_dalm)
#Inscr_dalm

result_dalm <- scaleweight(datsteps(Inscr_Dalmatia, "auto"),
                           var = "all")

ggplot(result_dalm, aes(x = DAT_step)) +
  geom_density(alpha = 0.5, fill = "grey30") + xlab("Dating")

ggplot(result_dalm, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "grey30") + xlab("Dating")

ggplot(data = result_dalm, aes(x = DAT_step)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  xlim(-50,400) +
  labs(x = "Date (BCE/CE)", y = "Maximum monument density") +
  ggtitle("Temporal Distribution", subtitle = "density")

ggplot(data = result_dalm, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  xlim(-50,400) +
  labs(x = "Date (BCE/CE)", y = "Maximum monument density") +
  ggtitle("Temporal Distribution", subtitle = "weighted density")

#with histogram

histogramscale <- get.histogramscale(result_dalm)

ggplot(data = result_dalm, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  xlim(-50,400) +
  labs(x = "Date (BCE/CE)", y = "Maximum monument density") +
  ggtitle("Temporal Distribution", subtitle = "Scaled density")

# military 
library(sqldf)
Inscr_Military <- sqldf("Select * from Dalmatia
                  WHERE inscription_interpretive_cleaning 
                    LIKE '%legio%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%cohor%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%ala%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%alae%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%milit%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%eques%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%equit%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%duplicari%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%veteran%'
                  or inscription_interpretive_cleaning
                    LIKE '%centuri%'
                  or inscription_interpretive_cleaning
                    LIKE '%immun%'
                  or inscription_interpretive_cleaning
                    LIKE '%miles%'
                  or inscription_interpretive_cleaning
                    LIKE '%beneficiar%'
                  or inscription_interpretive_cleaning
                    LIKE '%tesserari%'
                  or inscription_interpretive_cleaning
                    LIKE '%signifer%'
                  or inscription_interpretive_cleaning
                    LIKE '%aquilifer%'
                  or inscription_interpretive_cleaning
                    LIKE '%imaginifer%'
                  or inscription_interpretive_cleaning
                    LIKE '%corniculari%'
                  or inscription_interpretive_cleaning
                    LIKE '%principalis%'
                  or inscription_interpretive_cleaning
                    LIKE '%primus pilus%'
                  or inscription_interpretive_cleaning
                    LIKE '%primo pilo%'
                  or inscription_interpretive_cleaning
                    LIKE '%primi pili%'
                  or inscription_interpretive_cleaning
                    LIKE '%praefectus castrorum%'
                  or inscription_interpretive_cleaning
                    LIKE '%optio %'
                  or inscription_interpretive_cleaning
                    LIKE '%option%'
                  OR cleaned_place = 'Tilurium'
                  OR cleaned_place = 'Burnum'
                  OR cleaned_place = 'Andetrium'
                  OR cleaned_place = 'Bigeste'
                  ")

Inscr_Military <- na.omit(Inscr_Military %>%
                            select(`EDCS-ID`,cleaned_place,dating_to,dating_from))

Inscr_Military <- type.convert(Inscr_Military, as.is = TRUE)    # Modify column classes
Inscr_Military                                        # Print updated data frame

Inscr_Military %>% 
  rename(
    ID = `EDCS-ID`,
    DAT_min = dating_from,
    DAT_max = dating_to
  )

result_milit <- scaleweight(datsteps(Inscr_Military, "auto"),
                           var = "all")

ggplot(result_milit, aes(x = DAT_step)) +
  geom_density(alpha = 0.5, fill = "grey30") + xlab("Dating")

ggplot(result_milit, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "grey30") + xlab("Dating")

ggplot(data = result_milit, aes(x = DAT_step)) +
  geom_density(alpha = 0.5, fill = "darkgreen") +
  xlim(-50,400) +
  labs(x = "Date (BCE/CE)", y = "Maximum monument density") +
  ggtitle("Temporal Distribution", subtitle = "density")

ggplot(data = result_milit, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "darkgreen") +
  xlim(-50,400) +
  labs(x = "Date (BCE/CE)", y = "Maximum monument density") +
  ggtitle("'Military' Temporal Distribution", subtitle = "weighted density")

milit_histogramscale <- get.histogramscale(result_milit)
