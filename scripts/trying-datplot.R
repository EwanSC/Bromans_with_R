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

ggplot(data = result_dalm, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  xlim(-50,150) +
  labs(x = "Date (BCE/CE)", y = "Maximum monument density") +
  ggtitle("50 BCE-150 CE Distribution", subtitle = "weighted density")

#with 25 step size
result_dalm_25 <- scaleweight(datsteps(Inscr_Dalmatia, 25),
                           var = "all")

ggplot(data = result_dalm_25, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  xlim(-50,400) +
  labs(x = "Date (BCE/CE)", y = "Monument density") +
  ggtitle("Temporal Distribution", subtitle = "Scaled density with 25 stepsize")

#with histogram

histogramscale25 <- get.histogramscale(result_dalm_25)

ggplot(result_dalm_25, aes(x = DAT_step)) +
  geom_histogram(alpha = 1, fill = "darkorange4", binwidth = attributes(result_dalm_25)$stepsize,
                 position = "dodge") +
  stat_density(alpha = 0.5, position = "dodge", color = "black", fill = "darkorange",
               aes(y = (after_stat(density) * histogramscale25), weight = weight)) +
  xlim(-50,400) +
  labs(y = "Maximum number of monuments", x = "Date (BCE/CE)")

ggsave("output_images/epigraphic-distribution-dalmatia.jpeg", dpi = 300)

# military with step size 25 and scale
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
                  or status
                    LIKE '%milites%'
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

# run again, dont know why...
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

result_milit_25 <- scaleweight(datsteps(Inscr_Military, 25),
                            var = "all")

ggplot(data = result_milit, aes(x = DAT_step)) +
  geom_density(alpha = 0.5, fill = "chartreuse3") +
  xlim(-50,400) +
  labs(x = "Date (BCE/CE)", y = "Maximum monument density") +
  ggtitle("Temporal Distribution", subtitle = "density")

ggplot(data = result_milit, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "chartreuse3") +
  xlim(-50,400) +
  labs(x = "Date (BCE/CE)", y = "Maximum monument density") +
  ggtitle("'Military' Temporal Distribution", subtitle = "weighted density")

ggplot(data = result_milit, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "chartreuse3") +
  xlim(-50,150) +
  labs(x = "Date (BCE/CE)", y = "Maximum monument density") +
  ggtitle("'Military' Temporal Distribution", subtitle = "weighted density")

# now step 25
ggplot(data = result_milit_25, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "chartreuse3") +
  xlim(-50,400) +
  labs(x = "Date (BCE/CE)", y = "monument density") +
  ggtitle("'Military' Epigraphy Distribution", subtitle = "Scaled density with 25 stepsize")

ggplot(data = result_milit_25, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "chartreuse3") +
  xlim(-50,150) +
  labs(x = "Date (BCE/CE)", y = "monument density") +
  ggtitle("'Military' Epigraphy Distribution", subtitle = "Scaled density with 25 stepsize")

# now histogram

milit_histogramscale_25 <- get.histogramscale(result_milit_25)

ggplot(result_milit_25, aes(x = DAT_step)) +
  geom_histogram(alpha = 1, fill = "darkgreen", binwidth = attributes(result_milit_25)$stepsize,
                 position = "dodge") +
  stat_density(alpha = 0.5, position = "dodge", color = "black", fill = "chartreuse3",
               aes(y = (after_stat(density) * milit_histogramscale_25), weight = weight)) +
  xlim(-50,400) +
  labs(y = "Maximum number of monuments", x = "Date (BCE/CE)")

ggsave("output_images/epigraphic-distribution-dalmatia-military.jpeg", dpi = 300)

ggplot(result_milit_25, aes(x = DAT_step)) +
  geom_histogram(alpha = 1, fill = "darkgreen", binwidth = attributes(result_milit_25)$stepsize,
                 position = "dodge") +
  stat_density(alpha = 0.5, position = "dodge", color = "black", fill = "chartreuse3",
               aes(y = (after_stat(density) * milit_histogramscale_25), weight = weight)) +
  xlim(-50,150) +
  labs(y = "Maximum number of monuments", x = "Date (BCE/CE)")

# smaller scales historgram
result_milit_10 <- scaleweight(datsteps(Inscr_Military, 10),
                                              var = "all")

milit_histogramscale_10 <- get.histogramscale(result_milit_10)

ggplot(data = result_milit_10, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "chartreuse3") +
  xlim(-50,150) +
  labs(x = "Date (BCE/CE)", y = "monument density") +
  ggtitle("'Military' Epigraphy Distribution", subtitle = "Scaled density with 10 stepsize")

ggplot(result_milit_10, aes(x = DAT_step)) +
  geom_histogram(alpha = 1, fill = "darkgreen", binwidth = attributes(result_milit_10)$stepsize,
                 position = "dodge") +
  stat_density(alpha = 0.5, position = "dodge", color = "black", fill = "chartreuse3",
               aes(y = (after_stat(density) * milit_histogramscale_10), weight = weight)) +
  xlim(-50,150) +
  labs(y = "Maximum number of monuments", x = "Date (BCE/CE)") +
  ggtitle("'Military' Epigraphy Distribution", subtitle = "Scaled density and histogram with 10 stepsize")

# combine military and non using (*still not scaling correctly...)
#https://stackoverflow.com/questions/21192002/how-to-combine-2-plots-ggplot-into-one-plot?rq=3
Inscr_Dalmatia$variable<- "All"
Inscr_Military$variable<- "Military"

Inscr_Dal_Mil <- rbind(Inscr_Dalmatia, Inscr_Military)

Inscr_Dal_Mil <- Inscr_Dal_Mil[, c(1, 5, 3, 4, 2)]

Inscr_Dal_Mil_25 <- scaleweight(datsteps(Inscr_Dal_Mil, 25),
                               var = "all")

ggplot(data = Inscr_Dal_Mil_25, aes(x = DAT_step, weight=weight, group=variable, color=variable, fill=variable)) +
  geom_density(alpha = 0.5) +
  xlim(-50,400) +
  labs(x = "Date (BCE/CE)", y = "Monument density") +
  ggtitle("Temporal Distribution", subtitle = "Scaled density with 25 stepsize")
