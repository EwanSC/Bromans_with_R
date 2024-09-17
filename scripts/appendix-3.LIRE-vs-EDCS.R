#plotting dates edcs vs lire
## get packages
library(datplot)
library(ggplot2)
library(sqldf)

## run scripts/1.primary_epigraphic_cor(e)pus_functions and 2.scripts/primary_map 
# and 4.primary_epigraphic_cor(e)pus-LIRE.R (or working version)
## then, deploy function to get and clean df:

# LIRE_Dal_mil <- filter(LIRE_filtered, province == "Dalmatia")
#not working, run working-primary_epigraphic_cor(e)pus-LIRE first
# use LIRE_Dal_mil

LIRE_military_unclean <- LIRE_Dal_mil

EDCS_cleaned_place <- load_clean_place("data/2022-08-29-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json")

EDCS_military_nulls <- sqldf("Select * from EDCS_cleaned_place
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

#first EDCS
# need to change class of columns for dates as they are currently characters no integer
# following https://statisticsglobe.com/change-classes-data-frame-columns-automatically-r

EDCS_military_short <- na.omit(EDCS_military_nulls %>%
                                  select(`EDCS-ID`,cleaned_place,dating_to,dating_from))


EDCS_military_new <- type.convert(EDCS_military_short, as.is = TRUE)    # Modify column classes
EDCS_military <- na.omit(EDCS_military_new)
EDCS_military                                      # Print updated data frame

# now to try date using https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html
EDCS_military_new$date_mean <- (EDCS_military_new$dating_from + EDCS_military_new$dating_to) / 2
ggplot(EDCS_military_new, aes(x = date_mean), fill = date_mean) +
  labs(y = 'Monuments', x = 'Date (BCE/CE)') +
  ggtitle("Temporal distribution of monuments", subtitle = "Mean date") +
  geom_histogram(binwidth = 10, position = "dodge") 

# now for density (not working..) Work with https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html
system.time(EDCS_military_steps <- datsteps(EDCS_military, stepsize = 1))[3]

system.time(EDCS_military_steps <- datsteps(EDCS_military, stepsize = 25))[3]

EDCS_military_steps <- datsteps(EDCS_military, stepsize = 1)

ggplot(EDCS_military_steps, aes(x = DAT_step)) +
  geom_histogram(binwidth = 25, position = "dodge")

EDCS_military_dens <- datsteps(EDCS_military, stepsize = 5)
dens <- EDCS_military_dens
dens <- scaleweight(EDCS_military_dens, var = "all")
dens <- density(x = dens$DAT_step, weights = dens$weight)
plot(dens)

EDCS_military_scale <- scaleweight(EDCS_military_dens, var = 2)

# with weight variable
ggplot(data = EDCS_military_scale, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  labs(x = "Date (BCE/CE)") +
  ggtitle("Temporal Distribution", subtitle = "Scaled weighted density")


# without weight variable
ggplot(data = EDCS_military_scale, aes(x = DAT_step)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  labs(x = "Date (BCE/CE)", y = "Maximum monument density") +
  ggtitle("Temporal Distribution", subtitle = "Scaled density")

# now to combine

histogramscale <- get.histogramscale(EDCS_military_scale)

ggplot(EDCS_military_scale, aes(x = DAT_step)) +
  stat_density(alpha = 0.5, position = "dodge", fill = "darkorange",
               aes(y = (..density.. * histogramscale), weight = weight)) +
  geom_histogram(alpha = 0.5, binwidth = attributes(EDCS_military_scale)$stepsize,
                 position = "dodge", fill = "darkgrey") +
  labs(y = "Maximum number of monuments per year", x = "Dating (BCE/CE") +
  ggtitle("Temporal Distribution", subtitle = "Scaled density and mean date")


ggplot(EDCS_military_scale, aes(x = DAT_step)) +
  stat_density(alpha = 0.5, position = "dodge", fill = "darkorange",
               aes(y = (..density.. * histogramscale))) +
  geom_histogram(alpha = 0.5, binwidth = attributes(EDCS_military_scale)$stepsize,
                 position = "dodge", fill = "darkgrey") +
  labs(y = "Maximum number of monuments per year", x = "Dating (BCE/CE") +
  ggtitle("Temporal Distribution", subtitle = "Scaled density and histogram")

### now LIRE
LIRE_military_short <- na.omit(LIRE_military_unclean %>%
                                 select(`EDCS-ID`,cleaned_place,not_after,not_before))


LIRE_military_new <- type.convert(LIRE_military_short, as.is = TRUE)    # Modify column classes
LIRE_military <- na.omit(LIRE_military_new)
LIRE_military                                      # Print updated data frame

# now to try date using https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html
LIRE_military_new$date_mean <- (LIRE_military_new$not_after + LIRE_military_new$not_before) / 2
ggplot(LIRE_military_new, aes(x = date_mean), fill = date_mean) +
  labs(y = 'Monuments', x = 'Date (BCE/CE)') +
  ggtitle("Temporal distribution of monuments", subtitle = "Mean date") +
  geom_histogram(binwidth = 10, position = "dodge") 

# now for density (not working..) Work with https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html
system.time(LIRE_military_steps <- datsteps(LIRE_military, stepsize = 1))[3]

system.time(LIRE_military_steps <- datsteps(LIRE_military, stepsize = 25))[3]

LIRE_military_steps <- datsteps(LIRE_military, stepsize = 1)

ggplot(LIRE_military_steps, aes(x = DAT_step)) +
  geom_histogram(binwidth = 25, position = "dodge")

LIRE_military_dens <- datsteps(LIRE_military, stepsize = 5)
dens <- LIRE_military_dens
dens <- scaleweight(LIRE_military_dens, var = "all")
dens <- density(x = dens$DAT_step, weights = dens$weight)
plot(dens)

LIRE_military_scale <- scaleweight(LIRE_military_dens, var = 2)

# with weight variable
ggplot(data = LIRE_military_scale, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  labs(x = "Date (BCE/CE)") +
  ggtitle("Temporal Distribution", subtitle = "Scaled weighted density")


# without weight variable
ggplot(data = LIRE_military_scale, aes(x = DAT_step)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  labs(x = "Date (BCE/CE)", y = "Maximum monument density") +
  ggtitle("Temporal Distribution", subtitle = "Scaled density")

# now to combine

histogramscale <- get.histogramscale(LIRE_military_scale)

ggplot(LIRE_military_scale, aes(x = DAT_step)) +
  stat_density(alpha = 0.5, position = "dodge", fill = "darkorange",
               aes(y = (..density.. * histogramscale), weight = weight)) +
  geom_histogram(alpha = 0.5, binwidth = attributes(LIRE_military_scale)$stepsize,
                 position = "dodge", fill = "darkgrey") +
  labs(y = "Maximum number of monuments per year", x = "Dating (BCE/CE") +
  ggtitle("Temporal Distribution", subtitle = "Scaled density and mean date")


ggplot(LIRE_military_scale, aes(x = DAT_step)) +
  stat_density(alpha = 0.5, position = "dodge", fill = "darkorange",
               aes(y = (..density.. * histogramscale))) +
  geom_histogram(alpha = 0.5, binwidth = attributes(LIRE_military_scale)$stepsize,
                 position = "dodge", fill = "darkgrey") +
  labs(y = "Maximum number of monuments per year", x = "Dating (BCE/CE") +
  ggtitle("Temporal Distribution", subtitle = "Scaled density and histogram")

# Compare the pair
##EDCS
# with weight variable
ggplot(data = EDCS_military_scale, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  labs(x = "Date (BCE/CE)") +
  ggtitle("EDCS: Temporal Distribution", subtitle = "Scaled weighted density")

ggsave("output_images/temp-dist-EDCS-1.jpeg", dpi = 300)

ggplot(data = EDCS_military_scale, aes(x = DAT_step)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  labs(x = "Date (BCE/CE)", y = "Maximum monument density") +
  ggtitle("EDCS: Temporal Distribution", subtitle = "Scaled density")

ggsave("output_images/temp-dist-EDCS-2.jpeg", dpi = 300)

##LIRE
ggplot(data = LIRE_military_scale, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  labs(x = "Date (BCE/CE)") +
  ggtitle("LIRE Data: Temporal Distribution", subtitle = "Scaled weighted density")

ggsave("output_images/temp-dist-LIRE-1.jpeg", dpi = 300)

ggplot(data = LIRE_military_scale, aes(x = DAT_step)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  labs(x = "Date (BCE/CE)", y = "Maximum monument density") +
  ggtitle("LIRE Data: Temporal Distribution", subtitle = "Scaled density")

ggsave("output_images/temp-dist-LIRE-2.jpeg", dpi = 300)

#not to combine into one plot

ggplot(data = LIRE_military_scale, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  labs(x = "Date (BCE/CE)") +
  ggtitle("LIRE Data: Temporal Distribution", subtitle = "Scaled weighted density")

# add column https://sparkbyexamples.com/r-programming/add-column-to-dataframe-in-r

LIRE_military_scale_LIRE <- LIRE_military_scale %>% 
  add_column(dataset = "LIRE")

EDCS_military_scale_EDCS <- EDCS_military_scale %>% 
  add_column(dataset = "EDCS")

#combine
combined_military_scale <- rbind(EDCS_military_scale_EDCS, LIRE_military_scale_LIRE)

#plot https://stackoverflow.com/questions/6939136/how-to-overlay-density-plots-in-r
ggplot(combined_military_scale, aes(x = DAT_step, fill = dataset, weight = weight)) +
  geom_density(alpha = 0.5) +
  labs(x = "Date (BCE/CE)") +
  ggtitle("Temporal Distribution", subtitle = "Scaled weighted density")

ggsave("output_images/temp-dist-LIRE-EDCS.jpeg", dpi = 300)
