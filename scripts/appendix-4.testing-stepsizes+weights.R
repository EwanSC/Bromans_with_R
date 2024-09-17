# Weight and stepsize testing
# created 16/9/2024
# edited16/9/2024
## using https://cran.r-project.org/web/packages/datplot/datplot.pdf
## and https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html 
## see also https://doi.org/10.1017/aap.2021.8
library(datplot)
library(ggplot2)
library(sqldf)
library(tidyverse)

count(LIRE_Dal_corpus)
count(LIRE_Dal_corpus_clean)

LIRE_Dal_corpus_dates <- na.omit(LIRE_Dal_corpus %>%
                                   select(`LIST-ID`,province_label_clean,not_before,not_after))

LIRE_Dal_corpus_clean_dates <- na.omit(LIRE_Dal_corpus_clean %>%
                                         select(`LIST-ID`,province_label_clean,not_before,not_after))

LIRE_Dal_corpus_altered <- type.convert(LIRE_Dal_corpus_dates, as.is = TRUE)    # Modify column classes
LIRE_Dal_corpus_na <- na.omit(LIRE_Dal_corpus_altered)  #remove nulls
LIRE_Dal_corpus_na                                      # Print updated data frame

LIRE_Dal_corpus_clean_altered <- type.convert(LIRE_Dal_corpus_clean_dates, as.is = TRUE)    # Modify column classes
LIRE_Dal_corpus_clean_na <- na.omit(LIRE_Dal_corpus_clean_altered) #remove nulls
LIRE_Dal_corpus_clean_na                                           # Print updated data frame

# check distribution of monuments using clean
## Following https://static.cambridge.org/content/id/urn:cambridge.org:id:article:S2326376821000085/resource/name/S2326376821000085sup001.pdf
LIRE_Dal_corpus_clean %>%
  select(`LIST-ID`,province_label_clean,not_before,not_after) %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10) +
  labs(title = "Number of Inscriptions Dated to Different Timespans",
       x = "Lenght of Timespan (10 year bins)", y = "Number of Inscriptions",
       caption = "Following Steinmann & Weissova 2021, Supplementary document")

#ggsave

# now for density using clean
## Work with https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html
## try stepsize 1, 15, and 25
LIRE_Dal_corpus_clean_scaled <- scaleweight(datsteps(LIRE_Dal_corpus_clean_na, stepsize = "auto"),
                                            var = "all")

LIRE_Dal_corpus_clean_scaled_5 <- scaleweight(datsteps(LIRE_Dal_corpus_clean_na, stepsize = 5),
                                              var = "all")

LIRE_Dal_corpus_clean_scaled_15 <- scaleweight(datsteps(LIRE_Dal_corpus_clean_na, stepsize = 15),
                                               var = "all")

LIRE_Dal_corpus_clean_scaled_25 <- scaleweight(datsteps(LIRE_Dal_corpus_clean_na, stepsize = 25),
                                               var = "all")

ggplot(data = LIRE_Dal_corpus_clean_scaled, aes(x = DAT_step)) +
  geom_density(alpha = 0.5, fill = "darkorange")+
  labs(x = "Date (BCE/CE)", y = "Density") +
  labs(caption = "Based on data from LIRE v.3.0 (cleaned province = Dalmatia)") +
  ggtitle("Temporal Distribution (864 military monuments)", subtitle = "Scaled density (Unweighted. Stepsize = 1)")

ggplot(data = LIRE_Dal_corpus_clean_scaled_5, aes(x = DAT_step)) +
  geom_density(alpha = 0.5, fill = "darkorange")+
  labs(x = "Date (BCE/CE)", y = "Density") +
  labs(caption = "Based on data from LIRE v.3.0 (cleaned province = Dalmatia)") +
  ggtitle("Temporal Distribution (864 military monuments)", subtitle = "Scaled density (Unweighted. Stepsize = 5)")

ggplot(data = LIRE_Dal_corpus_clean_scaled_15, aes(x = DAT_step)) +
  geom_density(alpha = 0.5, fill = "darkorange")+
  labs(x = "Date (BCE/CE)", y = "Density") +
  labs(caption = "Based on data from LIRE v.3.0 (cleaned province = Dalmatia)") +
  ggtitle("Temporal Distribution (864 military monuments)", subtitle = "Scaled density (Unweighted. Stepsize = 15)")

ggplot(data = LIRE_Dal_corpus_clean_scaled_25, aes(x = DAT_step)) +
  geom_density(alpha = 0.5, fill = "darkorange")+
  labs(x = "Date (BCE/CE)", y = "Density") +
  labs(caption = "Based on data from LIRE v.3.0 (cleaned province = Dalmatia)") +
  ggtitle("Temporal Distribution (864 military monuments)", subtitle = "Scaled density (Unweighted. Stepsize = 25)")

ggplot(data = LIRE_Dal_corpus_clean_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  labs(x = "Date (BCE/CE)", y = "Density") +
  labs(caption = "Based on data from LIRE v.3.0 (cleaned province = Dalmatia)") +
  ggtitle("Temporal Distribution (864 military monuments)", subtitle = "Scaled density (Weighted. Stepsize = 1)")

ggplot(data = LIRE_Dal_corpus_clean_scaled_5, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  labs(x = "Date (BCE/CE)", y = "Density") +
  labs(caption = "Based on data from LIRE v.3.0 (cleaned province = Dalmatia)") +
  ggtitle("Temporal Distribution (864 military monuments)", subtitle = "Scaled density (Weighted. Stepsize = 5)")

ggplot(data = LIRE_Dal_corpus_clean_scaled_15, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  labs(x = "Date (BCE/CE)", y = "Density") +
  labs(caption = "Based on data from LIRE v.3.0 (cleaned province = Dalmatia)") +
  ggtitle("Temporal Distribution (864 military monuments)", subtitle = "Scaled density (Weighted. Stepsize = 15)")

ggplot(data = LIRE_Dal_corpus_clean_scaled_25, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  labs(x = "Date (BCE/CE)", y = "Density") +
  labs(caption = "Based on data from LIRE v.3.0 (cleaned province = Dalmatia)") +
  ggtitle("Temporal Distribution (864 military monuments)", subtitle = "Scaled density (Weighted. Stepsize = 25)")

LIRE_Dal_corpus_clean_histogramscale <- get.histogramscale(LIRE_Dal_corpus_clean_scaled)
LIRE_Dal_corpus_clean_histogramscale_5 <- get.histogramscale(LIRE_Dal_corpus_clean_scaled_5)
LIRE_Dal_corpus_clean_histogramscale_15 <- get.histogramscale(LIRE_Dal_corpus_clean_scaled_15)
LIRE_Dal_corpus_clean_histogramscale_25 <- get.histogramscale(LIRE_Dal_corpus_clean_scaled_25)

ggplot(LIRE_Dal_corpus_clean_scaled, aes(x = DAT_step)) +
  stat_density(alpha = 0.5, position = "dodge", colour = "black", fill = "darkorange",
               aes(y = (..density.. * LIRE_Dal_corpus_clean_histogramscale), weight = weight)) +
  geom_histogram(alpha = 0.5, binwidth = attributes(LIRE_Dal_corpus_clean_scaled)$stepsize,
                 position = "dodge", colour = "black", fill = "darkorange4") +
  labs(y = "Maximum number of monuments per year", x = "Dating (BCE/CE") +
  labs(caption = "Based on data from LIRE v.3.0 (Cleaned province = Dalmatia)") +
  ggtitle("Temporal Distribution (864 military monuments)", subtitle = "Scaled density and histogram (Weighted. Stepsize = 1)")

ggplot(LIRE_Dal_corpus_clean_scaled_5, aes(x = DAT_step)) +
  stat_density(alpha = 0.5, position = "dodge", colour = "black", fill = "darkorange",
               aes(y = (..density.. * LIRE_Dal_corpus_clean_histogramscale_5), weight = weight)) +
  geom_histogram(alpha = 0.5, binwidth = attributes(LIRE_Dal_corpus_clean_scaled_5)$stepsize,
                 position = "dodge", colour = "black", fill = "darkorange4") +
  labs(y = "Maximum number of monuments per year", x = "Dating (BCE/CE") +
  labs(caption = "Based on data from LIRE v.3.0 (Cleaned province = Dalmatia)") +
  ggtitle("Temporal Distribution (864 military monuments)", subtitle = "Scaled density and histogram (Weighted. Stepsize = 5)")

ggplot(LIRE_Dal_corpus_clean_scaled_15, aes(x = DAT_step)) +
  stat_density(alpha = 0.5, position = "dodge", colour = "black", fill = "darkorange",
               aes(y = (..density.. * LIRE_Dal_corpus_clean_histogramscale_15), weight = weight)) +
  geom_histogram(alpha = 0.5, binwidth = attributes(LIRE_Dal_corpus_clean_scaled_15)$stepsize,
                 position = "dodge", colour = "black", fill = "darkorange4") +
  labs(y = "Maximum number of monuments per year", x = "Dating (BCE/CE") +
  labs(caption = "Based on data from LIRE v.3.0 (Cleaned province = Dalmatia)") +
  ggtitle("Temporal Distribution (864 military monuments)", subtitle = "Scaled density and histogram (Weighted. Stepsize = 15)")

ggplot(LIRE_Dal_corpus_clean_scaled_25, aes(x = DAT_step)) +
  stat_density(alpha = 0.5, position = "dodge", colour = "black", fill = "darkorange",
               aes(y = (..density.. * LIRE_Dal_corpus_clean_histogramscale_25), weight = weight)) +
  geom_histogram(alpha = 0.5, binwidth = attributes(LIRE_Dal_corpus_clean_scaled_25)$stepsize,
                 position = "dodge", colour = "black", fill = "darkorange4") +
  labs(y = "Maximum number of monuments per year", x = "Dating (BCE/CE") +
  labs(caption = "Based on data from LIRE v.3.0 (Cleaned province = Dalmatia)") +
  ggtitle("Temporal Distribution (864 military monuments)", subtitle = "Scaled density and histogram (Weighted. Stepsize = 25)")

## Concl.
### going with a) weighted and b) stepsize or 15
### stepsize 1 and 5 too small (skews histogram), stepsize 25 too large 