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

LIRE_Dal_corpus <-
  read.csv("output_tables/corpus/LIRE_corpus.csv")

count(LIRE_Dal_corpus)

LIRE_Dal_corpus$variable <- "Military"

LIRE_Dal_corpus_dates <- na.omit(LIRE_Dal_corpus %>%
                                   select(`LIST.ID`,variable,not_before,not_after))

LIRE_Dal_corpus_altered <- type.convert(LIRE_Dal_corpus_dates, as.is = TRUE)    # Modify column classes
LIRE_Dal_corpus_na <- na.omit(LIRE_Dal_corpus_altered)  #remove nulls
LIRE_Dal_corpus_na                                      # Print updated data frame

# check distribution of monuments using clean
## Following https://static.cambridge.org/content/id/urn:cambridge.org:id:article:S2326376821000085/resource/name/S2326376821000085sup001.pdf
LIRE_Dal_corpus %>%
  select(`LIST.ID`,variable,not_before,not_after) %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10) +
  labs(title = "Number of Inscriptions Dated to Different Timespans",
       x = "Lenght of Timespan (10 year bins)", y = "Number of Inscriptions",
       caption = "Following Steinmann & Weissova 2021, Supplementary document")

# now for density using clean
## Work with https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html
## try stepsize 1, 15, and 25
LIRE_Dal_corpus_scaled <- scaleweight(datsteps(LIRE_Dal_corpus_na, stepsize = "auto"),
                                            var = "all")

LIRE_Dal_corpus_scaled_5 <- scaleweight(datsteps(LIRE_Dal_corpus_na, stepsize = 5),
                                              var = "all")

LIRE_Dal_corpus_scaled_15 <- scaleweight(datsteps(LIRE_Dal_corpus_na, stepsize = 15),
                                               var = "all")

LIRE_Dal_corpus_scaled_25 <- scaleweight(datsteps(LIRE_Dal_corpus_na, stepsize = 25),
                                               var = "all")

## now plot KDE
ggplot(data = LIRE_Dal_corpus_scaled, aes(x = DAT_step)) +
  geom_density(alpha = 0.5, fill = "darkorange")+
  labs(x = "Date (BCE/CE)", y = "Density") +
  ggtitle("Temporal Distribution (Unweighted. Stepsize = 1)") +
  xlim(-50, 350)

ggsave("output_images/chronological_distribution/testing_stepsize_and_weight/01.unweighted_1.jpeg",
       dpi = 600)

ggplot(data = LIRE_Dal_corpus_scaled_5, aes(x = DAT_step)) +
  geom_density(alpha = 0.5, fill = "darkorange")+
  labs(x = "Date (BCE/CE)", y = "Density") +
  ggtitle("Temporal Distribution (Unweighted. Stepsize = 5)") +
  xlim(-50, 350)

ggsave("output_images/chronological_distribution/testing_stepsize_and_weight/03.unweighted_5.jpeg",
       dpi = 600)

ggplot(data = LIRE_Dal_corpus_scaled_15, aes(x = DAT_step)) +
  geom_density(alpha = 0.5, fill = "darkorange")+
  labs(x = "Date (BCE/CE)", y = "Density") +
  ggtitle("Temporal Distribution (Unweighted. Stepsize = 15)") +
  xlim(-50, 350)

ggsave("output_images/chronological_distribution/testing_stepsize_and_weight/05.unweighted_15.jpeg",
       dpi = 600)

ggplot(data = LIRE_Dal_corpus_scaled_25, aes(x = DAT_step)) +
  geom_density(alpha = 0.5, fill = "darkorange")+
  labs(x = "Date (BCE/CE)", y = "Density") +
  ggtitle("Temporal Distribution (Unweighted. Stepsize = 25)") +
  xlim(-50, 350)

ggsave("output_images/chronological_distribution/testing_stepsize_and_weight/07.unweighted_25.jpeg",
       dpi = 600)

ggplot(data = LIRE_Dal_corpus_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  labs(x = "Date (BCE/CE)", y = "Density") +
  ggtitle("Temporal Distribution (Weighted. Stepsize = 1)") +
  xlim(-50, 350)

ggsave("output_images/chronological_distribution/testing_stepsize_and_weight/02.weighted_1.jpeg",
       dpi = 600)

ggplot(data = LIRE_Dal_corpus_scaled_5, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  labs(x = "Date (BCE/CE)", y = "Density") +
  ggtitle("Temporal Distribution (Weighted. Stepsize = 5)") +
  xlim(-50, 350)

ggsave("output_images/chronological_distribution/testing_stepsize_and_weight/04.weighted_5.jpeg",
       dpi = 600)

ggplot(data = LIRE_Dal_corpus_scaled_15, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  labs(x = "Date (BCE/CE)", y = "Density") +
  ggtitle("Temporal Distribution (Weighted. Stepsize = 15)") +
  xlim(-50, 350)

ggsave("output_images/chronological_distribution/testing_stepsize_and_weight/06.weighted_15.jpeg",
       dpi = 600)

ggplot(data = LIRE_Dal_corpus_scaled_25, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  labs(x = "Date (BCE/CE)", y = "Density") +
  ggtitle("Temporal Distribution (Weighted. Stepsize = 25)") +
  xlim(-50, 350)

ggsave("output_images/chronological_distribution/testing_stepsize_and_weight/08.weighted_25.jpeg",
       dpi = 600)

## Concl.
### going with a) weighted and b) stepsize of 15 and 25
### stepsize 1 and 5 too small (skews histogram)

LIRE_Dal_corpus_histogramscale <- get.histogramscale(LIRE_Dal_corpus_scaled)
LIRE_Dal_corpus_histogramscale_5 <- get.histogramscale(LIRE_Dal_corpus_scaled_5)
LIRE_Dal_corpus_histogramscale_15 <- get.histogramscale(LIRE_Dal_corpus_scaled_15)
LIRE_Dal_corpus_histogramscale_25 <- get.histogramscale(LIRE_Dal_corpus_scaled_25)

ggplot(LIRE_Dal_corpus_scaled, aes(x = DAT_step)) +
  stat_density(alpha = 0.5, position = "dodge", colour = "black", fill = "darkorange",
               aes(y = (..density.. * LIRE_Dal_corpus_histogramscale), weight = weight)) +
  geom_histogram(alpha = 0.5, binwidth = attributes(LIRE_Dal_corpus_scaled)$stepsize,
                 position = "dodge", colour = "black", fill = "darkorange4") +
  labs(y = "Maximum number of monuments per year", x = "Dating (BCE/CE") +
  ggtitle("Temporal Distribution (Weighted. Stepsize = 1)") +
  xlim(-50, 350)

ggsave("output_images/chronological_distribution/testing_stepsize_and_weight/09.weighted_histogram_1.jpeg",
       dpi = 600)

ggplot(LIRE_Dal_corpus_scaled_5, aes(x = DAT_step)) +
  stat_density(alpha = 0.5, position = "dodge", colour = "black", fill = "darkorange",
               aes(y = (..density.. * LIRE_Dal_corpus_histogramscale_5), weight = weight)) +
  geom_histogram(alpha = 0.5, binwidth = attributes(LIRE_Dal_corpus_scaled_5)$stepsize,
                 position = "dodge", colour = "black", fill = "darkorange4") +
  labs(y = "Maximum number of monuments per year", x = "Dating (BCE/CE") +
  ggtitle("Temporal Distribution (Weighted. Stepsize = 5)") +
  xlim(-50, 350)

ggsave("output_images/chronological_distribution/testing_stepsize_and_weight/10.weighted_histogram_5.jpeg",
       dpi = 600)

ggplot(LIRE_Dal_corpus_scaled_15, aes(x = DAT_step)) +
  stat_density(alpha = 0.5, position = "dodge", colour = "black", fill = "darkorange",
               aes(y = (..density.. * LIRE_Dal_corpus_histogramscale_15), weight = weight)) +
  geom_histogram(alpha = 0.5, binwidth = attributes(LIRE_Dal_corpus_scaled_15)$stepsize,
                 position = "dodge", colour = "black", fill = "darkorange4") +
  labs(y = "Maximum number of monuments per year", x = "Dating (BCE/CE") +
  ggtitle("Temporal Distribution (Weighted. Stepsize = 15)") +
  xlim(-50, 350)

ggsave("output_images/chronological_distribution/testing_stepsize_and_weight/11.weighted_histogram_15.jpeg",
       dpi = 600)

ggplot(LIRE_Dal_corpus_scaled_25, aes(x = DAT_step)) +
  stat_density(alpha = 0.5, position = "dodge", colour = "black", fill = "darkorange",
               aes(y = (..density.. * LIRE_Dal_corpus_histogramscale_25), weight = weight)) +
  geom_histogram(alpha = 0.5, binwidth = attributes(LIRE_Dal_corpus_scaled_25)$stepsize,
                 position = "dodge", colour = "black", fill = "darkorange4") +
  labs(y = "Maximum number of monuments per year", x = "Dating (BCE/CE") +
  ggtitle("Temporal Distribution (Weighted. Stepsize = 25)") +
  xlim(-50, 350)

ggsave("output_images/chronological_distribution/testing_stepsize_and_weight/12.weighted_histogram_25.jpeg",
       dpi = 600)

## Concl.
### going with 15 and 25 seems appropriate

## final product
ggplot(LIRE_Dal_corpus_scaled_15, aes(x = DAT_step)) +
  stat_density(alpha = 0.5, position = "dodge", colour = "black", fill = "darkorange",
               aes(y = (..density.. * LIRE_Dal_corpus_histogramscale_15), weight = weight)) +
  geom_histogram(alpha = 0.5, binwidth = attributes(LIRE_Dal_corpus_scaled_15)$stepsize,
                 position = "dodge", colour = "black", fill = "darkorange4") +
  labs(y = "Maximum number of monuments per year", x = "Dating (BCE/CE") +
  ggtitle("Temporal Distribution (Weighted. Stepsize = 15)") +
  xlim(-50, 350)

ggsave("output_images/chronological_distribution/testing_stepsize_and_weight/12.final_weighted_histogram_15.jpeg",
       dpi = 600)

ggplot(LIRE_Dal_corpus_scaled_25, aes(x = DAT_step)) +
  stat_density(alpha = 0.5, position = "dodge", colour = "black", fill = "darkorange",
               aes(y = (..density.. * LIRE_Dal_corpus_histogramscale_25), weight = weight)) +
  geom_histogram(alpha = 0.5, binwidth = attributes(LIRE_Dal_corpus_scaled_25)$stepsize,
                 position = "dodge", colour = "black", fill = "darkorange4") +
  labs(y = "Maximum number of monuments per year", x = "Dating (BCE/CE") +
  ggtitle("Temporal Distribution (Weighted. Stepsize = 25)") +
  xlim(-50, 350)

ggsave("output_images/chronological_distribution/testing_stepsize_and_weight/13.final_weighted_histogram_25.jpeg",
       dpi = 600)
