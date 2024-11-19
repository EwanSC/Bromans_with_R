# Plotting dates, LIRE, 25 stepsize
# Made 19/11/24
# Edited: 19/11/24
## using https://cran.r-project.org/web/packages/datplot/datplot.pdf
## and https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html 
# made to keep comparison of stepsize with 15
library(dplyr)
library(datplot)
library(ggplot2)
library(sqldf)
library(tidyverse)
library(RColorBrewer)

LIRE_Dal_corpus <-
  read.csv("output_tables/corpus/undated/LIRE_corpus.csv")
LIRE_Dal_corpus_place_filtering <-
  read.csv("output_tables/corpus/undated/LIRE_corpus_place_filter.csv")
LIRE_Dal <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia.csv")
LIRE_Dalmatia <-
  read.csv("data/LIRE/LIRE_Dalmatia.csv", na = c("","NA","NULL",NULL))
LIRE_Dal_corpus_epitaph <-
  read.csv("output_tables/corpus/undated/LIRE_corpus_epitaph.csv", na = c("","NA","NULL",NULL))
LIRE_Dal_epitaph <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia_epitaph.csv", na = c("","NA","NULL",NULL))
LIRE_Dal_corpus_stela <-
  read.csv("output_tables/corpus/undated/LIRE_corpus_stela.csv", na = c("","NA","NULL",NULL))
LIRE_Dal_stela <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia_stela.csv", na = c("","NA","NULL",NULL))

count(LIRE_Dal_corpus)
count(LIRE_Dal_corpus_place_filtering)
count(LIRE_Dal)
count(LIRE_Dalmatia)
count(LIRE_Dal_corpus_epitaph)
count(LIRE_Dal_epitaph)
count(LIRE_Dal_corpus_stela)
count(LIRE_Dal_stela)

# distinguish between the two types by adding variable column
LIRE_Dal_corpus$variable <- "Military"
LIRE_Dal_corpus_place_filtering$variable <- "Military"
LIRE_Dal$variable <- "All"
LIRE_Dalmatia$variable <- "All"
LIRE_Dal_corpus_epitaph$variable <- "Military"
LIRE_Dal_epitaph$variable <- "All"
LIRE_Dal_corpus_stela$variable <- "Military"
LIRE_Dal_stela$variable <- "All"

# make function to clean to only required data for aorist analysis
# this includes modifying column classes and removing nulls 

prepare_for_density <- function(dataframe) {
  library(dplyr)
  library(tidyverse)
  prepare_for_density_dates <- na.omit(dataframe %>%
               select(`LIST.ID`,variable,not_before,not_after))
  prepare_for_density_altered <- type.convert(prepare_for_density_dates, as.is = TRUE)    
  prepare_for_density <- na.omit(prepare_for_density_altered)
  return(prepare_for_density)
}

# use function
LIRE_Dal_corpus_na <- prepare_for_density(LIRE_Dal_corpus)
LIRE_Dal_corpus_place_filtering_na <- prepare_for_density(LIRE_Dal_corpus_place_filtering)
LIRE_Dal_na <- prepare_for_density(LIRE_Dal)
LIRE_Dalmatia_na <- prepare_for_density(LIRE_Dalmatia)
LIRE_Dal_corpus_epitaph_na <- prepare_for_density(LIRE_Dal_corpus_epitaph)
LIRE_Dal_epitaph_na <- prepare_for_density(LIRE_Dal_epitaph)
LIRE_Dal_corpus_stela_na <- prepare_for_density(LIRE_Dal_corpus_stela)
LIRE_Dal_stela_na <- prepare_for_density(LIRE_Dal_stela)

LIRE_Dal_corpus_count <- count(LIRE_Dal_corpus_na)
LIRE_Dal_corpus_place_filtering_count <- count(LIRE_Dal_corpus_place_filtering_na)
LIRE_Dal_count <- count(LIRE_Dal_na)
LIRE_Dalmatia_count <- count(LIRE_Dalmatia_na)
LIRE_Dal_corpus_epitaph_count <- count(LIRE_Dal_corpus_epitaph)
LIRE_Dal_epitaph_count <- count(LIRE_Dal_epitaph)
LIRE_Dal_corpus_stela_count <- count(LIRE_Dal_corpus_stela)
LIRE_Dal_stela_count <- count(LIRE_Dal_stela)

# check distribution of monuments using clean
## Following https://static.cambridge.org/content/id/urn:cambridge.org:id:article:S2326376821000085/resource/name/S2326376821000085sup001.pdf
LIRE_Dal_corpus %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10, fill = "#FF8247", colour="#9F522E") +
  labs(title = "Inscription date range",
       subtitle = "Dalmatia: Military, sacral and funerary",
       x = "Length of timespan (10 year bins)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_corpus_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags.\n",
                       "Method = Steinmann & Weissova 2021.")) +
   scale_x_continuous(breaks = seq(0, 600, by = 50))

LIRE_Dal_corpus_place_filtering %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10, fill = "#FF8247", colour="#9F522E") +
  labs(title = "Inscription date range",
       subtitle = "Dalmatia: Military, sacral and funerary",
       x = "Length of timespan (10 year bins)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_corpus_place_filtering_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words, tags, and places.\n",
                       "Method = Steinmann & Weissova 2021."))  +
  scale_x_continuous(breaks = seq(0, 600, by = 50))

LIRE_Dal_corpus_epitaph %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10, fill = "#FF8247", colour="#9F522E") +
  labs(title = "Inscription date range",
       subtitle = "Dalmatia: Military epitaphs",
       x = "Length of timespan (10 year bins)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_corpus_epitaph_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags.\n",
                       "Method = Steinmann & Weissova 2021."))  +
  scale_x_continuous(breaks = seq(0, 600, by = 50))

LIRE_Dal %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10, fill = "#3468d6", colour="#234183") +
  labs(title = "Inscription date range",
       subtitle = "Dalmatia: Sacral and funerary",
       x = "Length of timespan (10 year bins)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Method = Steinmann & Weissova 2021.")) +
  scale_x_continuous(breaks = seq(0, 600, by = 50))

LIRE_Dalmatia %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10, fill = "#3468d6", colour="#234183") +
  labs(title = "Inscription date range",
       subtitle = "Dalmatia: all inscriptions",
       x = "Length of timespan (10 year bins)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dalmatia_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Method = Steinmann & Weissova 2021.")) +
  scale_x_continuous(breaks = seq(0, 600, by = 50))

LIRE_Dal_epitaph %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10, fill = "#3468d6", colour="#234183") +
  labs(title = "Inscription date range",
       subtitle = "Dalmatia: epitaphs",
       x = "Length of timespan (10 year bins)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_epitaph_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Method = Steinmann & Weissova 2021.")) +
  scale_x_continuous(breaks = seq(0, 600, by = 50))

LIRE_Dal_epitaph %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10, fill = "#3468d6", colour="#234183") +
  labs(title = "Inscription date range",
       subtitle = "Dalmatia: epitaphs",
       x = "Length of timespan (10 year bins)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_epitaph_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Method = Steinmann & Weissova 2021.")) +
  scale_x_continuous(breaks = seq(0, 600, by = 50))

# now make histograms using the 'mean'
LIRE_Dal_corpus$DAT_mean <- (LIRE_Dal_corpus$not_after + LIRE_Dal_corpus$not_before) / 2

ggplot(LIRE_Dal_corpus, aes(x = DAT_mean)) +
  geom_histogram(binwidth = 20, position = "dodge", fill = "#FF8247", colour="#9F522E") +
  labs(x = "Date (BCE/CE)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_corpus_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of military funerary and sacral inscriptions",
       subtitle = paste("Using mean",
                        "with a binwidth of 20")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 20))

LIRE_Dal_corpus_place_filtering$DAT_mean <- (LIRE_Dal_corpus_place_filtering$not_after + LIRE_Dal_corpus_place_filtering$not_before) / 2

ggplot(LIRE_Dal_corpus_place_filtering, aes(x = DAT_mean)) +
  geom_histogram(binwidth = 20, position = "dodge", fill = "#FF8247", colour="#9F522E") +
  labs(x = "Date (BCE/CE)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_corpus_place_filtering_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words, tags, and places."),
       title = "Chronological distribution of military funerary and sacral inscriptions",
       subtitle = paste("Using mean",
                        "with a binwidth of 20")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 20))

LIRE_Dal_epitaph$DAT_mean <- (LIRE_Dal_epitaph$not_after + LIRE_Dal_epitaph$not_before) / 2

ggplot(LIRE_Dal_epitaph, aes(x = DAT_mean)) +
  geom_histogram(binwidth = 20, position = "dodge", fill = "#FF8247", colour="#9F522E") +
  labs(x = "Date (BCE/CE)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_epitaph_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of military epitaphs",
       subtitle = paste("Using mean",
                        "with a binwidth of 20")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 20))

LIRE_Dal$DAT_mean <- (LIRE_Dal$not_after + LIRE_Dal$not_before) / 2

ggplot(LIRE_Dal, aes(x = DAT_mean)) +
  geom_histogram(binwidth = 20, position = "dodge", fill = "#3468d6", colour="#234183") +
  labs(x = "Date (BCE/CE)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of all funerary and sacral inscriptions",
       subtitle = paste("Using mean",
                        "with a binwidth of 20")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 20))

LIRE_Dalmatia$DAT_mean <- (LIRE_Dalmatia$not_after + LIRE_Dalmatia$not_before) / 2

ggplot(LIRE_Dalmatia, aes(x = DAT_mean)) +
  geom_histogram(binwidth = 20, position = "dodge", fill = "#3468d6", colour="#234183") +
  labs(x = "Date (BCE/CE)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dalmatia_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of all inscriptions",
       subtitle = paste("Using mean",
                        "with a binwidth of 20")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 20))

LIRE_Dal_epitaph$DAT_mean <- (LIRE_Dal_epitaph$not_after + LIRE_Dal_epitaph$not_before) / 2

ggplot(LIRE_Dal_epitaph, aes(x = DAT_mean)) +
  geom_histogram(binwidth = 20, position = "dodge", fill = "#3468d6", colour="#234183") +
  labs(x = "Date (BCE/CE)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_epitaph_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of epitaphs",
       subtitle = paste("Using mean",
                        "with a binwidth of 20")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 20))

# now for weighted density using datplot and 'clean' dfs
## Work with https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html
LIRE_Dal_corpus_scaled <- scaleweight(datsteps(LIRE_Dal_corpus_na,
                                                     stepsize = 25),
                                            var = "all")

ggplot(data = LIRE_Dal_corpus_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#FF8247")+
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_corpus_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of military funerary and sacral inscriptions",
       subtitle = paste("Using the weighted output of datsteps() ",
                           "with stepsize of ",
                           attributes(LIRE_Dal_corpus_scaled)$stepsize,
                           sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

LIRE_Dal_corpus_place_filtering_scaled <- scaleweight(datsteps(LIRE_Dal_corpus_place_filtering_na,
                                                         stepsize = 25),
                                                var = "all")

ggplot(data = LIRE_Dal_corpus_place_filtering_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#FF8247") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_corpus_place_filtering_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words, tags, and places."),
       title = "Chronological distribution of military funerary and sacral inscriptions",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_corpus_place_filtering_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

LIRE_Dal_corpus_epitaph_scaled <- scaleweight(datsteps(LIRE_Dal_corpus_epitaph_na,
                                                         stepsize = 25),
                                                var = "all")

ggplot(data = LIRE_Dal_corpus_epitaph_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#FF8247") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_corpus_epitaph_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of military epitaphs",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_corpus_epitaph_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

LIRE_Dal_corpus_stela_scaled <- scaleweight(datsteps(LIRE_Dal_corpus_stela_na,
                                                         stepsize = 25),
                                                var = "all")

ggplot(data = LIRE_Dal_corpus_stela_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#FF8247") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_corpus_stela_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of military stelae",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_corpus_stela_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

LIRE_Dal_scaled <- scaleweight(datsteps(LIRE_Dal_na,
                                              stepsize = 25),
                                     var = "all")

ggplot(data = LIRE_Dal_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#3468d6") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of funerary and sacral inscriptions",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

LIRE_Dalmatia_scaled <- scaleweight(datsteps(LIRE_Dalmatia_na,
                                             stepsize = 25),
                                    var = "all")

ggplot(data = LIRE_Dalmatia_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#3468d6") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dalmatia_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of all inscriptions",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dalmatia_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# now for with a histogram
##get histogram 
LIRE_Dal_corpus_scaled_histogramscale <- get.histogramscale(LIRE_Dal_corpus_scaled)
LIRE_Dal_corpus_place_filtering_scaled_histogramscale <- get.histogramscale(LIRE_Dal_corpus_place_filtering_scaled)
LIRE_Dal_scaled_histogram <- get.histogramscale(LIRE_Dal_scaled)
LIRE_Dalmatia_scaled_histogram <- get.histogramscale(LIRE_Dalmatia_scaled)

## plot with histogram
ggplot(LIRE_Dal_corpus_scaled, aes(x = DAT_step)) +
  geom_histogram(alpha = 0.8, binwidth = attributes(LIRE_Dal_corpus_scaled)$stepsize,
                 position = "dodge", colour = "#000000", fill = "#9F522E") +
  stat_density(alpha = 0.8, position = "dodge", colour = "#000000", fill = "#FF8247",
               aes(y = (after_stat(density) * LIRE_Dal_corpus_scaled_histogramscale),
                   weight = weight)) +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_corpus_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of military funerary and sacral inscriptions",
     subtitle = paste("Using the weighted output of datsteps() ",
                      "with stepsize of ",
                      attributes(LIRE_Dal_corpus_scaled)$stepsize,
                      sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggplot(LIRE_Dal_corpus_place_filtering_scaled, aes(x = DAT_step)) +
  geom_histogram(alpha = 0.8, binwidth = attributes(LIRE_Dal_corpus_place_filtering_scaled)$stepsize,
                 position = "dodge", colour = "#000000", fill = "#9F522E") +
  stat_density(alpha = 0.8, position = "dodge", colour = "#000000", fill = "#FF8247",
               aes(y = (after_stat(density) * LIRE_Dal_corpus_place_filtering_scaled_histogramscale),
                   weight = weight)) +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_corpus_place_filtering_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words, tags, and places."),
       title = "Chronological distribution of military funerary and sacral inscriptions",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_corpus_place_filtering_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggplot(LIRE_Dal_scaled, aes(x = DAT_step)) +
  geom_histogram(alpha = 0.8, binwidth = attributes(LIRE_Dal_scaled)$stepsize,
                 position = "dodge", colour = "#000000", fill = "#234183") +
  stat_density(alpha = 0.8, position = "dodge", colour = "#000000", fill = "#3468d6",
               aes(y = (after_stat(density) * LIRE_Dal_scaled_histogram), weight = weight)) +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of funerary and sacral inscriptions",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggplot(LIRE_Dalmatia_scaled, aes(x = DAT_step)) +
  geom_histogram(alpha = 0.8, binwidth = attributes(LIRE_Dalmatia_scaled)$stepsize,
                 position = "dodge", colour = "#000000", fill = "#234183") +
  stat_density(alpha = 0.8, position = "dodge", colour = "#000000", fill = "#3468d6",
               aes(y = (after_stat(density) * LIRE_Dalmatia_scaled_histogram), weight = weight)) +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dalmatia_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of funerary and sacral inscriptions",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dalmatia_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

#now to compare
LIRE_both <- rbind(LIRE_Dal_na, LIRE_Dal_corpus_na)

LIRE_both_scaled <- scaleweight(datsteps(LIRE_both, stepsize = 25),
                                var = "all")

ggplot(data = LIRE_both_scaled, aes(x = DAT_step, weight = weight,
                                          group=variable, fill=variable)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("Military" = "#FF8247", "All" = "#3468d6"),
                    name = "Category") +
  labs(x = "Date (BCE/CE)", y = "Density",
       variable = "Category",
       caption = paste("Military n = ",
                       LIRE_Dal_corpus_count$n,
                       ". All n = ",
                       LIRE_Dal_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of funerary and sacral inscriptions (Density)",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_both_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/chronological_distribution/22.LIRE_Dalmatia_corpus_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_both_place_filtering <- rbind(LIRE_Dal_na, LIRE_Dal_corpus_place_filtering_na)

LIRE_both_place_filtering_scaled <- scaleweight(datsteps(LIRE_both_place_filtering, stepsize = 25),
                                          var = "all")

ggplot(data = LIRE_both_place_filtering_scaled, aes(x = DAT_step, weight = weight,
                                          group=variable, fill=variable)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("Military" = "#FF8247", "All" = "#3468d6"),
                    name = "Category") +
  labs(x = "Date (BCE/CE)", y = "Density",
       variable = "Category",
       caption = paste("Military n = ",
                       LIRE_Dal_corpus_place_filtering_count$n,
                       ". All n = ",
                       LIRE_Dal_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words, tags, and places."),
       title = "Chronological distribution of funerary and sacral inscriptions (Density)",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_both_place_filtering_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/chronological_distribution/23.LIRE_Dalmatia_corpus_place_filter_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)