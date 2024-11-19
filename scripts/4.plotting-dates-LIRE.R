# Plotting dates, LIRE
# Made 16/9/24
# Edited: 19/11/24
## using https://cran.r-project.org/web/packages/datplot/datplot.pdf
## and https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html 
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
LIRE_Dal_votive <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia_votive.csv", na = c("","NA","NULL",NULL))

count(LIRE_Dal_corpus)
count(LIRE_Dal_corpus_place_filtering)
count(LIRE_Dal)
count(LIRE_Dalmatia)
count(LIRE_Dal_corpus_epitaph)
count(LIRE_Dal_epitaph)
count(LIRE_Dal_corpus_stela)
count(LIRE_Dal_stela)
count(LIRE_Dal_votive)

# distinguish between the two types by adding variable column
LIRE_Dal_corpus$variable <- "Military"
LIRE_Dal_corpus_place_filtering$variable <- "Military"
LIRE_Dal$variable <- "All"
LIRE_Dalmatia$variable <- "All"
LIRE_Dal_corpus_epitaph$variable <- "Military"
LIRE_Dal_epitaph$variable <- "All"
LIRE_Dal_corpus_stela$variable <- "Military"
LIRE_Dal_stela$variable <- "All"
LIRE_Dal_votive$variable <- "All"

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
LIRE_Dal_votive_na <- prepare_for_density(LIRE_Dal_votive)

LIRE_Dal_corpus_count <- count(LIRE_Dal_corpus_na)
LIRE_Dal_corpus_place_filtering_count <- count(LIRE_Dal_corpus_place_filtering_na)
LIRE_Dal_count <- count(LIRE_Dal_na)
LIRE_Dalmatia_count <- count(LIRE_Dalmatia_na)
LIRE_Dal_corpus_epitaph_count <- count(LIRE_Dal_corpus_epitaph)
LIRE_Dal_epitaph_count <- count(LIRE_Dal_epitaph)
LIRE_Dal_corpus_stela_count <- count(LIRE_Dal_corpus_stela)
LIRE_Dal_stela_count <- count(LIRE_Dal_stela)
LIRE_Dal_votive_count <- count(LIRE_Dal_votive)

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

ggsave("output_images/chronological_distribution/01.LIRE_corpus_inscription_ranges.jpeg",
       dpi = 600)

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

ggsave("output_images/chronological_distribution/02.LIRE_corpus_place_filter_inscription_ranges.jpeg",
       dpi = 600)

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

ggsave("output_images/chronological_distribution/03.LIRE_corpus_epitaph_inscription_ranges.jpeg",
       dpi = 600)

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

ggsave("output_images/chronological_distribution/04.LIRE_Dalmatia_inscription_ranges.jpeg",
       dpi = 600)

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

ggsave("output_images/chronological_distribution/05.LIRE_Dalmatia_all_types_inscription_ranges.jpeg",
       dpi = 600)

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

ggsave("output_images/chronological_distribution/06.LIRE_Dalmatia_epitaph_inscription_ranges.jpeg",
       dpi = 600)

LIRE_Dal_stela %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10, fill = "#3468d6", colour="#234183") +
  labs(title = "Monument date range",
       subtitle = "Dalmatia: stelae",
       x = "Length of timespan (10 year bins)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_stela_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Method = Steinmann & Weissova 2021.")) +
  scale_x_continuous(breaks = seq(0, 600, by = 50))

ggsave("output_images/chronological_distribution/07.LIRE_Dalmatia_stela_inscription_ranges.jpeg",
       dpi = 600)

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

ggsave("output_images/chronological_distribution/08.LIRE_corpus_mean_histogram.jpeg",
       dpi = 600)

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

ggsave("output_images/chronological_distribution/09.LIRE_corpus_place_filter_mean_histogram.jpeg",
       dpi = 600)

LIRE_Dal_corpus_epitaph$DAT_mean <- (LIRE_Dal_corpus_epitaph$not_after + LIRE_Dal_corpus_epitaph$not_before) / 2

ggplot(LIRE_Dal_corpus_epitaph, aes(x = DAT_mean)) +
  geom_histogram(binwidth = 20, position = "dodge", fill = "#FF8247", colour="#9F522E") +
  labs(x = "Date (BCE/CE)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_corpus_epitaph_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of military epitaphs",
       subtitle = paste("Using mean",
                        "with a binwidth of 20")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 20))

ggsave("output_images/chronological_distribution/10.LIRE_corpus_epitaph_mean_histogram.jpeg",
       dpi = 600)

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

ggsave("output_images/chronological_distribution/11.LIRE_Dalmatia_mean_histogramn.jpeg",
       dpi = 600)

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

ggsave("output_images/chronological_distribution/12.LIRE_Dalmatia_all_types_mean_histogramn.jpeg",
       dpi = 600)

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

ggsave("output_images/chronological_distribution/13.LIRE_Dalmatia_epitaph_mean_histogramn.jpeg",
       dpi = 600)

# now for weighted density using datplot and 'clean' dfs
## Work with https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html
LIRE_Dal_corpus_scaled <- scaleweight(datsteps(LIRE_Dal_corpus_na,
                                                     stepsize = 15),
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

ggsave("output_images/chronological_distribution/14.LIRE_corpus_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_Dal_corpus_place_filtering_scaled <- scaleweight(datsteps(LIRE_Dal_corpus_place_filtering_na,
                                                         stepsize = 15),
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

ggsave("output_images/chronological_distribution/15.LIRE_corpus_place_filter_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_Dal_corpus_epitaph_scaled <- scaleweight(datsteps(LIRE_Dal_corpus_epitaph_na,
                                                         stepsize = 15),
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

ggsave("output_images/chronological_distribution/16.LIRE_corpus_epitaph_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_Dal_corpus_stela_scaled <- scaleweight(datsteps(LIRE_Dal_corpus_stela_na,
                                                         stepsize = 15),
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

ggsave("output_images/chronological_distribution/17.LIRE_corpus_stela_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_Dal_scaled <- scaleweight(datsteps(LIRE_Dal_na,
                                              stepsize = 15),
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

ggsave("output_images/chronological_distribution/18.LIRE_Dalmatia_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_Dalmatia_scaled <- scaleweight(datsteps(LIRE_Dalmatia_na,
                                             stepsize = 15),
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

ggsave("output_images/chronological_distribution/19.LIRE_Dalmatia_all_types_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_Dal_epitaph_scaled <- scaleweight(datsteps(LIRE_Dal_epitaph_na,
                                             stepsize = 15),
                                    var = "all")

ggplot(data = LIRE_Dal_epitaph_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#3468d6") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_epitaph_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of epitaphs",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_epitaph_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/chronological_distribution/20.LIRE_Dalmatia_epitaph_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_Dal_stela_scaled <- scaleweight(datsteps(LIRE_Dal_stela_na,
                                             stepsize = 15),
                                    var = "all")

ggplot(data = LIRE_Dal_stela_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#3468d6") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_stela_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of stelae",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_stela_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/chronological_distribution/21.LIRE_Dalmatia_stela_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

# now for with a histogram
##get histogram 
LIRE_Dal_corpus_scaled_histogramscale <- get.histogramscale(LIRE_Dal_corpus_scaled)
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

ggsave("output_images/chronological_distribution/22.LIRE_corpus_histogram_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

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

ggsave("output_images/chronological_distribution/23.LIRE_Dalmatia_histogram_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

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

ggsave("output_images/chronological_distribution/24.LIRE_Dalmatia_all_types_histogram_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

#now to compare
LIRE_both <- rbind(LIRE_Dal_na, LIRE_Dal_corpus_na)

LIRE_both_scaled <- scaleweight(datsteps(LIRE_both, stepsize = 15),
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

ggsave("output_images/chronological_distribution/25.LIRE_Dalmatia_comparison_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_both_place_filtering <- rbind(LIRE_Dal_na, LIRE_Dal_corpus_place_filtering_na)

LIRE_both_place_filtering_scaled <- scaleweight(datsteps(LIRE_both_place_filtering, stepsize = 15),
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

ggsave("output_images/chronological_distribution/26.LIRE_Dalmatia_place_filter_comparison_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_both_epitaph <- rbind(LIRE_Dal_epitaph_na, LIRE_Dal_corpus_epitaph_na)

LIRE_both_epitaph_scaled <- scaleweight(datsteps(LIRE_both_epitaph, stepsize = 15),
                                          var = "all")

ggplot(data = LIRE_both_epitaph_scaled, aes(x = DAT_step, weight = weight,
                                          group=variable, fill=variable)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("Military" = "#FF8247", "All" = "#3468d6"),
                    name = "Category") +
  labs(x = "Date (BCE/CE)", y = "Density",
       variable = "Category",
       caption = paste("Military n = ",
                       LIRE_Dal_corpus_epitaph_count$n,
                       ". All n = ",
                       LIRE_Dal_epitaph_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of epitaphs (Density)",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_both_epitaph_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/chronological_distribution/27.LIRE_Dalmatia_epitaph_comparison_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_both_stela <- rbind(LIRE_Dal_stela_na, LIRE_Dal_corpus_stela_na)

LIRE_both_stela_scaled <- scaleweight(datsteps(LIRE_both_stela, stepsize = 15),
                                          var = "all")

ggplot(data = LIRE_both_stela_scaled, aes(x = DAT_step, weight = weight,
                                          group=variable, fill=variable)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("Military" = "#FF8247", "All" = "#3468d6"),
                    name = "Category") +
  labs(x = "Date (BCE/CE)", y = "Density",
       variable = "Category",
       caption = paste("Military n = ",
                       LIRE_Dal_corpus_stela_count$n,
                       ". All n = ",
                       LIRE_Dal_stela_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of stelae (Density)",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_both_stela_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/chronological_distribution/28.LIRE_Dalmatia_stela_comparison_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

## now for differences based on type
LIRE_Dalmatia_types <-
  read.csv("data/LIRE/LIRE_Dalmatia.csv", na = c("","NA","NULL",NULL))

LIRE_Dalmatia_types <- LIRE_Dalmatia_types %>%
rename(variable = type_of_inscription_auto)

LIRE_Dalmatia_types_na <- prepare_for_density(LIRE_Dalmatia_types)

LIRE_Dalmatia_types_count <- count(LIRE_Dalmatia_types_na)

LIRE_Dalmatia_types_scaled <- scaleweight(datsteps(LIRE_Dalmatia_types_na,
                                                stepsize = 15),
                                       var = "all")

ggplot(data = LIRE_Dalmatia_types_scaled,
       aes(x = DAT_step, fill = variable, weight = weight)) +
  geom_density(position = "fill", alpha = 0.3) +
  scale_fill_manual(values = c('#000000','#228B22', '#EE0000', '#FFA500', '#6495ED', 
                               '#FF00FF', '#6E8B3D', '#FF6A6A', '#8B5A2B', '#00008B', 
                               '#E066FF','#8B1A1A',  '#9ACD32', '#FFA07A', '#CD853F',
                               "#FFA54F",'#A9A9A9', '#8B7E66', '#DDAD4B', '#7FFF00', 
                               '#54FF9F', '#FFE4B5', '#C71585', '#2E8B57','#98F5FF',
                               "#CAFF70" ,"#EE9A49" ,   "#CD4F39" , "#7CE3D8","#DCDCDC"),
                    name = "Category") +
    labs(x = "Date (BCE/CE)", y = "Relative type density",
       caption = paste("n = ",
                       LIRE_Dalmatia_types_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of all inscription types",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dalmatia_types_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

ggsave("output_images/chronological_distribution/29.LIRE_Dalmatia_types_all_types.jpeg",
       width = 211, height = 120, unit = "mm", dpi = 600)

#now just votive and epitaph and diploma
LIRE_Dalmatia_select_inscr_types <- LIRE_Dalmatia_types %>%
    filter(variable %in% c("epitaph",
                           "votive inscription",
                           "military diploma"))

LIRE_Dalmatia_select_inscr_types_na <- prepare_for_density(LIRE_Dalmatia_select_inscr_types)

LIRE_Dalmatia_select_inscr_types_count <- count(LIRE_Dalmatia_select_inscr_types_na)

LIRE_Dalmatia_select_inscr_types_scaled <- scaleweight(datsteps(LIRE_Dalmatia_select_inscr_types_na,
                                                stepsize = 15),
                                       var = "all")

ggplot(data = LIRE_Dalmatia_select_inscr_types_scaled,
       aes(x = DAT_step, fill = variable, weight = weight)) +
  geom_density(alpha = 0.3) +
  scale_fill_manual(values = c("votive inscription" = "#54FF9F",
                                "epitaph" = '#FFE4B5',
                                "military diploma" = '#EE0000'),
                    name = "Category") +
    labs(x = "Date (BCE/CE)", y = "Relative type density",
       caption = paste("n = ",
                       LIRE_Dalmatia_select_inscr_types_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of votive inscripitions, epitaphs, and diplomas",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dalmatia_select_inscr_types_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

ggsave("output_images/chronological_distribution/30.LIRE_Dalmatia_types_altar_epitaph_diploma.jpeg",
       width = 211, height = 120, unit = "mm", dpi = 600)

#military
LIRE_corpus_types <-
  read.csv("output_tables/corpus/undated/LIRE_corpus_all_types.csv")

LIRE_corpus_types <- LIRE_corpus_types %>%
rename(variable = type_of_inscription_auto)

LIRE_corpus_types_na <- prepare_for_density(LIRE_corpus_types)

LIRE_corpus_types_count <- count(LIRE_corpus_types_na)

LIRE_corpus_types_scaled <- scaleweight(datsteps(LIRE_corpus_types_na,
                                                stepsize = 15),
                                       var = "all")

ggplot(data = LIRE_corpus_types_scaled,
       aes(x = DAT_step, fill = variable, weight = weight)) +
  geom_density(position = "fill", alpha = 0.3) +
  scale_fill_manual(values = c('#000000','#FF8247', '#fcbfa3', '#d6d6d6', '#FF5100', 
                               '#ffc400', '#fde8a1', '#866700', "#DCDCDC"),
                    name = "Category") +
    labs(x = "Date (BCE/CE)", y = "Relative type density",
       caption = paste("n = ",
                       LIRE_corpus_types_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of military inscription types",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_corpus_types_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

ggsave("output_images/chronological_distribution/31.LIRE_corpus_types_all_types.jpeg",
       width = 211, height = 120, unit = "mm", dpi = 600)

# monument types
##remove fountain (weird dates)
LIRE_Dalmatia_monu_types <-
  read.csv("data/LIRE/LIRE_Dalmatia.csv", na = c("","NA","NULL",NULL))

LIRE_Dalmatia_monu_types <- LIRE_Dalmatia_monu_types[!(LIRE_Dalmatia_monu_types$LIST.ID == 475169),]

LIRE_Dalmatia_monu_types <- LIRE_Dalmatia_monu_types %>%
  rename(variable = type_of_monument_clean)

LIRE_Dalmatia_monu_types_na <- prepare_for_density(LIRE_Dalmatia_monu_types)

LIRE_Dalmatia_monu_types_count <- count(LIRE_Dalmatia_monu_types_na)

LIRE_Dalmatia_monu_types_scaled <- scaleweight(datsteps(LIRE_Dalmatia_monu_types_na,
                                                      stepsize = 15),
                                             var = "all")

ggplot(data = LIRE_Dalmatia_monu_types_scaled,
       aes(x = DAT_step, fill = variable, weight = weight)) +
  geom_density(position = "fill", alpha = 0.3) +
  scale_fill_manual(values = c('#000000','#228B22', '#EE0000', '#FFA500', '#6495ED', 
                               '#FF00FF', '#6E8B3D', '#FF6A6A', '#8B5A2B', '#00008B', 
                               '#E066FF','#8B1A1A',  '#9ACD32', '#FFA07A', '#CD853F',
                               "#FFA54F",'#A9A9A9', '#8B7E66', '#DDAD4B', '#7FFF00', 
                               '#54FF9F', '#FFE4B5', '#C71585', '#2E8B57','#98F5FF',
                               "#CAFF70" ,"#EE9A49" ,   "#CD4F39" , "#7CE3D8","#DCDCDC"),
                    name = "Category") +
  labs(x = "Date (BCE/CE)", y = "Relative type density",
       caption = paste("n = ",
                       LIRE_Dalmatia_monu_types_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of all monument types",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dalmatia_monu_types_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

ggsave("output_images/chronological_distribution/32.LIRE_Dalmatia_types_all_monu_types.jpeg",
       width = 211, height = 120, unit = "mm", dpi = 600)

# selection
LIRE_Dalmatia_select_monu_types <- LIRE_Dalmatia_monu_types %>%
  filter(variable %in% c("altar",
                         "stele",
                         "sarcophagus"))

LIRE_Dalmatia_select_monu_types_na <- prepare_for_density(LIRE_Dalmatia_select_monu_types)

LIRE_Dalmatia_select_monu_types_count <- count(LIRE_Dalmatia_select_monu_types)

LIRE_Dalmatia_select_monu_types_scaled <- scaleweight(datsteps(LIRE_Dalmatia_select_monu_types_na,
                                                                stepsize = 15),
                                                       var = "all")

ggplot(data = LIRE_Dalmatia_select_monu_types_scaled,
       aes(x = DAT_step, fill = variable, weight = weight)) +
  geom_density(alpha = 0.3) +
  scale_fill_manual(values = c("altar" = "#54FF9F",
                               "stele" = '#FFE4B5',
                               "sarcophagus" = '#EE0000'),
                    name = "Category") +
    labs(x = "Date (BCE/CE)", y = "Relative type density",
       caption = paste("n = ",
                       LIRE_Dalmatia_select_monu_types_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of altars, stelae, and sarcophagus",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dalmatia_select_monu_types_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

ggsave("output_images/chronological_distribution/33.LIRE_Dalmatia_types_altar_stela_sarcophagus.jpeg",
       width = 211, height = 120, unit = "mm", dpi = 600)

#military
LIRE_corpus_monu_types <-
  read.csv("output_tables/corpus/undated/LIRE_corpus_all_types.csv", na = c("","NA","NULL",NULL))

LIRE_corpus_monu_types <- LIRE_corpus_monu_types[!(LIRE_corpus_monu_types$LIST.ID == 475169),]

LIRE_corpus_monu_types <- LIRE_corpus_monu_types %>%
  rename(variable = type_of_monument_clean)

LIRE_corpus_monu_types_na <- prepare_for_density(LIRE_corpus_monu_types)

LIRE_corpus_monu_types_count <- count(LIRE_corpus_monu_types_na)

LIRE_corpus_monu_types_scaled_15 <- scaleweight(datsteps(LIRE_corpus_monu_types_na,
                                                    stepsize = 15),
                                           var = "all")

ggplot(data = LIRE_corpus_monu_types_scaled_15,
       aes(x = DAT_step, fill = variable, weight = weight)) +
  geom_density(position = "fill", alpha = 0.3) +
  scale_fill_manual(values = c('#000000','#FF8247', '#fcbfa3', '#d6d6d6', '#FF5100', 
                               '#ffc400', '#fde8a1', '#866700', '#FFA500', '#FF6A6A',
                               '#8B5A2B','#8B1A1A','#FFA07A', 
                               "#FFA54F",'#A9A9A9', '#8B7E66', '#DDAD4B',"#EE9A49"
                               ,"#CD4F39" ,"#DCDCDC"),
                    name = "Category") +
  labs(x = "Date (BCE/CE)", y = "Relative type density",
       caption = paste("n = ",
                       LIRE_corpus_monu_types_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of military inscription types",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_corpus_monu_types_scaled_15)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

ggsave("output_images/chronological_distribution/34.LIRE_corpus_all_types_monu_types.jpeg",
       width = 211, height = 120, unit = "mm", dpi = 600)

# what about just votives?
LIRE_Dal_votive_scaled <- scaleweight(datsteps(LIRE_Dal_votive_na,
                                              stepsize = 15),
                                     var = "all")

ggplot(data = LIRE_Dal_votive_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#3468d6") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_votive_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of all votive inscriptions",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_votive_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/chronological_distribution/35.LIRE_Dalmatia_votive_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

# beta: trying to make cumulative graph
## using https://www.geeksforgeeks.org/plotting-cumulative-counts-in-ggplot2-in-r/
##using mean 
LIRE_Dalmatia_cumulative <- LIRE_Dalmatia %>%
  arrange(DAT_mean) %>%
  mutate(cumulative_count = row_number())

ggplot(LIRE_Dalmatia_cumulative, aes(x = DAT_mean, y = cumulative_count)) +
  geom_line(color = "#3468d6", size = 1) +
  labs(x = "Date (BCE/CE)", y = "Cumulative count",
       caption = paste("n = ",
                       LIRE_Dalmatia_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Cumulative count of inscriptions",
       subtitle = paste("Dalmatia: using mean")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25))

ggsave("output_images/chronological_distribution/36.LIRE_Dalmatia_cumulative_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_Dal_epitaph_cumulative <- LIRE_Dal_epitaph %>%
  arrange(DAT_mean) %>%
  mutate(cumulative_count = row_number())

ggplot(LIRE_Dal_epitaph_cumulative, aes(x = DAT_mean, y = cumulative_count)) +
  geom_line(color = "#3468d6", size = 1) +
  labs(x = "Date (BCE/CE)", y = "Cumulative count",
       caption = paste("n = ",
                       LIRE_Dal_epitaph_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Cumulative count of epitaphs",
       subtitle = paste("Dalmatia: using mean")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25))

ggsave("output_images/chronological_distribution/37.LIRE_Dalmatia_epitaph_cumulative_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_Dal_epitaph_corpus_cumulative <- LIRE_Dal_corpus_epitaph %>%
  arrange(DAT_mean) %>%
  mutate(cumulative_count = row_number())

ggplot(LIRE_Dal_epitaph_corpus_cumulative, aes(x = DAT_mean, y = cumulative_count)) +
  geom_line(color = "#FF8247", size = 1) +
  labs(x = "Date (BCE/CE)", y = "Cumulative count",
       caption = paste("n = ",
                       LIRE_Dal_corpus_epitaph_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Cumulative count of military epitaphs",
       subtitle = paste("Dalmatia: using mean")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25))

ggsave("output_images/chronological_distribution/38.LIRE_corpus_epitaph_cumulative_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)
