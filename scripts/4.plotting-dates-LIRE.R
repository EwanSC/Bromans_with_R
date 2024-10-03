# Plotting dates, LIRE
# Made 16/9/24
# Edited: 16/9/24
### FIRST RUN /scripts/2.corpus-LIRE.R
## using https://cran.r-project.org/web/packages/datplot/datplot.pdf
## and https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html 
library(dplyr)
library(datplot)
library(ggplot2)
library(sqldf)
library(tidyverse)

count(LIRE_Dal_corpus)
count(LIRE_Dal_corpus_no_place_filtering)
count(LIRE_Dal_corpus_clean)
count(LIRE_Dal_corpus_no_place_filtering_clean)
count(LIRE_Dal)
count(LIRE_Dal_clean)

# distinguish between the two types by adding variable column

LIRE_Dal_corpus$variable <- "Military"
LIRE_Dal_corpus_no_place_filtering$variable <- "Military"
LIRE_Dal_corpus_clean$variable <- "Military"
LIRE_Dal_corpus_no_place_filtering_clean$variable <- "Military"
LIRE_Dal$variable <- "All"
LIRE_Dal_clean$variable <- "All"

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
LIRE_Dal_corpus_npf_na <- prepare_for_density(LIRE_Dal_corpus_no_place_filtering)
LIRE_Dal_corpus_clean_na <- prepare_for_density(LIRE_Dal_corpus_clean)
LIRE_Dal_corpus_npf_clean_na <- prepare_for_density(LIRE_Dal_corpus_no_place_filtering_clean)
LIRE_Dal_na <- prepare_for_density(LIRE_Dal)
LIRE_Dal_clean_na <- prepare_for_density(LIRE_Dal_clean)

LIRE_Dal_corpus_count <- count(LIRE_Dal_corpus_na)
LIRE_Dal_corpus_npf_count <- count(LIRE_Dal_corpus_npf_na)
LIRE_Dal_corpus_clean_count <- count(LIRE_Dal_corpus_clean_na)
LIRE_Dal_corpus_npf_clean_count <- count(LIRE_Dal_corpus_npf_clean_na)
LIRE_Dal_count <- count(LIRE_Dal_na)
LIRE_Dal_clean_count <- count(LIRE_Dal_clean_na)

# check distribution of monuments using clean
## Following https://static.cambridge.org/content/id/urn:cambridge.org:id:article:S2326376821000085/resource/name/S2326376821000085sup001.pdf
LIRE_Dal_corpus_clean %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10, fill = "#FF8247") +
  labs(title = "Inscription date range",
       subtitle = "Dalmatia: Military",
       x = "Length of timespan (10 year bins)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_corpus_clean_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0) (Cleaned province = Dalmatia).\n",
                       "Filtered by key words, tags, and places.\n",
                       "Method = Steinmann & Weissova 2021."))

ggsave("output_images/chronological_distribution/01.LIRE_inscription_ranges_corpus.jpeg",
       dpi = 600)

LIRE_Dal_corpus_no_place_filtering_clean %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10, fill = "#FF8247") +
  labs(title = "Inscription date range",
       subtitle = "Dalmatia: Military",
       x = "Length of timespan (10 year bins)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_corpus_npf_clean_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0) (Cleaned province = Dalmatia).\n",
                       "Filtered by key words and tags.\n",
                       "Method = Steinmann & Weissova 2021."))

ggsave("output_images/chronological_distribution/02.LIRE_inscription_ranges_corpus_no_place_filter.jpeg",
       dpi = 600)

LIRE_Dal_clean %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10, fill = "#009E73") +
  labs(title = "Inscription date range",
       subtitle = "Dalmatia: All categories",
       x = "Length of timespan (10 year bins)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_clean_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0) (Cleaned province = Dalmatia).\n",
                       "Method = Steinmann & Weissova 2021."))

ggsave("output_images/chronological_distribution/03.LIRE_inscription_ranges_Dalmatia.jpeg",
       dpi = 600)

# now for weitghed density using datplot and 'clean' dfs
## Work with https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html
LIRE_Dal_corpus_clean_scaled <- scaleweight(datsteps(LIRE_Dal_corpus_clean_na,
                                                     stepsize = 25),
                                            var = "all")

ggplot(data = LIRE_Dal_corpus_clean_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#FF8247") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_corpus_clean_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0) (Cleaned province = Dalmatia).\n",
                       "Filtered by key words, tags, and places."),
       title = "Chronological distribution of military funerary and sacral monuments",
       subtitle = paste("Using the weighted output of datsteps() ",
                           "with stepsize of ",
                           attributes(LIRE_Dal_corpus_clean_scaled)$stepsize,
                           sep = ""))

ggsave("output_images/chronological_distribution/04.LIRE_clean_corpus_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_Dal_corpus_npf_clean_scaled <- scaleweight(datsteps(LIRE_Dal_corpus_npf_clean_na,
                                                         stepsize = 25),
                                                var = "all")

ggplot(data = LIRE_Dal_corpus_npf_clean_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#FF8247") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_corpus_npf_clean_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0) (Cleaned province = Dalmatia).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of military funerary and sacral monuments",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_corpus_npf_clean_scaled)$stepsize,
                        sep = ""))

ggsave("output_images/chronological_distribution/05.LIRE_clean_corpus_plot_no_place_filter.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_Dal_clean_scaled <- scaleweight(datsteps(LIRE_Dal_clean_na,
                                              stepsize = 25),
                                     var = "all")

ggplot(data = LIRE_Dal_clean_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#009E73") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_clean_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0) (Cleaned province = Dalmatia)."),
       title = "Chronological distribution of funerary and sacral monuments",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_clean_scaled)$stepsize,
                        sep = ""))

ggsave("output_images/chronological_distribution/06.LIRE_clean_dalmatia_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

# now for with a histogram
##get histogram 
LIRE_Dal_corpus_clean_histogramscale <- get.histogramscale(LIRE_Dal_corpus_clean_scaled)
LIRE_Dal_corpus_npf_clean_histogramscale <- get.histogramscale(LIRE_Dal_corpus_npf_clean_scaled)
LIRE_Dal_clean_scaled_histogram <- get.histogramscale(LIRE_Dal_clean_scaled)


## plot with histogram
ggplot(LIRE_Dal_corpus_clean_scaled, aes(x = DAT_step)) +
  geom_histogram(alpha = 0.8, binwidth = attributes(LIRE_Dal_corpus_clean_scaled)$stepsize,
                 position = "dodge", colour = "black", fill = "#9F522E") +
  stat_density(alpha = 0.8, position = "dodge", colour = "black", fill = "#FF8247",
               aes(y = (after_stat(density) * LIRE_Dal_corpus_clean_histogramscale), weight = weight)) +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_corpus_clean_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0) (Cleaned province = Dalmatia).\n",
                       "Filtered by key words, tags, and places."),
       title = "Chronological distribution of military funerary and sacral monuments",
     subtitle = paste("Using the weighted output of datsteps() ",
                      "with stepsize of ",
                      attributes(LIRE_Dal_corpus_clean_scaled)$stepsize,
                      sep = ""))

ggsave("output_images/chronological_distribution/07.LIRE_clean_corpus_plot_histogram.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

ggplot(LIRE_Dal_corpus_npf_clean_scaled, aes(x = DAT_step)) +
  geom_histogram(alpha = 0.8, binwidth = attributes(LIRE_Dal_corpus_npf_clean_scaled)$stepsize,
                 position = "dodge", colour = "black", fill = "#9F522E") +
  stat_density(alpha = 0.8, position = "dodge", colour = "black", fill = "#FF8247",
               aes(y = (after_stat(density) * LIRE_Dal_corpus_npf_clean_histogramscale), weight = weight)) +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_corpus_npf_clean_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0) (Cleaned province = Dalmatia).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of military funerary and sacral monuments",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_corpus_npf_clean_scaled)$stepsize,
                        sep = ""))

ggsave("output_images/chronological_distribution/08.LIRE_clean_corpus_plot_histogram_no_place_filter.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

ggplot(LIRE_Dal_clean_scaled, aes(x = DAT_step)) +
  geom_histogram(alpha = 0.8, binwidth = attributes(LIRE_Dal_clean_scaled)$stepsize,
                 position = "dodge", colour = "black", fill = "#014e3a") +
  stat_density(alpha = 0.8, position = "dodge", colour = "black", fill = "#009E73",
               aes(y = (after_stat(density) * LIRE_Dal_clean_scaled_histogram), weight = weight)) +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_clean_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0) (Cleaned province = Dalmatia)."),
       title = "Chronological distribution of funerary and sacral monuments",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_corpus_clean_scaled)$stepsize,
                        sep = ""))

ggsave("output_images/chronological_distribution/09.LIRE_clean_dalmatia_plot_histogram.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

#now to compare
LIRE_clean_both <- rbind(LIRE_Dal_clean_na, LIRE_Dal_corpus_clean_na)

LIRE_clean_both_scaled <- scaleweight(datsteps(LIRE_clean_both, stepsize = 25),
                                var = "all")

ggplot(data = LIRE_clean_both_scaled, aes(x = DAT_step, weight = weight,
                                          group=variable, fill=variable)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("Military" = "#FF8247", "All" = "#009E73"),
                    name = "Category") +
  labs(x = "Date (BCE/CE)", y = "Density",
       variable = "Category",
       caption = paste("Military n = ",
                       LIRE_Dal_corpus_clean_count$n,
                       ". All n = ",
                       LIRE_Dal_clean_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0) (Cleaned province = Dalmatia).\n",
                       "Filtered by key words, tags, and places."),
       title = "Chronological distribution of funerary and sacral monuments (Density)",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_corpus_clean_scaled)$stepsize,
                        sep = ""))
       
ggsave("output_images/chronological_distribution/10.LIRE_clean_dalmatia_corpus_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_clean_both_npf <- rbind(LIRE_Dal_clean_na, LIRE_Dal_corpus_npf_clean_na)

LIRE_clean_both_scaled_npf <- scaleweight(datsteps(LIRE_clean_both_npf, stepsize = 25),
                                          var = "all")

ggplot(data = LIRE_clean_both_scaled_npf, aes(x = DAT_step, weight = weight,
                                          group=variable, fill=variable)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("Military" = "#FF8247", "All" = "#009E73"),
                    name = "Category") +
  labs(x = "Date (BCE/CE)", y = "Density",
       variable = "Category",
       caption = paste("Military n = ",
                       LIRE_Dal_corpus_npf_clean_count$n,
                       ". All n = ",
                       LIRE_Dal_clean_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0) (Cleaned province = Dalmatia).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of funerary and sacral monuments (Density)",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_corpus_clean_scaled)$stepsize,
                        sep = ""))

ggsave("output_images/chronological_distribution/11.LIRE_clean_dalmatia_corpus_plot_no_place_filter.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)
