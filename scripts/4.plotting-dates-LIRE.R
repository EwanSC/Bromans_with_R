# Plotting dates, LIRE
# Made 16/9/24
# Edited: 16/9/24
### FIRST RUN /scripts/4.primary_epigraphic_cor(e)pus-LIRE.R
## using https://cran.r-project.org/web/packages/datplot/datplot.pdf
## and https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html 
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

LIRE_Dal_corpus$variable <- "Military"
LIRE_Dal_corpus_no_place_filtering <- "Military"
LIRE_Dal_corpus_clean$variable <- "Military"
LIRE_Dal_corpus_no_place_filtering_clean <- "Military"
LIRE_Dal$variable <- "All"
LIRE_Dal_clean$variable <- "All"

LIRE_Dal_corpus_dates <- na.omit(LIRE_Dal_corpus %>%
                                   select(`LIST-ID`,variable,not_before,not_after))

LIRE_Dal_corpus_clean_dates <- na.omit(LIRE_Dal_corpus_clean %>%
                                         select(`LIST-ID`,variable,not_before,not_after))

LIRE_Dal_corpus_npf_dates <- na.omit(LIRE_Dal_corpus %>%
                                   select(`LIST-ID`,variable,not_before,not_after))

LIRE_Dal_corpus_npf_clean_dates <- na.omit(LIRE_Dal_corpus_clean %>%
                                         select(`LIST-ID`,variable,not_before,not_after))

LIRE_Dal_dates <- na.omit(LIRE_Dal %>%
                                   select(`LIST-ID`,variable,not_before,not_after))

LIRE_Dal_clean_dates <- na.omit(LIRE_Dal_clean %>%
                                   select(`LIST-ID`,variable,not_before,not_after))


LIRE_Dal_corpus_altered <- type.convert(LIRE_Dal_corpus_dates, as.is = TRUE)    # Modify column classes
LIRE_Dal_corpus_na <- na.omit(LIRE_Dal_corpus_altered)  #remove nulls
LIRE_Dal_corpus_na                                      # Print updated data frame

LIRE_Dal_corpus_clean_altered <- type.convert(LIRE_Dal_corpus_clean_dates, as.is = TRUE)    
LIRE_Dal_corpus_clean_na <- na.omit(LIRE_Dal_corpus_clean_altered) 
LIRE_Dal_corpus_clean_na

LIRE_Dal_corpus_npf_altered <- type.convert(LIRE_Dal_corpus_npf_dates, as.is = TRUE) 
LIRE_Dal_corpus_npf_na <- na.omit(LIRE_Dal_corpus_npf_altered)
LIRE_Dal_corpus_npf_na

LIRE_Dal_corpus_npf_clean_altered <- type.convert(LIRE_Dal_corpus_npf_clean_dates, as.is = TRUE)
LIRE_Dal_corpus_npf_clean_na <- na.omit(LIRE_Dal_corpus_npf_clean_altered)
LIRE_Dal_corpus_npf_clean_na

LIRE_all_Dal_dates_altered <- type.convert(LIRE_all_Dal_dates, as.is = TRUE) 
LIRE_all_Dal_dates_na <- na.omit(LIRE_all_Dal_dates_altered)
LIRE_all_Dal_dates_na

LIRE_all_Dal_clean_dates_altered <- type.convert(LIRE_all_Dal_clean_dates, as.is = TRUE)
LIRE_all_Dal_clean_dates_na <- na.omit(LIRE_all_Dal_clean_dates_altered) 
LIRE_all_Dal_clean_dates_na

LIRE_Dal_corpus_count <- count(LIRE_Dal_corpus_na)
LIRE_Dal_corpus_npf_count <- count(LIRE_Dal_corpus_npf_na)
LIRE_Dal_corpus_clean_count <- count(LIRE_Dal_corpus_clean_na)
LIRE_Dal_corpus_npf_clean_count <- count(LIRE_Dal_corpus_npf_clean_na)
LIRE_all_Dal_count <- count(LIRE_all_Dal_dates_na)
LIRE_all_Dal_clean_count <- count(LIRE_all_Dal_clean_dates_na)

# check distribution of monuments using clean
## Following https://static.cambridge.org/content/id/urn:cambridge.org:id:article:S2326376821000085/resource/name/S2326376821000085sup001.pdf
LIRE_Dal_corpus_clean %>%
  select(`LIST-ID`,province_label_clean,not_before,not_after) %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10, fill = "#FF8247") +
  labs(title = "Inscription date range",
       subtitle = "Dalmatia: Military",
       x = "Length of timespan (10 year bins)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_corpus_clean_count$n,
                       sep = "",
                       ". Data from LIRE v.3.0 (Cleaned province = Dalmatia).\n",
                       "Method = Steinmann & Weissova 2021."))

ggsave("output_images/chronological_distribution/LIRE_inscription_ranges_corpus.jpeg",
       dpi = 600)

LIRE_all_Dal_clean %>%
  select(`LIST-ID`,province_label_clean,not_before,not_after) %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10, fill = "#009E73") +
  labs(title = "Inscription date range",
       subtitle = "Dalmatia: All categories",
       x = "Length of timespan (10 year bins)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_all_Dal_clean_count$n,
                       sep = "",
                       ". Data from LIRE v.3.0 (Cleaned province = Dalmatia).\n",
                       "Method = Steinmann & Weissova 2021."))

ggsave("output_images/chronological_distribution/LIRE_inscription_ranges_Dalmatia.jpeg",
       dpi = 600)

# now for weitghed density using datplot and 'clean' dfs
## Work with https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html
LIRE_Dal_corpus_clean_scaled <- scaleweight(datsteps(LIRE_Dal_corpus_clean_na, stepsize = 25),
                                            var = "all")

LIRE_all_Dal_clean_scaled <- scaleweight(datsteps(LIRE_all_Dal_clean_dates_na, stepsize = 25),
                                         var = "all")

ggplot(data = LIRE_Dal_corpus_clean_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#FF8247") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_corpus_clean_count$n,
                       sep = "",
                       "Data from LIRE v.3.0 (Cleaned province = Dalmatia)."),
       title = "Chronological distribution of military monuments in Dalmatia",
       subtitle = paste("Using the weighted output of datsteps() ",
                           "with stepsize of ",
                           attributes(LIRE_Dal_corpus_clean_scaled)$stepsize,
                           sep = ""))

ggsave("output_images/chronological_distribution/LIRE_clean_corpus_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

ggplot(data = LIRE_all_Dal_clean_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#009E73") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_all_Dal_clean_count$n,
                       sep = "",
                       ". Data from LIRE v.3.0 (Cleaned province = Dalmatia)."),
       title = "Chronological distribution of monuments in Dalmatia",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_all_Dal_clean_scaled)$stepsize,
                        sep = ""))

ggsave("output_images/chronological_distribution/LIRE_clean_dalmatia_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

# now for with a histogram
LIRE_Dal_corpus_clean_histogramscale <- get.histogramscale(LIRE_Dal_corpus_clean_scaled)
LIRE_all_Dal_clean_scaled_histogram <- get.histogramscale(LIRE_all_Dal_clean_scaled)

ggplot(LIRE_Dal_corpus_clean_scaled, aes(x = DAT_step)) +
  geom_histogram(alpha = 0.8, binwidth = attributes(LIRE_Dal_corpus_clean_scaled)$stepsize,
                 position = "dodge", colour = "black", fill = "#9F522E") +
  stat_density(alpha = 0.8, position = "dodge", colour = "black", fill = "#FF8247",
               aes(y = (..density.. * LIRE_Dal_corpus_clean_histogramscale), weight = weight)) +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_corpus_clean_count$n,
                       sep = "",
                       ". Data from LIRE v.3.0 (Cleaned province = Dalmatia)."),
       title = "Chronological distribution of military monuments in Dalmatia",
     subtitle = paste("Using the weighted output of datsteps() ",
                      "with stepsize of ",
                      attributes(LIRE_Dal_corpus_clean_scaled)$stepsize,
                      sep = ""))

ggsave("output_images/chronological_distribution/LIRE_clean_corpus_plot_histogram.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

ggplot(LIRE_all_Dal_clean_scaled, aes(x = DAT_step)) +
  geom_histogram(alpha = 0.8, binwidth = attributes(LIRE_all_Dal_clean_scaled)$stepsize,
                 position = "dodge", colour = "black", fill = "#014e3a") +
  stat_density(alpha = 0.8, position = "dodge", colour = "black", fill = "#009E73",
               aes(y = (..density.. * LIRE_all_Dal_clean_scaled_histogram), weight = weight)) +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_all_Dal_clean_count$n,
                       sep = "",
                       ". Data from LIRE v.3.0 (Cleaned province = Dalmatia)."),
       title = "Chronological distribution of monuments in Dalmatia",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_corpus_clean_scaled)$stepsize,
                        sep = ""))

ggsave("output_images/chronological_distribution/LIRE_clean_dalmatia_plot_histogram.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

#now to compare
LIRE_clean_both <- rbind(LIRE_all_Dal_clean_dates_na, LIRE_Dal_corpus_clean_na)

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
                       LIRE_all_Dal_clean_count$n,
                       sep = "",
                       ".\nData from LIRE v.3.0 (Cleaned province = Dalmatia)."),
       title = "Chronological distribution of monuments in Dalmatia (Density)",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_corpus_clean_scaled)$stepsize,
                        sep = ""))
       
ggsave("output_images/chronological_distribution/LIRE_clean_dalmatia_corpus_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)
