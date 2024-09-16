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
library(RColorBrewer)

count(LIRE_Dal_corpus)
count(LIRE_Dal_corpus_clean)
count(LIRE_all_Dal)
count(LIRE_all_Dal_clean)

LIRE_Dal_corpus$variable <- "Military"
LIRE_Dal_corpus_clean$variable <- "Military"
LIRE_all_Dal$variable <- "All"
LIRE_all_Dal_clean$variable <- "All"

LIRE_Dal_corpus_dates <- na.omit(LIRE_Dal_corpus %>%
                                   select(`LIST-ID`,variable,not_before,not_after))

LIRE_Dal_corpus_clean_dates <- na.omit(LIRE_Dal_corpus_clean %>%
                                         select(`LIST-ID`,variable,not_before,not_after))

LIRE_all_Dal_dates <- na.omit(LIRE_all_Dal %>%
                                   select(`LIST-ID`,variable,not_before,not_after))

LIRE_all_Dal_clean_dates <- na.omit(LIRE_all_Dal_clean %>%
                                   select(`LIST-ID`,variable,not_before,not_after))


LIRE_Dal_corpus_altered <- type.convert(LIRE_Dal_corpus_dates, as.is = TRUE)    # Modify column classes
LIRE_Dal_corpus_na <- na.omit(LIRE_Dal_corpus_altered)  #remove nulls
LIRE_Dal_corpus_na                                      # Print updated data frame

LIRE_Dal_corpus_clean_altered <- type.convert(LIRE_Dal_corpus_clean_dates, as.is = TRUE)    # Modify column classes
LIRE_Dal_corpus_clean_na <- na.omit(LIRE_Dal_corpus_clean_altered) #remove nulls
LIRE_Dal_corpus_clean_na                                           # Print updated data frame

LIRE_all_Dal_dates_altered <- type.convert(LIRE_all_Dal_dates, as.is = TRUE)    # Modify column classes
LIRE_all_Dal_dates_na <- na.omit(LIRE_all_Dal_dates_altered) #remove nulls
LIRE_all_Dal_dates_na                                           # Print updated data frame

LIRE_all_Dal_clean_dates_altered <- type.convert(LIRE_all_Dal_clean_dates, as.is = TRUE)    # Modify column classes
LIRE_all_Dal_clean_dates_na <- na.omit(LIRE_all_Dal_clean_dates_altered) #remove nulls
LIRE_all_Dal_clean_dates_na                                           # Print updated data frame


LIRE_Dal_corpus_count <- count(LIRE_Dal_corpus_na)
LIRE_Dal_corpus_clean_count <- count(LIRE_Dal_corpus_clean_na)
LIRE_all_Dal_count <- count(LIRE_all_Dal_dates_na)
LIRE_all_Dal_clean_count <- count(LIRE_all_Dal_clean_dates_na)

# check distribution of monuments using clean
## Following https://static.cambridge.org/content/id/urn:cambridge.org:id:article:S2326376821000085/resource/name/S2326376821000085sup001.pdf
LIRE_Dal_corpus_clean %>%
  select(`LIST-ID`,province_label_clean,not_before,not_after) %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10, fill = "darkorange") +
  labs(title = "Number of Inscriptions Dated to Different Timespans in Dalmatia (Military)",
       x = "Lenght of Timespan (10 year bins)", y = "Number of Inscriptions",
       caption = paste("Following Steinmann & Weissova 2021. ",
                       "n = ",
                       LIRE_Dal_corpus_clean_count$n,
                       sep = "",
                       ". Data from LIRE v.3.0 (Cleaned province = Dalmatia)."))

ggsave("output_images/chronological_distribution/LIRE_inscription_ranges_corpus.jpeg", dpi = 600)

LIRE_all_Dal_clean %>%
  select(`LIST-ID`,province_label_clean,not_before,not_after) %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10, fill = "brown1") +
  labs(title = "Number of Inscriptions Dated to Different Timespans in Dalmatia",
       x = "Lenght of Timespan (10 year bins)", y = "Number of Inscriptions",
       caption = paste("Following Steinmann & Weissova 2021. ",
                       "n = ",
                       LIRE_all_Dal_clean_count$n,
                       sep = "",
                       ". Data from LIRE v.3.0 (Cleaned province = Dalmatia)."))

ggsave("output_images/chronological_distribution/LIRE_inscription_ranges_Dalmatia.jpeg", dpi = 600)

# now for weitghed density using datplot and 'clean' dfs
## Work with https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html
LIRE_Dal_corpus_clean_scaled <- scaleweight(datsteps(LIRE_Dal_corpus_clean_na, stepsize = 25),
                                            var = "all")

LIRE_all_Dal_clean_scaled <- scaleweight(datsteps(LIRE_all_Dal_clean_dates_na, stepsize = 25),
                                         var = "all")

ggplot(data = LIRE_Dal_corpus_clean_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "darkorange") +
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

ggsave("output_images/chronological_distribution/LIRE_clean_corpus_plot.jpeg", dpi = 600)

ggplot(data = LIRE_all_Dal_clean_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "brown1") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_all_Dal_clean_count$n,
                       sep = "",
                       ". Data from LIRE v.3.0 (Cleaned province = Dalmatia)."),
       title = "Temporal Distribution (? monuments)",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_all_Dal_clean_scaled)$stepsize,
                        sep = ""))

ggsave("output_images/chronological_distribution/LIRE_clean_dalmatia_plot.jpeg", dpi = 600)

# now for with a histogram
LIRE_Dal_corpus_clean_histogramscale <- get.histogramscale(LIRE_Dal_corpus_clean_scaled)
LIRE_all_Dal_clean_scaled_histogram <- get.histogramscale(LIRE_all_Dal_clean_scaled)

ggplot(LIRE_Dal_corpus_clean_scaled, aes(x = DAT_step)) +
  geom_histogram(alpha = 0.8, binwidth = attributes(LIRE_Dal_corpus_clean_scaled)$stepsize,
                 position = "dodge", colour = "black", fill = "darkorange4") +
  stat_density(alpha = 0.8, position = "dodge", colour = "black", fill = "darkorange",
               aes(y = (..density.. * LIRE_Dal_corpus_clean_histogramscale), weight = weight)) +
  labs(x = "Date (BCE/CE)", y = "Maximum number of monuments per year",
       caption = paste("n = ",
                       LIRE_Dal_corpus_clean_count$n,
                       sep = "",
                       ". Data from LIRE v.3.0 (Cleaned province = Dalmatia)."),
       title = "Chronological distribution of military monuments in Dalmatia",
     subtitle = paste("Using the weighted output of datsteps() ",
                      "with stepsize of ",
                      attributes(LIRE_Dal_corpus_clean_scaled)$stepsize,
                      sep = ""))

ggsave("output_images/chronological_distribution/LIRE_clean_corpus_plot_histogram.jpeg", dpi = 600)

ggplot(LIRE_all_Dal_clean_scaled, aes(x = DAT_step)) +
  geom_histogram(alpha = 0.8, binwidth = attributes(LIRE_all_Dal_clean_scaled)$stepsize,
                 position = "dodge", colour = "black", fill = "brown4") +
  stat_density(alpha = 0.8, position = "dodge", colour = "black", fill = "brown1",
               aes(y = (..density.. * LIRE_all_Dal_clean_scaled_histogram), weight = weight)) +
  labs(x = "Date (BCE/CE)", y = "Maximum number of monuments per year",
       caption = paste("n = ",
                       LIRE_all_Dal_clean_count$n,
                       sep = "",
                       ". Data from LIRE v.3.0 (Cleaned province = Dalmatia)."),
       title = "Chronological distribution of monuments in Dalmatia",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_corpus_clean_scaled)$stepsize,
                        sep = ""))

ggsave("output_images/chronological_distribution/LIRE_clean_dalmatia_plot_histogram.jpeg", dpi = 600)

#now to compare
LIRE_clean_both <- rbind(LIRE_all_Dal_clean_dates_na, LIRE_Dal_corpus_clean_na)

LIRE_clean_both_scaled <- scaleweight(datsteps(LIRE_clean_both, stepsize = 25),
                                var = "2")

ggplot(data = LIRE_clean_both_scaled, aes(x = DAT_step, weight = weight,
                                          group=variable, fill=variable)) +
  geom_density(alpha = 0.8) +
  labs(x = "Date (BCE/CE)", y = "Density",
       variable = "Category",
       caption = paste("Military n = ",
                       LIRE_Dal_corpus_clean_count$n,
                       ". All categories n = ",
                       LIRE_all_Dal_clean_count$n,
                       sep = "",
                       ". Data from LIRE v.3.0 (Cleaned province = Dalmatia)."),
       title = "Chronological distribution of monuments in Dalmatia (Density)",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_corpus_clean_scaled)$stepsize,
                        sep = ""))

ggsave("output_images/chronological_distribution/LIRE_clean_dalmatia_corpus_plot.jpeg", dpi = 600)
