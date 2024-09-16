# Plotting dates, LIRE
# Made 6/9/24
# Edited: 16/9/24
### FIRST RUN /scripts/4.primary_epigraphic_cor(e)pus-LIRE.R
# check dfs loaded from script 4, we will run both simultaneously 
library(datplot)
library(ggplot2)
library(sqldf)
library(tidyverse)

count(LIRE_Dal_corpus)
count(LIRE_Dal_corpus_clean)

LIRE_Dal_corpus_dates <- na.omit(LIRE_Dal_corpus %>%
                                 select(`EDCS-ID`,not_after,not_before))

LIRE_Dal_corpus_clean_dates <- na.omit(LIRE_Dal_corpus_clean %>%
                                 select(`EDCS-ID`,not_after,not_before))

LIRE_Dal_corpus_altered <- type.convert(LIRE_Dal_corpus_dates, as.is = TRUE)    # Modify column classes
LIRE_Dal_corpus_na <- na.omit(LIRE_Dal_corpus_altered)  #remove nulls
LIRE_Dal_corpus_na                                      # Print updated data frame

LIRE_Dal_corpus_clean_altered <- type.convert(LIRE_Dal_corpus_clean_dates, as.is = TRUE)    # Modify column classes
LIRE_Dal_corpus_clean_na <- na.omit(LIRE_Dal_corpus_clean_altered) #remove nulls
LIRE_Dal_corpus_clean_na                                           # Print updated data frame

# now date using https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html
LIRE_Dal_corpus_na$date_mean <- (LIRE_Dal_corpus_na$not_after + LIRE_Dal_corpus_na$not_before) / 2 #create mean column

ggplot(LIRE_Dal_corpus_na, aes(x = date_mean), fill = date_mean) +
  labs(y = 'Monuments', x = 'Date (BCE/CE)') +
  labs(caption = "Based on data from LIRE v.3.0") +
  ggtitle("Temporal distribution of monuments", subtitle = "Mean date Histogram") +
  geom_histogram(binwidth = 10, position = "dodge") 

LIRE_Dal_corpus_clean_na$date_mean <- (LIRE_Dal_corpus_clean_na$not_after + LIRE_Dal_corpus_clean_na$not_before) / 2
ggplot(LIRE_Dal_corpus_clean_na, aes(x = date_mean), fill = date_mean) +
  labs(y = 'Monuments', x = 'Date (BCE/CE)') +
  labs(caption = "Based on data from LIRE v.3.0") +
  ggtitle("Temporal distribution of monuments (Clean province)", subtitle = "Mean date Histogram") +
  geom_histogram(binwidth = 10, position = "dodge") 

# now for density (not working sometimes, run .1 and .5 first, seems to work after) Work with https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html
system.time(LIRE_Dal_corpus_steps <- datsteps(LIRE_Dal_corpus_na, stepsize = 1))[3]

system.time(LIRE_Dal_corpus_steps <- datsteps(LIRE_Dal_corpus_na, stepsize = 25))[3]

LIRE_Dal_corpus_steps <- datsteps(LIRE_Dal_corpus_na, stepsize = 1)

ggplot(LIRE_Dal_corpus_steps, aes(x = DAT_step)) +
  geom_histogram(binwidth = 25, position = "dodge")

LIRE_Dal_corpus_dens <- datsteps(LIRE_Dal_corpus_na, stepsize = 5)
dens1 <- LIRE_Dal_corpus_dens
dens1 <- scaleweight(LIRE_Dal_corpus_dens, var = "all")
dens1 <- density(x = dens1$DAT_step, weights = dens1$weight)
plot(dens1)

LIRE_Dal_corpus_scale <- scaleweight(LIRE_Dal_corpus_dens, var = 2)

# with weight variable
ggplot(data = LIRE_Dal_corpus_scale, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  labs(x = "Date (BCE/CE)", y = "Maximum monument density") +
  labs(caption = "Based on data from LIRE v.3.0") +
  ggtitle("Temporal Distribution (864 military monuments)", subtitle = "Scaled density (wieghted)")

ggsave("output_images/chronological_distribution/LIRE_unclean_corpus_date_histogram.jpeg", dpi = 300)

# without weight variable
ggplot(data = LIRE_Dal_corpus_scale, aes(x = DAT_step)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  labs(x = "Date (BCE/CE)", y = "Maximum monument density") +
  labs(caption = "Based on data from LIRE v.3.0") +
  ggtitle("Temporal Distribution (864 military monuments)", subtitle = "Scaled density (unweigthed)")

# now to combine with histogram

histogramscale <- get.histogramscale(LIRE_Dal_corpus_scale)

uncleanplot <- 
  ggplot(LIRE_Dal_corpus_scale, aes(x = DAT_step)) +
  stat_density(alpha = 0.5, position = "dodge", colour = "black", fill = "darkorange",
               aes(y = (..density.. * histogramscale), weight = weight)) +
  geom_histogram(alpha = 0.5, binwidth = attributes(LIRE_Dal_corpus_scale)$stepsize,
                 position = "dodge", colour = "black", fill = "darkorange4") +
  labs(y = "Maximum number of monuments per year", x = "Dating (BCE/CE") +
  labs(caption = "Based on data from LIRE v.3.0") +
  ggtitle("Temporal Distribution (864 military monuments)", subtitle = "Scaled density and histogram (weighted)")

plot(uncleanplot)

ggsave("output_images/chronological_distribution/LIRE_unclean_corpus_date.jpeg", dpi = 300)

ggplot(LIRE_Dal_corpus_scale, aes(x = DAT_step)) +
  stat_density(alpha = 0.5, position = "dodge", colour = "black", fill = "darkorange",
               aes(y = (..density.. * histogramscale))) +
  geom_histogram(alpha = 0.5, binwidth = attributes(LIRE_Dal_corpus_scale)$stepsize,
                 position = "dodge", colour = "black", fill = "darkorange4") +
  labs(y = "Maximum number of monuments per year", x = "Dating (BCE/CE") +
  labs(caption = "Based on data from LIRE v.3.0") +
  ggtitle("Temporal Distribution (864 military monuments)", subtitle = "Scaled density and histogram (unweighted)")

# now for clean provinces from LIRE
system.time(LIRE_Dal_corpus_clean_steps <- datsteps(LIRE_Dal_corpus_clean_na, stepsize = 1))[3]

system.time(LIRE_Dal_corpus_clean_steps <- datsteps(LIRE_Dal_corpus_clean_na, stepsize = 25))[3]

LIRE_Dal_corpus_clean_steps <- datsteps(LIRE_Dal_corpus_clean_na, stepsize = 1)

ggplot(LIRE_Dal_corpus_clean_steps, aes(x = DAT_step)) +
  geom_histogram(binwidth = 25, position = "dodge")

LIRE_Dal_corpus_clean_dens <- datsteps(LIRE_Dal_corpus_clean_na, stepsize = 5)
dens2 <- LIRE_Dal_corpus_clean_dens
dens2 <- scaleweight(LIRE_Dal_corpus_clean_dens, var = "all")
dens2 <- density(x = dens2$DAT_step, weights = dens2$weight)
plot(dens2)

LIRE_Dal_corpus_clean_scale <- scaleweight(LIRE_Dal_corpus_clean_dens, var = 2)

# with weight variable
ggplot(data = LIRE_Dal_corpus_clean_scale, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  labs(x = "Date (BCE/CE)", y = "Maximum monument density") +
  labs(caption = "Based on data from LIRE v.3.0 (clean provinces)") +
  ggtitle("Temporal Distribution (853 military monuments)", subtitle = "Scaled density (wieghted)")


# without weight variable
ggplot(data = LIRE_Dal_corpus_clean_scale, aes(x = DAT_step)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  labs(x = "Date (BCE/CE)", y = "Maximum monument density") +
  labs(caption = "Based on data from LIRE v.3.0 (clean provinces)") +
  ggtitle("Temporal Distribution (853 military monuments)", subtitle = "Scaled density (unweigthed)")

# now to combine with histogram

histogramscale <- get.histogramscale(LIRE_Dal_corpus_clean_scale)

cleanplot <-
ggplot(LIRE_Dal_corpus_clean_scale, aes(x = DAT_step)) +
  stat_density(alpha = 0.5, position = "dodge", colour = "black", fill = "darkorange",
               aes(y = (..density.. * histogramscale), weight = weight)) +
  geom_histogram(alpha = 0.5, binwidth = attributes(LIRE_Dal_corpus_clean_scale)$stepsize,
                 position = "dodge", colour = "black", fill = "darkorange4") +
  labs(y = "Maximum number of monuments per year", x = "Dating (BCE/CE") +
  labs(caption = "Based on data from LIRE v.3.0 (clean provinces)") +
  ggtitle("Temporal Distribution (853 military monuments)", subtitle = "Scaled density and histogram (weighted)")

plot(cleanplot)

ggsave("output_images/chronological_distribution/LIRE_clean_corpus_date.jpeg", dpi = 300)

ggsave("output_images/chronological_distribution/LIRE_clean_corpus_date.jpeg", dpi = 300)

ggplot(LIRE_Dal_corpus_clean_scale, aes(x = DAT_step)) +
  stat_density(alpha = 0.5, position = "dodge", colour = "black", fill = "darkorange",
               aes(y = (..density.. * histogramscale))) +
  geom_histogram(alpha = 0.5, binwidth = attributes(LIRE_Dal_corpus_clean_scale)$stepsize,
                 position = "dodge", colour = "black", fill = "darkorange4") +
  labs(y = "Maximum number of monuments per year", x = "Dating (BCE/CE") +
  labs(caption = "Based on data from LIRE v.3.0 (clean provinces)") +
  ggtitle("Temporal Distribution (853 military monuments)", subtitle = "Scaled density and histogram (unweighted)")


#compare clean and unclean provinces (weighted)
doubletrouble <- grid.arrange(uncleanplot, cleanplot, ncol = 2)

# remove grid test(?)
ggplot(LIRE_Dal_corpus_clean_scale, aes(x = DAT_step)) +
  stat_density(alpha = 0.5, position = "dodge", colour = "black", fill = "darkorange",
               aes(y = (..density.. * histogramscale), weight = weight)) +
  geom_histogram(alpha = 0.5, binwidth = attributes(LIRE_Dal_corpus_clean_scale)$stepsize,
                 position = "dodge", colour = "black", fill = "darkorange4") +
  labs(y = "Maximum number of monuments per year", x = "Dating (BCE/CE") +
  labs(caption = "Based on data from LIRE v.3.0 (clean provinces)") +
  ggtitle("Temporal Distribution (853 military monuments)", 
          subtitle = "Scaled density and histogram (weighted: step size 5)")+
  theme(panel.background = element_blank())

ggsave("output_images/chronological_distribution/LIRE_clean_corpus_date_no_grid.jpeg", dpi = 300)

# now compare with all Dalmatia
## check .4 has been run
LIRE_all_Dal$count<- 1
LIRE_all_Dal_clean$count<- 1

LIRE_all_Dal_dates <- na.omit(LIRE_all_Dal %>%
                                   select(`EDCS-ID`,not_after,not_before))

LIRE_all_Dal_clean_dates <- na.omit(LIRE_all_Dal_clean %>%
                                         select(`EDCS-ID`,not_after,not_before))

LIRE_all_Dal_altered <- type.convert(LIRE_all_Dal_dates, as.is = TRUE)    # Modify column classes
LIRE_all_Dal_na <- na.omit(LIRE_all_Dal_altered)  #remove nulls
LIRE_all_Dal_na                                      # Print updated data frame

LIRE_all_Dal_clean_altered <- type.convert(LIRE_all_Dal_clean_dates, as.is = TRUE)    # Modify column classes
LIRE_all_Dal_clean_na <- na.omit(LIRE_all_Dal_clean_altered) #remove nulls
LIRE_all_Dal_clean_na

#plot unclean provinces
## create mean column
LIRE_all_Dal_na$date_mean <- (LIRE_all_Dal_na$not_after + LIRE_all_Dal_na$not_before) / 2
LIRE_all_Dal_clean_na$date_mean <- (LIRE_all_Dal_clean_na$not_after + LIRE_all_Dal_clean_na$not_before) / 2

#need to do bigger step size
#try 25 step size if takes too long?
LIRE_all_Dal_dens <- datsteps(LIRE_all_Dal_na, stepsize = 5)
dens3 <- LIRE_all_Dal_dens
dens3 <- scaleweight(LIRE_all_Dal_dens, var = "all")
dens3 <- density(x = dens3$DAT_step, weights = dens3$weight)
plot(dens3)

LIRE_all_Dal_scale <- scaleweight(LIRE_all_Dal_dens, var = 2)

histogramscale <- get.histogramscale(LIRE_all_Dal_scale)

uncleanallplot <-
ggplot(LIRE_all_Dal_scale, aes(x = DAT_step)) +
  stat_density(alpha = 0.5, position = "dodge", colour = "black", fill = "darkorange",
               aes(y = (..density.. * histogramscale), weight = weight)) +
  geom_histogram(alpha = 0.5, binwidth = attributes(LIRE_all_Dal_scale)$stepsize,
                 position = "dodge", colour = "black", fill = "darkorange4") +
  labs(y = "Maximum number of monuments per year", x = "Dating (BCE/CE") +
  labs(caption = "Based on data from LIRE v.3.0") +
  ggtitle("Temporal Distribution (6508 monuments)", 
          subtitle = "Scaled density and histogram (weighted with step size 5)")

plot(uncleanallplot)

#plot clean provinces
LIRE_all_Dal_clean_dens <- datsteps(LIRE_all_Dal_clean_na, stepsize = 5)
dens4 <- LIRE_all_Dal_dens
dens4 <- scaleweight(LIRE_all_Dal_clean_dens, var = "all")
dens4 <- density(x = dens4$DAT_step, weights = dens4$weight)
plot(dens4)

LIRE_all_Dal_clean_scale <- scaleweight(LIRE_all_Dal_clean_dens, var = 2)

histogramscale <- get.histogramscale(LIRE_all_Dal_clean_scale)

cleanallplot <- 
  ggplot(LIRE_all_Dal_clean_scale, aes(x = DAT_step)) +
  stat_density(alpha = 0.5, position = "dodge", colour = "black", fill = "darkorange",
               aes(y = (..density.. * histogramscale), weight = weight)) +
  geom_histogram(alpha = 0.5, binwidth = attributes(LIRE_all_Dal_clean_scale)$stepsize,
                 position = "dodge", colour = "black", fill = "darkorange4") +
  labs(y = "Maximum number of monuments per year", x = "Dating (BCE/CE") +
  labs(caption = "Based on data from LIRE v.3.0") +
  ggtitle("Temporal Distribution (6352 monuments)", subtitle = "Scaled density and histogram (weighted)")

plot(cleanallplot)

#combine
doubletrouble <- grid.arrange(uncleanplot, uncleanallplot, ncol = 2)
doubletroubler <- grid.arrange(cleanplot, cleanallplot, ncol = 2)
quadrupeltrouble <- grid.arrange(uncleanplot, cleanplot, uncleanallplot, cleanallplot, ncol = 4) #looks bad

