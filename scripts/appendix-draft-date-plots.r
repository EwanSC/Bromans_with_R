# Plotting dates of types and cumulative density LIRE
# Made 16/9/24
# Edited: 20/11/24
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
count(LIRE_Dal)
count(LIRE_Dalmatia)
count(LIRE_Dal_corpus_epitaph)
count(LIRE_Dal_epitaph)
count(LIRE_Dal_corpus_stela)
count(LIRE_Dal_stela)
count(LIRE_Dal_votive)

# distinguish between the two types by adding variable column
LIRE_Dal_corpus$variable <- "Military"
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
LIRE_Dal_na <- prepare_for_density(LIRE_Dal)
LIRE_Dalmatia_na <- prepare_for_density(LIRE_Dalmatia)
LIRE_Dal_corpus_epitaph_na <- prepare_for_density(LIRE_Dal_corpus_epitaph)
LIRE_Dal_epitaph_na <- prepare_for_density(LIRE_Dal_epitaph)
LIRE_Dal_corpus_stela_na <- prepare_for_density(LIRE_Dal_corpus_stela)
LIRE_Dal_stela_na <- prepare_for_density(LIRE_Dal_stela)
LIRE_Dal_votive_na <- prepare_for_density(LIRE_Dal_votive)

LIRE_Dal_corpus_count <- count(LIRE_Dal_corpus_na)
LIRE_Dal_count <- count(LIRE_Dal_na)
LIRE_Dalmatia_count <- count(LIRE_Dalmatia_na)
LIRE_Dal_corpus_epitaph_count <- count(LIRE_Dal_corpus_epitaph)
LIRE_Dal_epitaph_count <- count(LIRE_Dal_epitaph)
LIRE_Dal_corpus_stela_count <- count(LIRE_Dal_corpus_stela)
LIRE_Dal_stela_count <- count(LIRE_Dal_stela)
LIRE_Dal_votive_count <- count(LIRE_Dal_votive)

LIRE_Dal_corpus$DAT_mean <- (LIRE_Dal_corpus$not_after + LIRE_Dal_corpus$not_before) / 2

LIRE_Dal_corpus_epitaph$DAT_mean <- (LIRE_Dal_corpus_epitaph$not_after + LIRE_Dal_corpus_epitaph$not_before) / 2

LIRE_Dal$DAT_mean <- (LIRE_Dal$not_after + LIRE_Dal$not_before) / 2

LIRE_Dalmatia$DAT_mean <- (LIRE_Dalmatia$not_after + LIRE_Dalmatia$not_before) / 2

LIRE_Dal_epitaph$DAT_mean <- (LIRE_Dal_epitaph$not_after + LIRE_Dal_epitaph$not_before) / 2

## experimentation
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


#now just votive and epitaph and diploma
LIRE_Dalmatia_select_inscr_types <- LIRE_Dalmatia_types %>%
    filter(variable %in% c("epitaph",
                           "votive inscription"))

LIRE_Dalmatia_select_inscr_types_na <- prepare_for_density(LIRE_Dalmatia_select_inscr_types)

LIRE_Dalmatia_select_inscr_types_count <- count(LIRE_Dalmatia_select_inscr_types_na)

LIRE_Dalmatia_select_inscr_types_scaled <- scaleweight(datsteps(LIRE_Dalmatia_select_inscr_types_na,
                                                stepsize = 15),
                                       var = "all")

ggplot(data = LIRE_Dalmatia_select_inscr_types_scaled,
       aes(x = DAT_step, fill = variable, weight = weight)) +
  geom_density(alpha = 0.3) +
  scale_fill_manual(values = c("votive inscription" = "#DCDCDC",
                                "epitaph" = '#EE0000'),
                    name = "Category") +
    labs(x = "Date (BCE/CE)", y = "Relative type density",
       caption = paste("n = ",
                       LIRE_Dalmatia_select_inscr_types_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of votive inscripitions and epitaphs",
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
  scale_fill_manual(values = c('#000000','#fcbfa3', '#FF8247', '#d6d6d6', '#FF5100', 
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


#now just votive and epitaph and diploma
LIRE_corpus_select_inscr_types <- LIRE_corpus_types %>%
    filter(variable %in% c("epitaph",
                           "votive inscription"))

LIRE_corpus_select_inscr_types_na <- prepare_for_density(LIRE_corpus_select_inscr_types)

LIRE_corpus_select_inscr_types_count <- count(LIRE_corpus_select_inscr_types_na)

LIRE_corpus_select_inscr_types_scaled <- scaleweight(datsteps(LIRE_corpus_select_inscr_types_na,
                                                stepsize = 15),
                                       var = "all")

ggplot(data = LIRE_corpus_select_inscr_types_scaled,
       aes(x = DAT_step, fill = variable, weight = weight)) +
  geom_density(alpha = 0.3) +
  scale_fill_manual(values = c("votive inscription" = "#DCDCDC",
                                "epitaph" = '#FF8247'),
                    name = "Category") +
    labs(x = "Date (BCE/CE)", y = "Relative type density",
       caption = paste("n = ",
                       LIRE_corpus_select_inscr_types_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of military votive inscripitions and epitaphs",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_corpus_select_inscr_types_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

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


# selection
LIRE_Dalmatia_select_monu_types <- LIRE_Dalmatia_monu_types %>%
  filter(variable %in% c("altar",
                         "stele",
                         "sarcophagus",
                         "tabula"))

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
                               "sarcophagus" = '#EE0000',
                               "tabula" ='#DCDCDC'),
                    name = "Category") +
    labs(x = "Date (BCE/CE)", y = "Relative type density",
       caption = paste("n = ",
                       LIRE_Dalmatia_select_monu_types_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of altars, stelae, sarcophagi, and tabulae",
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


LIRE_corpus_select_monu_types <- LIRE_corpus_monu_types %>%
  filter(variable %in% c("altar",
                         "stele",
                         "sarcophagus",
                         "tabula"))

LIRE_corpus_select_monu_types_na <- prepare_for_density(LIRE_corpus_select_monu_types)

LIRE_corpus_select_monu_types_count <- count(LIRE_corpus_select_monu_types)

LIRE_corpus_select_monu_types_scaled <- scaleweight(datsteps(LIRE_corpus_select_monu_types_na,
                                                                stepsize = 15),
                                                       var = "all")

ggplot(data = LIRE_corpus_select_monu_types_scaled,
       aes(x = DAT_step, fill = variable, weight = weight)) +
  geom_density(alpha = 0.3) +
  scale_fill_manual(values = c("altar" = "#000000",
                               "stele" = "#FFA07A",
                               "sarcophagus" = '#8B5A2B',
                               "tabula" = '#DCDCDC'),
                    name = "Category") +
    labs(x = "Date (BCE/CE)", y = "Relative type density",
       caption = paste("n = ",
                       LIRE_corpus_select_monu_types_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of militar altars, stelae, sarcophagi, and tabulae",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_corpus_select_monu_types_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())


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


# beta: trying to make cumulative graph
## using https://www.geeksforgeeks.org/plotting-cumulative-counts-in-ggplot2-in-r/
##using mean 
LIRE_Dalmatia_cumulative <- LIRE_Dalmatia %>%
  arrange(DAT_mean) %>%
  mutate(cumulative_count = row_number())

ggplot(LIRE_Dalmatia_cumulative, aes(x = DAT_mean, y = cumulative_count)) +
  geom_line(color = "#3468d6", linewidth = 1) +
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


LIRE_Dal_epitaph_cumulative <- LIRE_Dal_epitaph %>%
  arrange(DAT_mean) %>%
  mutate(cumulative_count = row_number())

ggplot(LIRE_Dal_epitaph_cumulative, aes(x = DAT_mean, y = cumulative_count)) +
  geom_line(color = "#3468d6", linewidth = 1) +
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


LIRE_Dal_epitaph_corpus_cumulative <- LIRE_Dal_corpus_epitaph %>%
  arrange(DAT_mean) %>%
  mutate(cumulative_count = row_number())

ggplot(LIRE_Dal_epitaph_corpus_cumulative, aes(x = DAT_mean, y = cumulative_count)) +
  geom_line(color = "#FF8247", linewidth = 1) +
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

# test with loess

ggplot(LIRE_Dal_epitaph_corpus_cumulative, aes(x = DAT_mean, y = cumulative_count)) +
  geom_line(color = "#FF8247", linewidth = 1) +
  geom_smooth(method = "loess", se = FALSE, colour="#9F522E")+
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
