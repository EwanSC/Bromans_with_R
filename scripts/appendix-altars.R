# Plotting altars
# Edited: 4/12/24
## using https://cran.r-project.org/web/packages/datplot/datplot.pdf
## and https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html 
### plotting altars to send to Bupe for potential chapter


library(dplyr)
library(datplot)
library(ggplot2)
library(sqldf)
library(tidyverse)

LIRE_Dal_altar <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia_altar.csv", na = c("","NA","NULL",NULL))
LIRE_Dal_votive <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia_votive.csv", na = c("","NA","NULL",NULL))

count(LIRE_Dal_altar)
count(LIRE_Dal_votive)

LIRE_Dal_altar$variable <- "All"
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
LIRE_Dal_altar_na <- prepare_for_density(LIRE_Dal_altar)
LIRE_Dal_votive_na <- prepare_for_density(LIRE_Dal_votive)

LIRE_Dal_altar_count <- count(LIRE_Dal_altar)
LIRE_Dal_votive_count <- count(LIRE_Dal_votive)


#plots
LIRE_Dal_altar_scaled <- scaleweight(datsteps(LIRE_Dal_altar_na,
                                               stepsize = 15),
                                      var = "all")

ggplot(data = LIRE_Dal_altar_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#3468d6") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_altar_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of all altars",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_altar_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/altars/LIRE_Dalmatia_altar_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

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

ggsave("output_images/altars/LIRE_Dalmatia_votive_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)
