# Using data from EDH
# ESC 29/04/2022

library(tibble)
library(data.table)
library(dplyr)

## import from https://edh.ub.uni-heidelberg.de/data
EDH <- read.csv('data/29-04-2020-edh_data_text.csv') 

firstcent_EDH <- EDH %>%
  filter(`dat_jahr_a` %in% (-30:100), `dat_jahr_e` %in% (1:150))

firstcent_EDH_Dal <- firstcent_EDH %>%
  filter(provinz == "Dal", .preserve = FALSE)
  