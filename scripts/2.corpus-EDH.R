# getting corpus from cleaned EDH data
# started 23/10/2024
# edited 23/10/2024
library(dplyr)
library(sqldf)
library(arrow)

# clean and read EDH data
# and make NA values consistent
# data as of 10/10/2024 (via /scripts/appendix-1.LIRE-EDCS-EDH-data.R)
EDH_Dalmatia <-
  read.csv("data/EDH/EDH_Dalmatia.csv", na = c("","NA","NULL",NULL))