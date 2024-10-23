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

## make function for filtering types
clean_monument_types <- function(dataframe) {
  library(dplyr)
  clean_monument_types <- dataframe %>%
    filter(i_gattung %in% c("titsep",
                            "titsac"))  
  return(clean_monument_types)
}

clean_monument_types_epitaph <- function(dataframe) {
  library(dplyr)
  clean_monument_types_epitaph <- dataframe %>%
    filter(i_gattung %in% c("titsep"))  
  return(clean_monument_types_epitaph)
}

EDH_Dalmatia_votive_epitaph <- clean_monument_types(EDH_Dalmatia)

EDH_Dalmatia_epitaph <- clean_monument_types_epitaph(EDH_Dalmatia)

write.csv(EDH_Dalmatia, file = "output_tables/corpus/EDH/EDH_Dalmatia.csv")
write.csv(EDH_Dalmatia_votive_epitaph, file = "output_tables/corpus/EDH/EDH_Dalmatia_votive_epitaph.csv")
write.csv(EDH_Dalmatia_epitaph, file = "output_tables/corpus/EDH/EDH_Dalmatia_epitaph.csv")

## make function for filtering military
military_monuments <- function(dataframe) {
  library(dplyr)
  military_monuments <- dataframe %>%
    filter(militaer %in% c("J",
                           "A",
                           "G"))  
  return(military_monuments)
}

EDH_Dalmatia_military <- military_monuments(EDH_Dalmatia)
EDH_Dalmatia_military_v_E <- military_monuments(EDH_Dalmatia_votive_epitaph)
EDH_Dalmatia_military_E <- military_monuments(EDH_Dalmatia_epitaph)

