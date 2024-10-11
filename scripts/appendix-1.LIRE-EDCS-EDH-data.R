# counting dalmatian inscriptions on
# LIRE, EDCS, EDH
# 27/09/2024
# last ran 11/10/2024
library(dplyr)
library(rjson)
library(tibble)
library(data.table)
library(arrow)

# LIRE
LIRE_all <-
  read_parquet("https://zenodo.org/records/8431452/files/LIRE_v3-0.parquet?download=1")

LIRE_Dalmatia <- filter(LIRE_all, province_label_clean == "Dalmatia" | province == "Dalmatia")

# EDCS
## downloaded via Ballsun-Stanton, B., Heřmánková, P., & Laurence, R. (2024).
## Latin Epigraphy Scraper (v2.0). GitHub.
## https://doi.org/10.5281/zenodo.12036540
## as of 2022-08-29

EDCS <- fromJSON(file= "data/EDCS/2022-08-29-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json")
EDCS_data <- EDCS$data
EDCS_Dalmatia <- data.table::rbindlist(EDCS_data, fill = TRUE) 

# EDH
EDH_all <-
 read.csv("https://edh.ub.uni-heidelberg.de/data/download/edh_data_text.csv")
EDH_Dalmatia <- filter(EDH_all, provinz == "Dal")

# count
LIRE_Dalmatia_count <- count(LIRE_Dalmatia)
EDCS_Dalmatia_count <- count(EDCS_Dalmatia)
EDH_Dalmatia_count <- count(EDH_Dalmatia)

LIRE_Dalmatia_count$database <- "LIRE"
EDCS_Dalmatia_count$database <- "EDCS"
EDH_Dalmatia_count$database <- "EDH"

#merge all data frames together
count_list <- list(LIRE_Dalmatia_count, EDCS_Dalmatia_count, EDH_Dalmatia_count)      

database_Dalmatia_count <- Reduce(function(x, y) merge(x, y, all=TRUE), count_list) 

write.csv(database_Dalmatia_count,
          file = "output_tables/appendices/database_Dalmatia_count.csv")

# count Salona
LIRE_Dalmatia_salona_count <- count(filter(LIRE_Dalmatia, findspot_ancient_clean == "Salonae" | place == "Solin / Salona"))
EDCS_Dalmatia_salona_count <- count(filter(EDCS_Dalmatia, place == "Solin / Salona"))
EDH_Dalmatia_salona_count <- count(filter(EDH_Dalmatia, fo_antik %like% "Salonae"))

LIRE_Dalmatia_salona_count$database <- "LIRE"
EDCS_Dalmatia_salona_count$database <- "EDCS"
EDH_Dalmatia_salona_count$database <- "EDH"

#merge all data frames together
salona_count_list <- list(LIRE_Dalmatia_salona_count,
                             EDCS_Dalmatia_salona_count,
                             EDH_Dalmatia_salona_count)

database_Dalmatia_salona_count <- Reduce(function(x, y) merge(x, y, all=TRUE),
                                            salona_count_list) 

write.csv(database_Dalmatia_salona_count,
          file = "output_tables/appendices/database_salona_count.csv")