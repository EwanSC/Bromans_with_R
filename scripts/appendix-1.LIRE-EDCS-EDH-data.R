# counting dalmatian inscriptions on
# LIRE, EDCS, EDH
# 27/09/2024
library(dplyr)
library(rjson)
library(tibble)
library(data.table)
library(arrow)

# LIRE
LIRE_all <-
  read_parquet("https://zenodo.org/records/8431452/files/LIRE_v3-0.parquet?download=1")

LIRE_Dalmatia <- filter(LIRE_all, province == "Dalmatia")

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
count(LIRE_Dalmatia)
count(EDCS_Dalmatia)
count(EDH_Dalmatia)

# count Salona
count(filter(LIRE_Dalmatia, findspot_ancient_clean == "Salonae" | place == "Solin / Salona"))
count(filter(EDCS_Dalmatia, place == "Solin / Salona"))
count(filter(EDH_Dalmatia, fo_antik %like% "Salonae"))

