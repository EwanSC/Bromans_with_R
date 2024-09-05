#seeing if JSON LIRE different to LIRE
library(dplyr)
library(arrow)

LIRE_JS <- read_parquet("https://zenodo.org/record/8147298/files/LIRE_v2-3.parquet?download=1")
LIRE_Dal_JS <- filter(LIRE, province == "Dalmatia")
LIRE_Dal_clean_JS <- filter(LIRE, province_label_clean == "Dalmatia")
#write.csv(LIRE_Dal)
#write.csv(LIRE_Dal_clean)