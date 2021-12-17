# Primary epigraphic Cor(e)pus
# ESC 17/12/2021
# Aim is to turn the LatEpig JSON files into a DF and then into a 1st century CE df
# as a function function which does just this
# Information from:
# https://www.datacamp.com/community/tutorials/r-data-import-tutorial#javascript
# https://stackoverflow.com/questions/53037844/multiple-list-json-to-data-frame-in-r
# https://swcarpentry.github.io/r-novice-inflammation/02-func-R/index.html

load_epig_data <- function(datascrape) {
  library(rjson)
  library(tibble)
  library(data.table)
  library(dplyr)
  json_filename <- datascrape
  json_data <- fromJSON(file= json_filename )
  json_epig_data <- json_data$data
  epig_data <- data.table::rbindlist(json_epig_data, fill = TRUE) 
  firstcent_epig_data <- epig_data %>%
  filter(`dating from` %in% (-30:100), `dating to` %in% (9:150)) %>%
  arrange("dating to", "dating from")
  return(firstcent_epig_data)
}

# example of deploying this in another script: 

"df" <- load_epig_data("data/2021-11-16-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json")
