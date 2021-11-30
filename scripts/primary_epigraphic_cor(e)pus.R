library(rjson) #https://www.datacamp.com/community/tutorials/r-data-import-tutorial#javascript
library(tibble)
library(data.table)
library(dplyr)

json_filename <- "data/2021-11-16-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json"

json_data <- fromJSON(file= json_filename )

json_epig_data <- json_data$data

epig_data <- data.table::rbindlist(json_epig_data, fill = TRUE) #https://stackoverflow.com/questions/53037844/multiple-list-json-to-data-frame-in-r

firstcent_epig_data <- epig_data %>%
  filter(`dating from` %in% (-30:100), `dating to` %in% (9:150)) %>%
  arrange("dating to", "dating from")

# make into a function
# function that imports, cleans and creates df for 1st century epig in Dalmatia
# following https://swcarpentry.github.io/r-novice-inflammation/02-func-R/index.html

load_epig_data <- function() {
  library(rjson)
  library(tibble)
  library(data.table)
  library(dplyr)
  json_filename <- "data/2021-11-16-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json"
  json_data <- fromJSON(file= json_filename )
  json_epig_data <- json_data$data
  epig_data <- data.table::rbindlist(json_epig_data, fill = TRUE) 
  firstcent_epig_data <- epig_data %>%
  filter(`dating from` %in% (-30:100), `dating to` %in% (9:150)) %>%
  arrange("dating to", "dating from")
  return(firstcent_epig_data)
}

# to deploy this in another script: 

"df" <- load_epig_data()