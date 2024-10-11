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


# try out API - using tutorial from https://www.dataquest.io/blog/r-api-tutorial/ #not working :( 

library(httr)
library(jsonlite)  
library(rjson)
library(tibble)
library(data.table)
library(dplyr)

EDH_request <- GET("https://edh.ub.uni-heidelberg.de/data/api/inschrift/suche?provinz=dal")

EDH_request

EDH_api_json <- fromJSON(file= EDH_request) +
  if (is.null(inFile)) +
  return(NULL)

EDH_api_dal_data <- EDH_api_json$data 

EDH_api_dal <- data.table::rbindlist(EDH_api_dal_data, fill = TRUE)

json_filename <- datascrape
json_data <- fromJSON(file= json_filename )
json_epig_data <- json_data$data
epig_data <- data.table::rbindlist(json_epig_data, fill = TRUE) 
