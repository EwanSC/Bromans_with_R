# Plotting dates
# Ewan Coopey
# created 29/08/2022
# last edit: 29/01/2024
# Create primary datasets for PhD research by combining 1st cent. CE and undated military inscriptions
# and then plot both (separately and together)
# sql: ## testing out using https://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/sql.html


## get packages
library(datplot)
library(ggplot2)
library(sqldf)


## run scripts/1.primary_epigraphic_cor(e)pus_functions and 2.scripts/primary_map
## then, deploy function to get and clean df:

cleaned_place <- load_clean_place("data/2022-08-29-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json")
clean_dated_data <- load_clean_epig_data_30_150("data/2022-08-29-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json")

# now analyse:
## 'all military' by using 'where like' clause and wildcard
# see here for more: https://www.w3schools.com/sql/sql_wildcards.asp#:~:text=SQL%20Wildcards,-%E2%9D%AE%20Previous%20Next&text=A%20wildcard%20character%20is%20used,specified%20pattern%20in%20a%20column.
## filtering to 'military' by inscription terms and place and then plotting using ggplot

#all military EDCS

military <- sqldf("Select * from cleaned_place
                  WHERE inscription_interpretive_cleaning 
                    LIKE '%legio%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%cohor%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%ala%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%alae%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%milit%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%eques%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%equit%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%duplicari%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%veteran%'
                  or inscription_interpretive_cleaning
                    LIKE '%centuri%'
                  or inscription_interpretive_cleaning
                    LIKE '%immun%'
                  or inscription_interpretive_cleaning
                    LIKE '%miles%'
                  or inscription_interpretive_cleaning
                    LIKE '%beneficiar%'
                  or inscription_interpretive_cleaning
                    LIKE '%tesserari%'
                  or inscription_interpretive_cleaning
                    LIKE '%signifer%'
                  or inscription_interpretive_cleaning
                    LIKE '%aquilifer%'
                  or inscription_interpretive_cleaning
                    LIKE '%imaginifer%'
                  or inscription_interpretive_cleaning
                    LIKE '%corniculari%'
                  or inscription_interpretive_cleaning
                    LIKE '%principalis%'
                  or inscription_interpretive_cleaning
                    LIKE '%primus pilus%'
                  or inscription_interpretive_cleaning
                    LIKE '%primo pilo%'
                  or inscription_interpretive_cleaning
                    LIKE '%primi pili%'
                  or inscription_interpretive_cleaning
                    LIKE '%praefectus castrorum%'
                  or inscription_interpretive_cleaning
                    LIKE '%optio %'
                  or inscription_interpretive_cleaning
                    LIKE '%option%'
                  or status
                    LIKE '%milites%'
                  OR cleaned_place = 'Tilurium'
                  OR cleaned_place = 'Burnum'
                  OR cleaned_place = 'Andetrium'
                  OR cleaned_place = 'Bigeste'
                  ")


# 1st cent. EDCS
dated_military <- sqldf("Select * from clean_dated_data
                  WHERE inscription_interpretive_cleaning 
                    LIKE '%legio%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%cohor%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%ala%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%alae%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%milit%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%eques%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%equit%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%duplicari%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%veteran%'
                  or inscription_interpretive_cleaning
                    LIKE '%centuri%'
                  or inscription_interpretive_cleaning
                    LIKE '%immun%'
                  or inscription_interpretive_cleaning
                    LIKE '%miles%'
                  or inscription_interpretive_cleaning
                    LIKE '%beneficiar%'
                  or inscription_interpretive_cleaning
                    LIKE '%tesserari%'
                  or inscription_interpretive_cleaning
                    LIKE '%signifer%'
                  or inscription_interpretive_cleaning
                    LIKE '%aquilifer%'
                  or inscription_interpretive_cleaning
                    LIKE '%imaginifer%'
                  or inscription_interpretive_cleaning
                    LIKE '%corniculari%'
                  or inscription_interpretive_cleaning
                    LIKE '%principalis%'
                  or inscription_interpretive_cleaning
                    LIKE '%primus pilus%'
                  or inscription_interpretive_cleaning
                    LIKE '%primo pilo%'
                  or inscription_interpretive_cleaning
                    LIKE '%primi pili%'
                  or inscription_interpretive_cleaning
                    LIKE '%praefectus castrorum%'
                  or inscription_interpretive_cleaning
                    LIKE '%optio %'
                  or inscription_interpretive_cleaning
                    LIKE '%option%'
                  OR cleaned_place = 'Tilurium'
                  OR cleaned_place = 'Burnum'
                  OR cleaned_place = 'Andetrium'
                  OR cleaned_place = 'Bigeste'
                  ")
# need to change class of columns for dates as they are currently characters no integer
# following https://statisticsglobe.com/change-classes-data-frame-columns-automatically-r

dated_military_short <- na.omit(dated_military %>%
                                  select(`EDCS-ID`,cleaned_place,dating_to,dating_from))

dated_military_new <- type.convert(dated_military_short, as.is = TRUE)    # Modify column classes
dated_military_new                                        # Print updated data frame

# now to try date using https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html

dated_military_new$date_mean <- (dated_military_new$dating_from + dated_military_new$dating_to) / 2
ggplot(dated_military_new, aes(x = date_mean), fill = date_mean) +
  labs(y = 'Monuments', x = 'Date (BCE/CE)') +
  ggtitle("Temporal distribution of monuments", subtitle = "Mean date") +
  geom_histogram(binwidth = 10, position = "dodge") 

# now for density (not working..) Work with https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html
system.time(dated_military_steps <- datsteps(dated_military_new, stepsize = 1))[3]

system.time(dated_military_steps <- datsteps(dated_military_new, stepsize = 25))[3]

dated_military_steps <- datsteps(dated_military_new, stepsize = 1)

ggplot(dated_military_steps, aes(x = DAT_step)) +
  geom_histogram(binwidth = 25, position = "dodge")

dated_military_dens <- datsteps(dated_military_new, stepsize = 5)
dens <- dated_military_dens
dens <- scaleweight(dated_military_dens, var = "all")
dens <- density(x = dens$DAT_step, weights = dens$weight)
plot(dens)

dated_military_scale <- scaleweight(dated_military_dens, var = 2)

# with weight variable
ggplot(data = dated_military_scale, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  labs(x = "Date (BCE/CE)") +
  ggtitle("Temporal Distribution", subtitle = "Scaled weighted density")


# without weight variable
ggplot(data = dated_military_scale, aes(x = DAT_step)) +
  geom_density(alpha = 0.5, fill = "darkorange") +
  labs(x = "Date (BCE/CE)", y = "Maximum monument density") +
  ggtitle("Temporal Distribution", subtitle = "Scaled density")

# now to combine

histogramscale <- get.histogramscale(dated_military_scale)

ggplot(dated_military_scale, aes(x = DAT_step)) +
  stat_density(alpha = 0.5, position = "dodge", fill = "darkorange",
               aes(y = (..density.. * histogramscale), weight = weight)) +
  geom_histogram(alpha = 0.5, binwidth = attributes(dated_military_scale)$stepsize,
                 position = "dodge", fill = "darkgrey") +
  labs(y = "Maximum number of monuments per year", x = "Dating (BCE/CE") +
  ggtitle("Temporal Distribution", subtitle = "Scaled density and mean date")


ggplot(dated_military_scale, aes(x = DAT_step)) +
  stat_density(alpha = 0.5, position = "dodge", fill = "darkorange",
               aes(y = (..density.. * histogramscale))) +
  geom_histogram(alpha = 0.5, binwidth = attributes(dated_military_scale)$stepsize,
                 position = "dodge", fill = "darkgrey") +
  labs(y = "Maximum number of monuments per year", x = "Dating (BCE/CE") +
  ggtitle("Temporal Distribution", subtitle = "Scaled density and histogram")

### all monuments
