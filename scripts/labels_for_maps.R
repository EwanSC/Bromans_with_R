# trying to do labels
# ESC 29/04/2022
# ! RUN scripts/epig_with_sql.R first

#trying to get labels with:
# https://community.rstudio.com/t/geom-sf-text-change-position-of-only-one-text-label/73419/4
# https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html

ggplot() +
  geom_sf(data = world, color = "black", fill = "lightgrey") + 
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = all_military_place_ll, aes(size = n), alpha=0.6, colour = '#cd2026') +
  geom_sf_text(data = all_military_place_ll, aes(x = longitude, y = latitude, label = cleaned_place), size = 2,
               nudge_x = 0.2, nudge_y = 0.2) +
  labs(size = 'Monuments') +
  ggtitle("Epigraphic Distribution of the Military in Dalmatia 30 BCE - 150 CE", subtitle = "Filtered Using Key Words and Places") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46))

#plotted_all_military <- ggplot() + 
#  geom_sf(data = world, color = "black", fill = "lightgrey") + 
#  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
#  geom_sf(data = all_military_place_ll, aes(size = n), alpha=0.6, colour = '#cd2026') +
#  geom_sf_text(aes(label = `cleaned_place`), size = 1) +
#  labs(size = 'Monuments') +
#  ggtitle("Epigraphic Distribution of the Military in Dalmatia 30 BCE - 150 CE", subtitle = "Filtered Using Key Words and Places") +
#  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46))

# plotted_all_military + geom_sf_label()

# plotted_all_military + geom_text_repel(aes(label = rownames(all_military_place_ll)),
#                                       size = 3.5)