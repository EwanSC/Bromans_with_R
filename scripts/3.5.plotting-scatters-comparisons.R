# if you want to generate comparison plots, run this file
# last edit: 3/10/2024
# RUN scripts/3.plotting-scatters.R first!
#now to combine https://www.geeksforgeeks.org/draw-multiple-ggplot2-plots-side-by-side/
## first, combine undated
doubletrouble <- grid.arrange(plot1, plot9, ncol = 2)

ggsave("output_images/geographical_distribution/LIRE_corpus_and_dal_scatter.pdf",
       doubletrouble, width = 11.7, height = 8.3)
ggsave("output_images/geographical_distribution/LIRE_corpus_and_dal_scatter.jpeg",
       doubletrouble, width = 180, height = 140, unit = "mm", dpi = 600)

doubletroubler <- grid.arrange(plot3, plot10, ncol = 2)

ggsave("output_images/geographical_distribution/dated_LIRE_clean_corpus_and_dal_scatter.pdf",
       doubletroubler, width = 11.7, height = 8.3)
ggsave("output_images/geographical_distribution/dated_LIRE_clean_corpus_and_dal_scatter.jpeg",
       doubletroubler, width = 180, height = 140, unit = "mm", dpi = 600)

## now to combine dated
doubletroublest <- grid.arrange(plot5, plot11, ncol = 2)

ggsave("output_images/geographical_distribution/LIRE_corpus_and_dal_scatter2.pdf",
       doubletrouble, width = 11.7, height = 8.3)
ggsave("output_images/geographical_distribution/LIRE_corpus_and_dal_scatter2.jpeg",
       doubletrouble, width = 180, height = 140, unit = "mm", dpi = 600)

doubletroublester <- grid.arrange(plot7, plot12, ncol = 2)

ggsave("output_images/geographical_distribution/dated_LIRE_clean_corpus_and_dal_scatter2.pdf",
       doubletroubler, width = 11.7, height = 8.3)
ggsave("output_images/geographical_distribution/dated_LIRE_clean_corpus_and_dal_scatter2.jpeg",
       doubletroubler, width = 180, height = 140, unit = "mm", dpi = 600)