library(ggplot2)
library(patchwork)

#####Creating a theme#####
My_theme = theme_bw() +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))

My_color = scale_color_manual("Site", values=c("NAN"="orangered2",
                                               "PIE"="darkblue"))