library(ggplot2)
library(patchwork)

#####Creating a theme#####
My_theme = theme_bw(base_size = 19) +
  theme(axis.title.x = element_text(size = 22, face = "bold"),
        axis.text.x = element_text(size = 19),
        axis.title.y = element_text(size = 22, face = "bold"),
        axis.text.y = element_text(size = 19),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        plot.title = element_text(size = 20))

My_color = scale_color_manual("Site", values=c("NAN"="orangered2",
                                               "PIE"="darkblue"))