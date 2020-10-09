## code to prepare `default_theme` dataset goes here
default_theme <- ggplot2::theme(panel.grid =  ggplot2::element_blank(),
                       axis.line =  ggplot2::element_line(),
                       axis.ticks =  ggplot2::element_line(),
                       axis.title.y =  ggplot2::element_text(size = 14, face = "bold", margin =  ggplot2::margin(0,15,0,0)),
                       axis.title.x =  ggplot2::element_text(size = 14, face = "bold", margin =  ggplot2::margin(10,0,0,0)),
                       plot.title =  ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
                       strip.text =  ggplot2::element_text(size = 13),
                       strip.background =  ggplot2::element_rect(fill = NA, color = NA),
                       legend.title =  ggplot2::element_text(size = 14, face = "bold"),
                       legend.text =  ggplot2::element_text(size = 13),
                       panel.spacing =  ggplot2::unit(0.5, "cm"))

usethis::use_data(default_theme, overwrite = TRUE, internal = TRUE)
