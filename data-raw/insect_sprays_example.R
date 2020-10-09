## code to prepare `insect_sprays_example` dataset goes here
insect_sprays_skylines_plot <- skylinePlot(df = InsectSprays,
                                           value = 'count',
                                           group = 'spray',
                                           bins = 20)

usethis::use_data(insect_sprays_skylines_plot)

insect_sprays_formatted_plot <- plotFormatting(insect_sprays_skylines_plot, brewer_fill = 'Set1', y_lab = 'Number of Insects')

usethis::use_data(insect_sprays_formatted_plot)

insect_sprays_stats_plot <- plotStats(insect_sprays_formatted_plot, CI_colour = "blue")

usethis::use_data(insect_sprays_stats_plot)

insect_sprays_sea_stack_plot <- verticalPlot(insect_sprays_stats_plot)

usethis::use_data(insect_sprays_sea_stack_plot)

usethis::use_data(insect_sprays_sea_stack_plot, overwrite = TRUE)
