## code to prepare `chick_weights_example` dataset goes here
chick_weights_skylines_plot <- skylinePlot(df = ChickWeight,
                                           value = 'weight',
                                           group = 'Diet',
                                           binwidth = 10)

usethis::use_data(chick_weights_skylines_plot)

chick_weights_formatted_plot <- plotFormatting(chick_weights_skylines_plot, brewer_fill = 'Set1')

usethis::use_data(chick_weights_formatted_plot)

chick_weights_stats_plot <- plotStats(chick_weights_formatted_plot, CI_colour = "blue")

usethis::use_data(chick_weights_stats_plot)

chick_weights_sea_stack_plot <- verticalPlot(chick_weights_stats_plot)

usethis::use_data(chick_weights_sea_stack_plot)
