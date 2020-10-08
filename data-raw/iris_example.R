iris <- datasets::iris

iris_skylines_plot <- skylinePlot(df = iris,
                                  value = 'Sepal.Length',
                                  group = 'Species',
                                  fill = c('blue', 'yellow'),
                                  binwidth = 0.1)

usethis::use_data(iris_skylines_plot)

iris_formatted_plot <- plotFormatting(iris_skylines_plot, brewer_fill = 'Set1')

usethis::use_data(iris_formatted_plot)

iris_stats_plot <- plotStats(iris_formatted_plot)

usethis::use_data(iris_stats_plot)

iris_sea_scape_plot <- verticalPlot(iris_stats_plot)

usethis::use_data(iris_sea_scape_plot)
