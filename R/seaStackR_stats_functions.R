########## STATS FUNCTIONS ##########

#' Standard error of the mean
#'
#' Calculates the standard error of the mean for a numeric vector, x
#'
#' @param x a vector of values for which the SEM is to be calculated
#' @param na.rm logical, TRUE to remove NA values from x (that will result in NA being returned),
#' defaults to FALSE
#'
#' @return standard error of the mean for x (numeric)
#'

sem <- function(x, na.rm = F) {

  if(na.rm == TRUE){
    x <- x[!is.na(x)]
  }

  sd(x)/sqrt(length(x))
}


#' Calculate Summary Statistics
#'
#' Calculate summary stats using the package dplyr. Here, we are starting with our data (df),
#' and then grouping the data by the column Group. For each group, we are calculating the mean,
#' standard deviation (SD), median, standard error of the mean (SEM) and the distance of the
#' confidence intervals from the mean
#'
#' @param df data frame or tibble of values with at least column for grouping variable and column for values
#' @param group name of the column containing grouping variable, should be given as a character string
#' @param value name of the column containing values, should be given as a character string
#' @param confidence_interval desired size of the confidence interval, value between 0 and 1 e.g. 0.95 for 95% (default)
#'
#' @examples
#' summaryStats(InsectSprays, 'spray', 'count', confidence_interval = 0.8)
#'
#' @return tibble with columns group (retains name of grouping variable column), \code{Mean} (mean),
#' \code{SD} (standard deviation), \code{Median} (median), \code{SEM} (standard error of the mean,
#' calculated using \code{\link{sem}}), \code{CI} (distance of confidence interval from mean),
#' \code{upperBound} (mean + one standard deviation) and \code{lowerBound} (mean - one standard
#' deviation).
#'
#' @export
#'

summaryStats <- function(df, group, value, confidence_interval = 0.95){
  z <- qnorm(confidence_interval+
               ((1-confidence_interval)/2) # this is because it's two tailed so the distance to 1 needs to be halved
  ) # calculate z value for confidence interval (two-tailed)

  df %>%
    dplyr::group_by(group) %>%
    dplyr::summarize(Mean = mean(value),
                     SD = sd(value),
                     Median = median(value),
                     SEM = sem(value, na.rm = T),
                     CI = z*sem(value, na.rm = T)) %>%
    dplyr::mutate(upperBound = Mean - SD,
                  lowerBound = Mean + SD)
}


#' Add standard deviation to histogram or skyline plot
#'
#' Adds a rectangle underneath the x axis showing the values within one standard deviation of the mean
#'
#' @param plot a histogram or skyline plot ggplot item
#' @param df_stats summary statistics for the data, if NULL (default) will calculate from data in the ggplot item
#' @param ymin the lower y limit of the rectangle
#' @param ymax the upper y limit of the rectangle
#' @param SD_fill character string specifying the fill for the standard deviation rectangle
#' @param SD_colour character string specifying the colour for the standard deviation rectangle
#'
#' @return a ggplot plot with rectangle showing range of mean +_ one standard deviation
#'

addSD <- function(plot, df_stats = NULL, ymin = 0, ymax, SD_fill, SD_colour){
  if(is.null(df_stats)){
    df_stats <- summaryStats(plot$layers[[1]]$data, 'group', 'value')
  }

  plot +
    ggplot2::geom_rect(data = df_stats,
                       ggplot2::aes(xmin = Mean - SD, xmax = Mean + SD,
                                    ymin = ymin, ymax = ymax),
                       fill = SD_fill, color = SD_colour,
                       alpha = 0.8)
}


#' Add confidence intervals to histogram or skyline plot
#'
#' Adds lines perpendicular to the x-axis at the
#'
#' @param CI_colour character string specifying the colour for the confidence interval lines
#' @param CI_max the top of the
#' @param CI_min the upper y limit of the rectangle
#'
#' @inheritParams summaryStats
#' @inheritParams addSD
#'
#' @return a ggplot plot with a lines for confidence interval added
#'

addCI <- function(plot, df_stats = NULL, CI_colour = 'red', CI_max, CI_min, CI_width, confidence_interval = 0.95){
  if(is.null(df_stats)){
    df_stats <- summaryStats(df = plot$layers[[1]]$data, group = 'group',
                             value = 'value', confidence_interval = confidence_interval)
  }

  plot +
    ggplot2::geom_segment(data = df_stats,
                          ggplot2::aes(x = Mean - CI, xend = Mean - CI,
                                       y = CI_max, yend = CI_min),
                          color = CI_colour,
                          size = CI_width) +
    ggplot2::geom_segment(data = df_stats,
                          ggplot2::aes(x = Mean + CI, xend = Mean + CI,
                                       y = CI_max, yend = CI_min),
                          color = CI_colour,
                          size = CI_width)
}


#' Add mean and median to plot
#'
#' Adds points to histogram, skyline plot or sea stack plot for mean and median defaults to
#' diamond for mean, and a circle for the median, I chose to plot a circle, of symbol size
#' slighty smaller than the symbol size for the mean. Positions of the mean in the middle of
#' the SD bar and median on the x-axis. Symbol size for mean defaults to 3.5, while the
#' median is 20 percent smaller.
#'
#' @param averages_point_size point size for the mean, median will be 20 percent smaller, defaults to 3.5
#' @param mean_shape point shape for the mean, defaults to 23 (a diamond)
#' @param mean_fill the fill colour for the mean, defaults to 'white'
#' @param mean_colour outline colour for the mean, defaults to 'black'
#' @param median_shape point shape for the median, defaults to 21 (a circle)
#' @param median_fill the fill colour for the median, defaults to 'black'
#' @param median_colour outline colour for the median, defaults to 'black'
#' @param show_mean logical, false if the mean is not to be added to the plot, defaults to TRUE
#' @param show_median logical, false if the median is not to be added to the plot, defaults to TRUE
#' @param averages_opacity alpha value for the mean and median points, numeric between 0 and 1, defaults to 0.8
#'
#' @inheritParams addSD
#'
#' @return a ggplot plot with a point for each groups mean and median added (depending on parameters)
#'

addAverages <- function(plot, df_stats,
                        ymin, ymax, averages_point_size = 3.5,
                        mean_shape = 23, mean_fill = 'white',
                        mean_colour = 'black', median_shape = 21,
                        median_fill = 'black', median_colour = 'black',
                        show_mean = T, show_median = T,
                        averages_opacity = 0.8){
  if(is.null(df_stats)){
    df_stats <- summaryStats(df = plot$layers[[1]]$data, group = 'group',
                             value = 'value', confidence_interval = confidence_interval)
  }


  ymean <- ymin - ((ymin-ymax)/2)
  ymedian <- 0

  size_mean <- averages_point_size
  size_median <- size_mean * 0.8

  # Add these to the plot
  if(isTRUE(show_mean)){
    plot <- plot +
      ggplot2::geom_point(data = df_stats,
                          ggplot2::aes(x = Mean, y = ymean),
                          fill = mean_fill, color = mean_colour,
                          size = size_mean, shape = mean_shape, stroke = 1.1,
                          alpha = averages_opacity)
  }

  if(isTRUE(show_median)){
    plot <- plot +
      ggplot2::geom_point(data = df_stats,
                          ggplot2::aes(x = Median, y = ymedian),
                          fill = median_fill, color = median_colour,
                          size = size_median, shape = median_shape,
                          alpha = averages_opacity)
  }

  return(plot)
}


#' Add summary statistics to a plot
#'
#' Add mean, median, standard deviation and/or confidence intervals to a histogram,
#' skyline plot, density plot, boxplot, violin plot or sea stack plot. NOTE: main plot
#' must be first layer of \code{plot} for this to work unless summary statistics from
#' \code{\link{summaryStats}} are added as parameter \code{df_stats}
#'
#' @param SD_size height of standard deviation rectangle, gets set to a tenth of the
#' height of the tallest bin unless specified. NOTE: this does not work on data that
#' has been processed before being plotted
#' @param show_CI logical, if false will not plot confidence interval marks, defaults to TRUE
#' @param show_SD logical, if false will not plot standard deviation rectangle, defaults to TRUE
#' @param df_stats summary statistics for plot, if NULL (default) will calculate from data behind
#' plot. NOTE: this does not work on data that has been processed before being plotted
#'
#' @inheritParams addSD
#' @inheritParams addCI
#' @inheritParams addAverages
#'
#' @importFrom magrittr %>%
#'
#' @return a ggplot list with original plot and chosen stats added
#'
#' @examples
#' plotStats(insect_sprays_formatted_plot)
#'
#' @export
#'

plotStats <- function(plot, SD_fill = "grey30",
                      SD_colour = NA, SD_size = NULL,
                      CI_colour = 'red', CI_size = 2,
                      CI_width = 1, show_CI = T,
                      show_SD = T, averages_point_size = 3.5,
                      mean_shape = 23, mean_fill = 'white',
                      mean_colour = 'black', median_shape = 21,
                      median_fill = 'black', median_colour = 'black',
                      show_mean = T, show_median = T,
                      averages_opacity = 0.8, df_stats = NULL,
                      confidence_interval = 0.95){  # CI size is a factor of how much bigger it is than SD_size
  if(is.null(df_stats)){
    df_stats <- summaryStats(df = plot$layers[[1]]$data, group = 'group',
                             value = 'value', confidence_interval = confidence_interval)
  }

  if(is.null(SD_size)){
    built_plot <- ggplot2::ggplot_build(plot)
    tallest_bin <- max(built_plot[["data"]][[1]][["count"]])
    ymax <- -tallest_bin/10
  } else{
    ymax <- -SD_size
  }

  CI_height <- CI_size * -ymax
  ymin <- 0
  CI_min <- -CI_height/2
  CI_max <- CI_height/2

  if(isTRUE(show_SD)){plot <- addSD(plot = plot, df_stats = df_stats,
                                    ymin = ymin, ymax = ymax,
                                    SD_fill = SD_fill, SD_colour = SD_colour)}

  if(isTRUE(show_CI)){plot <- addCI(plot = plot, df_stats = df_stats,
                                    CI_colour = CI_colour,
                                    CI_max = CI_max, CI_min = CI_min,
                                    CI_width = CI_width)}

  plot <- addAverages(plot = plot, df_stats = df_stats, ymin = ymin, ymax = ymax,
                      averages_point_size = averages_point_size, mean_shape = mean_shape, mean_fill = mean_fill, mean_colour = mean_colour,
                      median_shape = median_shape, median_fill = median_fill, median_colour = median_colour, show_mean = show_mean,
                      show_median = show_median, averages_opacity = averages_opacity)

  return(plot)
}
