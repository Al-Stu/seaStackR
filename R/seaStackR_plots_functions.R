#' creates a basic horizontal skyline plot
#'
#' created a plot made up of a histogram for each group stacked vertically, split by \code{\link{facet_grid}}
#'
#' @param colour character string vector of colour(s) for outline of histogram, will be repeated across groups if too short. Default is NULL.
#' @param fill character string vector of fill for histogram, will be repeated across groups if too short. Default is 'blue'.
#' @param histogram_opacity alpha value for histogram, between 0 and 1. Default is 0.7.
#'
#' @inheritParams summaryStats
#' @inheritParams ggplot2::geom_histogram
#'
#' @return a ggplot object with faceted histograms
#'
#' @examples
#' skylinePlot(df = InsectSprays,
#' value = 'weight',
#' group = 'Diet',
#' fill = c('blue', 'yellow', 'green'),
#' binwidth = 10)
#'
#' @export
#'
skylinePlot <- function(df, value, group, colour = NULL, fill = 'blue', histogram_opacity = 0.7, bins = NULL, binwidth = NULL){
  df <- dplyr::rename(df,
               value = all_of(value),
               group = all_of(group)) # rename value and group columns so they can be called more easily by ggplot

  plot <- ggplot2::ggplot() +
    ggplot2::geom_histogram(data = df,
                   ggplot2::aes(x = value,
                       fill = group),
                   bins = bins,
                   binwidth = binwidth,
                   alpha = histogram_opacity) + # create histogram
    ggplot2::facet_grid(group ~ .) + # split into facet grid by grouping variable
    ggplot2::xlab(value) # labels x-axis with values column header

  if(!is.null(fill)){
    plot <- plot + ggplot2::scale_fill_manual(values = rep_len(fill, length(unique(df$group))))
  } else{
    stop('fill must be assigned a value')
  }
  if(!is.null(colour)){
    plot <- plot + ggplot2::scale_colour_manual(values = rep_len(colour, length(unique(df$group))))
  }

  return(plot)
}

#' Improve the formatting of the basic skyline plot
#'
#' The default theme is not really cool, so will will make it all a bit nicer
#' potential to choose nicer colors using brewer_fill and brewer_colour variables
#'
#' @param plot basic plot from \code{\link{skylinePlot}}
#' @param legend logical, true to include legend and false to exclude, default value = F.
#' @param panel_spacing value for the spacing between faceted panels in cm, default value = 0.5cm
#' @param x_title_size text size for x-axis label, default value = 12
#' @param y_title_size text size for y-axis label, default value = 12
#' @param x_title_face text face for x-axis label, default value = 'bold'
#' @param y_title_face text face for y-axis label, default value = 'bold'
#' @param axis_text_size text size for axis text, default value = 12
#' @param group_label_size text size for group labels, default value = 12
#' @param axis_text_face text face for axis text, default value = NULL
#' @param group_label_size text face for group labels, default value = NULL
#' @param y_lab y-axis label, defaults to 'Count'
#' @param brewer_fill if you wish to have a brewer colour palette, as histogram fill, set the palette name here, if not leave as NULL (default)
#' @param brewer_colour if you wish to have a brewer colour palette, as histogram colour, set the palette name here, if not leave as NULL (default)
#'
#' @examples
#' formatPlot(insect_sprays_skylines_plot, brewer_fill = 'Set1')
#'
#' @return a ggplot plot with 'prettier' formatting than default
#'
formatPlot <- function(plot, legend = F,
                           panel_spacing = 0.5, x_title_size = 12,
                           y_title_size = 12, x_title_face = 'bold',
                           y_title_face = 'bold', axis_text_size = 12,
                           group_label_size = 12, axis_text_face = NULL,
                           group_label_face = NULL, y_lab = 'Count',
                           brewer_fill = NULL, brewer_colour = NULL
                           )
  {
  pretty_plot <- plot +
    ggplot2::theme_classic() + # change theme
    ggplot2::theme(legend.position = ifelse(legend, NULL, 'none'), # keeps legend if isTRUE(legend) else removes it
          panel.spacing = ggplot2::unit(panel_spacing, "cm"),
          axis.title.y = ggplot2::element_text(size = x_title_size, face = x_title_face, margin = ggplot2::margin(0,15,0,0)), # I've kept margins as they were originally, might want to make this customisable?
          axis.title.x = ggplot2::element_text(size = y_title_size, face = y_title_face, margin = ggplot2::margin(10,0,0,0)),
          axis.text = ggplot2::element_text(size = axis_text_size, face = axis_text_face),
          strip.text = ggplot2::element_text(size = group_label_size, face = group_label_face),
          strip.background = ggplot2::element_blank()) +
    ggplot2::labs(y = y_lab)

  if(!is.null(brewer_fill)){
    pretty_plot <- pretty_plot + ggplot2::scale_fill_brewer(palette = brewer_fill)
  }
  if(!is.null(brewer_colour)){
    pretty_plot <- pretty_plot + ggplot2::scale_color_brewer(palette = brewer_colour)
  }

  return(pretty_plot)
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
    df_stats <- summaryStats(plot$layers[[1]]$data, 'group', 'value', confidence_interval)
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
    df_stats <- summaryStats(plot$layers[[1]]$data, 'group', 'value', confidence_interval)
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
                      averages_opacity = 0.8, df_stats = NULL){  # CI size is a factor of how much bigger it is than SD_size
  if(is.null(df_stats)){
    df_stats <- summaryStats(plot$layers[[1]]$data, 'group', 'value')
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

  if(isTRUE(show_SD)){plot <- addSD(plot, df_stats, ymin, ymax, SD_fill, SD_colour)}

  if(isTRUE(show_CI)){plot <- addCI(plot, df_stats, CI_colour, CI_max, CI_min, CI_width)}

  plot <- addAverages(plot = plot, df_stats = df_stats, ymin = ymin, ymax = ymax,
                      averages_point_size = averages_point_size, mean_shape = mean_shape, mean_fill = mean_fill, mean_colour = mean_colour,
                      median_shape = median_shape, median_fill = median_fill, median_colour = median_colour, show_mean = show_mean,
                      show_median = show_median, averages_opacity = averages_opacity)

  return(plot)
}

#' Flip plots to be vertical
#'
#' Turn plots to be vertical and mirror axes so your sea stack plot stands up!
#'
#' @param vertical logical, if true the plot will be flipped vertically, if false it'll remain horizontal, defaults to TRUE
#' @param mirrored logical, if true the plot will be mirrored so histogram is on the left hand side, else it'll stay on the right, defaults to TRUE
#' @inheritParams addSD
#'
#' @return a ggplot plot that has been made vertical and mirrored
#'
#' @examples
#' verticalPlot(insect_sprays_stats_plot)
#'
#' @export
#'

verticalPlot <- function(plot, vertical = T, mirrored = T){
  if(vertical){
    plot <- plot +
      ggplot2::facet_grid(~ group) +
      ggplot2::coord_flip() +
      ggplot2::theme(panel.spacing = ggplot2::unit(0.2, "cm"),
            axis.text.x = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_text(size = 14))
  }

  # mirror the plots, so that the histograms are on the left hand side
  if(mirrored){
    plot <- plot +
      ggplot2::scale_y_reverse()
  }

  return(plot)
}


#' Sea Stack Plot with histogram lines
#'
#' Sea stack plot based off of geom_hist (classic way)
#'
#' @inheritParams skylinePlot
#' @inheritParams formatPlot
#' @inheritParams plotStats
#' @inheritParams verticalPlot
#'
#' @return a sea stack plot ggplot object
#'
#'
seaStackPlotHist <- function(df, value, group,
                        vertical = T, show_mean = T,
                        show_median = T, show_CI = T,
                        show_SD = T, bins = NULL,
                        binwidth = NULL, fill = NULL,
                        colour = NULL, legend = F,
                        panel_spacing = 0.5, x_title_size = 12,
                        y_title_size = 12, x_title_face = 'bold',
                        y_title_face = 'bold', axis_text_size = 12,
                        group_label_size = 12, axis_text_face = NULL,
                        group_label_face = NULL, y_lab = 'Count',
                        brewer_fill = NULL, brewer_colour = NULL,
                        SD_fill = "grey30", SD_colour = NA,
                        SD_size = NULL, CI_colour = 'red',
                        CI_size = 2, CI_width = 1,
                        averages_point_size = 3.5, mean_shape = 23,
                        mean_fill = 'white', mean_colour = 'black',
                        median_shape = 21, median_fill = 'black',
                        median_colour = 'black', averages_opacity = 0.8,
                        removeYAxisText = T, mirrored = T){

  basic_plot <- skylinePlot(df, value, group, colour, fill, bins)
  pretty_plot <- formatPlot(basic_plot, legend, panel_spacing,
                                x_title_size, y_title_size, x_title_face,
                                y_title_face, axis_text_size, group_label_size,
                                axis_text_face, group_label_face, y_lab,
                                brewer_colour, brewer_fill)
  stats_plot <- plotStats(plot = pretty_plot, SD_fill = SD_fill, SD_colour = SD_colour,
                          SD_size = SD_size, CI_colour = CI_colour, CI_size = CI_size,
                          CI_width = CI_width, show_CI = show_CI, show_SD = show_SD,
                          averages_point_size =  averages_point_size, mean_shape,
                          mean_fill, mean_colour, median_shape,
                          median_fill, median_colour, show_mean,
                          show_median, averages_opacity)
  if(removeYAxisText){
    stats_plot <- stats_plot +
      ggplot2::theme(axis.text.y = element_blank())
  }

  vertical_plot <- verticalPlot(stats_plot, vertical, mirrored) # note, this plot will not be vertical if vertical = F

  return(vertical_plot)
}

#' histogram bins
#'
#' @param renamed_df data with groups column renamed 'groups' and values column renamed 'values'
#' @inheritParams ggplot2::geom_histogram
#'
histogramBins <- function(renamed_df, bins, binwidth){
  if(is.null(binwidth)){
    if(is.null(bins)){
      bins <- 30
      warning('setting bins to default value of 30, please select a better value using bins or binwidth')
    }
    binwidth <- (ceiling(max(df$value)) - floor(min(df$value)))/bins
  }

  overall_breaks <- seq(from = floor(min(df$value)),
                        to = ceiling(max(df$value)),
                        by = binwidth)
}


#' get groups histogram data for seaStackPlotClean
#'
#' @inheritParams summaryStats
#' @inheritParams ggplot2::geom_histogram
#'

histogramData <- function(df, value, group, bins = NULL, binwidth = NULL, breaks = NULL){
  df <- dplyr::as_tibble(df) %>%
    dplyr::rename(value = all_of(value),
                  group = all_of(group)) # rename value and group columns so they can be called more easily by ggplot

  if(is.null(breaks)){
    overall_breaks <- histogramBins(df, bins, binwidth)
  } else {
    overall_breaks <- breaks
    binwidth <- breaks[2] - breaks[1]
  }

  for (i in unique(df$group)) {

    data.group <- df %>% dplyr::filter(group == i)

    min.break <- max(overall_breaks[overall_breaks <= min(data.group$value)]) # the closest break that is smaller than min value in group

    max.break <- min(overall_breaks[overall_breaks >= max(data.group$value)]) # the closest break that is larger than min value in group

    hist.breaks <- seq(from = min.break,
                       to = max.break,
                       by = binwidth)

    h <- hist(data.group$value, breaks = hist.breaks, plot = F)
    group.hist <- data.frame(value = c(min(hist.breaks),
                                       h$breaks,
                                       max(hist.breaks)),
                             Counts = c(0,h$counts,0,0), # Alice doesn't understand the two zeros here
                             group = i)

    if (i == unique(df$group)[1]) {

      df.hist <- group.hist

    }

    if (i != unique(df$group)[1]) {

      df.hist <- rbind(df.hist,group.hist)

    }

  }

  return(df.hist)
}

#' Sea Stack plot with clean inside
#'
#' based off of geom_step and geom_hist
#' @import ggplot2
#'
#' @inheritParams summaryStats
#' @inheritParams ggplot2::geom_histogram
#' @inheritParams formatPlot
#' @inheritParams plotStats
#' @inheritParams verticalPlot
#'

seaStackPlotClean <- function(df, value, group,
                              colour = 'grey50', fill = 'grey90', alpha = 0.5,
                              bins = NULL, binwidth = NULL,
                              confidence_interval = 0.95, legend = F,
                              panel_spacing = 0.5, x_title_size = 12,
                              y_title_size = 12, x_title_face = 'bold',
                              y_title_face = 'bold', axis_text_size = 12,
                              group_label_size = 12, axis_text_face = NULL,
                              group_label_face = NULL, y_lab = 'Count',
                              SD_fill = "grey30", SD_colour = NA, SD_size = NULL,
                              CI_colour = 'red', CI_size = 2, CI_width = 1,
                              show_CI = T, show_SD = T, averages_point_size = 3.5,
                              mean_shape = 23,mean_fill = 'white', mean_colour = 'black',
                              median_shape = 21, median_fill = 'black', median_colour = 'black',
                              show_mean = T, show_median = T, averages_opacity = 0.8,
                              df_stats = NULL, vertical = T, mirrored = T){
  df <- dplyr::as_tibble(df) %>%
    dplyr::rename(value = all_of(value),
                  group = all_of(group)) # rename value and group columns so they can be called more easily by ggplot

  df_stats <- summaryStats(df, 'group', 'value', confidence_interval)

  hist.bins <- histogramBins(df, bins, binwidth)

  df.hist <- histogramData(df, 'value', 'group', breaks = hist.bins)

  line_limits  <- df.hist %>% # this is to set the start and end of the line under histogram
    dplyr::group_by(group) %>%
    dplyr::summarise(min = min(value),
                     max = max(value))

  plot <- ggplot() +
    geom_histogram(data = df, aes(x = value),
                   fill = fill, color = NA,
                   breaks = hist.bins, alpha = alpha) +
    geom_step(data = df.hist, aes(x = value, y = Counts),
              stat = "identity", color = colour) +
    geom_segment(data = line_limits, aes(x = min, xend = max, y = 0, yend = 0), color = colour) +
    facet_grid(group ~ .) +
    # scale_x_continuous(breaks = seq(range(df$value)[1],range(df$value)[2],10), exp = c(0.05,0.05)) +
    # scale_y_continuous(limits = scale.limits, breaks = scale.breaks, labels = rep("",length(scale.breaks))) +
    labs(y = "Counts")

  formatted_plot <- formatPlot(plot, legend, panel_spacing, x_title_size, y_title_size, x_title_face,
                                  y_title_face, axis_text_size, group_label_size, axis_text_face, group_label_face,
                                  y_lab)

  stats_plot <- plotStats(formatted_plot, SD_fill, SD_colour, SD_size, CI_colour, CI_size, CI_width, show_CI, show_SD,
                          averages_point_size, mean_shape, mean_fill, mean_colour, median_shape, median_fill,
                          median_colour, show_mean, show_median, averages_opacity, df_stats)

  vertical_plot <- verticalPlot(stats_plot, vertical, mirrored)

  return(vertical_plot)
}
