######## PLOT FUNCTIONS ###########

#' creates a basic horizontal skyline plot
#'
#' created a plot made up of a histogram for each group stacked vertically, split by \code{\link{facet_grid}}
#'
#' @param colour character string vector of colour(s) for outline of histogram, will be repeated across groups if too short. Default is NULL.
#' @param fill character string vector of fill for histogram, will be repeated across groups if too short. Default is 'blue'.
#' @param alpha alpha value for histogram, between 0 and 1. Default is 0.7.
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
skylinePlot <- function(df, value, group, colour = NULL, fill = 'blue', alpha = 0.7, bins = NULL, binwidth = NULL){
  df <- dplyr::rename(df,
               value = all_of(value),
               group = all_of(group)) # rename value and group columns so they can be called more easily by ggplot

  plot <- ggplot2::ggplot() +
    ggplot2::geom_histogram(data = df,
                   ggplot2::aes(x = value,
                       fill = group),
                   bins = bins,
                   binwidth = binwidth,
                   alpha = alpha) + # create histogram
    ggplot2::facet_grid(group ~ .) + # split into facet grid by grouping variable
    ggplot2::xlab(value) # labels x-axis with values column header

  if(!is.null(fill)){
    plot <- plot + ggplot2::scale_fill_manual(values = rep_len(x = fill, length.out = length(unique(df$group))))
  } else{
    stop('fill must be assigned a value')
  }
  if(!is.null(colour)){
    plot <- plot + ggplot2::scale_colour_manual(values = rep_len(x = colour, length.out = length(unique(df$group))))
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
#' @export
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

seaStackPlotHist <- function(df, value, group,
                        vertical = T, show_mean = T,
                        show_median = T, show_CI = T,
                        show_SD = T, bins = NULL,
                        binwidth = NULL, fill = 'blue',
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
                        removeYAxisText = T, mirrored = T, alpha = 0.5,
                        confidence_interval = 0.95){

  basic_plot <- skylinePlot(df = df, value = value, group = group, colour = colour,
                            fill = fill, alpha = alpha, bins = bins, binwidth = binwidth)
  pretty_plot <- formatPlot(plot = basic_plot, legend = legend, panel_spacing = panel_spacing,
                            x_title_size = x_title_size, y_title_size = y_title_size,
                            x_title_face = x_title_face, y_title_face = y_title_face,
                            axis_text_size = axis_text_size, group_label_size = group_label_size,
                            axis_text_face = axis_text_face, group_label_face = group_label_face,
                            y_lab = y_lab, brewer_fill = brewer_fill, brewer_colour = brewer_colour)
  stats_plot <- plotStats(plot = pretty_plot, SD_fill = SD_fill, SD_colour = SD_colour,
                          SD_size = SD_size, CI_colour = CI_colour, CI_size = CI_size,
                          CI_width = CI_width, show_CI = show_CI, show_SD = show_SD,
                          averages_point_size =  averages_point_size, mean_shape = mean_shape,
                          mean_fill = mean_fill, mean_colour = mean_colour, median_shape = median_shape,
                          median_fill = median_fill, median_colour = median_colour, show_mean = show_mean,
                          show_median = show_median, averages_opacity = averages_opacity,
                          confidence_interval = confidence_interval)
  if(removeYAxisText){
    stats_plot <- stats_plot +
      ggplot2::theme(axis.text.y = element_blank())
  }

  vertical_plot <- verticalPlot(plot = stats_plot, vertical = vertical, mirrored = mirrored) # note, this plot will not be vertical if vertical = F

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
    binwidth <- (ceiling(max(renamed_df$value)) - floor(min(renamed_df$value)))/bins
  }

  overall_breaks <- seq(from = floor(min(renamed_df$value)),
                        to = ceiling(max(renamed_df$value)),
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
    overall_breaks <- histogramBins(renamed_df = df, bins = bins, binwidth = binwidth)
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

    h <- hist(x = data.group$value, breaks = hist.breaks, plot = F)
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
#' @return a ggplot sea stack plot with no lines within the histogram but an outline around it
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

  df_stats <- summaryStats(df = df, group = 'group', value = 'value', confidence_interval = confidence_interval)

  hist.bins <- histogramBins(renamed_df = df, bins = bins, binwidth = binwidth)

  df.hist <- histogramData(df = df, value = 'value', group = 'group', breaks = hist.bins)

  line_limits <- df.hist %>% # this is to set the start and end of the line under histogram
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

  formatted_plot <- formatPlot(plot = plot, legend = legend, panel_spacing = panel_spacing,
                               x_title_size = x_title_size, y_title_size = y_title_size,
                               x_title_face = x_title_face, y_title_face = y_title_face,
                               axis_text_size = axis_text_size, group_label_size = group_label_size,
                               axis_text_face = axis_text_face, group_label_face = group_label_face,
                               y_lab = y_lab)

  stats_plot <- plotStats(plot = formatted_plot, SD_fill = SD_fill, SD_colour = SD_colour,
                          SD_size = SD_size, CI_colour = CI_colour, CI_size = CI_size, CI_width = CI_width,
                          show_CI = show_CI, show_SD = show_SD, averages_point_size = averages_point_size,
                          mean_shape = mean_shape, mean_fill = mean_fill, mean_colour = mean_colour,
                          median_shape = median_shape, median_fill = median_fill, median_colour = median_colour,
                          show_mean = show_mean, show_median = show_median, averages_opacity = averages_opacity,
                          df_stats = df_stats)

  vertical_plot <- verticalPlot(plot = stats_plot, vertical = vertical, mirrored = mirrored)

  return(vertical_plot)
}

#' Create a Sea Stack Plot
#'
#' Create a Sea Stack Pot, a beautiful and informative alternative to boxplots and histograms
#'
#' @param lines whick lines should be plotted, can take values 'external' for only outer ridge
#' or histogram, 'all' for lines around all bins, or 'none' for no lines. Defaults to 'external'
#'
#' @inheritParams seaStackPlotClean
#' @inheritParams seaStackPlotHist
#'
#' @return a sea stack plot ggplot graph
#'
#' @examples
#' seaStackPlot(InsectSprays, 'spray', 'count')
#'
#' @export

seaStackPlot <- function(df, group, value, lines = 'external',
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
                     mean_shape = 23, mean_fill = 'white', mean_colour = 'black',
                     median_shape = 21, median_fill = 'black', median_colour = 'black',
                     show_mean = T, show_median = T, averages_opacity = 0.8,
                     df_stats = NULL, vertical = T, mirrored = T,
                     brewer_fill = NULL, brewer_colour = NULL,
                     removeYAxisText = T){
  if(!lines %in% c('external', 'none', 'all')){
    lines <- 'external'
    warning('setting lines to external (default) please select alternate value none for no lines
            or all for internal and external lines if prefered')
  }
  if(lines == 'external'){
    plot <- seaStackPlotClean(df = df, value = value, group = group,
                              colour = colour, fill = fill, alpha = alpha,
                              bins = bins, binwidth = binwidth,
                              confidence_interval = confidence_interval, legend = legend,
                              panel_spacing = panel_spacing, x_title_size = x_title_size,
                              y_title_size = y_title_size, x_title_face = x_title_face,
                              y_title_face = y_title_face, axis_text_size = axis_text_size,
                              group_label_size = group_label_size, axis_text_face = axis_text_face,
                              group_label_face = group_label_face, y_lab = y_lab,
                              SD_fill = SD_fill, SD_colour = SD_colour,
                              SD_size = SD_size, CI_colour = CI_colour,
                              CI_size = CI_size, CI_width = CI_width,
                              show_CI = show_CI, show_SD = show_SD,
                              averages_point_size = averages_point_size,
                              mean_shape = mean_shape, mean_fill = mean_fill,
                              mean_colour = mean_colour, median_shape = median_shape,
                              median_fill = median_fill, median_colour = median_colour,
                              show_mean = show_mean, show_median = show_median,
                              averages_opacity = averages_opacity, df_stats = df_stats,
                              vertical = vertical, mirrored = mirrored)
  } else {
    if(lines == 'none'){
      colour <- NA
    }
    if(lines == 'all' & is.null(colour)){
      colour <- 'grey20'
    }

    plot <- seaStackPlotHist(df = df, value = value,
                             group = group, vertical = vertical,
                             show_mean = show_mean, show_median = show_median,
                             show_CI = show_CI, show_SD = show_SD,
                             bins = bins, binwidth = binwidth,
                             fill = fill, colour = colour,
                             legend = legend, panel_spacing = panel_spacing,
                             x_title_size = x_title_size, y_title_size = y_title_size,
                             x_title_face = x_title_face, y_title_face = y_title_face,
                             axis_text_size = axis_text_size, group_label_size = group_label_size,
                             axis_text_face = axis_text_face, group_label_face = group_label_face,
                             y_lab = y_lab, brewer_fill = brewer_fill,
                             brewer_colour = brewer_colour, SD_fill = SD_fill,
                             SD_colour = SD_colour, SD_size = SD_size,
                             CI_colour = CI_colour, CI_size = CI_size,
                             CI_width = CI_width, averages_point_size = averages_point_size,
                             mean_shape = mean_shape, mean_fill = mean_fill,
                             mean_colour = mean_colour, median_shape = median_shape,
                             median_fill = median_fill, median_colour = median_colour,
                             averages_opacity = averages_opacity, removeYAxisText = removeYAxisText,
                             mirrored = mirrored, alpha = alpha)
  }

  return(plot)
}
