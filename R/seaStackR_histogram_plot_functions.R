######## HIST PLOT FUNCTIONS ###########


#' Sea Stack Plot with histogram lines
#'
#' Sea stack plot based off of geom_hist (classic way)
#'
#' @inheritParams seaStackPlot
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
                        averages_point_size = 3, mean_shape = 23,
                        mean_fill = 'white', mean_colour = 'black',
                        mean_stroke = 0.8, median_shape = 21,
                        median_fill = 'black', median_colour = 'black',
                        averages_opacity = 0.8, removeYAxisText = T,
                        mirrored = T, alpha = 0.5,
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
                          mean_fill = mean_fill, mean_colour = mean_colour, mean_stroke = mean_stroke,
                          median_shape = median_shape, median_fill = median_fill, median_colour = median_colour,
                          show_mean = show_mean, show_median = show_median, averages_opacity = averages_opacity,
                          confidence_interval = confidence_interval)
  if(removeYAxisText){
    stats_plot <- stats_plot +
      ggplot2::theme(axis.text.y = element_blank())
  }

  vertical_plot <- verticalPlot(plot = stats_plot, vertical = vertical, mirrored = mirrored) # note, this plot will not be vertical if vertical = F

  return(vertical_plot)
}


#' creates a basic horizontal sea stack plot
#'
#' created a plot made up of a histogram for each group stacked vertically, split by \code{\link{facet_grid}}
#'
#' @param colour character string vector of colour(s) for outline of histogram, will be repeated across groups if too short. Default is grey50.
#' @param fill character string vector of fill for histogram, will be repeated across groups if too short. Default is 'grey90'.
#' @param alpha alpha value for histogram, between 0 and 1. Default is 0.7.
#'
#' @inheritParams summaryStats
#' @inheritParams ggplot2::geom_histogram
#'
#' @return a ggplot object with faceted histograms
#'
#' @examples
#' seaStackPlotBasic(df = df,
#' value = 'Value',
#' group = 'Group',
#' fill = c('blue', 'yellow', 'green'),
#' binwidth = 10)
#'
#' @export
#'
seaStackPlotBasic <- function(df, value, group, colour = "grey50", fill = "grey90", alpha = 0.7, bins = NULL, binwidth = NULL){
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



