############ RIDGE SEA STACK PLOT ###############

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

seaStackPlotRidge <- function(df, value, group,
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
                          mean_stroke = mean_stroke, median_shape = median_shape, median_fill = median_fill,
                          median_colour = median_colour, show_mean = show_mean, show_median = show_median,
                          averages_opacity = averages_opacity, df_stats = df_stats)

  vertical_plot <- verticalPlot(plot = stats_plot, vertical = vertical, mirrored = mirrored)

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
    binwidth <- (ceiling(max(renamed_df$value, na.rm = T)) - floor(min(renamed_df$value, na.rm = T)))/bins
  }

  overall_breaks <- seq(from = floor(min(renamed_df$value, na.rm = T)),
                        to = ceiling(max(renamed_df$value, na.rm = T)),
                        by = binwidth)
}


#' get groups histogram data for seaStackPlotRidge
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

