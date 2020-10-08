#' Standard error of the mean
#' @param x a vector of values for which the SEM is to be calculated
#' @param na.rm logical, TRUE to remove NA values from x (that will result in NA being returned), defaults to FALSE
sem <- function(x, na.rm = F) {

  if(na.rm == TRUE){
    x <- x[!is.na(x)]
  }

  sd(x)/sqrt(length(x))
}

#' Calculate Summary Statistics
#' Calculate summary stats using the package dplyr
#' Here, we are starting with our data (df), and then grouping the data by the column Group
#' For each group, we are calculating the mean, standard deviation (SD), median,
#' standard error of the mean (SEM) and the 1.96 * SEM (part of the 95% CI)
#' @param df data frame or tibble of values with at least column for grouping variable and column for values
#' @param group name of the column containing grouping variable, should be given as a character string
#' @param value name of the column containing values, should be given as a character string
#' @param confidence_interval desired size of the confidence interval, value between 0 and 1 e.g. 0.95 for 95% (default)
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
