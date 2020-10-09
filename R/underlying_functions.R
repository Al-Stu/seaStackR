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
