#' Plot the Time Series
#'
#' Plot the time series data beautifully with ggplot.
#'
#' @param data a tibble
#' @param x x
#' @param y y
#' @param date_labels the x label
#' @param date_breaks the period of the x label
#'
#' @import ggplot2
#'
#' @return A ggplot figure of the time series
#' @export
#'
#' @examples
#' date <- as.Date("2015-01-01") + days(0:180)
#' thero <- returns(rGbm("thero", date))[-1]
#' tthero <- tibble(x = date[-1], y = thero)
#' gglineplot(tthero, aes(x, y), "%Y/%m", "1 months")
gglineplot <- function(data, x, y, date_labels = "%Y/%m/%d", date_breaks = "2 weeks") {
  gg <- ggplot(data = data, mapping = aes(x, y)) +
    geom_line(color = "steelblue") +
    geom_point(shape = 2, color = "red") +
    scale_x_date(date_labels = date_labels, date_breaks = date_breaks) +
    theme_bw() +
    xlab(NULL) +
    ylab(NULL)
  return(gg)
}


#' Plot the Histogram Figure
#'
#' Plot the histgram figure beautifully with ggplot.
#'
#' @param data a tibble
#' @param x x
#' @param bins the number of bins
#'
#' @importFrom stats density
#'
#' @import ggplot2
#'
#' @return A histogram figure by ggplot
#' @export
#'
#' @examples
#' date <- as.Date("2015-01-01") + days(0:180)
#' thero <- returns(rGbm("thero", date))[-1]
#' tthero <- tibble(x = date[-1], y = thero)
#' gghistplot(tthero, aes(x = thero, y = stat(density)), bins = 20)
gghistplot <- function(data, x, bins = 10) {
  ggplot(data = data, mapping = aes(x = x, y = stat(density))) +
    geom_histogram(bins = bins, fill = "steelblue") +
    geom_density(color = "red") +
    theme_bw() +
    xlab(NULL) +
    ylab(NULL)
}
