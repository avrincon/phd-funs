#' Plot correlation between two vars
#'
#' @description Plots a scatter plot of x and y and includes a regression line.
#'
#' @param dataset a data frame or tibble
#' @param xvar variable to plot on the x axis, as a character string. NA values are silently dropped.
#' @param yvar variable to plot on the y axis, as a character string. NA values are silently dropped.
#'
#' @return A ggplot object which may be further customised with e.g. theme() options.
#' @export
#' @import ggplot2
#' @examples 
#' plot_cor(mtcars, "wt", "mpg")

plot_cor <- function(dataset, xvar, yvar) {
  dataset %>% 
    tidyr::drop_na(xvar, yvar) %>%
    ggplot(aes_string(xvar, yvar)) + 
    geom_point() + 
    geom_smooth(method = "lm")
}