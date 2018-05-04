#' Plot line graph by group
#'
#' @description Plots a line graph connecting mean e.g. hormone values per day/week/month per subject. Manullay scales point shape and colour of up to 14 subjects so that each is easily identifiable. 
#'
#' @param dataframe a data frame or tibble
#' @param xvar variable to plot on the x axis, as a character string
#' @param yvar variable to plot on the y axis, as a character string. NA values are silently dropped.
#' @param grp_var variable to group by, as a character string. Default set to "focal_animal".
#'
#' @return A ggplot object which may be further customised with e.g. theme() options.
#' @export
#' @import ggplot2

plot_ln_grp <- function(dataframe, xvar, yvar, grp_var = "focal_animal") {
  
  dataframe %>% 
    tidyr::drop_na(yvar) %>% 
    ggplot(aes_string(x = xvar, y = yvar, group = grp_var)) +
    geom_point(aes_string(color = grp_var, 
                          shape = grp_var, 
                          fill = grp_var),
               size = 2) + 
    geom_line(aes_string(color = grp_var)) +
    scale_shape_manual(values = c(15,17,19,18,15,17,19,18,15,17,19,18,15,17))
}