#' Map function to list-column
#'
#' 1. Nest data by grouping variable(s)
#' 2. Map function to nested list column
#' 3. Unest data frame
#'
#' Duplicate vars if grouping vars are also used by f
#'
#' @param x A data frame
#' @param ... Variables to group_by() (comma-separated and unquoted)
#' @param f A function passed on to map()
#'
#' @return Returns unested x after mapping function
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import purrr
#'
map_nest <- function(x, ..., f) {
  group_var <- purrr::quos(...)
  f <- purrr::enquo(f)

  x <- x %>%
    # nest dataframe based on (duplicated) groups
    dplyr::group_by(!!!group_var) %>%
    tidyr::nest() %>%
    # map function to list column
    dplyr::mutate(new_data = purrr::map(data, (!!f))) %>%
    # unnest new data
    tidyr::unnest(new_data)
}
