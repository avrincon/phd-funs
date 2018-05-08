#' Check partner
#'
#' Check if action_partner was also a focal animal on the date of the action
#'
#' @param x A data frame. Needs columns "focal_animal" and "action_partner"
#'
#' @return Adds a new column "check_partner" to x, with "yes" or "no" if action partner was also a focal that day.
#' Also adds "rowid" column. Need this so that there are no duplicate rows, so that merging later works properly
#' @export

check_part <- function(x) {
  x <-
    x %>%
    tibble::rowid_to_column() %>%
    dplyr::mutate(
      dyad = paste(focal_animal, action_partner, sep = "-"),
      check_partner = ifelse(is.na(match(paste0(date, action_partner),
                                         paste0(date, focal_animal))),
                             "no", "yes"))
}
