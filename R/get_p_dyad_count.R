#' Get partner dyad count
#'
#' @param x A data frame. Needs columns "date", "focal_animal" and "action_partner"
#'
#' @return Adds new column "partner_dyad_count" to x. It is 0 when focal and partner were not focalled on the same day, and >0 when they were. The number indicates how many times they interacted that day from the focal animal's perspective.
#' @export


get_p_dyad_count <- function(x){
  # create vector with all date_focal_partner combinations - to be used in for loop
  date_focal_partner <- paste0(x$date, x$focal_animal, x$action_partner)

  # create action_partner partner_dyad_count column
  # whith count of dyads from action_partner's perspective
  for (i in seq_len(nrow(x))){
    # paste together the date, action_partner and focal
    # (needs to be this order, where focal and action_partner are switched in comparison to the vector dfp)
    date_partner_focal <-
      paste0(x$date[[i]], x$action_partner[[i]], x$focal_animal[[i]])

    if(date_partner_focal %in% date_focal_partner){
      # action_partner dyad count is the number of times which date_partner_focal ocurs in date_focal_partner
      x$partner_dyad_count[[i]] <-
        length(which(date_focal_partner == date_partner_focal))
    } else {
      # else dpf is NOT in dfp, therefore number of times is = 0
      x$partner_dyad_count[[i]] <- 0
    }
  }
  return(x)
}
