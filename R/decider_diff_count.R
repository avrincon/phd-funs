#' Decider different count
#'
#' Helper function for deciding which focal interaction to keep, when dyad counts are different. Will decide to keep whichever focal has the highest dyad count.
#'
#' This function will be used inside the "decider" function.
#'
#' @param focal_animal ID of animal that was being focalled, as a character string
#' @param action_partner ID of action partner of interaction, as a character string
#'
#' @return A string with the decision, either "include" or "exclude"
#' @export
#'

decider_diff_count <- function(focal_dyad_count, partner_dyad_count){ # focal and participants of a dyad
  # if focal_dyad_count is greater than partner_dyad_count, print include
  if(focal_dyad_count > partner_dyad_count){
    return("include")
    # if focal is NOT bigger than partner_dyad_count, print exclude
  } else {
    return("exclude")
  }
}
