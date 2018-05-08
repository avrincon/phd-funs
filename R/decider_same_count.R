#' Decider same count
#'
#' Helper Function for deciding which focal interaction to keep, when dyad counts are the same. Will decide to keep if its ID comes before the action partner alphabetically
#'
#' #' This function will be used inside the "decider" function.
#'
#' @param focal_animal ID of animal that was being focalled, as a character string
#' @param action_partner ID of action partner of interaction, as a character string
#'
#' @return A string with the decision, either "include" or "exclude"
#' @export

decider_same_count <- function(focal_animal, action_partner){ # focal and participants of a dyad
  # create vector and sort in aphabetical order
  x <- sort(c(focal_animal, action_partner))
  # if focal is first element in vector, then result/decision is "include"
  if(x[1] == focal_animal){
    return("include")
    # if focal is NOT first element in vector, then result/decision is "exclude"
  } else {
    return("exclude")
  }
}
