#' Add row with 0 observation time
#'
#' @description
#' function to add a row to obs_time for dra and fro who had 0 observation hours
#' (were not focalled)
#'
#' @param obs_time data frame created by obs_time_calculator()
#' @param focal_animal character string of focal ID
#'
#' @return A data frame with an extra row
#' @export
#'

add_zero_obstime <- function(obs_time, focal_animal){
  out <- dplyr::add_row(obs_time, focal_animal = focal_animal,
                        focal_time = "00:00:00",
                        out_of_sight_time = "00:00:00",
                        total_obs_time = "00:00:00")
}
