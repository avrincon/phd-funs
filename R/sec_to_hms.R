#' Function that converts a number of seconds to a character format hh:m:ss
#' @param secs a number of seconds to be formatted
#' @description This function converts a number of seconds to a character format hh:m:ss
#' @export

sec_to_hms <- function(secs) {

  if(! is.numeric(secs)){
    stop(paste("secs must be of class numeric"))

    } else {
    # integer division to ger the number of full min
    full.mins <- secs %/% 60

    # change duration format from period to character hh:mm:ss
    # integer division to get the number of full hours
    paste(formatC((full.mins %/% 60), width = 2, format = "d", flag = "0"),
          # remainder of the division is the number of minutes
          formatC((full.mins %% 60), width = 2, format = "d", flag = "0"),
          # remainder of the division is the number of secs
          formatC((secs %% 60), width = 2, format = "d", flag = "0"),
          sep = ":")
  }
}
