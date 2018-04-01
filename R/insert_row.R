#' Insert a new row into a data frame
#'
#' @param df a data frame
#' @param newrow new row to insert, must have same columns as df
#' @param r row index indicating where new row will be inserted
#'
#' @return
#' Returns a data frame with an extra row inserted at specified index
#' @export
#'
#' @examples
#' # example dataframe
#' d <- as.data.frame(matrix(seq(20), nrow=5, ncol=4))
#'
#'# row number that you want to insert
#' r <- 3
#'
#' # create a new row to insert
#' newrow <- seq(4)
#' # can be a subset of original dataframe
#' newrow2 <- d[1, ]
#'
#' # insert row
#' insert_row(d, newrow, r)
#' insert_row(d, newrow2, r)

insert_row <- function(df, newrow, r) {
  df[seq(r + 1, nrow(df) + 1), ] <- df[seq(r, nrow(df)), ]
  df[r, ] <- newrow
  df
}

