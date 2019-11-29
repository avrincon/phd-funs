#' Within-subjects centering
#' 
#' Centers a number of behaviours within-subjects by subtracting the subject's mean value from each observation value.
#'
#' @param x A data frame with different behaviours as different columns.
#' @param behaviours Behaviours/variables to calculate mean and center within-subjects
#' @param subject_var Unquoted name of subjects column in x
#'
#' @return A data frame with new columns with means and centered values for each behaviour.
#' 
#' @import dplyr
#' @import tidyr
#' 
#' @references van de Pol and Wright (2009). A simple method for distinguishing within- versus between-subject effects using mixed models.
#' 
#' @export

wtn_sbj_c <- function(x, behaviours, subject_var) {
  group_var <- enquo(subject_var)
  
  out <- 
    x %>% 
    # gather behaviours into one column to calculate mean per subject
    gather(key_1, value, behaviours) %>% 
    group_by(!!group_var, key_1) %>% 
    mutate(mean = mean(value)) %>% 
    ungroup() %>% 
    # center by subtracting mean from value
    mutate(wtn = value - mean) %>% 
    gather(key_2, value_2, value, mean, wtn) %>% 
    # spread behaviours back into columns
    unite(key_3, key_1, key_2, sep = "_") %>% 
    spread(key_3, value_2)
  
}