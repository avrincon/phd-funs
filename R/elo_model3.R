#' Get likelihood of elo-ratings 
#' 
#' @description 
#' This function calulates the log likelihood of the winning probability of winner,
#' or generates an elo sequence.
#' 
#' Function taken from Foerster et al. 2016
#'
#' @param par numeric vector of values. par[1] will be used as initial value of k. 
#' rest of vector will be used as starting elo values
#' @param IA_data A data frame with sequencce of Winner/Loser interactions. 
#' @param all_ids A character vector os unique individual IDs
#' @param return_likelihood If TRUE (default), returns a list with parameters estimated by maximum likelihood.
#' If FALSE, returns a IA_data with updated elo scores given the k value and initial scores set with 'par'. 
#'
#' @return
#' A likelihood value or elo sequence
#' 
#' @export
#'
#' @references 
#' Foerster, S., Franz, M., Murray, C. M., Gilby, I. C., Feldblum, J. T., Walker, K. K., & Pusey, A. E. (2016). Chimpanzee females queue but males compete for social status. Scientific Reports, 6, 35404. https://doi.org/10.1038/srep35404
#' 
#' @examples
#' 
#' # Calculate maximum likelihood
#' res_m_model3 <- optim(par=c(5, rep(0, length(ama_c))), 
#'                       elo.model3, 
#'                       all_ids = ama_c, 
#'                       IA_data = dom.ama2, 
#'                       return_likelihood=T, 
#'                       method='BFGS', 
#'                       control = list(maxit = 10000, reltol=1e-10))
#'                       
#' # Get K
#' k <- exp(res_m_model3$par[1])
#' # Get AIC
#' AIC <- res_m_model3$value * 2 + 2 * (length(ama_c) + 1)
#' 
#' # Get elo sequence given ML k and starting values
#' ama_seq3.2 <- elo.model3(par = res_m_model3$par, 
#'                          IA_data = dom.ama2, 
#'                          all_ids = ama_c, 
#'                          return_likelihood = FALSE)

elo_model3 <- function(par, IA_data, all_ids, return_likelihood = T)
{
  k <- par[1]
  init_elo <- par[2:length(par)]
  # Initialize output columns
  if (!return_likelihood) IA_data$elo_l_before <- IA_data$elo_w_before <- IA_data$elo_l_after <- IA_data$elo_w_after <- NA
  # Set intitial elo scores
  currentELO <- c(init_elo)
  names(currentELO) <- all_ids
  # Initialize the log likelihood
  L <- 0
  # Start loop
  for(i in 1:nrow(IA_data))
  {
    ind1 <- which(names(currentELO)==IA_data$winner[i])
    ind2 <- which(names(currentELO)==IA_data$loser[i])
    if (!return_likelihood)
    {
      IA_data$elo_w_before[i] <- currentELO[ind1]
      IA_data$elo_l_before[i] <- currentELO[ind2]
    }
    # calculate predited winning probablity of the winner
    p_win <- 1/(1+exp(-.01*(currentELO[ind1] - currentELO[ind2])))
    # Calculation of new ELO scores
    currentELO[ind1] <- currentELO[ind1] + exp(k) * (1 - p_win) # new Elo score of the Winner
    currentELO[ind2] <- currentELO[ind2] - exp(k) * (1 - p_win) # new Elo score of the Loser
    # write calculated elo scores to output columns
    if (!return_likelihood)
    {
      IA_data$elo_w_after[i] <- currentELO[ind1]
      IA_data$elo_l_after[i] <- currentELO[ind2]
    }
    # Update log likelihood
    L <- L + log(p_win)
  }
  if (return_likelihood) return(-1*L)
  else return(IA_data)
}