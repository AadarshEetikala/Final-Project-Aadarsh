#' Compute the maximum likelihood estimate of the success probability for a Bernoulli trial
#'
#' This function computes the maximum likelihood estimate of the success probability (p)
#' for a series of Bernoulli trials given the observed data.
#'
#' @param data A vector of 0s and 1s representing the outcomes of Bernoulli trials.
#' @return The maximum likelihood estimate of the success probability.
#' @examples
#' # Example usage:
#' data <- c(0, 1, 1, 0, 1)
#' logLikBernoulli(data)
#'
#' @importFrom stats log
logLikBernoulli <- function(data) {
  p_values <- seq(0, 1, by = 0.001)

  log_likelihoods <- sapply(p_values, function(p) {
    sum(data * log(p) + (1 - data) * log(1 - p))
  })

  max_index <- which.max(log_likelihoods)

  return(p_values[max_index])
}

