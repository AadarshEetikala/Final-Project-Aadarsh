logLikBernoulli <- function(data) {
  p_values <- seq(0, 1, by = 0.001)


  log_likelihoods <- sapply(p_values, function(p) {
    sum(data * log(p) + (1 - data) * log(1 - p))
  })

  max_index <- which.max(log_likelihoods)

  return(p_values[max_index])
}
