#' Compute minimum sample size for t-tests
#'
#' This function computes the minimum sample size required to achieve a specified power for one-sample or two-sample t-tests.
#'
#' @param x1 A numeric vector representing the data for one-sample or the first group for two-sample t-tests.
#' @param x2 A numeric vector representing the data for the second group for two-sample t-tests (optional).
#' @return The minimum sample size required to achieve the specified power.
#' @importFrom pwr pwr.t.test pwr.t2n.test
#' @examples
#' # For one-sample test
#' x1 <- rnorm(20)
#' minimumN(x1)
#'
#' # For two-sample test
#' x2 <- rnorm(30)
#' minimumN(x1, x2)
#'
#' @export
minimumN <- function(x1, x2 = NULL) {
  if (is.null(x2)) {
    # One-sample t-test
    pwr.t.test(d = mean(x1) / sd(x1), sig.level = 0.05, power = 0.8, type = "one.sample")$n
  } else {
    # Two-sample t-test
    pwr.t2n.test(
      n1 = length(x1),
      n2 = length(x2),
      sig.level = 0.05,
      power = 0.8,
      alternative = "two.sided"
    )$n
  }
}
