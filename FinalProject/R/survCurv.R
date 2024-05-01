#' Plot a Kaplan-Meier survival curve
#'
#' This function plots a Kaplan-Meier survival curve based on survival times and censoring statuses.
#'
#' @param status A vector of censoring statuses (0 for censored, 1 for event).
#' @param time A vector of survival times.
#' @import survival
#' @examples
#' # Example usage:
#' data <- read.csv("https://jlucasmckay.bmi.emory.edu/global/bmi510/Labs-Materials/survival.csv")
#' survCurv(data$status, data$time)
#'
#' @export
survCurv <- function(status, time) {
  surv_object <- survival::Surv(time, status)
  surv_fit <- survival::survfit(surv_object ~ 1)
  plot(surv_fit, main = "Survival Curve", xlab = "Time", ylab = "Survival Probability", col = "blue")
}
