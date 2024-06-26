% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survCurv.R
\name{survCurv}
\alias{survCurv}
\title{Plot a Kaplan-Meier survival curve}
\usage{
survCurv(status, time)
}
\arguments{
\item{status}{A vector of censoring statuses (0 for censored, 1 for event).}

\item{time}{A vector of survival times.}
}
\description{
This function plots a Kaplan-Meier survival curve based on survival times and censoring statuses.
}
\examples{
# Example usage:
data <- read.csv("https://jlucasmckay.bmi.emory.edu/global/bmi510/Labs-Materials/survival.csv")
survCurv(data$status, data$time)

}
