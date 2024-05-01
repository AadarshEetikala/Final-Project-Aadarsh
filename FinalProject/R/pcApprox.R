#' Approximate data using principal components
#'
#' This function approximates the input data matrix using a specified number of principal components.
#'
#' @param x A numeric matrix representing the data to be approximated.
#' @param npc An integer specifying the number of principal components to use for approximation.
#' @return A matrix containing the approximation of the input data using the specified number of principal components.
#' @examples
#' # Generate some sample data
#' set.seed(123)
#' data <- matrix(rnorm(100), ncol = 10)
#'
#' # Specify the number of principal components
#' npc <- 2
#'
#' # Call the pcApprox function to approximate the data using npc principal components
#' approximation <- pcApprox(data, npc)
#'
#' # Print the original data and the approximation
#' print("Original Data:")
#' print(data)
#'
#' print("Approximation:")
#' print(approximation)
#'
#' @export
pcApprox <- function(x, npc) {

  pca_result <- prcomp(x)
  pcs <- pca_result$x[, 1:npc]
  approximation <- pcs %*% t(pca_result$rotation[, 1:npc])

  scaled_approximation <- scale(approximation, center = FALSE, scale = apply(x, 2, sd))
  centered_scaled_approximation <- scaled_approximation + colMeans(x)

  return(centered_scaled_approximation)
}
