#' Unscale a scaled vector
#'
#' This function unscales a vector that was previously scaled using the `scale` function in R.
#'
#' @param x A numeric vector to be unscaled.
#' @return The unscaled vector.
#' @examples
#' scaled_vector <- scale(c(1, 2, 3, 4, 5))
#' unscaled_vector <- unscale(scaled_vector)
#' unscaled_vector
#' # Output:  1  2  3  4  5
#'
#' @export
unscale <- function(x) {

  attr <- attributes(x)

  if (!is.null(attr) && "scaled:center" %in% names(attr) && "scaled:scale" %in% names(attr)) {

    center <- attr[["scaled:center"]]
    scale <- attr[["scaled:scale"]]

    unscaled_x <- x * scale + center
    return(unscaled_x)
  } else {

    return(x)
  }
}
