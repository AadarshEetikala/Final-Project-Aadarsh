#' Standardize variable names to small camel case
#'
#' @param data A tibble or data frame
#' @return A tibble with variable names standardized to small camel case
#' @importFrom dplyr rename_with
#' @importFrom janitor make_clean_names
#' @importFrom snakecase to_lower_camel_case
#' @examples
#' data <- tibble::tibble(
#'   This_Is_A_Test = 1,
#'   Another_Test = 2
#' )
#' standardizeNames(data)
standardizeNames <- function(data) {
  renamed_data <- data %>%
    rename_with(~to_lower_camel_case(.x), everything()) %>%
    janitor::make_clean_names(case = "snake")

  return(renamed_data)
}

