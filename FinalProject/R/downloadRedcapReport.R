#' Download Redcap Report
#'
#' This function downloads a Redcap report using the Redcap API.
#'
#' @param redcapTokenName The name of the environment variable storing the Redcap API token.
#' @param redcapUrl The URL of the Redcap API.
#' @param redcapReportId The ID of the report you want to retrieve.
#'
#' @return A tibble containing the contents of the Redcap report.
#'
#' @import httr tibble
#' @export
#'
#' @examples
#' \dontrun{
#' # Replace 'redcapTokenName', 'redcapUrl', and 'redcapReportId' with actual values
#' redcap_report <- downloadRedcapReport("redcapToken", "https://redcap.emory.edu/api/", "46524")
#' print(redcap_report)
#' }
#'
#' @importFrom utils read.csv
#' @importFrom tibble as_tibble
#' @importFrom httr POST
#' @importFrom httr content

downloadRedcapReport <- function(redcapTokenName, redcapUrl, redcapReportId) {

  token <- Sys.getenv(redcapTokenName)


  formData <- list(
    "token" = token,
    "content" = 'report',
    "format" = 'csv',
    "report_id" = redcapReportId,
    "csvDelimiter" = '',
    "rawOrLabel" = 'raw',
    "rawOrLabelHeaders" = 'raw',
    "exportCheckboxLabel" = 'false',
    "returnFormat" = 'csv'
  )


  response <- httr::POST(redcapUrl, body = formData, encode = "form")

  result <- httr::content(response)

  report_tibble <- as_tibble(read.csv(text = result))

  return(report_tibble)
}
