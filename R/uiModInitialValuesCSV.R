library(shiny)
library(DT)

#' User Interface Module for Initial Values CSV file Down/Up-load
#'
#' @param id character, used to identify a namespace
#' other parameters , see \code{shiny::\link[shiny]{tabPanel}}
#' @return a \code{shiny::\link[shiny]{tabPanel}} containing UI elements
#'
#' @export
#'
uiModInitialValuesCSV <- function(id,
                         title = id,
                         ...,
                         value = title) {
  ns <- shiny::NS(id)
  tabPanel(
    title,
    br(),
    h4('Initial Values'),
    br(),
    DT::dataTableOutput(outputId = ns("InitialValues")),
    downloadLink(outputId = ns("DownloadInitial"),
                 "Download Initial Values to CSV file"
    ),
    fileInput(
      inputId = ns("UploadInital"),
      label = "Upload Initial Values from CSV file",
      accept = c("CSV Initial Values",
                 "CSV file",
                 ".csv"))
  )
}