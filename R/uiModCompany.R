library(shiny)
library(DT)

#' User Interface Module for a Company
#'
#' @param id character, used to identify a namespace
#' other parameters , see \code{shiny::\link[shiny]{tabPanel}}
#' @return a \code{shiny::\link[shiny]{tabPanel}} containing UI elements
#'
#' @export
#'
uiModCompany <- function(id,
                         title = id,
                         ...,
                         value = title) {
  ns <- shiny::NS(id)
  tabPanel(
    title,
    tabsetPanel(
      uiModResult(ns("Result"), title = "Result"),
      uiModParam(ns("Param"), title = "Parameter"),
      uiModInitialValues(ns("Initial"), title = "Initial Values")
    )
  )
}