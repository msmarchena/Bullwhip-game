library(shiny)
library(DT)

#' User Interface Module for Orders
#'
#' @param id character, used to identify a namespace
#' other parameters , see \code{shiny::\link[shiny]{tabPanel}}
#' @return a \code{shiny::\link[shiny]{tabPanel}} containing UI elements
#'
#' @export
#'
uiModOrders <- function(id,
                         title = id,
                         ...,
                         value = title) {
  ns <- shiny::NS(id)
  tabPanel(
    title,
    br(),
    h4('Resume of orders'),
    br(),
    DT::dataTableOutput(outputId = ns("perceivedTab")))
}