library(shiny)

#' User Interface Module for Result Graph
#'
#' @param id character, used to identify a namespace
#' other parameters , see \code{shiny::\link[shiny]{tabPanel}}
#' @return a \code{shiny::\link[shiny]{tabPanel}} containing UI elements
#'
#' @export
#'
uiModGraph <- function(id,
                         title = id,
                         ...,
                         value = title) {
  require(plotly)
  ns <- shiny::NS(id)
  tabPanel(
    title,
    br(),
    h4('Orders plot'),
    plotlyOutput(outputId = ns("bullwhip_plot"))
  )
}