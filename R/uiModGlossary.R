library(shiny)

#' User Interface Module for the Glossary
#'
#' @param id character, used to identify a namespace
#' other parameters , see \code{shiny::\link[shiny]{tabPanel}}
#' @return a \code{shiny::\link[shiny]{tabPanel}} containing UI elements
#'
#' @export
#'
uiModGlossary <- function(id,
                             title = id,
                             ...,
                             value = title) {
  ns <- shiny::NS(id)
  tabPanel(title,
           h3( 'Formulas and definitions'),
           br(),
           uiOutput(outputId = ns("notation")),
           tags$hr(),
           uiOutput(outputId = ns("formulas"))
  )
}