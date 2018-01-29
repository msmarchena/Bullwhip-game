library(shiny)
library(DT)

#' User Interface Module for Parameters
#'
#' @param id character, used to identify a namespace
#' other parameters , see \code{shiny::\link[shiny]{tabPanel}}
#' @return a \code{shiny::\link[shiny]{tabPanel}} containing UI elements
#' @export
#'
uiModParam <- function(id,
                         title = id,
                         ...,
                         value = title) {
  ns <- shiny::NS(id)
  tabPanel(
    title,
    fluidRow(
      column(12,
             h4('Before to start playing you need to setup the parameters below. All the participants follow the same parameters')
      )#endcolumn
    ),#endfluidRow
    br(),
    fluidRow(
      column(6,
             selectInput(inputId = ns("forecast"), label = "Forecast method", choices = list("Simple Moving Average (SMA)"="SMA",
                                                                                         "Exponential Smoothing (ES)"="ES", "Autoregressive model - AR(1)"="MMSE"), selected="SMA")
      ),#endcolumn
      column(4,
             conditionalPanel(ns = ns,
                              condition = "input.forecast == 'SMA'",
                              numericInput(ns("periods"),"Number of periods to be used", 5)
             ),#endconditionalPanel
             conditionalPanel(ns = ns,
                              condition = "input.forecast == 'ES'",
                              numericInput(ns("alpha"),"Smoothing parameter", 0.20),
                              helpText('A number between 0 and 1')
             )
      )#endcolumn
    ),#endfluidRow
    br(),
    fluidRow(
      column(4,
             selectInput(inputId = ns("L"),label = "Lead time",choices = c(1, 2, 3, 4, 5), selected=1)
      ),#endcolumn
      column(2),
      column(4,
             selectInput(inputId = ns("sl"),label = "Service level", choices = c(0.95, 0.97, 0.99), selected=0.95)
      )#endcolumn
    ),#endfluidRow
    br(),
    fluidRow(
      column(4,
             numericInput(inputId = ns("h_cost"), "Holding cost", 0.5)
      ),#endcolumn
      column(2),
      column(4,
             numericInput(inputId = ns("backlog_cost"), "Shortage Penalty cost", 2)
      )#endcolumn
    ),#endfluidRow
    helpText('Holding and shortage penalty cost must be positive numbers')
  )
}