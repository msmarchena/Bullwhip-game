library(shiny)
library(DT)

#' User Interface Module Play
#'
#' @param id character, used to identify a namespace
#' other parameters , see \code{shiny::\link[shiny]{tabPanel}}
#' @return a \code{shiny::\link[shiny]{tabPanel}} containing UI elements
#'
#' @export
#'
uiModPlay <- function(id,
                         title = id,
                         ...,
                         value = title) {
  ns <- shiny::NS(id)
  tabPanel(
    title,
    sidebarPanel(width = 3,
                 fileInput(
                   inputId = ns("UploadCustomerDemand"),
                   label = "Upload Customer demand from CSV file",
                   accept = c("CSV Customer demand",
                              "CSV file",
                              ".csv")),
                 numericInput(inputId = ns("c1"),
                              "Single Customer demand", NA),
                 tags$p(actionButton(inputId = ns("update"), "Update")),
                 tags$p(actionButton(inputId = ns("reset"), "Clear")),
                 tags$p(actionButton(inputId = ns("restart"), "Restart")),
                 textOutput(outputId = ns("count")),
                 br(),
                 htmlOutput(outputId = ns("display"))
                 
    ),
    mainPanel( width = 9,
               br(),
               tabsetPanel(
                 uiModCompany(ns("Retailer"), title = "Retailer"),
                 uiModCompany(ns("Wholesaler"), title = "Wholesaler"),
                 uiModCompany(ns("Distributor"), title = "Distributor"),
                 uiModCompany(ns("Factory"), title = "Factory"),
                 uiModOrders(ns("Orders"), title = "Orders"),
                 uiModGraph(ns("Graph"), title = "Graph")
               ))
  )
}