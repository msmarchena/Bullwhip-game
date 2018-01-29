library(shiny)
library(DT)

#' Server Module for Parameters
#'
#' @param input        standard \code{shiny} input
#' @param output       standard \code{shiny} output
#' @param session      standard \code{shiny} session
#'
#' @return param_rv
#' @export
#'
srvModParam <-     function(input, output, session) {
  ns <- session$ns
  
  # reactives
  param_rv <- reactiveValues(servl = 0.95,
                             lt = 1,
                             forecast_model = 'SMA',
                             sma_periods = 5,
                             es_alpha = 0.2,
                             holdingcost = 0.5,
                             backlogcost = 2)
  observe ({
    param_rv$servl = as.numeric(input$sl)
    output$default_sl <- renderPrint({param_rv$servl})
    param_rv$lt = as.numeric(input$L)
    output$default_lt <- renderPrint({param_rv$lt})
    param_rv$forcast_model = input$forecast
    output$default_forecast <- renderPrint({param_rv$forcast_model})
    param_rv$sma_periods = as.numeric(input$periods)
    output$default_periods <- renderPrint({param_rv$sma_periods})
    param_rv$es_alpha = as.numeric(input$alpha)
    output$default_alpha <- renderPrint({param_rv$es_alpha})
    param_rv$holdingcost = as.numeric(input$h_cost)
    output$default_h_cost <- renderPrint({param_rv$holdingcost})
    param_rv$backlogcost = as.numeric(input$backlog_cost)
    output$default_backlog_cost <- renderPrint({param_rv$backlogcost})
  })
  
  return(reactive(param_rv))
}