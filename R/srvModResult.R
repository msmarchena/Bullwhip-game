library(shiny)
library(DT)

#' Server Module for Results
#'
#' @param input        standard \code{shiny} input
#' @param output       standard \code{shiny} output
#' @param session      standard \code{shiny} session
#' @param values 
#'
#' @export
#'
srvModResult <-     function(input, output, session,
                              result = NULL) {
  ns <- session$ns
  # reactives
  result_rv <- reactiveVal()

  # events on parameter values from calling module
  observe({
    req(result)
    if(is.reactive(result)){
      result_rv(result())
    } else{
      result_rv(result)
    }
  })

  # outputs
  output$Results <- DT::renderDataTable({
    DT::datatable(result_rv(),
                  options = list(lengthMenu = c(5, 25, 50), 
                                 pageLength = 25))
  })
}