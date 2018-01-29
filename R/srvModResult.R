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
                              values = NULL) {
  ns <- session$ns
  # reactives
  # values
  values_re <- reactive({
    if(is.reactive(values)){
      return(values())
    } 
      return(values)
  })
  
  # outputs
  output$Results<- DT::renderDataTable(values_re(),
                                       # colnames = c('Time' = 1),
                                       options = list(lengthMenu = c(5, 25, 50), pageLength = 25)
  )
}