library(shiny)
library(DT)

#' Server Module for Orders
#'
#' @param input        standard \code{shiny} input
#' @param output       standard \code{shiny} output
#' @param session      standard \code{shiny} session
#' @param orders       to be shown in a table
#'
#' @export
#'
srvModOrders <-     function(input, output, session,
                              orders = NULL) {
  ns <- session$ns
  # reactives
  # orders
  orders_re <- reactive({
    if(is.reactive(orders)){
      return(orders())
    } 
    return(orders)
  })
  
  # outputs
  output$perceivedTab<- DT::renderDataTable(orders_re(),
                                       # colnames = c('Time' = 1),
                                       options = list(lengthMenu = c(5, 25, 50), pageLength = 25)
  )
}