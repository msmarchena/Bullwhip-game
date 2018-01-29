library(shiny)

#' Server Module for Result Graph
#'
#' @param input        standard \code{shiny} input
#' @param output       standard \code{shiny} output
#' @param session      standard \code{shiny} session
#' @param orders       to plot the graph
#'
#' @export
#'
srvModGraph <-     function(input, output, session,
                              orders = NULL) {
  ns <- session$ns
  
  # reactives
  # values
  orders_re <- reactive({
    if(is.reactive(orders)){
      return(orders())
    } else{
      return(orders)
    }
  })
  
  # outputs
  ####################################################################################################################    
  ## Bullwhip  line graph
  ####################################################################################################################  
  
  output$bullwhip_plot<- renderPlotly({ 
    require(plotly)
    plot_ly(orders_re(), x = 1:nrow(orders_re()), y = ~Customer, name = 'Customer', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~Retailer, name = 'Retailer', mode = 'lines') %>%
      add_trace(y = ~Wholesaler, name = 'Wholesaler', mode = 'lines') %>%
      add_trace(y = ~Distributor, name = 'Distributor', mode = 'lines') %>%
      add_trace(y = ~Factory, name = 'Factory', mode = 'lines') %>%
      layout(xaxis = list(title = "Time"),
             yaxis = list(title = "Order"),
             legend = list(x = 0.1, y = 0.9)
      )
  })
}