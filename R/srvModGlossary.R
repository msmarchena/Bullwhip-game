library(shiny)

#' Server Module for the Glossary
#'
#' @param input        standard \code{shiny} input
#' @param output       standard \code{shiny} output
#' @param session      standard \code{shiny} session
#'
#' @export
#'
srvModGlossary <-     function(input, output, session) {
  ns <- session$ns
  output$notation <- renderUI({
    fluidRow(
      column(12, 
             strong('Back orders (B),'), 'orders that can not be filled at current time.',
             br(),   
             strong('Bullwhip,'), ' simulated measure of the increase in demand variability that occurs at each echelon of the supply chain',
             br(), 
             strong('Cost,'), 'the inventory cost in current period.',
             br(), 
             strong('Demand (D),'), 'current demand.',
             br(),    
             strong('Forecast'), 'refers to the mean demand estimation.',
             br(),
             strong('Lead time (L)'), 'is the number of days between the time an order is placed and the time it is received.',
             br(),
             strong('Lead time demand (LTD)'), 'is the average demand during lead time.',
             br(),
             strong('Net stock (NS)'), 'is defined here as a current inventory level. ',
             br(),
             strong('Order (O),'), 'stocks ordered but not yet arrived. ',
             br(),
             strong('Order Up to Level (OUT)'), 'is a replenishment policy. Each period companies review stock levels and place an order to bring its stock levels up to a target level.',
             br(),
             strong(' Receive'), 'refers to the arrival of orders placed L periods ago.' ,
             br(),
             strong('Safety factor (z)'), 'is a constant associated with the service level, also called the z-score.',
             br(),
             strong('Safety stock (SS),'), 'the amount of inventory that companies keep in order to protect theirselves against stockout situations during lead time. ',
             br(),
             strong('Service level (SL),'), 'the probability of not running out of stock during the next replenishment cycle.',
             br(),  
             strong('Standart deviation (Std)'), 'of demand.'
      )#endcolumn     
    )#endfluidRow
  })#endrenderUI
  
  
  output$formulas <- renderUI({
    fluidRow(
      column(4, 
             withMathJax('Receive = O(-L) '),  
             br(),br(),
             withMathJax('NS = NS(-1) + Receive - D'),
             br(),br(),
             # withMathJax('b=\\frac{1}{2)'), 
             withMathJax('OUT = LTD + SS')
      ),#endcolumn
      column(4, 
             withMathJax('O = OUT - NS'),
             br(),br(),
             withMathJax('LTD = Forecast*L'),
             br(),br(),
             withMathJax('SS = z*Std*\\(\\sqrt{L}\\)')
             
      ),#endcolumn 
      column(4, 
             withMathJax('Bullwhip = Var(O)/Var(D)'),
             br(),br()
             
      )#endcolumn 
    )#endfluidRow
  })#endrenderUI
}