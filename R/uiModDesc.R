library(shiny)

#' User Interface Module for the Description
#'
#' @param id character, used to identify a namespace
#' other parameters , see \code{shiny::\link[shiny]{tabPanel}}
#' @return a \code{shiny::\link[shiny]{tabPanel}} containing UI elements
#'
#' @export
#'
uiModDesc <- function(id,
                             title = id,
                             ...,
                             value = title) {
  ns <- shiny::NS(id)
  tabPanel(title,
           tabsetPanel(
             tabPanel(
               "Overview",
               br(),
               p(strong('Classic bullwhipgame')),
               
               'The bullwhipgame is an educational game that has as purpose the illustration and exploration of
               the',
               em('bullwhip effect,'),
               'i.e, the increase in demand variability along the supply chain.',
               br(),
               br(),
               p(
                 'The game simulates the distribution process of a single product that uses a four stages supply chain:
                 reailer, wholesaler, distributor and factory. The members of the supply chain need to
                 meet customer demand with minimal shortage situations and inventory cost, while satisfying service level requirements. All
                 participants use the same inventory replenishment policy, forecast method, delivery lead time and service level.
                 Holding and shortage cost are fixed and information sharing and cooperation is not allowed.'
               )
               ),
             tabPanel(
               "Rules",
               sidebarPanel(
                 p(strong('Players:')),
                 p('One player which has the role of the final customer. '),
                 p(strong('Dynamics of the game:')),
                 p(
                   'Each company in the system is the customer of upstream and a supplier of downstream company in
                   the supply chain. For instance, the retailer observes customer demand and based on its current inventory situation places orders to its supplier,
                   in this case the wholesaler. She receives orders made after a delivery lead time.
                   '
                 ),
                 p(strong('Goal:')),
                 p('To minimize the total inventory cost in the supply chain. '),
                 p(strong('Results:')),
                 'After 10 interactions the total cost is displayed. A line graph of demand variability
                 in the supply chain ',
                 em('(bullwhip effect)'),
                 ' is also shown'
                 
                 ),
               mainPanel(br(), br(),
                         div(
                           img(
                             src = 'chain_425x550.png',
                             height = 450,
                             width = 540
                           ), style = "text-align: center;"
                         ))#endmainPanel
               ),
             tabPanel(
               "How To Play",
               br(),
               p(strong('Instructions')),
               p(
                 ' - You need to set up the participants inputs tab before start to play. All
                 companies follow the same parameters and use the same Order Up to Level (OUT) replenishment policy. '
               ),
               p(
                 ' - The mean demand forecast is calculated using one of the following methods: simple moving average, exponential smoothing
                 or autoregressive model order one . The default simple moving average method requires the number of periods to be used to calculate
                 the mean demand. In the case of the exponential smoothing method, you need to choose the smoothing parameter.
                 Default values for lead time, service level, holding and shortage cost may be changed. '
               ),
               p(
                 '- In the play tab, insert a value for the customer demand and click the "Update" button.'
               ),
               p(
                 '- Each time a value is updated, the reult tables of all participants are displayed in the main panel.
                 Note that there are alredy initial values.
                 '
               ),
               p(
                 '- The results are displayed after 10 interactions, but you can continue to play. '
               ),
               p(
                 '- Use the glossary tab to understand how variables are calculated.  '
               )
               )
               )
           )
}