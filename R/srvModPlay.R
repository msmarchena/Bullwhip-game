library(shiny)
library(DT)
#' Server Module Play
#'
#' @param input        standard \code{shiny} input
#' @param output       standard \code{shiny} output
#' @param session      standard \code{shiny} session
#'
#' @return customer_rv
#' @export
#'
srvModPlay <-     function(input, output, session,
                           initial = NULL) {
  ns <- session$ns
  # reactive values
  inputs_rv <- reactiveVal()
  initial_rv <- reactiveVal()
  customer_rv <- reactiveVal()
  
  # events on initial values
  observe({
    req(initial)
    if(is.reactive(initial)){
      initial_rv(initial())
    } else{
      initial_rv(initial)
    }
    customer_rv(initial_rv() %>% select(Demand))
    updateCounter$i <- 0 
  })
  
  # reactive expressions
  demand_re <- reactive({
    req(customer_rv(), factory_re())
    cc <- dplyr::count(customer_rv())
    cf <- dplyr::count(factory_re())
    if(cc == cf){
      tibble(Customer = customer_rv()$Demand,
             Retailer = retailer_re()$Order,
             Wholesaler = wholesaler_re()$Order,
             Distributor = distributor_re()$Order,
             Factory = factory_re()$Order
             )
    }
  })
  
  # event on restart button
  observeEvent(input$restart,{ 
    customer_rv(initial_rv() %>% select(Demand))
    updateCounter$i <- 0 
  })  
  
  ####################################################################################################################
  # delete the last row of all tables
  ####################################################################################################################  
  observeEvent(input$reset,{   
    customer <- customer_rv() %>% head(-1)
    customer_rv(customer)
  })
  
  # update values table on button click
  observeEvent(input$update,{
    #add a row
    customer <- customer_rv()
    new_row <- customer %>% tail(1)
    new_row$Demand <- round(as.numeric(input$c1),3)
    # add the new customer demand
    customer <- customer %>% bind_rows(new_row)
    customer_rv(customer)
  })
  
  # modules
  retailer_re <- callModule(module = srvModCompany,
                            id = "Retailer",
                            initial = initial_rv,
                            demand = customer_rv,
                            supply = wholesaler_re)
  wholesaler_re <- callModule(module = srvModCompany,
                           id = "Wholesaler",
                           initial = initial_rv,
                           demand = retailer_re,
                           supply = distributor_re)
  distributor_re <- callModule(module = srvModCompany,
                           id = "Distributor",
                           initial = initial_rv,
                           demand = wholesaler_re,
                           supply = factory_re)
  factory_re <- callModule(module = srvModCompany,
                           id = "Factory",
                           initial = initial_rv,
                           demand = distributor_re)
  callModule(module = srvModOrders,
             id = "Orders",
             orders = demand_re)
  callModule(module = srvModGraph,
             id = "Graph",
             orders = demand_re)
  callModule(module = srvModGlossary,
             id = "Glossary")
  
  # inputs
  observeEvent(input$UploadCustomerDemand, {
    req(input$UploadCustomerDemand, input$UploadCustomerDemand$datapath)
    file_all <- isolate(input$UploadCustomerDemand)
    customer <- customer_rv()
    new_rows <- read.csv2(file = file_all$datapath) %>% select(Demand)
    updateCounter$i <- updateCounter$i + dplyr::count(new_rows)
    customer <- customer %>% bind_rows(new_rows)
    customer_rv(customer)
  })
  
  # outputs
  ##################################################################################################################
  # Display main results
  ##################################################################################################################
  sc_partialcost_re <- reactive({
    req(factory_re(),
        distributor_re(),
        wholesaler_re(),
        retailer_re())
    if (dplyr::count(factory_re()) > 14){
      sum(retailer_re()$Cost[1:15]) +
        sum(wholesaler_re()$Cost[1:15]) +
        sum(distributor_re()$Cost[1:15]) +
        sum(factory_re()$Cost[1:15]) 
    }
  })
  
  sc_totalcost_re <- reactive({
    req(sc_partialcost_re())
    if (dplyr::count(factory_re()) > 15){
      sum(retailer_re()$Cost) +
      sum(wholesaler_re()$Cost) +
      sum(distributor_re()$Cost) +
      sum(factory_re()$Cost) 
    }
  })
  
  output$display  <- renderUI({ 
    req(sc_partialcost_re())
    str_pc <- paste("Partial cost:", sc_partialcost_re())
    str1 <- paste("Retailer:", sum(retailer_re()$Cost[1:15]))
    str2 <- paste("Wholesaler:", sum(wholesaler_re()$Cost[1:15]))
    str3 <- paste("Distributor:", sum(distributor_re()$Cost[1:15]))
    str4 <- paste("Factory:", sum(factory_re()$Cost[1:15]))
    partial_cost <- HTML(paste(str_pc, str1, str2, str3, str4, sep = '<br/>'))
    result <- partial_cost
    if(isTruthy(sc_totalcost_re())){
      str_tc <- paste("Total cost:", sc_totalcost_re())
      str5 <- paste("Retailer:", sum(retailer_re()$Cost))
      str6 <- paste("Wholesaler:", sum(wholesaler_re()$Cost))
      str7 <- paste("Distributor:", sum(distributor_re()$Cost))
      str8 <- paste("Factory:", sum(factory_re()$Cost))
      total_cost <- HTML(paste(str_tc, str5, str6, str7, str8, sep = '<br/>'))
      result <- HTML(paste(partial_cost, total_cost, sep = '<br/><br/>'))
    }
    return(result)
  })
    ####################################################################################################################  
    #reactive counter modified
    ####################################################################################################################  
    updateCounter <- reactiveValues(i = 0)
    
    output$count <- renderText({
      paste0("Iteractions: ", updateCounter$i)
    })
    
    observe({
      input$update
      
      isolate({
        updateCounter$i <- updateCounter$i + 1
      })
    })
    
    observe({
      input$reset
      isolate(updateCounter$i <- updateCounter$i - 1)
    })

    return(customer_rv)
}