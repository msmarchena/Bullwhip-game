library(shiny)
library(tidyverse)
#' Server Module for a Company
#'
#' @param input        standard \code{shiny} input
#' @param output       standard \code{shiny} output
#' @param session      standard \code{shiny} session
#' @param initial      initial values 
#' @param demand       demand company
#' @param supply       supply company 
#'
#' @return company_rv
#' @export
#'
srvModCompany <-     function(input, output, session,
                                     inputs = NULL,
                                     demand = NULL,
                                     supply = NULL) {
  ns <- session$ns
  # reactives
  company_rv <- reactiveVal()
  demand_rv <- reactiveVal()
  supply_rv <- reactiveVal()
  initial_rv <- reactiveVal()
  param_rv <- reactiveVal()
  
  # events on initial values from calling module
  observe({
    req(inputs(), inputs()$initial)
    initial_rv(inputs()$initial)
  })

  # events on parameter values from calling module
  observe({
    req(inputs(), inputs()$param)
    param_rv(inputs()$param)
  })
  
  # events on supply
  observe({
    req(supply)
    if(is.reactive(supply)){
      supply_rv(supply())
    } else{
      supply_rv(supply)
    }
  })
  
  # events on demand
  observe({
    req(demand)
    if(is.reactive(demand)){
      demand_rv(demand())
    } else{
      demand_rv(demand)
    }
  })
  
  # events on new demands
  observeEvent(demand_rv(),{
    req(demand_rv(), company_rv(), param_rv())
    if(dplyr::count(demand_rv()) > dplyr::count(company_rv())){
      new <- company_rv() %>% tail(1)
      # Is a customer no company?
      if(length(demand_rv()) < length(company_rv())){
        new$Demand <- demand_rv()$Demand %>% tail(1)
      } else{
        new$Demand <- demand_rv()$Order %>% head(-1) %>% tail(1)
      }
      new$Receive <- mylag(company_rv()$Order, param_rv()$lt)
      new$NS <- tail(company_rv()$NS, 1) + new$Receive - new$Demand  
      new$Forecast <- switch(param_rv()$forecast_model,
                             'SMA' = mean(tail(company_rv()$Demand, param_rv()$sma_periods)),
                             'ES' = param_rv()$es_alpha * tail(company_rv()$Demand, 1) + (1 - param_rv()$es_alpha) * tail(company_rv()$Forecast, 1),
                             'MMSE' = { r_model<- arima( company_rv()$Demand, order = c(1,0,0), method = "ML" )
                             ar1_r <- predict(r_model, 1)
                             new$Forecast <- round(ar1_r$pred[1],2)
                             }
      )
      new$SD <- round(sd(tail(company_rv()$Demand, param_rv()$sma_periods)),2)
      new$LTD <- param_rv()$lt * new$Forecast
      new$SS <- round(qnorm(p = param_rv()$servl, mean = 0, sd = 1) * new$SD * sqrt(param_rv()$lt),2)
      new$OUT <- new$LTD + new$SS
      new$Order <- new$OUT - new$NS
      new$Cost <- if(is.na(new$NS)){ return() }
      else{if(new$NS > 0){new$NS * param_rv()$holdingcost }
        else{  abs(new$NS * param_rv()$backlogcost) }
      }
      
      ##simulated bullwhip measure
      var_demand <- var(company_rv()$Demand)
      var_order <- var(company_rv()$Order)
      new$Bullwhip <-   var_order/var_demand
      
      # update the new here:
      new <- new %>% round(3)
      
      #store the result
      res <- company_rv() %>% bind_rows(new)
      company_rv(res)
    }
  })
  
  # events on initial values
  observeEvent(initial_rv(),{
    if(!isTruthy(company_rv())){
      company_rv(initial_rv())
    } else { 
    company_rv({
      initial_rv() %>%
      bind_rows({
        dplyr::company_rv() %>% tail(-dplyr::count(initial_rv()))
        })
      })
    }
  })
  
  # events on deleted demands => cut values
  observeEvent({
    req(demand_rv(), company_rv())
    (dplyr::count(demand_rv()) < dplyr::count(company_rv()))
  },
  {
    company_rv({
      company_rv() %>% head(dplyr::count(demand_rv()))
    })
  })
  
  # modules
  callModule(module = srvModResult,
             id = "Result",
             values = company_rv)
  param_rv <- callModule(module = srvModParam,
                           id = "Param")
  initial_rv <- callModule(module = srvModInitialValues,
                           id = "Initial",
                           initial = initial_rv)
  
  # outputs
  return(company_rv)
}