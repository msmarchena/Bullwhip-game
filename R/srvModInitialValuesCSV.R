library(shiny)
library(DT)

#' Server Module for Initial Values CSV file Down/Up-load
#'
#' @param input        standard \code{shiny} input
#' @param output       standard \code{shiny} output
#' @param session      standard \code{shiny} session
#' @param initial      initial values
#'
#' @return initial_rv
#' @export
#'
srvModInitialValuesCSV <-     function(input, output, session, 
                                    initial = NULL) {
  ns <- session$ns
  # reactives
  initial_rv <- reactiveVal()
  filename_re <- reactive({
    ns("InitalValues.csv")
  })
  
  # events
  # initializing values
  observe({
    initial_rv(bullwhipgame::initVal)  
  })

  # changes from calling module
  observe({
    req(initial)
    if(is.reactive(initial)){
      initial_rv(initial())
    } else {
      initial_rv(initial)
    }
  })

  # inputs
  observeEvent(input$UploadInital, {
    req(input$UploadInital, input$UploadInital$datapath)
    file_all <- isolate(input$UploadInital)
    initial <- read.csv2(file = file_all$datapath)
    initial_rv(initial %>% select(names(initial_rv())))
  })
  
  # outputs
  output$InitialValues <- DT::renderDataTable(initial_rv(),
                                       options = list(lengthMenu = c(5, 25, 50), pageLength = 25)
  )
  
  output$DownloadInitial <- downloadHandler(
    filename = function(){
      paste0(ns("Values"), ".csv")
    },
    content = function(file) {
      write.csv2(initial_rv(), file = file)
      # file.rename(fname,file)
    },
    contentType="application/csv"
  )
  
  return(initial_rv)
}