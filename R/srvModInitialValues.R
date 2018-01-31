library(shiny)
library(DT)

#' Server Module for Initial Values
#'
#' @param input        standard \code{shiny} input
#' @param output       standard \code{shiny} output
#' @param session      standard \code{shiny} session
#' @param initial      initial values
#'
#' @return initial_rv
#' @export
#'
srvModInitialValues <-     function(input, output, session, 
                                    initial = NULL) {
  ns <- session$ns
  # reactives
  initial_rv <- reactiveVal()
  filename_re <- reactive({
    ns("InitalValues.xlsx")
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
    require(openxlsx)
    req(input$UploadInital, input$UploadInital$datapath)
    file_all <- isolate(input$UploadInital)
    sheetName <- "InitialValues"
    initial_rv(openxlsx::readWorkbook(xlsxFile = file_all$datapath,
                                          sheet = sheetName,
                                          detectDates = TRUE))
  })
  
  # outputs
  output$InitialValues <- DT::renderDataTable(initial_rv(),
                                       options = list(lengthMenu = c(5, 25, 50), pageLength = 25)
  )
  
  output$DownloadInitial <- downloadHandler(
    filename = function(){
      paste0(ns("Values"), ".xlsx")
    },
    content = function(file) {
      require(openxlsx)
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb = wb, sheetName = "InitialValues")
      openxlsx::writeDataTable(wb = wb, sheet = 1, x = initial_rv())
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      # file.rename(fname,file)
    },
    contentType="application/xlsx"
  )
  
  return(initial_rv)
}