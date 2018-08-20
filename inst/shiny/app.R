library(shiny)
library(tidyverse)
library(bullwhipgame)

ui <- shinyUI(
  navbarPage(
    "bullwhipgame",
    id = "mainNavbarPage",
    theme = "flatly.css",
    uiModDesc("Description"),
    uiModInitialValuesCSV("Initial", title = "Initial Values"),
    uiModPlay("Play"),
    uiModGlossary("Glossary"),
    uiModAbout("About")
  ))

server <- shinyServer(function(input, output, session) {
  ns <- session$ns

  # reactive values
  initial_rv <- reactiveVal()

  observe({
    initial_rv(bullwhipgame::initVal %>% select(Demand))
  })
  
  initial_rv <- callModule(module = srvModInitialValuesCSV,
                           id = "Initial",
                           initial = initial_rv)
  callModule(module = srvModPlay,
                           id = "Play",
             initial = initial_rv)
  callModule(module = srvModGlossary,
             id = "Glossary")
})

shinyApp(ui, server)
