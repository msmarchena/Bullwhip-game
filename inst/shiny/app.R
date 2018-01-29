library(shiny)
library(tidyverse)
library(newbullwhipgame)

ui <- shinyUI(
  navbarPage(
    "newbullwhipgame",
    id = "mainNavbarPage",
    theme = "flatky.css",
    uiModDesc("Description"),
    uiModPlay("Play"),
    uiModGlossary("Glossary"),
    uiModAbout("About")
  ))

server <- shinyServer(function(input, output, session) {
  ns <- session$ns

  # reactive values
  inputs_rv <- reactiveVal()
  
  callModule(module = srvModPlay,
                           id = "Play")
  callModule(module = srvModGlossary,
             id = "Glossary")
})

shinyApp(ui, server)
