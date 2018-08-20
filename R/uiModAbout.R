library(shiny)

#' User Interface Module for the About
#'
#' @param id character, used to identify a namespace
#' other parameters , see \code{shiny::\link[shiny]{tabPanel}}
#' @return a \code{shiny::\link[shiny]{tabPanel}} containing UI elements
#'
#' @export
#'
uiModAbout <- function(id,
                             title = id,
                             ...,
                             value = title) {
  ns <- shiny::NS(id)
  tabPanel(title,
           fluidRow(
             column(12,
                    h4('About bullwhipgame'),
                    
                    p('The bullwhipgame is an Open Source project developed to illustrate and explore the', em('bullwhip effect.'),
                      'The main goal of our interactive tool is to present the dynamics of distribution of a product and to show typical problems arising
                      from a non-coordinated system. Our interactive tool use R programming language and Shiny
                      to offer an easy and friendly user experience.'),
                    br(), br(),
                    
                    p(' Created by ', strong('Marlene Silva Marchena')),
                    strong('Version: 0.1'),
                    br(),br(),
                    ' Code on  ', a("Github", href="https://github.com/msmarchena/Bullwhip-game",
                                    target="_blank"),
                    br(),br(),
                    strong('License:'), 'GPL3',
                    br(),br(),
                    img(src='gplv3-127x51.png',height=51, width=127),
                    br(),br(),
                    strong('Contact'),
                    br(),
                    'Comments, suggestions, bug report or just want to contribute to this project please send an email:',
                    img(src='email_Marlene.png'),
                    br(),br(),
                    strong('Acknowledgements'),
                    br(),
                    HTML(paste0(
                      'Bullwhip game is built using ',
                      a("R",
                        href="https://www.r-project.org/",
                        target="_blank"),
                      ' and ',
                      a("R Shiny",
                        href="http://shiny.rstudio.com",
                        target="_blank"),
                      ' framework',
                      ', with CSS from ',
                      a("Bootswatch",
                        href="http://bootswatch.com",
                        target="_blank"),
                      ' .'
                    ))
                    
             ) # end column
           ) # end fluidRow
  )#endtabPanel
}