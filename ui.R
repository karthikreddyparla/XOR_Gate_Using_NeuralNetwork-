
library(shiny)
library(visNetwork)
library(dplyr)
library(igraph)
library(DT)
library(shinythemes)
header <- headerPanel("KarthikReddyParla_Neural_Network")
header[[2]]$attribs$id = "header"
ui <-(fluidPage(theme = shinytheme("cerulean"),
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                ),
                header,   
                sidebarPanel(
                  uiOutput('Xv'),
                  uiOutput('Yv')        
                )
                ,
                mainPanel(
                  fluidRow(verbatimTextOutput("Summary1"),br(),visNetworkOutput("plottest"))
                )
)
)