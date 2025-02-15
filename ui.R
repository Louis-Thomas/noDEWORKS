#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)
library(tidyverse)
library(visNetwork)
library(geomnet)
library(igraph)

df <- dplyr::tibble(id = character(), label = character())

# Define UI for application that draws a histogram
fluidPage(

    titlePanel("Creating Interactive Node and Edge Networks"),
    # Application title
    
  navbarPage("Networks",
    tabPanel("Data-set",
      # Sidebar with a slider input for number of bins
      sidebarLayout(
          sidebarPanel(
            textInput(inputId = "Nname", label = "Node Name"),
            actionButton("add_node_row", "Make new node"),
            actionButton("del_node_row", "Delete node"),
            #actionButton("update_node", "Update node"),
            hr(),
            textInput(inputId = "Efrom", label = "From an Edge"),
            textInput(inputId = "Eto", label = "To an Edge"),
            numericInput(inputId = "Ewidth", label = "Edge width", value = 1),
            actionButton("add_edge_row", "Make new edge"),
            actionButton("del_edge_row", "Delete edge"),
            hr(),
            actionButton("table_arrange", "Arrange Both Tables"),
            actionButton("reset", "Reset Table"),
            #actionButton("update_edge", "Update edge"), 
            h5(id = "EdgeUpdater","Static text to change when making new edges"),
            hr(),
            fileInput("upload", NULL, buttonLabel = "Upload...", multiple = TRUE, accept = ".rda"),
            actionButton("bupload", "Press to Load own dataset"),
            downloadButton("download", "Download RDS file"),
          ),
  
          # Show a plot of the generated distribution
          mainPanel(
            DTOutput(outputId = "editable_Ntable"),
            DTOutput("editable_Etable")
          )
      )
    ),
    tabPanel("Plot",
      sidebarLayout(
        sidebarPanel(
          actionButton("Plot", "Plot Network")
        ),        
        mainPanel(
          visNetworkOutput(outputId = "NetworkPlot", width = "100%", height = "1000px")
        )
      )
    ),
    tabPanel("Help",
             )
  )
  

    
    #plotOutput("networkPlot")
)
