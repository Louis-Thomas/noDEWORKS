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
library(shinyscreenshot)

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
            #fileInput("file", NULL, buttonLabel = "Upload...", accept = c(".RData", ".rda")),
            #actionButton("bupload", "Press to Load own dataset"),
            downloadButton("download", "Download RDA file"),
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
          actionButton("Plot", "Plot Network"),
          actionButton("screenshot", "Take a screen-shot of the plot")
        ),        
        mainPanel(
          visNetworkOutput(outputId = "NetworkPlot", width = "100%", height = "1000px")
        )
      )
    ),
    tabPanel("Help",
             h3("How to use the app"),
             p("Nodes and Edges"),
             p("How we came up with the name:"),
             p("Brain storm session"),
             #img(src="https://github.com/Louis-Thomas/noDEWORKS/blob/main/test/shinyscreenshot.png"),
             img(
               #src = "https://github.com/Louis-Thomas/noDEWORKS/blob/main/test/shinyscreenshot.png",
               src = "https://github.com/Louis-Thomas/noDEWORKS/blob/main/test/Brainstorm.png",
               #src = "./test/Brainstorm.png",
               alt = "Image of us brainstorming the name: https://github.com/Louis-Thomas/noDEWORKS/blob/main/test/shinyscreenshot.png",
               width = 25, height = 25
             ),
             #img(src = "./test/shinyscreenshot.png"),
             hr(),
             h4("Current Plot"),
             p("This current plot"),
             hr(),
             p("Adding nodes: Input name and create node. "),
             p("Adding edges, make sure they have they exist in the node table first"),
             p("Reset the table you can using reset."),
             hr(),
             p("If you want to take a screenshot of the plot, press the button"),
             p("You can move the nodes around in the plot. Just hold and move."),
             img(
               src = "https://github.com/Louis-Thomas/noDEWORKS/blob/main/test/shinyscreenshot.png",
               alt = "Image of me adding and moving nodes around: https://github.com/Louis-Thomas/noDEWORKS/blob/main/test/shinyscreenshot.png",
               width = 25, height = 25
             ),             
             p("This image is what I created. This is the map of darksouls 1. It is moved around to match what the actual map would like")
             )
  )
  

    
    #plotOutput("networkPlot")
)
