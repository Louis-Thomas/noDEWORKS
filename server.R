#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(tidyverse)
library(visNetwork)
library(geomnet)
library(igraph)
library(shinyscreenshot)
library(vroom)

#places <- sort(c("Undead Asylum", "Firelink Shrine", "Undead Burg", "Undead Parish", "Darkroot Garden", "Darkroot Basin", "Valley of Drakes", "Blighttown", "Quelaag's Domain", "Depths", "Sen's Fortress", "Anor Londo", "Painted World of Ariamis", "The Catacombs", "Tomb of the Giants", "New Londo Ruins", "The Duke's Archives", "Crystal Cave", "Demon Ruins", "Lost Izalith", "Kiln of the First Flame", "Ash Lake", "Northern Undead Asylum", "Oolacile Township", "Chasm of the Abyss", "Royal Wood", "Sanctuary Garden"))
#places <- sort(c("Undead Asylum", "Firelink Shrine", "Undead Burg", "Undead Parish", "Darkroot Garden", "Darkroot Basin", "Valley of Drakes", "Blighttown", "Quelaag's Domain", "Depths", "Sen's Fortress", "Anor Londo", "Painted World of Ariamis", "The Catacombs", "Tomb of the Giants", "New Londo Ruins", "The Duke's Archives", "Crystal Cave", "Demon Ruins", "Lost Izalith", "Kiln of the First Flame", "Ash Lake", "Northern Undead Asylum", "Oolacile Township", "Chasm of the Abyss", "Royal Wood", "Sanctuary Garden"))
#node_preset_data <- data.frame(
#  id = places,
#  label = places
#)
#edge_preset_data <- data.frame(
#  from = c("Undead Asylum", "Firelink Shrine", "Firelink Shrine", "Undead Burg", "Undead Burg", "Undead Parish", "Undead Parish", "Darkroot Garden", "Darkroot Basin", "Valley of Drakes", "Blighttown", "Quelaag's Domain", "Depths", "Sen's Fortress", "Anor Londo", "Painted World of Ariamis", "The Catacombs", "Tomb of the Giants", "New Londo Ruins", "The Duke's Archives", "Crystal Cave", "Demon Ruins", "Lost Izalith", "Kiln of the First Flame", "Ash Lake", "Northern Undead Asylum", "Oolacile Township", "Chasm of the Abyss", "Royal Wood", "Sanctuary Garden", "Depths", "The Duke's Archives"),
#  to = c("Firelink Shrine", "Undead Burg", "New Londo Ruins", "Undead Parish", "Lower Undead Burg", "Darkroot Garden", "Sen's Fortress", "Darkroot Basin", "Valley of Drakes", "Blighttown", "Quelaag's Domain", "Demon Ruins", "Blighttown", "Anor Londo", "Painted World of Ariamis", "The Duke's Archives", "Tomb of the Giants", "Lost Izalith", "The Duke's Archives", "Crystal Cave", "Demon Ruins", "Lost Izalith", "Kiln of the First Flame", "Ash Lake", "Northern Undead Asylum", "Oolacile Township", "Chasm of the Abyss", "Royal Wood", "Sanctuary Garden", "Oolacile Township", "Undead Burg", "Anor Londo"),
#  width = c(2, 3, 4, 4, 5, 3, 6, 4, 5, 6, 7, 8, 6, 7, 8, 7, 9, 10, 8, 9, 8, 9, 10, 7, 3, 6, 8, 7, 5, 6, 4, 6)+5
#) %>% arrange(from)

#node_preset_data <- data_list$nodes
#edge_preset_data <- data_list$edges
load("./Network.rda")


# Define server logic required to draw a histogram
function(input, output, session) {
  
  # Node side of the plots:
  #node_wdata <- reactiveValues(df = node_preset_data, 
  #                             row_selected = NULL)
  node_wdata <- reactiveValues(df = data_list$nodes, 
                               row_selected = NULL)
  
  
  #output$editable_Ntable <- renderDT(node_wdata$df, selection = "single")  
  output$editable_Ntable <- renderDT(node_wdata$df %>% arrange(id), selection = "single",
                                     options = list(pageLength = 7))  
  
  #observe({updateSelectInput(session, inputId = "del_node_row")})
  
  # The node data displayed on the website.  
  # Saw in https://stackoverflow.com/questions/70186250/add-remove-and-edit-rows-in-a-dtdatatable-of-a-shiny-app
  ## Based most of the table code on this: https://thatdatatho.com/r-shiny-data-table-proxy-replace-data/
  
  observeEvent(input$add_node_row, {
    
    add_node <- input$Nname
    df <- node_wdata$df
    prev_node <- str_split(df$label, "_", simplify = TRUE)
      print(prev_node)
    if(add_node %in% prev_node == TRUE) {
      # The nodes must all be unique id's
      add_node <- invisible(paste(add_node, "Copy", 
                                  sum(add_node == prev_node), 
                                  sep = "_"))
    } 
    node_wdata$df <- node_wdata$df %>%
      bind_rows(
        data.frame(id = add_node, label = add_node)
        )
  })
  shiny::observeEvent(input$del_node_row, {
    del_row <- as.numeric(input$editable_Ntable_rows_selected) # get selected rows
    if(!is.null(del_row)){
      node_wdata$df <- node_wdata$df[-del_row, ]
    }
  })
  proxy <- DT::dataTableProxy('editable_Ntable')
  observe({
    replaceData(proxy, node_wdata$df)
  })
  
  # Edge side of the plots:
  edge_wdata <- reactiveValues(df = data_list$edges,
                               row_selected = NULL)
  
  output$editable_Etable <- renderDT(edge_wdata$df, selection = "single",
                                     options = list(pageLength = 7))  
  
  observeEvent(input$add_edge_row, {
    # Before Running this code, we need to do some checks:
    new_data <- data.frame(from = input$Efrom, to = input$Eto, width = input$Ewidth)
    vec_nodes <- node_wdata$df$label
      # Will be used to check if given node exists
    # Igraph has it's own checks when there are multiple connections
      ## But it only displays one edge if there are multiple.  
    if(new_data$from %in% vec_nodes & new_data$to %in% vec_nodes) {
      edge_wdata$df <- edge_wdata$df %>%
        bind_rows(
          new_data
        )      
    } else {
      showModal(modalDialog(title = "Edge Error",
                            "Looks like your from node or to node doesn't exist in the node dataset",
                            footer = modalButton("Dismiss")))
    }
  })
  observeEvent(input$del_edge_row, {
    del_row <- as.numeric(input$editable_Etable_rows_selected)
    if(!is.null(del_row)) {
      edge_wdata$df <- edge_wdata$df[-del_row, ]
    }
  })
  proxy <- DT::dataTableProxy('editable_Etable')
  observe({
    replaceData(proxy, edge_wdata$df)
  })
  
  #output$NetworkPlot <- renderPlot(
  #  simple_graph(edge_wdata$df, node_wdata$df)
  #)
  
  observeEvent(input$Plot, {
    print("plotting")
    #output$NetworkPlot <- renderVisNetwork(simple_graph(edge_wdata$df, node_wdata$df))
    output$NetworkPlot <- renderVisNetwork({
      edges <- as.data.frame(edge_wdata$df)
      nodes <- as.data.frame(node_wdata$df)
      graph <- graph_from_data_frame(edges, directed = FALSE)
      
      cluster <- cluster_louvain(graph)
      
      cluster_df <- data.frame(as.list(membership(cluster)))
      cluster_df <- as.data.frame(t(cluster_df))
      cluster_df$label <- rownames(cluster_df)
      
      #Create group column
      nodes <- left_join(nodes, cluster_df, by = "label")
      
      #print(as.data.frame(cluster_df))
      colnames(nodes)[3] <- "group" # Doing this for colouring reasons
      #print(nodes)
      
      visNetwork(nodes, edges, width = "700px", height = "700px") %>%
        visIgraphLayout() %>%
        visNodes(
          font = list(size = 25, font = "Comic Sans MS"),
          shape = "dot",
          color = list(
            background = "#0085AF",
            border = "#013848",
            highlight = "#FF8000"
          ),
          shadow = list(enabled = TRUE, size = 10)
        ) %>%
        visEdges(
          shadow = FALSE,
          color = list(color = "#0085AF", highlight = "#C62F4B")
        ) %>%
        visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
                   selectedBy = "group") %>% 
        visLayout(randomSeed = 11)
      # This was taken from the visNetwork guide
      ## If I have time later I will do more to make it prettier
    })
    #simple_graph(edge_wdata$df, node_wdata$df)
  })
  ## Plotting code:
  
  observeEvent(input$table_arrange, {
    edge_wdata$df <- edge_wdata$df %>% arrange(from)
    node_wdata$df <- node_wdata$df %>% arrange(label)
  }) # Just to arrange to make it easier to view
  observeEvent(input$reset, {
    node_wdata$df <- data.frame(id = character(), label = character())
    edge_wdata$df <- data.frame(from = character(), to = character(), wdith = numeric())
  })
  observeEvent(input$bupload, {
    if(!is.null(input$file)) {
      #upload_data <- readRDS(input$upload$datapath)
      #upload_data <- load(input$upload$datapath)
      infile <- input$file
      file <- infile$datapath
      print(infile)
      e = new.env()
      upload_data <- load(input$upload$datapath, envir = e)
      
      #print(e[[1]])
      #nodes_wdata$df <- upload_data$nodes
      #edges_wdata$df <- upload_data$edges
    }
    #data_input <- input_data()
    
    #node_wdata$df <- data_input$nodes
    #edge_wdata$df <- data_input$edges
  })
  
    #input_data <- reactive({
    #  req(input$file)
    #  
    #  ext <- tools::file_ext(input$file$name)
    #  switch(ext, 
    #         RData = load(input$file$datapath), delim = ",",
    #         rda = load(input$file$datapath),
    #         validate("Invalid Input"))
    #})
  
  data_list <- reactiveValues()
  observe({
    if(!is.null(node_wdata$df) & !is.null(edge_wdata$df)) {
      isolate(
        data_list <<- list(nodes = node_wdata$df,
                  edges = edge_wdata$df)
      )
    }
  }) # Had to use stackoverflow for this - just couldn't solve it normally
  output$download <- downloadHandler(
    filename = function() {
      paste("Network.rda")
    },
    content = function(file){
      #print(data_list)
      #return(data_list)
      #saveRDS(data_list, file = "Network.RData")
      
      print(data_list$edges)
      save(data_list, file=file)
    }
  )
  outputOptions(output, "download", suspendWhenHidden = FALSE)
    ## Kept running into a download.htm being downloaded. Didn't work unfortunately
  observeEvent(input$screenshot, {
    ## Taking a screenshot of the plot  
    screenshot(selector = "#NetworkPlot", scale = 3)
  })
}