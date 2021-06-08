library(shiny)
library(tercen)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggcyto)
library(flowWorkspace)
library(flowCore)
library(scales)

############################################
#### This part should not be modified
getCtx <- function(session) {
  # retreive url query parameters provided by tercen
  query <- parseQueryString(session$clientData$url_search)
  token <- query[["token"]]
  taskId <- query[["taskId"]]
  
  # create a Tercen context object using the token
  ctx <- tercenCtx(taskId = taskId, authToken = token)
  return(ctx)
}
####
############################################

server <- shinyServer(function(input, output, session) {
  
  dataInput <- reactive({
    getValues(session)
  })
  
  output$reacOut <- renderUI({
    plotOutput(
      "main.plot",
      height = input$plotHeight,
      width = input$plotWidth
    )
  }) 
  
  output$main.plot <- renderPlot({
    df <- dataInput()
    breaks_x <- as.numeric(unlist(strsplit(input$breaks_x, ",")))
    breaks_y <- as.numeric(unlist(strsplit(input$breaks_y, ",")))
    
    # ggplot object
    plt = ggplot() +
      scale_x_flowjo_biexp(widthBasis=input$widthBasisX, equal.space=input$space, 
                           neg=input$negX, pos=input$posX,
                           limits=c(min(breaks_x), max(breaks_x)), 
                           breaks=breaks_x) +
      scale_y_flowjo_biexp(widthBasis=input$widthBasisY, equal.space=input$space, 
                           neg=input$negY, pos=input$posY, 
                           limits=c(min(breaks_y), max(breaks_y)), 
                           breaks=breaks_y)
    
    
    plt = plt + geom_point(data=df, 
                           mapping=aes(x=.x, y=.y, colour=colors),
                           shape=1) +
      # labels
      labs(x = input$xlab, 
           y = input$ylab,
           color= input$legend) +
      ggtitle(input$title) +
      
      
      # theme stuff
      theme(legend.position="right",
            plot.title = element_text(hjust = 0.5)) +
      theme_bw()
    
    plt
  })
  
})

getValues <- function(session){
  ctx <- getCtx(session)
  df <- ctx %>% select(.x, .y, .ri, .ci) %>%
    group_by(.ri)
  
  colors <- 0
  if(length(ctx$colors)) colors <- ctx$select(ctx$colors[[1]])[[1]]
  if(!all(is.numeric(colors))) colors = as.factor(colors)
  
  tcn.labels <- NA
  sizes = NA
  if(length(ctx$labels)) tcn.labels <- ctx$select(ctx$labels[[1]])[[1]]
  if (!all(is.numeric(tcn.labels))) {
    labels = as.factor(tcn.labels)
  } else {
    sizes = tcn.labels
  }
  
  df = df %>% data.frame(labels, colors, sizes)
  
  cnames = ctx$cselect()[[1]]
  
  rnames = ctx$rselect()[[1]]
  df = df %>% 
    mutate(colors = colors, labels = labels) %>%
    left_join(data.frame(.ri = 0:(length(rnames)-1), rnames), by = ".ri") %>%
    left_join(data.frame(.ci = 0:(length(cnames)-1), cnames), by = ".ci")
  
  return(df)
}
