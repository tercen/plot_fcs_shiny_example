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
#### This part should not be included in ui.R and server.R scripts
# http://localhost:5402/admin/w/9bc1fd64ee4d8642eb4c61d22c237705/ds/515f20f0-154b-409e-8f2c-28b8ecf96a42
# http://localhost:5402/admin/w/9e55fb72f96b231f30fa4ef71912b8bc/ds/4cdbb206-a801-4959-86b2-623185ea3656
getCtx <- function(session) {
  ctx <- tercenCtx(stepId = "4cdbb206-a801-4959-86b2-623185ea3656",
                   workflowId = "9e55fb72f96b231f30fa4ef71912b8bc")
  return(ctx)
}
####
############################################

biexp2_trans <- function(lim = 100, decade.size = lim){
  trans <- function(x){
    ifelse(x <= lim,
           x,
           lim + decade.size * (suppressWarnings(log(x, 10)) -
                                  log(lim, 10)))
  }
  inv <- function(x) {
    ifelse(x <= lim,
           x,
           10^(((x-lim)/decade.size) + log(lim,10)))
  }
  breaks <- function(x) {
    if (all(x <= lim)) {
      pretty_breaks()(x)
    } else if (all(x > lim)) {
      log_breaks(10)(x)
    } else {
      unique(c(pretty_breaks()(c(x[1],lim)),
               log_breaks(10)(c(lim, x[2]))))
    }
  }
  trans_new(paste0("biexp-",format(lim)), trans, inv, breaks)
}


ui <- shinyUI(fluidPage(
  
  titlePanel("Flow Cytometry Plot"),
  
  sidebarPanel(
    textInput("title", "Graph title label", ""),
    textInput("xlab", "X-axis label", ""),
    textInput("ylab", "Y-axis label", ""),
    textInput("legend", "Legend title label", ""),
    textInput("breaks_x", "Breaks for X-Axis", 
              "0, 1e1, 1e2, 1e3, 1e4"),
    textInput("breaks_y", "Breaks for Y-Axis", 
              "0, 1e1, 1e2, 1e3, 1e4"),
    sliderInput("plotWidth", "Plot width (px)", 200, 2000, 500),
    sliderInput("plotHeight", "Plot height (px)", 200, 2000, 500),
    sliderInput("widthBasisX", "Width Basis (X)", -2000, 0, -10),
    sliderInput("widthBasisY", "Width Basis (Y)", -2000, 0, -10),
    sliderInput("negX", "Negative range in asymptotic decades (X)", 
                0, 50, 0, step=0.05),
    sliderInput("negY", "Negative range in asymptotic decades (Y)",
                0, 50, 0, step=0.05),
    sliderInput("posX", "Positive range in asymptotic decades (X)", 
                0, 50, 4.5, step=0.05),
    sliderInput("posY", "Positive range in asymptotic decades (Y)",
                0, 50, 4.5, step=0.05),
    checkboxInput("labs", "Apply labels", 0),
    checkboxInput("wrap", "Wrap panel grid", 0),
    checkboxInput("fixed", "Axes equal for panels", 0),
    checkboxInput("space", "Equal FCS space", 0),
  ),
  
  mainPanel(
    uiOutput("reacOut"),
  )
  
))

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

runApp(shinyApp(ui, server))  
