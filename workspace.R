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
getCtx <- function(session) {
  ctx <- tercenCtx(stepId = "c5515666-086f-44ab-ae2f-f78932c64898",
                   workflowId = "9bc1fd64ee4d8642eb4c61d22c237705")
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
    sliderInput("plotWidth", "Plot width (px)", 200, 2000, 500),
    sliderInput("plotHeight", "Plot height (px)", 200, 2000, 500),
    textInput("xlab", "x axis label", ""),
    textInput("ylab", "y axis label", ""),
    checkboxInput("labs", "Apply labels", 0),
    checkboxInput("wrap", "Wrap panel grid", 0),
    checkboxInput("fixed", "Axes equal for panels", 0)
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
    
    plt = ggplot(df, aes(.x, .y, color=colors, label=labels)) + 
      scale_y_continuous(trans=biexp2_trans(),
                         limits=c(0, 10000), breaks=c(0, 1, 10, 100, 1000, 10000),
                         labels=trans_format("log10", math_format(10^.x))) +
      scale_x_continuous(trans=biexp2_trans(),
                         limits=c(0, 10000), breaks=c(0, 1, 10, 100, 1000, 10000),
                         labels=trans_format("log10", math_format(10^.x))) +
      
      labs(x = input$xlab, y = input$ylab) +
      theme_bw() +
      theme(legend.position=NULL)
    
    if(input$labs & !all(is.na(df$labels))) {
      plt <- plt + geom_point() + geom_text_repel()
    }
    else if(input$labs & !all(is.na(df$sizes))){
      plt = plt + geom_point(aes(size = sizes))
    }
    else{
      plt = plt + geom_point()
    }
    
    if (!input$wrap){
      plt <- plt + facet_grid(rnames ~ cnames, scales = ifelse(input$fixed, "fixed", "free"))
    } else {
      plt = plt + facet_wrap(~ rnames + cnames, scales = ifelse(input$fixed, "fixed", "free"))
    }
    
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
