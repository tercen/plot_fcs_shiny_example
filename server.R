library(shiny)
library(tercen)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggcyto)
library(flowWorkspace)
library(flowCore)
library(scales)
library(ggallin)
library(RColorBrewer)

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

custom_log10 = function(breaks) {
  breaks <-
    sapply(breaks, function(x)
      if (x > 0)
        log10(x)
      else-log10(abs(x)))
  math_format(10 ^ .x)(breaks)
}

server <- shinyServer(function(input, output, session) {
  dataInput <- reactive({
    getValues(session)
  })
  
  output$reacOut <- renderUI({
    plotOutput("main.plot",
               height = input$plotHeight,
               width = input$plotWidth)
  })
  
  output$main.plot <- renderPlot({
    df <- dataInput()
    breaks_x <- as.numeric(unlist(strsplit(input$breaks_x, ",")))
    breaks_y <- as.numeric(unlist(strsplit(input$breaks_y, ",")))
    
    if (length(levels(df$colors)) > 74) {
      qual_col_pals = brewer.pal.info
    } else {
      qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    }
    
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    
    print(input$scale)
    
    # ggplot object
    plt = ggplot()
    
    if (input$scale == "biexponential") {
      plt = plt +
        scale_x_flowjo_biexp(
          widthBasis = input$widthBasisX,
          equal.space = input$space,
          neg = input$negX,
          pos = input$posX,
          limits = c(min(breaks_x), max(breaks_x)),
          breaks = breaks_x
        ) +
        scale_y_flowjo_biexp(
          widthBasis = input$widthBasisY,
          equal.space = input$space,
          neg = input$negY,
          pos = input$posY,
          limits = c(min(breaks_y), max(breaks_y)),
          breaks = breaks_y
        )
    } else if (input$scale == "log10") {
      plt = plt +
        scale_x_continuous(
          limits = c(min(breaks_x), max(breaks_x)),
          breaks = breaks_x,
          trans = ggallin::pseudolog10_trans,
          labels = custom_log10,
        ) +
        scale_y_continuous(
          limits = c(min(breaks_y), max(breaks_y)),
          breaks = breaks_y,
          trans = ggallin::pseudolog10_trans,
          labels = custom_log10,
        )
    }
    
    
    plt = plt + geom_point(
      data = df,
      mapping = aes(x = .x, y = .y, colour = colors),
      size = input$pointSize,
      shape = 1
    ) +
      # labels
      labs(x = input$xlab,
           y = input$ylab,
           color = input$legend) +
      ggtitle(input$title) +
      scale_color_manual(values=col_vector[1:length(levels(df$colors))]) +
      
      
      # theme stuff
      theme_classic() +
      theme(
        legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(
          colour = "black",
          fill = NA,
          size = 2
        )
      )
    
    plt
  })
  
})

getValues <- function(session) {
  ctx <- getCtx(session)
  df <- ctx %>% select(.x, .y, .ri, .ci) %>%
    group_by(.ri)
  
  colors <- 0
  if (length(ctx$colors))
    colors <- ctx$select(ctx$colors[[1]])[[1]]
  if (!all(is.numeric(colors)))
    colors = as.factor(colors)
  
  tcn.labels <- NA
  sizes = NA
  if (length(ctx$labels))
    tcn.labels <- ctx$select(ctx$labels[[1]])[[1]]
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
    left_join(data.frame(.ri = 0:(length(rnames) - 1), rnames), by = ".ri") %>%
    left_join(data.frame(.ci = 0:(length(cnames) - 1), cnames), by = ".ci")
  
  df$colors <- factor(df$colors, levels = levels(df$colors))
  
  return(df)
}