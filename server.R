
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)
library(tercen)
library(dplyr, warn.conflicts = FALSE)
library(jsonlite)

library(tidyr)
library(ggplot2)
#library(ggcyto)
library(flowWorkspace)
library(flowCore)
library(scales)
library(ggallin)
#library(ggh4x)
library(RColorBrewer)


# http://127.0.0.1:5402/admin/w/77d52bb01bd3676e779828d5a50047ae/ds/36600030-7fb6-4e61-a25c-fd421ec60367
# options("tercen.workflowId" = "abe673836a3f732410737d630179dffd"))
# options("tercen.stepId"= "fe19bf2c-d0f5-4270-b984-c0c1ee09fbff")

server <- shinyServer(function(input, output, session) {
  dataInput <- reactive({
    getValues(session)
  })
  
  output$biaxial <- renderPlot({
    df <- dataInput()
    
    #print(head(df))
    
    logticks_flag = ""
    
    breaks_x <- as.numeric(unlist(strsplit(input$breaks_x, ",")))
    breaks_y <- as.numeric(unlist(strsplit(input$breaks_y, ",")))
    
    if (length(levels(df$factor_colors)) > 74) {
      qual_col_pals = brewer.pal.info
    } else {
      qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    }
    
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    
    # ggplot object
    plt = ggplot()
    
    if (input$x_trans_type == "biexponential") {
      
      trans.fun = flowjo_biexp(pos = input$pos_decades_x, 
                               neg = input$neg_decades_x, 
                               widthBasis = input$width_basis_x)
      inv.fun = flowjo_biexp(pos = input$pos_decades_x, 
                             neg = input$neg_decades_x, 
                             widthBasis = input$width_basis_x, 
                             inverse = TRUE)
      
      x.breaks = custom_logicle_breaks(breaks_x, 
                                       trans.fun = trans.fun, 
                                       inverse.fun = inv.fun)
      df$x_trans = trans.fun(df$.x)
      
      plt = plt +
        scale_x_continuous(
          limits = trans.fun(c(min(breaks_x), max(breaks_x))), 
          breaks = x.breaks[[1]],
          labels = x.breaks[[2]]
        )
    } else if (input$x_trans_type == "logicle") {
      
      trans.fun <- logicleTransform(w = input$logicle_w_x, 
                                    t = input$logicle_t_x,
                                    m = input$logicle_m_x, 
                                    a = input$logicle_a_x)
      inv.fun <- inverseLogicleTransform(trans = trans.fun)
      
      x.breaks = custom_logicle_breaks(breaks_x, 
                                       trans.fun = trans.fun, 
                                       inverse.fun = inv.fun)
      df$x_trans = trans.fun(df$.x)
      
      plt = plt +
        scale_x_continuous(
          limits = trans.fun(c(min(breaks_x), max(breaks_x))), 
          breaks = x.breaks[[1]],
          labels = x.breaks[[2]]
        )
    } else if (input$x_trans_type == "log10") {
      df$x_trans = df$.x
      
      plt =  plt + 
        scale_x_continuous(
          limits = c(min(breaks_x), max(breaks_x)),
          breaks = breaks_x,
          trans = ggallin::pseudolog10_trans,
          labels = custom_log10
        )
      
      logticks_flag = "b"
    } else if (input$x_trans_type == "linear") {
      df$x_trans = df$.x
      
      plt = plt +
        scale_x_continuous(
          limits = c(min(breaks_x), max(breaks_x)),
          breaks = breaks_x
        )
    }
    
    if (input$y_trans_type == "biexponential") {
      
      trans.fun = flowjo_biexp(pos = input$pos_decades_y, 
                               neg = input$neg_decades_y, 
                               widthBasis = input$width_basis_y)
      inv.fun = flowjo_biexp(pos = input$pos_decades_y, 
                             neg = input$neg_decades_y, 
                             widthBasis = input$width_basis_y, 
                             inverse = TRUE)
      
      y.breaks = custom_logicle_breaks(breaks_y, 
                                       trans.fun = trans.fun, 
                                       inverse.fun = inv.fun)
      df$y_trans = trans.fun(df$.y)
      
      plt = plt +
        scale_y_continuous(
          limits = trans.fun(c(min(breaks_y), max(breaks_y))), 
          breaks = y.breaks[[1]],
          labels = y.breaks[[2]]
        )
      
    } else if (input$y_trans_type == "logicle") {
      
      trans.fun <- logicleTransform(w = input$logicle_w_y, 
                                    t = input$logicle_t_y,
                                    m = input$logicle_m_y, 
                                    a = input$logicle_a_y)
      inv.fun <- inverseLogicleTransform(trans = trans.fun)
      
      y.breaks = custom_logicle_breaks(breaks_y, 
                                       trans.fun = trans.fun, 
                                       inverse.fun = inv.fun)
      df$y_trans = trans.fun(df$.y)
      
      plt = plt +
        scale_y_continuous(
          limits = trans.fun(c(min(breaks_y), max(breaks_y))), 
          breaks = y.breaks[[1]],
          labels = y.breaks[[2]]
        )
      
    } else if (input$y_trans_type == "log10") {
      df$y_trans = df$.y
      
      plt = plt +
        scale_y_continuous(
          limits = c(min(breaks_y), max(breaks_y)),
          breaks = breaks_y,
          trans = ggallin::pseudolog10_trans,
          labels = custom_log10
        )
      
      logticks_flag = paste0(logticks_flag, "l")
    } else if (input$y_trans_type == "linear") {
      df$y_trans = df$.y
      
      plt = plt +
        scale_y_continuous(
          limits = c(min(breaks_y), max(breaks_y)),
          breaks = breaks_y,
        )
    }
    
    
    plt = plt + geom_point(
      data = df,
      mapping = aes(x = x_trans, y = y_trans, colour = factor_colors, shape = factor_shapes),
      size = input$point_size
    ) +
      # labels
      labs(x = input$x_label,
           y = input$y_label,
           color = input$color_legend,
           shape = input$shape_legend ) +
      ggtitle(input$title) +
      scale_color_manual(values=col_vector[1:length(levels(df$factor_colors))]) +
      
      # theme stuff
      theme_classic() +
      theme(
        legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(
          colour = "black",
          fill = NA,
          size = 1
        )
      )
    
    if (logticks_flag != "")
    {
      plt = plt + annotation_logticks(sides = logticks_flag, 
                                      outside = TRUE) + #, 
                                      #short = unit(.05, "cm"),
                                      #mid = unit(0.1, "cm"),
                                      #long = unit(0.15, "cm")) +
        coord_cartesian(clip = "off") 
    }
    
    plot(plt)
  })
  
  output$biexponential_width_basis_x <- renderUI({
    req(input$x_trans_type == "biexponential")
    sliderInput(inputId = "width_basis_x", 
                label = "Width Basis", 
                min = -2000, 
                max = 0, 
                value = -12.60)
  })
  
  output$biexponential_neg_decades_x <- renderUI({
    req(input$x_trans_type == "biexponential")
    sliderInput(inputId = "neg_decades_x", 
                label = "Extra negative decades",
                min = 0, 
                max = 10, 
                value = 0, 
                step = 0.01)
  })
  
  output$biexponential_pos_decades_x <- renderUI({
    req(input$x_trans_type == "biexponential")
    sliderInput(inputId = "pos_decades_x", 
                label = "Positive decades", 
                min = 0,
                max = 50,
                value = 4.5,
                step = 0.01)
  })
  
  output$logicle_w_x <- renderUI({
    req(input$x_trans_type == "logicle")
    sliderInput(inputId = "logicle_w_x", 
                label = "w: linearization width in asymptotic decades", 
                min = 0,
                max = 10,
                value = 0.5,
                step = 0.1)
  })

  output$logicle_t_x <- renderUI({
    req(input$x_trans_type == "logicle")
    sliderInput(inputId = "logicle_t_x", 
                label = "t: Top of the scale data value", 
                min = 10000,
                max = 262144,
                value = 262144,
                step = 1000)
  })
  
  output$logicle_m_x <- renderUI({
    req(input$x_trans_type == "logicle")
    sliderInput(inputId = "logicle_m_x", 
                label = "m: the full width of the transformed display in asymptotic decades", 
                min = 0.1,
                max = 10,
                value = 4.5,
                step = 0.1)
  })
  
  output$logicle_a_x <- renderUI({
    req(input$x_trans_type == "logicle")
    sliderInput(inputId = "logicle_a_x", 
                label = "a: extra negative decades", 
                min = 0,
                max = 10,
                value = 0,
                step = 0.01)
  })
  
  #output$biaxial <- renderUI({
  #  plotOutput("theplot",
  #             height = 500,
  #             width = 750)
  #})
  
  output$distribution_x <- renderPlot({
    df <- dataInput()
    
    breaks_x <- as.numeric(unlist(strsplit(input$breaks_x, ",")))
    
    # ggplot object
    hist_x = ggplot()
    if (input$x_trans_type == "biexponential") {

      trans.fun = flowjo_biexp(pos = input$pos_decades_x, 
                               neg = input$neg_decades_x, 
                               widthBasis = input$width_basis_x)
      inv.fun = flowjo_biexp(pos = input$pos_decades_x, 
                             neg = input$neg_decades_x, 
                             widthBasis = input$width_basis_x, 
                             inverse = TRUE)
      
      x.breaks = custom_logicle_breaks(breaks_x, 
                                       trans.fun = trans.fun, 
                                       inverse.fun = inv.fun)
      df$x_trans = trans.fun(df$.x)

      hist_x = hist_x + geom_density(data = df, mapping = aes(x_trans)) +
        scale_x_continuous(
          limits = trans.fun(c(min(breaks_x), max(breaks_x))), 
          breaks = x.breaks[[1]],
          labels = x.breaks[[2]]
        )
      
    } else if (input$x_trans_type == "logicle") {
      
      trans.fun <- logicleTransform(w = input$logicle_w_x, 
                                    t = input$logicle_t_x,
                                    m = input$logicle_m_x, 
                                    a = input$logicle_a_x)
      inv.fun <- inverseLogicleTransform(trans = trans.fun)
      
      x.breaks = custom_logicle_breaks(breaks_x, 
                                       trans.fun = trans.fun, 
                                       inverse.fun = inv.fun)
      df$x_trans = trans.fun(df$.x)
      
      hist_x = hist_x + geom_density(data = df, mapping = aes(x_trans)) +
        scale_x_continuous(
          limits = trans.fun(c(min(breaks_x), max(breaks_x))), 
          breaks = x.breaks[[1]],
          labels = x.breaks[[2]]
        )
      
    } else if (input$x_trans_type == "log10") {

      hist_x =  hist_x + geom_density(data = df, mapping = aes(.x)) +
        scale_x_continuous(
          limits = c(min(breaks_x), max(breaks_x)),
          breaks = breaks_x,
          trans = ggallin::pseudolog10_trans,
          labels = custom_log10
        )
    } else if (input$x_trans_type == "linear") {

      hist_x = hist_x + geom_density(data = df, mapping = aes(.x)) +
        scale_x_continuous(
          limits = c(min(breaks_x), max(breaks_x)),
          breaks = breaks_x,
        )
    }
    
    plt = hist_x +
      # theme stuff
      theme_classic() +
      theme(
        legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(
          colour = "black",
          fill = NA,
          size = 1
        )
      )
    
    plt
  })
  
  output$biexponential_width_basis_y <- renderUI({
    req(input$y_trans_type == "biexponential")
    sliderInput(inputId = "width_basis_y", 
                label = "Width Basis", 
                min = -2000, 
                max = 0, 
                value = -12.60)
  })
  
  output$biexponential_neg_decades_y <- renderUI({
    req(input$y_trans_type == "biexponential")
    sliderInput(inputId = "neg_decades_y", 
                label = "Extra negative decades",
                min = 0, 
                max = 10, 
                value = 0, 
                step = 0.01)
  })
  
  output$biexponential_pos_decades_y <- renderUI({
    req(input$y_trans_type == "biexponential")
    sliderInput(inputId = "pos_decades_y", 
                label = "Positive decades", 
                min = 0,
                max = 50,
                value = 4.5,
                step = 0.01)
  })
  
  output$logicle_w_y <- renderUI({
    req(input$y_trans_type == "logicle")
    sliderInput(inputId = "logicle_w_y", 
                label = "w: linearization width in asymptotic decades", 
                min = 0,
                max = 10,
                value = 0.5,
                step = 0.1)
  })
  
  output$logicle_t_y <- renderUI({
    req(input$y_trans_type == "logicle")
    sliderInput(inputId = "logicle_t_y", 
                label = "t: Top of the scale data value", 
                min = 10000,
                max = 262144,
                value = 262144,
                step = 1000)
  })
  
  output$logicle_m_y <- renderUI({
    req(input$y_trans_type == "logicle")
    sliderInput(inputId = "logicle_m_y", 
                label = "m: the full width of the transformed display in asymptotic decades", 
                min = 0.1,
                max = 10,
                value = 4.5,
                step = 0.1)
  })
  
  output$logicle_a_y <- renderUI({
    req(input$y_trans_type == "logicle")
    sliderInput(inputId = "logicle_a_y", 
                label = "a: extra negative decades", 
                min = 0,
                max = 10,
                value = 0,
                step = 0.01)
  })
  
  output$distribution_y <- renderPlot({
    df <- dataInput()
    
    breaks_y <- as.numeric(unlist(strsplit(input$breaks_y, ",")))
    
    # ggplot object
    hist_y = ggplot()
    if (input$y_trans_type == "biexponential") {
      
      trans.fun = flowjo_biexp(pos = input$pos_decades_y, 
                               neg = input$neg_decades_y, 
                               widthBasis = input$width_basis_y)
      inv.fun = flowjo_biexp(pos = input$pos_decades_y, 
                             neg = input$neg_decades_y, 
                             widthBasis = input$width_basis_y, 
                             inverse = TRUE)
      
      y.breaks = custom_logicle_breaks(breaks_y, 
                                       trans.fun = trans.fun, 
                                       inverse.fun = inv.fun)
      df$y_trans = trans.fun(df$.y)
      
      hist_y = hist_y + geom_density(data = df, mapping = aes(y_trans)) +
        scale_x_continuous(
          limits = trans.fun(c(min(breaks_y), max(breaks_y))), 
          breaks = y.breaks[[1]],
          labels = y.breaks[[2]]
        )
      
    } else if (input$y_trans_type == "logicle") {
      
      trans.fun <- logicleTransform(w = input$logicle_w_y, 
                                t = input$logicle_t_y,
                                m = input$logicle_m_y, 
                                a = input$logicle_a_y)
      inv.fun <- inverseLogicleTransform(trans = trans.fun)
      
      y.breaks = custom_logicle_breaks(breaks_y, 
                                       trans.fun = trans.fun, 
                                       inverse.fun = inv.fun)
      df$y_trans = trans.fun(df$.y)
      
      hist_y = hist_y + geom_density(data = df, mapping = aes(y_trans)) +
        scale_x_continuous(
          limits = trans.fun(c(min(breaks_y), max(breaks_y))), 
          breaks = y.breaks[[1]],
          labels = y.breaks[[2]]
        )
      
    } else if (input$y_trans_type == "log10") {
      
      hist_y =  hist_y + geom_density(data = df, mapping = aes(.y)) +
        scale_x_continuous(
          limits = c(min(breaks_y), max(breaks_y)),
          breaks = breaks_y,
          trans = ggallin::pseudolog10_trans,
          labels = custom_log10
        )
    } else if (input$y_trans_type == "linear") {
      
      hist_y = hist_y + geom_density(data = df, mapping = aes(.y)) +
        scale_x_continuous(
          limits = c(min(breaks_y), max(breaks_y)),
          breaks = breaks_y,
        )
    }
    
    plt = hist_y +
      # theme stuff
      theme_classic() +
      theme(
        legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(
          colour = "black",
          fill = NA,
          size = 1
        )
      )
    
    plt
  })
  
})

# shinyServer(function(input, output, session) {
#   
#   dataInput = reactive({getValues(session)})
#   mode = reactive({getMode(session)})
#   settingsValue = reactiveValues()
#   settingsValue$isInitialized = FALSE
#   msgReactive = reactiveValues(msg = "")
#  
#   
#   observeEvent(input$saveSettingsBtn, {
#     showModal(modalDialog(
#       title='Saving',
#       span('Saving settings, please wait ...'),
#       footer = NULL
#     ))
#     settings = list(bins=input$bins)
#     setSettings(session,settings)
#     removeModal()
#   })
#   
#   observeEvent(input$bins, {
#     if (settingsValue$isInitialized){
#       settingsValue$value = list(bins=input$bins)
#     } else {
#       settingsValue$value = getSettings(session)
#       settingsValue$isInitialized = TRUE
#       
#       # update ui
#       updateSliderInput(session, 'bins', value=settingsValue$value$bins)
#       shinyjs::show("bins")
#     }
#   })
#   
#   observeEvent(input$runBtn, {
#     
#     shinyjs::disable("runBtn")
#     
#     msgReactive$msg = "Running ... please wait ..."
# 
#     tryCatch({
#       ctx = getCtx(session)
#       ctx %>%
#         select(.y, .ci, .ri) %>%
#         group_by(.ci, .ri) %>%
#         summarise(mean = mean(.y)) %>%
#         ctx$addNamespace() %>%
#         ctx$save()
#       
#       msgReactive$msg = "Done"
#       
#     }, error = function(e) {
#       msgReactive$msg = paste0("Failed : ", toString(e))
#       print(paste0("Failed : ", toString(e)))
#     })
#   })
#   
#   output$mode = renderText({ 
#     mode()
#   })
#   
#   output$msg = renderText({ 
#     msgReactive$msg
#   })
#   
#   output$distPlot <- renderPlot({
#     m = mode()
#     
#     if (!is.null(m) && m == 'run'){
#       shinyjs::enable("runBtn")
#     }
#     
#     shinyjs::enable("saveSettingsBtn")
#     
#     if (is.null(settingsValue$value)){
#       settingsValue$value = getSettings(session)
#     } 
#     
#     settings = settingsValue$value
#     
#     # generate bins based on input$bins from ui.R
#     x    <- dataInput()[['.y']]
#     bins <- seq(min(x), max(x), length.out = settings$bins + 1)
#     # draw the histogram with the specified number of bins
#     hist(x, breaks = bins, col = 'darkgray', border = 'white')
#   })
# })
# 
# getSettings = function(session) {
#   fileSettings = getFileSettings(session)
#   if (is.null(fileSettings)){
#     settings = list(bins=30)
#     return(settings)
#   }
#   ctx = getCtx(session)
#   bytes = ctx$client$fileService$download(fileSettings$id)
#   settings = fromJSON(rawToChar(bytes))
#   return(settings)
# }
# 
# setSettings = function(session, settings){
#   ctx = getCtx(session)
#   fileSettings = getFileSettings(session)
#   if (!is.null(fileSettings)){
#     ctx$client$fileService$delete(fileSettings$id,fileSettings$rev)
#   }
#   
#   workflowId = getWorkflowId(session)
#   stepId = getStepId(session)
#   workflow = ctx$client$workflowService$get(workflowId)
#   
#   fileDoc = FileDocument$new()
#   fileDoc$name = 'webapp-operator-settings'
#   fileDoc$projectId = workflow$projectId
#   fileDoc$acl$owner = workflow$acl$owner
#   fileDoc$metadata$contentType = 'application/octet-stream'
#   
#   metaWorkflowId = Pair$new()
#   metaWorkflowId$key = 'workflow.id'
#   metaWorkflowId$value = workflowId
#   
#   metaStepId = Pair$new()
#   metaStepId$key = 'step.id'
#   metaStepId$value = stepId
#   
#   fileDoc$meta = list(metaWorkflowId, metaStepId)
#   
#   content = toJSON(settings)
#   bytes = charToRaw(content)
#   fileDoc = ctx$client$fileService$upload(fileDoc, bytes)
#   fileDoc
# }
# 
# getFileSettings = function(session) {
#   ctx = getCtx(session)
#   workflowId = getWorkflowId(session)
#   stepId = getStepId(session)
#   
#   files = ctx$client$fileService$findFileByWorkflowIdAndStepId(
#     startKey=list(workflowId,stepId),
#     endKey=list(workflowId,''),
#     descending=TRUE, limit=1 )
#   
#   if (length(files) > 0) {
#     return (files[[1]])
#   } 
#   
#   return (NULL)
# }
# 
# getMode = function(session){
#   # retreive url query parameters provided by tercen
#   query = parseQueryString(session$clientData$url_search)
#   return(query[["mode"]])
# }
# 
# getWorkflowId = function(session){
#   workflowId = getOption("tercen.workflowId")
#   if (!is.null(workflowId)) return(workflowId)
#   # retreive url query parameters provided by tercen
#   query = parseQueryString(session$clientData$url_search)
#   return(query[["workflowId"]])
# }
# 
# getStepId = function(session){
#   stepId = getOption("tercen.stepId")
#   if (!is.null(stepId)) return(stepId)
#   # retreive url query parameters provided by tercen
#   query = parseQueryString(session$clientData$url_search)
#   return(query[["stepId"]])
# }
# 
getCtx = function(session){
  # retreive url query parameters provided by tercen
  query = parseQueryString(session$clientData$url_search)

  token = query[["token"]]
  taskId = query[["taskId"]]

  # create a Tercen context object using the token
  ctx = tercenCtx(taskId=taskId, authToken=token)

  # dev
  # ctx = tercenCtx()

  return(ctx)
}
# 
# getValues = function(session){
#   ctx = getCtx(session)
#   data = ctx %>% select(.y , .ci , .ri )
#   return(data)
# }

#getCtx <- function(session) {
#  ctx <- tercenCtx(stepId = "f18104d6-6d29-47ce-80be-c24681a08117",
#                   workflowId = "abe673836a3f732410737d630179dffd")
#  return(ctx)
#}

custom_log10 = function(breaks) {
  breaks <-
    sapply(breaks, function(x)
      if (x > 0)
        log10(x)
      else-log10(abs(x)))
  math_format(10 ^ .x)(breaks)
}

getValues <- function(session) {
  
  ctx <- getCtx(session)
  
  df <- ctx %>% select(.x, .y, .ri, .ci) %>%
    group_by(.ri)
  
  factor_colors <- 0
  factor_shapes <- 0
  
  if (length(ctx$colors) >=1)
    factor_colors <- ctx$select(ctx$colors[[1]])[[1]]
  if (length(ctx$colors) > 1)
    factor_shapes <- ctx$select(ctx$colors[[2]])[[1]]
  
  if (! all(is.numeric(factor_colors)))
    factor_colors = as.factor(factor_colors)
  if (! all(is.numeric(factor_shapes)))
    factor_shapes = as.factor(factor_shapes)
  
  
  tcn.labels <- NA
  sizes = NA
  if (length(ctx$labels))
    tcn.labels <- ctx$select(ctx$labels[[1]])[[1]]
  if (!all(is.numeric(tcn.labels))) {
    labels = as.factor(tcn.labels)
  } else {
    sizes = tcn.labels
  }
  
  df = df %>% data.frame(labels, factor_colors, factor_shapes, sizes)
  
  cnames = ctx$cselect()[[1]]
  
  rnames = ctx$rselect()[[1]]
  df = df %>%
    mutate(factor_colors = factor_colors, factor_shapes = factor_shapes, labels = labels) %>%
    left_join(data.frame(.ri = 0:(length(rnames) - 1), rnames), by = ".ri") %>%
    left_join(data.frame(.ci = 0:(length(cnames) - 1), cnames), by = ".ci")
  
  df$factor_colors <- factor(df$factor_colors, levels = levels(df$factor_colors))
  df$factor_shapes <- factor(df$factor_shapes, levels = levels(df$factor_shapes))
  
  return(df)
}

## Code taken from
custom_logicle_breaks <- function(x, n = 6, equal.space = FALSE, trans.fun, inverse.fun) {
  
  rng.raw <- range(x, na.rm = TRUE)
  x.trans <- trans.fun(x)
  x.inv.trans <- inverse.fun(x.trans)
  
  v_min_max = c(minimum = x.inv.trans[1], 
                maximum = x.inv.trans[length(x.inv.trans)])
  
  log_min_max = c()
  for (i in c("minimum", "maximum")) {
    value = v_min_max[i]
    if ( i == "minimum" && sign(value) == 1 ) {
      log_min_max[i] = sign(value)*floor(log10(abs(value)))
    } else {
      log_min_max[i] = sign(value)*ceiling(log10(abs(value)))
    }
  }
  power = log_min_max["minimum"]:log_min_max["maximum"]
  sn = sign(power)
  pow = abs(power)
  
  decades = sn*10^pow; # node that decades includes 0. e.g.: -100, -10, -1, 0, 1, 10.
  decades.trans = trans.fun(decades)
  n_decades = length(decades)
  n_ticks = (n_decades-1)*9 + 1 + 2*sum(decades==0) #if we have 0 included in our decades, add 2 to the number of ticks because we will tick at -1 and 1
  obj.Tick = rep(0, n_ticks)
  obj.TickLabel = rep('', n_ticks)
  tick_index = 1
  previous_labeled_decade=NA
  
  for(k in 1:n_decades) {
    if ( !is.na(previous_labeled_decade) && decades.trans[k]-decades.trans[previous_labeled_decade] < 0.02) { # if the distance between this decade and the last is less than 0.02, do not label this decade because we may overlap the labels
      obj.TickLabel[tick_index] = ''
    } else {
      if (sn[k] == 0) {
        obj.TickLabel[tick_index] = '0'
      } else {
        if (sn[k] == -1) {
          sign_string = '-'
        } else {
          sign_string = ''
        }
        obj.TickLabel[tick_index] = paste(sign_string, "10^", as.character(pow[k]), sep="") 
      }
      previous_labeled_decade = k
    }
    
    if (k == n_decades) {
      # write Tick for final decade
      obj.Tick[tick_index] = decades.trans[k]
      # Fill any subsequent ticks
      # which may be labelled '' so that the Tick vector is
      # monotonically increasing;
      if (tick_index < n_ticks) {
        obj.Tick[(tick_index+1):length(obj.Tick)] = seq(obj.Tick[tick_index]+0.1, 
                                                        obj.Tick[tick_index]+0.2, 
                                                        length.out = n_ticks - tick_index)
      }
      break;
    }
    
    # write Tick for this decade in 9 increments if the ends of
    # the decades are powers of 10 increments if the right hand
    # end of the gap is 0 (i.e.10^{-inf})
    if (decades[k+1] == 0) {
      n_increments = 11
      lhs = decades[k]
      rhs = decades[k+1] - min(abs(c(lhs,decades[k+1])))
    } else if (decades[k]==0) {
      n_increments = 9
      lhs = 1
      rhs = decades[k+1] - min(abs(c(lhs,decades[k+1])))
    } else {
      n_increments = 9
      lhs = decades[k]
      rhs = decades[k+1] - min(abs(c(lhs,decades[k+1])))
    }
    obj.Tick[tick_index:(tick_index+n_increments-1)] = trans.fun(seq(from = lhs, to = rhs, length.out = n_increments))
    
    # write empty TickLabel for the next 8 or 9 linear
    # increments within this decade
    for (i in (tick_index+1):(tick_index+n_increments-1)){
      obj.TickLabel[i] = '';
    }
    
    tick_index = tick_index + n_increments;
  }
  
  return(list(obj.Tick, obj.TickLabel))
}

