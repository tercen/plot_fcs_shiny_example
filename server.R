
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
library(flowWorkspace)
library(flowCore)
library(scales)
library(ggallin)
library(RColorBrewer)

source("helpers.R")

# http://127.0.0.1:5402/admin/w/77d52bb01bd3676e779828d5a50047ae/ds/36600030-7fb6-4e61-a25c-fd421ec60367
#options("tercen.workflowId" = "abe673836a3f732410737d630179dffd")
#options("tercen.stepId"= "fe19bf2c-d0f5-4270-b984-c0c1ee09fbff")

server <- shinyServer(function(input, output, session) {
  dataInput <- reactive({
    getValues(session)
  })
  
  custom_biexp_scale <- reactive({
    custom_biexp_trans <- flowjo_biexp(pos = input$pos_decades_x, 
                                       neg = input$neg_decades_x, 
                                       widthBasis = input$width_basis_x)
    custom_biexp_inv_trans <- flowjo_biexp(pos = input$pos_decades_x, 
                                           neg = input$neg_decades_x, 
                                           widthBasis = input$width_basis_x, 
                                           inverse = TRUE)
    
    scales::trans_new(name = 'custom biexponential',
                      transform = custom_biexp_trans,
                      inverse = custom_biexp_inv_trans)
  })

  custom_logicle_scale <- reactive({
    
    custom_logicle_trans <- logicleTransform(w = input$logicle_w_x,
                                             t = input$logicle_t_x,
                                             m = input$logicle_m_x,
                                             a = input$logicle_a_x)
    
    custom_logicle_inv_trans <- inverseLogicleTransform(trans = custom_logicle_trans)
    
    scales::trans_new(name = 'custom logicle',
                      transform = custom_logicle_trans,
                      inverse = custom_logicle_inv_trans)
  })
  
  calculate_x_breaks <- reactive({
    breaks_x <- as.numeric(unlist(strsplit(input$breaks_x, ",")))
    
    break_transform(breaks = breaks_x, 
                    transformation = input$x_trans_type)
  })
  
  calculate_y_breaks <- reactive({
    breaks_y <- as.numeric(unlist(strsplit(input$breaks_y, ",")))
    
    break_transform(breaks = breaks_y, 
                    transformation = input$y_trans_type)
  })
  
  break_transform <- function(breaks, transformation)
  {
    if ((transformation == "biexponential") || (transformation == "logicle")) {
      x.breaks <- custom_logicle_breaks(breaks)
    } else {
      x.breaks <- breaks
    }
    return(x.breaks)
  }
  
  output$biaxial <- renderPlot({
    df <- dataInput()
    
    x.breaks = calculate_x_breaks()
    y.breaks = calculate_y_breaks()
    
    logticks_flag = ""
    
    col_vector <- create_colors_vector(length(levels(df$factor_colors)))
    
    # ggplot object
    plt = ggplot()
    
    if (input$x_trans_type == "biexponential") {
      
      plt = plt + 
        scale_x_continuous(
          limits = range(x.breaks),
          breaks = x.breaks,
          trans = custom_biexp_scale(),
          labels = custom_tick_labels(x.breaks)
        )
      
    } else if (input$x_trans_type == "logicle") {
      
      plt = plt + 
        scale_x_continuous(
          limits = range(x.breaks),
          breaks = x.breaks,
          trans = custom_logicle_scale(),
          labels = custom_tick_labels(x.breaks)
        )
      
    } else if (input$x_trans_type == "log10") {
      
      plt =  plt + 
        scale_x_continuous(
          limits = range(x.breaks),
          breaks = x.breaks,
          trans = ggallin::pseudolog10_trans,
          labels = custom_tick_labels(x.breaks)
        )
      
      logticks_flag = "b"
    } else if (input$x_trans_type == "linear") {
      
      plt = plt +
        scale_x_continuous(
          limits = range(x.breaks),
          breaks = x.breaks
        )
    }
    
    if (input$y_trans_type == "biexponential") {
      
      plt = plt + 
        scale_y_continuous(
          limits = range(y.breaks),
          breaks = y.breaks,
          trans = custom_biexp_scale(),
          labels = custom_tick_labels(y.breaks)
        )
      
    } else if (input$y_trans_type == "logicle") {
      
      plt = plt + 
        scale_y_continuous(
          limits = range(y.breaks),
          breaks = y.breaks,
          trans = custom_logicle_scale(),
          labels = custom_tick_labels(y.breaks)
        )
      
    } else if (input$y_trans_type == "log10") {
      
      plt = plt +
        scale_y_continuous(
          limits = range(y.breaks),
          breaks = y.breaks,
          trans = ggallin::pseudolog10_trans,
          labels = custom_tick_labels(y.breaks)
        )
      
      logticks_flag = paste0(logticks_flag, "l")
    } else if (input$y_trans_type == "linear") {
      
      plt = plt +
        scale_y_continuous(
          limits = range(y.breaks),
          breaks = y.breaks
        )
    }
    
    plt = plt + geom_point(
      data = df,
      mapping = aes(x = .x, y = .y, colour = factor_colors, shape = factor_shapes),
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
      theme_classic() + theme_fcs()
    
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
    
    x.breaks = calculate_x_breaks()
    
    # ggplot object
    hist_x = ggplot(data = df, mapping = aes(.x)) + geom_density() 
    if (input$x_trans_type == "biexponential") {
      hist_x = hist_x +
        scale_x_continuous(
          limits = range(x.breaks),
          breaks = x.breaks,
          trans = custom_biexp_scale(),
          labels = custom_tick_labels(x.breaks))
      
    } else if (input$x_trans_type == "logicle") {

      hist_x = hist_x +
        scale_x_continuous(
          limits = range(x.breaks),
          breaks = x.breaks,
          trans = custom_logicle_scale(),
          labels = custom_tick_labels(x.breaks)
        )     
    } else if (input$x_trans_type == "log10") {
      
      hist_x =  hist_x +
        scale_x_continuous(
          limits = range(x.breaks),
          breaks = x.breaks,
          trans = ggallin::pseudolog10_trans,
          labels = custom_tick_labels(x.breaks)
        )
    } else if (input$x_trans_type == "linear") {
      
      hist_x = hist_x +
        scale_x_continuous(
          limits = range(x.breaks),
          breaks = x.breaks,
        )
    }
    
    plt = hist_x +
      # theme stuff
      theme_classic() + 
      theme_fcs()
    
    plot(plt)
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
    
    y.breaks = calculate_y_breaks()
    
    # ggplot object
    hist_y = ggplot(data = df, mapping = aes(.y)) + geom_density()
    
    if (input$y_trans_type == "biexponential") {
      
      hist_y = hist_y +
        scale_x_continuous(
          limits = range(y.breaks),
          breaks = y.breaks,
          trans = custom_biexp_scale(),
          labels = custom_tick_labels(y.breaks)
        )
      
    } else if (input$y_trans_type == "logicle") {
      
      hist_y =  hist_y + scale_x_continuous(
        limits = range(y.breaks),
        breaks = y.breaks,
        trans = custom_logicle_scale(),
        labels = custom_tick_labels(y.breaks)
      )
      
    } else if (input$y_trans_type == "log10") {
      
      hist_y = hist_y + scale_x_continuous(
        limits = range(y.breaks),
        breaks = y.breaks,
        trans = ggallin::pseudolog10_trans,
        labels = custom_tick_labels(y.breaks)
      )
    } else if (input$y_trans_type == "linear") {
      
      hist_y = hist_y + scale_x_continuous(
        limits = range(y.breaks),
        breaks = y.breaks)
    }
    
    plt = hist_y +
      # theme stuff
      theme_classic() +
      theme_fcs()
    
    plot(plt)
  })
  
})
