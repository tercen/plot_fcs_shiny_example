
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)

ui <- shinyUI(
  fluidPage(
  
    titlePanel("Visualization & Data transformation "),
    helpText("Use the following options to transform & visualize your Flow Cytometry Data"),
    sidebarPanel(
      textInput("title", "Graph title label", "Biaxial plot"),
      textInput("x_label", "X-axis label", "PE :: 585_29[561]"),
      textInput("y_label", "Y-axis label", "APC :: 670_30[640]"),
      textInput("color_legend", "Color legend title label", ""),
      textInput("shape_legend", "Shape legend title label", ""),
      sliderInput("point_size", "Individual point size", min = 0, max = 10, value = 5, step = 1),
      sliderInput("plot_width", "Plot width (px)", min = 200, max = 2000, value = 500),
      sliderInput("plot_height", "Plot height (px)", min = 200, max = 2000, value = 500)
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          title = "Bi-axial plot",
          plotOutput(outputId = "biaxial")
        ),
        tabPanel(
          title = "x axis transformation",
          plotOutput(outputId = "distribution_x"),
          textInput(inputId = "breaks_x", 
                    label = "x-axis breaks", 
                    value = "-10, 0, 1e1, 1e2, 1e3, 1e4"),
          selectInput(inputId = "x_trans_type", 
                      label = "Transformation:",
                      choices = c("Linear" = "linear", 
                                  "Logarithmic" = "log10",
                                  "Biexponential" = "biexponential", 
                                  "Logicle" = "logicle"), 
                      selected = "Linear"),
          uiOutput("biexponential_width_basis_x"),
          uiOutput("biexponential_neg_decades_x"),
          uiOutput("biexponential_pos_decades_x"),
          uiOutput("logicle_w_x"),
          uiOutput("logicle_t_x"),
          uiOutput("logicle_m_x"),
          uiOutput("logicle_a_x")
        ),
        tabPanel(
          title = "y axis transformation",
          plotOutput(outputId = "distribution_y"),
          textInput(inputId = "breaks_y", 
                    label = "y-axis breaks", 
                    value = "-10, 0, 1e1, 1e2, 1e3, 1e4"),
          selectInput(inputId = "y_trans_type", 
                      label = "Transformation:",
                      choices = c("Linear" = "linear", 
                                  "Logarithmic" = "log10",
                                  "Biexponential" = "biexponential", 
                                  "Logicle" = "logicle"), 
                      selected = "Linear"),
          uiOutput("biexponential_width_basis_y"),
          uiOutput("biexponential_neg_decades_y"),
          uiOutput("biexponential_pos_decades_y"),
          uiOutput("logicle_w_y"),
          uiOutput("logicle_t_y"),
          uiOutput("logicle_m_y"),
          uiOutput("logicle_a_y")
        )#,
        #tabPanel(
        #  "Misc",
        #  checkboxInput("labs", "Apply labels", 0),
        #  checkboxInput("wrap", "Wrap panel grid", 0),
        #  checkboxInput("fixed", "Axes equal for panels", 0),
        #  checkboxInput("space", "Equal FCS space", 0),
        #)
      )
    )
  )
)

