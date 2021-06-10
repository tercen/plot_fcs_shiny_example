library(shiny)
library(shinyjs)

ui <- shinyUI(
  fluidPage(
    shinyjs::useShinyjs(),
    tags$script(
      HTML(
        'setInterval(function(){ $("#hiddenButton").click(); }, 1000*30);'
      )
    ),
    tags$footer(shinyjs::hidden(
      actionButton(inputId = "hiddenButton", label = "hidden")
    )),
    
    titlePanel("Flow Cytometry Plot"),
    
    sidebarPanel(tabsetPanel(
      tabPanel(
        "Title and Graph",
        textInput("title", "Graph title label", ""),
        textInput("xlab", "X-axis label", ""),
        textInput("ylab", "Y-axis label", ""),
        textInput("legend", "Legend title label", ""),
        sliderInput("plotWidth", "Plot width (px)", 200, 2000, 500),
        sliderInput("plotHeight", "Plot height (px)", 200, 2000, 500),
      ),
      tabPanel(
        "Breaks & Points",
        textInput("breaks_x", "Breaks for X-Axis",
                  "0, 1e1, 1e2, 1e3, 1e4"),
        textInput("breaks_y", "Breaks for Y-Axis",
                  "0, 1e1, 1e2, 1e3, 1e4"),
        selectInput("scale", "Type of scale", choices = c("biexponential", "log10")),
        sliderInput("pointSize", "Individual point size",
                    0, 10, 1, step = 1),
      ),
      tabPanel(
        "Bi-Exp",
        sliderInput("widthBasisX", "Width Basis (X)", -2000, 0, -10),
        sliderInput("widthBasisY", "Width Basis (Y)", -2000, 0, -10),
        sliderInput("negX", "Negative range in asymptotic decades (X)",
                    0, 50, 0, step = 0.05),
        sliderInput("negY", "Negative range in asymptotic decades (Y)",
                    0, 50, 0, step = 0.05),
        sliderInput(
          "posX",
          "Positive range in asymptotic decades (X)",
          0,
          50,
          4.5,
          step = 0.05
        ),
        sliderInput(
          "posY",
          "Positive range in asymptotic decades (Y)",
          0,
          50,
          4.5,
          step = 0.05
        )
      ),
      tabPanel(
        "Misc",
        checkboxInput("labs", "Apply labels", 0),
        checkboxInput("wrap", "Wrap panel grid", 0),
        checkboxInput("fixed", "Axes equal for panels", 0),
        checkboxInput("space", "Equal FCS space", 0),
      )
    ), width=5),
    
    mainPanel(uiOutput("reacOut"), width=7)
  )
)