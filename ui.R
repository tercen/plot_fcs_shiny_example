library(shiny)

ui <- shinyUI(fluidPage(
  
  titlePanel("Flow Cytometry Plot"),
  
  sidebarPanel(
    textInput("title", "Graph title label", ""),
    textInput("xlab", "X-axis label", ""),
    textInput("ylab", "Y-axis label", ""),
    textInput("legend", "Legend title label", ""),
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