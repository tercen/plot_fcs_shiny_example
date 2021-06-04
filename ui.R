library(shiny)

shinyUI(fluidPage(
  
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