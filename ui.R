library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  includeCSS("www/assets/css/styles.css"),
  fluidRow(class = "topRow",
  column(10, uiOutput("subsetTabs")),
  column(2, uiOutput("changeLang"))),
  fluidRow(
    div(class = "chooseOptions",
        uiOutput("subsetOptions"))),
  fluidRow(uiOutput("reportButtons"))
))
