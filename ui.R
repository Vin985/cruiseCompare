library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(bootstrapPage(
  includeCSS("www/css/styles.css"),
  fluidRow(class = "topRow",
  column(10, uiOutput("subsetTabs")),
  column(2, uiOutput("changeLanguage"))),
  uiOutput("pageContent")
))
