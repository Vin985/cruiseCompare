library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  includeCSS("www/css/styles.css"),
  fluidRow(class = "topRow",
           column(10, uiOutput("subsetTabs")),
           column(2, uiOutput("changeLanguage"))),
  fluidRow(column(12, uiOutput("pageContent")))
))
