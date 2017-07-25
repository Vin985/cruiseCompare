library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  useShinyjs(),
  includeCSS("www/css/styles.css"),
  fluidRow(
    class = "topRow",
    column(10, uiOutput("subsetTabs")),
    column(2, class = "navButtons",
           tagList(
             uiOutput("goToMain", style = "display:inl"),
             uiOutput("changeLanguage")
           ))
  ),
  fluidRow(column(12, uiOutput("pageContent")))
))
