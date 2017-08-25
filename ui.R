library(shiny)
library(leaflet)

shinyUI(fluidPage(
  useShinyjs(),
  includeCSS("www/css/styles.css"),
  fluidRow(
    class = "topRow",
    column(9, uiOutput("subsetTabs")),
    column(3, class = "navButtons",
           tagList(
             uiOutput("goToMain", style = "display:inline-block"),
             uiOutput("changeLanguage")
           ))
  ),
  fluidRow(column(12, uiOutput("pageContent")))
))
