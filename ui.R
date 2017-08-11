library(shiny)
library(leaflet)

shinyUI(fluidPage(
  useShinyjs(),
  includeCSS("www/css/styles.css"),
  fluidRow(
    class = "topRow",
    column(10, uiOutput("subsetTabs")),
    column(2, class = "navButtons",
           tagList(
             #uiOutput("goToMain", style = "display:inl"),
             uiOutput("changeLanguage")
           ))
  ),
  fluidRow(column(12, uiOutput("pageContent")))
))
