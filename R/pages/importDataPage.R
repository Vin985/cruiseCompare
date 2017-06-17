## Page Id
IMPORT_DATA_PAGE <- "importData"

IMPORT_TYPE_CSV <- "csv"
IMPORT_TYPE_RDATA <- "rdata"

RDATASETS <- c("alcidae", "quebec")


importDataPage <- function(input, output, session, userInfo) {
  importDataPageUI(input, output, userInfo)

  observeEvent(input$importDataAction, {
    changePage(SELECTION_PAGE, userInfo)
  })

}

importDataPageUI <- function(input, output, userInfo) {
  output$importDataPage <- renderUI({
    tagList(column(12,
                   fluidRow(uiOutput(
                     "importDataChoice"
                   )),
                   fluidRow(uiOutput(
                     "importDataButton"
                   ))))
  })

  output$importDataChoice <- renderUI({
    importChoices <- list(IMPORT_TYPE_RDATA, IMPORT_TYPE_CSV)
    names(importChoices) <-
      c(
        geti18nValue("import.type.rdata", userInfo$lang),
        geti18nValue("import.type.csv", userInfo$lang)
      )
    div(class = "well",
        column(
            3,
            radioButtons(
              "dataImportType",
              geti18nValue("import.data.choice", userInfo$lang),
              choices = importChoices
            )
          ),
          column(3, offset = 1, uiOutput("importDataOptions")),
          div(style = "clear:both")
        )
  })

  output$importDataButton <- renderUI({
    tagList(div(
      class = "actionButtons",
      actionButton(
        class = "actionButton",
        "importDataAction",
        geti18nValue("import.data", userInfo$lang)
      )
    ))
  })

  output$importDataOptions <- renderUI({
    if (input$dataImportType == IMPORT_TYPE_RDATA) {
      uiOutput("importDataRdata")
    } else {
      uiOutput("importDataCsv")
    }
  })


  output$importDataRdata <- renderUI({
    selectInput("rDataChoice", label = geti18nValue("rdata.choice.label", userInfo$lang), choices = RDATASETS)
  })

  output$importDataCsv <- renderUI({
    "test"
  })



}
