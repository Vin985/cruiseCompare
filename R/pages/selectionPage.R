
## Page Id
SELECTION_PAGE <- "selection"

selectionPage <- function(input, output, session, userInfo) {

  # selectDataFilters(input, output, session, userInfo)
  createReport(input, output, session, userInfo)


  selectionPageUI(input,output,userInfo)

  observeEvent(input$viewDataAction, {
    filterSubsets(userInfo)
    # reset all distance analysis
    userInfo$distance <- NULL
    changePage(VIEW_DATA_PAGE, userInfo)
  })

  observeEvent(input$importDataPageAction, {
    changePage(IMPORT_DATA_PAGE, userInfo)
    userInfo$imported <- FALSE
  }, ignoreInit = TRUE)

}

selectionPageUI <- function(input,output,userInfo) {
  output$selectionPage <- renderUI({
    print("selection")
    tagList(
      div(class = "description globalDesc", i18nText("filters.global.desc", userInfo$lang)),
      fluidRow(class = "chooseOptions",
          uiOutput("subsetFilters")),
      fluidRow(uiOutput("selectionActionButtons")))
  })

  output$selectionActionButtons <- renderUI({
    tagList(div(
      class = "actionButtons",
      actionButton(
        class = "actionButton",
        "importDataPageAction",
        geti18nValue("import.data", userInfo$lang)
      ),
      actionButton(
        class = "actionButton",
        "viewDataAction",
        geti18nValue("view.data", userInfo$lang)
      )
    ))
  })
}
