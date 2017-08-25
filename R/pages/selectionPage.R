
## Page Id
SELECTION_PAGE <- "selection"

selectionPage <- function(input, output, session, userInfo) {

  # selectDataFilters(input, output, session, userInfo)
  createReport(input, output, session, userInfo)


  selectionPageUI(input,output,userInfo)

  observeEvent(input$viewDataAction, {
    # load("savedSubsets.Rdata")
    # userInfo$subsets <- ss
    filterSubsets(userInfo)
    # reset all distance analysis
    userInfo$distance <- NULL
    changePage(VIEW_DATA_PAGE, userInfo)
  })

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
        "viewDataAction",
        geti18nValue("view.data", userInfo$lang)
      )
    ))
  })
}
