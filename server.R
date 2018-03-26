

## Files are loaded in global.R


shinyServer(function(input, output, session) {
  logdebug("init")

  userInfo <- reactiveValues()

  lang <- "fr"
  user <- NULL
  queryArgs <- getInfoFromQueryString(parseQueryString(isolate(session$clientData$url_search)), defaultLang = "fr")
  if (!is.null(queryArgs)) {
    lang <- queryArgs$lang
    user <- queryArgs$user
  }
  userInfo$lang <- lang
  userInfo$user <- user

  userInfo$subsetCpt <- 1
  userInfo$page <- IMPORT_DATA_PAGE
  createSubset(userInfo)


  navbar(input, output, session, userInfo)

  importDataPage(input, output, session, userInfo)
  selectionPage(input, output, session, userInfo)
  viewDataPage(input, output, session, userInfo)


  ## Event observers for the selectors
  selectDataFiltersObservers(input, output, session, userInfo)
  ## Render filters
  selectDataFiltersRender(input, output, session, userInfo)
  initializeFilters(FILTER_LIST, input, output, session, userInfo)

  output$pageContent <- renderUI({
    loginfo("page selection %s", userInfo$page)
    uiOutput(paste0(userInfo$page, "Page"))
  })

  ## Little hack to keep the connection alive on EC network
  ## We refresh an empty box every 1:57mn to avoid disconnection
  output$keepalive <- reactive({
    invalidateLater(117000);
    ""
  })

})

navbar <- function(input, output, session, userInfo) {
  changeSubsets(input, output, session, userInfo)

  ## handle language change
  changeLanguageHandler(input, userInfo, event = CHANGE_LANG_EVENT)
  output$changeLanguage <- renderUI({
    changeLanguageOutput(userInfo$lang, button = TRUE)
  })

  ## go to application selection
  applicationObserver("main", input, userInfo$lang, userInfo$user)
  output$goToMain <- renderUI({
    applicationLink("main", userInfo$lang, button = FALSE)
  })
}
