

## Files are loaded in global.R


shinyServer(function(input, output, session) {
  logdebug("init")

  lang <- "fr"
  query <-
    parseQueryString(isolate(session$clientData$url_search))
  lg <- query[["lang"]]
  if (!is.null(lg) && lg %in% c("fr", "en")) {
    lang <- lg
  }

  userInfo <- reactiveValues(lang = lang)
  userInfo$subsetCpt <- 1
  userInfo$page <- IMPORT_DATA_PAGE
  createSubset(userInfo)


  navbar(input, output, session, userInfo)

  importDataPage(input, output, session, userInfo)
  selectionPage(input, output, session, userInfo)
  viewDataPage(input, output, session, userInfo)


  output$pageContent <- renderUI({
    loginfo("page selection %s", userInfo$page)
    uiOutput(paste0(userInfo$page, "Page"))
  })


})

navbar <- function(input, output, session, userInfo) {
  changeSubsets(input, output, session, userInfo)

  ## handle language change
  checkQueryLanguage(session, userInfo)
  changeLanguageHandler(input, userInfo, event = CHANGE_LANG_EVENT)
  output$changeLanguage <- renderUI({
    changeLanguageOutput(userInfo$lang, button = TRUE)
  })

  ## go to application selection
  applicationObserver("main", input, userInfo$lang)
  output$goToMain <- renderUI({
    applicationLink("main", userInfo$lang)
  })
}
