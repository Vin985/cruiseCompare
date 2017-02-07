


shinyServer(function(input, output, session) {
  logdebug("init")
  
  userInfo <- reactiveValues(lang = "fr")
  userInfo$data <- spdata
  userInfo$subsetCpt <- 1
  # userInfo$subsets <- list()
  # userInfo$subsetData <- list()
  createSubset(userInfo)
  
  ## handle language change
  # userInfo$lang <- "fr"
  checkQueryLanguage(session, userInfo)
  changeLanguageHandler(input, userInfo, event = CHANGE_LANG_EVENT)
  output$changeLang <- renderUI({
    changeLanguageOutput(userInfo$lang, button = TRUE)
  })
  
  changeSubsets(input, output, session, userInfo)
  selectDataFilters(input, output, session, userInfo)
  createReport(input, output, session, userInfo)
})