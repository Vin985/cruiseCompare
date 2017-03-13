
## Files are loaded in global.R


shinyServer(function(input, output, session) {
  logdebug("init")
  
  userInfo <- reactiveValues(lang = "fr")
  userInfo$data <- spdata
  userInfo$subsetCpt <- 1
  userInfo$page <- SELECTION_PAGE
  createSubset(userInfo)
  
  
  navbar(input, output, session, userInfo)
  
  selectionPage(input, output, session, userInfo)
  viewDataPage(input, output, session, userInfo)
  
  
  output$pageContent <- renderUI({
    loginfo("page selection %s", userInfo$page)
    uiOutput(paste0(userInfo$page, "Page"))
  })
  
})

navbar <- function(input, output, session, userInfo){
  changeSubsets(input, output, session, userInfo)
  
  ## handle language change
  checkQueryLanguage(session, userInfo)
  changeLanguageHandler(input, userInfo, event = CHANGE_LANG_EVENT)
  output$changeLanguage <- renderUI({
    changeLanguageOutput(userInfo$lang, button = TRUE)
  })
  
}
