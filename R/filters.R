###################
### Initialize
##################


filterList <- c(OBSERVER_FILTER, SPECIES_FILTER, DATE_FILTER, REGION_FILTER)

## Define all filters
selectDataFilters <- function(input, output, session, userInfo) {

  initializeFilters(filterList, input, output, session, userInfo)

  ## Event observers for the selectors
  selectDataFiltersObservers(input, output, session, userInfo)
  ## Render filters
  selectDataFiltersRender(input, output, session, userInfo)
}


################
### Observers
###############


## Global observers for the filters
selectDataFiltersObservers <- function(input, output, session, userInfo) {
  ## Check if the user clicks on the subset data button
  observeEvent(input$subsetData, {
    # If subsets are defined, filter the data
    filterSubsets(userInfo)
    launchEvent(SUBSET_DATA_EVENT, userInfo)
  })


  observeEvent(input$resetData, {
    # If subsets are defined, filter the data
      logdebug("Resetting current subsets...")
      userInfo$subsetData <- list()
  })

  observeEvent(userInfo$event, {
    propagateEvent(filterList, input, output, session, userInfo)
  })

}


##############
### Renders
#############


## Main display for all filters
selectDataFiltersRender <- function(input, output, session, userInfo) {
  ## Display everything
  output$subsetFilters <- renderUI({
    isolate(tagList(
      column(4,
             uiOutput("selectObserver"),
             uiOutput("selectSpecies"),
             uiOutput("selectDate"),
             uiOutput("actionButtons")),
     column(8, uiOutput("selectRegion"))))
  })

  output$actionButtons <- renderUI({
    tagList(
    actionButton("subsetData", geti18nValue("data.subset", userInfo$lang)),
    actionButton("resetData", geti18nValue("reset.data.subset", userInfo$lang)))
  })

}



