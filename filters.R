###################################
### Main file for defining filters 
###################################

source("filterUtils.R")
source("filters/regionFilter.R")
source("filters/speciesFilter.R")
source("filters/dateFilter.R")
source("filters/observerFilter.R")


###################
### Initialize
##################


## Define all filters 
selectDataFilters <- function(input, output, session, userInfo) {
  
  ## Filters
  selectObserver(input, output, session, userInfo)
  selectSpecies(input, output, session, userInfo)
  selectDate(input, output, session, userInfo)
  selectRegion(input, output, session, userInfo)
  
  
  ## Event observers for the selectors
  selectDataFiltersObservers(input, output, session, userInfo)
  ## Render filters 
  selectDataFiltersRender(input, output, session, userInfo)
}


################
### Observers
###############


## Apply filters to extract data for the defined subset
filterData <- function(subset, userInfo) {
  logdebug("filtering...")
  
  # Get filters for the current subset
  filters <- getFilters(subset)
  
  # Get whole data
  tmp <- isolate(userInfo$data)
  
  # Apply filters
  tmp <- filterBy(OBSERVER_FILTER, filters, tmp)
  tmp <- filterBy(SPECIES_FILTER, filters, tmp)
  tmp <- filterBy(DATE_FILTER, filters, tmp)
  tmp <- filterBy(REGION_FILTER, filters, tmp)
  
  tmp
}


## Global observers for the filters 
selectDataFiltersObservers <- function(input, output, session, userInfo) {
  ## Check if the user clicks on the subset data button
  observeEvent(input$subsetData, {
    # If subsets are defined, filter the data
    subsets <- getSubsets(userInfo)
    if (!is.null(subsets)) {
      logdebug("subsetting...")
      subsetsData <- lapply(subsets, filterData, userInfo)
      names(subsetsData) <- names(subsets)
      userInfo$subsetData <- subsetsData
    }
    launchEvent(SUBSET_DATA_EVENT, userInfo)
  })
  
  
  observeEvent(input$resetData, {
    # If subsets are defined, filter the data
      logdebug("Resetting current subsets...")
      userInfo$subsetData <- list()
  })
  
}


##############
### Renders
#############


## Main display for all filters
selectDataFiltersRender <- function(input, output, session, userInfo) {
  ## Display everything
  output$subsetOptions <- renderUI({
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



