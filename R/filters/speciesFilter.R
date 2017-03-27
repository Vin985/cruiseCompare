

## Name of the filter
SPECIES_FILTER <- "species"

## Type of filters
TYPE_SPECIES <- "species"

###################
### Initialize
##################


## Main entry for the filter. Register Observers and Renders
initSpeciesFilter <- function(input, output, session, userInfo) {
  selectSpeciesRender(input, output, session, userInfo)
  selectSpeciesObserver(input, output, session, userInfo)
}


##############
###  Filter
#############


## Filter data by region
speciesFilter <- function(data, condition) {
  loginfo("Filtering by species with species: %s", condition)
  if (length(condition) > 0) {
    
    # get all watch ids to calculate the effort
    nodup <- data[!duplicated(data@data[, "WatchID"]), ]
    
    # get all data concerning selected species
    temp <- data[data$Alpha %in% condition[[TYPE_SPECIES]], ]
    
    # Reset everything
    nodup$Count <- NA
    nodup$Alpha <- ""
    nodup$Distance <- NA
    
    # Remove all watches where the selected species were seen
    tmpid <- unique(temp$WatchID)
    nodup <- nodup[!nodup$WatchID %in% tmpid, ]
    
    # Add selected species
    data <- rbind(nodup, temp)
    
    # get watchIds where the selected species were observed
    # m <- match(nodup$WatchID, temp$WatchID)
    
    # Add species names and count to the watches
    # nodup$Count[!is.na(m)] <- temp$Count[m[!is.na(m)]]
    # nodup$Alpha[!is.na(m)] <- temp$Alpha[m[!is.na(m)]]
    # nodup$Alpha[!is.na(m)] <- temp$Distance[m[!is.na(m)]]
    
    
    # data$Count[data$Alpha %in% condition[[TYPE_SPECIES]]] <- 0
    
    # data <- nodup
  }
  return(data)
}


## Add the species filter to the subset
addSpeciesFilter <- function(selections, userInfo) {
  filter <- getCurrentFilter(userInfo, SPECIES_FILTER)
  condition <- getCondition(filter)
  
  loginfo("add species filter: %s", selections)
  condition[[TYPE_SPECIES]] <- selections
  
  filter <- setCondition(condition, filter)
  addFilterToSubset(userInfo, filter)
}


################
### Observers
###############

speciesFilterEventHandler <- function(input, output, session, userInfo) {
  event <- isolate(userInfo$event) 
  if (event$type == CHANGE_LANG_EVENT || event$type == CHANGE_PAGE_EVENT){
    ## Update checkbox label
    updateCheckboxInput(session,
                        "useNames",
                        value = session$userData$useSpeciesNames,
                        label = geti18nValue("species.use.common.name", userInfo$lang))
  }
   updateSpeciesInput(input, session, userInfo)
}

## Get the list of species based on subset and display common names or not 
getSpeciesChoices <- function(userInfo, useNames) {
  logdebug("Update species choices")
  names <- isolate(userInfo$species)
  
  ## If a subset already exists, limit the number of species
  ## to the ones found in the subset
  data <- getCurrentSubsetData(userInfo)
  if (!is.empty(data)) {
    sp <- unique(data$Alpha)
    sp <- sp[sp != ""]
    names <- unique(names[names$Alpha %in% sp, ])
  }
  
  ## Use common names
  if (!is.null(useNames) && useNames) {
    lang <-
      geti18nValue(paste0("species.", userInfo$lang), userInfo$lang)
    names <- names[order(names[[lang]]), ]
    labels <- names[[lang]]
    
  } else {
    names <- names[order(names$Alpha), ]
    labels <- names$Alpha
  }
  
  choices <- as.list(names$Alpha)
  names(choices) <- labels
  
  return(list(NOGA = "NOGA"))
  return(choices)
}


## Get selected species in filter
getSelectedSpecies <- function(userInfo) {
  return(getFilterSelection(userInfo, SPECIES_FILTER, TYPE_SPECIES))
}


## Update species input
updateSpeciesInput <- function(input, session, userInfo) {
  selected <- getSelectedSpecies(userInfo)
  choices <- getSpeciesChoices(userInfo, input$useNames)
  updateSelectizeInput(
    session,
    "speciesFilter",
    label = geti18nValue("select.species", userInfo$lang),
    choices = choices,
    selected = selected,
    options = list(
      plugins = list("remove_button"),
      placeholder = geti18nValue("select.all", userInfo$lang)
    )
  )
}


## Main observer function for species selection. All observers are defined here
selectSpeciesObserver <-
  function(input, output, session, userInfo) {
    ##Isolate species names and codes
    userInfo$species <-
      isolate(distinct(dplyr::select(getFullData(as.df = TRUE), Alpha, English, French, Latin)))
    
    
    ## Update choices list if names are selected
    observeEvent(input$useNames, {
      updateSpeciesInput(input, session, userInfo)
      session$userData$useSpeciesNames <- input$useNames
    }, ignoreInit = TRUE)
    
    
    ## If species are selected, add to selection
    observeEvent(input$speciesFilter, {
      addSpeciesFilter(input$speciesFilter, userInfo)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
  }



##############
### Renders
#############


## Main render function for species selection. All UI render function are here
selectSpeciesRender <- function(input, output, session, userInfo) {
  session$userData$useSpeciesNames <- FALSE
  ## Species title
  output$speciesTitle <- renderUI({
    h4(geti18nValue("title.species", userInfo$lang))
  })
  
  ## Species selector
  output$selectSpecies <- renderUI({
    isolate({
      div(class = "selector selectSpecies",
          uiOutput("speciesTitle"),
          tagList(
            checkboxInput(
              "useNames",
              label = geti18nValue("species.use.common.name", userInfo$lang),
              value = session$userData$useSpeciesNames
            ),
            selectizeInput(
              "speciesFilter",
              label = geti18nValue("select.species", userInfo$lang),
              choices = NULL,
              multiple = TRUE,
              options = list(
                plugins = list("remove_button"),
                placeholder = geti18nValue("select.all", userInfo$lang)
              )
            )
          )
        )
    })
  })
}
