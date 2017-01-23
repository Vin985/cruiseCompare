










###################
### Initialize
##################

changeSubsets <- function(input, output, session, userInfo) {
  ## Observers for changing subsets
  changeSubsetsObserver(input, output, session, userInfo)
  
  ## Render for changing subsets
  changeSubsetsRender(input, output, session, userInfo)
  
}


################
### Observers
###############

createSubsetLinkObserver <- function(subset, input, userInfo) {
  linkId <- paste0(subset, "Link")
  observeEvent(input[[linkId]], {
   setCurrentSubset(subset, userInfo)
  })
}


generateSubsetsObservers <- function(input, session, userInfo) {
  subsets <- names(getSubsets(userInfo))
  lapply(subsets, createSubsetLinkObserver, input, userInfo)
}


addNewSubset <- function(input, output, userInfo) {
  # Get subset label
  subsetLabel <- input$subsetLabel

    # Create the new subset
  subsetId <- createSubset(userInfo, input$subsetLabel)
  
  # Use filters from another subset?
  if (input$useFilterChoices) {
    
    # get the selected subset's filter
    # subsetFilter <- getFiltersBySubset(input$subsetChoice, userInfo)
   
    
     # get the names of the list
    filterNames <- names(getFilterValues(input$subsetChoice, userInfo))
    
    # Iterate on filternames to see if inputs are selected
    lapply(filterNames, function(filterName, input, userInfo, subsetId) {
      inputId <- paste0("filter.", filterName)
      if (input[[inputId]]) {
        
        ## filter ids are: 1. Filter name, 2. Filter type
        ids <- strsplit(filterName, "\\.")[[1]]
        
        ## Get the current condition for the new subset
        condition <- getConditionBySubset(subsetId, userInfo, ids[1])
        ## Get the selection of the old subset
        selection <-
          getFilterSelection(userInfo, ids[1], ids[2], input$subsetChoice)
        
        condition[[ids[2]]] <- selection
        
        filter <- ECFilter(type = ids[1],
                           condition = condition)
        
        addFilterToSubset(userInfo,
                          filter = filter,
                          subsetId = subsetId)
      }
    }, input, userInfo, subsetId)
    
  }
  subsetId
}


changeSubsetsObserver <-
  function(input, output, session, userInfo) {
    ## Generate observers for subset links
    generateSubsetsObservers(input, session, userInfo)
    
    ## User want to add a subset, display the form
    observeEvent(input$addSubset, {
      showModal(addSubsetModal(userInfo))
    })
    
    ## Create a new subset
    observeEvent(input$createSubset, {
      id <- addNewSubset(input, output, userInfo)
      if (!is.null(id)) {
        createSubsetLinkObserver(id, input, userInfo)
        removeModal(session)
      }
    })
    
    ## Close the create subset window
    observeEvent(input$cancelSubset, {
      removeModal(session)
    })
    
  }


##############
### Renders
#############



createSubsetLinkRender <- function(subsetId, userInfo, selected) {
  id <- paste0(subsetId, "Link")
  sel <- ""
  if (subsetId == selected) {
    sel <- " selected"
  }
  subset <- getSubset(subsetId, userInfo)
  span(class = paste0("subsetLink", sel),
       actionLink(id, getLabel(subset)))
}


changeSubsetsRender <- function(input, output, session, userInfo) {
  output$subsetTabs <- renderUI({
    subsets <- names(getSubsets(userInfo))
    selected <- getCurrentSubsetId(userInfo)
    tagList(div(
      class = "subsetLinks",
      lapply(subsets, createSubsetLinkRender, userInfo, selected),
      span(class = "subsetLink addSubset", actionLink(
        "addSubset",
        " + ",
        title = geti18nValue("add.subset.title", userInfo$lang)
      ))
    ))
  })
  
  
  ## Display filter options to create a new subset from another one
  output$subsetChoices <- renderUI({
    if (input$useFilterChoices) {
      loginfo("Displaying list of subsets...")
      isolate({
        # List all subsets
        subsets <- getSubsets(userInfo)
        subsetChoices <- names(subsets)
        names(subsetChoices) <-
          unlist(lapply(subsets, function(x) {
            return(getLabel(x))
          }))
      })
      tagList(
        selectizeInput(
          "subsetChoice",
          geti18nValue("filter.choices.subset", userInfo$lang),
          choices = subsetChoices
        ),
        displayFilters(input, userInfo)
      )
    }
  })
}


## Get the values of the filters and convert them in a single
## string if they are of atomic type
getConditionValues <- function(condition) {
  ## Create a single string with values
  if (is.atomic(condition)) {
    return(paste0(condition, collapse = "; "))
  } else {
    ## For non atomic types (e.g: region selection)
    return("")
  }
}

## Get data from filter conditions
getFilterData <- function(filter) {
  condition <- getCondition(filter)
  values <- lapply(condition, getConditionValues)
  names(values) <- names(condition)
  return(values)
}

getFilterValues <- function(subset, userInfo) {
  filters <- getFiltersBySubset(subset, userInfo)
  values <- lapply(filters, getFilterData)
  names(values) <- names(filters)
  unlist(values)
}

## Iterate on filters to generate checkboxes
displayFilters <- function(input, userInfo) {
  if (!is.empty(input$subsetChoice)) {
    values <- getFilterValues(input$subsetChoice, userInfo)
    tagList(div(
      class = "useSubsetFilters",
      lapply(
        names(values),
        addFilterCheckbox,
        values = values,
        lang = userInfo$lang
      )
    ))
  }
}


## Add a checkbox for the filters
addFilterCheckbox <-
  function(id,
           values,
           lang) {
    loginfo("subfilter: %s", id)
    value <- values[id]
    div(class = "filter",
        checkboxInput(paste0("filter.", id),
                      geti18nValue(paste0("filter.", id), lang)),
        div(class = "filterValue", paste0(value, collapse = "; ")))
  }


addSubsetModal <- function(userInfo) {
  modalDialog(
    tagList(
      textInput(
        "subsetLabel",
        geti18nValue("add.subset.label", userInfo$lang)
      ),
      checkboxInput(
        "useFilterChoices",
        geti18nValue("use.subset.filter", userInfo$lang),
        value = FALSE
      ),
      uiOutput("subsetChoices"),
      div(
        style = "text-align: right;",
        actionButton(
          "createSubset",
          geti18nValue("subset.create", userInfo$lang)
        ),
        actionButton(
          "cancelSubset",
          geti18nValue("subset.cancel", userInfo$lang)
        )
      )
    ),
    footer = NULL,
    easyClose = TRUE
  )
}
