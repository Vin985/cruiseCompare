




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
  obs <- observeEvent(input[[linkId]], {
    if (subset != userInfo$currentSubset) {
      setCurrentSubset(subset, userInfo)
      launchEvent(CHANGE_SUBSET_EVENT, userInfo)
    }
  })
  setSubsetObserver(subset, obs, userInfo)
}


generateSubsetsObservers <- function(input, session, userInfo) {
  subsets <- names(getSubsets(userInfo))
  r <- lapply(subsets, createSubsetLinkObserver, input, userInfo)
}


addNewSubset <- function(input, output, userInfo) {
  loginfo("Add new subset")
  # Get subset label
  subsetLabel <- input$subsetLabel

  # Create the new subset
  subsetId <- createSubset(userInfo, input$subsetLabel)

  # Use filters from another subset?
  if (input$useFilterChoices) {
    # get the selected subset's filter
    subsetFilters <-
      getFiltersBySubset(input$subsetFilterChoices, userInfo)

    # get the names of the list
    # filterNames <- names(getFilterValues(input$subsetFilterChoices, userInfo))
    filterNames <- getFilterNamesList(subsetFilters)

    # Iterate on filternames to see if inputs are selected
    lapply(filterNames, function(filterName, input, userInfo, subsetId) {
      inputId <- paste0("filter.", filterName)
      if (input[[inputId]]) {
        ## filter ids are: 1. Filter name, 2. Filter type
        ids <- strsplit(filterName, "\\.")[[1]]

        ## Get the current condition for the new subset
        condition <-
          getConditionBySubset(subsetId, userInfo, ids[1])
        ## Get the selection of the old subset
        selection <-
          getFilterSelection(userInfo, ids[1], ids[2], input$subsetFilterChoices)

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
    ## Generate observers for subset1 as it will always exist

     # generateSubsetsObservers(input, session, userInfo)

    ## User want to add a subset, display the form
    observeEvent(input$addSubset, {
      showModal(addSubsetModal(userInfo))
    })

    ## Create a new subset
    observeEvent(input$createSubset, {
      id <- addNewSubset(input, output, userInfo)
      if (!is.null(id)) {
        loginfo("creating links")
        createSubsetLinkObserver(id, input, userInfo)
        removeModal(session)
        launchEvent(CHANGE_SUBSET_EVENT, userInfo)
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
    if (userInfo$page != IMPORT_DATA_PAGE) {
      subsets <- names(getSubsets(userInfo, isolate = FALSE))
      selected <- getCurrentSubsetId(userInfo, isolate = FALSE)
      tagList(div(
        class = "subsetLinks",
        lapply(subsets, createSubsetLinkRender, userInfo, selected),
        uiOutput("addSubsetLink", inline = TRUE)
      ))
    }
  })

  output$addSubsetLink <- renderUI({
    if (userInfo$page == SELECTION_PAGE) {
      span(class = "subsetLink addSubset",
           actionLink(
             "addSubset",
             " + ",
             title = geti18nValue("add.subset.title", userInfo$lang)
           ))
    }
  })

  ## Display filter options to create a new subset from another one
  output$subsetFilterSelect <- renderUI({
    if (input$useFilterChoices) {
      loginfo("Displaying list of subsets...")
      isolate({
        # List all subsets
        subsets <- getSubsets(userInfo)
        subsetChoices <- names(subsets)
        names(subsetChoices) <- getSubsetsLabels(subsets)
      })
      tagList(
        selectizeInput(
          "subsetFilterChoices",
          geti18nValue("filter.choices.subset", userInfo$lang),
          choices = subsetChoices,
          multiple = TRUE
        ),
        uiOutput("displayFilters")
        # displayFilters(input, userInfo)
      )
    }
  })

  ## Iterate on filters to generate checkboxes
  output$displayFilters <- renderUI({
    if (!is.empty(input$subsetFilterChoices)) {
      values <- getFilterValues(input$subsetFilterChoices, userInfo)
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
  })
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
  loginfo("refresh modal")
  modalDialog(
    tagList(
      labelWithHelp("subset.create", userInfo$lang, textclass = "h4"),
      textInput(
        "subsetLabel",
        geti18nValue("add.subset.label", userInfo$lang)
      ),
      checkboxInput(
        "useFilterChoices",
        geti18nValue("use.subset.filter", userInfo$lang),
        value = FALSE
      ),
      uiOutput("subsetFilterSelect"),
      div(
        style = "text-align: right;",
        actionButton(
          "createSubset",
          geti18nValue("subset.create", userInfo$lang)
        ),
        actionButton(
          "cancelSubset",
          geti18nValue("button.cancel", userInfo$lang)
        )
      )
    ),
    footer = NULL,
    easyClose = TRUE
  )
}
