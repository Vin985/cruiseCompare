


## Name of the filter
REGION_FILTER <- "region"

## Type of filters
TYPE_REGION <- "region"

MISC_REGION_REDRAW <- "redraw"

###################
### Initialize
##################


## Main entry for the filter. Register Observers and Renders
initRegionFilter <- function(input, output, session, userInfo) {
  selectRegionObserver(input, output, session, userInfo)
  selectRegionRender(input, output, session, userInfo)
}


#####################
### Util functions
####################


## Register our custom javascript file to extend leaflet
extraDependencies <- function(map) {
  map$dependencies <- c(map$dependencies,
                        list(
                          htmltools::htmlDependency(
                            "extra",
                            src = c(href = "js"),
                            version = "0.1.0",
                            script = c("draw-extras.js")
                          )
                        ))
  map
  
}


## Extension of the draw tool of leaflet-extras to clear the selection
clearLayerGroup <- function(map, group) {
  invokeMethod(map, getMapData(map), "removeDrawnFeatures", group)
}


##############
###  Filter
#############


## Filter data by region
regionFilter <- function(data, condition) {
  logdebug("Filtering by region...")
  pols <- drawSelections2sp(condition[[TYPE_REGION]], PROJ_AREA)
  if (!is.null(pols)) {
    data <- data[!is.na(over(data, pols)),]
  }
  data
}


## add the region filter to the subset
addRegionFilter <- function(selections, userInfo) {
  filter <- getCurrentFilter(userInfo, REGION_FILTER)
  condition <- getCondition(filter)
  
  logdebug("add region filter")
  condition[[TYPE_REGION]] <- selections
  
  filter <- setCondition(condition, filter)
  addFilterToSubset(userInfo, filter)
}


################
### Observers
###############
regionFilterEventHandler <-
  function(input, output, session, userInfo) {
    event <- isolate(userInfo$event)
    if (event$type == CHANGE_PAGE_EVENT &&
        userInfo$page == SELECTION_PAGE) {
      userInfo$redrawMap <- userInfo$redrawMap + 1
    }
    if (event$type == CHANGE_LANG_EVENT ||
        (event$type == CHANGE_PAGE_EVENT)) {
      #&& userInfo$page != SELECTION_PAGE)) {
      return()
    }
    
    updateMap(userInfo)
  }

## Display observations on the map
displayMarkers <- function(map, data) {
  loginfo("rendering markers")
  clearGroup(map, "observations")
  if (!is.empty(data)) {
    addCircleMarkers(
      map,
      data = data,
      lng = ~ LongStart,
      lat = ~ LatStart,
      radius = 2,
      fillColor = "blue",
      group = "observations",
      stroke = TRUE,
      color = "black",
      weight = 1,
      clusterOptions = markerClusterOptions()
    )
  }
}


## Display the region selection on the map
displayRegionSelection <- function(map, userInfo) {
  ## Add the selections
  loginfo("rendering selection")
  clearLayerGroup(map, "draw")
  filter <- getCurrentFilter(userInfo, REGION_FILTER)
  if (!is.null(filter)) {
    condition <- getCondition(filter)
    if (!is.null(condition)) {
      pols <- selection2polygon(condition[[TYPE_REGION]])
      lapply(pols, function(x) {
        addPolygons(
          map,
          data = x,
          group = "draw",
          color = "tomato",
          opacity = 0.4,
          weight = 1
        )
      })
    }
  }
  map
}


## Update the map
updateMap <- function(userInfo) {
  leafletProxy("regionMap", deferUntilFlush = TRUE) %>%
    ## Add the selections
    displayRegionSelection(userInfo) %>%
    ## Add the markers
    displayMarkers(getCurrentSubsetData(userInfo))
}


## Main observer function for region selection. All observers are defined here
selectRegionObserver <- function(input, output, session, userInfo) {
  ## Check if data has changed and update map
  # observeEvent(userInfo[[SUBSET_DATA_EVENT]], {
  #   updateMap(userInfo)
  # })
  
  
  ## Check if map selection has been made
  observeEvent(input$regionMap_draw_all_features, {
    selections <- input$regionMap_draw_all_features$features
    addRegionFilter(selections, userInfo)
  })
  
  
  # ## Update selections if subset changes
  # observeEvent(userInfo[[CHANGE_SUBSET_EVENT]], {
  #   updateMap(userInfo)
  # })
}


##############
### Renders
#############

## Main render function for map selection. All UI render function are here
selectRegionRender <- function(input, output, session, userInfo) {
  userInfo$redrawMap <- 0
  
  ## Map selector
  output$selectRegion <- renderUI({
    loginfo("rendering region")
    tagList(uiOutput("mapTitle"),
            uiOutput("displayMap"))
  })
  
  ## Map title
  output$mapTitle <- renderUI({
    h4(geti18nValue("title.region", userInfo$lang))
  })
  
  ## Map container
  output$displayMap <- renderUI({
    loginfo("renderMap")
    div(class = "displayMap",
        leafletOutput("regionMap", height = "100%", width = "100%"))
  })
  
  ## Map
  output$regionMap <-  renderLeaflet({
    redraw <- userInfo$redrawMap > 0
    withProgress({
      map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addTiles(group = "Base") %>%
        addDrawToolbar(
          position = "topright",
          targetGroup = 'draw',
          circleOptions = FALSE,
          markerOptions = FALSE,
          polylineOptions = FALSE,
          editOptions = editToolbarOptions(
            selectedPathOptions = selectedPathOptions(),
            allowIntersection = FALSE
          )
        )  %>% extraDependencies() %>%
        setView(lng = -65,
                lat = 49,
                zoom = 5) %>%
        addLayersControl(
          overlayGroups = c('draw'),
          baseGroups = c("Base"),
          options = layersControlOptions(collapsed = TRUE),
          position = "bottomright"
        )
      if (redraw) {
        updateMap(userInfo)
        
      }
      map
    }, message = "Patience...")
  })
}
