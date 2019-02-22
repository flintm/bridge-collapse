library(shiny)
# source("PlotFailedBridgesMapShiny.R")
source("SetupEncoding.R")
load("df.Fail.NBI.Gage.RData")
BridgesDataFrame <- df.Fail.NBI.Gage
BridgesDataFrame[12,"DATE_P_BEGIN_USGS"] <- "1964-04-04"
BridgesDataFrame[BridgesDataFrame$COMMENT_LINKED_HURRICANE=="","COMMENT_LINKED_HURRICANE"] <- "none"
BridgesDataFrame[BridgesDataFrame$FAIL_TYPE=="","FAIL_TYPE"] <- "U"
BridgesDataFrame$FAIL_TYPE <- factor(BridgesDataFrame$FAIL_TYPE, levels = c("PC","TC","U"),labels = c("partial","total","unknown"))
pal <- colorFactor(colorsP$Fail, names(colorsP$Fail))


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  filteredBridges <- reactive({
    subset(BridgesDataFrame,  DRAIN_SQKM >= input$drainArea[1] &  DRAIN_SQKM <= input$drainArea[2] &
             FAIL_CAUS_CODE %in% input$FailCause)
  })
  
  bridgesInBounds <- reactive({
    if (is.null(input$map_bounds)) return(BridgesDataFrame[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(filteredBridges(),
           LATDD >= latRng[1] & LATDD <= latRng[2] &
             LONGDD >= lngRng[1] & LONGDD <= lngRng[2])
  })
  
  output$histTfail <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(bridgesInBounds()) == 0) return(NULL)
    # print(head(bridgesInBounds()$T_FAIL_D_HECD_USGS))
    hist(bridgesInBounds()$T_FAIL_D_HECD_USGS,
         # breaks = centileBreaks,
         main = "Return periods of failure flows",
         xlab = "T (years)" ,
         xlim = range(BridgesDataFrame$T_FAIL_D_HECD_USGS)#,
         # col = '#00DD00',
         # border = 'white'
         )
  })
  # output$Thist <- hist(filteredBridges$T_FAIL_D_HECD_USGS,plot = TRUE)
  
  output$map <- renderLeaflet({
    leaflet(BridgesDataFrame) %>%
      addTiles(group = "OSM (default)") %>%
      # addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4) #s%>%
#       addLayersControl(
#         baseGroups = c("OSM (default)", "Toner Lite"),
#         overlayGroups = c("Bridges", "Gages"),
#         options = layersControlOptions(collapsed = FALSE)
#       )
  })
  
#   observe({
#     if ()
#     leaflet("map") 
#   })
  observe({
    leafletProxy("map", data = filteredBridges()) %>%
      clearShapes() %>% clearControls()
      # gages
      if ("GAUGES" %in% input$plotTypes){
        leafletProxy("map", data = filteredBridges()) %>%
      addCircles(lat = ~LAT_GAGE, lng = ~LNG_GAGE, radius = ~sqrt(DRAIN_SQKM)*1000, color = "black", 
                 fill = TRUE, opacity = 1, weight = 1, fillOpacity = 0.05,
                 group = "Gages", layerId = ~STAID) %>%
      addCircleMarkers(lat = ~LAT_GAGE, lng = ~LNG_GAGE, radius = 1, color = "black", fill = TRUE, opacity = 1, weight = 1, 
                       group = "Gages", layerId = ~STAID)
        }
      # bridges
    if ("BRIDGES" %in% input$plotTypes){
      leafletProxy("map", data = filteredBridges()) %>%
      addCircles(lat = ~LATDD, lng = ~LONGDD, radius = ~sqrt(DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM)*1000, color = ~pal(FAIL_CAUS_CODE), 
                 fill = TRUE, opacity = 1, weight = 1, fillOpacity = 0.2, stroke = FALSE,
                 group = "Bridges", layerId = ~ID) %>%
      addCircleMarkers(lat = ~LATDD, lng = ~LONGDD, radius = 1, color = ~pal(FAIL_CAUS_CODE), fill = TRUE, opacity = 1,
                       weight = 1, group = "Bridges", layerId = ~ID) %>%
      addLegend("bottomleft", pal = pal, values = ~FAIL_CAUS_CODE,
                title = "Failure Cause",
                opacity = 1
      )
    }
  })
  
  
  showBridgePopup <- function(ID, lat, lng) {
    selectedBridge <- BridgesDataFrame[BridgesDataFrame$ID == ID,]
    content <- as.character(tagList(
      tags$h4("Failed bridge ID:",selectedBridge$ID),
      tags$strong(HTML(sprintf("%s, %s - %s",
                               selectedBridge$STATE, format(selectedBridge$YR_BLT_EST,"%Y"), format(selectedBridge$YR_FAIL_EST,"%Y")
      ))), 
      tags$br(),
      sprintf("Location: %s", selectedBridge$LOCATION), tags$br(),
      sprintf("Feature under: %s", selectedBridge$FEAT_UND), tags$br(),
      sprintf("Drainage area: %s sq.km", round(selectedBridge$DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM)), tags$br(),
      sprintf("Bridge material: %s", selectedBridge$MAT), tags$br(),
      sprintf("Bridge type: %s", selectedBridge$TYPE), tags$br(),
      sprintf("Failure cause: %s", selectedBridge$FAIL_CAUS), tags$br(),
      sprintf("Fail date: %s", tolower(selectedBridge$BOOL_KNOWN_FAIL_DATE)), tags$br(),
      sprintf("Link to tropical cyclone: %s", selectedBridge$COMMENT_LINKED_HURRICANE), tags$br(),
      sprintf("Collapse (total or partial): %s", selectedBridge$FAIL_TYPE), tags$br(),
      sprintf("Comment: %s", selectedBridge$COMMENTS)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = ID)
  }
  
  showGagePopup <- function(STAID, lat, lng) {
    selectedGage <- BridgesDataFrame[BridgesDataFrame$STAID == STAID,][1,]
    content <- as.character(tagList(
      tags$h4("Linked Gauge STAID:",selectedGage$STAID),
      tags$strong(HTML(sprintf("%s, peaks %s - %s",
                               selectedGage$STATE_CODE, substr(selectedGage$DATE_P_BEGIN_USGS,1,4), substr(selectedGage$DATE_P_END_USGS,1,4)
      ))), 
      tags$br(),
      sprintf("Location: %s", selectedGage$STANAME), tags$br(),
      sprintf("Drainage area: %s sq.km", round(selectedGage$DRAIN_SQKM)), tags$br(),
      sprintf("Distance to bridge: %s km", signif(selectedGage$DIST_TO_GAGE),2), tags$br(),
      sprintf("Drainage ratio gage/bridge: %s", signif(selectedBridge$DRAIN_SQKM/selectedBridge$DRAIN_AREA_BRIDGE_NHDFLOWPLUS_SQKM,2)), tags$br(),
      sprintf("Regulation: %s", selectedGage$COMMENT_REGULATION)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = STAID)
  }
  
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event) | input$Popups == "NONE")
      return()
    if (input$Popups == "BRIDGE"){
      isolate({
        showBridgePopup(event$id, event$lat, event$lng)
      })
    }
    if (input$Popups == "GAUGE"){
      isolate({
        showGagePopup(event$id, event$lat, event$lng)
      })
    }
  })
  observe({
    cols <- switch(input$data,
                   "Failed Bridge" = c(1:21,25:28,30,33),
                   "Linked National Bridge Inventory"    = c(1,39:50),
                   "Linked USGS Gauge"   = c(1,53:63,69),
                   "Flow and return period analysis" = c(1,32,66:67,70:105)
                   )
    output$table <- renderDataTable({BridgesDataFrame[,cols]})
  
  })
  
})