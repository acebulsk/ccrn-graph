#### Station Map Tab ####

stations <- readRDS('data/ccrn_sites.rds') %>% mutate(across(station_no:station_longitude, as.numeric))

#render leaflet
output$map <- renderLeaflet({
  leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles("CartoDB.Positron") %>%
    # addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    # addLayersControl(baseGroups = c("Default", "Satellite"), options = layersControlOptions(collapsed = FALSE), position = ("topleft")) %>%
    setView(lng = -115, lat = 53.75, zoom = 5) %>%
    addMarkers(data = stations, layerId = ~station_id, ~station_longitude, ~station_latitude, label = ~station_name, labelOptions = labelOptions(textsize = "12px"))
})

# # When map is clicked, show a popup with city info
observe({
  leafletProxy("map") %>% clearPopups()
  event <- input$map_marker_click
  zoom <- isolate(input$map_zoom)

  if (is.null(event))
    return()

  isolate({
    showPopup(event, event$lat, event$lng, zoom)
  })
})

