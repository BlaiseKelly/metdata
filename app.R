library(shiny)
library(leaflet)

load("/data/mapinfo.RData")

ui <- fluidPage(
  
  # App title ----
  titlePanel("EU Weather Data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width = 3,
                 # Include clarifying text ----
                 helpText("The map shows the coverage of the NOAA meteorological stations,
                           the ERA5 satellite data and the UK environment agency weather data"),
                 
                 # Input: Select the random distribution type ----
                 selectInput("latitude", label = h3("Select Route"), 
                             choices = lats,
                             selected = lats[70]),
                 
                 # Input: Select the random distribution type ----
                 selectInput("longitude", label = h3("Select Route"), 
                             choices = lons,
                             selected = lons[81]),
                 
                 radioButtons("met_data", label = h3("Source"),
                              choices = list("NOAA", "ERA5", "EA"), 
                              selected = "ERA5"),
                 
                 radioButtons("download", label = h3("Download format"),
                              choices = list(".csv" = 1, ".met" = 2, ".ews" = 3), 
                              selected = 1),
                 
                 sliderInput("years", label = h3("Year Range"), min = 1979, 
                             max = d8, value = c(2019, 2019))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("world_map", leafletOutput("map", width = "100%", height = "1000px")),
                  tabPanel("About", includeMarkdown("data/README.md"))
      )
      
      
      
    )
  ))

server <- shinyServer(function(input, output) {

  # ecmwf_areas <- readRDS("data/land_areas.RDS")
  # ecmwf <- st_centroid(ecmwf_areas)
  # ecmwf <- st_cast(ecmwf, "POINT")
  
  #met_stations <- read.csv("data/weather_stations.csv", header = FALSE)
  #EU_codez <- readRDS("data/codez.RDS")
  #met_info <- getMeta(country = met_stations$V1)
  #met_info2 <- getMeta(country = EU_codez)
  
  
  
  cntnt_met <- paste(paste(
    met_info$station,
    paste("Code:", met_info$code),
    paste("Start:", met_info$begin),
    paste("End:", met_info$end),
    sep = "<br/>"))
  
  cntnt_rain <- paste(paste(
    rain_data$station_name,
    paste("Code:", rain_data$station_number),
    paste("Wettest Day (date):", rain_data$wettest_day_date),
    paste("Wettest Day (mm):", rain_data$wettest_day_mm),
    paste("Last Updated:", rain_data$last_updated),
    paste("Download Link:", rain_data$link),
    sep = "<br/>"))
  
  cntnt_ecmwf <- paste(paste(
    ecmwf$lat,
    paste("Minimum Latitude:", ecmwf$lat-0.25),
    paste("Maximum Latitude:", ecmwf$lat+0.25),
    paste("Minimum Longitude:", ecmwf$lon-0.25),
    paste("Maximum Longitude:", ecmwf$lon+0.25),
    paste("Parameters:", "lots of parameters"),
    sep = "<br/>"))
  
  rainfall_icon <- makeIcon(
    iconUrl = "data/Raindrop.png",
    iconWidth = 25, iconHeight = 25,
  )
  
  weather_icon <- makeIcon(
    iconUrl = "data/weather_station.png",
    iconWidth = 35, iconHeight = 35,
  )
  
  satellite_icon <- makeIcon(
    iconUrl = "data/satellite.ico",
    iconWidth = 25, iconHeight = 25,
  )
  
  lat <- 51.456481
  lon <- -2.586347
  
  # pal = colorBin("viridis", bins = c(0, 2000, 4000, 6000, 8000, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000,300000), domain = routez$total_t)
  # pal_t <- colorFactor(pal = c("red","green"), 
  #                      domain = c("HIGHER", "LOWER")) 
  # pal_s <- colorFactor(pal = c("green","red"), 
  #                      domain = c("HIGHER", "LOWER")) 
  
  html_legend <- "<img src='data/Raindrop.png'style='width:30px;height:30px;'> EA Rain Gauges<br/>
<img src='data/weather_station.png'style='width:30px;height:30px;'> NOAA Met Stations<br/>
<img src='data/satellite.ico'style='width:30px;height:30px;'> ERA-5 Satellite data"

  
  # create the leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lon, lat, zoom = 12) %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri Satellite") %>%
      addProviderTiles("OpenStreetMap", group = "Open Street Map") %>% 
      leafem::addMouseCoordinates(proj4string = CRS(ukgrid), native.crs = F) %>%
      addMiniMap() %>%
      addMarkers(data = rain_data,
                 icon = rainfall_icon, popup = cntnt_rain, group = "rainfall") %>%
      addMarkers(data = met_info,
                 icon = weather_icon, popup = cntnt_met, group = "met stations") %>%
      addMarkers(data = ecmwf,
                 icon = satellite_icon, popup = cntnt_ecmwf, group = "satellite data") %>%
      addPolygons(data = ecmwf_areas, color = "grey", weight = 0.5, smoothFactor = 0.5,
                  opacity = 0.6, fillOpacity = 0.05,
                  fillColor = "grey", popup = cntnt_ecmwf, group = "satellite data") %>% 
      addControl(html = html_legend, position = "bottomleft") %>% 
      addLayersControl(overlayGroups = c("rainfall", "met stations", "satellite data"),
                       baseGroups = c("CartoDB", "Esri Satellite", "Open Street Map"),
                       options = layersControlOptions(collapsed = FALSE), position = "topright") %>% 
      hideGroup(c())
  })
  
  output$ui <- renderUI({
    if (is.null(input$met_data))
      return()
  
  # Depending on input$input_type, we'll generate a different
  # UI component and send it to the client.
  switch(input$met_data,
         "NOAA" = selectInput("dynamic", "Dynamic",
                              choices = met_codes,
                              selected = "030050-99999"),
  )
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- "data/README.md"
    dataset
  })
  
})

# Return a Shiny app object
shinyApp(ui = ui, server = server)
