library(shiny)
library(shinydashboard)
library(tidyverse)
library(arrow)
library(leaflet)
library(waiter)

# Function for calculating distance between two points

get_geo_distance = function(long1, lat1, long2, lat2, units = "meters") {
  loadNamespace("purrr")
  loadNamespace("geosphere")
  longlat1 = purrr::map2(long1, lat1, function(x,y) cbind(x,y))
  longlat2 = purrr::map2(long2, lat2, function(x,y) cbind(x,y))
  distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
  #distance_m = list_extract(distance_list, position = 1)
  distance_m = sapply(distance_list, function(col) { col[1] })
  if (units == "km") {
    distance = distance_m / 1000.0;
  }
  else if (units == "miles") {
    distance = distance_m / 1609.344
  }
  else {
    distance = distance_m
    # This will return in meter as same way as distHaversine function.
  }
  round(distance)
}


# Function for the Pop up information

showShipPopup <- function(df) {

  selectedShip <- df

  contents <- map_chr(seq(nrow(df)), ~{
    as.character(tagList(

      tags$strong(paste("Ship", selectedShip$SHIPNAME[.x], "has sailed", round(selectedShip$distance[.x]), "meters."))

    ))
  }) %>% unlist()

  return(contents)
}

# Read data using Arrow

# df <- read_csv_arrow("data/ships.csv", as_data_frame = TRUE, col_select = c("LAT", "LON", "SHIPNAME", "ship_type", "DATETIME")) %>%
#   filter(!is.na(LON), !is.na(LAT)) %>% 
#   group_by(ship_type, SHIPNAME) %>%
#   mutate(LON1 = lag(LON), LAT1 = lag(LAT)) %>%
#   mutate(distance = get_geo_distance(long1 = `LON`, lat1 = `LAT`,
#                                      long2 = LON1, lat2 = LAT1, units = "meters")) %>%
#   arrange(desc(distance), desc(DATETIME)) %>% 
#   slice(n = 1) %>% 
#   ungroup() %>% 
#   select(c("LAT", "LON", "LAT1", "LON1", "SHIPNAME", "ship_type", "distance", "DATETIME")) %>% 
#   collect()

# 
# df %>%
#   write_csv_arrow("data/ship_arrow_v2.csv")


df <- read_csv_arrow("data/ship_arrow_v2.csv", as_data_frame = TRUE, col_select = c("LAT", "LON", "LAT1", "LON1", "SHIPNAME", "ship_type", "distance", "DATETIME"))



ship_type_choice <- df %>% 
  distinct(ship_type) %>% 
  arrange(ship_type) %>% 
  pull(ship_type)



ui <- dashboardPage(
  
  dashboardHeader(title = "Ship Router"),
  
  dashboardSidebar(
    
    shinyWidgets::pickerInput("vessel_type", "Vessel Type", choices = ship_type_choice), 
    
    vessel_ui(id = "x1")
  ),
  
  dashboardBody(
    
    
    use_waiter(),
    
    leafletOutput("map", height = "600px", width = "1200px")
    
    
    
  )
)


server <- function(input, output, session) {
  
  
  
  vessel_server(id = "x1", df = df, reactive({input$vessel_type}))
  
  # transparent~ish background
  w <- Waiter$new(html = spin_2())
  
  
  observeEvent(input$vessel_type, {
    
    
    w$show()
    
    Sys.sleep(.3)
    
    
    
  })
  
  
  # Create the map
  output$map <- renderLeaflet({
    
    
    
    
    vessel_type <- input$vessel_type
    
    vessel_name <- input$vessel_name
    
    
    df_map <- df %>% 
      filter(SHIPNAME == vessel_name, ship_type == vessel_type) %>% 
      select(SHIPNAME, LAT, LON, LAT1, LON1, distance) %>% 
      mutate(contents_pop = showShipPopup(.))
    
    w$hide()
    
    leaflet(
      df_map
      ) %>%
      addTiles() %>% 
      addMarkers(lng = ~LON, 
                 lat = ~LAT, 
                 icon = awesomeIcons(
                   icon = 'ios-close',
                   iconColor = 'black',
                   library = 'ion',
                   markerColor = "green"
                 ),
                 #label = ~contents_pop,
                 popup = ~contents_pop
                      ) %>%
      addMarkers(lng = ~LON1, lat = ~LAT1,
                 icon = awesomeIcons(
                   icon = 'ios-close',
                   iconColor = 'black',
                   library = 'ion',
                   markerColor = "red"
                 ),
                 popup = ~contents_pop) %>% 
      setView(lng = df_map$LON, lat = df_map$LAT, zoom = case_when(
        df_map$distance < 1000 ~ 14, 
        df_map$distance < 4000 ~ 12, 
        df_map$distance < 7000 ~ 10, 
        df_map$distance < 10000 ~ 8,
        TRUE ~ 6
        
      ))
    
    
    
  })
  
  
  
}

shinyApp(ui, server)