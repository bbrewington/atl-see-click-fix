library(shiny); library(leaflet); library(tidyverse); 
library(rgdal); library(rgeos)
library(lubridate)

# SeeClickFix Issues
issues <- read_csv("ATL_seeclickfix_issues_20191119.csv") %>%
  mutate_if(is.POSIXct, funs(force_tz(., tzone = "America/New_York")))

# ATL City Council Districts
# atl_city_districts <- readOGR("atlanta regional commission/City_of_Atlanta_Council_Districts/City_of_Atlanta_Council_Districts.shp")
atl_city_districts <- read_rds("atl_city_districts.rds")
centers <- data.frame(gCentroid(atl_city_districts, byid=TRUE))
centers$y[12] <- centers$y[12] - .01
centers$y[2] <- centers$y[2] - .005
centers$x[3] <- centers$x[3] + .01
atl_city_districts@data$district_center_x <- centers$x
atl_city_districts@data$district_center_y <- centers$y

ui <- fluidPage(
  titlePanel("Atlanta SeeClickFix Map"),
  sidebarLayout(
    sidebarPanel(sliderInput("slider_range", label = "Issue Create Date", 
                             min = ymd("2017-01-01"),
                             max = as.Date(max(issues$created_at), tz = "America/New_York"),
                             value = c(ymd("2017-01-01"), as.Date(max(issues$created_at), tz = "America/New_York")), 
                             step = 1, timeFormat = "%m/%d/%y", ticks = FALSE),
                 sliderInput("slider_markerradius", label = "Marker Size",
                             min = 1, max = 10, value = 3, step = 1),
                 checkboxInput("checkbox_colorstatus", label = "Color Points by Status", value = TRUE)),
    mainPanel(leafletOutput("map", width = "700", height = "600"))
  ))

server <- function(input, output, session) {
  
  # Static Values
  pal_colorstatus <- leaflet::colorFactor(levels = c("Open", "Acknowledged", "Closed"), ordered = TRUE, 
                                          palette = c("#CD1223","#FBD115","#009460"))
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    issues %>% filter(created_at >= input$slider_range[1] & 
                        created_at <= input$slider_range[2])
  })

  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearMarkers() %>%
      addCircleMarkers(radius = input$slider_markerradius, weight = 1, 
                       fillColor = (if(input$checkbox_colorstatus) {
                         ~pal_colorstatus(status) } else {"black"}),
                       stroke = TRUE,
                       color = "black",
                       fillOpacity = 0.7, popup = ~paste(paste0("Issue: ", summary), 
                                                         paste0("Status: ", status), 
                                                         paste0("Created: ", created_at),
                                                         paste0("Last Updated: ", updated_at),
                                                         paste0("Issue Link: <a href=", url, ">", url, "</a>"),
                                                         if_else(is.na(image_square), "", paste0("<img src=", image_square, ">")),
                                                         sep = "<br>")
      ) %>%
      addLabelOnlyMarkers(data = atl_city_districts,
                          lng = ~district_center_x, lat = ~district_center_y, 
                          label = ~NAME,
                          labelOptions = labelOptions(noHide = TRUE, direction = 'top',
                                                      textOnly = TRUE, 
                                                      style = list(
                                                        "color" = "white",
                                                        "font-family" = "helvetica",
                                                        "font-weight" = "bold",
                                                        "font-size" = "12px",
                                                        "border-color" = "rgba(0,0,0,0.5)")), 
                          group = "atl_city_districts") %>%
      addLayersControl(overlayGroups = "atl_city_districts",
                       options = layersControlOptions(collapsed = FALSE), 
                       position = "topright")

  })
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>% addTiles() %>%
      addPolygons(data = atl_city_districts,
                  color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.2,
                  highlightOptions = 
                    highlightOptions(color = "white", weight = 2,
                                     bringToFront = TRUE), 
                  group = "atl_city_districts") %>%
      addLegend("bottomright", pal = pal_colorstatus, values = c("Open", "Acknowledged", "Closed"),
                title = "Issue Status",
                opacity = 1
      )
  })
}

shinyApp(ui, server)
