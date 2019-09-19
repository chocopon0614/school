# The site I referred to
# https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example
#
# Atsushi Mori 21/12/2018 first created

library(shiny)
library(leaflet)
library(scales)
library(dplyr)
options(scipen=20)

regiondata <- read.csv("Region.csv",header = T)

ui <- navbarPage("WorkProduct", id="graph",
                 div(class="outer",
                     
                     tags$head(
                       includeCSS("styles.css")
                     ),
                     
                 tabPanel("Map",
                    
                    
                  leafletOutput("map",width=1900, height=900),
                  

                  absolutePanel(id = "controls", fixed = TRUE,class = "panel panel-default", 
                    draggable = TRUE, top = 90, left = "auto", right = 20, bottom = "auto",
                    width = 330, height = "auto",
                                 
                    h2("House Explorer"),

                    selectInput("bedroom_min", "Bedroom(min~max)", choices = c(1,2,3),
                          selected = 1),
                    selectInput("bedroom_max", "", choices = c(1,2,3),
                          selected = 3),
                  
                    selectInput("bathroom_min", "Bathroom(min~max)", choices = c(1,2,3),
                          selected = 1),
                    selectInput("bathroom_max", "", choices = c(1,2,3),
                          selected = 3),
                    
                    selectInput("price_min", "Price(min~max)", choices = c(100000,250000,500000,750000,1000000,1250000,1500000,1750000),
                          selected = 100000),
                    selectInput("price_max", "", choices = c(250000,500000,750000,1000000,1250000,1500000,1750000,2000000),
                          selected = 2000000)

                    )
           )
        )
    )

server <- function(input, output) {
  # leaflet, the basic map 
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      )%>% setView(lng = 174.767017, lat = -36.868910, zoom = 13)  
  })
  
  regiondataALL <- reactive({
    
    bedroomby_min_in <- as.numeric(input$bedroom_min)
    bedroomby_max_in <- as.numeric(input$bedroom_max)
    
    if(bedroomby_min_in > bedroomby_max_in) {
      bedroomby_min <- bedroomby_max_in
      bedroomby_max <- bedroomby_min_in
    }else{
      bedroomby_min <- bedroomby_min_in
      bedroomby_max <- bedroomby_max_in
    }
    
    bathroomby_min_in <- as.numeric(input$bathroom_min)
    bathroomby_max_in <- as.numeric(input$bathroom_max)
    
    if(bathroomby_min_in > bathroomby_max_in) {
      bathroomby_min <- bathroomby_max_in
      bathroomby_max <- bathroomby_min_in
    }else{
      bathroomby_min <- bathroomby_min_in
      bathroomby_max <- bathroomby_max_in
    }

    priceby_min_in <- as.numeric(input$price_min)
    priceby_max_in <- as.numeric(input$price_max)
    
    if(priceby_min_in > priceby_max_in) {
      priceby_min <- priceby_max_in
      priceby_max <- priceby_min_in
    }else{
      priceby_min <- priceby_min_in
      priceby_max <- priceby_max_in
    }
    
    # create dataset with the inputted condition
    df <- subset(regiondata,
                 regiondata$Bedroom >= bedroomby_min & regiondata$Bedroom <= bedroomby_max &
                 regiondata$Bathroom >= bathroomby_min & regiondata$Bathroom <= bathroomby_max &
                   regiondata$Price_min >= priceby_min & regiondata$Price_max <= priceby_max )

    # group by area,latitude,longtitude
    df %>% group_by(Area,Latitude,Longitude) %>% 
      summarise(Bedroom_min=min(Bedroom),Bedroom_max=max(Bedroom),
                Bathroom_min=min(Bathroom),Bathroom_max=max(Bathroom),
                Price_min=min(Price_min),Price_max=max(Price_max),List=sum(List)) %>% ungroup() ->df2
    
  })
  
  observe({
    
    palfac <- colorFactor(palette=c("red", "blue", "green", "purple","yellow" ,"orange","gold","brown","pink"), domain=regiondataALL()$Area)
    
    leafletProxy("map", data = regiondataALL()) %>% clearShapes() %>%
      addCircles(~Longitude, ~Latitude, radius = ~sqrt(List)*125,layerId = ~Area,
                 fillOpacity=0.5, color=~palfac(Area) ) %>%
      addLegend("bottomleft", pal=palfac, values=~Area, title="Area",
                layerId="colorLegend")
    
    #add the pop-up into the leaflet
    showPopup <- function(areaby, lat, lng) {
      selecteddata <- subset(regiondataALL(),regiondataALL()$Area == areaby)      
      content <- as.character(tagList(
        tags$h3(selecteddata$List,"hits!"),
        tags$strong(HTML(sprintf("%s",selecteddata$Area))), tags$br(),
        sprintf("Bedroom: %s ~ %s", selecteddata$Bedroom_min,selecteddata$Bedroom_max), tags$br(),
        sprintf("Bathroom: %s ~ %s", selecteddata$Bathroom_min,selecteddata$Bathroom_max),tags$br(),
        sprintf("Price: %s ~ %s", dollar(selecteddata$Price_min),dollar(selecteddata$Price_max))
      ))
      leafletProxy("map") %>% addPopups(lng, lat, content, layerId = areaby)
      
    }
    
    #event driven of mouse click
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showPopup(event$id, event$lat, event$lng)
    })
    
  })   
}

shinyApp(ui = ui, server = server)

