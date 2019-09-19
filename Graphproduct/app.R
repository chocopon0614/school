# The site I referred to
# https://github.com/rstudio/shiny-examples/tree/master/054-nvd3-line-chart-output
#
# Atsushi Mori 21/12/2018 first created

library(shiny)
library(dplyr)
library(forecast)
library(lubridate)
library(tseries)
library(xts)
options(scipen=19)


house_price <- read.csv("all_area.csv",header = T)
house_price$Date = as.Date(house_price$Date)


ui <-
  navbarPage("WorkProduct", id="graph",

              tabPanel("Graph",

                 singleton(tags$head(
                         tags$link(rel = "stylesheet", type = "text/css",href = "nv.d3.css"),
                         tags$script(src = "d3.v3.min.js"),
                         tags$script(src = "nv.d3.js"),
                         tags$script(src = "graph.js"),
                         tags$script(src = "graph2.js"))),

                 
                  titlePanel("Housing Price Data"),
                      
                    sidebarPanel(
                    sliderInput(inputId="sliderDate",
                                label="DATE",min=min(house_price$Date),max=max(house_price$Date),
                                value = min(house_price$Date),
                                step = 10,timeFormat = "%b-%Y",
                                animate = animationOptions(interval = 70, loop = TRUE)
                        )
                    
                    ),
                 
                      
                  sidebarPanel(
                    selectInput("area1", "Area1",
                                    choices = c("NONE",names(house_price[c(2:ncol(house_price))])),selected = names(house_price[2])),
                    selectInput("area2", "Area2",
                                    choices = c("NONE",names(house_price[c(2:ncol(house_price))])),selected = names(house_price[3])),
                    selectInput("area3", "Area3",
                                    choices = c("NONE",names(house_price[c(2:ncol(house_price))])),selected = names(house_price[4]))
                        
                    ),
                      
                    sidebarPanel(
                      selectInput("area4", "Area4",
                                    choices = c("NONE",names(house_price[c(2:ncol(house_price))])),selected = names(house_price[5])),
                      selectInput("area5", "Area5",
                                    choices = c("NONE",names(house_price[c(2:ncol(house_price))])),selected = names(house_price[6])),
                      selectInput("area6", "Area6",
                                    choices = c("NONE",names(house_price[c(2:ncol(house_price))])),selected = names(house_price[7]))
                    ),

                 div(tableOutput("values"),style="height:200px"),
                 
                 # bind the javascript file with the class name (graph.js)
                 div(id="graph",  class="nvd3-linechart",tag("svg", list(style="height:500px;width:100%")))

             ),
             
             tabPanel("Prediction",
                      
                      titlePanel("House Price Prediction"),

                      sidebarPanel(
                        selectInput("parea1", "Area1",
                                    choices = names(house_price[c(2:ncol(house_price))]),selected = names(house_price[2]))

                      ),
                
                h4("Akaike information criterion"),
                tableOutput("values2"),

                # bind the javascript file with the class name (graph2.js)
                div(id="prediction",  class="nvd3-linechart2",tag("svg", list(style="height:500px;width:100%")))
                      
             )
             
         )


server <- function(input, output) {
   
  output$graph <-   function(){
    
    inputdate <- input$sliderDate
    area <- c()
    
    area1 <- input$area1
    area2 <- input$area2
    area3 <- input$area3
    area4 <- input$area4
    area5 <- input$area5
    area6 <- input$area6
    
    if(area1 != "NONE") area <- append(area, area1) 
    if(area2 != "NONE") area <- append(area, area2) 
    if(area3 != "NONE") area <- append(area, area3) 
    if(area4 != "NONE") area <- append(area, area4) 
    if(area5 != "NONE") area <- append(area, area5) 
    if(area6 != "NONE") area <- append(area, area6) 
    
    inputdate <- floor_date(inputdate, "month")
    row <- grep(inputdate, house_price$Date)
    
    areav <- c()
    if(area1 != "NONE") areav <- append(areav,house_price[area1][row,])
    if(area2 != "NONE") areav <- append(areav,house_price[area2][row,])
    if(area3 != "NONE") areav <- append(areav,house_price[area3][row,])
    if(area4 != "NONE") areav <- append(areav,house_price[area4][row,])
    if(area5 != "NONE") areav <- append(areav,house_price[area5][row,])
    if(area6 != "NONE") areav <- append(areav,house_price[area6][row,])
    
    
    # List (key =  ,(( x = , y= ),( x = , y= ),,,))
    list1 <- mapply(function(key) {
      
      values <- mapply(function(i, val) {
        list(x = i, y = val)
      }, area,areav, SIMPLIFY=FALSE, USE.NAMES=FALSE)
      
       list(key = "key", values = values)
    }, "key" , SIMPLIFY=FALSE, USE.NAMES=FALSE)
  }
  
  TableValues <- reactive({
    inputdate <- input$sliderDate
    inputdate <- floor_date(inputdate, "month")

    area <- c()
    areav <- c()
    row <- grep(inputdate, house_price$Date)
    
    area1 <- input$area1
    area2 <- input$area2
    area3 <- input$area3
    area4 <- input$area4
    area5 <- input$area5
    area6 <- input$area6
    
    if(area1 != "NONE" & !any(area == area1))
      {area <- append(area, area1) 
       areav <- append(areav,house_price[area1][row,])}
    if(area2 != "NONE" & !any(area == area2)){area <- append(area, area2)
       areav <- append(areav,house_price[area2][row,])}
    if(area3 != "NONE" & !any(area == area3)){area <- append(area, area3)
       areav <- append(areav,house_price[area3][row,])}
    if(area4 != "NONE" & !any(area == area4)){area <- append(area, area4)
       areav <- append(areav,house_price[area4][row,])}
    if(area5 != "NONE" & !any(area == area5)){area <- append(area, area5)
       areav <- append(areav,house_price[area5][row,])}
    if(area6 != "NONE" & !any(area == area6)){area <- append(area, area6)
       areav <- append(areav,house_price[area6][row,])}

    data.frame(
      Area =area,Price = areav,stringsAsFactors = FALSE)

  })
  
  
  output$values <- renderTable({
    TableValues() },width = "2px",spacing=c("xs"))
  
  

  output$prediction <-   function(){
    
    parea1 <- input$parea1
    house_price_p <- house_price[c("Date",parea1)]

    # time-series object
    ts_houseprice1 <- ts(house_price[,parea1],start=c(2014,1),freq=12)
    
    #ETS model. "h" means the number of months
    m_pre1 <-ets(ts_houseprice1)
    f_pre1 <-forecast(m_pre1,h=60)
    
    #ARIMA model
    m_pre2 <-auto.arima(ts_houseprice1)
    f_pre2 <-forecast(m_pre2,h=60)
    
    #TBAT model
    m_pre3 <-tbats(ts_houseprice1)
    f_pre3 <-forecast(m_pre3,h=60)
    
    
    # transforming ts object to data frame
    last_date = index(ts_houseprice1)[length(ts_houseprice1)]
    date =last_date + seq(1/12, 5, by=1/12)
    
    year = floor(date)
    month = round(((date %% 1) * 12) + 1)
    month <- formatC(month,width=2,flag="0")
    date1 <- paste(year,"-",month,"-01",sep="")
    
    forecast1 <- 
      data.frame(Date = date1 , price_predicted1=f_pre1$mean)
    names(forecast1)[2] <- parea1
    
    # merge the past price data into the forecast data
    forecast1 <- rbind(house_price_p,forecast1)
    
    
    forecast2 <- 
      data.frame(Date = date1 , price_predicted2=f_pre2$mean)
    names(forecast2)[2] <- parea1
    forecast2 <- rbind(house_price_p,forecast2)
    
    
    forecast3 <- 
      data.frame(Date = date1 , price_predicted3=f_pre3$mean)
    names(forecast3)[2] <- parea1
    forecast3 <- rbind(house_price_p,forecast3)
    
    # List (key = "ETS" ,(( x = , y= ),( x = , y= ),,,)) 
    list1 <- mapply(function(key) {
      
      values <- mapply(function(i, val) {
        list(x = i, y = val)
      }, forecast1$Date, forecast1[,2], SIMPLIFY=FALSE, USE.NAMES=FALSE)
      
      list(key = key, values = values)
    }, "Model1:ETS" , SIMPLIFY=FALSE, USE.NAMES=FALSE)


    # List (key = "ARIMA" ,(( x = , y= ),( x = , y= ),,,)) 
    list2 <- mapply(function(key) {
      
      values <- mapply(function(i, val) {
        list(x = i, y = val)
      }, forecast2$Date, forecast2[,2], SIMPLIFY=FALSE, USE.NAMES=FALSE)
      
      list(key = key, values = values)
    }, "Model2:ARIMA" , SIMPLIFY=FALSE, USE.NAMES=FALSE)

    # List (key = "TBATS" ,(( x = , y= ),( x = , y= ),,,)) 
    list3 <- mapply(function(key) {
      
      values <- mapply(function(i, val) {
        list(x = i, y = val)
      }, forecast3$Date, forecast3[,2], SIMPLIFY=FALSE, USE.NAMES=FALSE)
      
      list(key = key, values = values)
    }, "Model3:TBATS" , SIMPLIFY=FALSE, USE.NAMES=FALSE)
    
    list <- c(list1,list2,list3)
    
  }
  
  TableValues2 <- reactive({
    parea1 <- input$parea1
    ts_houseprice1 <- ts(house_price[,parea1],start=c(2014,1),freq=12)
    
    m_pre1_t <-ets(ts_houseprice1)
    m_pre2_t <-auto.arima(ts_houseprice1)
    m_pre3_t <-tbats(ts_houseprice1)

    data.frame(
      MODEL =c("ETS","ARIMA","TBATS"),
      AIC = c(m_pre1_t$aic,m_pre2_t$aic,m_pre3_t$AIC),
      stringsAsFactors = FALSE)
  })
  
  
  output$values2 <- renderTable({
    TableValues2() })

}

shinyApp(ui = ui, server = server)

