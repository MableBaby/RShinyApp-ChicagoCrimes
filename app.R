library(shiny)
library(shinythemes)
library(leaflet)
library(sqldf)
require(maps)
require(ggmap)
library(raster)
library(sp)
library(maptools)
gpclibPermit()
library(mapdata)
library(geosphere)
library(maps)
library(ggplot2)
library(tibble)
library(dplyr)
library(grid)
library(gridExtra)
library(caret)

ui<-fluidPage(theme = shinytheme("darkly"),
              headerPanel("Crimes in Chicago"),

              sidebarPanel(
                
                img(src = "cpd.png", align = "center"),
                selectInput(inputId = "District",label = "Select a District" , choices = c( 'Central', 'Wentworth',  'Grand Crossing',  'South Chicago', 'Calumet' ,  'Englewood', 'Chicago Lawn', 'Deering' , 'Ogden',  'Harrison' , 'Near West' ,'Shakespeare' , 'Austin','Jefferson Park'  ,'Albany Park'  ,'Near North' , 'Town Hall',  'Lincoln','Morgan Park', 'Rogers Park','Grand Central' ) ,selected = "1", multiple = FALSE, selectize = TRUE),
                selectInput(inputId = "timeSlot",label = "Select a time slot" , choices = c("04:00 to 12:00","12:00 to 20:00","20:00 to 04:00") ,selected = "1", multiple = FALSE, selectize = TRUE),
                selectInput(inputId = "WeekDay",label = "Select a Day" , choices = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday") ,selected = "1", multiple = FALSE, selectize = TRUE),
                selectInput(inputId = "Month",label = "Select a Month" , choices = c("January","February","March","April","May","June","July", "August", "September", "October", "November", "December") ,selected = "1", multiple = FALSE, selectize = TRUE),
                submitButton("Apply Changes", icon("refresh"))
                
                ),
              mainPanel(
                tabsetPanel(
                  tabPanel("Spread of Crimes", 
                           leafletOutput("plot3",width="100%",height="550px")),
                  tabPanel("Concentration of Crimes",
                           plotOutput("plot2"), width = "100%"),
                  tabPanel("Prediction of Crimes",
                          htmlOutput("futureData"))
                )
              )
)

#reading data from csv file
crimes <- read.csv("CrimeMapFile.csv")
crimeCoefficients <- read.csv("CrimeCoefficients.csv")

server <- function(input, output, session){
  
  output$plot3<-renderLeaflet({
    
    chicagomap <- leaflet(height="100%") %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      setView(-87.6298, 41.8781, zoom = 10) %>%
      addMarkers(lat=41.8781, lng=-87.6298, popup="Chicago")
    data<- subset(crimes, (crimes$WeekDay == input$WeekDay & crimes$District == input$District) | (crimes$Month == input$Month & crimes$timeSlot == input$timeSlot) )  
    crimePlot1 <- chicagomap %>% 
      addTiles() %>% 
      setView(-87.6298, 41.8781, zoom = 10) %>% 
      addCircleMarkers(data = data, lng = ~ Longitude, lat = ~ Latitude, radius = 5, 
                       color = ~ ifelse(Classification == 'DEADLY', 'red', ifelse(Classification == 'AGGRESSIVE','orange', 
                                                                                  ifelse(Classification == 'PROPERTY', 'yellow',
                                                                                         ifelse(Classification == 'NONVIOLENT', 'green', 'blue')))),
                       clusterOptions = markerClusterOptions())
    
    print(crimePlot1)
    
  })
  
  output$plot2<-renderPlot({

    histCrimes<- filter(crimes, crimes$WeekDay == input$WeekDay & crimes$District == input$District | crimes$Month == input$Month & crimes$timeSlot == input$timeSlot)
    crimePlot <- ggplot(as.data.frame(histCrimes)  , aes(x=histCrimes$Classification, fill = histCrimes$Classification)) +
      geom_bar(colour = 'black', width = 0.5, fill = "#FF6666") + theme(axis.text.x.top = element_text(size = 15, hjust = 1)) + 
      ggtitle("Crime Frequency") + 
      labs(x = "CLASSIFICATION") + facet_grid(~input$WeekDay)
    
    print(crimePlot)
  },height = 600, width = 700)
  
  output$futureData<-renderUI({
    
    predVector <- vector(length = nrow(crimeCoefficients))
    predVector <- rep(0,length(predVector))
    names(predVector) <- crimeCoefficients[ , 1]
    predVector[1:2] <- c(1,6)
    Name1 <- ifelse(input$District == "Albany Park", "(Intercept)", paste0("DistrictName",input$District))
    predVector[Name1] <- 1
    Name2 <- ifelse(input$timeSlot == "04:00 to 12:00","(Intercept)", paste0("timeSlot",input$timeSlot))
    predVector[Name2] <- 1
    season <- ifelse(input$Month == "January"| input$Month == "November" | input$Month == "December", "Cold",
                     ifelse(input$Month == "March" | input$Month == "April" | input$Month == "September" | input$Month == "October", "Temperate",
                            ifelse(input$Month == "Feb","February",
                                   ifelse( input$Month == "May" | input$Month == "June" | input$Month == "July" | input$Month == "August", "Hot", "Broken"))))
    Name3 <- ifelse(season == "Cold", "(Intercept)", paste0("tempCategory",season))
    predVector[Name3] <- 1
    dayCat <-  ifelse(input$WeekDay == "Sunday" | input$WeekDay== "Monday" | input$WeekDay == "Tuesday" | input$WeekDay == "Wednesday" | input$WeekDay == "Thursday", "Week",
                      ifelse(input$WeekDay == "Friday" | input$WeekDay == "Saturday", "Weekend", "Broken"))
    Name4 <- ifelse(dayCat == "week", "(Intercept)", paste0("dayCategory",dayCat))
    predVector[Name4] <- 1
    predVector
    predicted <- apply(X = (crimeCoefficients[2:5]*predVector),MARGIN = 2, FUN = sum)
    dayDivider <- ifelse(dayCat == "week",5,2)
    seasonDivider <- ifelse(season == "Cold",3,
                                   ifelse(season == "Temperate",4,
                                          ifelse(season == "Hot",4,1)))
    predNon <- round(max(predicted[1]/dayDivider/seasonDivider/4,0),digits = 0)
    predDead <- round(max(predicted[2]/dayDivider/seasonDivider/4,0), digits = 0)
    predProp <- round(max(predicted[3]/dayDivider/seasonDivider/4,0), digits = 0)
    predAgg <- round(max(predicted[4]/dayDivider/seasonDivider/4,0), digits = 0)
    
    str1<- paste("You have selected ")
    Week<-paste(" WeekDay: ",input$WeekDay )
    Month<- paste(" Month:", input$Month)
    Time<- paste (" Time slot : " ,input$timeSlot )
    District<-paste(" District :" , input$District)
    str2<-paste("The predictions for crimes are: ") 
    str3<-paste("Deadly : ", predDead) 
    str4<-paste(" Aggressive : ", predAgg)
    str5<-paste(" Property: ", predProp)
    str6<-paste(" Non Violent : ", predNon)
    str7<- ifelse(predDead>0, "Highly Dangerous area . Avoid travelling !!" , 
                  ifelse(predAgg >=25, " Dangerous to travel. Proceed with caution!!", 
                        ifelse(predProp >20, "Safe for travel but take care of your belongings!!", 
                               ifelse(predNon < 5 |predProp <=20 | predAgg <25, "Safe zone !!", "No information available")) ))
    
    str8<- ifelse(predDead>0, "Minion1.gif" , 
                  ifelse(predAgg >25, "Minion2.gif", 
                         ifelse(predProp >20, "Minion3.gif", 
                                ifelse(predNon < 5 |predProp <=20 | predAgg <=25, "Minion4.gif", "Minion4.gif")) ))
    
    tags$div(
      tags$p(str1),
      tags$ol(
        tags$li(Week), 
        tags$li(Month), 
        tags$li(Time),
        tags$li(District)
      ),
      tags$br(),
      tags$p(str2),
      tags$ol(
        tags$li(style="color:blue",str3), 
        tags$li(style="color:blue",str4), 
        tags$li(style="color:blue",str5),
        tags$li(style="color:blue",str6)
      ),
      tags$br(),
      tags$h3(style="color:red", str7)
      ,tags$img(src = str8, width = "250px", height = "200px")
    )
    
  })
}

shinyApp(ui=ui, server=server)