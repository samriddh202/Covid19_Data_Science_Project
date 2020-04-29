library(shiny)
library(leaflet)
library(tidyverse)
library(lubridate)
library(shinythemes)

## Getting the data
India_virus<-read.csv("complete.csv")
Italy_virus<-read.csv("covid19_italy_region.csv")

India_virus<-India_virus %>% 
    select(Date,Name.of.State...UT,Latitude,Longitude,
           Cured.Discharged.Migrated,Death,Total.Confirmed.cases) %>% 
    pivot_longer(-c(Date,Name.of.State...UT,Longitude,Latitude),
                 names_to = "Status",values_to = "Value") %>%
    mutate(Date=as.Date(Date,"%m/%d/%Y"))

charts1<-India_virus


Italy_virus<-Italy_virus %>% 
    mutate(Date=as.Date(as.POSIXct(Date, origin="1970-01-01"))) %>%
    select(Date,RegionName,Latitude,Longitude,
           Recovered,Deaths,TotalPositiveCases) %>% 
    pivot_longer(-c(Date,RegionName,Longitude,Latitude),names_to = "Status",values_to = "Value")
charts2<-Italy_virus


dat <-read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series/time_series_19-covid-Confirmed_archived_0325.csv")

new_dat<-dat %>%
    pivot_longer(-c(`Province/State`,`Country/Region`,Lat,Long),names_to = "date",values_to = "Confirmed_Cases") %>% 
    select(`Country/Region`,Lat,Long,date,Confirmed_Cases) %>% 
    mutate(date=as.Date(date,"%m/%d/%y")) %>% 
    drop_na()



max_india<-max(as.Date(India_virus$Date,"%m/%d/%Y"))
min_india<-min(as.Date(India_virus$Date,"%m/%d/%Y"))

max_Italy<-max(as.Date(Italy_virus$Date,"%Y-%m-%d"))
min_Italy<-min(as.Date(Italy_virus$Date,"%Y-%m-%d"))

max_World<-max(as.Date(new_dat$date,"%m/%d/%y"))
min_World<-min(as.Date(new_dat$date,"%m/%d/%y"))


## pannel layout
ui <- fluidPage(
    tags$head(tags$style(
        HTML('
         body, label, input, button, select { 
         font-family: "Helvetica Neue", Helvetica;
         font-weight: 200;
         }
         
         #Total_cases{
         color: #9932CC;
         font-size: 24px;
         }
         
         #Total_cases1{
         color: #9932CC;
         font-size: 24px;
         }
         
         #Total_recoverd{
         color: #9932CC;
         font-size: 24px;
         }
         
         #Total_recoverd1{
         color: #9932CC;
         font-size: 24px;
         }
         
         #Total_Deaths{
         color: #9932CC;
         font-size: 24px;
         }
         
         #Total_Deaths1{
         color: #9932CC;
         font-size: 24px;
         }
         
         #sidebar {
            background-color: white;
            padding: 0 10px 10px 10px;
            cursor: move;
            opacity: 0.5;
            
         }
         #sidebar:hover {
         opacity: 0.75;
         transition-delay: 0;
         }
             
         #sidebar2 {
            background-color: white;
            padding: 0 10px 10px 10px;
            cursor: move;
            opacity: 0.5;
            
         }
         #sidebar2:hover {
         opacity: 0.75;
         transition-delay: 0;
         }
             
        #sidebar3 {
            background-color: white;
            padding: 0 10px 10px 10px;
            cursor: move;
            opacity: 0.5;
            
         }
         #sidebar3:hover {
         opacity: 0.75;
         transition-delay: 0;
         }
             
             ')
        
        
    )),
    ## navbar pannel
    navbarPage(theme = shinytheme("flatly"),
               "COVID-19 mapper",
               tabPanel("Map Of India ", 
                        leafletOutput("mymap1", height=1000),
                        absolutePanel(id="sidebar",top = 100, left = 40, width = 250, fixed=TRUE,
                                      draggable = TRUE, height = "auto",
                                      h6(textOutput("Total_cases"), align = "right"),
                                      h6(textOutput("Total_recoverd"), align = "right"),
                                      h6(textOutput("Total_Deaths"), align = "right"),
                                      tags$i(h6("Stats for India as of 18th April 2020")),
                                      tags$i(h6("The Data shown Visualize here are from ",
                                                tags$a(href="https://www.kaggle.com/imdevskp/covid19-corona-virus-india-dataset#complete.csv", "State/UT/NCR wise COVID-19 data."))),
                                      selectInput("Status", "Status", 
                                                  choices=c("Confirmed Cases"="Total.Confirmed.cases",
                                                  "Recovered"="Cured.Discharged.Migrated",
                                                  "Death"="Death")),
                                      sliderInput("India_date", "Select the date",
                                                  min = as.Date(min_india,"%m/%d/%Y"),
                                                  max = as.Date(max_india,"%m/%d/%Y"),
                                                  value = as.Date(max_india),
                                                  timeFormat = "%d %b", 
                                                  animate=animationOptions(interval = 1000,loop = FALSE)),
                                      plotOutput("plot1", height="130px", width="100%"))),
               
               
               tabPanel("Map Of Italy", 
                        leafletOutput("mymap2", height=1000),
                        absolutePanel(id="sidebar2",top = 100, left = 40, width = 250, fixed=TRUE,
                                      draggable = TRUE, height = "auto",
                                      h6(textOutput("Total_cases1"), align = "right"),
                                      h6(textOutput("Total_recoverd1"), align = "right"),
                                      h6(textOutput("Total_Deaths1"), align = "right"),
                                      tags$i(h6("Stats for Italy as of 2nd April 2020")),
                                      tags$i(h6("The Data shown Visualize here are from ",
                                                tags$a(href="https://www.kaggle.com/sudalairajkumar/covid19-in-italy", "COVID-19 in Italy."))),
                                      selectInput("Status2", "Status", 
                                                  choices=c("Confirmed Cases"="TotalPositiveCases",
                                                            "Recovered"="Recovered",
                                                            "Death"="Deaths")),
                                      sliderInput("Italy_date", "Select the date",
                                                  min = as.Date(min_Italy,"%m/%d/%Y"),
                                                  max = as.Date(max_Italy,"%m/%d/%Y"),
                                                  value = as.Date(max_Italy),
                                                  timeFormat = "%d %b", 
                                                  animate=animationOptions(interval = 1000,loop = FALSE)),
                                      plotOutput("plot2", height="130px", width="100%"))),
               
               
               
               tabPanel("Map Of World", 
                        leafletOutput("mymap3", height=1000),
                        absolutePanel(id="sidebar3",top = 100, left = 40, width = 250, fixed=TRUE,
                                      draggable = TRUE, height = "auto",
                                      tags$i(h4("Total Active Cases",align = "right")),
                                      tags$i(h6("Stats for World as of 23th March 2020")),
                                      tags$i(h6("The Data shown Visualize here are from ",
                                                tags$a(href="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series/time_series_19-covid-Confirmed_archived_0325.csv",
                                                       "Johns Hopkins CSSE."))),
                                      sliderInput("World_date", "Select the date",
                                                  min = as.Date(min_World,"%m/%d/%y"),
                                                  max = as.Date(max_World,"%m/%d/%y"),
                                                  value = as.Date(max_World),
                                                  timeFormat = "%d %b", 
                                                  animate=animationOptions(interval = 1000,loop = FALSE)),
                                      plotOutput("plot3", height="130px", width="100%"))))
                                      
    
    
)    

server <- function(input, output, session) {
     
   
    output$Total_cases <- renderText({
        rand<-India_virus %>% 
            filter(Date==max_india,
                   Status=="Total.Confirmed.cases")
        paste0(prettyNum(sum(rand$Value), big.mark=","), " Total cases")
    })
    
    output$Total_recoverd <- renderText({
        rand<-India_virus %>% 
            filter(Date==max_india,
                   Status=="Cured.Discharged.Migrated")
        paste0(prettyNum(sum(rand$Value), big.mark=","), " Recovered ")
    })
    
    output$Total_Deaths <- renderText({
        rand<-India_virus %>% 
            filter(Date==max_india,
                   Status=="Death")
        paste0(prettyNum(sum(rand$Value), big.mark=","), " Deaths")
    })
    
    
    
    
    output$Total_cases1 <- renderText({
        rand<-Italy_virus %>% 
            filter(Date==max_Italy,
                   Status=="TotalPositiveCases")
        paste0(prettyNum(sum(rand$Value), big.mark=","), " Total cases")
    })
    
    output$Total_recoverd1 <- renderText({
        rand<-Italy_virus %>% 
            filter(Date==max_Italy,
                   Status=="Recovered")
        paste0(prettyNum(sum(rand$Value), big.mark=","), " Recovered")
    })
    
    output$Total_Deaths1 <- renderText({
        rand<-Italy_virus %>% 
            filter(Date==max_Italy,
                   Status=="Deaths")
        paste0(prettyNum(sum(rand$Value), big.mark=","), " Deaths")
    })
    
    
    

    edited_by_date<-reactive({
       India_virus %>% filter(Status == input$Status,
                              Date == as.Date(input$India_date,"%m/%d/%Y"))
   })
    
    edited_by_date2<-reactive({
       Italy_virus %>% filter(Status == input$Status2,
                              Date == as.Date(input$Italy_date,"%m/%d/%Y"))
   })
    
   output$plot1<-renderPlot({
       
       charts1 %>% 
           filter(Status==input$Status,
                  Date <= as.Date(input$India_date,"%m/%d/%Y")) %>% 
           group_by(Date) %>% 
           summarise(Total=sum(Value)) %>% 
           ggplot(aes(x=Date,y=Total,color="#9932CC"))+
           geom_line()+
           theme_bw()+
           xlab("Date")+
           ylab("Number of Patients")+
           theme(legend.position = "none")
   })
   
   

   output$plot2<-renderPlot({
       
       charts2 %>% 
           filter(Status==input$Status2,
                  Date <= as.Date(input$Italy_date,"%m/%d/%Y")) %>% 
           group_by(Date) %>% 
           summarise(Total=sum(Value)) %>% 
           ggplot(aes(x=Date,y=Total,color="#9932CC"))+
           geom_line()+
           theme_bw()+
           xlab("Date")+
           ylab("Number of Patients")+
           theme(legend.position = "none")
   })
   
       
   output$mymap1 <- renderLeaflet({
        leaflet() %>%
            addTiles(group = "OSM (default)") %>%
            addProviderTiles("Stamen.Toner", group = "Toner") %>%
            addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
            addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
            setView(lng = 80,lat = 20,zoom = 5) %>% 
            addLayersControl(baseGroups = c("OSM (default)",
                                            "Toner",
                                            "Toner Lite",
                                            "CartoDB"),
                             options = layersControlOptions(collapsed = FALSE)) %>% 
            addCircleMarkers(data = edited_by_date(),
                       radius = ~ifelse(Status== "Total.Confirmed.cases", 
                                        sqrt(Value), 
                                        ifelse(Status== "Cured.Discharged.Migrated",
                                               sqrt(Value),Value/3)),
                       color = ~ifelse(Status== "Total.Confirmed.cases","#FF0000",
                                       ifelse(Status== "Cured.Discharged.Migrated",
                                              "#09FF00","#FF4D00")),
                       stroke = FALSE, fillOpacity = 0.5)
    })
    
    
   
   
   output$mymap2 <- renderLeaflet({
        leaflet() %>%
            addTiles(group = "OSM (default)") %>%
            addProviderTiles("Stamen.Toner", group = "Toner") %>%
            addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
            addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
            addLayersControl(baseGroups = c("OSM (default)",
                                            "Toner",
                                            "Toner Lite",
                                            "CartoDB"),
                             options = layersControlOptions(collapsed = FALSE)) %>%
            setView(lng = 12.646361,lat = 42.504154,zoom = 6) %>%
            addCircleMarkers(data = edited_by_date2(),
                             radius = ~ifelse(Status== "TotalPositiveCases", sqrt(Value)/2, sqrt(Value)),
                             color = ~ifelse(Status== "TotalPositiveCases","#FF0000",
                                             ifelse(Status== "Recovered",
                                                    "#09FF00","#FF4D00")),
                             stroke = FALSE, fillOpacity = 0.5)
    })
   
   
   edited_by_World<-reactive({
       new_dat %>% filter(date == as.Date(input$World_date,"%m/%d/%y"))
   })
   
   
   output$plot3<-renderPlot({
       
       new_dat %>% 
           filter(date <= as.Date(input$World_date,"%m/%d/%y")) %>% 
           group_by(date) %>% 
           summarise(Total=sum(Confirmed_Cases)) %>% 
           ggplot(aes(x=date,y=Total,color="#9932CC"))+
           geom_line()+
           theme_bw()+
           xlab("Date")+
           ylab("Number of Patients")+
           theme(legend.position = "none")
   })
   
   
   output$mymap3 <- renderLeaflet({
       leaflet() %>%
           addTiles(group = "OSM (default)") %>%
           addProviderTiles("Stamen.Toner", group = "Toner") %>%
           addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
           addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
           addLayersControl(baseGroups = c("OSM (default)",
                                           "Toner",
                                           "Toner Lite",
                                           "CartoDB"),
                            options = layersControlOptions(collapsed = FALSE)) %>%
           addCircleMarkers(data = edited_by_World(),
                            radius = ~sqrt(Confirmed_Cases)/10,
                            color = ~ifelse(Confirmed_Cases>100000,
                                            "#CC0066",
                                            ifelse(Confirmed_Cases>50000 & Confirmed_Cases<100000,
                                                   "#FF007F",
                                                   ifelse(Confirmed_Cases<50000 & Confirmed_Cases>25000,
                                                          "#FF3399",
                                                          ifelse(Confirmed_Cases<25000 & Confirmed_Cases>10000,
                                                                 "#FF66B2",
                                                                 ifelse(Confirmed_Cases<10000 & Confirmed_Cases>1000,
                                                                        "#FF99CC","#FFCCE5"))))))
   })
}

shinyApp(ui, server)