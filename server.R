library(shiny)
library(shinydashboard)
library(DT)
library(mapproj)
library(readr)
library(tidyverse)

shinyServer(function(input, output, session) {
  mygitpath <- 'https://raw.githubusercontent.com/oroberts19/COVID-19-Dashboard/master/cases.csv'
  df <- reactiveFileReader(intervalMillis = 10000, session = session, filePath = mygitpath, readFunc = read_csv)
  output$mydata <-DT::renderDataTable({
    df<- df()
    df<- df[,c(1,2)]
    df<- df %>% filter(is.na(County) == FALSE) %>% arrange(desc(Cases)) %>% datatable(options = list(pageLength = 20)) %>% formatRound(columns = 'Cases', digits = 0)
    })
  output$myplot <- renderPlot({
    df <- df()
    df<- df %>% filter(is.na(County) == FALSE)
    df<- df[,c(1,2)]
    
    for (i in 1:nrow(df)){
      df[i,1]<- tolower(df[i,1])
    }
    counties<- df
    counties<- counties %>% filter(County != "unknown")
    counties[counties$County == "dade",][1]<- "miami-dade"
    counties[counties$County == "st. johns",][1]<- "st johns"
    counties[counties$County == "st. lucie",][1]<- "st lucie"
    florida_data <- map_data("county") %>% filter(region == "florida")
    
    map_data<- florida_data %>% left_join(counties, by = c("subregion" = "County"))
    map_data$Cases[is.na(map_data$Cases)]<- 0
    
    p<- ggplot(map_data, aes(x = long, y = lat, group = group, fill = Cases)) + 
      geom_polygon(color = "black", size = 0.5) + scale_fill_gradient(low = "lightsalmon", high = "midnightblue") + 
      labs(fill = "Cases", x = "Longitude", y = "Latitude") + 
      coord_map(projection = "albers", lat0 = 25, lat1 = 31) + theme_minimal()
    return(p)
  })
})
