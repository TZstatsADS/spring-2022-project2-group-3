if (!require("shiny")) {
    install.packages("shiny")
    library(shiny)
}
if (!require("leaflet")) {
    install.packages("leaflet")
    library(leaflet)
}
if (!require("leaflet.extras")) {
    install.packages("leaflet.extras")
    library(leaflet.extras)
}
if (!require("dplyr")) {
    install.packages("dplyr")
    library(dplyr)
}
if (!require("magrittr")) {
    install.packages("magrittr")
    library(magrittr)
}
if (!require("mapview")) {
    install.packages("mapview")
    library(mapview)
}
if (!require("leafsync")) {
    install.packages("leafsync")
    library(leafsync)
}

if (!require("mapdeck")) {
    install.packages("mapdeck")
    library(mapdeck)
}

if (!require("tidyverse")) {
    install.packages("tidyverse")
    library(tidyverse)
}

if (!require("htmltools")) {
    install.packages("htmltools")
    library(htmltools)
}

if (!require("leaflet.extras")) {
    install.packages("leaflet.extras")
    library(leaflet.extras)
}

heatPlugin <- htmlDependency("Leaflet.heat", "99.99.99",
             src = c(href = "http://leaflet.github.io/Leaflet.heat/dist/"),
             script = "leaflet-heat.js")

registerPlugin <- function(map, plugin) {
    map$dependencies <- c(map$dependencies, list(plugin))
    map}



data_raw = read.csv('../data/License_Applications.csv')
df = data_raw %>%
    select(ID="Application.ID", l_type="License.Type", 
           app_or_renew="Application.or.Renewal", status="Status",
           s_date="Start.Date", e_date="End.Date",
           category="License.Category", app_cat="Application.Category",
           city="City", state="State", zip="Zip", longitude="Longitude", 
           latitude="Latitude") %>%
    drop_na() %>%
    mutate(s_date = 
               as.Date(s_date, format = "%m/%d/%Y")) %>%
    mutate(e_date = 
               as.Date(e_date, format = "%m/%d/%Y")) %>%
    filter(state=="NY") %>% 
    filter(s_date >= as.Date("01/01/2017",format = "%m/%d/%Y")) %>%
    filter(s_date < e_date)

shinyServer(function(input, output) {
    output$left_map <- renderLeaflet(
        {
            left_df = df %>%
                filter(s_date <= as.Date("05/31/2019",format = "%m/%d/%Y"))
            if(input$adjust_status != "All"){
                left_df = left_df %>%
                    filter(status == input$adjust_status)
            }
            if(input$adjust_type != "All"){
                left_df = left_df %>%
                    filter(app_or_renew == input$adjust_type)
            }
            
        
            left_df %>%
                leaflet(options = leafletOptions(minZoom = 11, maxZoom = 13)) %>%
                addTiles() %>%
                addProviderTiles(providers$CartoDB) %>%
                setView(-73.9834,40.7504,zoom = 12) %>%
                addWebGLHeatmap(~longitude, ~latitude, size = 200, alphaRange = 0.5, intensity = 0.1)
    })
    
    output$right_map <- renderLeaflet(
        {
            right_df = df %>%
                filter(s_date >= as.Date("04/30/2020",format = "%m/%d/%Y"))
            if(input$adjust_status != "All"){
                right_df = right_df %>%
                    filter(status == input$adjust_status)
            }
            
            if(input$adjust_type != "All"){
                right_df = right_df %>%
                    filter(app_or_renew == input$adjust_type)
            }
            
            
            
            right_df %>%
                leaflet(options = leafletOptions(minZoom = 11, maxZoom = 13)) %>%
                addTiles() %>%
                addProviderTiles(providers$CartoDB) %>%
                setView(-73.9834,40.7504,zoom = 12) %>%
                addWebGLHeatmap(~longitude, ~latitude, size = 200, alphaRange = 0.5, intensity = 0.1)
        })
})