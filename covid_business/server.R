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

licence_df = read.csv('../data/License_Applications.csv')
licence_df = licence_df[1: 100, ]

shinyServer(function(input, output) {

    output$map <- renderLeaflet({

        map <- licence_df %>%
            leaflet(options = leafletOptions(minZoom = 11, maxZoom = 13)) %>%
            addTiles() %>%
            addProviderTiles("CartoDB.Positron",
                            options = providerTileOptions(noWrap = TRUE)) %>%
            setView(-73.9834,40.7504,zoom = 12)

        map %>%
            addMarkers(
                lng=~Longitude,
                lat=~Latitude
            )

    })
})