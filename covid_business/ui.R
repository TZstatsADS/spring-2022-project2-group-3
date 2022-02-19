if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}
if (!require("shinyWidgets")) {
  install.packages("shinyWidgets")
  library(shinyWidgets)
}
if (!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)
}
if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}
if (!require("leaflet.extras")) {
  install.packages("leaflet.extras")
  library(leaflet.extras)
}
shinyUI(
     navbarPage(strong("License Application Study",style="color: white;"), 
               theme=shinytheme("cerulean"), # select your themes https://rstudio.github.io/shinythemes/
#------------------------------- tab panel - Maps ---------------------------------
                tabPanel("Maps",
                         icon = icon("map-marker-alt"), #choose the icon for
                         div(class = 'outer',
                        # side by side plots
                        leafletOutput("map",width="100%",height=1200)
                            ) #Maps - Div closing
                        ) #tabPanel maps closing
   


    ) #navbarPage closing  
) #Shiny UI closing    
