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
                        fluidRow(
                          splitLayout(cellWidths = c("50%", "50%"), 
                                      leafletOutput("left_map",width="100%",height=1200),
                                      leafletOutput("right_map",width="100%",height=1200))),
                        
                        absolutePanel(id = "control", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
                                      top = 200, left = 50, right = "auto", bottom = "auto", width = 250, height = "auto",
                                      tags$h4('License Application Comparison'), 
                                      tags$br(),
                                      prettyRadioButtons(
                                        inputId = "adjust_status",
                                        label = "Status:", 
                                        choices = c("Issued", 
                                                    "Denied",
                                                    "All"),
                                        inline = TRUE, 
                                        status = "danger",
                                        fill = TRUE
                                      ),
                                      prettyRadioButtons(
                                        inputId = "adjust_type",
                                        label = "Type:", 
                                        choices = c("Renewal", 
                                                    "Application",
                                                    "All"),
                                        inline = TRUE, 
                                        status = "danger",
                                        fill = TRUE
                                      ),
                                      # selectInput('adjust_weather',
                                      #             label = 'Adjust for Weather',
                                      #             choices = c('Yes','No'), 
                                      #             selected = 'Yes'
                                      #             ),
                                      style = "opacity: 0.80"
                                      
                        ), #Panel Control - Closing
                      ) #Maps - Div closing
                  ) #tabPanel maps closing
                
   


    ) #navbarPage closing  
) #Shiny UI closing    
