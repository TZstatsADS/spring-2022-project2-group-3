
if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}
if (!require("shinyWidgets")) {
  install.packages("shinyWidgets")
  library(shinyWidgets)
}
if (!require("shinydashboard")) {
  install.packages("shinydashboard")
  library(shinydashboard)
}
if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}
if (!require("leaflet.extras")) {
  install.packages("leaflet.extras")
  library(leaflet.extras)
}

shinyUI(dashboardPage(
  dashboardHeader(title="COVID-19's Impact on NYC Businesses", 
                  titleWidth=400),
  
  # SideBar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName="home", icon=icon("home")),
      menuItem("MAP", tabName="map", icon=icon("map")),
      menuItem("Interactive Plots", tabName="plots", icon=icon("signal"),
               startExpanded=TRUE,
               menuSubItem("New application", tabName="new_app"),
               menuSubItem("Processing Time", tabName="proc_time"),
               menuSubItem("Pass Rate", tabName="pass_rate")
               #ADD CONTENT XXXXXXXXXXXXXX
               ),
      menuItem("Findings", tabName="findings", icon=icon("star"))
      
    )
  ),
  
  # Body content
  dashboardBody(
    tabItems(
      #-----------home------------
      tabItem(tabName="home",
              fluidPage(
                h2(strong("Title XXXXXXXXX"), align="center"),
                h3("Team Members XXXXXXXXXXX", align="center")
                #ADD CONTENT XXXXXXXXXXXXXX
              )
      ),
      
      #-----------map------------
      tabItem(tabName="map",
              fluidPage(
                h3("XXXXXXXXXXX", align="center")
                #ADD CONTENT XXXXXXXXXXXXXX
              )
      ),
      
      #---------new_app----------
      tabItem(tabName="new_app",
              fluidPage(
                h3("XXXXXXXXXXX", align="center")
                #ADD CONTENT XXXXXXXXXXXXXX
              )
      ),
      
      #---------proc_time----------
      tabItem(tabName="proc_time",
              fluidPage(
                h3("XXXXXXXXXXX", align="center")
                #ADD CONTENT XXXXXXXXXXXXXX
              )
      ),
      
      #---------proc_time----------
      tabItem(tabName="pass_rate",
              fluidPage(
                h3("XXXXXXXXXXX", align="center")
                #ADD CONTENT XXXXXXXXXXXXXX
              )
      ),
      
      #---------findings----------
      tabItem(tabName="findings",
              fluidPage(
                h3("XXXXXXXXXXX", align="center")
                #ADD CONTENT XXXXXXXXXXXXXX
              )
      )
    )
  )
  
  )
)
