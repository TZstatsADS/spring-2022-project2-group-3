
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
                sidebarLayout(
                  sidebarPanel(
                    radioButtons("app_or_renew", 
                                 label=h3("Type of Application"),
                                 choices=list("All"=1, "New Application"=2,
                                                 "Renewal"=3)
                                ),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    radioButtons("category", label=h3("Business Category"),
                                 choices=list("Home Improvement Contractor",
                                              "Tobacco Retail Dealer",
                                              "Secondhand Dealer - General",
                                              "Electronic Cigarette Dealer",
                                              "Laundries",
                                              "Electronics Store",
                                              "Stoop Line Stand",
                                              "Garage",
                                              "Electronic & Appliance Service",
                                              "Dealer In Products",
                                              "Pawnbroker",
                                              "Secondhand Dealer - Auto")),
                    width=2
                  ),
                mainPanel(
                  h3("All Businesses", align="center"),
                  plotOutput("plot_proc_time_1"),
                  hr(),
                  h3("Business-type Specific", align="center"),
                  plotOutput("plot_proc_time_2")
                ))
              )
      ),
      
      #---------pass_rate----------
      tabItem(tabName="pass_rate",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    radioButtons("app_or_renew_1", 
                                 label=h3("Type of Application"),
                                 choices=list("All"=1, "New Application"=2,
                                              "Renewal"=3),
                                 selected=1),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    radioButtons("category_1", label=h3("Business Category"),
                                 choices=list("Home Improvement Contractor",
                                              "Tobacco Retail Dealer",
                                              "Secondhand Dealer - General",
                                              "Electronic Cigarette Dealer",
                                              "Laundries",
                                              "Electronics Store",
                                              "Stoop Line Stand",
                                              "Garage",
                                              "Electronic & Appliance Service",
                                              "Dealer In Products",
                                              "Pawnbroker",
                                              "Secondhand Dealer - Auto"),
                                 selected="Home Improvement Contractor"),
                    width=2
                  ),
                  mainPanel(
                    h3("All Businesses", align="center"),
                    plotOutput("plot_pass_rate_1"),
                    hr(),
                    h3("All Businesses", align="center"),
                    plotOutput("plot_pass_rate_2")
                  ))
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
