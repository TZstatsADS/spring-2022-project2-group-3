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

if (!require("shinydashboard")) {
  install.packages("shinydashboard")
  library(shinydashboard)
}

if (!require("plotly")) install.packages("plotly")
if (!require("viridis")) install.packages("viridis")
if (!require("hrbrthemes")) install.packages("hrbrthemes")
library(plotly)
library(viridis)
library(hrbrthemes)

var = c("Auction House Premises","Auctioneer","Cabaret","Dealer In Products"            
        ,"Debt Collection Agency",         "Electronic & Appliance Service"
        ,"Electronics Store",          "Employment Agency"             
        ,"Games of Chance",                "Garage"                        
        ,"Garage and Parking Lot"  ,       "General Vendor"                
        ,"Home Improvement Contractor" ,   "Home Improvement Salesperson"  
        ,"Laundries"         ,             "Laundry"                       
        , "Laundry Jobber"     ,            "Locksmith"                     
        , "Newsstand"        ,              "Parking Lot"                   
        , "Pawnbroker"       ,              "Pedicab Driver"                
        , "Process Server Individual" ,    "Scrap Metal Processor"         
        , "Secondhand Dealer - Auto"  ,     "Secondhand Dealer - General"   
        , "Sidewalk Cafe"        ,          "Sightseeing Guide"             
        , "Special Sale"      ,             "Stoop Line Stand"              
        , "Temporary Street Fair Vendor" ,  "Ticket Seller"                 
        , "Tobacco Retail Dealer" ,         "Tow Truck Company"             
        , "Tow Truck Driver"    ,           "Commercial Lessor"             
        , "Pedicab Business"   ,            "Scale Dealer Repairer"         
        , "Amusement Device Temporary" ,    "Catering Establishment"        
        , "Horse Drawn Cab Owner" ,         "Horse Drawn Driver"            
        , "Amusement Arcade"    ,           "Amusement Device Portable"     
        , "Amusement Device Permanent" ,    "Car Wash"                      
        , "Locksmith Apprentice"  ,         "Process Serving Agency"        
        , "General Vendor Distributor"  ,   "Storage Warehouse"             
        , "Booting Company"    ,            "Pool or Billiard Room"         
        , "Sightseeing Bus"    ,            "Secondhand Dealer - Firearms"  
        , "Gaming Cafe"       ,             "Ticket Seller Business"        
        , "Tow Truck Exemption"   ,         "Electronic Cigarette Dealer"   
        , "Bingo Game Operator"   ,         "Third Party Food Delivery"   )

yr <- c("2022","2021","2020","2019","2018","2017")

shinyUI(
  navbarPage(strong("License Application Study",style="color: white;"), theme=shinytheme("cerulean"), 

                
#------------------------------- tab panel - Graphs ---------------------------------
    tabPanel("Graphs",
         icon = icon("map-marker-alt"), #choose the icon for
         dashboardHeader(title="COVID-19's Impact on NYC Businesses", 
                         titleWidth=400),
         
         # SideBar content
         dashboardSidebar(
           sidebarMenu(
             menuItem("Home", tabName="home", icon=icon("home")),
             # menuItem("Map", tabName="map", icon=icon("map")),
             menuItem("Interactive Plots", tabName="plots", icon=icon("signal"),
                      startExpanded=TRUE,
                      menuSubItem("New application", tabName="new_app"),
                      menuSubItem("Processing Time", tabName="proc_time"),
                      menuSubItem("Passing Rate", tabName="pass_rate"),
                      menuSubItem("Time Series", tabName = "time_series")
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
                       h2(strong("COVID-19's Impact on NYC Businesses"), align="center"),
                       h3("Group Members: Guosheng Cai, Sibo Geng, Ke Liu, Xubo Wang", align="center"),
                       br(),
                       br(),
                       p("In this project, our group will explore the dataset containing the business license application from NYC open data.", 
                         style = "font-size:20pt; margin-left:50px; margin-right:50px"),
                       p("As many have observed, many businesses have suffered tremendously during the COVID-19 pandamic. While many old 
                  businesses have been shut down, a much few number of new businesses have started during the period. We visualized the 
                  data to provide clear and direct view of COVID-19's impact on businesses in New York, particularly from the angle of 
                  the pandemic's impact on the license application, both for new applications
                  and renewal of license.", style = "font-size:20pt; margin-left:50px; margin-right:50px"),
                       p("In this project, we want to explore offer some insights into both the impact on businesses overrall, 
                  as well as the impact to particular types of businesses. We hope this project would allow you to gain some insights, 
                  especially for types of businesses that are often forgotten by by the media such as home improvement services 
                  and aundries.", style = "font-size:20pt; margin-left:50px; margin-right:50px"),
                       br(),
                       br(),
                       p("The dataset is available at https://data.cityofnewyork.us/Business/License-Applications/ptev-4hud", 
                         style = "font-size:16pt; margin-left:50px; margin-right:50px")
                     )
             ),
             
             #---------new_app----------
             tabItem(tabName="new_app",
                     fluidPage(
                       titlePanel("Applications of Different License Category"),
                       
                       tabsetPanel(
                         tabPanel("Overall",  
                                  sidebarLayout(
                                    position="left",
                                    sidebarPanel(
                                      selectInput("app_year",label = "Select year", choices = yr, selected = "2017", multiple = F),
                                      selectInput("app_category", label= "Select a License Category", choices = var,
                                                  selected = "Home Improvement Contractor", multiple = F),
                                      checkboxGroupInput("app_action",label = c("Actions"),
                                                         choices =c("All","Application","Renewal"),
                                                         selected = "All")
                                    ),
                                    
                                    mainPanel(plotOutput("app_hist"),
                                              plotOutput('app_plot',hover  = "plot_hover"),
                                              verbatimTextOutput("app_info"))
                                  )
                         ), #tabpanel1
                         
                         tabPanel("Application vs Covid",
                                  sidebarLayout(
                                    position = "left",
                                    sidebarPanel(
                                      selectInput("app_category2", label= "Select a License Category", choices = var,
                                                  selected = "Home Improvement Contractor", multiple = F)
                                    )
                                    
                                    ,
                                    mainPanel(plotlyOutput('app_plot1'),
                                              #label = "Average Applications per Month"
                                              textOutput("app_text"),
                                              verbatimTextOutput("app_code"))
                                    
                                  )
                         ) #tabpanel 2
                       )#tabsetpanel
                       
                       #ADD CONTENT XXXXXXXXXXXXXX
                     )
             ),
             #---------proc_time (processing time) ----------
             tabItem(tabName="proc_time",
                     fluidPage(
                       sidebarLayout(
                         sidebarPanel(
                           
                           # introduction of the content in the current tab
                           p(strong("In this tab, you can explore the the change in processing time for license applications."), 
                             style = "font-size:12pt; color:grey"),
                           p(strong("The top graph will demonstrate the the processing time for all types of businesses, while
                      you can choose a particular type of business to be displayed in bottom graph for comparison"),
                             style = "font-size:12pt; color:grey"),
                           hr(),
                           
                           # select the type of application
                           radioButtons("app_or_renew", 
                                        label=h3("Type of Application"),
                                        choices=list("All"=1, "New Application"=2,
                                                     "Renewal"=3)
                           ),
                           
                           # show the average processing time (2017-2022, before COVID, and after COVID)
                           span(strong(textOutput("val_proc_time_1")), style="font-size:18px; color:orange"),
                           span(strong(textOutput("val_proc_time_1_1")), style="font-size:16px; color:blue"),
                           span(strong(textOutput("val_proc_time_1_2")), style="font-size:16px; color:blue"),
                           br(),
                           hr(),
                           
                           # select the type of biz
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
                           
                           # show the average processing time (2017-2022, before COVID, and after COVID)
                           span(strong(textOutput("val_proc_time_2")), style="font-size:18px; color:orange"),
                           span(strong(textOutput("val_proc_time_2_1")), style="font-size:16px; color:blue"),
                           span(strong(textOutput("val_proc_time_2_2")), style="font-size:16px; color:blue"),
                           
                           width=3
                         ),
                         
                         mainPanel(
                           # display the graph for all biz
                           h2("All Businesses", align="center"),
                           plotOutput("plot_proc_time_1"),
                           
                           # display the graph for the selected type of biz
                           h2("Business-type Specific", align="center"),
                           plotOutput("plot_proc_time_2")
                         ))
                     )
             ),
             
             #---------pass_rate (passing rate)----------
             tabItem(tabName="pass_rate",
                     fluidPage(
                       sidebarLayout(
                         sidebarPanel(
                           # introduction of the content in the current tab
                           p(strong("In this tab, you can explore the the change in passing rate for license applications."), 
                             style = "font-size:12pt; color:grey"),
                           p(strong("The top graph will demonstrate the the passing for all types of businesses, while
                      you can choose a particular type of business to be displayed in bottom graph for comparison"),
                             style = "font-size:12pt; color:grey"),
                           
                           # select the type of application
                           radioButtons("app_or_renew_1", 
                                        label=h3("Type of Application"),
                                        choices=list("All"=1, "New Application"=2,
                                                     "Renewal"=3),
                                        selected=1),
                           
                           # show the average passing rate (2017-2022, before COVID, and after COVID)
                           span(strong(textOutput("val_pass_rate_1")), style="font-size: 18px; color:orange"),
                           span(strong(textOutput("val_pass_rate_1_1")), style="font-size:16px; color:blue"),
                           span(strong(textOutput("val_pass_rate_1_2")), style="font-size:16px; color:blue"),
                           br(),
                           hr(),
                           
                           # select the type of biz
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
                           
                           # show the average passing rate (2017-2022, before COVID, and after COVID)
                           span(strong(textOutput("val_pass_rate_2")), style="font-size: 18px; color:orange"),
                           span(strong(textOutput("val_pass_rate_2_1")), style="font-size:16px; color:blue"),
                           span(strong(textOutput("val_pass_rate_2_2")), style="font-size:16px; color:blue"),
                           
                           width=3
                         ),
                         mainPanel(
                           # display the graph for all biz
                           h2("All Businesses", align="center"),
                           plotOutput("plot_pass_rate_1"),
                           
                           # display the graph for the selected type of biz
                           h2("Business-type Specific", align="center"),
                           plotOutput("plot_pass_rate_2")
                         ))
                     )
             ),
             #---------time series----------
             tabItem(tabName="time_series",
                     fluidPage(
                       
                       titlePanel("Time Series Analysis"),
                       
                       sidebarPanel(
                         
                         # introduction of the content in the current tab
                         p(strong("In this tab, you can find the number of applications changing over time and the influence of COVID-19 on it. It can be viewed as a whole, or from different license type and application category."), 
                           style = "font-size:12pt; color:grey"),
                         p(strong("The orange line represents the fluctuation of the amount of new cases from 2020 to 2022."),
                           style = "font-size:12pt; color:grey"),
                         hr(),
                         selectInput('ts_input3', "Category",
                                     choices = c('All','Application or Renew','Basic or Special'),
                                     selected = 'All'),
                         
                         selectInput('ts_input1', 'Year',
                                     choices = c('All','2017','2018','2019','2020','2021','2022'),
                                     selected = 'All'),
                         selectInput('ts_input2', "License Type",
                                     choices = c('All','Business','Individual'),
                                     selected = 'All')
                         
                       ),
                       
                       mainPanel(
                         h2("Time Series Analysis Plot", align="center"),
                         plotOutput('ts_plot')
                       )
                     )
             ),
             
             
             #---------findings----------
             tabItem(tabName="findings",
                     fluidPage(
                       h5("DCA offers two license types: Business, license is issued to an entity/organization based on their address and Individual, license is issued to an individual person. From the plots we can find that the number of applications of individual type license was decreased significantly during the beginning of Covid-19 period while the amount of applications of business suffered a less loss. Also, most of the applications of individual during this time are renewal rather than application. (New application is for applicant does not currently hold a DCA license, a renewal is for applicant holds a DCA license that is about to expire)."),
                      
                       h5("Applications are categorized as either Basic or Special. Special applications require additional documentation and/or liaising with external agencies. From 2020 to 2022, basic type took the majority of individual application while 2018 was still a blooming stage of special type license. On the contrary, the balance of basic and special in business type was kept successfully even during the special time."),
                       

                      h5("New York City’s Emergency Executive Orders (EEOs) extended license expiration dates and renewal application deadlines for those whose licenses were going to expire from March 12, 2020 through August 14, 2021. As a result, the number of application for renewal after March 2020 decreased. 
At the end of 2020, the quantity of application began to drift upwards because the number of Covid case in nyc continued to fall, and it returned to normal due to the new outbreak of virus at the start of 2021.
On June 24, 2021, New York’s COVID-19 State of Emergency expired and many New York Forward industry reopening guidance documents are no longer mandatory. We can see from the plots that the total amount of application, especially the renewal increased.")
                       
                       
                       #ADD CONTENT XXXXXXXXXXXXXX
                     )
             )
           )
         )
    ), #navbarPage closing  
#------------------------------- tab panel - Maps ---------------------------------
tabPanel("Maps",icon = icon("map-marker-alt"), 
         div(class = 'outer',
             fluidRow(
               splitLayout(cellWidths = c("50%", "50%"), 
                           leafletOutput("left_map",width="100%",height=1200),
                           leafletOutput("right_map",width="100%",height=1200))
             ),
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
                           # select the type of biz
                           radioButtons(
                             "adjust_business_category",
                             label="Business Category",
                             choices=c("All",
                                       "Home Improvement Contractor",
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
                            selected="All",
                            inline = TRUE
                          ),
                          style = "opacity: 0.80"
             ) #Panel Control - Closing
         ) #Maps - Div closing
      ) #tabPanel maps closing
  )
) #Shiny UI closing    
