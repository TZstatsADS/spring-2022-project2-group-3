
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


# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    titlePanel("Applications of Different License Category"),
    
    tabsetPanel(
      tabPanel("Overall",  
               sidebarLayout(
                 position="left",
                 sidebarPanel(
                   selectInput("year",label = "Select year", choices = yr, selected = "2017", multiple = F),
                   selectInput("category", label= "Select a License Category", choices = var,
                               selected = "Home Improvement Contractor", multiple = F),
                   checkboxGroupInput("action",label = c("Actions"),
                                      choices =c("All","Application","Renewal"),
                                      selected = "All")
                 ),
                 
                 mainPanel(plotOutput("hist"),
                           plotOutput('plot',hover  = "plot_hover"),
                           verbatimTextOutput("info"))
               )
      ), #tabpanel1
      
      tabPanel("Application vs Covid",
               sidebarLayout(
                 position = "left",
                 sidebarPanel(
                   selectInput("category2", label= "Select a License Category", choices = var,
                               selected = "Home Improvement Contractor", multiple = F)
                 )
                 
                 ,
                 mainPanel(plotlyOutput('plot1'),
                           #label = "Average Applications per Month"
                           textOutput("text"),
                           verbatimTextOutput("code"))
                 
               )
        ) #tabpanel 2
     )#tabsetpanel
    
    
  ) #fluidpage closing
  
) #Shiny UI closing    
