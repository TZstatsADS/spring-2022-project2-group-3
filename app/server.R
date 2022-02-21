#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
###############################Install Related Packages #######################
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

if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require("lubridate")) {
  install.packages("lubridate")
  library(lubridate)
}

#Data Loading
data_raw = read.csv('../data/License_Applications.csv')

#Data Processing
data = data_raw %>%
  select(ID="Application.ID", l_type="License.Type", 
         app_or_renew="Application.or.Renewal", status="Status",
         s_date="Start.Date", e_date="End.Date",
         category="License.Category", app_cat="Application.Category",
         city="City", state="State", zip="Zip", longitute="Longitude", 
         latitude="Latitude") %>%
  drop_na() %>%
  mutate(s_date = 
           as_date(s_date, format = "%m/%d/%Y")) %>%
  mutate(e_date = 
           as_date(e_date, format = "%m/%d/%Y")) %>%
  filter(state=="NY") %>% 
  filter(s_date >= as_date("01/01/2017",format = "%m/%d/%Y")) %>%
  filter(s_date < e_date)

#Identifying major categories
#cats = data %>% count(category) %>%
#  filter(n>1000) %>%
#  select(category)
#data = data %>%
#  filter(category %in% cats$category)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    #---------proc_time----------
    data_proc_time <- reactive({
      dat = data %>% group_by(month = floor_date(s_date, "month")) %>%
        {if (input$app_or_renew==2) filter(., app_or_renew=="Application") else .} %>%
        {if (input$app_or_renew==3) filter(., app_or_renew=="Renewal") else .} %>%
        summarize(proc_time=mean(difftime(e_date, s_date)))
      return(dat)
    })
    
    data_proc_time_cat <- reactive({
      dat = data %>% group_by(month = floor_date(s_date, "month")) %>%
        {if (input$app_or_renew==2) filter(., app_or_renew=="Application") else .} %>%
        {if (input$app_or_renew==3) filter(., app_or_renew=="Renewal") else .} %>%
        filter(category==input$category) %>%
        summarize(proc_time=mean(difftime(e_date, s_date)))
      return(dat)
    })
  
    output$plot_proc_time_1 <- renderPlot({
      ggplot(data_proc_time(), aes(x=month, y=proc_time)) +
        geom_line(size=1.5) + xlab("Month") + 
        ylab("Average Processing Time in Days")
    })
    
    output$plot_proc_time_2 <- renderPlot({
      ggplot(data_proc_time_cat(), aes(x=month, y=proc_time)) +
        geom_line(size=1.5) + xlab("Month") + 
        ylab("Average Processing Time in Days")
    })
    
    
    #---------pass_rate----------
    data_pass_rate <- reactive({
      dat = data %>% group_by(month = floor_date(s_date, "month")) %>%
        {if (input$app_or_renew_1==2) filter(., app_or_renew=="Application") else .} %>%
        {if (input$app_or_renew_1==3) filter(., app_or_renew=="Renewal") else .} %>%
        summarize(pass_rate = sum(status=="Issued")/(sum(status=="Issued")+
                                                         sum(status=="Denied")))
      return(dat)
    })
    
    data_pass_rate_cat <- reactive({
      dat = data %>% group_by(month = floor_date(s_date, "month")) %>%
        {if (input$app_or_renew_1==2) filter(., app_or_renew=="Application") else .} %>%
        {if (input$app_or_renew_1==3) filter(., app_or_renew=="Renewal") else .} %>%
        filter(category==input$category_1) %>%
        summarize(pass_rate = sum(status=="Issued")/(sum(status=="Issued")+
                                                       sum(status=="Denied")))
      return(dat)
    })
    
    output$plot_pass_rate_1 <- renderPlot({
      ggplot(data_pass_rate(), aes(x=month, y=pass_rate)) +
        geom_line(size=1.5) + xlab("Month") + 
        ylab("Average Passing Rate")
    })
    
    output$plot_pass_rate_2 <- renderPlot({
      ggplot(data_pass_rate_cat(), aes(x=month, y=pass_rate)) +
        geom_line(size=1.5) + xlab("Month") + 
        ylab("Average Passing Rate")
    })

})


