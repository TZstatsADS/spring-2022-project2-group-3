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

if (!require("ggplot2")) {
    install.packages("ggplot2")
    library(ggplot2)
}

if (!require("lubridate")) {
    install.packages("lubridate")
    library(lubridate)
}

heatPlugin <- htmlDependency("Leaflet.heat", "99.99.99",
             src = c(href = "http://leaflet.github.io/Leaflet.heat/dist/"),
             script = "leaflet-heat.js")

registerPlugin <- function(map, plugin) {
    map$dependencies <- c(map$dependencies, list(plugin))
    map}



data_raw = read.csv('../data/License_Applications.csv')
data = data_raw %>%
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

data_pre = data %>%
    filter(s_date < as_date("03/01/2020",format = "%m/%d/%Y"))

data_after = data %>%
    filter(s_date > as_date("03/01/2020",format = "%m/%d/%Y"))

shinyServer(function(input, output) {
    output$left_map <- renderLeaflet(
        {
            left_df = data %>%
                filter(s_date <= as.Date("05/31/2019",format = "%m/%d/%Y"))
            if(input$adjust_status != "All"){
                left_df = left_df %>%
                    filter(status == input$adjust_status)
            }
            if(input$adjust_type != "All"){
                left_df = left_df %>%
                    filter(app_or_renew == input$adjust_type)
            }
            if(input$adjust_business_category != "All"){
                left_df = left_df %>%
                    filter(category == input$adjust_business_category)
            }
            
        
            left_df %>%
                leaflet(options = leafletOptions(minZoom = 11, maxZoom = 13)) %>%
                addTiles() %>%
                addProviderTiles(providers$CartoDB) %>%
                setView(-73.9824,40.7364,zoom = 12) %>%
                addWebGLHeatmap(~longitude, ~latitude, size = 200, alphaRange = 0.5, intensity = 0.1)
    })
    
    output$right_map <- renderLeaflet(
        {
            right_df = data %>%
                filter(s_date >= as.Date("04/30/2020",format = "%m/%d/%Y"))
            if(input$adjust_status != "All"){
                right_df = right_df %>%
                    filter(status == input$adjust_status)
            }
            
            if(input$adjust_type != "All"){
                right_df = right_df %>%
                    filter(app_or_renew == input$adjust_type)
            }
            
            if(input$adjust_business_category != "All"){
                right_df = right_df %>%
                    filter(category == input$adjust_business_category)
            }
            
            
            right_df %>%
                leaflet(options = leafletOptions(minZoom = 11, maxZoom = 13)) %>%
                addTiles() %>%
                addProviderTiles(providers$CartoDB) %>%
                setView(-73.9824,40.7364,zoom = 12) %>%
                addWebGLHeatmap(~longitude, ~latitude, size = 200, alphaRange = 0.5, intensity = 0.1)
        })
    
    #---------proc_time (processing time)----------
    
    # create appropriate data in accordance with the user input
    # all data
    data_proc_time <- reactive({
        dat = data %>% group_by(month = floor_date(s_date, "month")) %>%
            {if (input$app_or_renew==2) filter(., app_or_renew=="Application") else .} %>%
            {if (input$app_or_renew==3) filter(., app_or_renew=="Renewal") else .} %>%
            summarize(proc_time=mean(difftime(e_date, s_date)))
        return(dat)
    })
    
    # all data pre covid
    data_proc_time_pre <- reactive({
        dat = data_pre %>% group_by(month = floor_date(s_date, "month")) %>%
            {if (input$app_or_renew==2) filter(., app_or_renew=="Application") else .} %>%
            {if (input$app_or_renew==3) filter(., app_or_renew=="Renewal") else .} %>%
            summarize(proc_time=mean(difftime(e_date, s_date)))
        return(dat)
    })
    
    # all data after covid
    data_proc_time_after <- reactive({
        dat = data_after %>% group_by(month = floor_date(s_date, "month")) %>%
            {if (input$app_or_renew==2) filter(., app_or_renew=="Application") else .} %>%
            {if (input$app_or_renew==3) filter(., app_or_renew=="Renewal") else .} %>%
            summarize(proc_time=mean(difftime(e_date, s_date)))
        return(dat)
    })
    
    # data by biz type
    data_proc_time_cat <- reactive({
        dat = data %>% group_by(month = floor_date(s_date, "month")) %>%
            {if (input$app_or_renew==2) filter(., app_or_renew=="Application") else .} %>%
            {if (input$app_or_renew==3) filter(., app_or_renew=="Renewal") else .} %>%
            filter(category==input$category) %>%
            summarize(proc_time=mean(difftime(e_date, s_date)))
        return(dat)
    })
    
    # data by biz type pre covid
    data_proc_time_cat_pre <- reactive({
        dat = data_pre %>% group_by(month = floor_date(s_date, "month")) %>%
            {if (input$app_or_renew==2) filter(., app_or_renew=="Application") else .} %>%
            {if (input$app_or_renew==3) filter(., app_or_renew=="Renewal") else .} %>%
            filter(category==input$category) %>%
            summarize(proc_time=mean(difftime(e_date, s_date)))
        return(dat)
    })
    
    # data by biz type after covid
    data_proc_time_cat_after <- reactive({
        dat = data_after %>% group_by(month = floor_date(s_date, "month")) %>%
            {if (input$app_or_renew==2) filter(., app_or_renew=="Application") else .} %>%
            {if (input$app_or_renew==3) filter(., app_or_renew=="Renewal") else .} %>%
            filter(category==input$category) %>%
            summarize(proc_time=mean(difftime(e_date, s_date)))
        return(dat)
    })
    
    # outputs (plots)
    # plot all data
    output$plot_proc_time_1 <- renderPlot({
        ggplot(data_proc_time(), aes(x=month, y=proc_time)) +
            geom_line(size=1.5) + xlab("Month") + 
            ylab("Average Processing Time in Days") + theme_minimal()
    })
    
    # plot data by biz type
    output$plot_proc_time_2 <- renderPlot({
        ggplot(data_proc_time_cat(), aes(x=month, y=proc_time)) +
            geom_line(size=1.5) + xlab("Month") + 
            ylab("Average Processing Time in Days") +  theme_minimal()
    })
    
    # outputs (texts)
    # all data text
    output$val_proc_time_1 <- renderText({
        paste("Average processing time: ", 
              round(mean(data_proc_time()$proc_time),0), " days",
              sep="")
    })
    
    # all data pre covid text
    output$val_proc_time_1_1 <- renderText({
        paste("Before COVID-19: ", 
              round(mean(data_proc_time_pre()$proc_time),0), " days",
              sep="")
    })
    
    # all data after covid text
    output$val_proc_time_1_2 <- renderText({
        paste("Since COVID-19: ", 
              round(mean(data_proc_time_after()$proc_time),0), " days", 
              sep="")
    })
    
    # data by biz type text
    output$val_proc_time_2 <- renderText({
        paste("Average processing time: ", 
              round(mean(data_proc_time_cat()$proc_time),0), " days",
              sep="")
    })
    
    # data by biz type pre covid text
    output$val_proc_time_2_1 <- renderText({
        paste("Before COVID-19: ", 
              round(mean(data_proc_time_cat_pre()$proc_time),0), " days",
              sep="")
    })
    
    # data by biz type after covid text
    output$val_proc_time_2_2 <- renderText({
        paste("Since COVID-19: ", 
              round(mean(data_proc_time_cat_after()$proc_time),0), " days",
              sep="")
    })
    
    
    
    #---------pass_rate (passing rate)----------
    # create appropriate data in accordance with the user input
    # all data
    data_pass_rate <- reactive({
        dat = data %>% group_by(month = floor_date(s_date, "month")) %>%
            {if (input$app_or_renew_1==2) filter(., app_or_renew=="Application") else .} %>%
            {if (input$app_or_renew_1==3) filter(., app_or_renew=="Renewal") else .} %>%
            summarize(pass_rate = sum(status=="Issued")/(sum(status=="Issued")+
                                                             sum(status=="Denied")))
        return(dat)
    })
    
    # all data pre covid
    data_pass_rate_pre <- reactive({
        dat = data_pre %>% group_by(month = floor_date(s_date, "month")) %>%
            {if (input$app_or_renew_1==2) filter(., app_or_renew=="Application") else .} %>%
            {if (input$app_or_renew_1==3) filter(., app_or_renew=="Renewal") else .} %>%
            summarize(pass_rate = sum(status=="Issued")/(sum(status=="Issued")+
                                                             sum(status=="Denied")))
        return(dat)
    })
    
    # all data after covid
    data_pass_rate_after <- reactive({
        dat = data_after %>% group_by(month = floor_date(s_date, "month")) %>%
            {if (input$app_or_renew_1==2) filter(., app_or_renew=="Application") else .} %>%
            {if (input$app_or_renew_1==3) filter(., app_or_renew=="Renewal") else .} %>%
            summarize(pass_rate = sum(status=="Issued")/(sum(status=="Issued")+
                                                             sum(status=="Denied")))
        return(dat)
    })
    
    # data by biz type
    data_pass_rate_cat <- reactive({
        dat = data %>% group_by(month = floor_date(s_date, "month")) %>%
            {if (input$app_or_renew_1==2) filter(., app_or_renew=="Application") else .} %>%
            {if (input$app_or_renew_1==3) filter(., app_or_renew=="Renewal") else .} %>%
            filter(category==input$category_1) %>%
            summarize(pass_rate = sum(status=="Issued")/(sum(status=="Issued")+
                                                             sum(status=="Denied")))
        return(dat)
    })
    
    # data by biz type pre covid
    data_pass_rate_cat_pre <- reactive({
        dat = data_pre %>% group_by(month = floor_date(s_date, "month")) %>%
            {if (input$app_or_renew_1==2) filter(., app_or_renew=="Application") else .} %>%
            {if (input$app_or_renew_1==3) filter(., app_or_renew=="Renewal") else .} %>%
            filter(category==input$category_1) %>%
            summarize(pass_rate = sum(status=="Issued")/(sum(status=="Issued")+
                                                             sum(status=="Denied")))
        return(dat)
    })
    
    # data by biz type after covid
    data_pass_rate_cat_after <- reactive({
        dat = data_after %>% group_by(month = floor_date(s_date, "month")) %>%
            {if (input$app_or_renew_1==2) filter(., app_or_renew=="Application") else .} %>%
            {if (input$app_or_renew_1==3) filter(., app_or_renew=="Renewal") else .} %>%
            filter(category==input$category_1) %>%
            summarize(pass_rate = sum(status=="Issued")/(sum(status=="Issued")+
                                                             sum(status=="Denied")))
        return(dat)
    })
    
    # output (plots)
    # plot all data
    output$plot_pass_rate_1 <- renderPlot({
        ggplot(data_pass_rate(), aes(x=month, y=pass_rate)) +
            geom_line(size=1.5) + xlab("Month") + 
            ylab("Average Passing Rate") + theme_minimal()
    })
    
    # plot data by biz type
    output$plot_pass_rate_2 <- renderPlot({
        ggplot(data_pass_rate_cat(), aes(x=month, y=pass_rate)) +
            geom_line(size=1.5) + xlab("Month") + 
            ylab("Average Passing Rate") + theme_minimal()
    })
    
    # output (texts)
    # all data text
    output$val_pass_rate_1 <- renderText({
        paste("Average passing rate: ", 
              round(100*mean(data_pass_rate()$pass_rate),2), "%",
              sep="")
    })
    
    # all data before covid text
    output$val_pass_rate_1_1 <- renderText({
        paste("Before COVID-19: ", 
              round(100*mean(data_pass_rate_pre()$pass_rate),2), "%",
              sep="")
    })
    
    # all data after covid text
    output$val_pass_rate_1_2 <- renderText({
        paste("Since COVID-19: ", 
              round(100*mean(data_pass_rate_after()$pass_rate),2), "%",
              sep="")
    })
    
    # data by biz type text
    output$val_pass_rate_2 <- renderText({
        paste("Average passing rate: ", 
              round(100*mean(data_pass_rate_cat()$pass_rate),2), "%",
              sep="")
    })
    
    # data by biz type before covid text
    output$val_pass_rate_2_1 <- renderText({
        paste("Before COVID-19: ", 
              round(100*mean(data_pass_rate_cat_pre()$pass_rate),2), "%",
              sep="")
    })
    
    # data by biz type after covid text
    output$val_pass_rate_2_2 <- renderText({
        paste("Since COVID-19: ", 
              round(100*mean(data_pass_rate_cat_after()$pass_rate),2), "%",
              sep="")
    })
})
