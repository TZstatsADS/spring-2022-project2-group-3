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

if (!require("ggpubr")) {
    install.packages("ggpubr")
    library(ggpubr)
}

if (!require("plotly")) install.packages("plotly")
if (!require("viridis")) install.packages("viridis")
if (!require("hrbrthemes")) install.packages("hrbrthemes")
library(plotly)
library(viridis)
library(hrbrthemes)

heatPlugin <- htmlDependency("Leaflet.heat", "99.99.99",
             src = c(href = "http://leaflet.github.io/Leaflet.heat/dist/"),
             script = "leaflet-heat.js")

registerPlugin <- function(map, plugin) {
    map$dependencies <- c(map$dependencies, list(plugin))
    map}



data_raw = read.csv('./data/License_Applications.csv')

df_ts <- data_raw %>% mutate(Start.Date = mdy(Start.Date), End.Date = mdy(End.Date))
df_ts <- df_ts %>% filter(year(Start.Date) >= 2017, State == 'NY')
df_ts <- df_ts %>% select(Application.ID, Start.Date, License.Type, Application.or.Renewal, Status, Application.Category)

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

Covid_19_raw <- read_csv("data/COVID-19.csv")

covid_ts <- Covid_19_raw %>% select(DATE_OF_INTEREST,CASE_COUNT) %>%
    mutate(DATE_OF_INTEREST = mdy(DATE_OF_INTEREST)) %>%
    rename(date = DATE_OF_INTEREST, case = CASE_COUNT)


#---------------------------------------------Xubo Data Start---------------------------------------------
License_Application <- read_csv("data/License_Applications.csv")
NYC_License <- License_Application %>% filter(State == "NY") %>%
    mutate(Start_date = mdy(`Start Date`), End_date = mdy(`End Date`), City = tolower(City)) %>%
    dplyr::select(Start_date, End_date, `Application ID`, `License Number`, `License Type`,
                  `Application or Renewal`,`Business Name`,Status,`License Category`,`Application Category`,`Building Number`,Street,City,State,Zip,Longitude,Latitude) %>%
    filter(Start_date >= as.Date("2017-01-01")) %>%
    filter(City %in% c("bronx","brooklyn","new york",
                       "staten island", "manhattan", "queens village", "queens vlg")) 
License_by_month <- NYC_License %>% 
    group_by(month = floor_date(Start_date,"month"),`Application or Renewal`,`License Category`) %>% 
    summarise(cnt = n()) %>%
    arrange(month)

#by year
Category_cnt <- NYC_License %>% group_by(year = floor_date(Start_date,"year"),`Application or Renewal`,`License Category`) %>% summarise(cnt = n()) %>% 
    arrange(year, desc(cnt))

##Covid 19 info
Covid_19 <- Covid_19_raw %>% mutate(Date = mdy(DATE_OF_INTEREST), .before = DATE_OF_INTEREST) %>%
    dplyr::select(Date, CASE_COUNT,probable_case_count, HOSPITALIZED_COUNT,DEATH_COUNT,
                  DEATH_COUNT_PROBABLE,CASE_COUNT_7DAY_AVG,all_case_count_7day_avg,
                  HOSP_COUNT_7DAY_AVG,DEATH_COUNT_7DAY_AVG,all_death_count_7day_avg)

Covid_by_month <- Covid_19 %>% group_by(month = floor_date(Date,"month")) %>% 
    summarise(monthly_case_count = sum(CASE_COUNT), monthly_death = sum(DEATH_COUNT), monthly_hospitalized = sum(HOSPITALIZED_COUNT),
              monthly_case_probable = sum(probable_case_count), monthly_death_probable = sum(DEATH_COUNT_PROBABLE)) %>% 
    arrange(desc(month))

##Join two tables 
Month_Application_Covid <- License_by_month %>%
    inner_join(Covid_by_month, by = c("month" = "month"))

#add in variable "All"
All_by_month <- License_by_month %>% pivot_wider(names_from = `Application or Renewal`, values_from = cnt) %>% 
    replace_na(list(Application = 0, Renewal = 0)) %>%
    mutate(All = Application + Renewal) %>%
    pivot_longer(cols = c(Application,Renewal,All), names_to = "action", values_to = "cnt" )

#data need to be used for comparison
comp_data <- Month_Application_Covid %>%  
    filter(`Application or Renewal` == "Application") %>%
    mutate(text = paste("Date:",month,"\nCovid_19 cases:",monthly_case_count,"\nApplications:",cnt,"\nLicense Category:",`License Category`, sep = ""))

before_after <- All_by_month %>% filter(action=="Application") %>% mutate(status = ifelse(month >= "2020-03-01","after","before")) %>%
    group_by(`License Category`, status) %>%
    summarise(average_applications = round(mean(cnt),1)) %>% arrange(desc(average_applications))

var <- unique(License_by_month$`License Category`)
yr <- c("2022","2021","2020","2019","2018","2017")
#------------------------------------------Xubo data end---------------------------------------


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
    
    #---------Applications by Category start------------------------------
    
    output$app_action <- renderPrint({input$app_action})
    output$app_hist <- renderPlot({
        
        ggplot(data = filter(Category_cnt,year(year) == !!input$app_year ),aes(x = `License Category`, y = cnt, fill = `Application or Renewal`)) + 
            geom_col() + 
            labs(y = "Number of Applications or Renewals ",
                 title = "Application and Renewal for License Category") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, size = 10)) 
        
        
    })
    
    
    
    output$app_plot <-  renderPlot({
        ggplot(data = filter(All_by_month,`License Category` == !!input$app_category, action %in% c(!!!input$app_action)),aes(x = month, y = cnt,color = action)) +
            geom_point() +
            geom_line() +
            labs(title = "Applications and Renewals for License categories",
                 x = "month",
                 y = "Number of Applications or Renewals") +
            scale_x_date(breaks = "4 month") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
    })
    
    
    output$app_info <- renderText({
        input$plot_hover$y
    })
    
    output$app_plot1 <- renderPlotly({
        ggplotly(ggplot(data = filter(comp_data,`License Category` == !!input$app_category2),aes(x=month,y=monthly_case_count,size = cnt, color = `License Category`,text = text))+
                     geom_point(alpha=0.7) +
                     geom_line(aes(y = monthly_case_count)) +
                     scale_size(range = c(0.5, 12), name="Applications") +
                     scale_color_manual(values = c("#E7B800")) +
                     labs(title = "Applications under Covid_19",
                          y = "Covid_19 Monthly Case Count") +
                     theme_ipsum() +
                     theme(legend.position="none"), tooltip = "text" )
        
    })
    
    output$app_text <- renderText({
        "Average Applications per Month"
    })
    output$app_code <- renderText({
        before <- before_after %>% filter(`License Category` == !!input$app_category, status =="before")
        after <- before_after %>% filter(`License Category` == !!input$app_category, status == "after")
        paste0("Before Covid: ", before$average_applications, "\nAfter Covid: ", after$average_applications, sep="")
        
    })
    
    #-------Applications by Category end-----------------------------------------
    
    
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
    
    # ts_plot
    output$ts_plot <- renderPlot({
        
        year = input$ts_input1
        license = input$ts_input2
        category = input$ts_input3
        
        aorr <- df_ts %>% select(Start.Date, Application.or.Renewal)
        ac <- df_ts %>% select(Start.Date, Application.Category)
        lic <- df_ts %>% filter(License.Type == license) %>%
            select(Start.Date, Application.Category, Application.or.Renewal)
        
        if (year == 'All'){
            covid_1 <- covid_ts %>% 
                mutate(date = format(as.Date(date),'%Y-%m'), case = case/100) %>%
                group_by(date) %>%
                mutate(n = sum(case)) %>%
                select(date, n) %>%
                unique()
            if (license == "All") {
                if (category == "All") {
                    df_0 <- df_ts %>% mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m')) %>%
                        select(Start.Date)
                    
                    ggplot() + geom_bar(data = df_0, aes(x = Start.Date), fill = '#6699CC') + 
                        geom_line(data = covid_1, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
                        geom_point(data = covid_1, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
                }
                else if (category == "Application or Renew") {
                    
                    
                    aorr_0 <- aorr %>% mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m'))
                    
                    ggplot() + geom_bar(data = aorr_0, aes(x = Start.Date, fill = Application.or.Renewal)) + scale_fill_brewer() +
                        geom_line(data = covid_1, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
                        geom_point(data = covid_1, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
                }
                
                else {
                    ac_0 <- ac %>% mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m'))
                    
                    ggplot() + geom_bar(data = ac_0, aes(x = Start.Date, fill = Application.Category)) + scale_fill_brewer() +
                        geom_line(data = covid_1, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
                        geom_point(data = covid_1, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
                }
            }
            
            else {
                lic <- lic %>% mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m'))
                if (category == "All") {
                    ggplot() + geom_bar(data = lic, aes(x = Start.Date), fill = '#6699CC') + 
                        geom_line(data = covid_1, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
                        geom_point(data = covid_1, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
                }
                else if (category == "Application or Renew") {
                    ggplot() + geom_bar(data = lic, aes(x = Start.Date, fill = Application.or.Renewal)) + scale_fill_brewer() +
                        geom_line(data = covid_1, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
                        geom_point(data = covid_1, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
                }
                else {
                    ggplot() + geom_bar(data = lic, aes(x = Start.Date, fill = Application.Category)) + scale_fill_brewer() +
                        geom_line(data = covid_1, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
                        geom_point(data = covid_1, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
                }
            }
        }
        
        else if (strtoi(year) >= 2017 && strtoi(year) <= 2019) {
            df_year <- df_ts %>% filter(year(Start.Date) == strtoi(year)) %>%
                mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m')) %>%
                select(Start.Date)
            if (license == "All") {
                if (category == "All") {
                    ggplot() + geom_bar(data = df_year, aes(x = Start.Date),fill = '#6699CC')
                }
                else if (category == "Application or Renew"){
                    aorr_year <- aorr %>% filter(year(Start.Date) == strtoi(year)) %>%
                        mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m'))
                    ggplot() + geom_bar(data = aorr_year, aes(x = Start.Date, fill = Application.or.Renewal)) + scale_fill_brewer()
                }
                else {
                    ac_year <- ac %>% filter(year(Start.Date) == strtoi(year)) %>%
                        mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m'))
                    ggplot() + geom_bar(data = ac_year, aes(x = Start.Date, fill = Application.Category)) + scale_fill_brewer()
                }
            }
            
            else {
                lic_year <- lic %>% filter(year(Start.Date) == strtoi(year)) %>%
                    mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m'))
                if (category == "All") {
                    ggplot() + geom_bar(data = lic_year, aes(x = Start.Date),fill = '#6699CC')
                }
                else if (category == "Application or Renew"){
                    ggplot() + geom_bar(data = lic_year, aes(x = Start.Date, fill = Application.or.Renewal)) + scale_fill_brewer()
                }
                else {
                    ggplot() + geom_bar(data = lic_year, aes(x = Start.Date, fill = Application.Category)) + scale_fill_brewer()
                }
            }
        }
        
        else {
            df_year <- df_ts %>% filter(year(Start.Date) == strtoi(year)) %>%
                mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m')) %>%
                select(Start.Date)
            covid_year <- covid_ts %>% filter(year(date) == strtoi(year)) %>%
                mutate(date = format(as.Date(date),'%Y-%m'), case = case/100) %>%
                group_by(date) %>%
                mutate(n = sum(case)) %>%
                select(date, n) %>%
                unique()
            if (license == "All") {
                if (category == "All") {
                    ggplot() + geom_bar(data = df_year, aes(x = Start.Date),fill = '#6699CC') +
                        geom_line(data = covid_year, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
                        geom_point(data = covid_year, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
                }
                else if (category == "Application or Renew"){
                    aorr_year <- aorr %>% filter(year(Start.Date) == strtoi(year)) %>%
                        mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m'))
                    ggplot() + geom_bar(data = aorr_year, aes(x = Start.Date, fill = Application.or.Renewal)) + scale_fill_brewer() +
                        geom_line(data = covid_year, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
                        geom_point(data = covid_year, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
                }
                else {
                    ac_year <- ac %>% filter(year(Start.Date) == strtoi(year)) %>%
                        mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m'))
                    ggplot() + geom_bar(data = ac_year, aes(x = Start.Date, fill = Application.Category)) + scale_fill_brewer() +
                        geom_line(data = covid_year, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
                        geom_point(data = covid_year, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
                }
            }
            
            else {
                lic_year <- lic %>% filter(year(Start.Date) == strtoi(year)) %>%
                    mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m'))
                if (category == "All") {
                    ggplot() + geom_bar(data = lic_year, aes(x = Start.Date),fill = '#6699CC') +
                        geom_line(data = covid_year, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
                        geom_point(data = covid_year, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
                }
                else if (category == "Application or Renew"){
                    ggplot() + geom_bar(data = lic_year, aes(x = Start.Date, fill = Application.or.Renewal)) + scale_fill_brewer() +
                        geom_line(data = covid_year, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
                        geom_point(data = covid_year, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
                }
                else {
                    ggplot() + geom_bar(data = lic_year, aes(x = Start.Date, fill = Application.Category)) + scale_fill_brewer() +
                        geom_line(data = covid_year, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
                        geom_point(data = covid_year, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
                }
            }
        }
        
    })
})
