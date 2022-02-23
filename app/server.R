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
if (!require("tidyverse")) {
    install.packages("tidyverse")
    library(tidyverse)
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

if (!require("lubridate")) {
    install.packages("lubridate")
    library(lubridate)
}

if (!require("plotly")) install.packages("plotly")
if (!require("viridis")) install.packages("viridis")
if (!require("hrbrthemes")) install.packages("hrbrthemes")
library(plotly)
library(viridis)
library(hrbrthemes)

#Data Processing
License_Application <- read_csv("../data/License_Applications.csv")
##compute the monthly and annual license applications and renewals in nyc
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
Covid_19_raw <- read_csv("../data/COVID-19.csv")
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
data <- Month_Application_Covid %>% filter(`Application or Renewal` == "Application") %>% 
    mutate(text = paste("Date:",month,"\nCovid_19 cases:",monthly_case_count,"\nApplications:",License_cnt,"\nLicense Category:",`License Category`, sep = ""))

var <- unique(License_by_month$`License Category`)

yr <- c("2022","2021","2020","2019","2018","2017")



shinyServer(function(input, output) {

    output$actions <- renderPrint({ input$actions })
    output$hist <- renderPlot({
        
        ggplot(data = filter(Category_cnt,year(year) == !!input$year ),aes(x = `License Category`, y = cnt, fill = `Application or Renewal`)) + 
            geom_col() + 
            labs(y = "Number of Applications or Renewals ",
                 title = "Application and Renewal for License Category") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, size = 10)) 
        
        
    })
    
    
    
    output$plot <-  renderPlot({
        ggplot(data = filter(All_by_month,`License Category` == !!input$category, action %in% c(!!!input$action)),aes(x = month, y = cnt,color = action)) +
            geom_point() +
            geom_line() +
            labs(title = "Applications and Renewals for License categories",
                 x = "month",
                 y = "Number of Applications or Renewals") +
            scale_x_date(breaks = "4 month") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
    })
    
    
    output$info <- renderText({
        input$plot_hover$y
    })
    
    output$plot1 <- renderPlotly({
        ggplotly(ggplot(data = filter(data,`License Category` == !!input$category2),aes(x=month,y=monthly_case_count,size = License_cnt, color = `License Category`,text = text))+
                     geom_point(alpha=0.7) +
                     geom_line(aes(y = monthly_case_count)) +
                     scale_size(range = c(0.5, 12), name="Applications") +
                     scale_color_manual(values = c("#E7B800")) +
                     labs(title = "Applications under Covid_19",
                          y = "Covid_19 Monthly Case Count") +
                     theme_ipsum() +
                     theme(legend.position="none"), tooltip = "text" )
        
    })
    
    output$text <- renderText({
        "Average Applications per Month"
    })
    output$code <- renderText({
        before <- before_after %>% filter(`License Category` == !!input$category, status =="before")
        after <- before_after %>% filter(`License Category` == !!input$category, status == "after")
        paste0("Before Covid: ", before$average_applications, "\nAfter Covid: ", after$average_applications, sep="")
        
    })
    
    

})


