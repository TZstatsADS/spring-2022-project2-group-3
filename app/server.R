library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)


function(input, output) {
  # setwd("E:/cu/ads/pj2")
  df0 <- read.csv('License_Applications.csv')
  covid <- read.csv('COVID-19_Daily_Counts_of_Cases.csv')
  covid <- covid %>% select(DATE_OF_INTEREST,CASE_COUNT) %>%
    mutate(DATE_OF_INTEREST = mdy(DATE_OF_INTEREST)) %>%
    rename(date = DATE_OF_INTEREST, case = CASE_COUNT)
  df <- df0 %>% mutate(Start.Date = mdy(Start.Date), End.Date = mdy(End.Date))
  df <- df %>% filter(year(Start.Date) >= 2017, State == 'NY')
  df <- df %>% select(Application.ID, Start.Date, License.Type, Application.or.Renewal, Status, Application.Category)
  
  output$plot <- renderPlot({
    
    year = input$input1
    print(year)
    license = input$input2
    category = input$input3
    
    aorr <- df %>% select(Start.Date, Application.or.Renewal)
    ac <- df %>% select(Start.Date, Application.Category)
    lic <- df %>% filter(License.Type == license) %>%
      select(Start.Date, Application.Category, Application.or.Renewal)
    
    if (year == 'All'){
      covid_1 <- covid %>% 
        mutate(date = format(as.Date(date),'%Y-%m'), case = case/100) %>%
        group_by(date) %>%
        mutate(n = sum(case)) %>%
        select(date, n) %>%
        unique()
      if (license == "All") {
        if (category == "All") {
          df_0 <- df %>% mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m')) %>%
            select(Start.Date)
          
          p <- ggplot() + geom_bar(data = df_0, aes(x = Start.Date), fill = '#6699CC') + 
            geom_line(data = covid_1, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
            geom_point(data = covid_1, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
        }
        else if (category == "Application or Renew") {
          
          
          aorr_0 <- aorr %>% mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m'))
          
          p <- ggplot() + geom_bar(data = aorr_0, aes(x = Start.Date, fill = Application.or.Renewal)) + scale_fill_brewer() +
              geom_line(data = covid_1, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
              geom_point(data = covid_1, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
        }
        
        else {
          ac_0 <- ac %>% mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m'))
          
          p <- ggplot() + geom_bar(data = ac_0, aes(x = Start.Date, fill = Application.Category)) + scale_fill_brewer() +
            geom_line(data = covid_1, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
            geom_point(data = covid_1, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
        }
      }
      
      else {
        lic <- lic %>% mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m'))
        if (category == "All") {
          p <- ggplot() + geom_bar(data = lic, aes(x = Start.Date), fill = '#6699CC') + 
            geom_line(data = covid_1, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
            geom_point(data = covid_1, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
        }
        else if (category == "Application or Renew") {
          p <- ggplot() + geom_bar(data = lic, aes(x = Start.Date, fill = Application.or.Renewal)) + scale_fill_brewer() +
              geom_line(data = covid_1, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
              geom_point(data = covid_1, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
        }
        else {
          p <- ggplot() + geom_bar(data = lic, aes(x = Start.Date, fill = Application.Category)) + scale_fill_brewer() +
            geom_line(data = covid_1, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
            geom_point(data = covid_1, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
        }
      }
    }
    
    else if (strtoi(year) >= 2017 && strtoi(year) <= 2019) {
      df_year <- df %>% filter(year(Start.Date) == strtoi(year)) %>%
        mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m')) %>%
        select(Start.Date)
      if (license == "All") {
        if (category == "All") {
          p <- ggplot() + geom_bar(data = df_year, aes(x = Start.Date),fill = '#6699CC')
        }
        else if (category == "Application or Renew"){
          aorr_year <- aorr %>% filter(year(Start.Date) == strtoi(year)) %>%
            mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m'))
          p <- ggplot() + geom_bar(data = aorr_year, aes(x = Start.Date, fill = Application.or.Renewal)) + scale_fill_brewer()
        }
        else {
          ac_year <- ac %>% filter(year(Start.Date) == strtoi(year)) %>%
            mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m'))
          p <- ggplot() + geom_bar(data = ac_year, aes(x = Start.Date, fill = Application.Category)) + scale_fill_brewer()
        }
      }
      
      else {
        lic_year <- lic %>% filter(year(Start.Date) == strtoi(year)) %>%
          mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m'))
        if (category == "All") {
          p <- ggplot() + geom_bar(data = lic_year, aes(x = Start.Date),fill = '#6699CC')
        }
        else if (category == "Application or Renew"){
          p <- ggplot() + geom_bar(data = lic_year, aes(x = Start.Date, fill = Application.or.Renewal)) + scale_fill_brewer()
        }
        else {
          p <- ggplot() + geom_bar(data = lic_year, aes(x = Start.Date, fill = Application.Category)) + scale_fill_brewer()
        }
      }
    }
    
    else {
      df_year <- df %>% filter(year(Start.Date) == strtoi(year)) %>%
        mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m')) %>%
        select(Start.Date)
      covid_year <- covid %>% filter(year(date) == strtoi(year)) %>%
          mutate(date = format(as.Date(date),'%Y-%m'), case = case/100) %>%
          group_by(date) %>%
          mutate(n = sum(case)) %>%
          select(date, n) %>%
          unique()
      if (license == "All") {
        if (category == "All") {
          p <- ggplot() + geom_bar(data = df_year, aes(x = Start.Date),fill = '#6699CC') +
              geom_line(data = covid_year, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
              geom_point(data = covid_year, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
        }
        else if (category == "Application or Renew"){
          aorr_year <- aorr %>% filter(year(Start.Date) == strtoi(year)) %>%
            mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m'))
          p <- ggplot() + geom_bar(data = aorr_year, aes(x = Start.Date, fill = Application.or.Renewal)) + scale_fill_brewer() +
            geom_line(data = covid_year, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
            geom_point(data = covid_year, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
        }
        else {
          ac_year <- ac %>% filter(year(Start.Date) == strtoi(year)) %>%
            mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m'))
          p <- ggplot() + geom_bar(data = ac_20, aes(x = Start.Date, fill = Application.Category)) + scale_fill_brewer() +
              geom_line(data = covid_20, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
              geom_point(data = covid_20, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
        }
      }
      
      else {
        lic_year <- lic %>% filter(year(Start.Date) == strtoi(year)) %>%
          mutate(Start.Date = format(as.Date(Start.Date),'%Y-%m'))
        if (category == "All") {
          p <- ggplot() + geom_bar(data = lic_year, aes(x = Start.Date),fill = '#6699CC') +
              geom_line(data = covid_year, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
              geom_point(data = covid_year, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
        }
        else if (category == "Application or Renew"){
          p <- ggplot() + geom_bar(data = lic_year, aes(x = Start.Date, fill = Application.or.Renewal)) + scale_fill_brewer() +
            geom_line(data = covid_year, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
            geom_point(data = covid_year, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
        }
        else {
          p <- ggplot() + geom_bar(data = lic_year, aes(x = Start.Date, fill = Application.Category)) + scale_fill_brewer() +
            geom_line(data = covid_year, aes(x = date, y = n, group = 1),size = 1.2, color = '#FFCC99')+
            geom_point(data = covid_year, aes(x = date, y = n, group = 1), size = 1.2, color = '#FFCC99')
        }
      }
    }
      
    
    print(p)
    
  }, height=600)
  
}