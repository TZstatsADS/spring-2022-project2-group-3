
fluidPage(
  
  titlePanel("Time Series Analysis"),
  
  sidebarPanel(
    
    selectInput('input1', 'Year',
                choices = c('All','2017','2018','2019','2020','2021','2022'),
                selected = 'All'),
    selectInput('input2', "License Type",
                choices = c('All','Business','Individual'),
                selected = 'All'),
    selectInput('input3', "Category",
                choices = c('All','Application or Renew','Application Categoty'),
                selected = 'All'),

  ),
  
  mainPanel(
    plotOutput('plot')
  )
)