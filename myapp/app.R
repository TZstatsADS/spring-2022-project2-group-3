library("shiny")
source("./myapp/ui.R")
source("./myapp/server.R")
shinyApp(ui = shinyUI, server = shinyServer)
