setwd('C:\Users\silva\Documents\Estatistica-ProjetoR')

source('global.R')
source('ui.R')
source('server.R')

shinyApp(
  ui = ui,
  server = server
)

