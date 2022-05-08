header <- dashboardHeader(title = 'Projeto de Estatistica')

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Métricas", tabName = "m", icon = icon("chart-line")),
    menuItem("Comparando propriedades", tabName = "comp", icon = icon("chart-bar"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "m",
            fluidRow(
              box(title = 'Selecione as ações', width = 12, solidHeader = TRUE, status = 'warning',
                  selectInput('stock', 'Ação', stocks_list, multiple = FALSE),
                  dateRangeInput('date', strong('Intervalo de tempo desejado'), 
                                 min = '2015-01-02', 
                                 max = '2021-12-29', 
                                 start = '2015-01-02', 
                                 end = '2021-12-29', 
                                 separator = "ate", 
                                 language = 'pt', 
                                 format = 'dd/mm/yy'),
                  uiOutput("timedate"),
                  actionButton('go', 'Submeter')
              )
            ),
            fluidRow(

              box(title = "Informacoes sobre os valores de fechamento da acao", width = 12, solidHeader = TRUE,

                  DTOutput('info')
              )
            ),
            fluidRow(
              box(title = "Gráfico em linha da ação", width = 12, solidHeader = TRUE,
                  plotOutput('sh')
              )
            ),
            
            fluidRow(
              box(title = "Histograma da ação", width = 12, solidHeader = TRUE,
                  plotOutput('hi')
              )
            ),
            
            fluidRow(
              box(title = "Boxplot da ação", width = 12, solidHeader = TRUE,
                  plotOutput('bo')
              )
            ),
    ),
    tabItem(tabName = 'comp',
            fluidRow(
              box(title = 'Selecione suas duas ações', width = 12, solidHeader = TRUE, status = 'warning',
                  selectInput('stock_comp', 'Ação', stocks_list, multiple = TRUE),
                  dateRangeInput('date_comp', strong('Intervalo de tempo desejado'), 
                                 min = '2015-01-02', 
                                 max = '2021-12-29', 
                                 start = '2015-01-02', 
                                 end = '2021-12-29', 
                                 separator = "ate", 
                                 language = 'pt', 
                                 format = 'dd/mm/yy'),
                  uiOutput("timedate_comp"),
                  actionButton('go_comp', 'Submeter')
              )
            ),
            
            fluidRow(
              box(title = "Correlação entre as ações escolhidas", width = 12, solidHeader = TRUE,
                  DTOutput('co_comp')
              )
            ),
            
            fluidRow(
              box(title = "Grafico de linha das altas das acoes", width = 12, solidHeader = TRUE,
                  plotOutput('li_comp')
              )
            ),
            
            fluidRow(
              box(title = "Gráfico em barra das medias das ações", width = 12, solidHeader = TRUE,
                  plotOutput('ba_comp')
              )
            ),
            
            fluidRow(
              box(title = "Scatterplot das ações", width = 12, solidHeader = TRUE,
                  plotOutput('sc_comp')
              )
            ),
    )
  )
)

ui <- dashboardPage(
  skin = 'black',
  header, sidebar, body
)
