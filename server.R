server <- function(input, output) {
  
  select_stock <- eventReactive(input$go, {
    stock_name <- input$stock
    date <- input$date
    
    df_stock_name <- master_df %>% 
      filter(Stock == stock_name)
    
    df_stock <- df_stock_name %>%
      filter(Date >= date[1] & Date <= date[2])
    
    return(df_stock)
  })
  
  
  df_options <- list(
    pageLenght = 10,
    language = list(
      url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
    )
  )
  ################## ? ##################
  output$sh <- renderPlot({
    df <- select_stock()
    
    aux <- df$High %>% na.omit() %>% as.numeric()
    aux1 <- min(aux)
    aux2 <- max(aux)
    
    remove_missing(df)
    
    df$Date <- ymd(df$Date)
    df %>%
      ggplot(aes(Date, High, group = 1)) +
      geom_path(color = 'orange') +
      ylab('Preco de alta da acao em $') +
      xlab('Intervalo temporal desejado') +
      coord_cartesian(ylim = c(aux1, aux2)) +
      theme_gray() +
      
      scale_x_date(date_labels = "%Y-%m-%d")
    
    
  })
  ################## ? ##################
  
  output$hi <- renderPlot({
    df <- select_stock()
    aux <- df$High %>% na.omit() %>% as.numeric()
    aux1 <- min(aux)
    aux2 <- max(aux)
    remove_missing(df)
    df$Date <- ymd(df$Date)
    
    df %>% 
      ggplot(aes(Date, High, group = 1)) +
      coord_cartesian(ylim = c(aux1, aux2)) +
      geom_bar(stat = 'identity', fill = "orange") +
      theme_gray() +
      scale_x_date(date_labels = "%Y-%m-%d") +
      ylab('Preco de alta da acao em $') +
      xlab('Intervalo temporal desejado')
  })
  
  output$bo <- renderPlot({
    df <- select_stock()
    aux <- df$High %>% na.omit() %>% as.numeric()
    aux1 <- min(aux)
    aux2 <- max(aux)
    remove_missing(df)
    df$Date <- ymd(df$Date)
    
    df %>%
      ggplot(aes(Date, High)) +
      geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=3, fill = "orange") +
      theme_gray() +
      scale_x_date(date_labels = "%Y-%m-%d") +
      coord_cartesian(ylim = c(aux1, aux2)) +
      xlab('Intervalo temporal desejado') +
      ylab('Variacao da acao em $')
  })
  
  ################## Metrics ##################
  
  Info_DataTable <- eventReactive(input$go,{
    df <- select_stock()

    getmode <- function(value) {
      unique_value <- unique(value)
      unique_value[which.max(tabulate(match(value, unique_value)))]
    }
    
    m <- select(df, Close)
    Media <- apply(m, 2, mean)
    Mediana <- apply(m, 2, median)
    Moda <- apply(m, 2, getmode)
    Desvio_Padrao <- apply(m, 2, sd)
    ValorMaximo <- apply(m, 2, max)
    ValorMinimo <- apply(m, 2, min)
    
    Stock <- input$stock
    
    df_tb <-  data.frame(Stock, Media, Mediana, Moda, Desvio_Padrao, ValorMaximo, ValorMinimo)
    colnames(df_tb) <- c("Stock", "Media", "Mediana", "Moda", "Desvio Padrao", "Valor Maximo", "Valor Minimo")
    df_tb <- as.data.frame(t(df_tb))
    
    return(df_tb)
  })
  
  ################## Stocks Info ##################
  
  output$info <- renderDT({
    Info_DataTable() %>%
      as.data.frame() %>% 
      DT::datatable(options=list(
        language=list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
        )
      ))
  })
  
  ################## Line Plot ##################
  comp_line <- eventReactive(input$go_comp, {
    
    if (length(input$stock_comp) != 2){
      return()
    }
    
    stock_name1 <- input$stock_comp[1]
    stock_name2 <- input$stock_comp[2]
    date <- input$date_comp
    
    df <- master_df[
      master_df$Stock == stock_name1 |
      master_df$Stock == stock_name2,
      ] %>% 
      filter(Date >= date[1] & Date <= date[2])
      
    
    aux <- df$High %>% na.omit() %>% as.numeric()
    aux1 <- min(aux)
    aux2 <- max(aux)
    
    remove_missing(df)
    
    df$Date <- ymd(df$Date)
    df %>%
      ggplot(aes(Date, High, group = 1, colour = Stock)) +
      geom_path() +
      ylab('Preco de alta das acoes em $') +
      xlab('Intervalo temporal desejado') +
      coord_cartesian(ylim = c(aux1, aux2)) +
      theme_gray() +
      theme(legend.position = "bottom") +
      scale_x_date(date_labels = "%Y-%m-%d")
  })
  
  
  output$li_comp <- renderPlot(comp_line())

  ################## Bar Plot ##################
  
  bar_line <- eventReactive(input$go_comp, {
    
    if (length(input$stock_comp) != 2){
      return()
    }
    
    
    stock_name1 <- input$stock_comp[1]
    stock_name2 <- input$stock_comp[2]
    date <- input$date_comp
    

    data_test1 <- subset(master_df,Stock==stock_name1)
    data_test2 <- subset(master_df,Stock==stock_name2)
    
    data_test1 <- subset(data_test1,Date > date[1] & Date < date[2])
    data_test2 <- subset(data_test2,Date > date[1] & Date < date[2])
    
    mean1 <- mean(data_test1[["Close"]])
    mean2 <- mean(data_test2[["Close"]])
    
    
    
    data <- data.frame(
      Acoes=c(stock_name1,stock_name2) ,  
      Medias=c(mean1,mean2)
    )
    

    ggplot(data, aes(x=Acoes, y=Medias)) + 
      geom_bar(stat = "identity")
    
  })
  
  output$ba_comp <- renderPlot(bar_line())
  
  
  ################## Scatter Plot ##################
  
  
  scatter_line <- eventReactive(input$go_comp, {
    
    if (length(input$stock_comp) != 2){
      return()
    }
    
    stock_name1 <- input$stock_comp[1]
    stock_name2 <- input$stock_comp[2]
    date <- input$date_comp
    
    data_test1 <- subset(master_df,Stock==stock_name1)
    data_test2 <- subset(master_df,Stock==stock_name2)
    
    data_test1 <- subset(data_test1, Date > date[1] & Date < date[2])
    data_test2 <- subset(data_test2, Date > date[1] & Date < date[2])
    
    
    plot(x=data_test1[,c("Close")],y=data_test2[,c("Close")],
         xlab= stock_name1,
         ylab= stock_name2)
    
    
  })
  
  output$sc_comp <- renderPlot(scatter_line())
  
  ################## Correlations Plot ##################

  correlations1 <- eventReactive(input$go_comp, {
    
    if (length(input$stock_comp) != 2){
      return()
    }
    
    stock_name1 <- input$stock_comp[1]
    stock_name2 <- input$stock_comp[2]

    date <- input$date_comp
    
    data_test1 <- subset(master_df, Stock==stock_name1)
    data_test2 <- subset(master_df, Stock==stock_name2)
    
    getmode <- function(value) {
      unique_value <- unique(value)
      unique_value[which.max(tabulate(match(value, unique_value)))]
    }
    
    m1 <- select(data_test1, Close)
    Media1 <- apply(m1, 2, mean)
    Mediana1 <- apply(m1, 2, median)
    Moda1 <- apply(m1, 2, getmode)
    Desvio_Padrao1 <- apply(m1, 2, sd)
    ValorMaximo1 <- apply(m1, 2, max)
    ValorMinimo1 <- apply(m1, 2, min)
    
    m2 <- select(data_test2, Close)
    Media2 <- apply(m2, 2, mean)
    Mediana2 <- apply(m2, 2, median)
    Moda2 <- apply(m2, 2, getmode)
    Desvio_Padrao2 <- apply(m2, 2, sd)
    ValorMaximo2 <- apply(m2, 2, max)
    ValorMinimo2 <- apply(m2, 2, min)
    
    df_tb1 <-  data.frame(stock_name1, Media1, Mediana1, Moda1, Desvio_Padrao1, ValorMaximo1, ValorMinimo1)
    df_tb2 <-  data.frame(stock_name2, Media2, Mediana2, Moda2, Desvio_Padrao2, ValorMaximo2, ValorMinimo2)
    colnames(df_tb1) <- c("Stock", "Media", "Mediana", "Moda", "Desvio Padrao", "Valor Maximo", "Valor Minimo")
    colnames(df_tb2) <- c("Stock", "Media", "Mediana", "Moda", "Desvio Padrao", "Valor Maximo", "Valor Minimo")
    df_tb <- data.frame(t(df_tb1), t(df_tb2))
    
    return(df_tb)
    
  })
  
    output$co_comp <- renderDT(correlations1())
}
