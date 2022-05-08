library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(lubridate)

master_df <- read.csv('Dataset.csv')
stocks_list <- c('FB', 'AMZN', 'AAPL', 'NFLX', 'GOOG')


master_df$X <- NULL

master_df$Date <- strptime(master_df$Date, format='%Y-%m-%d')