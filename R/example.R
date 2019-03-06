
# Title: Example of forecast using State Space Models
# Description: This script obtain forecast employing State Space Models
#              using data from xls or xlsx
# Version: 0.1.0
# Author: Elisa M. Jorge González <elisajg0@gmail.com>


######################
# LOADING R PACKAGES #
######################
# To read xls and xlsx files
library(readxl)
# To export xls and xlsx files
library(xlsx)
# To execute State Space Models
library(KFAS)


#######################
# LOADING R FUNCTIONS #
#######################
# Function to get forecast
source("R/forecast.R", encoding = 'UTF-8') 


####################################
# READ DATA FROM XLS OR XLSX FILES #
####################################
# Data to forecast
Data     <- read.xlsx('data/Data.xls', sheetIndex = 1)
# Exogenous variables
Calendar <- read.xlsx("data/Calendar.xls", sheetIndex = 1)

# Number of available data
m <- nrow(Data)
# Number of available date
d <- nrow(Calendar)


#########################
# TIME SERIES STRUCTURE #
#########################
# Data to forecast
Data_ts <- as.data.frame(ts(Data[,3:ncol(Data)], start = c(Data[1,1],Data[1,2]), frequency = 12))
Data_ts <- ts(rbind(as.matrix(Data[,3:ncol(Data)]),matrix(NA, ncol = ncol(Data) - 2, nrow = 6)), 
                              start = c(Data[1,1],Data[1,2]), 
                              frequency = 12)
# Exogenous variables
Calendar_ts <- ts(Calendar[,2:ncol(Calendar)], 
                  start = c(Data[1,1],Data[1,2]), 
                  end = time(Data_ts)[nrow(Data_ts)], 
                  frequency = 12)


################
# GET FORECAST #
################
Z <- get_forecast(Data_ts,Calendar_ts,m)


######################
# EXTRACT THE VALUES #
######################
# Forecast values
Prediction <- round(Z$Prediction,0)
colnames(Prediction) <- colnames(Data)[3:ncol(Data)]
# MAPE values
MAPE <- Z$MAPE


##########
# OUTPUT #
##########
# Input data
write.xlsx(Data, file = "output/Forecast.xls", sheetName = "Data", append = FALSE)
# Forecast data
Date_f   <- as.character(Calendar[(nrow(Data) + 1):nrow(Data_ts),1])
Forecast <- cbind(Date_f, Prediction[(nrow(Data) + 1):nrow(Data_ts),])
write.xlsx(Forecast, file = "output/Forecast.xls", sheetName = "Forecast", append = TRUE)
# MAPE values
if (ncol(Prediction) == 1)
{names(MAPE) <- colnames(Data)[3:ncol(Data)]}
if (ncol(Prediction) > 1)
{colnames(MAPE) <- colnames(Data)[3:ncol(Data)]}
write.xlsx(MAPE, file = "output/Forecast.xls", sheetName = "MAPE", append = TRUE)
