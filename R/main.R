
# Title: Example of forecast using State Space Models
# Description: This script obtain forecast employing State Space Models
#              using data from xls or xlsx
# Version: 0.1.1


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
# Function to extract the date of a ts object
source("R/as_Date_ts.R", encoding = 'UTF-8')


####################################
# READ DATA FROM XLS OR XLSX FILES #
####################################
# Data to forecast
Data     <- read.xlsx('data/Data.xls', sheetIndex = 1)
# Exogenous variables
Exog_Var <- read.xlsx("data/Calendar.xls", sheetIndex = 1)

# Number of available data
n_data <- nrow(Data)


################
# GET FORECAST #
################
# SSM with exogenous variables
Z <- get_forecast(X = Data, Y = Exog_Var, n = n_data, n_f = 6, freq = 12)
# SSM without exogenous variables
# Z <- get_forecast(X = Data, n = n_data, n_f = 6, freq = 12)


######################
# EXTRACT THE VALUES #
######################
# Forecast values
Prediction <- round(Z$Prediction, 0)
colnames(Prediction) <- colnames(Data)[3:ncol(Data)]
# MAPE values
MAPE <- round(Z$MAPE, 2)
# Date of forecasts
Date_f <- Z$Date_f


##########
# OUTPUT #
##########
# The first sheet contain the Input data
write.xlsx(Data, file = "output/Forecast.xls", sheetName = "Data", append = FALSE, row.names = FALSE)
# Forecast data
Forecast <- cbind(Date = Date_f, Forecast = Prediction[(nrow(Data) + 1):nrow(Prediction),])
write.xlsx(Forecast, file = "output/Forecast.xls", sheetName = "Forecast", append = TRUE, row.names = FALSE)
# MAPE values
if (ncol(Prediction) == 1)
  {names(MAPE) <- colnames(Data)[3:ncol(Data)]}
if (ncol(Prediction) > 1)
  {colnames(MAPE) <- colnames(Data)[3:ncol(Data)]}
write.xlsx(MAPE, file = "output/Forecast.xls", sheetName = "MAPE", append = TRUE, row.names = FALSE)
