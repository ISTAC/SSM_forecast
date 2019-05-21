# State Space Model forecast
#
# Author: Elisa Jorge González <elisajg0@gmail.com>

#' @title Get forecast
#' @description Executes the Basic Structural Model (BSM) with/without the inclusion of exogenous variables and seasonal
#' @param X Times series or set of time series to be forecast  
#' @param Y Exogenous variables
#' @param n Number of available data
#' @param n_f Number of data to forecast
#' @param freq Frequency of the data
#' @return mylist List of forecast of a time series or set of time series and the value of MAPE for the estimations  
#' @example 
#   get_forecast(X = myseries, Y = myexogenous_va, n = 72, n_f = 6, freq = 12)

get_forecast <- function(X, Y = NULL, n, n_f, freq){
  
  # TIME SERIES STRUCTURE
  # Data to forecast
  X_ts <- as.data.frame(ts(X[,3:ncol(X)], start = c(X[1,1], X[1,2]), frequency = freq))
  X_ts <- ts(rbind(as.matrix(X[,3:ncol(X)]), 
                   matrix(NA, ncol = ncol(X) - 2, nrow = n_f)), 
             start = c(X[1,1], X[1,2]), 
             frequency = freq)
  
  # CREATION OF OUTPUT VARIABLES
  # Variable for MAPE values
  MAPE <- matrix(NA, ncol = ncol(X_ts), nrow = 1)
  # Variable for forecast values 
  Prediction <- matrix(0, ncol = ncol(X_ts), nrow = nrow(X_ts))
  
  if (!is.null(Y))
  {
    # TIME SERIES STRUCTURE
    # Exogenous variables
    Y_ts <- ts(Y[,2:ncol(Y)], 
               start = c(X[1,1], X[1,2]), 
               end = time(X_ts)[nrow(X_ts)], 
               frequency = freq)
    
    # Loop to execute BSM with the inclusion of exogenous variables and seasonal
    if (ncol(X_ts) > 1)
    {
      for (i in 1:ncol(X_ts))
      {
        ssKi <- SSModel(log(as.numeric(X_ts[,i])) ~ SSMtrend(degree = 2, Q = list(matrix(NA), matrix(NA)))+
                                                    SSMseasonal(freq, Q = matrix(NA))+
                                                    SSMregression(~Y_ts, Q = matrix(0)),
                        H = matrix(NA))
        fitSSKI <- fitSSM(ssKi, inits = c(0,0,0,0), method = "BFGS")
        out <- KFS(fitSSKI$model, filtering = "state")
        dfSSKSIStates <- ts(out$alphahat, start = time(X_ts)[1], frequency = freq)
      
        for (j in 1:ncol(Y_ts))
        {Prediction[,i] <- Prediction[,i] + dfSSKSIStates[,j]*Y_ts[,j]}
        
        Prediction[,i] <- Prediction[,i] + dfSSKSIStates[, ncol(Y_ts) + 1] + dfSSKSIStates[, ncol(Y_ts) + 3] 
        Prediction[,i] <- exp(Prediction[,i])
      
        MAPE[,i] <- (mean(abs((Prediction[1:n,i]) - as.numeric(X_ts[1:n,i]))/as.numeric(X_ts[1:n,i])))*100
      }
    }
    else
    {
      ssKi <- SSModel(log(X_ts) ~ SSMtrend(degree = 2, Q = list(matrix(NA), matrix(NA)))+
                                  SSMseasonal(freq, Q = matrix(NA))+
                                  SSMregression(~Y_ts, Q = matrix(0)),
                      H = matrix(NA))
      fitSSKI <- fitSSM(ssKi, inits = c(0,0,0,0), method = "BFGS")
      out <- KFS(fitSSKI$model, filtering = "state")
      dfSSKSIStates <- ts(out$alphahat, start = time(X_ts)[1], frequency = freq)
      
      for (j in 1:ncol(Y_ts))
      {Prediction <- Prediction + dfSSKSIStates[,j]*Y_ts[,j]}
      
      Prediction <- Prediction + dfSSKSIStates[, ncol(Y_ts) + 1] + dfSSKSIStates[, ncol(Y_ts) + 3] 
      Prediction <- exp(Prediction)
      
      MAPE <- (mean(abs(Prediction[1:n] - as.numeric(X_ts[1:n]))/as.numeric(X_ts[1:n])))*100
    }
  }
  else
  {
    # Loop to execute BSM without the inclusion of exogenous variables and seasonal
    if (ncol(X_ts) > 1)
    {
      for (i in 1:ncol(X_ts))
      {
        ssKi <- SSModel(log(as.numeric(X_ts[,i])) ~ SSMtrend(degree = 2, Q = list(matrix(NA), matrix(NA)))+
                                                    SSMseasonal(freq, Q = matrix(NA)),
                        H = matrix(NA))
        fitSSKI <- fitSSM(ssKi, inits = c(0,0,0,0), method = "BFGS")
        out <- KFS(fitSSKI$model, filtering = "state")
        dfSSKSIStates <- ts(out$alphahat, start = time(X_ts)[1], frequency = freq)
      
        Prediction[,i] <- dfSSKSIStates[,1] + dfSSKSIStates[,3] 
        Prediction[,i] <- exp(Prediction[,i])
      
        MAPE[,i] <- (mean(abs((Prediction[1:n,i]) - as.numeric(X_ts[1:n,i]))/as.numeric(X_ts[1:n,i])))*100
      }
    }
    else
    {
      ssKi <- SSModel(log(X_ts) ~ SSMtrend(degree = 2, Q = list(matrix(NA), matrix(NA)))+
                                  SSMseasonal(freq, Q = matrix(NA)),
                      H = matrix(NA))
      fitSSKI <- fitSSM(ssKi, inits = c(0,0,0,0), method = "BFGS")
      out <- KFS(fitSSKI$model, filtering = "state")
      dfSSKSIStates <- ts(out$alphahat, start = time(X_ts)[1], frequency = freq)
      
      Prediction <- dfSSKSIStates[,1] + dfSSKSIStates[,3] 
      Prediction <- exp(Prediction)
      
      MAPE <- (mean(abs(Prediction[1:n] - as.numeric(X_ts[1:n]))/as.numeric(X_ts[1:n])))*100
    }
  }
  
  # Date of forecasts
  Date_f   <- as.character(as_Date_ts(X_ts[,1])[(nrow(X) + 1):nrow(X_ts)])
  
  mylist <- list(MAPE, Prediction, Date_f)
  names(mylist) <- c("MAPE", "Prediction", "Date_f")
  
  return(mylist)
  
}
