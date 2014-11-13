#backtester.r

Backtest_ClassifierAlgo <- function(inp_params) {
  
  # Perform a backtest of a specified classifier type using specified backtest parameters.
  #
  # Calls: Get_XData, Extract_CrossTrainingXData, Fit_Classifier, Extract_CrossPredictionXData
  #        Get_ClassifierPrediction
  #
  # Inputs:
  #     inp_params: list: backtest parameters. Note: the list contains the following items:
  #                 Model_XDataFile, XData_To_Use, Fit_Window, Rolling_Window_Performance
  #                 Prediction_Adjust_Factor, AdjustPredictedPositions,
  #                 Classifier_Type, Refit_Classifier_Periodicity, Start_AUM, Reinvest_Daily
  #
  # Returns: backtest results object (a list) containing the following items:
  #             Backtest_Parameters, XData, Cross_Classifiers, Cross_Predictions
  #             Cross_Predictions_Accuracy, Cross_Predictions_Accuracy_Rolling
  #             Currency_Targets, Currency_Pnls, Currency_Returns, Currency_Turnovers
  
  #inp_params <- my_tmp_test_params
  
  outp_results <- list()  # List of backtest results and various calculated objects
  classifiers_per_cross <- list()  # List object to hold trained classifiers for each cross, at each time (if using re-fitting)
  
  # Add the backtest parameters list to the output list for user information
  outp_results$Backtest_Parameters <- inp_params
  
  # Import the XData file
  # ---
  Logger("Importing XData...")
  model_xdata <- Get_XData(inp_params$Model_XDataFile)
  outp_results$XData <- model_xdata
  # ---
  
  
  # Perform the initial classifier model fitting
  # ---
  Logger("Performing initial classifier training...")
  tmp_fit_date <- model_xdata$Datetime[inp_params$Fit_Window]
  for (i_cross in config$Crosses) {
    inp_params$Cross <- i_cross
    tmp_training_data <- Extract_CrossTrainingXData(model_xdata, inp_params, inp_params$Fit_Window)
    classifiers_per_cross[[i_cross]][[tmp_fit_date]] <- Fit_Classifier(tmp_training_data, inp_params)
  }
  # ---
  
  
  # Iterate through all data
  # ---
  Logger("Beginning backtest...")
  
  # Initialise some data.frames to store data throughout iterations 
  tmp_predictions_raw <- data.frame(matrix(NA, nrow(model_xdata), length(config$Crosses), dimnames=list(model_xdata$Datetime, config$Crosses)))
  tmp_predictions_adj <- data.frame(matrix(NA, nrow(model_xdata), length(config$Crosses), dimnames=list(model_xdata$Datetime, config$Crosses)))
  tmp_predictions_accuracy <- data.frame(matrix(NA, nrow(model_xdata), length(config$Crosses), dimnames=list(model_xdata$Datetime, config$Crosses)))
  tmp_predictions_accuracy_rolling <- data.frame(matrix(NA, nrow(model_xdata), length(config$Crosses), dimnames=list(model_xdata$Datetime, config$Crosses)))
  tmp_currency_targets <- data.frame(matrix(NA, nrow(model_xdata), length(config$Currencies), dimnames=list(model_xdata$Datetime, config$Currencies)))
  tmp_currency_pnls <- data.frame(matrix(NA, nrow(model_xdata), length(config$Currencies) + 1, dimnames=list(model_xdata$Datetime, c(config$Currencies, "Aggregate"))))
  tmp_currency_returns <- data.frame(matrix(NA, nrow(model_xdata), length(config$Currencies) + 1, dimnames=list(model_xdata$Datetime, c(config$Currencies, "Aggregate"))))
  tmp_daily_start_aum <- rep(inp_params$Start_AUM, nrow(model_xdata))
  
  # Extract concise array of cross returns for ease of use in backtester
  cross_returns_array <- model_xdata[, c(1, match(paste(config$Crosses, ".1_Return", sep=""), names(model_xdata)))]  # Array of cross returns
  colnames(cross_returns_array) <- c("Datetime", config$Crosses)  # Put better headers (excluding the .1_Return bit)
  
  # Begin iterations through model_xdata (beyond initial classifier training data range), generating targets
  time_since_fit <- 0
  for (i in (inp_params$Fit_Window + 1):nrow(model_xdata)) {
  
    # Progress print at set intervals if specified by inp_params
    # ---
    if ((inp_params$Show_Backtest_Progress) && (i %% 50 == 0))
      Logger(paste("On date ", i, " of ", nrow(model_xdata), " ... ", sep=""))
    # ---
    
    # Section here to do something special like re-fit a model if certain criteria met
    # ---
    if (!is.na(inp_params$Refit_Classifier_Periodicity)) {  # Refit the classifier models here
      # This should all live in a separate method, just to be called now
      if (time_since_fit == inp_params$Refit_Classifier_Periodicity) {
        Logger("Re-training all classifiers...")
        tmp_fit_date <- model_xdata$Datetime[i]
        for (k_cross in config$Crosses) {
          inp_params$Cross <- k_cross
          tmp_training_data <- Extract_CrossTrainingXData(model_xdata, inp_params, i)
          classifiers_per_cross[[k_cross]][[tmp_fit_date]] <- Fit_Classifier(tmp_training_data, inp_params)
        }
        time_since_fit <- 0
      }
    }
    # ---
    
    ## Alternatively, the above re-training procedure can be based on some cross-specific performance stats, and some classifiers can be re-trained independently of others
    ## ---
    #tmp_fit_date <- model_xdata$Datetime[i]
    #for (i_cross in config$Crosses) {
    #  if (TRUE) {  # Some criteria for re-training the classifier
    #    Logger(paste("Re-training classifier:", i_cross, sep=" "))
    #    inp_params$Cross <- i_cross
    #    tmp_training_data <- Extract_CrossTrainingXData(model_xdata, inp_params, i)
    #    classifiers_per_cross[[i_cross]][[tmp_fit_date]] <- Fit_Classifier(tmp_training_data, inp_params)
    #  }
    #}
    ## ---

    # Classify next day returns
    # ---
    for (j in seq_along(config$Crosses)) {
      tmp_cross <- config$Crosses[j]
      
      # Get today data for classifier for this cross
      inp_params$Cross <- tmp_cross
      tmp_today_data <- Extract_CrossPredictionXData(model_xdata[i, ], inp_params)  # Current day xdata
      
      # Get prediction and record the result as a 1 (buy) or -1 (sell)
      tmp_today_predictions <- Get_ClassifierPrediction(classifiers_per_cross[[tmp_cross]][[length(classifiers_per_cross[[tmp_cross]])]], tmp_today_data, inp_params)   # Use latest classifier here (to take in to account any re-fitting done)
      if (tmp_today_predictions$class == "B") {
        tmp_predictions_raw[i, j] <- 1
        if (!inp_params$AdjustPredictedPositions) {  # Not adjusting predictions, so write raw value
          tmp_predictions_adj[i, j] <- 1
        }
      } else if (tmp_today_predictions$class == "S") {
        tmp_predictions_raw[i, j] <- -1
        if (!inp_params$AdjustPredictedPositions) {  # Not adjusting predictions, so write raw value
          tmp_predictions_adj[i, j] <- -1
        }
      }
        
      # Record whether this was a correct prediction using next day cross return (if not already last row)
      if (i < nrow(model_xdata))
        tmp_predictions_accuracy[i, j] <- as.numeric(sign(tmp_predictions_raw[i, j]) == sign(cross_returns_array[[tmp_cross]][i + 1]))
      
      # Update rolling stats if enough observations
      if (i > (inp_params$Fit_Window + inp_params$Rolling_Window_Performance)) {  # enough obs to get rolling accuracy window
        tmp <- tmp_predictions_accuracy[(i - inp_params$Rolling_Window_Performance):(i - 1), j]  # Window of prediction accuracies for this cross
        tmp_predictions_accuracy_rolling[i, j] <- sum(tmp == 1) / inp_params$Rolling_Window_Performance  # Rolling accuracy ratio        
        
        # If using adjusted predictions, now use these rolling stats to help adjust prediction value
        if (inp_params$AdjustPredictedPositions) {
          tmp_predictions_adj[i, j] <- tmp_predictions_raw[i, j] * (1 + inp_params$Prediction_Adjust_Factor * (tmp_predictions_accuracy_rolling[i, j] - 0.5) / 0.5)
        }
        
      }
    }
    # ---
    
    # Aggregate predictions to get currency level targets
    # ---
    for (j in seq_along(config$Currencies)) {  
      tmp_ccy <- config$Currencies[j]  # This currency
      
      tmp_cxs_as_base <- config$Crosses[grep(tmp_ccy, substr(config$Crosses, 1, 3))]  # Crosses with the target ccy as base
      tmp_cxs_as_quote <- config$Crosses[grep(tmp_ccy, substr(config$Crosses, 4, 6))]  # Crosses with the target ccy as quote
      
      tmp_pos_as_base <- tmp_pos_as_quote <- 0  # Default values at 0
      if (!all(is.na(tmp_predictions_adj[tmp_cxs_as_base][i, ])))
        tmp_pos_as_base <- sum(tmp_predictions_adj[tmp_cxs_as_base][i, ], na.rm=TRUE)  # Sum of predictions where currency is base
      if (!all(is.na(tmp_predictions_adj[tmp_cxs_as_quote][i, ])))
        tmp_pos_as_quote <- sum(tmp_predictions_adj[tmp_cxs_as_quote][i, ], na.rm=TRUE)  # Sum of predictions where currency is quote
      
      tmp_currency_targets[i, j] <- tmp_pos_as_base - tmp_pos_as_quote  # Total aggregated currency holding ("as quote" sum contributes as negative)
    }
    
    # Now convert currency targets row to USD values based on starting daily aum
    tmp_currency_targets[i, ] <- tmp_daily_start_aum[i] * (tmp_currency_targets[i, ] / sum(abs(tmp_currency_targets[i, ])))
    # ---
    
    # Get resulting pnls and returns from the target currency holdings
    # ---
    if (i < nrow(model_xdata)) {
      for (j in seq_along(config$Currencies)) {
        
        tmp_ccy <- config$Currencies[j]
        if (tmp_ccy == "USD") {
          tmp_currency_pnls[i, j] <- 0
          tmp_currency_returns[i, j] <- 0
        } else if (tmp_ccy %in% c("EUR", "GBP", "AUD", "NZD")) {
          tmp_currency_pnls[i, j] <- cross_returns_array[[config$Currencies_To_USD[[tmp_ccy]]]][i + 1] * tmp_currency_targets[i, j]
          tmp_currency_returns[i, j] <- tmp_currency_pnls[i, j] / abs(tmp_currency_targets[i, j])
        } else {
          tmp_currency_pnls[i, j] <- cross_returns_array[[config$Currencies_To_USD[[tmp_ccy]]]][i + 1] * tmp_currency_targets[i, j] * -1
          tmp_currency_returns[i, j] <- tmp_currency_pnls[i, j] / abs(tmp_currency_targets[i, j])
        }
        
      }
      
      # Add an aggregate pnl and return column to end of each array
      tmp_currency_pnls[i, j + 1] <- sum(tmp_currency_pnls[i, ], na.rm=TRUE)
      tmp_currency_returns[i, j + 1] <- tmp_currency_pnls[i, j + 1] / tmp_daily_start_aum[i]
    }
    # ---
    
    # Update the daily starting aum vector
    if (inp_params$Reinvest_Daily) {  # Using daily reinvestment... add current day pnl to last aum number
      tmp_daily_start_aum[i + 1] <- tmp_daily_start_aum[i] + tmp_currency_pnls[i, j + 1]
    }
      
    time_since_fit <- time_since_fit + 1
    
  }
  Logger("Done!", inp_new_line = FALSE)
  
  # Create a dataframe of target position turnover data on the currency level
  tmp_currency_turnovers <- tmp_currency_targets[-1, ] - tmp_currency_targets[-nrow(tmp_currency_targets), ]

  # Add the fitted classifiers object to return list for user information
  outp_results$Cross_Classifiers <- classifiers_per_cross
  
  # Add some of the data arrays to the return list for user
  outp_results$Cross_Predictions <- tmp_predictions_adj
  outp_results$Cross_Predictions_Accuracy <- tmp_predictions_accuracy
  outp_results$Cross_Predictions_Accuracy_Rolling <- tmp_predictions_accuracy_rolling
  outp_results$Currency_Targets <- tmp_currency_targets
  outp_results$Currency_Pnls <- tmp_currency_pnls
  outp_results$Currency_Returns <- tmp_currency_returns
  outp_results$Currency_Turnovers <- tmp_currency_turnovers
  outp_results$Daily_Opening_AUMs <- tmp_daily_start_aum
  
  return (outp_results)
  
}




