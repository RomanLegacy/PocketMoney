#backtester.r

Backtest_ClassifierAlgo <- function(inp_params) {
  # Perform a backtest of a specified classifier type using specified backtest parameters.
  # Parameter list must contain the following objects:
  # Model_XDataFile, XData_To_Use, Fit_Window, Rolling_Window_Performance, Prediction_Adjust_Factor, AdjustPredictedPositions
  # Classifier_Type, Refit_Classifier_Periodicity
  
#   inp_params <- my_params
#   inp_params <- list("Model_XDataFile" = "Data/SmallTestFile.csv",
#                      "XData_To_Use" = c("Returns", "PC1", "PC2"),
#                      "Fit_Window" = 250,
#                      "Rolling_Window_Performance" = 10,
#                      "Prediction_Adjust_Factor" = 0.5,
#                      "AdjustPredictedPositions" = TRUE,
#                      "Classifier_Type" = "lda",
#                      "Refit_Classifier_Periodicity" = NA)
  
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
  Logger("Fitting initial classifier models...")
  tmp_fit_date <- model_xdata$Datetime[inp_params$Fit_Window]
  for (i in seq_along(config$Crosses)) {
    cat("Currently on cross:", config$Crosses[i], "\n")
    inp_params$Cross <- config$Crosses[i]
    tmp_training_data <- head(model_xdata, inp_params$Fit_Window)
    tmp_data <- Get_SubsetData_ForClassification(inp_data = tmp_training_data, inp_params = inp_params)
    classifiers_per_cross[[config$Crosses[i]]][[tmp_fit_date]] <- Fit_Classifier(inp_data = tmp_data, inp_params = inp_params)
  }
  # ---
  
  
  # Iterate through all data
  # ---
  Logger("Performing backtest...")
  
  # Initialise some data.frames to store data throughout iterations 
  tmp_predictions_raw <- data.frame(matrix(NA, nrow(model_xdata), length(config$Crosses), dimnames=list(model_xdata$Datetime, config$Crosses)))
  tmp_predictions_adj <- data.frame(matrix(NA, nrow(model_xdata), length(config$Crosses), dimnames=list(model_xdata$Datetime, config$Crosses)))
  tmp_predictions_accuracy <- data.frame(matrix(NA, nrow(model_xdata), length(config$Crosses), dimnames=list(model_xdata$Datetime, config$Crosses)))
  tmp_predictions_accuracy_rolling <- data.frame(matrix(NA, nrow(model_xdata), length(config$Crosses), dimnames=list(model_xdata$Datetime, config$Crosses)))
  tmp_currency_targets <- data.frame(matrix(NA, nrow(model_xdata), length(config$Currencies), dimnames=list(model_xdata$Datetime, config$Currencies)))
  tmp_currency_returns <- data.frame(matrix(NA, nrow(model_xdata), length(config$Currencies), dimnames=list(model_xdata$Datetime, config$Currencies)))
  
  # Extract concise array of cross returns for ease of use in backtester
  cross_returns_array <- model_xdata[, c(1, match(paste(config$Crosses, ".1_Return", sep=""), names(model_xdata)))]  # Array of cross returns
  colnames(cross_returns_array) <- c("Datetime", config$Crosses)  # Put better headers (excluding the .1_Return bit)
  
  # Begin iterations through model_xdata (beyond initial classifier training data range), generating targets
  time_since_fit <- 0
  #for (i in (inp_params$Fit_Window + 1):nrow(model_xdata)) {
  for (i in (inp_params$Fit_Window + 1):(inp_params$Fit_Window + 30)) { 
    # Progress print at set intervals
    # ---
    if (i %% 50 == 0)
      Logger(paste("On date ", i, " of ", nrow(model_xdata), " ... ", sep=""))
    # ---
    
    # Section here to do something special like re-fit a model if certain criteria met
    # ---
    if (!is.na(inp_params$Refit_Classifier_Periodicity)) {  # Refit the classifier models here
      # This should all live in a separate method, just to be called now
      if (time_since_fit == inp_params$Refit_Classifier_Periodicity) {
        Logger("Re-fitting classifier models...")
        tmp_fit_date <- model_xdata$Datetime[i]
        for (k in seq_along(config$Crosses)) {
          cat("Currently on cross:", config$Crosses[k], "\n")
          inp_params$Cross <- config$Crosses[k]
          tmp_training_data <- model_xdata[(i - inp_params$Fit_Window):(i - 1), ]
          tmp_data <- Get_SubsetData_ForClassification(inp_data = tmp_training_data, inp_params = inp_params)
          classifiers_per_cross[[config$Crosses[k]]][[tmp_fit_date]] <- Fit_Classifier(inp_data = tmp_data, inp_params = inp_params)
        }
        time_since_fit <- 0
      }
    }
    # ---
    
    # Use xdata to classify the next day return
    # ---
    for (j in seq_along(config$Crosses)) {
      tmp_cross <- config$Crosses[j]
      
      # Get today data for classifier for this cross
      inp_params$Cross <- tmp_cross
      tmp_today_data <- Get_SubsetData_ForPrediction(inp_data = model_xdata[i, ], inp_params = inp_params)  # Current day xdata
      
      # Get prediction and record the result as a 1 (buy) or -1 (sell)
      tmp_today_predictions <- Get_ClassifierPrediction(classifiers_per_cross[[tmp_cross]][[length(classifiers_per_cross[[tmp_cross]])]], tmp_today_data, inp_params)   # Use latest classifier here (to take in to account any re-fitting done)
      if (tmp_today_predictions$class == "B")
        tmp_predictions_raw[i, j] <- 1
      else if (tmp_today_predictions$class == "S")
        tmp_predictions_raw[i, j] <- -1
      
      # Record whether this was a correct prediction using next day cross return (if not already last row)
      if (i < nrow(model_xdata))
        tmp_predictions_accuracy[i, j] <- as.numeric(sign(tmp_predictions_raw[i, j]) == sign(cross_returns_array[[tmp_cross]][i + 1]))
      
      # Calculate the rolling window prediction accuracy ratio (if sufficient predictions made) and create a performance-adjusted prediction 
      if (i > (inp_params$Fit_Window + inp_params$Rolling_Window_Performance)) {
        tmp <- tmp_predictions_accuracy[(i - inp_params$Rolling_Window_Performance):(i - 1), j]  # Window of prediction accuracies for this cross
        tmp_predictions_accuracy[i, j] <- sum(tmp == 1) / inp_params$Rolling_Window_Performance  # Accuracy ratio      
        tmp_predictions_adj[i, j] <- tmp_predictions_raw[i, j] * (1 + inp_params$Prediction_Adjust_Factor * (tmp_predictions_accuracy[i, j] - 0.5) / 0.5)
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
    # ---
    
    # Get resulting return from the target currency holdings
    # ---
    if (i < nrow(model_xdata)) {
      for (j in seq_along(config$Currencies)) {
        
        tmp_ccy <- config$Currencies[j]
        if (tmp_ccy == "USD") {
          tmp_currency_returns[i, j] <- 0
        } else if (tmp_ccy %in% c("EUR", "GBP", "AUD", "NZD")) {
          tmp_currency_returns[i, j] <- cross_returns_array[[config$Currencies_To_USD[[tmp_ccy]]]][i + 1] * tmp_currency_targets[i, j]
        } else {
          tmp_currency_returns[i, j] <- cross_returns_array[[config$Currencies_To_USD[[tmp_ccy]]]][i + 1] * tmp_currency_targets[i, j] * -1
        }
        
      }
    }
    # ---
    
    time_since_fit <- time_since_fit + 1
    
  }
  Logger("Done!", inp_new_line = FALSE)
  
  # Add the fitted classifiers object to return list for user information
  outp_results$Cross_Classifiers <- classifiers_per_cross
  
  # Add some of the data arrays to the return list for user
  outp_results$Cross_Predictions <- tmp_predictions_adj
  outp_results$Cross_Predictions_Accuracy <- tmp_predictions_accuracy
  outp_results$Cross_Predictions_Accuracy_Rolling <- tmp_predictions_accuracy_rolling
  outp_results$Currency_Targets <- tmp_currency_targets
  outp_results$Currency_Returns <- tmp_currency_returns
  
  return (outp_results)
  
}



# Yet to go through the stuff below here.. should be able to remove it all after finished with above method...

Backtest_ClassifierAlgo_WithRefits <- function(inp_params) {
  # Perform a backtest of a specified classifier
  #inp_params <- my_params
  outp_results <- list()  # List of backtest results and various calculated objects
  
  # Import the XData file
  # ---
  Logger("Importing XData...")
  model_xdata <- Get_XData(inp_params$Model_XDataFile)
  outp_results$XData <- model_xdata
  # ---
  
  
  # Perform the initial classifier model fitting
  # ---
  Logger("Fitting classifier models...")
  classifiers_per_cross <- list()
  for (i in seq_along(config$Crosses)) {
    cat("Currently on cross:", config$Crosses[i], "\n")
    inp_params$Cross <- config$Crosses[i]
    tmp_training_data <- head(model_xdata, inp_params$Fit_Window)
    tmp_data <- Get_SubsetData_ForClassification(inp_data = tmp_training_data, inp_params = inp_params)
    classifiers_per_cross[[config$Crosses[i]]] <- Fit_Classifier(inp_data = tmp_data, inp_params = inp_params)
  }
  outp_results$Cross_Classifiers <- classifiers_per_cross
  # ---
  
  
  # Iterate through all data
  # ---
  Logger("Performing backtest...")
  
  # Define all arrays of output information that we will generate during backtest
  outp_predictions_raw <- matrix(NA, nrow(model_xdata), length(config$Crosses))  # Raw cross classifications
  outp_predictions_adj <- matrix(NA, nrow(model_xdata), length(config$Crosses))  # Adjusted cross classifications
  outp_predictions_accuracy <- matrix(NA, nrow(model_xdata), length(config$Crosses))  # Rolling prediction accuracy ratios
  outp_rolling_accuracy <- matrix(NA, nrow(model_xdata), length(config$Crosses))  # Rolling prediction accuracy ratios
  outp_currency_targets_raw <- matrix(NA, nrow(model_xdata), length(config$Currencies))  # Currency targets (aggregation of raw cross predictions)
  outp_currency_targets_adj <- matrix(NA, nrow(model_xdata), length(config$Currencies))  # Currency targets (aggregation of adjusted cross predictions)
  outp_aggregated_currency_returns_raw <- matrix(NA, nrow(model_xdata), length(config$Currencies))  # Returns from the raw target currency positions
  outp_aggregated_currency_returns_adj <- matrix(NA, nrow(model_xdata), length(config$Currencies))  # Returns from the adjusted target currency positions
  
  # Extract concise array of cross returns for ease of use in backtester
  cross_returns_array <- model_xdata[, c(1, match(paste(config$Crosses, ".1_Return", sep=""), names(model_xdata)))]  # Array of cross returns
  colnames(cross_returns_array) <- c("Datetime", config$Crosses)  # Put better headers (excluding the .1_Return bit)
  
  # Begin iterations through model_xdata (beyond initial classifier training data range), generating targets
  time_to_refit <- inp_params$Classifier_Refit_Period
  for (i in (inp_params$Fit_Window + 1):nrow(model_xdata)) {
    if (i %% 50 == 0)
      Logger(paste("On date ", i, " of ", nrow(model_xdata), " ... ", sep=""))
    
    
    # Perform a re-fitting of the classifiers per cross if time_to_refit has reached 0
    # ---
    if (time_to_refit == 0) {
      Logger("Re-fitting classifier models...")
      classifiers_per_cross <- list()
      for (k in seq_along(config$Crosses)) {
        cat("Currently on cross:", config$Crosses[k], "\n")
        inp_params$Cross <- config$Crosses[k]
        tmp_training_data <- model_xdata[(i - inp_params$Fit_Window):(i - 1), ]
        tmp_data <- Get_SubsetData_ForClassification(inp_data = tmp_training_data, inp_params = inp_params)
        classifiers_per_cross[[config$Crosses[k]]] <- Fit_Classifier(inp_data = tmp_data, inp_params = inp_params)
      }
      outp_results$Cross_Classifiers <- classifiers_per_cross
      time_to_refit <- inp_params$Classifier_Refit_Period
    }
    # ---
    
    # Get cross prediction/classifications for next day returns using current day xdata
    for (j in seq_along(config$Crosses)) {
      tmp_cross <- config$Crosses[j]
      
      # Get today data for classifier for this cross
      inp_params$Cross <- tmp_cross
      tmp_today_data <- Get_SubsetData_ForPrediction(inp_data = model_xdata[i, ], inp_params = inp_params)  # Current day xdata
      
      # Get prediction and record the result as a 1 (buy) or -1 (sell)
      tmp_today_predictions <- Get_ClassifierPrediction(classifiers_per_cross[[tmp_cross]], tmp_today_data, inp_params) 
      if (tmp_today_predictions$class == "B")
        outp_predictions_raw[i, j] <- 1
      else if (tmp_today_predictions$class == "S")
        outp_predictions_raw[i, j] <- -1
      
      # Record whether this was a correct prediction using next day cross return (if not already last row)
      if (i < nrow(model_xdata))
        outp_predictions_accuracy[i, j] <- as.numeric(sign(outp_predictions_raw[i, j]) == sign(cross_returns_array[[tmp_cross]][i + 1]))
      
      # Calculate the rolling window prediction accuracy ratio (if sufficient predictions made) and create a performance-adjusted prediction 
      if (i > (inp_params$Fit_Window + inp_params$Rolling_Window_Performance)) {
        tmp <- outp_predictions_accuracy[(i - inp_params$Rolling_Window_Performance):(i - 1), j]  # Window of prediction accuracies for this cross
        outp_rolling_accuracy[i, j] <- sum(tmp == 1) / inp_params$Rolling_Window_Performance  # Accuracy ratio      
        outp_predictions_adj[i, j] <- outp_predictions_raw[i, j] * (1 + inp_params$Prediction_Adjust_Factor * (outp_rolling_accuracy[i, j] - 0.5) / 0.5)
      }
      
    }
    
    # Aggregate all predictions to get currency level targets
    for (j in seq_along(config$Currencies)) {
      tmp_ccy <- config$Currencies[j]  # This currency
      tmp_cols_as_base <- grep(tmp_ccy, substr(config$Crosses, 1, 3))  # Prediction columns where this currency is base ccy
      tmp_cols_as_quote <- grep(tmp_ccy, substr(config$Crosses, 4, 6))  # Prediction columns where this currency is quote ccy
      
      # Get sum of preds where ccy appears as base currency 
      tmp_pos_as_base_raw <- sum(outp_predictions_raw[i, tmp_cols_as_base])  # (raw preds)
      tmp_pos_as_base_adj <- sum(outp_predictions_adj[i, tmp_cols_as_base])  # (adj preds)
      
      # Get sum of preds where ccy appears as quote currency 
      tmp_pos_as_quote_raw <- sum(outp_predictions_raw[i, tmp_cols_as_quote])  # Row sums from where this currency appears as quote currency in cross (raw preds)
      tmp_pos_as_quote_adj <- sum(outp_predictions_adj[i, tmp_cols_as_quote])  # Row sums from where this currency appears as quote currency in cross (adj preds)
      
      # Get total position from base and subtract total where appear as quote
      outp_currency_targets_raw[i, j] <- tmp_pos_as_base_raw - tmp_pos_as_quote_raw  # Total aggregated currency holding, add "from base" contributions and subtract "from quote" contributions (RAW PREDS)
      outp_currency_targets_adj[i, j] <- tmp_pos_as_base_adj - tmp_pos_as_quote_adj  # Total aggregated currency holding, add "from base" contributions and subtract "from quote" contributions (ADJ PREDS)
      
    }
    
    # Get resulting return from the target currency holdings
    if (i < nrow(model_xdata)) {
      for (j in seq_along(config$Currencies)) {
        
        tmp_ccy <- config$Currencies[j]
        if (tmp_ccy == "USD") {
          outp_aggregated_currency_returns_raw[i, j] <- 0
          outp_aggregated_currency_returns_adj[i, j] <- 0
        } else if (tmp_ccy %in% c("EUR", "GBP", "AUD", "NZD")) {
          outp_aggregated_currency_returns_raw[i, j] <- cross_returns_array[[config$Currencies_To_USD[[tmp_ccy]]]][i + 1] * outp_currency_targets_raw[i, j]
          outp_aggregated_currency_returns_adj[i, j] <- cross_returns_array[[config$Currencies_To_USD[[tmp_ccy]]]][i + 1] * outp_currency_targets_adj[i, j]
        } else {
          outp_aggregated_currency_returns_raw[i, j] <- cross_returns_array[[config$Currencies_To_USD[[tmp_ccy]]]][i + 1] * outp_currency_targets_raw[i, j] * -1
          outp_aggregated_currency_returns_adj[i, j] <- cross_returns_array[[config$Currencies_To_USD[[tmp_ccy]]]][i + 1] * outp_currency_targets_adj[i, j] * -1
        }
        
      }
    }
    
    time_to_refit <- time_to_refit - 1
    
  }
  Logger("Done!", inp_new_line = FALSE)
  
  # Convert output arrays to dataframes and add to return list
  # Predictions_raw
  outp_predictions_raw <- data.frame(outp_predictions_raw)
  colnames(outp_predictions_raw) <- config$Crosses; rownames(outp_predictions_raw) <- model_xdata[, 1]
  outp_results$Predictions_Raw <- outp_predictions_raw
  # Predictions_adj
  outp_predictions_adj <- data.frame(outp_predictions_adj)
  colnames(outp_predictions_adj) <- config$Crosses; rownames(outp_predictions_adj) <- model_xdata[, 1]
  outp_results$Predictions_Adjusted <- outp_predictions_adj
  # Predictions_accuracy
  outp_predictions_accuracy <- data.frame(outp_predictions_accuracy)
  colnames(outp_predictions_accuracy) <- config$Crosses; rownames(outp_predictions_accuracy) <- model_xdata[, 1]
  outp_results$Predictions_Accuracy <- outp_predictions_accuracy
  # Predictions_rolling_accuracy
  outp_rolling_accuracy <- data.frame(outp_rolling_accuracy)
  colnames(outp_rolling_accuracy) <- config$Crosses; rownames(outp_rolling_accuracy) <- model_xdata[, 1]
  outp_results$Predictions_Rolling_Accuracy <- outp_rolling_accuracy
  # Currency_targets_raw
  outp_currency_targets_raw <- data.frame(outp_currency_targets_raw)
  colnames(outp_currency_targets_raw) <- config$Currencies; rownames(outp_currency_targets_raw) <- model_xdata[, 1]
  outp_results$Currency_Targets_Raw <- outp_currency_targets_raw
  # Currency_targets_adj
  outp_currency_targets_adj <- data.frame(outp_currency_targets_adj)
  colnames(outp_currency_targets_adj) <- config$Currencies; rownames(outp_currency_targets_adj) <- model_xdata[, 1]
  outp_results$Currency_Targets_Adjusted <- outp_currency_targets_adj
  # Currency_target_returns_raw
  outp_aggregated_currency_returns_raw <- data.frame(outp_aggregated_currency_returns_raw)
  colnames(outp_aggregated_currency_returns_raw) <- config$Currencies; rownames(outp_aggregated_currency_returns_raw) <- model_xdata[, 1]
  outp_results$Currency_Target_Returns_Raw <- outp_aggregated_currency_returns_raw
  # Currency_target_returns_adj
  outp_aggregated_currency_returns_adj <- data.frame(outp_aggregated_currency_returns_adj)
  colnames(outp_aggregated_currency_returns_adj) <- config$Currencies; rownames(outp_aggregated_currency_returns_adj) <- model_xdata[, 1]
  outp_results$Currency_Target_Returns_Adjusted <- outp_aggregated_currency_returns_adj
  
  return (outp_results)
  
}

Backtest_ClassifierAlgo_CrossSpecificClassifiers <- function(inp_params, inp_params_cross_specific_regression) {
  # Perform a backtest of a specified classifier
  #inp_params <- my_params
  outp_results <- list()  # List of backtest results and various calculated objects
  
  # Import the XData file
  # ---
  Logger("Importing XData...")
  model_xdata <- Get_XData(inp_params$Model_XDataFile)
  outp_results$XData <- model_xdata
  # ---
  
  
  # Perform the initial classifier model fitting
  # ---
  Logger("Fitting classifier models...")
  classifiers_per_cross <- list()
  for (i in seq_along(config$Crosses)) {
    cat("Currently on cross:", config$Crosses[i], "\n")
    inp_params$Cross <- config$Crosses[i]
    inp_params$XData_To_Use <- inp_params_cross_specific_regression[[config$Crosses[i]]]$XData_To_Use
    inp_params$Classifier_Type <- inp_params_cross_specific_regression[[config$Crosses[i]]]$Classifier_Type
    inp_params$Fit_Window <- inp_params_cross_specific_regression[[config$Crosses[i]]]$Fit_Window
    tmp_training_data <- head(model_xdata, inp_params$Fit_Window)
    tmp_data <- Get_SubsetData_ForClassification(inp_data = tmp_training_data, inp_params = inp_params)
    classifiers_per_cross[[config$Crosses[i]]] <- Fit_Classifier(inp_data = tmp_data, inp_params = inp_params)
  }
  outp_results$Cross_Classifiers <- classifiers_per_cross
  # ---
  
  
  # Iterate through all data
  # ---
  Logger("Performing backtest...")
  
  # Define all arrays of output information that we will generate during backtest
  outp_predictions_raw <- matrix(NA, nrow(model_xdata), length(config$Crosses))  # Raw cross classifications
  outp_predictions_adj <- matrix(NA, nrow(model_xdata), length(config$Crosses))  # Adjusted cross classifications
  outp_predictions_accuracy <- matrix(NA, nrow(model_xdata), length(config$Crosses))  # Rolling prediction accuracy ratios
  outp_rolling_accuracy <- matrix(NA, nrow(model_xdata), length(config$Crosses))  # Rolling prediction accuracy ratios
  outp_currency_targets_raw <- matrix(NA, nrow(model_xdata), length(config$Currencies))  # Currency targets (aggregation of raw cross predictions)
  outp_currency_targets_adj <- matrix(NA, nrow(model_xdata), length(config$Currencies))  # Currency targets (aggregation of adjusted cross predictions)
  outp_aggregated_currency_returns_raw <- matrix(NA, nrow(model_xdata), length(config$Currencies))  # Returns from the raw target currency positions
  outp_aggregated_currency_returns_adj <- matrix(NA, nrow(model_xdata), length(config$Currencies))  # Returns from the adjusted target currency positions
  
  # Extract concise array of cross returns for ease of use in backtester
  cross_returns_array <- model_xdata[, c(1, match(paste(config$Crosses, ".1_Return", sep=""), names(model_xdata)))]  # Array of cross returns
  colnames(cross_returns_array) <- c("Datetime", config$Crosses)  # Put better headers (excluding the .1_Return bit)
  
  # Begin iterations through model_xdata (beyond initial classifier training data range), generating targets
  for (i in (inp_params$Fit_Window + 1):nrow(model_xdata)) {
    
    if (i %% 50 == 0)
      Logger(paste("On date ", i, " of ", nrow(model_xdata), " ... ", sep=""))
    
    # Get cross prediction/classifications for next day returns using current day xdata
    for (j in seq_along(config$Crosses)) {
      tmp_cross <- config$Crosses[j]
      
      # Get today data for classifier for this cross
      inp_params$Cross <- tmp_cross
      inp_params$XData_To_Use <- inp_params_cross_specific_regression[[tmp_cross]]$XData_To_Use
      inp_params$Classifier_Type <- inp_params_cross_specific_regression[[tmp_cross]]$Classifier_Type
      inp_params$Fit_Window <- inp_params_cross_specific_regression[[tmp_cross]]$Fit_Window
      tmp_today_data <- Get_SubsetData_ForPrediction(inp_data = model_xdata[i, ], inp_params = inp_params)  # Current day xdata
      
      # Get prediction and record the result as a 1 (buy) or -1 (sell)
      tmp_today_predictions <- Get_ClassifierPrediction(classifiers_per_cross[[tmp_cross]], tmp_today_data, inp_params) 
      if (tmp_today_predictions$class == "B")
        outp_predictions_raw[i, j] <- 1
      else if (tmp_today_predictions$class == "S")
        outp_predictions_raw[i, j] <- -1
      
      # Record whether this was a correct prediction using next day cross return (if not already last row)
      if (i < nrow(model_xdata))
        outp_predictions_accuracy[i, j] <- as.numeric(sign(outp_predictions_raw[i, j]) == sign(cross_returns_array[[tmp_cross]][i + 1]))
      
      # Calculate the rolling window prediction accuracy ratio (if sufficient predictions made) and create a performance-adjusted prediction 
      if (i > (inp_params$Fit_Window + inp_params$Rolling_Window_Performance)) {
        tmp <- outp_predictions_accuracy[(i - inp_params$Rolling_Window_Performance):(i - 1), j]  # Window of prediction accuracies for this cross
        outp_rolling_accuracy[i, j] <- sum(tmp == 1) / inp_params$Rolling_Window_Performance  # Accuracy ratio      
        outp_predictions_adj[i, j] <- outp_predictions_raw[i, j] * (1 + inp_params$Prediction_Adjust_Factor * (outp_rolling_accuracy[i, j] - 0.5) / 0.5)
      }
      
    }
    
    # Aggregate all predictions to get currency level targets
    for (j in seq_along(config$Currencies)) {
      tmp_ccy <- config$Currencies[j]  # This currency
      tmp_cols_as_base <- grep(tmp_ccy, substr(config$Crosses, 1, 3))  # Prediction columns where this currency is base ccy
      tmp_cols_as_quote <- grep(tmp_ccy, substr(config$Crosses, 4, 6))  # Prediction columns where this currency is quote ccy
      
      # Get sum of preds where ccy appears as base currency 
      tmp_pos_as_base_raw <- sum(outp_predictions_raw[i, tmp_cols_as_base])  # (raw preds)
      tmp_pos_as_base_adj <- sum(outp_predictions_adj[i, tmp_cols_as_base])  # (adj preds)
      
      # Get sum of preds where ccy appears as quote currency 
      tmp_pos_as_quote_raw <- sum(outp_predictions_raw[i, tmp_cols_as_quote])  # Row sums from where this currency appears as quote currency in cross (raw preds)
      tmp_pos_as_quote_adj <- sum(outp_predictions_adj[i, tmp_cols_as_quote])  # Row sums from where this currency appears as quote currency in cross (adj preds)
      
      # Get total position from base and subtract total where appear as quote
      outp_currency_targets_raw[i, j] <- tmp_pos_as_base_raw - tmp_pos_as_quote_raw  # Total aggregated currency holding, add "from base" contributions and subtract "from quote" contributions (RAW PREDS)
      outp_currency_targets_adj[i, j] <- tmp_pos_as_base_adj - tmp_pos_as_quote_adj  # Total aggregated currency holding, add "from base" contributions and subtract "from quote" contributions (ADJ PREDS)
      
    }
    
    # Get resulting return from the target currency holdings
    if (i < nrow(model_xdata)) {
      for (j in seq_along(config$Currencies)) {
        
        tmp_ccy <- config$Currencies[j]
        if (tmp_ccy == "USD") {
          outp_aggregated_currency_returns_raw[i, j] <- 0
          outp_aggregated_currency_returns_adj[i, j] <- 0
        } else if (tmp_ccy %in% c("EUR", "GBP", "AUD", "NZD")) {
          outp_aggregated_currency_returns_raw[i, j] <- cross_returns_array[[config$Currencies_To_USD[[tmp_ccy]]]][i + 1] * outp_currency_targets_raw[i, j]
          outp_aggregated_currency_returns_adj[i, j] <- cross_returns_array[[config$Currencies_To_USD[[tmp_ccy]]]][i + 1] * outp_currency_targets_adj[i, j]
        } else {
          outp_aggregated_currency_returns_raw[i, j] <- cross_returns_array[[config$Currencies_To_USD[[tmp_ccy]]]][i + 1] * outp_currency_targets_raw[i, j] * -1
          outp_aggregated_currency_returns_adj[i, j] <- cross_returns_array[[config$Currencies_To_USD[[tmp_ccy]]]][i + 1] * outp_currency_targets_adj[i, j] * -1
        }
        
        
      }
    }
    
  }
  Logger("Done!", inp_new_line = FALSE)
  
  # Convert output arrays to dataframes and add to return list
  # Predictions_raw
  outp_predictions_raw <- data.frame(outp_predictions_raw)
  colnames(outp_predictions_raw) <- config$Crosses; rownames(outp_predictions_raw) <- model_xdata[, 1]
  outp_results$Predictions_Raw <- outp_predictions_raw
  # Predictions_adj
  outp_predictions_adj <- data.frame(outp_predictions_adj)
  colnames(outp_predictions_adj) <- config$Crosses; rownames(outp_predictions_adj) <- model_xdata[, 1]
  outp_results$Predictions_Adjusted <- outp_predictions_adj
  # Predictions_accuracy
  outp_predictions_accuracy <- data.frame(outp_predictions_accuracy)
  colnames(outp_predictions_accuracy) <- config$Crosses; rownames(outp_predictions_accuracy) <- model_xdata[, 1]
  outp_results$Predictions_Accuracy <- outp_predictions_accuracy
  # Predictions_rolling_accuracy
  outp_rolling_accuracy <- data.frame(outp_rolling_accuracy)
  colnames(outp_rolling_accuracy) <- config$Crosses; rownames(outp_rolling_accuracy) <- model_xdata[, 1]
  outp_results$Predictions_Rolling_Accuracy <- outp_rolling_accuracy
  # Currency_targets_raw
  outp_currency_targets_raw <- data.frame(outp_currency_targets_raw)
  colnames(outp_currency_targets_raw) <- config$Currencies; rownames(outp_currency_targets_raw) <- model_xdata[, 1]
  outp_results$Currency_Targets_Raw <- outp_currency_targets_raw
  # Currency_targets_adj
  outp_currency_targets_adj <- data.frame(outp_currency_targets_adj)
  colnames(outp_currency_targets_adj) <- config$Currencies; rownames(outp_currency_targets_adj) <- model_xdata[, 1]
  outp_results$Currency_Targets_Adjusted <- outp_currency_targets_adj
  # Currency_target_returns_raw
  outp_aggregated_currency_returns_raw <- data.frame(outp_aggregated_currency_returns_raw)
  colnames(outp_aggregated_currency_returns_raw) <- config$Currencies; rownames(outp_aggregated_currency_returns_raw) <- model_xdata[, 1]
  outp_results$Currency_Target_Returns_Raw <- outp_aggregated_currency_returns_raw
  # Currency_target_returns_adj
  outp_aggregated_currency_returns_adj <- data.frame(outp_aggregated_currency_returns_adj)
  colnames(outp_aggregated_currency_returns_adj) <- config$Currencies; rownames(outp_aggregated_currency_returns_adj) <- model_xdata[, 1]
  outp_results$Currency_Target_Returns_Adjusted <- outp_aggregated_currency_returns_adj
  
  return (outp_results)
  
}
