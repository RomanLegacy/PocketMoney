#backtester.r

Backtest_ClassifierAlgo <- function(inp_params) {
  # Perform a backtest of a specified classifier type using specified backtest parameters.
  # Parameter list must contain the following objects:
  # Model_XDataFile, XData_To_Use, Fit_Window, Rolling_Window_Performance, Prediction_Adjust_Factor, AdjustPredictedPositions
  # Classifier_Type, Refit_Classifier_Periodicity
  
#   inp_params <- my_tmp_test_params
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
  tmp_currency_returns <- data.frame(matrix(NA, nrow(model_xdata), length(config$Currencies), dimnames=list(model_xdata$Datetime, config$Currencies)))
  
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
    
#     # Alternatively, the above re-training procedure can be based on some cross-specific performance stats, and some classifiers can be re-trained independently of others
#     # ---
#     tmp_fit_date <- model_xdata$Datetime[i]
#     for (i_cross in config$Crosses) {
#       if (TRUE) {  # Some criteria for re-training the classifier
#         Logger(paste("Re-training classifier:", i_cross, sep=" "))
#         inp_params$Cross <- i_cross
#         tmp_training_data <- Extract_CrossTrainingXData(model_xdata, inp_params, i)
#         classifiers_per_cross[[i_cross]][[tmp_fit_date]] <- Fit_Classifier(tmp_training_data, inp_params)
#       }
#     }
#     # ---

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
          tmp_predictions_adj[i, j] <- tmp_predictions_raw[i, j] * (1 + inp_params$Prediction_Adjust_Factor * (tmp_predictions_accuracy[i, j] - 0.5) / 0.5)
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




