#methods.r


Log <- function(inp_string, inp_new_line=TRUE, inp_end_line=TRUE) {
  
  # Simple logger method that will produce debug info in an easy, structured manner
  #
  # Inputs:
  #     inp_string:     string to be printed
  #     inp_new_line:   boolean indicating whether this should be printed to a new line (TRUE) or current one (FALSE), default is TRUE 
  #     inp_end_line:   boolean indicating whether line should terminate at end of string (TRUE), or stay on current line (FALSE), default is TRUE 
  
  # If need datetime stamp
  if (inp_new_line)
    tmp <- paste("< ", strftime(Sys.time(), "%Y-%m-%d %H:%M:%OS")," >", sep="")
  else
    tmp <- ""
  # If need to terminate line at end
  if (inp_end_line)
    tmp <- paste(tmp, inp_string, "\n", sep=" ")
  else
    tmp <- paste(tmp, inp_string, sep=" ")
  
  # print string
  cat(tmp)
  
}

Get_XData <- function(inp_filename) {
  # Read in X-data file and put in format required for further work (special column headers, etc...)
  
  Log(paste("Reading in data from file:", inp_filename, "...", sep=" "), inp_end_line = FALSE)
  
  tmp_data <- read.table(inp_filename, header=TRUE, sep=",", skip=2)
  tmp <- read.csv(inp_filename, header=FALSE, skip=1, nrows=1, stringsAsFactors=FALSE)
  tmpnames <- names(tmp_data)
  names(tmp_data) <- paste(tmpnames, tmp, sep="_")
  
  Log("Complete!", inp_new_line = FALSE)
  
  return (tmp_data)
  
}

Get_SubsetData_ForClassification <- function(inp_data, inp_params) {
  # Strip out a subsetof the data that matches the specified securities, etc, for classification
  
  #   inp_data <- head(model_xdata, 1); inp_params <- my_params
  
  ccy_base <- substr(inp_params$Cross, 1, 3); ccy_quote <- substr(inp_params$Cross, 4, 6)
  col_names_used <- c()
  for (i in seq_along(inp_params$XData_To_Use)) {
    if (inp_params$XData_To_Use[i] == "Returns") {  # Cross return
      col_names_used <- c(col_names_used, paste(inp_params$Cross, ".1_Return", sep=""))
    } else if (grepl("PC", inp_params$XData_To_Use[i])) {  # A principal component
      col_names_used <- c(col_names_used, paste(ccy_base, inp_params$XData_To_Use[i], "_PC", sep=""))  # Base ccy PC
      col_names_used <- c(col_names_used, paste(ccy_quote, inp_params$XData_To_Use[i], "_PC", sep=""))  # Quote ccy PC
    } else if (inp_params$XData_To_Use[i] == "Index") {
      col_names_used <- c(col_names_used, paste(config$Market_Indices[[ccy_base]], "EXO", sep="_"))  # Base ccy index
      col_names_used <- c(col_names_used, paste(config$Market_Indices[[ccy_quote]], "EXO", sep="_"))  # Quote ccy index
    } else if (inp_params$XData_To_Use[i] %in% c("Copper", "Gold", "Crude")) {
      col_names_used <- c(col_names_used, paste(config$Market_Exos[[inp_params$XData_To_Use[i]]], "EXO", sep="_"))
    } 
  }
  col_names_used_inds <- match(col_names_used, names(inp_data))
  
  # Get "Group" tag based on next day return ("B" if was up, "S" if was down)
  tmp_groups <- rep(NA, nrow(inp_data) - 1)  # -1 because we cannot put a group tag on last entry
  tmp <- inp_data[[paste(inp_params$Cross, ".1_Return", sep="")]][-1]
  tmp_groups[which(tmp < 0)] <- "S"
  tmp_groups[which(tmp > 0)] <- "B"
  
  # Trim out the subset of data to contain only the required columns
  outp_data_sub <- data.frame("Group"=tmp_groups, inp_data[-nrow(inp_data), col_names_used_inds])
  
  return (outp_data_sub)
  
}

Get_SubsetData_ForPrediction <- function(inp_data, inp_params) {
  # Strip out a subsetof the data that matches the specified securities, etc, for prediction
  
  #   inp_data <- head(model_xdata, 1); inp_params <- my_params
  
  ccy_base <- substr(inp_params$Cross, 1, 3); ccy_quote <- substr(inp_params$Cross, 4, 6)
  col_names_used <- c()
  for (i in seq_along(inp_params$XData_To_Use)) {
    if (inp_params$XData_To_Use[i] == "Returns") {  # Cross return
      col_names_used <- c(col_names_used, paste(inp_params$Cross, ".1_Return", sep=""))
    } else if (grepl("PC", inp_params$XData_To_Use[i])) {  # A principal component
      col_names_used <- c(col_names_used, paste(ccy_base, inp_params$XData_To_Use[i], "_PC", sep=""))  # Base ccy PC
      col_names_used <- c(col_names_used, paste(ccy_quote, inp_params$XData_To_Use[i], "_PC", sep=""))  # Quote ccy PC
    } else if (inp_params$XData_To_Use[i] == "Index") {
      col_names_used <- c(col_names_used, paste(config$Market_Indices[[ccy_base]], "EXO", sep="_"))  # Base ccy index
      col_names_used <- c(col_names_used, paste(config$Market_Indices[[ccy_quote]], "EXO", sep="_"))  # Quote ccy index
    } else if (inp_params$XData_To_Use[i] %in% c("Copper", "Gold", "Crude")) {
      col_names_used <- c(col_names_used, paste(config$Market_Exos[[inp_params$XData_To_Use[i]]], "EXO", sep="_"))
    } 
  }
  col_names_used_inds <- match(col_names_used, names(inp_data))
  
  # Trim out the subset of data to contain only the required columns
  outp_data_sub <- inp_data[1, col_names_used_inds]
  
  return (outp_data_sub)
  
}

Test_Classifier <- function(inp_data, inp_params, inp_classifier_type, inp_print_results = TRUE) {
  # Test classifier on subset data specified
  
  n_entries_training <- round(nrow(inp_data) * inp_params$Training_Size)
  tmp_data_training <- head(inp_data, n_entries_training)
  tmp_data_testing <- inp_data[-(1:n_entries_training), ]
  
  if (inp_classifier_type == "lda") {
    
    tmp_fit <- lda(Group ~ ., tmp_data_training)
    tmp_preds <- predict(tmp_fit, newdata = tmp_data_testing)
    tmp_conf_mat <- table(tmp_preds$class, tmp_data_testing$Group)
    
  } else if (inp_classifier_type == "qda") {
    
    tmp_fit <- qda(Group ~ ., tmp_data_training)
    tmp_preds <- predict(tmp_fit, newdata = tmp_data_testing)
    tmp_conf_mat <- table(tmp_preds$class, tmp_data_testing$Group)
    
  } else if (inp_classifier_type == "nnet") {
    
    cat("NNET not implemented yet...\n")
    return (NULL)
    #     tmp_fit <- nnet(Group ~ ., tmp_data_training, size = 4, decay = 0.1)
    #     tmp_preds <- predict(tmp_fit, newdata = tmp_data_testing)
    #     tmp_conf_mat <- table(tmp_preds$class, tmp_data_testing$Group)
    
  } else if (inp_classifier_type == "knn") {
    
    tmp_training_zeros <- which(as.numeric(rowSums(is.na(tmp_data_training) > 0)) > 0)  # Detect any NA values in dataset
    if (length(tmp_training_zeros) > 0)
      tmp_data_training <- tmp_data_training[-tmp_training_zeros, ]  # Trim out NA values in training set (knn won't work otherwise)
    
    tmp_preds <- knn(tmp_data_training[, -1], tmp_data_testing[, -1], tmp_data_training$Group, k = inp_params$Classifier$N_Neighbours, l = 0, prob = FALSE, use.all = TRUE)
    tmp_conf_mat <- table(tmp_preds, tmp_data_testing$Group)
    
  } else {
    cat("Classifier:", inp_classifier_type, "not recognised...\n")
    return (NULL)
  }
  tmp_accuracy <- (tmp_conf_mat[1, 1] + tmp_conf_mat[2, 2]) / sum(tmp_conf_mat)
  
  if (inp_print_results) {
    Log(paste("Classifier run = ", inp_classifier_type, " , Cross = ", inp_params$Cross, sep=""))
    cat(paste("Independent variables used: ", paste(inp_params$XData_To_Use, collapse=", "), sep=""))
    cat("Percent correct classifications:", tmp_accuracy, "\n")
    print(tmp_conf_mat)
    cat("\n")
  }
  
  
  return (tmp_accuracy)
  
}

Fit_Classifier <- function(inp_data, inp_params) {
  # Fit the classifier model to data supplied, returning the model, ready to use for future predictions
  
  if (inp_params$Classifier_Type == "lda") {
    tmp_model <- lda(Group ~ ., inp_data)
  } else if (inp_params$Classifier_Type == "qda") {
    tmp_model <- qda(Group ~ ., inp_data)
  } else if (inp_params$Classifier_Type == "nnet") {
    tmp_model <- NULL  # Not implemented yet
  } else {
    cat("Classifier:", inp_params$Classifier_Type, "not recognised...\n")
    tmp_model <- NULL
  }
  
  return (tmp_model)
  
}

Get_ClassifierPrediction <- function(inp_classifier, inp_data, inp_params) {
  #inp_classifier <- classifiers_per_cross[[tmp_cross]]; inp_data <- tmp_today_data
  tmp_prediciton <- predict(inp_classifier, newdata = inp_data)
  return (tmp_prediciton)
  
}

Backtest_ClassifierAlgo <- function(inp_params) {
  # Perform a backtest of a specified classifier
  #inp_params <- my_params
  outp_results <- list()  # List of backtest results and various calculated objects
  
  # Import the XData file
  # ---
  Log("Importing XData...")
  model_xdata <- Get_XData(inp_params$Model_XDataFile)
  outp_results$XData <- model_xdata
  # ---
  
  
  # Perform the initial classifier model fitting
  # ---
  Log("Fitting classifier models...")
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
  Log("Performing backtest...")
  
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
      Log(paste("On date ", i, " of ", nrow(model_xdata), " ... ", sep=""))
    
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
    
  }
  Log("Done!", inp_new_line = FALSE)
  
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

Backtest_ClassifierAlgo_WithRefits <- function(inp_params) {
  # Perform a backtest of a specified classifier
  #inp_params <- my_params
  outp_results <- list()  # List of backtest results and various calculated objects
  
  # Import the XData file
  # ---
  Log("Importing XData...")
  model_xdata <- Get_XData(inp_params$Model_XDataFile)
  outp_results$XData <- model_xdata
  # ---
  
  
  # Perform the initial classifier model fitting
  # ---
  Log("Fitting classifier models...")
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
  Log("Performing backtest...")
  
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
      Log(paste("On date ", i, " of ", nrow(model_xdata), " ... ", sep=""))
    
    
    # Perform a re-fitting of the classifiers per cross if time_to_refit has reached 0
    # ---
    if (time_to_refit == 0) {
      Log("Fitting classifier models...")
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
  Log("Done!", inp_new_line = FALSE)
  
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
  Log("Importing XData...")
  model_xdata <- Get_XData(inp_params$Model_XDataFile)
  outp_results$XData <- model_xdata
  # ---
  
  
  # Perform the initial classifier model fitting
  # ---
  Log("Fitting classifier models...")
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
  Log("Performing backtest...")
  
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
      Log(paste("On date ", i, " of ", nrow(model_xdata), " ... ", sep=""))
    
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
  Log("Done!", inp_new_line = FALSE)
  
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