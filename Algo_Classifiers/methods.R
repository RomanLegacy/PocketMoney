#methods.r

Logger <- function(inp_string, inp_new_line=TRUE, inp_end_line=TRUE) {
  
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
  #
  # Calls: NA
  #
  # Inputs:
  #     inp_filename: string: filename for the X-dataset to read in
  #
  # Returns: dataframe containing the imported X-data
  
  
  Logger(paste("Reading in data from file:", inp_filename, "...", sep=" "), inp_end_line = FALSE)
  
  # Read the x-data array and the 2nd row containing the instrument type, to use in headers
  tmp_data <- read.table(inp_filename, header=TRUE, sep=",", skip=2)
  tmp <- read.csv(inp_filename, header=FALSE, skip=1, nrows=1, stringsAsFactors=FALSE)
  
  # Remove any extra columns at the end with NA's
  tmp_na_columns <- which(is.na(tmp))
  if (length(tmp_na_columns) > 0) {
    tmp <- tmp[-tmp_na_columns]
    tmp_data <- tmp_data[, -tmp_na_columns]
  }
    
  # Add the column names to dataframe
  tmp_names <- names(tmp_data)
  names(tmp_data) <- paste(tmp_names, tmp, sep="_")
  
  # Convert the first column (to be called "Datetime") to a nicer format
  names(tmp_data)[1] <- "Datetime"
  tmp_data$Datetime <- gsub("-", "-", as.Date(tmp_data$Datetime, "%d/%m/%Y"))
  
  Logger("Complete!", inp_new_line = FALSE)
  
  return (tmp_data)
  
}

Get_CrossXDataColumns <- function(inp_params) {
  
  # Get X-data column headers to use for specified cross
  #
  # Calls: NA
  #
  # Inputs:
  #     inp_params: list: list of backtest parameters
  #
  # Returns: vector of string column names
  
  ccy_base <- substr(inp_params$Cross, 1, 3); ccy_quote <- substr(inp_params$Cross, 4, 6)
  outp_colnames <- c()
  for (i in seq_along(inp_params$XData_To_Use)) {
    if (inp_params$XData_To_Use[i] == "Returns") {  # Cross return
      outp_colnames <- c(outp_colnames, paste(inp_params$Cross, ".1_Return", sep=""))
    } else if (grepl("PC", inp_params$XData_To_Use[i])) {  # A principal component
      outp_colnames <- c(outp_colnames, paste(ccy_base, inp_params$XData_To_Use[i], "_PC", sep=""))  # Base ccy PC
      outp_colnames <- c(outp_colnames, paste(ccy_quote, inp_params$XData_To_Use[i], "_PC", sep=""))  # Quote ccy PC
    } else if (inp_params$XData_To_Use[i] == "Index") {
      outp_colnames <- c(outp_colnames, paste(config$Market_Indices[[ccy_base]], "EXO", sep="_"))  # Base ccy index
      outp_colnames <- c(outp_colnames, paste(config$Market_Indices[[ccy_quote]], "EXO", sep="_"))  # Quote ccy index
    } else if (inp_params$XData_To_Use[i] %in% c("Copper", "Gold", "Crude")) {
      outp_colnames <- c(outp_colnames, paste(config$Market_Exos[[inp_params$XData_To_Use[i]]], "EXO", sep="_"))
    } 
  }
  
  return (outp_colnames)
  
}

Extract_CrossTrainingXData <- function(inp_data, inp_params, inp_current_index) {
  
  # Extract a subset of data to use to train a classifier in a specific cross
  #
  # Calls: Get_CrossXDataColumns
  #
  # Inputs:
  #     inp_data:           dataframe:  x-data from which we take a subset
  #     inp_params:         list:       backtest parameters
  #     inp_current_index:  numeric:    row/iteration of x-data that we are currently at
  #
  # Returns: dataframe subset of the xdata containing just columns relevant for the cross provided, and the required training set lookback window
  
  tmp_data <- inp_data[(inp_current_index - inp_params$Fit_Window):(inp_current_index - 1), ]  # Subset of historical data to use
  cross_xdata_colnames <- Get_CrossXDataColumns(inp_params)  # XData columns to use
  col_names_used_inds <- match(cross_xdata_colnames, names(tmp_data))  # Columns indices of XData columns
  
  # Get "Group" tag based on next day return ("B" if was up, "S" if was down)
  tmp_groups <- rep(NA, nrow(tmp_data) - 1)  # -1 because we cannot put a group tag on last entry (no knowledge of realised next day return)
  tmp <- tmp_data[[paste(inp_params$Cross, ".1_Return", sep="")]][-1]
  tmp_groups[which(tmp < 0)] <- "S"
  tmp_groups[which(tmp > 0)] <- "B"
  
  # Trim out the subset of data to contain only the required columns
  outp_data <- data.frame("Group"=tmp_groups, tmp_data[-nrow(tmp_data), col_names_used_inds])
  
  return (outp_data)
  
}

Extract_CrossPredictionXData <- function(inp_data, inp_params, inp_current_index) {
  
  # Extract a subset of data (single row) to use to predict/classify a next-day return
  #
  # Calls: Get_CrossXDataColumns
  #
  # Inputs:
  #     inp_data:           dataframe:  x-data from which we take a subset
  #     inp_params:         list:       backtest parameters
  #     inp_current_index:  numeric:    row/iteration of x-data that we are currently at
  #
  # Returns: dataframe subset (single row) of the x-data containing just columns relevant for the cross provided
  
  cross_xdata_colnames <- Get_CrossXDataColumns(inp_params)
  col_names_used_inds <- match(cross_xdata_colnames, names(inp_data))
  
  # Trim out the subset of data to contain only the required columns
  outp_data_sub <- inp_data[inp_current_index, col_names_used_inds]
  
  return (outp_data_sub)
  
}

Fit_Classifier <- function(inp_data, inp_params) {
  
  # Fit the classifier model to training data supplied, ready to use for future predictions
  #
  # Calls: NA
  #
  # Inputs:
  #     inp_data:           dataframe:  training x-data
  #     inp_params:         list:       backtest parameters
  #
  # Returns: a fitted classifier model object
  
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
  
  # Uses a supplied classifier model object and a set of x-data to produce a prediction/classification for next day return
  #
  # Calls: NA
  #
  # Inputs:
  #     inp_classifier:  model object: fitted classifier model object
  #     inp_data:           dataframe: training x-data
  #     inp_params:              list: backtest parameters
  #
  # Returns: a fitted classifier model object
  
  tmp_prediciton <- predict(inp_classifier, newdata = inp_data)
  return (tmp_prediciton)
  
}
