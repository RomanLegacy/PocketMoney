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
  
  Logger(paste("Reading in data from file:", inp_filename, "...", sep=" "), inp_end_line = FALSE)
  
  tmp_data <- read.table(inp_filename, header=TRUE, sep=",", skip=2)
  tmp <- read.csv(inp_filename, header=FALSE, skip=1, nrows=1, stringsAsFactors=FALSE)
  tmpnames <- names(tmp_data)
  names(tmp_data) <- paste(tmpnames, tmp, sep="_")
  
  # Convert the first column (to be called "Datetime") to a nicer format
  names(tmp_data)[1] <- "Datetime"
  tmp_data$Datetime <- gsub("-", "-", as.Date(tmp_data$Datetime, "%d/%m/%Y"))
  
  Logger("Complete!", inp_new_line = FALSE)
  
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

# Checked to here...

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
    Logger(paste("Classifier run = ", inp_classifier_type, " , Cross = ", inp_params$Cross, sep=""))
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

