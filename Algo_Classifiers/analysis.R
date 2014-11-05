#analysis.r
source('S:/Roman/R/Research/Classifiers/config.R')

# Read in data, run a classifier for a given cross and column set
# ---
my_params <- list("Cross" = "NOKSEK",
                  "XData_To_Use" = c("Returns", "PC1", "PC2", "Copper", "Gold"),
                  "Training_Size" = 0.5,
                  "XDataset" = "S:/Roman/Data/misc_model_data_for_classifying2.csv")
my_data <- Get_XData(my_params$XDataset)
my_data_subset <- Get_SubsetData_ForClassification(my_data, my_params)
Test_Classifier(my_data_subset, my_params, inp_classifier_type = "lda")
# ---


# Read in data, run a classifier for a given selection of crosses and record classifier performance per each one
# ---
my_params <- list("Cross" = NA,
                  "XData_To_Use" = c("Returns", "PC1", "PC2"),
                  "Training_Size" = 0.5,
                  "XDataset" = "S:/Roman/Data/misc_model_data_for_classifying2.csv",
                  "Classifier" = list("Type" = "lda")
)
all_crosses <- config$Crosses
outp_accuracies <- numeric(length(all_crosses))
my_data <- Get_XData(my_params$XDataset)
for (i in seq_along(all_crosses)) {
  my_params$Cross <- all_crosses[i]
  my_data_subset <- Get_SubsetData_ForClassification(my_data, my_params)
  outp_accuracies[i] <- Test_Classifier(my_data_subset, my_params, inp_classifier_type = my_params$Classifier$Type, inp_print_results = FALSE)
}
names(outp_accuracies) <- all_crosses
my_res <- list("XData" = my_params$XData_To_Use,
               "Classifier_Info" = my_params$Classifier,
               "Classifier_Accuracy" = outp_accuracies,
               "Training_Size" = my_params$Training_Size)
my_res
# ---



# Run classifiers in more of a backtest setting, where we predict each day and aggregate targets and then monitor implied forward performance
# Note that we fit classifier once at start but, in practice, this can be re-fit at intervals...
# ---
my_params <- list("Model_XDataFile" = "S:/Roman/Data/misc_model_data_for_classifying2.csv",
                  "XData_To_Use" = c("Returns", "PC1", "PC2"),
                  "Fit_Window" = 250,
                  "Rolling_Window_Performance" = 10,
                  "Prediction_Adjust_Factor" = 0.5,
                  "AdjustPredictedPositions" = TRUE,
                  "Classifier_Type" = "lda")
backtest_results <- Backtest_ClassifierAlgo(my_params)

# Plot the cumulative returns for each of the currencies and the aggregate portfolio
backtest_results$CurrencyTarget_Returns[is.na(backtest_results$CurrencyTarget_Returns)] <- 0
for (i in 1:ncol(backtest_results$CurrencyTarget_Returns)) {
  plot(cumsum(backtest_results$CurrencyTarget_Returns[[i]]), type="l", main=colnames(backtest_results$CurrencyTarget_Returns)[i])
}
# ---