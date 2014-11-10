#analysis.r

source('config.R')

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
my_params <- list("Backtest_Tag" = "NoAdj,WithRefit50",
                  "Model_XDataFile" = "Data/SmallTestFile.csv",
                  "XData_To_Use" = c("Returns", "PC1", "PC2"),
                  "Fit_Window" = 250,
                  "Rolling_Window_Performance" = 10,
                  "Prediction_Adjust_Factor" = 0.5,
                  "AdjustPredictedPositions" = FALSE,
                  "Classifier_Type" = "lda",
                  "Refit_Classifier_Periodicity" = 50)
my_params2 <- list("Backtest_Tag" = "WithAdj,WithRefit50",
                  "Model_XDataFile" = "Data/SmallTestFile.csv",
                  "XData_To_Use" = c("Returns", "PC1", "PC2"),
                  "Fit_Window" = 250,
                  "Rolling_Window_Performance" = 10,
                  "Prediction_Adjust_Factor" = 0.5,
                  "AdjustPredictedPositions" = TRUE,
                  "Classifier_Type" = "lda",
                  "Refit_Classifier_Periodicity" = 50)
backtest_results <- Backtest_ClassifierAlgo(my_params)
backtest_results2 <- Backtest_ClassifierAlgo(my_params2)


tmp_ccys <- config$Currencies
for (i in seq_along(tmp_ccys)) {
  tmp_ccy <- tmp_ccys[i]
  tmp <- backtest_results$[[tmp_ccy]]; tmp[is.na(tmp)] <- 0
  tmp2 <- backtest_results2$Currency_Returns[[tmp_ccy]]; tmp2[is.na(tmp2)] <- 0;
  plot(cumsum(tmp), ylim=range(c(cumsum(tmp), cumsum(tmp2))), type="l", main=paste(backtest_results$Backtest_Parameters$Backtest_Tag, backtest_results2$Backtest_Parameters$Backtest_Tag, tmp_ccy, sep=" : "))
  lines(cumsum(tmp2), col="red", lty=2)
  legend("topleft", legend=c(backtest_results$Backtest_Parameters$Backtest_Tag, backtest_results2$Backtest_Parameters$Backtest_Tag), col=1:2, lty=1:2)
}
tmp <- rowSums(backtest_results$Currency_Returns, na.rm=TRUE); tmp[is.na(tmp)] <- 0
tmp2 <- rowSums(backtest_results2$Currency_Returns, na.rm=TRUE); tmp2[is.na(tmp2)] <- 0
plot(cumsum(tmp), ylim=range(c(cumsum(tmp), cumsum(tmp2))), type="l", main="Aggregate")
lines(cumsum(tmp2), col="red", lty=2)
legend("topleft", legend=c(backtest_results$Backtest_Parameters$Backtest_Tag, backtest_results2$Backtest_Parameters$Backtest_Tag), col=1:2, lty=1:2)

# Plot the cumulative returns for each of the currencies and the aggregate portfolio
backtest_results$CurrencyTarget_Returns[is.na(backtest_results$CurrencyTarget_Returns)] <- 0
for (i in 1:ncol(backtest_results$CurrencyTarget_Returns)) {
  plot(cumsum(backtest_results$CurrencyTarget_Returns[[i]]), type="l", main=colnames(backtest_results$CurrencyTarget_Returns)[i])
}
# ---