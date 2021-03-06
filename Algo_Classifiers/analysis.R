#analysis.r

source('config.R')

# Test that the backtester runs
tmp_res <- Backtest_ClassifierAlgo(my_tmp_test_params)
tmp_res$Backtest_Parameters
tail(tmp_res$, 20)

# Run backtest twice (with different parameters) and compare the performance of each
# ---
my_params <- list("Backtest_Tag" = "NoDailyReinvest",
                  "Model_XDataFile" = "Data/SmallTestFile.csv",  # RegXData_London.csv
                  "XData_To_Use" = c("Returns", "PC1", "PC2"),
                  "Fit_Window" = 250,
                  "Show_Backtest_Progress" = TRUE,
                  "Rolling_Window_Performance" = 10,
                  "Prediction_Adjust_Factor" = 0.5,
                  "AdjustPredictedPositions" = TRUE,
                  "Classifier_Type" = "lda",
                  "Refit_Classifier_Periodicity" = NA,
                  "Start_AUM" = 1e6,
                  "Reinvest_Daily" = FALSE)
backtest_results <- Backtest_ClassifierAlgo(my_params)
tmp_stats <- Calculate_AlgoPerformanceStats(backtest_results)

my_params <- list("Backtest_Tag" = "WithDailyReinvest",
                  "Model_XDataFile" = "Data/SmallTestFile.csv",  # SmallTestFile.csv
                  "XData_To_Use" = c("Returns", "PC1", "PC2"),
                  "Fit_Window" = 250,
                  "Show_Backtest_Progress" = TRUE,
                  "Rolling_Window_Performance" = 10,
                  "Prediction_Adjust_Factor" = 0.5,
                  "AdjustPredictedPositions" = TRUE,
                  "Classifier_Type" = "lda",
                  "Refit_Classifier_Periodicity" = NA,
                  "Start_AUM" = 1e6,
                  "Reinvest_Daily" = TRUE)
backtest_results2 <- Backtest_ClassifierAlgo(my_params)

# Tmp plots
tmp <- backtest_results$Currency_Pnls$Aggregate; tmp[is.na(tmp)] <- 0
tmp2 <- backtest_results2$Currency_Pnls$Aggregate; tmp2[is.na(tmp2)] <- 0
plot(cumsum(tmp), type="l"); lines(cumsum(tmp2), col="red")

tmp_ccys <- config$Currencies
for (i in seq_along(tmp_ccys)) {
  tmp_ccy <- tmp_ccys[i]
  tmp <- backtest_results$Currency_Returns[[tmp_ccy]]; tmp[is.na(tmp)] <- 0
  tmp2 <- backtest_results2$Currency_Returns[[tmp_ccy]]; tmp2[is.na(tmp2)] <- 0
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


tmp_acc <- unlist(lapply(tmp_stats$Cross, function(d){d$Accuracy}))
tmp_acc <- tmp_acc[order(tmp_acc, decreasing = TRUE)]

tmp_imp <- unlist(lapply(tmp_stats$Cross, function(d){d$Impact}))
tmp_imp <- tmp_imp[order(tmp_imp, decreasing = TRUE)]

tmp_perf <- unlist(lapply(tmp_stats$Cross, function(d){d$Performance}))
tmp_perf <- tmp_perf[order(tmp_perf, decreasing = TRUE)]


