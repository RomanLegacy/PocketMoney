tmp <- lapply(classifiers_per_cross, function(d){
  coef(d[[1]])
})

write.table(tmp_predictions_raw, "Results/AUDIT_Cross_Predictions_Raw.csv", sep=",", quote=FALSE, row.names = TRUE)
write.table(outp_results$Cross_Predictions, "Results/AUDIT_Cross_Predictions_Adj.csv", sep=",", quote=FALSE, row.names = TRUE)
write.table(outp_results$Cross_Predictions_Accuracy, "Results/AUDIT_Cross_Predictions_Accuracy.csv", sep=",", quote=FALSE, row.names = TRUE)
write.table(outp_results$Cross_Predictions_Accuracy_Rolling, "Results/AUDIT_Cross_Predictions_Accuracy_Rolling.csv", sep=",", quote=FALSE, row.names = TRUE)
write.table(outp_results$Currency_Targets, "Results/AUDIT_Currency_Targets.csv", sep=",", quote=FALSE, row.names = TRUE)
write.table(outp_results$Currency_Returns, "Results/AUDIT_Currency_Returns.csv", sep=",", quote=FALSE, row.names = TRUE)
write.table(outp_results$XData, "Results/AUDIT_XData.csv", sep=",", quote=FALSE, row.names = TRUE)


dim(backtest_results2$Cross_Predictions)
rownames(backtest_results2$Cross_Predictions)
