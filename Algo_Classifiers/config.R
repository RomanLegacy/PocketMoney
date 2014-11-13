#config.r

source('backtester.R')
source('methods.R')

require(MASS)
require(nnet)
require(class)
require(lubridate)

# Temporary test parameters, to be used to test backtester still runs through and is not broken
my_tmp_test_params <- list("Backtest_Tag" = "NoAdj,WithRefit50",
                  "Model_XDataFile" = "Data/SmallTestFile.csv",
                  "XData_To_Use" = c("Returns", "PC1", "PC2"),
                  "Fit_Window" = 250,
                  "Show_Backtest_Progress" = TRUE,
                  "Rolling_Window_Performance" = 10,
                  "Prediction_Adjust_Factor" = 0.5,
                  "AdjustPredictedPositions" = FALSE,
                  "Classifier_Type" = "lda",
                  "Refit_Classifier_Periodicity" = 50,
                  "Start_AUM" = 1e6,
                  "Reinvest_Daily" = FALSE)

config <- list("Crosses" = c("EURGBP", "EURAUD", "EURNZD", "EURUSD", "EURCAD", "EURCHF", "EURNOK", "EURSEK", "EURJPY",
                             "GBPAUD", "GBPNZD", "GBPUSD", "GBPCAD", "GBPCHF", "GBPNOK", "GBPSEK", "GBPJPY",
                             "AUDNZD", "AUDUSD", "AUDCAD", "AUDCHF", "AUDNOK", "AUDSEK", "AUDJPY",
                             "NZDUSD", "NZDCAD", "NZDCHF", "NZDNOK", "NZDSEK", "NZDJPY",
                             "USDCAD", "USDCHF", "USDNOK", "USDSEK", "USDJPY",
                             "CADCHF", "CADNOK", "CADSEK", "CADJPY",
                             "CHFNOK", "CHFSEK", "CHFJPY",
                             "NOKSEK", "NOKJPY",
                             "SEKJPY"),
               "Currencies" = c("EUR", "GBP", "AUD", "NZD", "USD", "CAD", "CHF", "NOK", "SEK", "JPY"),
               "Currencies_To_USD" = list("EUR"="EURUSD", "GBP"="GBPUSD", "AUD"="AUDUSD",
                                          "NZD"="NZDUSD", "CAD"="USDCAD", "CHF"="USDCHF",
                                          "NOK"="USDNOK", "SEK"="USDSEK", "JPY"="USDJPY"),
               "Market_Indices" = list("EUR"="DAX", "GBP"="UKX", "AUD"="AS30", "NZD"="NZSE",
                                       "USD"="SPXES1", "CAD"="PT1", "CHF"="SMI",
                                       "NOK"="OBX", "SEK"="OMX", "JPY"="NKY"),
               "Market_Exos" = list("Copper"="HG1", "Crude"="CL1", "Gold"="XAUUSD")
)