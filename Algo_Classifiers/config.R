#config.r
source('S:/Roman/R/Research/Classifiers/methods.R')

require(MASS)
require(nnet)
require(class)

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
                                          "NOK"="USDNOK", "SEK"="USDNOK", "JPY"="USDJPY"),
               "Market_Indices" = list("EUR"="DAX", "GBP"="UKX", "AUD"="AS30", "NZD"="NZSE",
                                       "USD"="SPXES1", "CAD"="PT1", "CHF"="SMI",
                                       "NOK"="OBX", "SEK"="OMX", "JPY"="NKY"),
               "Market_Exos" = list("Copper"="HG1", "Crude"="CL1", "Gold"="XAUUSD")
)