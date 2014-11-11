tmp <- lapply(classifiers_per_cross, function(d){
  coef(d[[1]])
})



