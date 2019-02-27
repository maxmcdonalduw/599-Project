get_mae <- function(fit, data, method="lm"){
  require(MASS)
  pred.err <- NULL
  DV <- formula(fit)[[2]]
  for (i in 1:nrow(data)){
    data.cv <- data[-i,]
    fit.cv <- lm(formula(fit), data=data.cv)
    yhat <- predict(fit.cv, data[i,])
    pred.err[i] <- abs(data[[DV]][i] - yhat)
  }
  return(mean(pred.err))
}

func.err1 <- get_mae(fit = fit1, data=data)
