get_mae <- function(fit, data, method="lm"){
  require(MASS)
  pred.err <- NULL
  DV <- formula(fit)[[2]]
  for (i in 1:nrow(data)){
    data.cv <- data[-i,]
    if (method=="lm"){
      fit.cv <- lm(formula(fit), data=data.cv)
    } else if (method == "lmer"){
      require(lme4)
      fit.cv <- lmer(formula(fit), data=data.cv, REML=FALSE)
    } else {
        return("Method not supported")
      break
      }
    yhat <- predict(fit.cv, data[i,])
    pred.err[i] <- abs(data[[DV]][i] - yhat)
  }
  return(mean(pred.err))
}
