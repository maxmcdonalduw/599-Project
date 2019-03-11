get_mae_lm <- function(fit, data){
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

get_mae_lmer <- function(fit, data){
  require(MASS)
  require(lme4)
  pred.err <- NULL
  DV <- formula(fit)[[2]]
  for (i in 1:nrow(data)){
    data.cv <- data[-i,]
    fit.cv <- lmer(formula(fit), data=data.cv, REML=FALSE)
    yhat <- predict(fit.cv, data[i,])
    pred.err[i] <- abs(data[[DV]][i] - yhat)
  }
  return(mean(pred.err))
}