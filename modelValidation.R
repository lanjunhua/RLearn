# this file is for model validatin of some basic index 
# like KS GINI AUC(ROC)
library(ROCR)


# just get the index value
# Gini coefficient function
SumModelGini <- function(target, predict) {
  df = data.frame(target = target, predict = predict)
  df <- df[order(df$predict, decreasing = TRUE),]
  df$random = (1:nrow(df))/nrow(df)
  totalPos <- sum(df$target)
  df$cumPosFound <- cumsum(df$target) # this will store the cumulative number of positive examples found (used for computing "Model Lorentz")
  df$Lorentz <- df$cumPosFound / totalPos # this will store the cumulative proportion of positive examples found ("Model Lorentz")
  df$Gini <- df$Lorentz - df$random # will store Lorentz minus random
  return(sum(df$Gini))
}

# return index function
msModelValidationFunc <- function(predict, target){
  pred <- prediction(predict, target)
  perf <- performance(pred,"tpr","fpr")
  ks = round(max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]]),4)*100
  perf2 <- performance(pred, 'auc')
  auc <- round(unlist(perf2@y.values),4)
  gini <- round((2 * SumModelGini(target, predict))/length(target),4)
  dfValidation <- data.frame(ks = ks, auc = auc, gini = gini)
  return(dfValidation)
}

# lift chart func
msModelLiftCharFunc <- function( predict 
                                 ,target
                                 ,isVariable = FALSE
                                 ,groupNum = 10
                                 ,groupMethods = c('sum', 'mean', 'median')){
  df = data.frame(target = target, predict = predict)
  df <- df[order(df$predict, decreasing = TRUE),]
  totalPos <- sum(df$target)
  nObs <- length(df$target)
  df$order <- c(1:nObs)
  df$group <- cut(df$order, breaks = groupNum)
  dfGroup <- aggregate(target ~ group, data = df, groupMethods)
  if(!isVariable){
    dfGroup$posPercModel <- round(dfGroup$target/totalPos, 4)*100
    dfGroup$posPercRandom <- round(1/groupNum, 4)*100
  }
  return(dfGroup)
}


# Lorenz curve function
LorenzCuverFunc <- function(predictions, labels){
  labelOrdered <- labels[order(predictions, decreasing = FALSE)]
  popuPct <- c(1:length(labelOrdered))/length(labelOrdered)
  posCumPct <- cumsum(labelOrdered)/sum(labelOrdered)
  data.frame(posCumPct, popuPct)
}



# KS function
ksLorenzStd <- function(predictions, labels){
  labelOrdered <- labels[order(predictions, decreasing = TRUE)]
  popuPct <- c(1:length(labelOrdered))/length(labelOrdered)
  posCumPct <- cumsum(labelOrdered)/sum(labelOrdered)
  negCumPct <- cumsum(1-labelOrdered)/sum(1-labelOrdered)
  data.frame(posCumPct, negCumPct, popuPct)
}
