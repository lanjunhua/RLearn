# PDD table aggregate
# just generate one record of one loan record


msRecallPbocPDD <- function(dfSTPbocPDD, ...){
  dfSTPbocPDD <- within(dfSTPbocPDD, {
    TIME_NUM_DQ <- paste(TIME_DQ, NUM_DQM, sep = ':')
  })
  
  dfRecallPbocDQStr <- aggregate(TIME_NUM_DQ ~ ID_PTRA + CATG, data = dfSTPbocPDD, function(x){paste(x, collapse = ',')})
  return(dfRecallPbocDQStr)
}
