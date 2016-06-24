# generate the R code for pboc 24M generate

library(lubridate)

msPbocRecall24M <- function(TIMEL_LSTINQ, TIMEL_ISS, RCD_RP24M, TIME_NUM_DQ, monthOffSet){
  if(TIMEL_ISS > TIMEL_LSTINQ - months(monthOffSet)) return(NA)
  if(TIMEL_ISS < TIMEL_LSTINQ - months(monthOffSet)){
    if(monthOffSet < 24){
      
      remained24MStr <- ifelse(is.na(RCD_RP24M), NA, unlist(strsplit(RCD_RP24M, ""))[1:(24 - monthOffSet)])
      
      if(floor(difftime(TIMEL_LSTINQ, TIMEL_ISS, units = 'days')/30) == 24 + monthOffSet){
        TIMEL_SRPRCD24M <- TIMEL_LSTINQ - months(24 + monthOffSet - 1)
        day(TIMEL_SRPRCD24M) <- 1
        end24MTime <- TIMEL_LSTINQ - months(monthOffSet)
        TIMEL_LSTINQ <- end24MTime
        day(TIMEL_ERPRCD24M) <- 30
        frontStr <- '*'
        
        mapStratTime <- TIMEL_SRPRCD24M 
        mapEndTime <- TIMEL_LSTINQ - months(24)
        day(mapEndTime) <- 30
        
        middleStr <- map(mapStratTime, mapEndTime, )
        
        all24MStr = paste0(na.omit(c(frontStr, middleStr, remained24MStr)), collapse = '')
        return(all24MStr)
      } else if(floor(difftime(TIMEL_LSTINQ, TIMEL_ISS, units = 'days')/30) < 24 + monthOffSet){
        frontStr <- rep('/', 24 + monthOffSet - floor(difftime(TIMEL_LSTINQ, TIMEL_ISS, units = 'days')/30))
        
        TIMEL_SRPRCD24M <- TIMEL_LSTINQ - months(24 + monthOffSet - 1)
        day(TIMEL_SRPRCD24M) <- 1
        end24MTime <- TIMEL_LSTINQ - months(monthOffSet)
        TIMEL_ERPRCD24M <- end24MTime
        day(TIMEL_ERPRCD24M) <- 30
        
        mapStratTime <- TIMEL_SRPRCD24M
        mapEndTime <- TIMEL_LSTINQ - months(monthOffSet)
        middleStr <- map()
        
        all24MStr = paste0(na.omit(c(frontStr, middleStr, remained24MStr)), collapse = '')
        return(all24MStr)
      } else if(floor(difftime(TIMEL_LSTINQ,TIMEL_ISS, units = 'days')/30) > 24 + monthOffSet){
        frontStr <- NULL
        
        TIMEL_SRPRCD24M <- TIMEL_LSTINQ - months(24 + monthOffSet - 1)
        day(TIMEL_SRPRCD24M) <- 1
        end24MTime <- TIMEL_LSTINQ - months(monthOffSet)
        TIMEL_ERPRCD24M <- end24MTime
        day(TIMEL_ERPRCD24M) <- 30
        
        mapStratTime <- TIMEL_SRPRCD24M + months(1)
        mapEndTime <- TIMEL_LSTINQ - months(monthOffSet)
        middleStr <- map()
        
        all24MStr = paste0(na.omit(c(frontStr, middleStr, remained24MStr)), collapse = '')
        
      }
    }
  }
}
