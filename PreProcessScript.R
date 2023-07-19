library('stringr')
library('RWeka')
library(caret)
library(neuralnet)
# adjust this number for more/less examples used
numPrev<- 10

if (numPrev == 1) {
  df <- data.frame("reading1")
} else {
  df <- data.frame("reading1")
  for (i in 2:numPrev){
    df[str_interp("tBetween${i-1}${i}")]<- NA
    df[str_interp("action${i-1}")]<- NA
    df[str_interp("reading${i}")]<- NA
  }
  flag <- FALSE
  for (x in 1:70) {
    fileName <- if(x<10) str_interp("data-0${x}") else str_interp("data-${x}")
    data <- read.table(fileName,sep="\t", header=FALSE)
    rMax <- nrow(data) 
    
    for (r in (numPrev+1):rMax) {
      if(nrow(df)>0){
        v <- list(data[r-numPrev,4])
        for (i in (numPrev-1):1){
          hours1 <- as.integer(unlist(strsplit(data[r-i-1,2], ":"))[1])
          mins1 <- as.integer(unlist(strsplit(data[r-i-1,2], ":"))[2])
          dateTime1 <- ISOdatetime(as.integer(substr(data[r-i-1,1], 7,10)), as.integer(substr(data[r-i-1,1],0,2)), as.integer(substr(data[r-i-1,1],4,5)), hours1, mins1, 0, tz = "")
          hours <- as.integer(unlist(strsplit(data[r-i,2], ":"))[1])
          mins <- as.integer(unlist(strsplit(data[r-i,2], ":"))[2])
          dateTime2 <- ISOdatetime(as.integer(substr(data[r-i,1], 7,10)), as.integer(substr(data[r-i,1],0,2)), as.integer(substr(data[r-i,1],4,5)), hours, mins, 0, tz = "")
          timeDiff <- difftime(dateTime2, dateTime1, units = "mins")
          # if (is.na(timeDiff) || is.na(data[r-i,3]) || is.na(data[r-i,4])){
          #   print(str_interp("${dateTime1} ${as.integer(substr(data[r-i-1,1], 7,10))} ${as.integer(substr(data[r-i-1,1], 0,2))} ${as.integer(substr(data[r-i-1,1], 4,5))} ${hours1} ${mins1}"))
          #   print(str_interp("${dateTime2} ${as.integer(substr(data[r-i,1], 7,10))} ${as.integer(substr(data[r-i,1],0,2))} ${as.integer(substr(data[r-i,1], 4,5))} ${hours} ${mins}"))
          #   
          #   print(data[r-i,3])
          #   print(data[r-i,4])
          #   print(str_interp("${r}, ${i}"))
          #   print(fileName)
          #   flag <- TRUE
          #   break
          # }          
          v[length(v)+1] <- timeDiff
          v[length(v)+1] <- data[r-i,3]
          v[length(v)+1] <- data[r-i,4]
          
        }
        df[nrow(df) + 1,] <- v
        
      }
      if (flag) break
      
    }
    if (flag) break
    
  }
  
  
  for (i in 1:(numPrev-1)) {
    df[str_interp("action${i}")] <- as.factor(unlist(df[str_interp("action${i}")]))
  }
  df$X.reading1. <- as.numeric(df$X.reading1.)
  for (i in 2:numPrev){
    df[str_interp("reading${i}")]<- as.numeric(unlist(df[str_interp("reading${i}")]))
  }
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }

  df <- na.omit(df)
  dummy <- dummyVars(" ~ .", data=df)
  final_df <- data.frame(predict(dummy, newdata=df))
  final_df <- subset(final_df, select = -c(action1.0, action2.0, action3.0, action4.0, action5.0, action6.0, action7.0, action8.0, action9.0, action1.4, action2.4, action3.4, action4.4, action5.4, action6.4, action7.4, action8.4, action9.4))
  df_n <- as.data.frame(lapply(final_df, normalize))
  
  train <- df_n[1:14000,]
  test <- df_n[14000:15000,]
  neuralm <- neuralnet(reading10~.,data=train)
  results <- compute(neuralm, test)
  pred <- results$net.result
  cor(pred, test$reading10)
  # write.csv(df, "allData.csv", row.names=FALSE)
}
