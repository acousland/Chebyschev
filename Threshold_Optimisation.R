# Threshold testing
rm(result)
result <- data.frame("Threshold"=0,"Negative"=0,"Positive"=0)

for (i in seq(.9,1,by=0.001))
{
  threshold <- i
  logger.results.threshold <- logger.results
  logger.results.threshold$PrFault2 <- ifelse(logger.results.threshold$PrFault<threshold,0,logger.results.threshold$PrFault)
  
  # Measure performance
  result.tmp <- logger.results.threshold %>%
    group_by(FAULT) %>%
    summarise (Score = sum(PrFault2))
  
  result <- rbind(result,c(threshold,result.tmp$Score))
}
result$success <- result$Positive/result$Negative
print(result)

plot(result$Threshold,result$success)