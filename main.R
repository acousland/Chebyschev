#########################################################
# Chebyschev Implementation 
#
# Calculate non-fault mean and std dev over a sliding 
# window and look for deviations from this to detect
# faults
# 
# - Aaron Cousland 01/05/2015
########################################################

require (RODBC)        # Load RODBC package
require (lubridate)   # Required to manipulate dates
require (ggplot2)     # Required for nice graphs 
require (gridExtra)   # For further graph manipulation
require (dplyr)

source ('Chebyschev_Function.R')

# Create a connection to the database called "channel"
local.connection <- odbcConnect("RTV", believeNRows=FALSE)

# Query the database and put the results into the data frame logging.results
logger.results <- sqlQuery(local.connection,"SELECT * FROM ELSPEC.RMS_TRAINING where ts between '17/Mar/15 08:00:00 AM' and '17/Mar/15 03:00:00 PM';")

#Order by timestamp
logger.results <- logger.results[with(logger.results, order(logger.results$TS)),]

logger.results$TS <- force_tz(logger.results$TS,"AEST")

# Initialise probability of fault column and window length
logger.results$PrFault <- 0
window.length = 500

# Run Chebyschev analysis on results
logger.results <- Chebyschev(logger.results,window.length)

# Display Results
plot(logger.results$TS,logger.results$RMSI1, type="l")
polygon(logger.results$TS,logger.results$PrFault*max(logger.results$RMSI1), col =rgb(1,0,0,alpha=0.3),xlab="",ylab="",yaxt="n",border = NA)
axis(4)

# Zoom and view
StartTime <- force_tz(as.POSIXct("2015-03-17 08:45:00 AM", format = "%Y-%m-%d %H:%M:%OS"),"AEST")
EndTime <- force_tz(as.POSIXct("2015-03-17 11:00:00 AM", format = "%Y-%m-%d %H:%M:%OS"),"AEST")

filtered.results <- subset(logger.results, logger.results$TS >= StartTime & logger.results$TS <= EndTime)


# Display Results
plot.I1 <- ggplot(filtered.results, aes(x=TS, y=RMSI1,colour=FAULT)) + geom_point()
plot.PrFault <- ggplot(filtered.results, aes(x=TS, y=PrFault, ymin=0, ymax=PrFault)) + geom_ribbon()

grid.arrange(plot.I1, plot.PrFault, ncol=1)

# Threshold testing
rm(result)
result <- data.frame("Threshold"=0,"Negative"=0,"Positive"=0)

for (i in seq(0,1,by=0.01))
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


########################################################### Not needed yet ######################


# Thresholding
threshold <- .5
logger.results.threshold <- logger.results
logger.results.threshold$PrFault2 <- ifelse(logger.results.threshold$PrFault<0.5,0,logger.results.threshold$PrFault)

# Measure performance
result <- data.frame("Threshold"=0,"Negative"=0,"Positive"=0)
result.tmp <- logger.results.threshold %>%
  group_by(FAULT) %>%
  summarise (Score = sum(PrFault2))

result <- rbind(result,c(threshold,result.tmp$Score))
