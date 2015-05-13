#########################################################
# Window optimisation program
#
# Independent program that does not 
# form part of main.R
#
# Run unsupervised to optimise the window length
# for the given data set.
#
########################################################

require (RODBC)       # Load RODBC package
require (lubridate)   # Required to manipulate dates
require (ggplot2)     # Required for nice graphs 
require (gridExtra)   # For further graph manipulation
require (dplyr)       # Required for performance measurement

source ('Chebyschev_Function.R')
source ('Threshold_Optimisation.R')

# Create a connection to the database called "RTV"
local.connection <- odbcConnect("RTV", believeNRows=FALSE)

# Query the database and put the results into the data frame logging.results
logger.results <- sqlQuery(local.connection,"SELECT * FROM ELSPEC.RMS_TRAINING where ts between '17/Mar/15 08:00:00 AM' and '17/Mar/15 03:00:00 PM';")

#Order by timestamp and force local timestamp
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

# Optimise the trigger threshold
results <- Threshold_Optimise(logger.results,0,1,0.01)
threshold <- results[which.max(results[,4]),1]
plot(results$Threshold,results$success)

# Perform thresholding as per optimum value
logger.results.threshold <- logger.results
logger.results.threshold$PrFault2 <- ifelse(logger.results.threshold$PrFault<threshold,0,logger.results.threshold$PrFault)

# Measure performance
performance <- logger.results.threshold %>%
  group_by(FAULT) %>%
  summarise (Score = sum(PrFault2))

print(performance)
print(paste("Score =",performance$Score[2]-performance$Score[1],"/",sum(logger.results$FAULT==TRUE)))

# Interrogate results
StartTime <- force_tz(as.POSIXct("2015-03-17 08:45:00", format = "%Y-%m-%d %H:%M:%OS"),"AEST")
EndTime <- force_tz(as.POSIXct("2015-03-17 11:00:00", format = "%Y-%m-%d %H:%M:%OS"),"AEST")
logger.results.threshold2 <- subset(logger.results.threshold, logger.results.threshold$TS >= StartTime & logger.results.threshold$TS <= EndTime)
plot(logger.results.threshold2$TS,logger.results.threshold2$RMSI1, type="l")
polygon(logger.results.threshold2$TS,logger.results.threshold2$PrFault2*max(logger.results.threshold2$RMSI1), col =rgb(1,0,0,alpha=0.3),xlab="",ylab="",yaxt="n",border = NA)
axis(4)
