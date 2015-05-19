#########################################################
# Chebyschev Implementation 
#
# Calculate non-fault mean and std dev over a sliding 
# window and look for deviations from this to detect
# faults
# 
# - Aaron Cousland 01/05/2015
########################################################

require (RODBC)       # Load RODBC package
require (lubridate)   # Required to manipulate dates
require (ggplot2)     # Required for nice graphs 
require (gridExtra)   # For further graph manipulation
require (dplyr)       # Required for performance measurement

source ('Chebyschev_Function.R')
source ('Threshold_Optimisation.R')

# Create a connection to the database called "RTV"
odbcCloseAll()
local.connection <- odbcConnect("RTV", believeNRows=FALSE)

# Query the database and put the results into the data frame logging.results
logger.results <- sqlQuery(local.connection,"SELECT * FROM ELSPEC.RMS_TRAINING where ts between '17/Mar/15 08:00:00 AM' and '17/Mar/15 05:00:00 PM';")
odbcCloseAll()

#Order by timestamp and force local timestamp
logger.results <- logger.results[with(logger.results, order(logger.results$TS)),]
logger.results$TS <- force_tz(logger.results$TS,"UTC")

# Initialise probability of fault column and window length
logger.results$PrFault <- 0
window.length = 1440

# Superimpose load current on fault phase
#logger.results$RMSI1 <- logger.results$RMSI1 + logger.results$RMSI2 

# Run Chebyschev analysis on results
logger.results <- Chebyschev(logger.results,window.length)

# Optimise the trigger threshold
results <- Threshold_Optimise(logger.results,0,1,0.01)
threshold <- results[which.max(results[,4]),1]
plot(results$Threshold,results$success)

# Perform thresholding as per optimum value
logger.results.threshold <- logger.results
logger.results.threshold$PrFault2 <- ifelse(logger.results.threshold$PrFault<threshold,0,1)

# Measure performance
performance <- logger.results.threshold %>%
  group_by(FAULT) %>%
  summarise (Score = sum(PrFault2))

print(performance$Score)
print(paste("Score =",performance$Score[2]-performance$Score[1],"/",sum(logger.results$FAULT==TRUE)))

# Interrogate results
StartTime <- force_tz(as.POSIXct("2015-03-17 13:18:00", format = "%Y-%m-%d %H:%M:%OS"),"UTC")
EndTime <- force_tz(as.POSIXct("2015-03-17 13:19:00", format = "%Y-%m-%d %H:%M:%OS"),"UTC")
logger.results.threshold2 <- subset(logger.results.threshold, logger.results.threshold$TS >= StartTime & logger.results.threshold$TS <= EndTime)
plot(logger.results.threshold2$TS,logger.results.threshold2$RMSI1, type="l")
polygon(logger.results.threshold2$TS,logger.results.threshold2$PrFault2*max(logger.results.threshold2$RMSI1), col =rgb(1,0,0,alpha=0.3),xlab="",ylab="",yaxt="n",border = NA)
axis(4)

