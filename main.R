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

# Threasholding


# Measure performance
results <- filtered.results %>%
  group_by(FAULT) %>%
  summarise (Score = sum(PrFault))
