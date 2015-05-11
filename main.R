#########################################################
# Chebyschev Trial 1 
#
# Hypothesis:
# Fault current belongs to a different statistical 
# distribution than non-fault current
#
# Method:
# Calculate non-fault mean and std dev over a sliding 
# window and look for deviations from this to detect
# faults
# 
# - Aaron Cousland 01/05/2015
########################################################

require (RODBC)        # Load RODBC package
require (lubridate)   # Required to manipulate dates

# Create a connection to the database called "channel"
local.connection <- odbcConnect("RTV", believeNRows=FALSE)

# Query the database and put the results into the data frame "LoggingResults"
logger.results <- sqlQuery(local.connection, 
                           "SELECT DATEANDTIME, SECONDS, RMSI1, RMSI3 FROM ELSPEC_LOGGER_HARMONICS;")

logger.results$TIMESTAMP <- dmy_hm(logger.results$DATEANDTIME) + as.numeric(logger.results$SECONDS)

#Order by timestamp
logger.results <- logger.results[with(logger.results, order(logger.results$TIMESTAMP)),]

# Set filtered data subset times and subset the data
StartTime <- as.POSIXct("2015-03-17 10:00:00", format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
EndTime <- as.POSIXct("2015-03-17 11:00:00", format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")

filtered.results <- subset(logger.results, logger.results$TIMESTAMP >= StartTime & logger.results$TIMESTAMP <= EndTime)

# Initialise probability of fault column and window length
filtered.results$PrFault <- 0
window.length = 5000
sensitivity = .45

# Run Chebyschev analysis on results
filtered.results <- Chebyschev(filtered.results,window.length,sensitivity)

# Display Results
par(mfcol=c(2,1))
plot(filtered.results$TIMESTAMP,filtered.results$PrFault)
plot(filtered.results$TIMESTAMP,filtered.results$RMSI1)

# Zoom and view
StartTime2 <- as.POSIXct("2015-03-17 10:30:00", format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
EndTime2 <- as.POSIXct("2015-03-17 11:35:00", format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")

filtered.results2 <- subset(filtered.results, filtered.results$TIMESTAMP >= StartTime2 & filtered.results$TIMESTAMP <= EndTime2)
par(mfcol=c(2,1))
plot(filtered.results2$TIMESTAMP,filtered.results2$PrFault)
plot(filtered.results2$TIMESTAMP,filtered.results2$RMSI1)
