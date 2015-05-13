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
odbcCloseAll()
local.connection <- odbcConnect("RTV", believeNRows=FALSE)

# Query the database and put the results into the data frame logging.results
logger.results <- sqlQuery(local.connection,"SELECT * FROM ELSPEC.RMS_TRAINING where ts between '17/Mar/15 08:00:00 AM' and '17/Mar/15 11:00:00 PM';")
odbcCloseAll()
rm(local.connection)

# Order by timestamp and force local timestamp
logger.results <- logger.results[with(logger.results, order(logger.results$TS)),]
logger.results$TS <- force_tz(logger.results$TS,"AEST")

# Initialise probability of fault column
logger.results$PrFault <- 0

# Loop to calculate the window length with the highest sucess rate
for (i in seq(5,11,by=0.5))
{
  window.length = 10
  
  # Run Chebyschev analysis on results
  ptm <- proc.time()
  logger.results <- Chebyschev(logger.results,window.length)
  
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
  
  print(t(performance$Score))
  print(paste("Score =",performance$Score[2]-performance$Score[1],"/",sum(logger.results$FAULT==TRUE)))
  
  time.elepsed <- proc.time() - ptm
  write.table(t(c(window.length,performance$Score,performance$Score[2]-performance$Score[1],time.elepsed[3])),file="Window_Optimisation_Results.csv", sep=",",append=T, row.names=F, col.names = F)
}
  


