Chebyschev <- function(data,window.length=1000)
{
  for (row in 1:(nrow(data)-window.length))
  {
    # Extract analysis window from dataset
    window <- data[(1+row):((window.length)+row),] 
    
    # Work out the number of std devs point is off from window
    point.deviation <- abs(mean(window$RMSI1)-window$RMSI1[window.length])/(sd(window$RMSI1))
    
    # Get rid of the std deviations below 1. Chebyschev doesn't like them
    if(point.deviation <= 1)
    {
      point.deviation=1
    }
    
    # Calculate the probability the point has a fault
    pr.fault <- (1-(1/(point.deviation^2)))
    
    # Write probability back to data frame
    data$PrFault[window.length+row] <- pr.fault
  }
  return(data)
}
  
