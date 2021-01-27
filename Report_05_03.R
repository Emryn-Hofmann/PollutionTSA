library(tidyverse) # loads dplyr, ggplot2 and more
library(tseries) # used in tsa

X <- read.csv("pollution_us_2000_2016.csv") # Load in the data
X_Ca <- subset(X, State == 'California') # Subset the data to California
X_Ca$Date.Local <- as.Date(X_Ca$Date.Local) # Format the data in the date column as a date type.