library(tidyverse) # loads dplyr, ggplot2 and more
library(tseries) # used in tsa
library(corrplot) # To plot the correlation between pollutants

X <- read.csv("pollution_us_2000_2016.csv") # Load in the data
X_Ca <- subset(X, State == 'California') # Subset the data to California
X_Ca$Date.Local <- as.Date(X_Ca$Date.Local) # Format the data in the date column as a date type.

# Make a correlation plot to spot interactions between the pollutants
X_Ca_Mean_corr <- select(X_Ca, NO2.Mean, O3.Mean, SO2.Mean, CO.Mean) %>%
  drop_na()
cor(X_Ca_Mean_corr)
corrplot(cor(X_Ca_Mean_corr), method = "circle")
# Might want to look at NO2 and O3, since these have the most data (no NAs)
# Also a negative correlation between them


X_Ca_NO2_and_O3 <- select(X_Ca, Site.Num, County.Code, Address, Date.Local, NO2.Mean,
                   O3.Mean) # Select specific columns of interest
X_Ca_NO2_and_O3 <- distinct(X_Ca_NO2_and_O3) # Get the distinct values, due to repetition
# (because other pollutants had different sampling rates)

#Plot the O3 and NO2 means from the various sites
ggplot(X_Ca_NO2_and_O3, aes(x=Date.Local)) +
  geom_line(aes(y=O3.Mean)) + 
  facet_wrap(~Site.Num)
ggplot(X_Ca_NO2_and_O3, aes(x=Date.Local)) +
  geom_line(aes(y=NO2.Mean)) + 
  facet_wrap(~Site.Num)

# We can see that not all sites were active across the 16 years, and that
# the gas levels vary by site


# Selecting two sites of interest
X_CoolSites <- filter(X_Ca_NO2_and_O3, Site.Num==1103| Site.Num==5005)

# Plotting the sites again, but this time with only the two sites of interest.
ggplot(X_CoolSites, aes(x=Date.Local)) +
  geom_line(aes(y=NO2.Mean)) + 
  facet_wrap(~Site.Num)
ggplot(X_CoolSites, aes(x=Date.Local)) +
  geom_line(aes(y=O3.Mean)) + 
  facet_wrap(~Site.Num)
# From these we can see that we are missing a number of values 
# around 2012, so we decide to only look at data up to those missing values