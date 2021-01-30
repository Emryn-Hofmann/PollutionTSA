library(tidyverse) # loads dplyr, ggplot2 and more
library(tseries) # used in tsa
library(corrplot) # To plot the correlation between pollutants
library(imputeTS) # For filling in NAs
library(forecast)

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

# Get the individual Time Series, and get them the same units
X_NO2_1103 <- filter(X_CoolSites, Site.Num==1103) %>% select(Date.Local, NO2.Mean)
X_NO2_5005 <- filter(X_CoolSites, Site.Num==5005) %>% select(Date.Local, NO2.Mean)
X_O3_1103 <- filter(X_CoolSites, Site.Num==1103) %>% select(Date.Local, O3.Mean) %>% transform(O3.Mean=1000*O3.Mean)
X_O3_5005 <- filter(X_CoolSites, Site.Num==5005) %>% select(Date.Local, O3.Mean) %>% transform(O3.Mean=1000*O3.Mean)

# combining the two sites' O3s into one df
O3s_both <- merge(X_O3_1103,X_O3_5005, by="Date.Local") %>%
  rename(O3.Mean.1103=O3.Mean.x) %>%
  rename(O3.Mean.5005=O3.Mean.y)

O3s_both <- O3s_both %>% complete(Date.Local = seq.Date(
  min(Date.Local), 
  max(Date.Local), by="day")) # get NAs for missing dates

# combining the two sites' NO2s into one df
NO2s_both <- merge(X_NO2_1103,X_NO2_5005, by="Date.Local") %>%
  rename(NO2.Mean.1103=NO2.Mean.x) %>%
  rename(NO2.Mean.5005=NO2.Mean.y)

NO2s_both <- NO2s_both %>% complete(Date.Local = seq.Date(
  min(Date.Local), 
  max(Date.Local), by="day")) # get NAs for missing dates

# the O3 mean from both sites (two colours), along with their smoothed values
O3.BothSitesGraph <- ggplot(O3s_both, aes(x=Date.Local))+
  geom_line(aes(y=O3.Mean.1103), color="blue")+
  geom_line(aes(y=O3.Mean.5005), color="red")+
  geom_smooth(aes(y=O3.Mean.1103), color="cyan")+
  geom_smooth(aes(y=O3.Mean.5005), color="pink") +
  ylab("O3 Mean value") +
  xlab("Date")
O3.BothSitesGraph

# the NO2 mean from both sites (two colours), along with their smoothed values
NO2.BothSitesGraph <- ggplot(NO2s_both, aes(x=Date.Local))+
  geom_line(aes(y=NO2.Mean.1103), color="blue")+
  geom_line(aes(y=NO2.Mean.5005), color="red")+
  geom_smooth(aes(y=NO2.Mean.1103), color="cyan")+
  geom_smooth(aes(y=NO2.Mean.5005), color="pink") +
  ylab("NO2 Mean value") +
  xlab("Date")
NO2.BothSitesGraph

O3.finding <- filter(O3s_both, O3s_both$Date.Local>="2004-04-24" & O3s_both$Date.Local<"2010-11-20")
statsNA(O3.finding$O3.Mean.5005) # Checking for missing values, only 8.5%
statsNA(O3.finding$O3.Mean.1103) # ditto

NO2.finding <- filter(NO2s_both, NO2s_both$Date.Local>="2004-04-24" & NO2s_both$Date.Local<"2010-11-20")
statsNA(NO2.finding$NO2.Mean.5005) # Checking for missing values, only 8.5%
statsNA(NO2.finding$NO2.Mean.1103) # ditto

# So we subset the data, and only do analysis on the data between 24th April 2004 and 31st December 2010
# And impute the other missing values
O3s_both <- filter(O3s_both, O3s_both$Date.Local>="2004-04-24" & O3s_both$Date.Local<"2010-11-20") %>% na_kalman()
NO2s_both <- filter(NO2s_both, NO2s_both$Date.Local>="2004-04-24" & NO2s_both$Date.Local<"2010-11-20")  %>% na_kalman()