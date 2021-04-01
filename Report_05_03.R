library(tidyverse) # loads dplyr, ggplot2 and more
library(tseries) # used in tsa
library(corrplot) # To plot the correlation between pollutants
library(imputeTS) # For filling in NAs
library(forecast)

# From http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



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

# To get weekly means
O3s_both$week_id <- rep(seq_len(length(O3s_both$Date.Local) / 7), each = 7)
O3s_both.weekly <- aggregate(cbind(O3.Mean.1103, O3.Mean.5005) ~ week_id, O3s_both, mean)
O3s_both.weekly <- mutate(O3s_both.weekly, Date.Local=as.Date(12533+(7*(week_id-1)))) %>% select(Date.Local, O3.Mean.1103, O3.Mean.5005)
NO2s_both$week_id <- rep(seq_len(length(NO2s_both$Date.Local) / 7), each = 7)
NO2s_both.weekly <- aggregate(cbind(NO2.Mean.1103, NO2.Mean.5005) ~ week_id, NO2s_both, mean)
NO2s_both.weekly <- mutate(NO2s_both.weekly, Date.Local=as.Date(12533+(7*(week_id-1)))) %>% select(Date.Local, NO2.Mean.1103, NO2.Mean.5005)

# Produce some time series objects
O3.1103.ts <- ts(O3s_both.weekly$O3.Mean.1103, frequency = 52)
O3.5005.ts <- ts(O3s_both.weekly$O3.Mean.5005, frequency = 52)
NO2.1103.ts <- ts(NO2s_both.weekly$NO2.Mean.1103, frequency = 52)
NO2.5005.ts <- ts(NO2s_both.weekly$NO2.Mean.5005, frequency = 52)

# the O3 mean from both sites (two colours), along with their smoothed average values
O3.BothSitesGraph <- ggplot(O3s_both, aes(x=Date.Local))+
  geom_line(aes(y=O3.Mean.1103), color="blue")+
  geom_line(aes(y=O3.Mean.5005), color="red")+
  geom_smooth(aes(y=O3.Mean.1103), color="cyan")+
  geom_smooth(aes(y=O3.Mean.5005), color="pink") +
  ylab("O3 Mean value") +
  xlab("Date")
O3.BothSitesGraph
adf.test(O3.1103.ts) # Stationary
adf.test(O3.5005.ts) # Stationary

# the NO2 mean from both sites (two colours), along with their smoothed average values
NO2.BothSitesGraph <- ggplot(NO2s_both, aes(x=Date.Local))+
  geom_line(aes(y=NO2.Mean.1103), color="blue")+
  geom_line(aes(y=NO2.Mean.5005), color="red")+
  geom_smooth(aes(y=NO2.Mean.1103), color="cyan")+
  geom_smooth(aes(y=NO2.Mean.5005), color="pink") +
  ylab("NO2 Mean value") +
  xlab("Date")
NO2.BothSitesGraph
adf.test(NO2.1103.ts) # Stationary
adf.test(NO2.5005.ts) # Stationary
multiplot(O3.BothSitesGraph, NO2.BothSitesGraph,cols=2) # To get a nice side-by-side plot in RStudio
#0.0213ppm NO2 - 21.3
#0.0509ppm O3 -50.9

# These plots show the 4 time series, along with their smoothed averages, and the WHO guideline average.
O3.1103Graph <- ggplot(O3s_both, aes(x=Date.Local))+
  geom_line(aes(y=O3.Mean.1103))+
  geom_smooth(aes(y=O3.Mean.1103))+
  geom_hline(yintercept =50.9, color="red")
O3.5005Graph <- ggplot(O3s_both, aes(x=Date.Local))+
  geom_line(aes(y=O3.Mean.5005))+
  geom_smooth(aes(y=O3.Mean.5005))+
  geom_hline(yintercept=50.9, color="red")
NO2.1103Graph <- ggplot(NO2s_both, aes(x=Date.Local))+
  geom_line(aes(y=NO2.Mean.1103))+
  geom_smooth(aes(y=NO2.Mean.1103))+
  geom_hline(yintercept=21.3, color="red")
NO2.5005Graph <- ggplot(NO2s_both, aes(x=Date.Local))+
  geom_line(aes(y=NO2.Mean.5005))+
  geom_smooth(aes(y=NO2.Mean.5005))+
  geom_hline(yintercept=21.3, color="red")
multiplot(O3.1103Graph,O3.5005Graph,NO2.1103Graph,NO2.5005Graph, cols=2)

#Split training and test data for all 4 time series:
N_1103_O3 <- length(O3.1103.ts)
N_1103_NO2 <- length(NO2.1103.ts)
N_5005_O3 <- length(O3.5005.ts)
N_5005_NO2 <- length(NO2.5005.ts)
#clearly, all the same length
ts.length <- length(O3.1103.ts)
test.length <- floor(0.1*ts.length)
train.length <- ceiling(0.9*ts.length)

# Tried it not diffed, but concluded they're not stationary, so needed to diff
O3.1103.acf = acf(O3.1103.ts[1:train.length], demean=FALSE, plot=TRUE, lag.max=2401)
O3.1103.pacf = pacf(O3.1103.ts[1:train.length], demean=FALSE, plot=TRUE, lag.max=2401)
O3.1103.diffed.acf = acf(diff(O3.1103.ts[1:train.length]), demean=FALSE, plot=TRUE, lag.max=2401)
# Interesting behaviour at regular lags, redo plot with default lag.max to inspect
O3.1103.diffed.pacf = pacf(diff(O3.1103.ts[1:train.length]), demean=FALSE, plot=TRUE, lag.max=2443)
# Quite noisy still, will try default lag.max to inspect
O3.1103.diffed.acf = acf(diff(O3.1103.ts[1:train.length]), demean=FALSE, plot=TRUE)
#Spikes at lags k=52n, i.e. yearly seasonality
O3.1103.diffed.pacf = pacf(diff(O3.1103.ts[1:train.length]), demean=FALSE, plot=TRUE)
# Definite seasonality

# Tried it not diffed, but concluded they're not stationary, so needed to diff
NO2.1103.acf = acf(NO2.1103.ts[1:train.length], demean=FALSE, plot=TRUE, lag.max=2401)
NO2.1103.pacf = pacf(NO2.1103.ts[1:train.length], demean=FALSE, plot=TRUE, lag.max=2401)
NO2.1103.diffed.acf = acf(diff(NO2.1103.ts[1:train.length]), demean=FALSE, plot=TRUE, lag.max=2401)
# Interesting behaviour at regular lags, redo plot with default lag.max to inspect
NO2.1103.diffed.pacf = pacf(diff(NO2.1103.ts[1:train.length]), demean=FALSE, plot=TRUE, lag.max=2401)
# Quite noisy still, will try default lag.max to inspect
NO2.1103.diffed.acf = acf(diff(NO2.1103.ts[1:train.length]), demean=FALSE, plot=TRUE)
#Spikes at lags k=52n, i.e. weekly seasonality
NO2.1103.diffed.pacf = pacf(diff(NO2.1103.ts[1:train.length]), demean=FALSE, plot=TRUE)
# Positives only at lags k=52n, but large negative lags elsewhere...

#Differencing with lag 52, to remove seasonality.
O3.1103.yearly.diff <- diff(O3.1103.ts[1:train.length], lag=52)
O3.5005.yearly.diff <- diff(O3.5005.ts[1:train.length], lag=52)
NO2.1103.yearly.diff <- diff(NO2.1103.ts[1:train.length], lag=52)
NO2.5005.yearly.diff <- diff(NO2.5005.ts[1:train.length], lag=52)

#Trying ACF & PACF again
O3.1103.acf = acf(O3.1103.yearly.diff, demean=FALSE, plot=TRUE)
O3.1103.pacf = pacf(O3.1103.yearly.diff, demean=FALSE, plot=TRUE)
# Indicates the following model on the diffed data - SARIMA(6,0,4,0,1,1,52)
mod1103O3 = arima(O3.1103.yearly.diff, order=c(6,0,4), seasonal = list(order = c(0,0,1), period = 52))
mod1103O3.tsdiag <- tsdiag(mod1103O3)
mod1103O3.pacf <- pacf(mod1103O3$residuals)
mod1103O3.acf <- acf(mod1103O3$residuals)
#which is this model on regular data - SARIMA(6,0,4,0,1,1,52)
mod1103O3 = arima(O3.1103.ts[1:train.length], order=c(6,0,4), seasonal = list(order = c(0,1,1), period = 52))
mod1103O3.tsdiag <- tsdiag(mod1103O3)
mod1103O3.pacf <- pacf(mod1103O3$residuals)
mod1103O3.acf <- acf(mod1103O3$residuals)
# Now we forecast and plot
mod1103O3 %>% forecast(h=test.length) %>% autoplot()+
  xlab("Year")+
  ylab("O3 weekly means for LA, site 1103 in ppm")+
  scale_x_continuous(breaks=c(37,89,142,194,246,298), labels=c("2005","2006","2007","2008","2009","2010"))
# Calculate the metrics
O3.1103.fc <- mod1103O3 %>% forecast(h=test.length)
O3.1103.fc <- O3.1103.fc$mean
test.O3.1103 <- as.vector(O3.1103.ts[310:343])
fc.O3.1103 <- as.vector(O3.1103.fc)
d = vector()
for (i in 1:34){ 
  d[i] = fc.O3.1103[i]-test.O3.1103[i] 
}
MAE.O3.1103 = mean(abs(d))
RMSE.O3.1103 = sqrt(mean(d^2))


#Now NO2 for 1103
#Trying ACF & PACF again
NO2.1103.acf = acf(NO2.1103.yearly.diff, demean=FALSE, plot=TRUE)
NO2.1103.pacf = pacf(NO2.1103.yearly.diff, demean=FALSE, plot=TRUE)
# Indicates the following model on the diffed data - SARIMA(1,0,0,0,1,1,52)
mod1103NO2 = arima(NO2.1103.yearly.diff, order=c(1,0,0), seasonal = list(order = c(0,0,1), period = 52))
mod1103NO2.tsdiag <- tsdiag(mod1103NO2)
mod1103NO2.pacf <- pacf(mod1103NO2$residuals)
mod1103NO2.acf <- acf(mod1103NO2$residuals)
#which is this model on regular data - SARIMA(1,0,0,0,1,1,52)
mod1103NO2 = arima(NO2.1103.ts[1:train.length], order=c(1,0,0), seasonal = list(order = c(0,1,1), period = 52))
mod1103NO2.tsdiag <- tsdiag(mod1103NO2)
mod1103NO2.pacf <- pacf(mod1103NO2$residuals)
mod1103NO2.acf <- acf(mod1103NO2$residuals)
# Now we forecast and plot
mod1103NO2 %>% forecast(h=test.length) %>% autoplot()+
  xlab("Year")+
  ylab("NO2 weekly means for LA, site 1103 in ppm")+
  scale_x_continuous(breaks=c(37,89,142,194,246,298), labels=c("2005","2006","2007","2008","2009","2010"))
# Calculate the metrics
NO2.1103.fc <- mod1103NO2 %>% forecast(h=test.length)
NO2.1103.fc <- NO2.1103.fc$mean
test.NO2.1103 <- as.vector(NO2.1103.ts[310:343])
fc.NO2.1103 <- as.vector(NO2.1103.fc)
d = vector()
for (i in 1:34){ 
  d[i] = fc.NO2.1103[i]-test.NO2.1103[i] 
}
MAE.NO2.1103 = mean(abs(d))
RMSE.NO2.1103 = sqrt(mean(d^2))

# Tried it not diffed, but concluded they're not stationary, so needed to diff
O3.5005.acf = acf(O3.5005.ts[1:train.length], demean=FALSE, plot=TRUE, lag.max=2401)
O3.5005.pacf = pacf(O3.5005.ts[1:train.length], demean=FALSE, plot=TRUE, lag.max=2401)
O3.5005.diffed.acf = acf(diff(O3.5005.ts[1:train.length]), demean=FALSE, plot=TRUE, lag.max=2401)
# Interesting behaviour at regular lags, redo plot with default lag.max to inspect
O3.5005.diffed.pacf = pacf(diff(O3.5005.ts[1:train.length]), demean=FALSE, plot=TRUE, lag.max=2443)
# Quite noisy still, will try default lag.max to inspect
O3.5005.diffed.acf = acf(diff(O3.5005.ts[1:train.length]), demean=FALSE, plot=TRUE)
#Spikes at lags k=52n, i.e. yearly seasonality
O3.5005.diffed.pacf = pacf(diff(O3.5005.ts[1:train.length]), demean=FALSE, plot=TRUE)
# Definite seasonality

# Tried it not diffed, but concluded they're not stationary, so needed to diff
NO2.5005.acf = acf(NO2.5005.ts[1:train.length], demean=FALSE, plot=TRUE, lag.max=2401)
NO2.5005.pacf = pacf(NO2.5005.ts[1:train.length], demean=FALSE, plot=TRUE, lag.max=2401)
NO2.5005.diffed.acf = acf(diff(NO2.5005.ts[1:train.length]), demean=FALSE, plot=TRUE, lag.max=2401)
# Interesting behaviour at regular lags, redo plot with default lag.max to inspect
NO2.5005.diffed.pacf = pacf(diff(NO2.5005.ts[1:train.length]), demean=FALSE, plot=TRUE, lag.max=2401)
# Quite noisy still, will try default lag.max to inspect
NO2.5005.diffed.acf = acf(diff(NO2.5005.ts[1:train.length]), demean=FALSE, plot=TRUE)
#Spikes at lags k=52n, i.e. weekly seasonality
NO2.5005.diffed.pacf = pacf(diff(NO2.5005.ts[1:train.length]), demean=FALSE, plot=TRUE)
# Positives only at lags k=52n, but large negative lags elsewhere...

#Trying ACF & PACF again
O3.5005.acf = acf(O3.5005.yearly.diff, demean=FALSE, plot=TRUE)
O3.5005.pacf = pacf(O3.5005.yearly.diff, demean=FALSE, plot=TRUE)
# Indicates the following model on the diffed data - SARIMA(6,0,0,0,1,1,52)
mod5005O3 = arima(O3.5005.yearly.diff, order=c(6,0,0), seasonal = list(order = c(0,0,1), period = 52))
mod5005O3.tsdiag <- tsdiag(mod5005O3)
mod5005O3.pacf <- pacf(mod5005O3$residuals)
mod5005O3.acf <- acf(mod5005O3$residuals)
#which is this model on regular data - SARIMA(1,0,0,0,1,1,52)
mod5005O3 = arima(O3.5005.ts[1:train.length], order=c(6,0,0), seasonal = list(order = c(0,1,1), period = 52))
mod5005O3.tsdiag <- tsdiag(mod5005O3)
mod5005O3.pacf <- pacf(mod5005O3$residuals)
mod5005O3.acf <- acf(mod5005O3$residuals)
# Now we forecast and plot
mod5005O3 %>% forecast(h=test.length) %>% autoplot()+
  ylab("O3 weekly means for LA, site 5005 in ppm")+
  scale_x_continuous(breaks=c(37,89,142,194,246,298), labels=c("2005","2006","2007","2008","2009","2010"))+
  xlab("Year")
# Calculate the metrics
O3.5005.fc <- mod5005O3 %>% forecast(h=test.length)
O3.5005.fc <- O3.5005.fc$mean
test.O3.5005 <- as.vector(O3.5005.ts[310:343])
fc.O3.5005 <- as.vector(O3.5005.fc)
d = vector()
for (i in 1:34){ 
  d[i] = fc.O3.5005[i]-test.O3.5005[i] 
}
MAE.O3.5005 = mean(abs(d))
RMSE.O3.5005 = sqrt(mean(d^2))