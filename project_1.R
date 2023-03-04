## Project 1
## Author: Kyle Chmielewski
## Date: March 1, 2023

#* Project Description:
#* 
#* The goal of this project is to showcase the ability of Monte Carlo Simulations to be used for 
#* financial modeling. In this specific case, I will be using a Monte Carlo simulation to calculate
#* the expected return and risk of different types of portfolios. Using this information I will compare
#* the efficacy of individual stock portfolios vs. ETF portfolios. 

# Code for Single Stock Portfolio
install.packages("tidyquant")

# importing needed packages
library(tidyquant) 
library(ggplot2) 

# Define the number of simulations to run
num_sims <- 1000

# Define the number of years in the future to simulate returns for
years <- 5

# Define the number of stocks in the portfolio
num_stocks <- 10

#define weights of portfolio
weight <- 1 / num_stocks
weight_matrix <- matrix(weight, num_stocks, 1)


# Define the start and end dates for the historical stock price data
start_date <- "2013-01-01"
end_date <- "2023-03-2"

# Define the ticker symbols for the stocks in the portfolio
# Top Ten US companies by Market Capitalization
tickers <- c("AAPL", "GOOG", "MSFT", "AMZN", "JPM", "TSLA", "NVDA", "V", "META", "XOM")

# Create a new environment to store the getSymbols() results
stock_portf <- new.env()

# Import ticker data, storing it in the 'stock_portf' environment
getSymbols(tickers, src = 'yahoo', from = start_date, to = end_date, env = stock_portf)

# Loop over each object in 'stock_portf',
# and calculate the Daily adjusted close return for the last year
returns <- lapply(stock_portf, function(x) { periodReturn(tail(Ad(x), 2558), 'daily') })

# Merge the daily returns into one xts object
returns <- setNames(Reduce(merge, returns), names(returns))

# Calculate the mean and covariance matrix of the returns
mean_returns <- colMeans(returns)
cov_matrix <- cov(returns)
sd_returns <- sd(returns)


# Monte Carlo simulation for Stock Portfolio

# Defining Number of Days in the Future to project to 
no_of_days <- 365 * years

# Defining the Starting Balance of the Portfolio
starting_balance <- 100000

# Defining the amount of money allocated to each security
money_per_asset <- starting_balance / num_stocks

# Defining the starting amount of money held in each stock
Apple_holdings <- round((money_per_asset / stock_portf$AAPL$AAPL.Close[2023-03-1]), 0) * stock_portf$AAPL$AAPL.Close[2023-03-1]
Amazon_holdings <- round(money_per_asset / stock_portf$AMZN$AMZN.Close[2023-03-1], 0) * stock_portf$AMZN$AMZN.Close[2023-03-1]
Google_holdings <- round(money_per_asset / stock_portf$GOOG$GOOG.Close[2023-03-1], 0) * stock_portf$GOOG$GOOG.Close[2023-03-1]
Microsoft_holdings <- round(money_per_asset / stock_portf$MSFT$MSFT.Close[2023-03-1], 0) * stock_portf$MSFT$MSFT.Close[2023-03-1]
JPM_holdings <- round(money_per_asset / stock_portf$JPM$JPM.Close[2023-03-1], 0) * stock_portf$JPM$JPM.Close[2023-03-1]
Meta_holdings <- round(money_per_asset / stock_portf$META$META.Close[2023-03-1], 0) * stock_portf$META$META.Close[2023-03-1]
Nvidia_holdings <- round(money_per_asset / stock_portf$NVDA$NVDA.Close[2023-03-1], 0) * stock_portf$NVDA$NVDA.Close[2023-03-1]
TSLA_holdings <- round(money_per_asset / stock_portf$TSLA$TSLA.Close[2023-03-1], 0) * stock_portf$TSLA$TSLA.Close[2023-03-1]
Visa_holdings <- round(money_per_asset / stock_portf$V$V.Close[2023-03-1], 0) * stock_portf$V$V.Close[2023-03-1]
Exxon_holdings <- round(money_per_asset /stock_portf$XOM$XOM.Close[2023-03-1], 0) * stock_portf$XOM$XOM.Close[2023-03-1]


# Combining all above holdings to show total portfolio starting value
starting_price <- Apple_holdings + Amazon_holdings + Google_holdings + Microsoft_holdings + JPM_holdings + Meta_holdings + Nvidia_holdings + TSLA_holdings + Visa_holdings + Exxon_holdings
starting_price <- as.integer(starting_price)


# Monte Carlo Simulation
set.seed(101)
returns_list <- matrix(0, nrow = num_sims, ncol = no_of_days) # define returns matrix
prices_list <- matrix(0, nrow = num_sims, ncol = no_of_days+1)  # define prices matrix

# Loop to run 1000 simulations
for(i in 1:num_sims) { # for loop - 1001 iterations
  returns_list[i,] <- rnorm(no_of_days, mean=mean_returns, sd=sd_returns) #Generate random variables
  prices_list[i,] <- cumprod(c(starting_price, 1+returns_list[i,]))#Calculate cumulative product
}

#Evaluating Returns
total_returns <- array(NA, dim= num_sims, dimnames=NULL)
for (i in 1:num_sims) {
  total_returns[i] <- (prices_list[i, no_of_days + 1]-prices_list[i, 1])/prices_list[i,1] #calculate total % return for each 365 day simulation
}

# Summary Statistics
summary(total_returns)

# calculating Mean
mean_totalReturn <- mean(total_returns)
print(mean_totalReturn)

# Standard Deviation 
sd_totalReturn <- sd(total_returns)
print(sd_totalReturn)

# Variance
var_totalReturn <- var(total_returns)
print(var_totalReturn)

# calculating a confidence interval
sample_length <- length(total_returns)
sample_error <- mean_totalReturn / sample_length
alpha = 0.05
degrees.freedom = sample_length - 1
t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
print(t.score)

margin.error <- t.score * sample_error
lower.bound <- mean_totalReturn - margin.error
upper.bound <- mean_totalReturn + margin.error
print(c(lower.bound,upper.bound))

# Graph Best, Worst, and Median
# Calculating the Max
max <- which.max(total_returns)

# Calculating the Min
min <- which.min(total_returns)

# Calculating the median
new_data <- order(prices_list[,no_of_days + 1])
median_index <- new_data[num_sims / 2]

# Graphing Best, Median, and Worst
plot(prices_list[max,], type = 'l', xlab = "Days", ylab = "Portfolio Value", main = "Simulated Values of Best, Median, and Worst Portfolios", col = 'green',ylim = c(prices_list[min,no_of_days + 1], prices_list[max,no_of_days+1]))
lines(prices_list[min,], type = 'l', col = 'red')
lines(prices_list[median_index,], type = 'l', col = 'orange')

# Histogram
ggplot(data.frame(total_returns), aes(x = total_returns)) +
  geom_histogram(binwidth = 2, color = "black", fill = "blue", alpha = 0.5) +
  labs(x = "Simulated Return", y = "Count", title = "Monte Carlo Simulation of Stock Portfolio Returns")

#* ETF Portfolio
#* 
#* ETFs is short for Exchange Traded Funds, which are mangaed funds that are able to be purchased like a stock, on the common 
#* stock exchanges such as NYSE or Nasdaq.
#* 

# Define the number of simulations to run
num_sims <- 1000

# Define the number of years in the future to simulate returns for
years <- 5

# Define the number of stocks in the portfolio
num_etfs <- 5

#define weights of portfolio
weight <- 1 / num_etfs
weight_matrix <- matrix(weight, num_etfs, 1)

# Define the start and end dates for the historical stock price data
start_date <- "2013-01-01"
end_date <- "2023-03-2"

# Define the ticker symbols for the stocks in the portfolio
# Top Ten US companies by Market Capitalization
tickers <- c("VTI", "QQQ", "SPY", "VEA", "VOO")

# Create a new environment to store the getSymbols() results
ETF_portf <- new.env()

# Import ticker data, storing it in the 'ETF_portf' environment
getSymbols(tickers, src = 'yahoo', from = start_date, to = end_date, env = ETF_portf)

# Loop over each object in 'ETF_portf',
# and calculate the Daily adjusted close return for the last year
returns <- lapply(ETF_portf, function(x) { periodReturn(tail(Ad(x), 2558), 'daily') })

# Merge the daily returns into one xts object
returns <- setNames(Reduce(merge, returns), names(returns))

# Calculate the mean and covariance matrix of the returns
mean_returns <- colMeans(returns)
cov_matrix <- cov(returns)
sd_returns <- sd(returns)

# Monte Carlo simulation for ETF Portfolio

# Defining Number of Days in the Future to project to 
no_of_days <- 365 * years

# Defining the Starting Balance of the Portfolio
starting_balance <- 100000

# Defining the amount of money allocated to each security
money_per_asset <- starting_balance / num_etfs

# Defining the starting amount of money held in each stock
VTI_holdings <- round((money_per_asset / ETF_portf$VTI$VTI.Close[2023-03-1]), 0) * ETF_portf$VTI$VTI.Close[2023-03-1]
QQQ_holdings <- round((money_per_asset / ETF_portf$QQQ$QQQ.Close[2023-03-1]), 0) * ETF_portf$QQQ$QQQ.Close[2023-03-1]
SPY_holdings <- round((money_per_asset / ETF_portf$SPY$SPY.Close[2023-03-1]), 0) * ETF_portf$SPY$SPY.Close[2023-03-1]
VEA_holdings <- round((money_per_asset / ETF_portf$VEA$VEA.Close[2023-03-1]), 0) * ETF_portf$VEA$VEA.Close[2023-03-1]
VOO_holdings <- round((money_per_asset / ETF_portf$VOO$VOO.Close[2023-03-1]), 0) * ETF_portf$VOO$VOO.Close[2023-03-1]

# Combinging all above holdings to show total portfolio starting value
starting_price <- VTI_holdings + QQQ_holdings + SPY_holdings + VEA_holdings + VOO_holdings
starting_price <- as.integer(starting_price)


# Monte Carlo Simulation

set.seed(101) # Set seed for reproducibility of the random numbers

returns_list <- matrix(0, nrow = num_sims, ncol = no_of_days) # define returns matrix
prices_list <- matrix(0, nrow = num_sims, ncol = no_of_days+1)  # define prices matrix


# Loop to run 1000 simulations
for(i in 1:num_sims) { # for loop - 1001 iterations
  returns_list[i,] <- rnorm(no_of_days, mean=mean_returns, sd=sd_returns) #Generate random variables
  prices_list[i,] <- cumprod(c(starting_price, 1+returns_list[i,]))#Calculate cumulative product
}

#Evaluating Returns
total_returns <- array(NA, dim= num_sims, dimnames=NULL)
for (i in 1:num_sims) {
  total_returns[i] <- (prices_list[i, no_of_days+1]-prices_list[i, 1])/prices_list[i,1] #calculate total % return for each 120 day simulation
}

# Summary Statistics
summary(total_returns)

# Mean
mean_totalReturn <- mean(total_returns)

# Standard Deviation 
sd_totalDeviation <- sd(total_returns)
print(sd_totalDeviation)

# Variance
var_totalReturn <- var(total_returns)
print(var_totalReturn)

# calculating a confidence interval
sample_length <- length(total_returns)
sample_error <- mean_totalReturn / sample_length
alpha = 0.05
degrees.freedom = sample_length - 1
t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
print(t.score)

margin.error <- t.score * sample_error
lower.bound <- mean_totalReturn - margin.error
upper.bound <- mean_totalReturn + margin.error
print(c(lower.bound,upper.bound))

# Graph Best, Worst, and Median
# Calculating the Max
max <- which.max(total_returns)

# Calculating the Min
min <- which.min(total_returns)

# Calculating the median
new_data <- order(prices_list[,no_of_days+1])
median_index <- new_data[num_sims / 2]

# Graphing Best, Median, and Worst
plot(prices_list[max,], type = 'l', xlab = "Days", ylab = "Portfolio Value", main = "Simulated Values of Best, Median, and Worst Portfolios", col = 'green',ylim = c(prices_list[min,no_of_days+1], prices_list[max,no_of_days+1]))
lines(prices_list[min,], type = 'l', col = 'red')
lines(prices_list[median_index,], type = 'l', col = 'orange')

# Histogram
ggplot(data.frame(total_returns), aes(x = total_returns)) +
  geom_histogram(binwidth = 0.1, color = "black", fill = "blue", alpha = 0.5) +
  labs(x = "Simulated Return", y = "Count", title = "Monte Carlo Simulation of ETF Portfolio Returns")