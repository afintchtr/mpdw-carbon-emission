# Import Libraries
library(readxl)
library(forecast)
library(TTR)
library(graphics)

# Read data
data <- read_excel("co2_emission_idn.xlsx")
head(data)

# Split data into train and test data
data.train = data[1:42,]
data.test = data[43:59,]
data.train.ts=ts(data.train[,2])
data.test.ts=ts(data.test[,2])
class(data.train.ts)
class(data.test.ts)

# Find the best alpha parameter
list_alpha = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99, 0.999, 0.99999, 1)
list_beta = c()
list_mape_train = c()
list_mad_train = c()
list_mse_train = c()
list_mape_test = c()
list_mad_test = c()
list_mse_test = c()

for(alpha in list_alpha)
{
  data.train.des=HoltWinters(alpha=alpha, data.train.ts, gamma=FALSE)
  data.train.des
  data.train.des$fitted[,2]
  # Count error
  e.train = data.train.des$fitted[,2] - data.train.ts
  n.train = length(data.train.ts)
  MAD.train = sum(abs(e.train))/n.train
  MSE.train = data.train.des$SSE/n.train
  MAPE.train = (sum(abs(e.train)/data.train.ts)/n.train)*100
  list_mad_train <- c(list_mad_train, MAD.train)
  list_mse_train <- c(list_mse_train, MSE.train)
  list_mape_train <- c(list_mape_train, MAPE.train)
  list_beta <- c(list_beta, data.train.des$beta)
  
  # Forecast training data
  train.ramal=forecast(data.train.des, h=6)
  train.ramal
  plot(train.ramal)
  
  trainramal = as.numeric(train.ramal$mean)
  trainramal = ts(trainramal)
  
  # Forecast testing data
  e.test = trainramal-data.test.ts
  n.test = length(data.test.ts)
  MAD.test = sum(abs(e.test))/n.test
  MSE.test = sum(e.test^2)/n.test
  MAPE.test = (sum(abs(e.test)/data.test.ts)/n.test)*100
  list_mad_test <- c(list_mad_test, MAD.test)
  list_mse_test <- c(list_mse_test, MSE.test)
  list_mape_test <- c(list_mape_test, MAPE.test)
}

# Create and show compared dataframe of error
df_error_train <- data.frame(list_alpha, list_beta, list_mad_train, list_mape_train, list_mse_train)
df_error_test <- data.frame(list_alpha, list_beta, list_mad_test, list_mape_test, list_mse_test)


# Forecast for all data
data.ts=ts(data$co2_emissions)
data.des=HoltWinters(data.ts, alpha=0.6, beta=0.5846006, gamma=FALSE)
data.des
data.des$fitted[,2]

dataall.pred=forecast(data.des, h=6)
dataall.pred
plot(dataall.pred, xaxt="n")
axis(1, at=seq(1,70,1), label=seq(1960,2029,by=1)) # 1 = below axis

str(data.des)