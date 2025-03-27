library(lmtest)
library(tseries)
library(forecast)
library(dplyr)


library(readr)
library(readr)
IBM_2006_01_01_to_2018_01_01 <- read_csv("archive (1)/IBM_2006-01-01_to_2018-01-01.csv")
View(IBM_2006_01_01_to_2018_01_01)

data <- IBM_2006_01_01_to_2018_01_01



# Convert Date column to Date object
data$Date <- as.Date(data$Date)

# Extract month and year
data$Month <- format(data$Date, "%Y-%m")

# Group by month and calculate the average
data2 <- data %>%
  group_by(Month) %>%
  summarise(
    Open_Avg = mean(Open, na.rm = TRUE),
    High_Avg = mean(High, na.rm = TRUE),
    Low_Avg = mean(Low, na.rm = TRUE),
    Close_Avg = mean(Close, na.rm = TRUE),
    Volume_Avg = mean(Volume, na.rm = TRUE)
  )

draft <- data2 %>%
  
  summarise(
    Open = Open_Avg,
    High = High_Avg ,
    Low = Low_Avg ,
    Close = Close_Avg ,
    Volume = Volume_Avg 
  )




nrow(draft) 

train <- draft[1:100,]
test <- draft[101:144,]
summary(train)

model_1<-lm(draft$Close~ draft$Open + draft$High + draft$Low + draft$Volume)
summary(model_1)

model_2<-lm(draft$Close~ draft$Open + draft$High + draft$Low )
summary(model_2)


AIC(model_1)
AIC(model_2)

#> AIC(model_1)
#[1] -145.904
#> AIC(model_2)
#[1] -147.3206

#hence we go with model_2


bptest(model_2)
#evidence of heteroskedasticity

kpss.test(train$Close)
#KPSS Test for Level Stationarity
#data:  train$Close
#KPSS Level = 1.9052, Truncation lag parameter = 4, p-value = 0.01



adf.test(train$Close)

#Dickey-Fuller = -2.1298, Lag order = 4, p-value = 0.5229
#alternative hypothesis: stationary


kpss.test(diff(train$Close))
# now it became stationary
#KPSS Level =0.066438, Truncation lag parameter = 3, p-value = 0.1



adf.test(diff(train$Close))
#Dickey-Fuller = -4.101, Lag order = 4, p-value = 0.01

#unit root not present

value_bc <- BoxCox.lambda(train$Close)
#[1]1.691239

reg<-cbind(train$Close, train$Open , train$High , train$Low , train$Volume)

model_final<-auto.arima(train$Close,xreg
                        =reg,lambda=value_bc,d=1)

summary(model_final)

reg2<-cbind(train$Close, train$Open , train$High , train$Low )

model_final2<-auto.arima(train$Close,xreg
                        =reg2,lambda=value_bc,d=1)

summary(model_final2)

 z<-resid(model_final)
 #plot(z)
 Box.test(z)
 #no evidence of heteroscedasticity. 
 
 z2<-resid(model_final2)
 #plot(z2)
 Box.test(z2)
 
 new<-cbind(test$Close, test$Open , test$High , test$Low , test$Volume)
 
 new2<-cbind(test$Close, test$Open , test$High , test$Low )
 
 
output  <- forecast(model_final,xreg=new)

output2 <- forecast(model_final2,xreg=new2)

plot(forecast(model_final,xreg=new))
#plot(Box.test(z))
plot(forecast(model_final2,xreg=new2))
 Box.test(z)
 
 # Assuming you have a variable 'my_variable' and you want to save its value to a CSV file.
 
 # Example variable (replace with your actual variable)

 
 # Create a data frame (even if it's a single column)
 df <- data.frame(value = output)
 
 # Specify the file path for the new CSV file
 file_path <- "C:/Users/shubhendu/my_file_24.csv"
 
 # Save the data frame to the CSV file
 write.csv(df, file = file_path, row.names = FALSE) # row.names = FALSE prevents row numbers from being written
 
 # Create a data frame (even if it's a single column)
 df <- data.frame(value = draft)
 
 # Specify the file path for the new CSV file
 file_path <- "C:/Users/shubhendu/my_.csv"
 
 # Save the data frame to the CSV file
 write.csv(df, file = file_path, row.names = FALSE) # row.names = FALSE prevents row numbers from being written
 

