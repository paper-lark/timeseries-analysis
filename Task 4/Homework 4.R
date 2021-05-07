library("TSA")
library("tidyverse")
library(lmtest)


data <- read_csv('Task 4/data/Test_variant_4.csv')
data %>% head()

# MA(1) or AR(3) (?)
acf(data)
pacf(data)

# MA(1)
ma1 <- arima(data, order = c(0,0,1), method="ML")
ma1$coef
ma1.res <- ma1$residuals
plot.ts(ma1.res,
        col = "blue",
        lwd = 2,
        type = "l", 
        main = "residuals")
coeftest(ma1) 
confint(ma1, level = 0.95) # NOTE: коэффициент значим
Box.test(ma1.res, lag = 3, type = "Ljung-Box", fitdf = 1) # NOTE: модель адекватна (0.46)
AIC(ma1) # NOTE: 879.125

# AR(3)
ar3 <- arima(data, order = c(3,0,0), method="ML")
ar3$coef
ar3.res <- ar3$residuals
plot.ts(ar3.res,
        col = "blue",
        lwd = 2,
        type = "l", 
        main = "residuals")
coeftest(ar3) 
confint(ar3, level = 0.95) # NOTE: все коэффициенты значимы
Box.test(ar3.res, lag = 3, type = "Ljung-Box", fitdf = 1) # NOTE: модель адекватна (0.1623)
AIC(ar3) # NOTE: 892.2466 -- хуже
