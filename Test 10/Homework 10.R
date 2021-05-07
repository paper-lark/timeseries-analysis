library(TSA)
library(tidyverse)
library(tseries)
library(lmtest)
library(urca)

df <- read.csv('./Test 10/var_4.csv',header = T)
df %>% head()
matplot(df,type = "l",lty = 1,col = 1:dim(df)[2],main = "Data")  

# Построим модель коинтеграции
vecm <- ca.jo(df, ecdet = "none",type="eigen",K=2,spec="longrun")
lambda <- vecm@lambda
lambda

# Проверим гипотезу о порядке коинтеграции
outmat <-rbind(vecm@cval[,2],vecm@teststat)
legends <- c("critical values","statistics")
barplot(outmat, main="Statistics vs Critical Value for 5% test ",legend.text = legends,beside = TRUE, col=c("red","blue"), las=2)
h <- 4

# Построим результирующие стационарные ряды
df.ma <- as.matrix(df)
res <- cbind(
  df.ma %*% vecm@V[,1],
  df.ma %*% vecm@V[,2],
  df.ma %*% vecm@V[,3],
  df.ma %*% vecm@V[,4]
)
res.ma <- as.matrix(res)

# Проверим выбранное значение наглядно (не должно быть тренда)
matplot(res, type = "l", lty = 1, col = 1:dim(res)[2],main = "Stationary")  

# Сохраним результат
res <- list(lambda=lambda,
            h = h,
            z = res.ma)
saveRDS(res,"./Test 10/result.rds")
