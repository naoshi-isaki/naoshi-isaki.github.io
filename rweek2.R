## ----setup, , echo=TRUE, fig.height = 5, fig.width = 7, fig.align = "center", include = TRUE----
library(tidyverse)
library(dplyr)
library(gridExtra)
windowsFonts(family_sans = windowsFont("Noto Sans JP")) 
par(family = "family_sans")
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
rnorm(100, 50, 10) 
curve(dnorm(x, 50, 10), 0, 100)　#正規分布のグラフ描画
pnorm(60, 50, 10) #大きさ１の標本の値が60より小さくなる確率


## ------------------------------------------------------------------------
rnorm(100, 50, 10)
Z <- rnorm(100, 50, 10)
Z
hist(Z) #ヒストグラム
Z[1:10] #最初の10個のデータを抜き出し
a <- c(5, 10, 100)
Z[a] #5番目,10番目,100番目の位置にある数字を抜き出し
Z[c(5, 10, 100)] #aを使わずに同上の操作

#特定の意味を持つ数値を抽出
max(Z) #最大値
which.max(Z) #最大値の位置
min(Z) #最小値
which.min(Z) #最小値の位置
mean(Z) #平均値
summary(Z) #基本統計量を一括表示


## ------------------------------------------------------------------------
1:10
sample(1:10, 3) #無作為抽出、sample(範囲, 抽出個数)
sample(Z, 5) #sample(標本, 抽出個数)
sample(Z, 5) #sample(標本, 抽出個数)

fruits <- c("ミカン", "バナナ", "リンゴ", "レモン", "モモ")
fruits
sample(fruits, 1) #fruitsから1個抽出
sample(fruits, 2) #fruitsから2個抽出
sample(fruits, 1) #fruitsから1個抽出
sample(fruits, 1) #fruitsから1個抽出


## ------------------------------------------------------------------------
coin <- c("Head", "Tail") 
#sample(coin, 5) #うまくいかない　∵非復元抽出になっている
sample(coin, 5, replace = TRUE) #復元抽出, replace=TRUE


## ------------------------------------------------------------------------
mean(Z) #標本平均
mean(rnorm(100, 50, 10))
mean(rnorm(100, 50, 10)) #Z <- rnorm(100, 50, 10)

#より多い試行、抽出1000回、標本の大きさ100
S <- 1000
rec1 <- numeric(S)
for(i in 1:S){
  rec1[i] <- mean(rnorm(100, 50, 10))
}
rec1
hist(rec1, main = "n = 100") #main = "n = 100", ラベル
summary(rec1)

rec2 <- numeric(S)
for(i in 1:S){
  rec2[i] <- mean(rnorm(10000, 50, 10))
}
rec2
hist(rec2, main = "n = 10000") #main = "n = 10000", ラベル
summary(rec2)


## ------------------------------------------------------------------------
sample(1:6, 10, replace = TRUE) #サイコロを10回振る試行
mean(sample(1:6, 1000, replace = TRUE)) #サイコロを1000回振った結果の目の平均値, 3.5に近似

#サイコロ10回の標本平均×1000
S <- 1000
rec3 <- numeric(S)
for(i in 1:S){
  rec3[i] <- mean(sample(1:6, 10, replace = TRUE))} #サイコロ10回の標本平均×1000
summary(rec3)

#サイコロ1000回の標本平均×1000
S <- 1000
rec4 <- numeric(S)
for(i in 1:S){
  rec4[i]<-mean(sample(1:6, 1000, replace = TRUE))} #サイコロ1000回の標本平均×1000
summary(rec4)

#サイコロ10000回の標本平均×1000
S <- 1000
rec5 <- numeric(S)
for(i in 1:S){
  rec5[i] <- mean(sample(1:6, 10000, replace = TRUE))} #サイコロ1000回の標本平均×10000
summary(rec5)


## ------------------------------------------------------------------------
#母集団N(50, 10^2)から大きさ1000の標本を無作為に抽出
x <- rnorm(1000, 50, 10)
var(x) #分散
sd(x) #標準偏差 #10に近似
#分散・標準偏差における大数の法則
#n = 1000
S <- 1000; n <- 1000
rec <- numeric(S)
for(i in 1:S){
  rec[i]<-sd(rnorm(n, 50, 10))}
summary(rec) #標準偏差10に近似

#より大きいサイズで確認
#n = 10000
S <- 10000; n <- 10000
rec <- numeric(S)
for(i in 1:S){
  rec[i]<-sd(rnorm(n, 50, 10))}
summary(rec) #より10に近似＝大数の法則


## ------------------------------------------------------------------------
x <- rnorm(100, 50, 10)
y <- rnorm(100, 50, 10)
plot(x, y, main = "散布図(x, y)") #x,yの散布図

#xと関係のあるzを追加
z <- (x+y)/2
plot(x, z, main = "散布図(x,z)") #x,zの散布図


## ------------------------------------------------------------------------
x <- rnorm(100, 50, 10)
y <- rnorm(100, 50, 10)
plot(x, y, main = "散布図(x, y)") #x, yの散布図
z <- (x+y)/2 #xと関係のあるzを追加
plot(x, z, main = "散布図(x, z)") #x, zの散布図


## ------------------------------------------------------------------------
cor(x, y)
cor(x, z)


## ------------------------------------------------------------------------
cov(x, y)
cov(x, z)


## ------------------------------------------------------------------------
x <- rnorm(100, 50, 10)
y <- rnorm(100, 50, 10)
par(family="family_sans")
plot(x, y, main = "散布図(x, y)") #x, yの散布図
z <- (x+y)/2 #xと関係のあるzを追加
plot(x, z, main = "散布図(x, z)") #x,zの散布図

library(ggplot2)
ggplot() +
  geom_point(aes(x, y))+
  labs(title="散布図(x, y)") +
  theme_grey(base_family = "family_sans") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot() +
  geom_point(aes(x, z)) +
  labs(title="散布図(x, z)") +
  theme_grey(base_family = "family_sans") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot() +
  geom_point(aes(x, y)) +
  labs(title="散布図(x, y)") +
  theme_bw(base_family = "family_sans") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot() +
  geom_point(aes(x, z)) +
  labs(title = "散布図(x, z)") +
  theme_bw(base_family = "family_sans") +
  theme(plot.title = element_text(hjust = 0.5))


## ------------------------------------------------------------------------
#2.1.1.
runif(100)

#2.1.2.
P <- runif(100)
mean(P) #平均
var(P) #分散
sd(P) #標準偏差

#2.1.3.
S <- 1000; n = 100
recP <- numeric(S)
for(i in 1:S){
  recP[i] <- mean(P)}
summary(recP)

#Re
x <- runif(100)
#2.1.2.
mean(x)
var(x)
sd(x)
#2.1.3.
S <- 1000; n <- 1000
rec <- numeric(S)
for(i in 1:S){
  rec[i] <- mean(runif(n))}
summary(rec)


## ------------------------------------------------------------------------
x <- runif(100)
y <- rnorm(100, 0, 1)
z <- 1.3*x-0.7*y

#2.2.1.
plot(x, y, main = "散布図(x, y)")
plot(x, z, main = "散布図(x, z)")
plot(y, z, main = "散布図(y, z)")
cor(x, y)
cor(x, z)
cor(y, z)

#2.2.2.
x <- runif(n)
y <- rnorm(n, 0, 1)
z <- 1.3*x-0.7*y
n = 10000
cor(x, y)
cor(x, z)
cor(y, z)
par(mfrow = c(1,3))
plot(x, y, main = "散布図(x, y)")
plot(x, z, main = "散布図(x, z)")
plot(y, z, main = "散布図(y, z)")
w <- cbind(x, y, z)
cor(w)

#おまけ
library(ggplot2)
x <- runif(100)
y <- rnorm(100, 0, 1)
z <- 1.3*x-0.7*y

g1 <- ggplot() +
  geom_point(aes(x, y)) +
  labs(title = "散布図(x, y)") +
  theme_bw(base_family = "family_sans") +
  theme(plot.title = element_text(hjust = 0.5))
g2 <- ggplot() +
  geom_point(aes(x, z)) +
  labs(title = "散布図(x, z)") +
  theme_bw(base_family = "family_sans") +
  theme(plot.title = element_text(hjust = 0.5))
g3 <- ggplot() +
  geom_point(aes(y, z)) +
  labs(title = "散布図(y, z)") +
  theme_bw(base_family = "family_sans") +
  theme(plot.title = element_text(hjust = 0.5))
grid.arrange(g1, g2, g3, ncol = 3)

