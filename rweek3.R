## ----setup, echo = TRUE, fig.height = 5, fig.width = 7, fig.align = "center", include = TRUE----
library(tidyverse)
library(dplyr)
library(gridExtra)
windowsFonts(family_sans = windowsFont("Noto Sans JP")) 
par(family = "family_sans")
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
#コイン投げ
coin <- c("Head","Tail")
sample(coin, 100, replace = TRUE) #sample(標本/範囲, 抽出個数) #, replace = TRUE 復元抽出

#コイン投げのシミュレーション
#P(A)=100!/50!50!=1/2^100
factorial(100)/factorial(50)^2/2^100 #factorial(), 階乗

#Rを用いて確認
coin <- c(1, 0) #1 = "Head", 0 = "Tail"
z <- sample(coin, 100, replace = TRUE) #コイン投げ100回
sum(z) #コイン投げ100回で表が出た回数

#繰り返し, forループ
S <- 100000 #コイン投げ100回×100000
rec <- numeric(S) #rec<-試行結果の記録
coin <- c(1, 0) #コイン表(=1),裏(=0)
for(i in 1:S){ #forループ
  z <- sample(coin, 100, replace = TRUE) 
  rec[i]<-sum(z)
}
hist(rec, main = "コイン投げ100回(表)", xlab = "表の出た回数")
summary(rec)


## ------------------------------------------------------------------------
#おまけ
library(ggplot2)
REC <- as.data.frame(rec)
ggplot(REC, aes(rec)) +
  geom_histogram()
hist(rec, main = "コイン投げ100回(表)", xlab = "表の出た回数")
ggplot(REC,aes(rec)) +
  geom_histogram()
ggplot(REC,aes(rec)) +
  geom_histogram(binwidth = 2)


## ------------------------------------------------------------------------
(2>1) #2は1よりも大きい<-真
(2>1000) #2は1000よりも大きい<-偽
(200==100*2) #"="ではなく"=="
TRUE + TRUE #TRUE=1, FALSE=0
TRUE + FALSE #TRUE=1,FALSE=0
count <- (rec==50) #ちょうど50回表が出た
sum(count)
mean(count) #相対頻度
#理論値とおよそ同じ


## ------------------------------------------------------------------------
S <- 100000	#シミュレーション回数
X <- rnorm(S, 50, 10)	#Xを抽出
Y <- rnorm(S, 50, 10)	#Yを抽出
Z <- X+Y #Zを構成
#P(X>70)*P(Z>100)
mean((X>70))*mean((Z>100))	
#P(X>70 かつ Z>100)
mean((X>70)*(Z>100))
mean(X>70)


## ------------------------------------------------------------------------
X <- rnorm(100000, 50, 10); Y <- rnorm(100000, 50, 10)
cor(X, Y)
Z <- (X-50)^2/10
cor(X, Z)
plot(X, Z)


## ------------------------------------------------------------------------
curve(pnorm(x, 50, 10), 0, 100)
pnorm(60, 50, 10)-pnorm(40, 50, 10) #「Xが40より大きく60以下の値をとる」


## ------------------------------------------------------------------------
#Fx(a+h)-Fx(a)
pnorm(50 + 0.1, 50, 10) - pnorm(50, 50, 10) #X = 50となる確率P(X = 50)
#X = 50の確率0
#確率をhで割って正規化, 表3.1
#確率密度関数
dnorm(50, 50, 10) #Xが正規分布N(50, 10^2)に従うとき、X = 50となる確率密度
dnorm(80, 50, 10) #X = 80となる確率密度
#X = 80となる確率は、X = 50となる確率よりも小さい
curve(dnorm(x, 50, 10), 0, 100)


## ------------------------------------------------------------------------
malesdata <- read.csv("wage.csv")
head(malesdata)
summary(malesdata)
plot(malesdata)
sch12 <- malesdata[malesdata$school==12, ] #高校卒業者の抜き出し
malesdata$school
malesdata$exper
mean(sch12$wage)
sch16 <- malesdata[malesdata$school==16, ] #学部卒業者の抜き出し
exp(mean(sch12$wage)) #高校卒業者の平均賃金
exp(mean(sch16$wage)) #学部卒業者の平均賃金
#学部卒業者よりも高校卒業者の平均賃金が高くなっている
#正しい推定
malesdata <- read.csv("wage.csv")
sch11 <- malesdata[malesdata$school==11, ] #高校未満の抜き出し
sch12 <- malesdata[malesdata$school>=12, ] #高卒以上の抜き出し
exp(mean(sch11$wage)) #高卒未満の平均賃金
exp(mean(sch12$wage)) #高卒以上の平均賃金


## ------------------------------------------------------------------------
X <- rnorm(10000, 50, 10)
Xbar <- mean(X); Sn <- var(X) #標本平均、標本分散
Zn <- sqrt(10000)*(Xbar-50)/sqrt(Sn)

#中心極限定理、繰り返し計算シミュレーション
S <- 10000 #シミュレーション回数
n <- 10000 #標本の大きさ

Zn <- numeric(S) #結果記録用のリスト
for(i in 1:S){ #繰り返し開始
  X <- rnorm(n,50,10)	#N(50,10)から標本抽出
  Xbar <- mean(X)
  Sn <- var(X)
  Zn[i] <- sqrt(n)*(Xbar-50)/sqrt(Sn)
}	#繰り返し終了

hist(Zn) #ヒストグラム作成
hist(Zn, main = "CLT (Figure 3.5)") #ヒストグラム作成


## ------------------------------------------------------------------------
S <- 10000
n <- 10000
rec <- numeric(S)
for(i in 1:S){
  X <- rnorm(n, 50, 10)
  Xbar <- mean(X); Sn <- var(X)
  rec[i] <- (Xbar - 1.96*sqrt(Sn/n) < 50)*(50 < Xbar + 1.96*sqrt(Sn/n))
}
mean(rec)


## ------------------------------------------------------------------------
pnorm(50 + (1.96*10), 50, 10) - pnorm(50 - (1.96*10), 50, 10)


## ------------------------------------------------------------------------
read.csv("temperature.csv")
data <- read.csv("temperature.csv")

#3.1.1.
mean(data$temp) #気温平均

#3.1.2.
mean(data$temp[1:100]) #気温平均（100番目まで）
mean(data$temp); mean(data$temp[1:100]) #1と比較

#3.1.3.
sample(data$temp, 100) #無作為抽出
mean(sample(data$temp, 100))
mean(data$temp); mean(sample(data$temp, 100)) #1と比較

#3.1.4.
#違いがなぜ生じたのか


## ------------------------------------------------------------------------
#3.2.1.
data2 <- read.csv("icecream.csv")
max(data2$icecream)
which.max(data2$icecream)
data2 #17番目は富山市

#3.2.2.
x <- data2$icecream
y <- data2$income
plot(x, y, main = "Figure 3.2.2.")
cor(x, y)

#3.2.3
z <- data2$u15
plot(x, z, main = "Figure 3.2.3.")
cor(x, z)

#3.2.4
#略

#3.3.1
S <- 1000
X <- rnorm(S, 50, 10)
rec<-numeric(S)
for(i in 1:S){
  rec[i]<-(X[i]>10)
}
mean(rec)

#3.3.2. 
S <- 1000
X <- rnorm(S, 50, 10)
rec <- numeric(S)
for(i in 1:S){
  rec[i] <- (-10<X[i])&(X[i]<10)
}
mean(rec)

#3.3.3
S <- 1000
X <- rnorm(S, 50, 10)
Y <- rnorm(S, 50, 10)
rec <- numeric(S)
for(i in 1:S){
  rec[i] <- (X[i]>Y[i]^2)
}
mean(rec)

#3.4.
S <- 10000
n <- 10000
rec <- numeric(S)
for(i in 1:S){
  X <- rnorm(n, 50, 10)
  Xbar <- mean(X)
  Sn <- var(X)
  rec[i] <- (Xbar-1.64*sqrt(Sn/n)<50)&(50<Xbar+1.64*sqrt(Sn/n))
}
mean(rec)

