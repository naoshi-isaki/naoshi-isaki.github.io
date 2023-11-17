## ----setup, echo = TRUE, fig.height = 5, fig.width = 7, fig.align = "center", include = TRUE----
library(tidyverse)
library(dplyr)
library(gridExtra)
library(readr)
library(lubridate)
windowsFonts(family_sans = windowsFont("Noto Sans JP")) 
par(family = "family_sans")
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
tempdata <- read_csv("temperature_aug.csv")

ggplot(tempdata, aes(x = time, y = temp)) +
  geom_point() +
  labs(title = "図4.2 2014年8月の東京都における時刻と気温", x = "時刻", y = "気温") +
  theme_bw(base_family = "family_sans") +
  theme(plot.title = element_text(hjust = 0.5))


## ------------------------------------------------------------------------
head(tempdata) #データフレーム：項目名と番号という枠（フレーム）を備えた、さまざまな属性のデータの塊
print(tempdata, n = 100)
#特定の行や列を抽出 [行, 列]
tempdata[3, ]
head(tempdata[, 2])
tempdata[3, 2]
tempdata$time
tempdata$time[3]


## ------------------------------------------------------------------------
tempdata %>% 
  summarise(mean_temp = mean(temp))

tempdata %>% 
  summarise(mean_temp = mean(temp),
            sd_temp   = sd(temp),
            max_temp  = max(temp),
            min_temp  = min(temp))

tempdata %>%
  group_by(time) %>% 
  summarise(mean_temp = mean(temp))

tempdata %>%
  group_by(time) %>%
  summarise(mean_temp = mean(temp)) %>%
  ggplot(aes(x = time, y = mean_temp)) +
  geom_line() +
  labs(title = "図4.3　ノンパラメトリック回帰の結果", x = "時間", y = "気温") + 
  theme_bw()

tempdata %>% 
  ggplot(aes(x = time, y = temp)) +
  stat_summary(geom = "line", fun = "mean") + #stat_summary(geom = , fun = ): 引数yに与えられた変数を対象にして、統計量を計算して描画
  labs(title = "", x = "時間", y = "気温") + 
  theme_bw()


## ------------------------------------------------------------------------
tempdata %>% 
  ggplot(aes(x = time, y = temp)) +
  geom_point() +
  stat_summary(geom = "line", fun = "mean") +
  labs(title = "図4.4　回帰曲線と散布図の関係", x = "時間", y = "気温") + 
  coord_cartesian(ylim = c(20, 30)) +
  theme_bw()


## ------------------------------------------------------------------------
with(tempdata, cor(temp, elec))
tempdata %>% 
  summarise(cor(temp, elec))

tempdata %>% 
  with(cor(temp, elec))

tempdata %>% 
  ggplot(aes(x = temp, y = elec)) +
  stat_summary(geom = "line", fun = "mean") + 
  labs(title = "図4.6　電気使用量の回帰曲線",
       x = "電気使用量の回帰曲線", y = "電気使用量（万kw）") +
  theme_bw()


## ------------------------------------------------------------------------
lm(elec ~ temp, data = tempdata)


## ------------------------------------------------------------------------
result <- lm(elec ~ temp,
             data = tempdata)
result$coefficients

tempdata %>% 
  ggplot(aes(x = temp, y = elec)) +
  geom_point() +
  geom_abline(intercept = result$coefficients[1],
              slope     = result$coefficients[2]) +
  labs(title = "図4.7　単回帰の結果", x ="気温", y = "電気使用量（万kw）") +
  theme_bw()


## ------------------------------------------------------------------------
tempdata %>% 
  ggplot(aes(x = time, y = elec)) +
  geom_point() +
  labs(title = "図4.8　電力使用量の時間推移", x = "時刻", y = "電気使用量（万kw）") +
  theme_bw()

tempdata$daytime <- 
  (tempdata$time >= 9) & (tempdata$time <= 18)

tempdata <- tempdata %>% 
  mutate(daytime = 1 * (9 <= time & time <= 18),
         elec100 = elec / 100,
         time12  = time %% 12,
         ampm    = ifelse(time < 12, "a.m.", "p.m."))


## ------------------------------------------------------------------------
lm(elec ~ temp + daytime, data = tempdata)
lm(elec ~ temp + daytime + prec, data = tempdata)
tempdata %>%
  mutate(sunday = 1 * 
           (date == "2014/8/3")  |
           (date == "2014/8/10") |
           (date == "2014/8/17") |
           (date == "2014/8/24") |
           (date == "2014/8/31")) %>%
  head()
tempdata <- tempdata %>% 
  mutate(date = ymd(date))
tempdata %>% print(n = 4)

wday(tempdata$date, label = TRUE)
tempdata <- tempdata %>% 
  mutate(dow    = wday(date, label = TRUE),
         sunday = 1 * (dow == "日"))

tempdata <- tempdata %>% 
  mutate(recess = 1 * ("2014-08-11" <= date & 
                         date <= "2014-08-16"))

lm(elec ~ temp + daytime + prec + sunday + recess,
   data = tempdata)

179.07 + 113.48 * 28 + 563.53 * 1 + 14.27 * 0 - 448.39 * 0 - 438.23 * 0

result <- lm(elec ~ temp + daytime + prec + sunday + recess, data = tempdata)
sum(result$coefficients * c(1, 28, 1, 0, 0, 0))


## ------------------------------------------------------------------------
lm(elec ~ temp + daytime + prec + sunday + recess,
   data = tempdata) %>% 
  summary()

lm(elec ~ temp,
   data = tempdata) %>% 
  summary()


## ------------------------------------------------------------------------
set.seed(2022) #set.seed(): 常に同じ乱数を発生, 解析ごとに結果が変わってしまうことを防止

a <- sample(1:6, 15, replace = TRUE)
b <- sample(1:6, 15, replace = TRUE)
c <- sample(1:6, 15, replace = TRUE)
d <- sample(1:6, 15, replace = TRUE)
e <- sample(1:6, 15, replace = TRUE)
f <- sample(1:6, 15, replace = TRUE)
g <- sample(1:6, 15, replace = TRUE)

summary(lm(a ~ b + c + d + e + f + g))


## ------------------------------------------------------------------------
tempdata <- read_csv("temperature_aug.csv")
result <- lm(elec ~ temp,
             data = tempdata)
ehat <- result$residuals
sum(ehat)
sum(tempdata$temp*ehat)


## ------------------------------------------------------------------------
library(dplyr)
library(lubridate)
library(broom)


## ------------------------------------------------------------------------
elecdata <- read.csv("temperature_aug.csv")
elecdata <- elecdata %>%
  mutate(morning = 1*(6 <= time & time <= 12), 
         afternoon = 1*(13 <= time & time <= 18))
elecdata %>%
  select(time, morning, afternoon)


## ------------------------------------------------------------------------
elecdata <- read.csv("temperature_aug.csv")
elecdata <- elecdata %>%
  mutate(data = ymd(data), 
         dow  = wdat(data, label = TRUE),
         saturday = 1*(dow == "土"))


## ------------------------------------------------------------------------
elecdata <- read.csv("temperature_aug.csv")
elecdata <- elecdata %>%
  mutate(sunday = 1*(dow == "日"), 
         recess = 1*("2014-08-11" <= data & data <= "2014-08-16"))
electdata %>%
  lm(elec~temp + prec + sunday +
       recess + morning + afternoon + saturday, data =.) %>%
  tidy()


## ------------------------------------------------------------------------
elecdata <- read.csv("temperature_aug.csv")
elecdata %>%
  lm(elec ~ temp + prec + sunday +
       recess + morning + afternonn + saturday,
     data = .) %>%
  glance() %>%
  pull(r.squared)


## ------------------------------------------------------------------------
icedata <- read.csv("icecream.csv")
icedata %>%
  ggplot(aes(x = income, y = icecream)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


## ------------------------------------------------------------------------
icedata <- read.csv("icecream.csv")
icedata %>%
  lm(icecream ~ income + u15, data =.) %>%
  glance %>%
  pull(r.squared)


## ------------------------------------------------------------------------
wagedata <- read.csv("wage.csv")


## ------------------------------------------------------------------------
wagedata <- read.csv("wage.csv")
wagedata %>%
  lm(log(wage) ~ educ, data =.) %>%
  augment() %>%
  ggplot(aes(x = educ, y = `log(wage)`)) +
  geom_point() +
  geom_line(colour = "blue", aes(y = .fitted))


## ------------------------------------------------------------------------
wagedata <- read.csv("wage.csv")
wagedata %>%
  lm(log(wage) ~ educ + exper, data = .) %>%
  tidy()



wagedata <- read.csv("wage.csv")
wagedata %>%
  lm(log(wage) ~ educ, data =.) %>%
  summary()

wagedata %>%
  lm(log(wage) ~ educ + exper, data =.) %>%
  summary()