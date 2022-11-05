TZ = 'GMT+3'
Sys.getlocale()
setwd()


#    Imports
# ============================================

install.packages('readxl')
library(readxl)
install.packages('xts')
library(xts)
install.packages("forecast")
library(forecast)


data <- read_excel('timeseries.xlsx')
str(data)


#    Сonversion to time series
# ============================================

zp <- na.omit(ts(data$'Среднемесячная номинальная начисленная заработная плата работников по полному кругу организаций, Костромская область', 
                 start = c(2013,1), 
                 frequency = 12))

ipc <- na.omit(ts(data$'ИПЦ на свежие помидоры, Бурятия', 
                  start=c(2013, 1), 
                  frequency=12))

unempl <- na.omit(ts(data$'Уровень безработицы населения в возрасте 15-72 лет, Томская обл., процент', 
                     start=2000))

science <- na.omit(ts(data$'Внутренние затраты на научные исследования и разработки, млн. руб, Санкт-Петербург', 
                      start=2010))


#    Visualizations
# ============================================

plot(zp, xlab='Время', ylab='Зарплата')
plot(ipc, xlab='Время', ylab='ИПЦ на свежие помидоры')
plot(unempl, xlab='Время', ylab='Уровень безработицы, %')
plot(science, xlab='Время', ylab='Затраты на науку, млн. руб.')


#    Decompose
# ============================================

dec_zp <- decompose(zp, type='additive') # Хольт-Уинтерс мультипликативная
plot(dec_zp)

dec_ipc <- decompose(ipc, type='additive') # Хольт-Уинтерс с аддитивной сезонностью
plot(dec_ipc)

# временной ряд не имеет или имеет меньше 2 периодов:

# unempl - линейная модель y=a+bt
# science - Хольт, подстраивающийся под отклонения


#    Building forecasts
# ============================================

hw_zp <- HoltWinters(zp, seasonal='multiplicative')
plot(hw_zp)
str(hw_zp)
head(hw_zp$fitted)

predict_zp <- predict(hw_zp, 2, prediction.interval = TRUE)


# ===========

hw_ipc <- HoltWinters(ipc, seasonal='additive')
plot(hw_ipc)
str(hw_ipc)
head(hw_ipc$fitted)

predict_ipc <- predict(hw_ipc, 2, prediction.interval = TRUE)


# ===========

fit <- tslm(unempl~trend)
summary(fit)

forecast(fit, h = 3)


# ===========

h_science <- HoltWinters(science, gamma = FALSE)

plot(h_science)
str(h_science)
head(h_science$fitted)

predict_science <- predict(h_science, 2, prediction.interval = TRUE, level=0.95)


# ===========
# ===========
# ===========

h_unempl <- HoltWinters(unempl, gamma = FALSE)

plot(h_unempl)
str(h_unempl)
head(h_unempl$fitted)

predict_h_unempl <- predict(h_unempl, 3)

# ===========

fit_science <- tslm(science~trend)
summary(fit_science)

forecast(fit_science, h = 3)
