library(daltoolbox)
library(harbinger)
library(reticulate)
library(ggplot2)

source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/jupyter.R")
source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/R/hcd_kswin.R")

source_python('time_series/seed.py')
set.seed(1)
seed_everything(1)

n <- 100  # size of each segment
serie1 <- c(sin((1:n)/pi), 2*sin((1:n)/pi), 10 + sin((1:n)/pi),
            10-10/n*(1:n)+sin((1:n)/pi)/2, sin((1:n)/pi)/2)
serie2 <- 2*c(sin((1:n)/pi/6), 2*cos((1:n)/pi), 10 + cos((1:n)/pi /4),
              10-10/n*(1:n)+cos((1:n)/pi/12)/2, cos((1:n)/pi/6)/2)
event <- rep(FALSE, length(serie1))
event[c(100, 200, 300, 400)] <- TRUE
dataset <- data.frame(serie1, serie2)


sw_size <- 5
ts_1<- ts_data(dataset$serie1, sw_size)
ts_2<- ts_data(dataset$serie2, sw_size)
dataset_ts <- data.frame(ts_1, ts_2)

plot_ts(x = 1:length(dataset$serie1), y = dataset$serie1)

plot_ts(x = 1:length(dataset$serie2), y = dataset$serie2)


auto <- autoenc_encode(10, 2)
auto <- fit(auto, dataset_ts)
result <- transform(auto, dataset_ts)
result_series = as.data.frame(result)


#Aplicando Método Page Hinkley
model <- fit(hcd_page_hinkley(threshold=1), result_series)
detection <- detect(model, result_series)
print(detection[(detection$event),])
grf <- har_plot(model, result_series$V2, detection)
grf <- grf + ggplot2::ylab("value")
grf <- grf
plot(grf)

#Aplicando Método Kswin
model <- hcd_kswin()
model <- fit(model, result_series)

detection <- detect(model, result_series)
print(detection[(detection$event),])

grf <- har_plot(model, result_series$V2, detection)
grf <- grf + ggplot2::ylab("value")
grf <- grf
plot(grf)

