library(daltoolbox)
library(harbinger)
library(reticulate)
library(ggplot2)

source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/jupyter.R")
source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/R/hcd_kswin.R")

source_python('time_series/seed.py')
set.seed(1)
seed_everything(1)
sample()

create_random_series <- function(number_elements, range, denominator){
  series <- c(sample(range, size = number_elements, replace = T))/denominator
  
  series
}


# Criar séreies aleatórias
y = c(create_random_series(100, 5:10, 1), create_random_series(20, 5:10, 1), create_random_series(50, 0:5, 1), create_random_series(50, 0:15, 1))
x = c(create_random_series(50, 15:20, 1), create_random_series(70, 2:5, 1), create_random_series(25, 5:10, 1), create_random_series(75, 0:4, 1))
dataset = data.frame("V1" = x, "V2" = y)
cor(dataset)
plot_ts(x = 1:length(dataset$V1), y = dataset$V1)
plot_ts(x = 1:length(dataset$V2), y = dataset$V2)
ggplot(dataset, aes(x=dataset$V1, y=dataset$V2)) +  geom_point()
dataset <- dataset[(dataset[1]<15) & (dataset[2] <15),]
#n <- 100  # size of each segment
#serie1 <- c(sin((1:n)/pi), 2*sin((1:n)/pi), 10 + sin((1:n)/pi),
#            10-10/n*(1:n)+sin((1:n)/pi)/2, sin((1:n)/pi)/2)
#serie2 <- 2*c(sin((1:n)/pi/6), 2*cos((1:n)/pi), 10 + cos((1:n)/pi /4),
#              10-10/n*(1:n)+cos((1:n)/pi/12)/2, cos((1:n)/pi/6)/2)
#event <- rep(FALSE, length(serie1))
#event[c(100, 200, 300, 400)] <- TRUE
#dataset <- data.frame(serie1, serie2)


sw_size <- 5
ts_1<- ts_data(dataset$V1, sw_size)
ts_2<- ts_data(dataset$V2, sw_size)
dataset_ts <- data.frame(ts_1, ts_2)

plot_ts(x = 1:length(dataset$V1), y = dataset$V1)

plot_ts(x = 1:length(dataset$V2), y = dataset$V2)


auto <- autoenc_encode(10, 1)
auto <- fit(auto, dataset_ts)
result <- transform(auto, dataset_ts)
result_series = data.frame("RESULT" = result,  "event" = rep(FALSE, length(result)))


#Aplicando Método Page Hinkley
model <- fit(hcd_page_hinkley(threshold=1), result_series)
detection <- detect(model, result_series)
print(detection[(detection$event),])
grf <- har_plot(model, result_series$RESULT, detection)
grf <- grf + ggplot2::ylab("value")
grf <- grf
plot(grf)

#Aplicando Método Kswin
model <- hcd_kswin()
model <- fit(model, result_series)

detection <- detect(model, result_series)
print(detection[(detection$event),])

grf <- har_plot(model, result_series$RESULT, detection)
grf <- grf + ggplot2::ylab("value")
grf <- grf
plot(grf)

