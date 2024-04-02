#loading DAL
library(daltoolbox)
library(harbinger)
library(reticulate)


source_python('seed.py')

data(sin_data)

set.seed(1)
seed_everything(1)

sw_size <- 5
ts <- ts_data(sin_data$y, sw_size)

ts_head(ts)

preproc <- ts_norm_gminmax()
preproc <- fit(preproc, ts)
ts <- transform(preproc, ts)

ts_head(ts)

ts <- data.frame(ts)

auto <- autoenc_encode(5, 3)

auto <- fit(auto, ts)

result <- transform(auto, ts)

