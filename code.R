# DAL ToolBox
# version 1.0.727

source("https://raw.githubusercontent.com/cefet-rj-dal/daltoolbox-examples/main/jupyter.R")

#loading DAL
load_library("daltoolbox")
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

samp <- ts_sample(ts, test_size = 10)
train <- as.data.frame(samp$train)
test <- as.data.frame(samp$test)

auto <- autoenc_encode(5, 3)

auto <- fit(auto, train)

print(head(test))
result <- transform(auto, test)
print(head(result))
