#source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger-examples/main/jupyter.R")

#load_library("devtools")
#load_library("daltoolbox")
#load_library("harbinger")

data(har_examples)

dataset = har_examples[[15]]
head(dataset)

plot_ts(x = 1:length(dataset$serie), y = dataset$serie)

model = hmo_sax(26, 3, 3)
model = fit(model, dataset$serie)

detection = detect(model, dataset$serie)

print(detection |> dplyr::filter(event==TRUE))

evaluation = evaluate(model, detection$event, dataset$event)
print(evaluation$confMatrix)

grf = har_plot(model, dataset$serie, detection, dataset$event)
plot(grf)
