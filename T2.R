#loading DAL
library(daltoolbox)
library(harbinger)
library(reticulate)

source_python('seed.py')


#Definição das funções do sax

binning_sax = function(v, a) {
  p = base::seq(from = 0, to = 1, by = 1/a)
  q = stats::quantile(v, p)
  qf = base::matrix(c(q[1:(length(q)-1)],q[2:(length(q))]), ncol=2)
  vp = base::cut(v, unique(q), FALSE, include.lowest=TRUE)
  m = base::tapply(v, vp, mean)
  vm = m[vp]
  mse = base::mean((v - vm)^2, na.rm = TRUE)
  return (list(binning=m, bins_factor=vp, q=q, qf=qf, bins=vm, mse=mse))
}

convert_to_sax = function(num, nbase) {
  chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  if(nbase < 2 || nbase > stringr::str_length(chars))
    return("")
  newNumber = ""
  r = 0
  while(num >= nbase)
  {
    r = num %% nbase
    newNumber = base::sprintf("%s%s", substr(chars, r+1, r+1), newNumber)
    num = as.integer(num / nbase)
  }
  newNumber = base::sprintf("%s%s", substr(chars, num+1, num+1), newNumber)
  return (newNumber)
}

convert_to_sax_vec = function(num, nbase) {
  n = length(num)
  result = rep("", n)
  for (i in 1:n) {
    result[i] = convert_to_sax(num[i], nbase)
  }
  return(result)
}

norm_sax = function (vector, slices)
{
  vectorNorm = (vector - base::mean(vector, na.rm = T))/stats::sd(vector, na.rm = T)
  mybin = binning_sax(vectorNorm, slices)
  i = base::ceiling(log(slices, 26))
  mycode = stringr::str_pad(convert_to_sax_vec(0:(slices-1), 26), i, pad="0")
  saxvector = mycode[mybin$bins_factor]
  return(saxvector)
}

#Aplicação do encoder

data(har_examples)

dataset = har_examples[[15]]
head(dataset)

set.seed(1)
seed_everything(1)

sw_size = 5
ts = ts_data(dataset$serie, sw_size)

preproc = ts_norm_gminmax()
preproc = fit(preproc, ts)
ts = transform(preproc, ts)

ts_head(ts)

ts = as.data.frame(ts)

auto = autoenc_encode(5, 3)
auto = fit(auto, ts)
encoded = transform(auto, as.data.frame(ts))

tsw = norm_sax(as.vector(encoded), 10)
tsw <- matrix(tsw, ncol=3, byrow=TRUE)

#Utilização do modelo sax

obj = harbinger()
obj$a = 10
obj$w = 3
obj$qtd = 3
class(obj) = append("hmo_sax", class(obj))

#Detecção de motifs

seq = base::apply(tsw, MARGIN = 1, function(x) paste(as.vector(x), collapse=""))
data = data.frame(i = 1:nrow(tsw), seq)
result = data |> dplyr::group_by(seq) |> dplyr::summarise(total_count=dplyr::n())
result = result[result$total_count >= obj$qtd,]
result = result |> dplyr::arrange(dplyr::desc(total_count)) |> dplyr::select(seq)
result = result$seq
motifs = data[data$seq %in% result,]

mots = rep(FALSE, nrow(tsw))
seqs = rep(NA, nrow(tsw))
mots[motifs$i] = TRUE
seqs[motifs$i] = motifs$seq

detection = data.frame(idx=1:length(mots), event = mots, type="")
detection$type[mots] = "motif"

print(detection)

print(detection[(detection$event),])
