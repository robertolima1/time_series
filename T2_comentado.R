source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger-examples/main/jupyter.R")

load_library("devtools")
load_library("daltoolbox")
load_library("harbinger")
source_python('~/T2/seed.py')

#Definição das funções do sax


#Divide a série temporal com base nos pontos definidos por a (tamanho do alfabeto)
binning_sax = function(vector, alphabet) {
  #Calcula uma sequencia de pontos de 0 a 1, com distância entre eles de 1/a
  points = base::seq(from = 0, to = 1, by = 1/alphabet)
  #Define intervalos com base nos pontos
  quantiles = stats::quantile(vector, points)
  #Divide o vetor nos quantis calculados 
  vp = base::cut(vector, unique(quantiles), FALSE, include.lowest=TRUE)
  return (vp)
}

#Converte para os símbolos do alfabeto
convert_to_sax = function(num, nbase) {
  #Define os símbolos que servirão pra representar o SAX
  chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  #Verifica se a base possui tamanho suficiente para ser representada
  if(nbase < 2 || nbase > stringr::str_length(chars))
    return("")
  
  newNumber = ""
  r = 0
  #Inicia loop de conversão
  while(num >= nbase)
  {
    r = num %% nbase
    #Adiciona o caractere correspondente ao resto da divisão (r) na frente da representação SAX 
    newNumber = base::sprintf("%s%s", substr(chars, r+1, r+1), newNumber)
    num = as.integer(num / nbase)
  }
  #Adiciona o último caractere à representação SAX
  newNumber = base::sprintf("%s%s", substr(chars, num+1, num+1), newNumber)
  return (newNumber)
}

#Converte para um array de símbolos
convert_to_sax_vec = function(num, nbase) {
  n = length(num)
  #inicializa vetor vazio do mesmo tamanho de num (025)
  result = rep("", n)
  #preenche o array com os valores convertidos de acordo com a base
  for (i in 1:n) {
    result[i] = convert_to_sax(num[i], nbase)
  }
  return(result)
}

#Aplica a tradução do vetor para sax
norm_sax = function (vector, slices)
{
  #normaliza o vetor ((vetor - media)-desvio) para garantir a centralização em zero e escala comparável
  vectorNorm = (vector - base::mean(vector, na.rm = T))/stats::sd(vector, na.rm = T)
  #realiza discretização do vetor em intervalos usando SAX
  mybin = binning_sax(vectorNorm, slices)
  #calcula número de digitos necessários para a representação SAX
  i = base::ceiling(log(slices, 26))
  #Gera um vetor de códigos SAX para cada intervalo (bin) possível
  mycode = stringr::str_pad(convert_to_sax_vec(0:(slices-1), 26), i, pad="0")
  #Usa mybin para indexar o vetor mycode, ou seja, aplica a codificação aos intervalos
  saxvector = mycode[mybin]
  return(saxvector)
}

#Aplicação do encoder

#Carrega grupo de datasets
data(har_examples)

#Selecionada dataset
dataset = har_examples[[15]]
head(dataset)

#Define uma semente para geração de números aleatórios
set.seed(1)
#Propaga a semente para todos os geradores aleatórios
seed_everything(1)

#Define o tamanho da janela deslizante para 5
sw_size = 5
#Cria uma série temporal a partir da coluna "serie" com janela deslizante = 5
ts = ts_data(dataset$serie, sw_size)

#Cria objeto para normalização dos dados utilizando gminmax
preproc = ts_norm_gminmax()
#Ajusta o objeto de normalização aos dados da série temporal
preproc = fit(preproc, ts)
#Aplica a normalização aos dados da série temporal
ts = transform(preproc, ts)

ts_head(ts)

#Cria amostra da série temporal para obter conjuntos de treinamento e teste de tamanho 10
samp = ts_sample(ts, test_size = 10)
#Converte o conjunto de treinamento para dataframe
train = as.data.frame(samp$train)
#Converte o conjunto de teste para dataframe
test = as.data.frame(samp$test)

#Cria um modelo de autoencoder com 5 entradas e 3 saídas
auto = autoenc_encode(5, 3)
#Ajusta o modelo com os dados do treinamento
auto = fit(auto, train)
#Aplica o modelo de autoencoder aos dados da série temporal
encoded = transform(auto, as.data.frame(ts))

#Adaptação para utilizar o resultado do encoder no modelo sax

#Utilização do modelo sax

obj = harbinger()
#define tamanho do alfabeto
obj$a = 26
#define tamanho da palavra
obj$w = 3
#define tamanho do numero de ocorrencias para serem identificadas como motifs
obj$qtd = 3
#Adiciona a classe "hmo_sax" à classe do objeto obj
class(obj) = append("hmo_sax", class(obj))

#Chama a função norm_sax para normalizar e converter as representações codificadas (encoded) em representações SAX.
tsw = norm_sax(as.vector(encoded), 26)
#Ajusta as dimensões de tsw para serem iguais às dimensões de encoded
dim(tsw) = dim(encoded)

#Detecção de motifs

#Aplica a função paste que concatenar os elementos das linhas de tsw (matriz de representações SAX) em uma única string
seq = base::apply(tsw, MARGIN = 1, function(x) paste(as.vector(x), collapse=""))
#Cria dataframe com 2 colunas, i(indices) e seq
data = data.frame(i = 1:nrow(tsw), seq)
#Agrupa o data pelos valores em seq e resume contando o número total de ocorrências de cada sequência
result = data |> dplyr::group_by(seq) |> dplyr::summarise(total_count=dplyr::n())
#Filtra as linhas de result para manter apenas as que o número total de ocorrências é >= a obj$qtd 
result = result[result$total_count >= obj$qtd,]
#Ordena result em ordem decrescente de acordo com numero total de ocorrencias e seleciona seq do dataframe resultante
result = result |> dplyr::arrange(dplyr::desc(total_count)) |> dplyr::select(seq)
#Armazena as sequências resultantes 
result = result$seq
#Filtra as linhas de data para manter apenas aquelas em que a sequência (seq) está presente em result
motifs = data[data$seq %in% result,]

#Cria um vetor lógico (mots) de tamanho igual ao número de linhas em tsw
mots = rep(FALSE, nrow(tsw))
#Cria um vetor (seqs) de tamanho igual ao número de linhas em tsw
seqs = rep(NA, nrow(tsw))
#Define como TRUE os elementos em mots nas posições especificadas pelas linhas de motifs
mots[motifs$i] = TRUE
#Preenche as posições correspondentes em seqs com as sequências associadas às linhas de motifs.
seqs[motifs$i] = motifs$seq

#Cria um dataframe três colunas: idx representa os índices de 1 até o comprimento de mots, event recebe os valores lógicos de mots
detection = data.frame(idx=1:length(mots), event = mots, type="")
#Define como "motif" os valores da coluna type nas posições onde mots é TRUE.
detection$type[mots] = "motif"

print(detection)
