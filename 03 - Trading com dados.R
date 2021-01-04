# Aula 02 – Trabalhando com as ações do IBOV através da BatchGetSymbols - Trading com Dados
# https://youtu.be/w1yHB2HtW5s
# Aula 03 – Plotando os gráficos das ações - Trading com Dados
# https://youtu.be/chC3bwEL55M?list=PLjdDBZW3EmXcIuLGbS3vp53H-DqMZ4IOS

install.packages("BatchGetSymbols")
install.packages("quantmod")
install.packages("GetDFPData")

library(BatchGetSymbols)
library(quantmod)
library(GetDFPData)

library(ggplot2)
library(ggthemes)
library(reshape2)
library(plyr)

# Inicio do nosso codigo
?BatchGetSymbols

acao = 'WEGE3.SA'
di = '2019-01-01'
df = Sys.Date()
benchmark = '^BVSP'



dados_acao = BatchGetSymbols(
  tickers = acao,
  first.date = di,
  last.date = df,
  bench.ticker = benchmark,
)

dados_acao = dados_acao$df.tickers

p = ggplot(dados_acao, aes(ref.date, price.adjusted)) + geom_line(color = 'blue')


p + labs(x = "Data", y = "Preço Ajustado", title = "Variação do Preço da Ação", subtitle = "De 01/01/2016 a 04/01/2020" )


# Seção 02
# Dados de várias ações de uma vez

ibov = GetIbovStocks()

ibov$tickersSA = paste(ibov$tickers, ".SA", sep= '')

dados_ibov = BatchGetSymbols(
  tickers = ibov$tickersSA,
  first.date = di,
  last.date = df,
  bench.ticker = benchmark,
)


dados_ibov = dados_ibov$df.tickers

dados_ibov2 = dlply(dados_ibov, .(ticker), function(x) {rownames(x) = x$row; x$row = NULL;x})
 

acao = dados_ibov2[[1]][,c(7,6)]

colnames(acao) = c("Data",paste("Preços", dados_ibov2[[1]][1,8]))

for (i in 2:79) {
  novaacao = dados_ibov2[[i]][,c(7,6)]
  
  colnames(novaacao) = c("Data",paste("Preços", dados_ibov2[[i]][1,8]))
  
  acao =  merge(acao, novaacao, by = "Data")
  
}



# Gerando Gráfico com várias ações
# Ações do setor bancário

f = ggplot() +
  geom_line(data = acao, aes(x = Data, y = acao$'Preços BBAS3.SA', color = "Banco do Brasil"))+
  geom_line(data = acao, aes(x = Data, y = acao$'Preços BBDC4.SA', color = "Bradesco"))+
  geom_line(data = acao, aes(x = Data, y = acao$'Preços ITUB4.SA', color = "Itaú Unibanco"))+
  geom_line(data = acao, aes(x = Data, y = acao$'Preços SANB11.SA', color = "Santander"))+
  
  xlab("Data")+
  ylab("Preço")
  
  f$labels$colour = "Bancos"

print(f)
  
  
