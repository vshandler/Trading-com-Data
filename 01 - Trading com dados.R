# Aula 01 - Conceitos de Investimentos e Introdução às Bibliotecas - Trading com Dados
# Video: https://youtu.be/7sOMtzdxTxs

install.packages("BatchGetSymbols")
install.packages("quantmod")
install.packages("GetDFPData")

library(BatchGetSymbols)
library(quantmod)
library(GetDFPData)

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




