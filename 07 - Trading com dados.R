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
  
  
# Seção 03
# Aula 04 – Normalizando o preço das ações - Trading com Dados

# Utilizar indices de referência no mercado financeiro

IBOV =  BatchGetSymbols(
  tickers = '^BVSP',
  first.date = di,
  last.date = df,
  bench.ticker = benchmark,
)

IBOV = IBOV$df.tickers

colnames(IBOV)[6] = 'IBOV'
colnames(IBOV)[7] = 'Data'

IBOV = IBOV[,c(7,6)]
  
SP500 =  BatchGetSymbols(
  tickers = '^GSPC',
  first.date = di,
  last.date = df,
  bench.ticker = '^GSPC',
)

SP500 = SP500$df.tickers

colnames(SP500)[6] = 'SP500'
colnames(SP500)[7] = 'Data'

SP500 = SP500[,c(7,6)]

ibov_sp500  = merge(IBOV, SP500, by = "Data")

total  = merge(ibov_sp500, acao, by = "Data")


normalizado = total[,-c(1)]

novo_total = data.frame(lapply(normalizado, function(x) x/x[1]))

novo_total$Data = total$Data

g = ggplot() +
  geom_line(data = novo_total, aes(x = Data, y = novo_total$Preços.EZTC3.SA, color = "EZTEC"))+
  geom_line(data = novo_total, aes(x = Data, y = novo_total$Preços.MRVE3.SA, color = "MRV"))+
  geom_line(data = novo_total, aes(x = Data, y = novo_total$Preços.CYRE3.SA, color = "Cyrela"))+
  geom_line(data = novo_total, aes(x = Data, y = novo_total$IBOV, color = "IBOV"))+
  geom_line(data = novo_total, aes(x = Data, y = novo_total$SP500, color = "S&P 500"))+
  
  xlab("Data")+
  ylab("Preço")

g$labels$colour = "Construção"

print(g)


# Plotar todas as colunas 

df = melt(novo_total, id.vars = 'Data', variable.name = 'series')

ggplot(df, aes(Data, value)) + geom_line(aes(colour = series))

novo_total2 = novo_total[,c(82,1:4)]

df = melt(novo_total2, id.vars = 'Data', variable.name = 'series')

ggplot(df, aes(Data, value)) + geom_line(aes(colour = series))

# Visualizar em plots separados

ggplot(df, aes(Data, value)) + geom_line() + facet_grid(series ~.)


# Seção 04
# Calculando correlação e construindo nosso próprio portifólio
install.packages('corrplot')
library(corrplot)

# Tudo
correlacoes = cor(normalizado, use = "complete.obs", method = 'spearman')
corrplot(correlacoes, number.cex = 0.001, number.font = 5)

# Filtrando com numeros
tabela01 = normalizado[,c(1,2,15:25)]

correlacoes = cor(tabela01, use = "complete.obs", method = 'spearman')
corrplot(correlacoes, number.cex = 1, number.font = 1, method = 'number', type = 'lower')

# Filtrando com nomes
tabela02 = normalizado[,colnames(normalizado) %in% c("Preços ABEV3.SA", "Preços B3SA3.SA", "Preços ITUB4.SA","Preços MGLU3.SA", "Preços WEGE3.SA"   )]

correlacoes = cor(tabela02, use = "complete.obs", method = 'spearman')
corrplot(correlacoes, number.cex = 1, number.font = 1, method = 'number', type = 'lower')


# Construção do portfólio

#Se estiver com valores negativos
#novo_total$Preços.MGLU3.SA = (novo_total$Preços.MGLU3.SA)*(-1)

novo_total$carteira = 0.2*novo_total$Preços.ABEV3.SA +
  0.15*novo_total$Preços.B3SA3.SA +
  0.15*novo_total$Preços.EZTC3.SA +
  0.3*novo_total$Preços.WEGE3.SA +
  0.2*novo_total$Preços.MGLU3.SA


h = ggplot() +
  geom_line(data = novo_total, aes(x = Data, y = novo_total$carteira, color = "Meu Portfólio"))+
  geom_line(data = novo_total, aes(x = Data, y = novo_total$Preços.WEGE3.SA, color = "Weg"))+
  geom_line(data = novo_total, aes(x = Data, y = novo_total$Preços.MGLU3.SA, color = "Magazine Luiza"))+
  geom_line(data = novo_total, aes(x = Data, y = novo_total$IBOV, color = "IBOV"))+
  geom_line(data = novo_total, aes(x = Data, y = novo_total$SP500, color = "S&P 500"))+
  
  xlab("Data")+
  ylab("Preço")

h$labels$colour = "Ativos vs. Portfólio"

print(h)

#Correlação da carteira
tabela03 = novo_total [,colnames(novo_total) %in% c("Preços.ABEV3.SA", "Preços.B3SA3.SA", "Preços.ITUB4.SA","Preços.MGLU3.SA", "Preços.WEGE3.SA","carteira", "SP500", "IBOV")]

correlacoes = cor(tabela03, use = "complete.obs", method = 'spearman')
corrplot(correlacoes, number.cex = 1, number.font = 1, method = 'number', type = 'lower')


# Seção 5
# Utilizando a Biblioteca quantmod

library(quantmod)
?getSymbols

getSymbols(Symbols = "AAPL", from = '2020-01-01', src = 'yahoo')

dados_apple = getSymbols(Symbols = "AAPL", from = '2020-01-01', src = 'yahoo', auto.assign = FALSE)

chart_Series(dados_apple)

dados_weg = getSymbols(Symbols = "WEGE3.SA", from = '2020-01-01', src = 'yahoo', auto.assign = FALSE)

chart_Series(dados_weg)







