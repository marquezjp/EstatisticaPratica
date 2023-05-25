# Exemplo: Estimativas de Localização de População e Taxas de Homocídio

## Carregar os Pacotes
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("matrixStats")
library("matrixStats")
install.packages("ascii")
library(ascii)



## Carregar o Datasets state,csv
caminho <- file.path('.')
state <- read.csv(file.path(caminho, 'data', 'state.csv'))

## Exemplo 1.1 - Estimativas de Localização
mean(state[["Population"]])            ## Média
mean(state[["Population"]], trim=0.1)  ## Média Aparada (trim=0.1 exlui 10% de cada ponta)
median(state[["Population"]])          ## Mediana

## Exemplo 1.2 - Estimativas de Localização 
weighted.mean(state[["Murder.Rate"]], w=state[["Population"]])

## Exemplo 1.3 - Estimativas de Variabilidade
sd(state[["Population"]])   ## Desvio Padrão
IQR(state[["Population"]])  ## Amplitude Interquartis
mad(state[["Population"]])  ## Desvio Absoluto Mediano da Mediana 

## Exemplo 1.4 - Percentis
quantile(state[["Murder.Rate"]], p=c(.05, .25, .5, .75, .95))

ascii(
  quantile(state[["Murder.Rate"]], p=c(.05, .25, .5, .75, .95)),
  include.rownames=FALSE, include.colnames=TRUE, digits=rep(2,5), align=rep("r", 5), 
  caption="Percentiles of murder rate by state.")

## Exemplo 1.5 - Boxplots
boxplot(state[["Population"]]/1000000, ylab="Population (millions)")

png(filename=file.path(caminho, "figures", "1.5-Boxplots.png"), width = 3, height=4, units='in', res=300)
par(mar=c(0,4,0,0)+.1)
boxplot(state[["Population"]]/1000000, ylab="Population (millions)")
dev.off()

## Exemplo 1.6 - Tabela de Frequencia
breaks <- seq(from=min(state[["Population"]]), to=max(state[["Population"]]), length=11)
pop_freq <- cut(state[["Population"]], breaks=breaks, right=TRUE, include.lowest = TRUE)
state['PopFreq'] <- pop_freq
table(pop_freq)

state_abb <- state %>%
  arrange(Population) %>%
  group_by(PopFreq) %>%
  summarize(state = paste(Abbreviation, collapse=","), .drop=FALSE) %>%
  complete(PopFreq, fill=list(state='')) %>%
  select(state) 

state_abb <- unlist(state_abb)

lower_br <- formatC(breaks[1:10], format="d", digits=0, big.mark=",")
upper_br <- formatC(c(breaks[2:10]-1, breaks[11]), format="d", digits=0, big.mark=",")

pop_table <- data.frame("BinNumber"=1:10,
                        "BinRange"=paste(lower_br, upper_br, sep="-"),
                        "Count"=as.numeric(table(pop_freq)),
                        "States"=state_abb)
ascii(pop_table, include.rownames=FALSE, digits=c(0, 0, 0, 0), align=c("l", "r", "r", "l"), 
      caption="A frequency table of population by state.")

## Exemplo 1.7 - Histogramas
hist(state[["Population"]], breaks=breaks)

png(filename=file.path(caminho, "figures", "1.7-Histogramas.png"),  width = 4, height=4, units='in', res=300)
par(mar=c(4,4,0,0)+.1)
pop_hist <- hist(state[["Population"]], breaks=breaks,
                 xlab="Population", main="")
dev.off()

## Exemplo 1.8 - Estimativa de Densidade
hist(state[["Murder.Rate"]], freq=FALSE )
lines(density(state[["Murder.Rate"]]), lwd=3, col="blue")

png(filename=file.path(PSDS_PATH, "figures", "1.8-Densidade.png"),  width = 4, height=4, units='in', res=300)
par(mar=c(4,4,0,0)+.1)
hist(state[["Murder.Rate"]], freq=FALSE, xlab="Murder Rate (per 100,000)", main="" )
lines(density(state[["Murder.Rate"]]), lwd=3, col="blue")
dev.off()

dfw <- read.csv(file.path(caminho, 'data', 'dfw_airline.csv'))
ascii(
  100*as.matrix(dfw/sum(dfw)),
  include.rownames=FALSE, include.colnames=TRUE, digits=rep(2,5), align=rep("r", 5), 
  caption="Percentage of delays by cause at Dallas-Ft. Worth airport.")

## Exemplo 1.9 - Grafico de Barras
barplot(as.matrix(dfw)/6, cex.axis = 0.8, cex.names = 0.7)

png(filename=file.path(PSDS_PATH, "figures", "1.9-Barras.png"),  width = 4, height=4, units='in', res=300)
par(mar=c(4, 4, 0, 1) + .1)
barplot(as.matrix(dfw)/6, cex.axis = 0.8, cex.names = 0.7)
dev.off()
