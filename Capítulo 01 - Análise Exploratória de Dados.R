# Exemplo: Estimativas de Localização de População e Taxas de Homocídio

## Carregar o Datasets state,csv
PSDS_PATH <- file.path('.')
state <- read.csv(file.path(PSDS_PATH, 'data', 'state.csv'))

## Exemplo 1.1
mean(state[["Population"]])            ## Média
mean(state[["Population"]], trim=0.1)  ## Média Aparada (trim=0.1 exlui 10% de cada ponta)
median(state[["Population"]])          ## Mediana
