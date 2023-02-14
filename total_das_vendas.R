# total das vendas
# vendas agregadas, sem historicidade

# ler base de dados

install.packages('readxl')

library(readxl)

# arquivo do excel depois de inverter a ordem
total_das_vendas = read_excel("C:/Users/Master/OneDrive/Área de Trabalho/PPGE/2 Econometria I/R/total_das_vendas.xls")


# data frame do arquivo excel
total_das_vendas



# perc   100%      50%       75%       80%    95%
# qtd    13594     124       648       955    4242

plot(total_das_vendas$Valor)
plot(total_das_vendas$Valor[1:124])
plot(total_das_vendas$Valor[1:648])
plot(total_das_vendas$Valor[1:955])
plot(total_das_vendas$Valor[1:4242])
