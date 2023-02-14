# historico geral das vendas de março a 25 de junho
# demanda geral

# ler base de dados
install.packages('readxl')
library(readxl)

# arquivo do excel depois de inverter a ordem
historico_vendas_mar_jun = read_excel("C:/Users/Master/OneDrive/Área de Trabalho/PPGE/2 Econometria I/R/historico_vendas_mar_jun.xls")

# vendas líquidas = dinheiro gasto no mercado

vendas_liquidas = historico_vendas_mar_jun$`Venda Líquida`
qtd_cupom = historico_vendas_mar_jun$`Qtde. Cupom`

vendas_liquidas_mar = vendas_liquidas[1:31]
qtd_cupom_mar = qtd_cupom[1:31]
hist_mar = cbind(1:31, vendas_liquidas_mar, qtd_cupom_mar)
colnames(hist_mar) = c("periodo", "vendas", "cupons")

# plotando e criando linha de tendência

pacman::p_load(dplyr, ggplot2, car, rstatix, lmtest, ggpubr)


# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

#dados = read.csv2('Banco de Dados 11.csv', stringsAsFactors = T) # Carregamento do arquivo csv
#View(dados)                                 # Visualização dos dados em janela separada
#glimpse(dados)                              # Visualização de um resumo dos dados

# Passo 3: Verificação dos pressupostos para a regressão linear

## Relação linear entre a VD e a VI:
### VD: Vendas - o que se quer prever
### VI: Publicidade - o que tem influência sobre a variável dependente

#plot(dados$Publicidade, dados$Vendas)

plot(hist_mar[,1],hist_mar[,2])
plot(hist_mar[,1],hist_mar[,3])

# modelos
mod1 <- lm(hist_mar[,2] ~ hist_mar[,1])
mod2 <- lm(hist_mar[,3] ~ hist_mar[,1])

## Análise gráfica:

par(mfrow=c(2,2))
plot(mod1)
plot(mod2)

# testes

# teste de normalidade dos resíduos
# se o p valor for maior que 0.05 então se considera que há normalidade
shapiro.test(mod1)

# outliers nos resíduos
# se os resíduos fogem do intervalo -3,+3 então há outliers
summary(rstandard(mod1))

# independência dos resíduos
# nao funciona com residuos nao normais
# statistic deve estar entre 1 e 3
# pvalor maior que 0.05 indica autocorrelação nula
durbinWatsonTest(mod1)

# homocedasticidade
# nao funciona com residuos nao normais
# pvalor maior que 0.05 indica que ha homocedasticidade
bptest(mod1)

# análise do modelo
summary(mod1)
summary(mod2)

#mod1$coefficients
#(Intercept) hist_mar[, 1] 
# 37423.7472     -209.2159

# fazendo um modelo com variáveis mais apertadas
mod3 <- lm(log(hist_mar[,2]) ~ hist_mar[,1])
plot(log(hist_mar[,2]) ~ hist_mar[,1])
mod4 <- lm(log(hist_mar[,3]) ~ hist_mar[,1])
plot(log(hist_mar[,3]) ~ hist_mar[,1])
plot(mod3)
plot(mod4)
summary(mod3)
summary(mod4)

# o R2 mostra que não há muita tendência, apesar do intercepto negativo
# consclusão, fazer regressoes para dias

# gráfico de dispersão
# nao sei como escolher x e y
ggplot(data = hist_mar, mapping = aes(x = hist_mar[,2], y = hist_mar[,1])) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label..,
                                          sep = "*plain(\",\")~~")),
                        label.x = 0, label.y = 400) +
  theme_classic()


vendas_liquidas_abr = vendas_liquidas[32:61]
vendas_liquidas_mai = vendas_liquidas[62:92]
vendas_liquidas_jun = vendas_liquidas[93:118]

plot(vendas_liquidas_mar)
plot(vendas_liquidas_abr)
plot(vendas_liquidas_mai)
plot(vendas_liquidas_jun)

#vendas_liquidas_mar1 = vendas_liquidas_mar
#vendas_liquidas_mar2
#vendas_liquidas_mar3
#vendas_liquidas_mar4

# 