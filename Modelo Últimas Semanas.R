# venda pao mensal

### A ideia agora é decidir quantas semanas atrás pegar ###

# ler base de dados

install.packages('readxl')

library(readxl)

# arquivo do excel depois de inverter a ordem
venda_pao_diario = read_excel("C:/Users/Master/OneDrive/Área de Trabalho/PPGE/2 Econometria I/R/venda_pao_diario.xls")


# data frame do arquivo excel
venda_pao_diario


# criar um data frame apenas com quantidade, custo total, venda total, custo unitário e preço unitário

qtd_pao_diario=as.matrix(venda_pao_diario$Quantidade)
custo_pao_diario=as.matrix(venda_pao_diario$`Total Custo`)
totvend_pao_diario=as.matrix(venda_pao_diario$`Total Venda`)

qtd_custo_totvend_pao_diario = data.frame(cbind(qtd_pao_diario,
                                                custo_pao_diario,
                                                totvend_pao_diario,
                                                custo_pao_diario/qtd_pao_diario,
                                                totvend_pao_diario/qtd_pao_diario))

colnames(qtd_custo_totvend_pao_diario) = c("Quantidade",
                                           "Custo Total",
                                           "Vendas Totais",
                                           "Custo Unitário",
                                           "Preço Unitário")

# data frame com apenas 3 colunas originais e mais duas derivadas
qtd_custo_totvend_pao_diario

# separar por dias
# deu errado pois as colunas derivadas de resultados iguais a zero resultam em valores NaN

qtd_custo_totvend_pao_seg = qtd_custo_totvend_pao_diario[seq(1, 571, by = 7),]
qtd_custo_totvend_pao_ter = qtd_custo_totvend_pao_diario[seq(2, 571, by = 7),]
qtd_custo_totvend_pao_qua = qtd_custo_totvend_pao_diario[seq(3, 571, by = 7),]
qtd_custo_totvend_pao_qui = qtd_custo_totvend_pao_diario[seq(4, 571, by = 7),]
qtd_custo_totvend_pao_sex = qtd_custo_totvend_pao_diario[seq(5, 571, by = 7),]
qtd_custo_totvend_pao_sab = qtd_custo_totvend_pao_diario[seq(6, 571, by = 7),]
qtd_custo_totvend_pao_dom = qtd_custo_totvend_pao_diario[seq(7, 571, by = 7),]

qtd_custo_totvend_pao_diasem = data.frame(cbind(qtd_custo_totvend_pao_seg,
                                                qtd_custo_totvend_pao_ter,
                                                qtd_custo_totvend_pao_qua,
                                                qtd_custo_totvend_pao_qui,
                                                qtd_custo_totvend_pao_sex,
                                                qtd_custo_totvend_pao_sab,
                                                qtd_custo_totvend_pao_dom))

# tem que arrumar o erro
qtd_custo_totvend_pao_diasem

# regressão entre preço unitário e quantidade
# deu ruim
# não é regressão linear

# reg_p_qtd = lm ( qtd_custo_totvend_pao_diario$Quantidade 
#                 ~ qtd_custo_totvend_pao_diario$`Preço Unitário`)

# library(tidyverse)

# ggplot(qtd_custo_totvend_pao_diario, aes(x = qtd_custo_totvend_pao_diario$Quantidade, 
#                                         y = qtd_custo_totvend_pao_diario$`Preço Unitário`)) +
#                                        geom_point() +
#                                        geom_smooth(method = "lm", se = FALSE)

# criar data frame a partir do vetor único das quantidades
# separar por dias

qtd_pao_seg = qtd_pao_diario[seq(1, 571, by = 7)]
qtd_pao_ter = qtd_pao_diario[seq(2, 571, by = 7)]
qtd_pao_qua = qtd_pao_diario[seq(3, 571, by = 7)]
qtd_pao_qui = qtd_pao_diario[seq(4, 571, by = 7)]
qtd_pao_sex = qtd_pao_diario[seq(5, 571, by = 7)]
qtd_pao_sab = qtd_pao_diario[seq(6, 571, by = 7)]
qtd_pao_dom = qtd_pao_diario[seq(7, 571, by = 7)]

qtd_pao_diasem = data.frame(cbind(qtd_pao_seg,
                                  qtd_pao_ter,
                                  qtd_pao_qua,
                                  qtd_pao_qui,
                                  qtd_pao_sex,
                                  qtd_pao_sab,
                                  qtd_pao_dom))

qtd_pao_diasem

plot(qtd_pao_diasem$qtd_pao_seg,
     qtd_pao_diasem$qtd_pao_ter,
     qtd_pao_diasem$qtd_pao_qua,
     qtd_pao_diasem$qtd_pao_qui,
     qtd_pao_diasem$qtd_pao_sex,
     qtd_pao_diasem$qtd_pao_sab,
     qtd_pao_diasem$qtd_pao_dom)

summary(qtd_pao_diasem)

desio_qtd_pao_diasem = sqrt(diag(var(qtd_pao_diasem)))
desio_qtd_pao_diasem


# selecionar por dias

#qtd

qtd_pao_diario=as.matrix(venda_pao_diario$Quantidade)

qtd_pao_diario

qtd_pao_seg = qtd_pao_diario[seq(1, 571, by = 7)]
qtd_pao_ter = qtd_pao_diario[seq(2, 571, by = 7)]
qtd_pao_qua = qtd_pao_diario[seq(3, 571, by = 7)]
qtd_pao_qui = qtd_pao_diario[seq(4, 571, by = 7)]
qtd_pao_sex = qtd_pao_diario[seq(5, 571, by = 7)]
qtd_pao_sab = qtd_pao_diario[seq(6, 571, by = 7)]
qtd_pao_dom = qtd_pao_diario[seq(7, 571, by = 7)]

qtd_pao_diasem = data.frame(cbind(qtd_pao_seg,
                                  qtd_pao_ter,
                                  qtd_pao_qua,
                                  qtd_pao_qui,
                                  qtd_pao_sex,
                                  qtd_pao_sab,
                                  qtd_pao_dom))


colnames(qtd_pao_diasem) = c("Segunda",
                             "Terça",
                             "Quarta",
                             "Quinta",
                             "Sexta",
                             "Sábado",
                             "Domingo")

qtd_pao_diasem

plot(qtd_pao_diasem$qtd_pao_seg,
     qtd_pao_diasem$qtd_pao_ter,
     qtd_pao_diasem$qtd_pao_qua,
     qtd_pao_diasem$qtd_pao_qui,
     qtd_pao_diasem$qtd_pao_sex,
     qtd_pao_diasem$qtd_pao_sab,
     qtd_pao_diasem$qtd_pao_dom)

summary(qtd_pao_diasem)

desio_qtd_pao_diasem = sqrt(diag(var(qtd_pao_diasem)))
desio_qtd_pao_diasem

# valor vendido

totvend_pao_diario=as.matrix(venda_pao_diario$`Total Venda`)

qtd_pao_diario

qtd_pao_seg = qtd_pao_diario[seq(1, 571, by = 7)]
qtd_pao_ter = qtd_pao_diario[seq(2, 571, by = 7)]
qtd_pao_qua = qtd_pao_diario[seq(3, 571, by = 7)]
qtd_pao_qui = qtd_pao_diario[seq(4, 571, by = 7)]
qtd_pao_sex = qtd_pao_diario[seq(5, 571, by = 7)]
qtd_pao_sab = qtd_pao_diario[seq(6, 571, by = 7)]
qtd_pao_dom = qtd_pao_diario[seq(7, 571, by = 7)]

qtd_pao_diasem = data.frame(cbind(qtd_pao_seg,
                                  qtd_pao_ter,
                                  qtd_pao_qua,
                                  qtd_pao_qui,
                                  qtd_pao_sex,
                                  qtd_pao_sab,
                                  qtd_pao_dom))

qtd_pao_diasem = data.frame(qtd_pao_diasem)

qtd_pao_diasem

plot(qtd_pao_diasem$qtd_pao_seg,
     qtd_pao_diasem$qtd_pao_ter,
     qtd_pao_diasem$qtd_pao_qua,
     qtd_pao_diasem$qtd_pao_qui,
     qtd_pao_diasem$qtd_pao_sex,
     qtd_pao_diasem$qtd_pao_sab,
     qtd_pao_diasem$qtd_pao_dom)

summary(qtd_pao_diasem)

desio_qtd_pao_diasem = sqrt(diag(var(qtd_pao_diasem)))
desio_qtd_pao_diasem

# gráfico com pontos e densidade

plot(venda_pao_diario$Quantidade)
plot(density(venda_pao_diario$Quantidade))

plot(qtd_pao_seg)
plot(density(qtd_pao_seg))

plot(qtd_pao_ter)
plot(density(qtd_pao_ter))

plot(qtd_pao_qua)
plot(density(qtd_pao_qua))

plot(qtd_pao_qui)
plot(density(qtd_pao_qui))

plot(qtd_pao_sex)
plot(density(qtd_pao_sex))

plot(qtd_pao_sab)
plot(density(qtd_pao_sab))

plot(qtd_pao_dom)
plot(density(qtd_pao_dom))

summary(venda_pao_diario$Quantidade)
summary(qtd_pao_seg)
summary(qtd_pao_ter)
summary(qtd_pao_qua)
summary(qtd_pao_qui)
summary(qtd_pao_sex)
summary(qtd_pao_sab)
summary(qtd_pao_dom)

# separar por meses

qtd_pao_mens_nov19 = qtd_pao_diario[1:27]
qtd_pao_mens_dez19 = qtd_pao_diario[28:58]
qtd_pao_mens_jan20 = qtd_pao_diario[59:89]
qtd_pao_mens_fev20 = qtd_pao_diario[90:118]
qtd_pao_mens_mar20 = qtd_pao_diario[119:149]
qtd_pao_mens_abr20 = qtd_pao_diario[150:179]
qtd_pao_mens_mai20 = qtd_pao_diario[180:210]
qtd_pao_mens_jun20 = qtd_pao_diario[211:240]
qtd_pao_mens_jul20 = qtd_pao_diario[241:271]
qtd_pao_mens_ago20 = qtd_pao_diario[272:302]
qtd_pao_mens_set20 = qtd_pao_diario[303:332]
qtd_pao_mens_out20 = qtd_pao_diario[333:363]
qtd_pao_mens_nov20 = qtd_pao_diario[365:393]
qtd_pao_mens_dez20 = qtd_pao_diario[394:423]
qtd_pao_mens_jan21 = qtd_pao_diario[425:455]
qtd_pao_mens_fev21 = qtd_pao_diario[456:483]
qtd_pao_mens_mar21 = qtd_pao_diario[484:514]
qtd_pao_mens_abr21 = qtd_pao_diario[515:544]
qtd_pao_mens_mai21 = qtd_pao_diario[545:575]

plot(qtd_pao_mens_nov19)
plot(qtd_pao_mens_dez19)
plot(qtd_pao_mens_jan20)
plot(qtd_pao_mens_fev20)
plot(qtd_pao_mens_mar20)
plot(qtd_pao_mens_abr20)
plot(qtd_pao_mens_mai20)
plot(qtd_pao_mens_jun20)
plot(qtd_pao_mens_jul20)
plot(qtd_pao_mens_ago20)
plot(qtd_pao_mens_set20)
plot(qtd_pao_mens_out20)
plot(qtd_pao_mens_nov20)
plot(qtd_pao_mens_dez20)
plot(qtd_pao_mens_jan21)
plot(qtd_pao_mens_fev21)
plot(qtd_pao_mens_mar21)
plot(qtd_pao_mens_abr21)
plot(qtd_pao_mens_mai21)

summary(qtd_pao_mens_nov19)
summary(qtd_pao_mens_dez19)
summary(qtd_pao_mens_jan20)
summary(qtd_pao_mens_fev20)
summary(qtd_pao_mens_mar20)
summary(qtd_pao_mens_abr20)
summary(qtd_pao_mens_mai20)
summary(qtd_pao_mens_jun20)
summary(qtd_pao_mens_jul20)
summary(qtd_pao_mens_ago20)
summary(qtd_pao_mens_set20)
summary(qtd_pao_mens_out20)
summary(qtd_pao_mens_nov20)
summary(qtd_pao_mens_dez20)
summary(qtd_pao_mens_jan21)
summary(qtd_pao_mens_fev21)
summary(qtd_pao_mens_mar21)
summary(qtd_pao_mens_abr21)
summary(qtd_pao_mens_mai21)





# NÃO ROLOU
# tentando usar cbind na qtd_pao_diario

# qtd_pao_diario

# dia=(1:575)
# dias=(1:82)
# diass=(1:81)

### REG ####

# reg_qtd_pao_diario = lm ( dia ~ qtd_pao_diario )
# reg_qtd_pao_seg = lm ( dias ~ qtd_pao_seg )
# reg_qtd_pao_ter = lm(dias~qtd_pao_ter)
# reg_qtd_pao_qua = lm(dias~qtd_pao_qua)
# reg_qtd_pao_qui = lm(dias~qtd_pao_qui)
#reg_qtd_pao_sex = lm(diass~qtd_pao_sex)
#reg_qtd_pao_sab = lm(diass~qtd_pao_sab)
#reg_qtd_pao_dom = lm(diass~qtd_pao_dom)

### PLOT ####

#plot(density(reg_qtd_pao_diario))
#plot(reg_qtd_pao_seg)
#plot(reg_qtd_pao_ter)
#plot(reg_qtd_pao_qua)
#plot(reg_qtd_pao_qui)
#plot(reg_qtd_pao_sex)
#plot(reg_qtd_pao_sab)
#plot(reg_qtd_pao_dom)

### SUMMARY ####

#summary(reg_qtd_pao_diario)
#summary(reg_qtd_pao_seg)
#summary(reg_qtd_pao_ter)
#summary(reg_qtd_pao_qua)
#summary(reg_qtd_pao_qui)
#summary(reg_qtd_pao_sex)
#summary(reg_qtd_pao_sab)
#summary(reg_qtd_pao_dom)

#plot(density(venda_pao_diario$Quantidade))
