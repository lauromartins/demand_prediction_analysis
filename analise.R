## TRATAMENTO E PREPARACAO DOS DADOS


#inclusao dos pacotes necessarios
#install.packages('dplyr') #Pacote para a transformacao dos dados
suppressMessages(library(dplyr))

#troque pelo seu diretorio de trabalho
#faz a leitura do arquivo
arquivo = read.csv("C:/Users/Lauro Martins/Desktop/desafio.csv")


#obtem apenas os produtos que foram vendidos de fato
produtosVendidos = filter(arquivo, process_status == 'processado')


#mostra as 6 primeiras linhas do conjunto de dados obtido pela linha acima
#Obs.: algumas colunas foram ocultadas
head(produtosVendidos[, c(2, 3, 4, 8)])


#Para facilitar a compreensão, o código (code) e a categoria de cada produto foram 
#transformados em um número inteiro
produtosVendidos[, 'code'] = as.numeric(produtosVendidos$code) 
produtosVendidos[, 'category'] = as.numeric(produtosVendidos$category)


#mostra as 6 primeiras linhas do conjunto de dados após a conversão acima
#Obs.: algumas colunas foram ocultadas
head(produtosVendidos[, c(2, 3, 4, 8)])


#retira a coluna order_id por não ser necessária na análise
produtosVendidos['order_id'] = NULL


#obtem a quantidade de vendas de cada produto em ordem decrescente
qtdVendasCadaProduto = count(produtosVendidos, code, sort = TRUE) 


#mostra os 6 primeiros produtos com suas respectivas quantidades (n)
#Por exemplo, o produto 25 foi vendido 18943 vezes
head(qtdVendasCadaProduto)


#obtem a quantidade de produtos diferentes (131 produtos)
qtdProdutos = nrow(qtdVendasCadaProduto) 

#obtem a quantidade de vendas em cada categoria em ordem decrescente
qtdVendasCadaCategoria = count(produtosVendidos, category, sort = TRUE)

#mostra as 6 primeiras categorias com suas respectivas quantidade de vendas
#Por exemplo a categoria 1 foi a mais vendida, com 133046 vendas
head(qtdVendasCadaCategoria)

#obtem a quantidade de categorias diferentes (11 categorias)
qtdCategorias = nrow(qtdVendasCadaCategoria)




## ANÁLISE DOS DADOS - agrupamento



#obtem apenas as colunas numericas que interessam para o agrupamento
analise1 = select(produtosVendidos, code, quantity, price, pis_cofins, icms, tax_substitution, 
                  category, liquid_cost)

#ordena as linhas pelo codigo do produto
analise1 = arrange(analise1, code)


#agrupa pela categoria do produto
#obtem a quantidade de cada produto vendido
#obtem o preco total das vendas de cada produto
#obtem o  valor total do custo liquido de cada produto
#ordena em ordem decrescente pelo preco total das vendas de cada produto
x = analise1 %>% 
  group_by(category) %>%  
  summarise(qtd_produtos = sum(quantity),  
            total_venda = sum(price),  
            custo_liq = sum(liquid_cost)) %>%   
  arrange(desc(total_venda))  

#Segue abaixo uma tabela que mostra a relação de cada categoria com os seus respectivos valores
#Por exemplo, a categoria 1 foi a mais vendida
print(x)



#utiliza o algoritmo k-means da função padrão em R com 3 clusters (grupos)
#k-means é um algoritmo de agrupamento não supervisionado
km = kmeans(x, 3)


#plota um grafico que mostra 6 comparacoes:
#categoria de cada produto vs. quantidade de cada produto vendido
#categoria de cada produto vs. preco total das vendas de cada produto
#categoria de cada produto vs. valor total do custo liquido de cada produto
#quantidade cada produto vendido vs. preco total das vendas de cada produto
#quantidade cada produto vendido vs. valor total do custo liquido de cada produto
#preco total das vendas de cada produto vs. valor total do custo liquido de cada produto
plot(x, col = km$cluster+1, main = 'Resultado agrupamento com 3 clusters', pch = 20, cex = 3)





## ANÁLISE DOS DADOS - previsão


#pacotes para análise de séries temporais
#install.packages("forecast")
suppressMessages(library(xts))
suppressMessages(library(forecast))



#obtem os dados ordenados pela data de processamento (process_date)
analise2 = arrange(produtosVendidos, process_date)

#retira as duas primeiras linhas da tabela porque as datas sao invalidas: 0000-00-00
analise2 = analise2[-c(1, 2), ]


#obtem um subconjunto para a categoria 1
#para cada dia, mostra: 
#a quantidade de produtos vendidos, 
#o valor total de vendas,
#o valor total do custo líquido de cada produto, 
#o lucro obtido no dia
y = analise2 %>%
  filter(category == 1) %>%
  group_by(process_date) %>%
  summarise(qtd_produtos = sum(quantity),
            total_venda = sum(price),
            custo_liq = sum(liquid_cost),
            lucro = total_venda - custo_liq) #%> arrange(desc(lucro))


#mostra as 6 primeiras linhas do subconjunto obtido acima
head(y)

#mostra as 6 últimas linhas do mesmo subconjunto
tail(y)


#retira as 6 últimas linhas do subconjunto
#mostra que a última data agora é 31 de maio de 2017
y = y[-c((nrow(y)-5):nrow(y)), ]
tail(y)


#mostra uma serie temporal para o lucro da categoria 1
serie_lucro1 = xts(y$lucro, as.Date(y$process_date), frequency = 12)
plot(serie_lucro1, type = 'l', xlab = 'Data', ylab = 'Lucro', 
     main = 'Série Temporal para o Lucro', col = 'blue')




y %>%
  arrange(desc(lucro)) %>%
  head




#install.packages("ggfortify")
suppressMessages(library(ggfortify)) #pacote para recursos avançados de gráfico

#a média do total de vendas para cada mês foi calculada separadamente
#devido ao período das datas abranger dois anos diferentes (2016 e 2017)
mes = c('2016-06-30', '2016-07-31', '2016-08-31', '2016-09-30', '2016-10-31', 
        '2016-11-30', '2016-12-31', '2017-01-31', '2017-02-28', '2017-03-31', 
        '2017-04-30', '2017-05-31')
media_totalVenda = c(54043.64, 72910.23, 51237.94, 70816.98, 68800.35, 115493, 
                     77868.55, 111107.1, 68578.55, 91426.53, 80651.83, 118593.3)

#obtem uma tabela com a média do total de vendas para cada mês
df = data.frame(mes, media_totalVenda)


#obtem uma série temporal
ts_datas = ts(df$media_totalVenda, start = c(2016, 6), frequency = 12)
#plot(ts_datas)
ets_datas = ets(ts_datas)
f_ets = forecast(ets_datas, h = 3)
#plot(f_ets)

#plota um gráfico da série temporal com a previsão
autoplot(f_ets, ts.colour = 'blue', predict.colour = 'red', predict.linetype = 'dashed', 
         conf.int = TRUE, conf.int.fill = 'yellow') + 
  ggtitle('Projeção da média de vendas para os próximos 90 dias') +
  labs(x = 'Mês', y = 'Média total vendas') 



#obtem a quantidade de vendas para cada produto da categoria 1 em ordem decrescente
w1 = produtosVendidos %>%
  filter(category == 1) %>%
  group_by(code) %>%
  summarise(qtd_itensVendidos = sum(quantity)) %>%
  arrange(desc(qtd_itensVendidos))

head(w1)




## CONCLUSÕES



#obtem a quantidade de vendas para todos os produtos
w2 = produtosVendidos %>%
  group_by(code) %>%
  summarise(qtd_itensVendidos = sum(quantity)) %>%
  arrange(desc(qtd_itensVendidos))

#mostra somente as 6 primeiras linhas da tabela obtida acima
head(w2)





#obtem a quantidade de vendas para todos os produtos
w3 = arquivo %>%
  filter(process_status == 'processado') %>%
  group_by(code) %>%
  summarise(qtd_itensVendidos = sum(quantity)) %>%
  arrange(desc(qtd_itensVendidos))

#mostra somente as 6 primeiras linhas da tabela obtida acima
head(w3)