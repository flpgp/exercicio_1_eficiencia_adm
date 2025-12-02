# código que análise os municipios eficientes para agricultura, conforme fornecido pelo professor.
# Instalar e cargar librerias
library(dplyr)
library(ggplot2)
library(geobr)
library(tidyverse)
library(deaR)
library(leaflet)
library(leaflet.extras)
library(rworldxtra)
library(raster)
library(sf)
library(leaflet.extras)
library(rworldxtra)
library(raster)
library(ggspatial)

getwd()

# baixar dados

# x1	Área dos estabelecimentos (hectares)
# x2	Gasto anual com combustíveis e lubrificantes (Mil reais)
# x3	Gasto anual com insumos para produção vegetal e animal (Mil reais)
# x4	Mão de obra ocupada nos estabelecimentos (assalariada e familiar)
# x5	Outras despesas (mil reais)
# y1	Receita bruta anual (Mil reais)
# y2	Área de matas e florestas naturais e plantadas nos estabelecimentos (hectares)
# y3	Emissões anuais de gases de efeito estufa sector agropecuário (toneladas GWP)
# y4	Índice de diversidade de Shannon-Weaver
# z1	Percentual de estabelecimentos classificados  como agricultura familiar
# z2	Percentagem dos estabelecimentos que receberam assistência técnica
# z3	Percentagem dos proprietários associados à cooperativa
# z4	Percentagem dos estabelecimentos em que o produtor possui pelo menos ensino médio
# z5	Percentagem dos estabelecimentos em que o produtor é proprietário da terra
# z6	Percentagem dos estabelecimentos que obtiveram financiamento
# z7	Densidade populacional do município (hab./km²)
# z8	Latitude
# z9	Longitude

library(readr)
DadosMaster1EcoEffBr <- read_delim("00.dados/DadosMaster1EcoEffBr.csv", 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

#DEcoEfMB5<-DEcoEfMB5 %>% mutate(Longitude=as.numeric (Longitude))
# filtrar os dados de Go
MunGoias<-DadosMaster1EcoEffBr %>% filter(CódIBGEE==52)
summary(MunGoias)
MunCO <- DadosMaster1EcoEffBr %>% filter(REGIÃO=="CENTRO-OESTE")

# O operador %>%, conhecido como "pipe", pertence ao pacote aos pacote tidyverse 
# e é utilizado para facilitar a encadeação de funções. 
# Ele permite que o resultado de uma expressão à esquerda seja 
# passado como o primeiro argumento para a função à direita. 
# Isso torna o código mais legível e modular.
# Para utilizar o operador %>% no R, você pode também carregar o pacote dplyr

## Utilizaremos o deaR. Para acessar a documentação das funções deaR 
# usamos a função help() escrevendo: help(package="deaR")

# Uma vez carregados os dados, o próximo passo é adaptá-los ao formato que 
# o deaR utiliza para lê-los.Para isso usamos a função make_deadata 
# com as colunas das dmus, inputs e outputs.

data_MGo <- make_deadata(MunGoias,dmus = 1,
                         inputs = 9:13, 
                         outputs = 14:17)
# Uma vez preparados os dados para que possam ser lidos pelo deaR, o próximo
# passo é selecionar o modelo DEA e executá-lo.Neste caso usaremos o model_basic
# execute o modelo DEA CCR orientado aos inputs para todas as DMUs

# Aviso: Isso indica que, dentro das colunas selecionadas (Inputs 9:13 e Outputs 14:17), 
# alguns valores são muito pequenos (próximos de zero, como 0.001) e outros são 
#muito grandes (como 100.000 ou 1.000.000).

result_MGo_CRS_IO<- model_basic(data_MGo,  orientation='io',  rts="crs", compute_multiplier =TRUE) 

eff <- efficiencies(result_MGo_CRS_IO) # Indica as pontuações de eficiência
s <- slacks(result_MGo_CRS_IO) #  Indica as folga
lamb <- lambdas(result_MGo_CRS_IO)# Indica as intensidades ou lambdas
tar <- targets(result_MGo_CRS_IO) # Indica os valores alvo (melhoras)
ref <- references(result_MGo_CRS_IO) # Indica o conjunto de referência de DMUs ineficientes
returns <- rts(result_MGo_CRS_IO) # retornos de escala que caracterizam uma DMU
multipliers(result_MGo_CRS_IO)
summary(result_MGo_CRS_IO, exportExcel = TRUE, filename = "resultado1.xlsx")
plot(result_MGo_CRS_IO) # gráficos e Press [rum] to continue



result_MGo_CRS_OO<- model_basic(data_MGo,  orientation='oo',  rts="crs") 
result_MGo_VRS_IO<- model_basic(data_MGo,  orientation='io',  rts="vrs") 
result_MGo_VRS_OO<- model_basic(data_MGo,  orientation='oo',  rts="vrs") 


# Exportar resultado para Excel
summary(result_MGo_CRS_IO , exportExcel = TRUE, filename = NULL)
summary(result_MGo_CRS_OO , exportExcel = TRUE, filename = NULL)
summary(result_MGo_VRS_IO , exportExcel = TRUE, filename = NULL)
summary(result_MGo_VRS_OO , exportExcel = TRUE, filename = NULL)

# onde foi salvo o Excel?
getwd()

# Teste de diferenca de modelos
# Gráficos
g1 <- efficiencies(result_MGo_CRS_IO)
g2 <- efficiencies(result_MGo_VRS_IO)
par(mfrow = c(2, 2))
hist(g1, xlab=" Ecoeficiência RCE", ylab= "Densidade", probability=TRUE)
lines(density(g1))
hist(g2, xlab=" Ecoeficiência RVE", ylab= "Densidade", probability=TRUE)
lines(density(g2))
boxplot(g1)
boxplot(g2)

# Aqui o R executa o Teste de Kolmogorov-Smirnov (KS) para duas amostras com a hipótese 
# alternativa bicaudal (bilateral).
ks.test(g1,g2, alternative = "two.sided")
# O Teste de Kolmogorov-Smirnov (KS) para duas amostras pode ser utilizado para 
# avaliar se a distribuição de probabilidade dos escores de eficiência obtidos pelo 
# modelo DEA-CCR é diferente da distribuição dos escores obtidos pelo modelo DEA-BCC.
# Hipótese Nula (Ho): As distribuições dos escores de CCR e BCC são as mesmas.
# Hipótese Alternativa (Ha): As distribuições dos escores são diferentes.
# Se o teste KS rejeitar $H_0$, a diferença nas distribuições dos escores sugere 
# que há uma ineficiência de escala significativa na amostra, o que é 
# uma descoberta importante em análises DEA.

# gráficos e Press [rum] to continue 
plot(result_MGo_CRS_IO)

par(mfrow=c(1,2))
hist(eff, xlab=" Ecoeficiência", ylab= "Densidade", probability=TRUE, main="")
lines(density(eff))
boxplot(eff)
resumem<- summary(eff)


######################
# Mapa
# O pacote geobr tem as rotinas para o download de dados e 
# mapas com divisões territoriais variadas.
# Permite baixar os dados com as coordenadas do Brasil
# O pacote geobr permite aos usuários acessar facilmente 
# os shapefiles ( dados para representar feições geográficas) 
# do IBGE e outros conjuntos oficiais de dados espaciais do Brasil. 

help(geobr)
List <- list_geobr()
# é utilizada para retornar um data frame ou lista de todas as 
# bases de dados geográficas do Brasil
# ajuda quando a internet é lenta
options(timeout= 4000000)

# baixar dados dos estados
EstadosB <- read_state(year = 2019, showProgress = FALSE)
#para baixar os municípios de Goiás fazendo uso da função:
mun_Go <- read_municipality(code_muni=52, year=2019)

# plot
par(mfrow=c(1,1))
plot(EstadosB)
ggplot() +
  geom_sf(data=EstadosB, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="Estados do Brasil", size=8) +
  theme_minimal()

# mesoregião
MesoReg <- read_meso_region(year = 2020)
ggplot() +
  geom_sf(data=MesoReg, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="Meso regioes", size=8) +
  theme_minimal()

#Municípios
MapaMB2<-read_municipality(year = 2016)
#plot(MapaMB2)
ggplot() +
  geom_sf(data=MapaMB2, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="Municípios", size=8) +
  theme_minimal()

# A seguinte função é usada para carregar o shapefile dos 
# municípios de um estado específico no Brasil, em um determinado ano.
MapaMGo<-geobr::read_municipality(code_muni = "GO",  year = 2016)
#plot(MapaMGo)
ggplot() +
  geom_sf(data=MapaMGo, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="Municípios de GO", size=8) +
  theme_minimal()

# baixar Biomas
Biomas <- geobr::read_biomes(year = 2019)
ggplot() +
  geom_sf(data=Biomas, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="States", size=8) +
  theme_minimal()

# baixar dados das escolas
Escola <- geobr::read_schools(year = 2020)
EscBrs<-Escola %>% filter(name_muni=="Brasília")
SCBrs <- read_census_tract(code_tract="DF")
Brs <- geobr::read_municipality(code_muni = "DF",  year = 2016)
ggplot() +
  geom_sf(data=Brs, fill="#2D3E50", size=.15, show.legend = FALSE) +
  geom_sf(data=SCBrs,  color="#FEBF57", size=.5, show.legend = FALSE) +
  geom_sf(data=EscBrs,  color="red", size=.5, show.legend = FALSE) +
  labs(subtitle="Escolas de Brasília", size=10) +
  theme_minimal()
###
library(leaflet)
#  O pacote leaflet do R é uma biblioteca usada para criar mapasweb 
# interativos e dinâmicos.
map <- leaflet(EscBrs)%>%addTiles()
map%>%addMarkers()

# mun_Go <- read_municipality(code_muni=52, year=2019)
# filtrar os dados de Go
MapaMunGoias<-MapaMB2 %>% filter(code_state==52)

# plot
ggplot() +
  geom_sf(data=MapaMunGoias, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="Municípios de Go", size=8) +
  theme_minimal()

# Incorpora os índices de eficiência (eff) no banco de dados MunGoias
df<-data.frame(MunGoias,eff)
df1<-data.frame(MapaMunGoias,eff)

## No df, renomear CódIBGE para fazer a junção do dt com MapaMunGoias
ResultsDEAGO<-df %>% rename(code_muni=CódIBGE)

# fazer a junção dos data.frame
# As junções (join) adicionam colunas de um df a outro, 
# combinando observações com base nas chaves. 

MunGoias=MapaMunGoias %>% left_join(ResultsDEAGO)

summary(df$eff)

# plotar o mapa temático do pacote ggplot2
ggplot()+
  geom_sf(data=MunGoias,aes(fill=eff))+
  scale_fill_distiller(palette = "Greens", direction = 1, 
                       name="Ecoeficiência dos municípios de Goiás", 
                       limits=c(0,1.01))+
  labs(title = 'Ecoeficiência CRS orientada a minimizar os insumos da agropecuária de Goiás', 
       subtitle = "(min:0.1285, 1ºQ.: 0.2724, média:0.3738, 3ºQ.:0.5441, max: 1.0000)",
       caption =  'Fonte: Cálculo com base nos dados do censo agropecuário de 2017')+
  annotation_scale(location='br')+
  annotation_north_arrow(location='tl')

# aes(fill = eff): pinta cada município conforme seu índice de ecoeficiência eff.
# annotation_scale(location='br')= Insere uma barra de escala no canto inferior 
# direito (br = bottom right).
# annotation_north_arrow= Coloca uma seta de orientação Norte 
# no canto superior esquerdo (tl = top left)

# scale_fill_gradientn(colours = grey(seq(1, 0, length.out = 256)))

ggplot()+
  geom_sf(data=MunGoias,aes(fill=eff))+
  scale_fill_gradientn(colours = grey(seq(1, 0, length.out = 256)), 
                       name="Ecoeficiência dos municípios de Goiás", 
                       limits=c(0,1.01))+
  labs(title = 'Ecoeficiência CRS orientada a minimizar os insumos da agropecuária de Goiás', 
       subtitle = "(min:0.1285, 1ºQ.: 0.2724, média:0.3738, 3ºQ.:0.5441, max: 1.0000)",
       caption =  'Fonte: Cálculo com base nos dados do censo agropecuário de 2017')+
  annotation_scale(location='br')+
  annotation_north_arrow(location='tl')

###########
# Construir uma rede (grafo) que mostra quais municípios ineficientes são 
# conectados aos seus benchmarks utilizando resultados
library(tidyverse)

lambda_mat <- lambdas(result_MGo_CRS_IO)
lambda_df <- as.data.frame(lambda_mat)

lambda_df$DMU <- rownames(lambda_df)

# Converte matriz de lambdas em formato longo
edges <- lambda_df %>%
  pivot_longer(-DMU, names_to = "Benchmark", values_to = "lambda") %>%
  filter(lambda > 1e-6) %>%            # remove zeros numéricos
  filter(DMU != Benchmark)             # remove auto-ligações

library(igraph)

g <- graph_from_data_frame(edges, directed = TRUE)
plot(g,
     vertex.size = 7,
     vertex.label.cex = 0.7,
     edge.arrow.size = 0.3,
     vertex.color = "skyblue",
     main = "Rede de Ligações: DMUs Ineficientes → Benchmarks (DEA)")

plot(g,
     vertex.size = 5,
     vertex.label.cex = 0.7,
     edge.arrow.size = 0.3,
     vertex.color = ifelse(eff == 1, "green3", "tomato"),
     main = "Rede de Ligações: DMUs Ineficientes → Benchmarks (DEA)")
