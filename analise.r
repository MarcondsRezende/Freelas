# Puxando os dados automaticamente do computador.
caminho_arquivo_completo <- "C:/Users/marco/OneDrive/Documentos/Freelancer/Jessica TCC - Indicação Rayza/DataBase.csv"
suport <- read.csv(caminho_arquivo_completo)
suport_edit <- suport[, 5:20]

caminho_arquivo_dados <- "C:/Users/marco/OneDrive/Documentos/Freelancer/Jessica TCC - Indicação Rayza/Dados.csv"
data <- read.csv(caminho_arquivo_dados)

# Extraindo as colunas para usar posteriormente.
colunas <- colnames(suport)
colunas <- c(colunas)
class(colunas)

# Instalando as bibliotecas necessárias.
library(tidyverse)
install.packages("likert")
library(likert)
library(lattice)
library(psych)
library(FactoMineR)
library(corrplot)
options(warn=-1)
warning=FALSE
library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)
install.packages("GPArotation")
library(GPArotation)


# Criando um novo dataframe com essas informações para colocarmos na base de dados (3 e 4 coluna da base antiga).
extract <- suport[, 3:4]
new_columns <- c("Pergunta.17", "Pergunta.18")
colnames(extract) <- new_columns

# Aqui teremos uma base de dados completa com os dois dataframes
df <- cbind(data,extract[, c("Pergunta.17", "Pergunta.18")])

# Aqui vamos estar calculando o omega do df completo
o <- omega(data, plot=F)

# Vamos utilizar o omega para converter os scores fatoriais em um novo df e adicionar esses scores no nosso df
g_scores <- as.data.frame(o$scores)
df <- cbind(df, g=g_scores$g)

# Agora vamos aplicar um modelo de PCA ou Análise de Componentes Principais
res <- PCA(df[, c(1,2,3,4,5,6,7,14)], graph=F) #Perguntas selecionadas

# Criando o primeiro gráfico.
options(repr.plot.width=10, repr.plot.height=10)
factoextra::fviz_eig(res, addlabels = T, barcolor = "gray47", barfill = "gray47") + ggthemes::theme_fivethirtyeight() + labs(title="% e Variância Explicada pelas Variáveis da PCA") +
theme(plot.title = element_text(size=28), plot.subtitle = element_text(size=24), plot.caption = element_text(size=15), axis.text=element_text(size=18), axis.text.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")

# 2 graph
graph_2 <- df[, c(1,2,3,4,5,6,7)]  #Perguntas selecionadas
matriz_correlacao <- cor(graph_2)
colors <- colorRampPalette(c("white", "black"))(10)
options(repr.plot.width=10, repr.plot.height=10)
corrplot(matriz_correlacao, Colv=NA, Rowv=NA, col=colors, method='square', addCoef.col = "grey", diag=F)


# 3 graph
bar_color <- c(rep(NA,29),rep("withcolor",13))
options(repr.plot.width=13, repr.plot.height=16)
data %>% group_by(airline) %>% summarise(g=median(g), n=n()) %>% mutate(z=(g - mean(g))/sd(g)) %>% filter(n>=83) %>% arrange(-z) %>%
  ggplot(aes(reorder(airline,z),z, fill=factor(bar_color))) + geom_bar(stat='identity') + geom_label(aes(label=round(z,2), size=5)) + coord_flip() +
  labs(title="TITULO", subtitle="SUBTITULO", caption = "legenda inferior") +
  ggthemes::theme_fivethirtyeight() + 
  theme(plot.title = element_text(size=28), plot.subtitle = element_text(size=24), plot.caption = element_text(size=15), axis.text=element_text(size=18), axis.text.x=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")

