# Puxando os dados automaticamente do computador.
caminho_arquivo_completo <- "C:/Users/marco/OneDrive/Documentos/Freelancer/Jessica TCC - Indicação Rayza/DataBase.csv"
suport <- read.csv(caminho_arquivo_completo)

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
#extract$Pergunta.17 <- if_else(extract$Pergunta.17 == "Menos de 5 anos", 1, 0)
extract$Pergunta.18 <- if_else(extract$Pergunta.18 == "Sim", 1, 0)

# Aqui teremos uma base de dados completa com os dois dataframes
df <- cbind(data,extract[, c("Pergunta.17", "Pergunta.18")])

# Aqui vamos estar calculando o omega do df completohttp://127.0.0.1:47453/graphics/plot_zoom_png?width=1920&height=1009
o <- omega(data %>% select(1:16), plot=F)

# Vamos utilizar o omega para converter os scores fatoriais em um novo df e adicionar esses scores no nosso df
g_scores <- as.data.frame(o$scores)
df <- cbind(df, g=g_scores$g)

# Agora vamos aplicar um modelo de PCA ou Análise de Componentes Principais
res <- PCA(df[,c(1,3:5,7:8)])

if(!require('factoextra')) {
  install.packages('factoextra')
  library('factoextra')
}

res.pca <- prcomp(df[, c(1,3:5,7:8)],  scale = TRUE)
get_eig(res.pca)

# Criando o primeiro gráfico.
options(repr.plot.width=10, repr.plot.height=10)
factoextra::fviz_eig(res, addlabels = TRUE, barcolor = "gray47", barfill = "gray47") + ggthemes::theme_fivethirtyeight() + labs(title="% em Variância Explicada pelas Variáveis da PCA") +
theme(plot.title = element_text(size=28), plot.subtitle = element_text(size=24), plot.caption = element_text(size=15), axis.text=element_text(size=18), axis.text.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")

# 2 graph
graph_2 <- df[, c(1,3:5,7:8)]  #Perguntas selecionadas
matriz_correlacao <- cor(graph_2)
colors <- colorRampPalette(c("white", "black"))(10)
options(repr.plot.width=50, repr.plot.height=50)
corrplot(matriz_correlacao, Colv=NA, Rowv=NA, col=colors, method='shade', addCoef.col = "grey", diag=F)

# 3 graph logistic
df$Pergunta.1 <- as.numeric(df$Pergunta.1)
df$Pergunta.2 <- as.numeric(df$Pergunta.2)
df$Pergunta.3 <- as.numeric(df$Pergunta.3)
df$Pergunta.4 <- as.numeric(df$Pergunta.4)
df$Pergunta.5 <- as.numeric(df$Pergunta.5)
df$Pergunta.6 <- as.numeric(df$Pergunta.6)
df$Pergunta.7 <- as.numeric(df$Pergunta.7)
df$Pergunta.8 <- as.numeric(df$Pergunta.8)
df$Pergunta.9 <- as.numeric(df$Pergunta.9)
df$Pergunta.10 <- as.numeric(df$Pergunta.10)
df$Pergunta.11 <- as.numeric(df$Pergunta.11)
df$Pergunta.12 <- as.numeric(df$Pergunta.12)
df$Pergunta.13 <- as.numeric(df$Pergunta.13)
df$Pergunta.14 <- as.numeric(df$Pergunta.14)
df$Pergunta.15 <- as.numeric(df$Pergunta.15)
df$Pergunta.16 <- as.numeric(df$Pergunta.16)

fit <- fit <- glm(Pergunta.18 ~ Pergunta.1 + Pergunta.2 + Pergunta.3 + Pergunta.4 + Pergunta.5 + Pergunta.6 + Pergunta.7 + Pergunta.8 + Pergunta.9 + Pergunta.10 + Pergunta.11 + Pergunta.12 + Pergunta.13 + Pergunta.14 + Pergunta.15 + Pergunta.16, df, family = "binomial")
p <- predict(fit, newdata = df[,c(1:17)], type = "response")
pred <- ifelse(p>0.5,1,0)
tab <- table(Predicted=pred, Actual=df$Pergunta.18)
caret::confusionMatrix(tab)
c <- as.data.frame(fit$coefficients)
c$name <- rownames(c)
colnames(c)[1] <- "coef"
c$odds <- exp(c$coef)
c <- c[-1,c(2,3)]
c <- c %>% arrange(-odds)
df$predict <- p
knitr::kable(c, caption="Baseado no histórico de dificuldades dos gestores, é necessário a definição e esclarecimento de cada função do colaborador", row.names = F)

SS# 4 graph
# Defina as categorias desejadas para o tempo de empresa
tempo_emp <- c('Mais de 5 anos', 'Menos de 5 anos')

# Filtrar os dados com base no tempo de empresa
data_total <- df %>% filter(Pergunta.17 %in% tempo_emp)

# Converter as variáveis categóricas em fatores
data_total <- as.data.frame(data_total) 

# Converter as variáveis categóricas em fatores (exceto Pergunta.17, Pergunta.18, g e predict)
for (i in 1:7) {
  col_name <- paste("Pergunta.", i, sep = "")
  data_total[[col_name]] <- factor(data_total[[col_name]])
}

# Converter Pergunta.17 em fator
data_total$Pergunta.17 <- factor(data_total$Pergunta.17)

# Converter Pergunta.18 em fator, se necessário
data_total$Pergunta.18 <- factor(data_total$Pergunta.18)

# Criar o gráfico Likert
lg <- likert(data_total[,c(1), drop=F], grouping=data_total$Pergunta.17)

# Configurar as opções do gráfico
options(repr.plot.width=15, repr.plot.height=10)

# Criar o gráfico Likert personalizado
plot(lg, colors = c("grey29", "grey39", "grey49", "grey59", "grey69"), text.size=8, center=1.5, digits = 0, include.center=F, group.order=c("Pergunta.1", "Pergunta.2", "Pergunta.3", "Pergunta.4", "Pergunta.5", "Pergunta.6", "Pergunta.7", "Pergunta.8", "Pergunta.9", "Pergunta.10", "Pergunta.11", "Pergunta.12", "Pergunta.13", "Pergunta.14", "Pergunta.15", "Pergunta.16", "Pergunta.17")) + 
  labs(title = "Legend", subtitle = "Sublegend") + 
  ggthemes::theme_fivethirtyeight() +
  theme(strip.text = element_text(size = 18), axis.text = element_text(size = 18), plot.title = element_text(size = 28), plot.subtitle = element_text(size = 24), plot.caption = element_text(size = 15))

