# tiagoalvescordeiro_RM561791_fase2_cap9 | EdsonHenriqueFelixBatisa_RM566321 | MatheusParra_RM561907


library(readxl)
library(ggplot2)

# Importar os dados
dados <- read_excel("dados_soja_estado.xlsx")

# --- ANÁLISE DA VARIÁVEL QUANTITATIVA CONTÍNUA: Produção Soja (ton) ---

# Medidas de Tendência Central
media <- mean(dados$`Produção Soja (ton)`)
mediana <- median(dados$`Produção Soja (ton)`)
moda <- as.numeric(names(sort(table(dados$`Produção Soja (ton)`), decreasing = TRUE)[1]))

# Medidas de Dispersão
desvio_padrao <- sd(dados$`Produção Soja (ton)`)
variancia <- var(dados$`Produção Soja (ton)`)
amplitude <- max(dados$`Produção Soja (ton)`) - min(dados$`Produção Soja (ton)`)

# Medidas Separatrizes
quartis <- quantile(dados$`Produção Soja (ton)`)
decis <- quantile(dados$`Produção Soja (ton)`, probs = seq(0, 1, 0.1))
percentis <- quantile(dados$`Produção Soja (ton)`, probs = seq(0, 1, 0.01))

# Gráfico - Boxplot
boxplot(dados$`Produção Soja (ton)`, main = "Boxplot da Produção de Soja", ylab = "Toneladas")

# Histograma
hist(dados$`Produção Soja (ton)`, main = "Histograma da Produção de Soja", xlab = "Toneladas", col = "lightblue", breaks = 10)

# --- ANÁLISE DA VARIÁVEL QUALITATIVA: Tipo de Cultivo ---

# Frequência
table(dados$`Tipo de Cultivo`)

# Gráfico de barras
barplot(table(dados$`Tipo de Cultivo`), main = "Tipo de Cultivo", col = "lightgreen", ylab = "Frequência")

# Gráfico de pizza
pie(table(dados$`Tipo de Cultivo`), main = "Distribuição do Tipo de Cultivo")

