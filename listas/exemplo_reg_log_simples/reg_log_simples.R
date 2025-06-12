# 1) Configurações iniciais --------------------------------------------------
# --- limpar área de trabalho
rm(list = ls())

pacman::p_load(tidyverse,readxl,dplyr,pROC)
# 2.2) Alternativamente, importando um arquivo Excel

carang <- read_excel("dados/Dados_carang_2025_1.xlsx", 
                     sheet = "Dados_carang", col_types = c("skip", 
                                                           "text", "numeric")) 
carang = carang %>%
  mutate(X =  gsub(",", ".", as.character(X)),
         X = str_replace_all(X, "\\s+", ""), #por algum motivo existe um "caracter invisivel"
         X = trimws(X,which = "both"), #nao sei se é necessario mas coloquei aqui
         X = as.numeric(X)) 

# se vier com cabeçalhos "ID", "X", "Y", eles serão usados automaticamente

# 3) Inspeção do data.frame --------------------------------------------------
str(carang)
head(carang, 10)
summary(carang)

# 4) Análise descritiva ------------------------------------------------------
# 4.1) Distribuição da variável resposta (Y)
table(carang$Y)
prop.table(table(carang$Y))

# 4.2) Estatísticas e gráficos da variável contínua (X)
summary(carang$X)
sd(carang$X, na.rm = TRUE)

# Histograma e boxplot
hist(carang$X,
     main = "Histograma de X (largura da carapaça)",
     xlab = "X (cm)",
     breaks = 12)
boxplot(carang$X,
        main = "Boxplot de X",
        ylab = "X (cm)")

# 5) Regressão Logística Simples ---------------------------------------------
# Em R, se y for numérico 0/1, glm() já ajusta logit[P(Y=1)] por padrão:

# 5.1) Modelo padrão: logit(P[Y=1]) = β0 + β1·X
m1 <- glm(Y ~ X,
          data   = carang,
          family = binomial(link = "logit"))
summary(m1)

# 5.2) Para “sucesso = 0” (equivalente ao proc logistic sem descending em SAS),
# você precisa reverter o fator para que o evento modelado seja y = 0:
carang$Y0 <- relevel(factor(carang$Y), ref = "1")
m2 <- glm(Y0 ~ X,
          data   = carang,
          family = binomial(link = "logit"))
# note que agora ele modela logit[P(Y0 = "0")] = log[P(Y=0)/P(Y=1)]
summary(m2)

# 5.3) Definindo explicitamente o evento de interesse via factor
# (equivalente a model y(event='1') no SAS)
carang$Y1 <- factor(carang$Y, levels = c(0,1))
m3 <- glm(Y1 ~ X,
          data   = carang,
          family = binomial(link = "logit"))
# aqui logit[P(Y=1)] novamente
summary(m3)

# 6) Previsões e curva ROC (opcional) ----------------------------------------
# install.packages("pROC")

# probabilidades ajustadas
p_hat <- predict(m1, type = "response")

# ROC
roc_obj <- roc(carang$Y, p_hat)
plot(roc_obj, main = paste("AUC =", round(auc(roc_obj), 3)))

# 7) Fim ----------------------------------------------------------------------
