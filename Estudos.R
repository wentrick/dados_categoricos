# 1) Dados de exemplo (toy data)
#    aqui usamos um conjunto simples de larguras (x) e presença de satélites (y)
x <- c(20, 22, 23, 24, 25, 26, 27, 28)
y <- c( 0,  0,  0,  1,  0,  1,  1,  1)

# 2) Visualização rápida
plot(x, y, pch=19, xlab="Largura (cm)", ylab="Satélite (0/1)")

# 3) Função logística e logito
#    η_i = β0 + β1 * x_i
#    p_i = exp(η_i) / (1 + exp(η_i))
logit  <- function(eta) exp(eta) / (1 + exp(eta))

# teste com valores arbitrários de beta
beta0 <- -15
beta1 <-  0.6
eta   <- beta0 + beta1 * x
p_hat <- logit(eta)

# Veja as probabilidades preditas
data.frame(x, eta, p_hat)

# 4) Log-verossimilhança (máxima verossimilhança)
#    ℓ(β) = ∑[ y_i * η_i  –  log(1 + exp(η_i)) ]
logLik_fun <- function(b, x, y) {
  eta <- b[1] + b[2] * x
  sum(y * eta - log(1 + exp(eta)))
}
logLik_fun(c(beta0, beta1), x, y)

# 5) Estimação via otimização numérica
negLogLik <- function(b, x, y) {
  -logLik_fun(b, x, y)
}
init <- c(0, 0)  # chute inicial para (β0, β1)
opt  <- optim(init, negLogLik, x = x, y = y, hessian = TRUE)
coef_manual <- opt$par
coef_manual

# 6) Comparação com glm()
fit_glm <- glm(y ~ x, family = binomial(link="logit"))
coef(fit_glm)

# 7) Avaliando as probabilidades ajustadas
eta_glm <- coef(fit_glm)[1] + coef(fit_glm)[2] * x
p_glm   <- logit(eta_glm)
data.frame(x, Observado=y, Manual=paste0(round(logit(coef_manual[1] + coef_manual[2]*x),3)),
           GLM=paste0(round(p_glm,3)))

# 8) Plot das curvas ajustadas
curve(logit(coef_manual[1] + coef_manual[2] * x), from=19, to=29,
      ylim=c(0,1), ylab="P(satélite=1)", xlab="Largura (cm)",
      main="Ajuste Logístico Manual vs glm()")
points(x, y)
curve(logit(coef(fit_glm)[1] + coef(fit_glm)[2] * x), from=19, to=29,
      add=TRUE, lty=2)
legend("topleft", legend=c("Manual","glm()"), lty=1:2, bty="n")

################################################################################

# 1) Dados (frequências)
#   Renda: "<25" ou ">=25"; Cartão: 0 = não, 1 = sim; freq = número de observações
renda   <- c("<25", "<25", ">=25", ">=25")
cartao  <- c(    0,      1,      0,       1 )
freq    <- c(  200,    100,    100,     400 )  # da tabela de uso de cartão :contentReference[oaicite:1]{index=1}

# 2) Variável dummy: X = 0 se renda <25, 1 se >=25
X <- ifelse(renda == ">=25", 1, 0)

# 3) Função logística (logito)
logit <- function(eta) exp(eta) / (1 + exp(eta))

# 4) Log‑verossimilhança ponderada
#    ℓ(β) = ∑_i freq_i [ y_i η_i – log(1+exp(η_i)) ],  η_i = β0 + β1 X_i
logLik_fun <- function(b, X, y, w) {
  eta <- b[1] + b[2] * X
  sum(w * (y * eta - log(1 + exp(eta))))
}

# 5) Otimização para maximizar ℓ → minimizamos -ℓ
negLogLik <- function(b, X, y, w) -logLik_fun(b, X, y, w)
init     <- c(0, 0)  # chute inicial (β0, β1)
opt      <- optim(init, negLogLik, X = X, y = cartao, w = freq, hessian = TRUE)
coef_man <- opt$par
names(coef_man) <- c("Intercept", "β1")
coef_man

# 6) Odds ratio para a renda >=25 vs <25
exp(coef_man["β1"])

# 7) Ajuste com glm(), para conferir
df <- data.frame(cartao = cartao, renda_dummy = X, freq = freq)
fit_glm <- glm(cartao ~ renda_dummy,
               family = binomial(link = "logit"),
               weights = freq,
               data = df)
coef(fit_glm)

# 8) Probabilidades previstas em cada nível de renda
eta_man <- coef_man["Intercept"] + coef_man["β1"] * X
p_man   <- logit(eta_man)
eta_glm <- coef(fit_glm)[1] + coef(fit_glm)[2] * X
p_glm   <- logit(eta_glm)
data.frame(renda, freq, p_manual = round(p_man,3), p_glm = round(p_glm,3))

# 9) Gráfico comparativo
barplot(rbind(p_man[X==0], p_man[X==1]),
        beside = TRUE,
        names.arg = c("<25","≥25"),
        ylim = c(0,1),
        legend.text = c("manual","glm"),
        args.legend = list(x = "topleft"),
        main = "P(cartão = sim) por nível de renda",
        ylab = "Probabilidade")




