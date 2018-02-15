# leitura do conjunto de dados - http://archive.ics.uci.edu/ml/datasets/Challenger+USA+Space+Shuttle+O-Ring

temp = c(66, 70, 69, 68, 67, 72, 73, 70, 57, 63, 70, 78, 67, 53, 67, 75, 70, 81, 76, 79, 75, 76, 58)
ring = c(0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1)

temp <- (temp-32)*5/9

dados <- data.frame(temperatura=temp, falha=ring)

# 

library(ggplot2)
theme_set(theme_bw())

ggplot(dados, aes(x=temperatura, y=falha)) +
  geom_point() +
  labs(x="Temperatura (ºF)", y="Falha") +
  scale_y_continuous(breaks=c(0, 1))

# ajuste do modelo

ajuste <- glm(falha ~ temperatura, data=dados, family=binomial)

summary(ajuste)

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

ggplot(dados, aes(x=temperatura, y=falha)) +
  geom_point() +
  labs(x="Temperatura (ºF)", y="Falha") +
  scale_y_continuous(breaks=c(0, 1)) +
  binomial_smooth(se=FALSE)

temperatura <- seq(0, 40, 5)
data.frame(temperatura=temperatura, 
           probabilidade=predict(ajuste, data.frame(temperatura=temperatura), type="response"))


# plot dos resultados

library(ggfortify)
autoplot(ajuste)

