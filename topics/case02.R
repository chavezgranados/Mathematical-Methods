#                  Chapter 2: basic OLS
###########################################################
# Luis Chávez, 2024

## Problema 1: Data oldfaith

library(alr4)
data(oldfaith)
head(oldfaith)
help(oldfaith)
dim(oldfaith)

# Pregunta 1a: Diagrama de dispersion
windows()
plot(Duration ~ Interval, data = oldfaith)

# Pregunta 1b: Coeficientes de correlacion
cor(oldfaith$Interval, oldfaith$Duration, method = "pearson")
cor(oldfaith$Interval, oldfaith$Duration, method = "spearman")

# Pregunta 1c: Modelo de regresion lineal simple
oldfaith.m1 <- lm(Duration ~ Interval, data = oldfaith)
summary(oldfaith.m1)
abline(oldfaith.m1, col = "red")

# Pregunta 1d: Valores estimados y residuales
fitted.m1 <- fitted(oldfaith.m1)
head(fitted.m1)
residuales.m1 <- residuals(oldfaith.m1)
head(residuales.m1)

# Pregunta 1e: Intervalos de confianza para los coeficientes de regresion
confint(oldfaith.m1, level = 0.92)

# Pregunta 1f: Analisis de varianza
anova(oldfaith.m1)

# Pregunta 1g: Coeficiente de determinacion
summary(oldfaith.m1)$r.square
cor(oldfaith$Duration, oldfaith$Interval)^2

# Pregunta 1h: Predicciones
predict(oldfaith.m1, data.frame(Interval = 95))

# Pregunta 1i: Intervalo de confianza
predict(oldfaith.m1, data.frame(Interval = 95), level = 0.98, interval = 'confidence')

# Pregunta 1j: Intervalo de prediccion
predict(oldfaith.m1, data.frame(Interval = 95), level = 0.98, interval = 'prediction')

# Pregunta 1k: Evaluacion de supuestos
par(mfrow = c(2, 2))
plot(oldfaith.m1)

# Pregunta 1l: Conjunto de entrenamiento y prueba

library(caret)
RNGkind(sample.kind = "Rejection")
set.seed(4926)
ind.train <- createDataPartition(y = oldfaith$Duration, p = 0.70, list = FALSE)
data.train <- oldfaith[ind.train, ]
data.test <- oldfaith[-ind.train, ]

ctrl <- trainControl(method = 'none')
m1 <- train(Duration ~ Interval, data = data.train, method = 'lm',
            trControl = ctrl)
m1
m1.pred <- predict(m1, newdata = data.test)
postResample(m1.pred, data.test$Duration)

