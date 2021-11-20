# Base de datos Kyphosis
# Modelo para predecir el tipo de deformación kyphosis, ausente o presente

library(rpart)
head(kyphosis)

kyphosis$y <- ifelse(kyphosis$Kyphosis=="absent", -1, 1)

# Modelo de referencia de un árbol tradicional

mod0 <- rpart(y ~ Age + Number + Start, data=kyphosis, method='class')
y0 <- predict(mod0, type='class')
tabla0 <- table(y0, kyphosis$y)
sum(diag(tabla0)) / sum(tabla0)

#Adaboost
#Se crean 20 iteraciones de manera manual para crear 20 tocones

T <- 20 # numero de iteraciones
m <- nrow(kyphosis)
Dt <- rep(1/m, m) # D1
alphas <- numeric(T) # Para almacenar alpha
y_hats <- matrix(NA, ncol=T, nrow=m) # Para almacenar la predicciones

for (i in 1:T) {
  mod <- rpart(y ~ Age + Number + Start, weights=Dt, data=kyphosis, 
               method='class', control=rpart.control(maxdepth = 1))
  y_hat <- predict(mod, type='class')
  error <- ifelse(y_hat == kyphosis$y, 0, 1) # 1=error, 0=ok
  epsilon_t <- sum(error * Dt)
  alpha_t <- 0.5 * log((1-epsilon_t)/epsilon_t)
  Fi <- ifelse(y_hat == kyphosis$y, exp(-alpha_t), exp(alpha_t))
  Dt <- Dt * Fi
  Dt <- Dt / sum(Dt)
  alphas[i] <- alpha_t
  y_hats[, i] <- ifelse(y_hat == '-1', -1, 1)
}

# Obtener estimaciones de los 20 tocones

y_final <- c(sign(y_hats %*% matrix(alphas, ncol=1)))
tabla_final <- table(y_final, kyphosis$y)
sum(diag(tabla_final)) / sum(tabla_final)

#Aplicamos el boosting de manera automática

library(adabag)
adaboost <- bagging(Kyphosis ~ Age + Number + Start,
                    coeflearn='Freund',
                    data=kyphosis, mfinal=20)
yhat_adaboost <- predict(adaboost, newdata=kyphosis)$class
tabla_adaboost <- table(yhat_adaboost, kyphosis$Kyphosis)
sum(diag(tabla_adaboost)) / sum(tabla_adaboost)

# Comparamos resultados
#Resultado de árbol tradicional de referencia (tasa de clasicicación menor)

sum(diag(tabla0)) / sum(tabla0)

#Resultado Adaboost manual

sum(diag(tabla_final)) / sum(tabla_final)

#Resultado Adaboost automático

sum(diag(tabla_adaboost)) / sum(tabla_adaboost)

