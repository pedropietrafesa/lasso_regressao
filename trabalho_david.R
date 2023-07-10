#### Regressão Lasso ####

### pacotes ####
install.packages("glmnet")
install.packages("selectiveInference")
install.packages("ISLR")
install.packages("sjmisc")

library(glmnet)
library(selectiveInference)
library(ISLR)
library(corrplot)
library(sjmisc)

### Dados ####
# Conjunto de dados dos desempenhos dos rebatedores MLB de 1986 a 1987

set.seed(123)
?Hitters

dados <- na.omit(Hitters)
head(dados)


### colinearidade ###
# Uma das Justificativas para realizar uma Regressão Lasso

# retirar as variáveis categóricas

r <- cor(dados[, -c(14,15,20)])

corrplot(r, method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black",
         tl.srt = 45,
         diag = F)
# Há presença de 10 variáveis altamente correlacionadas, de um total de 20

# matriz X do modelo de regressão 
# comando model.matrix padroniza variáveis dummy, necessário ao comando glmnet,
#não entra os dados como data.frame
x <- model.matrix(Salary ~., dados)[,-1]  # retirar beta0
y <- dados$Salary # variável resposta


#### Econtrar o lambda - Validação Cruzada #####

cv.lambda.lasso <- cv.glmnet(x=x, y=y, 
                             alpha = 1) 
plot(cv.lambda.lasso)

cv.lambda.lasso

plot(cv.lambda.lasso$glmnet.fit, 
     "lambda", label=FALSE)

#### Estimativas de Lasso ##### 

l.lasso.min <- cv.lambda.lasso$lambda.1se


lasso.model <- glmnet(x=x, y=y,
                      alpha  = 1, 
                      lambda = l.lasso.min)
lasso.model$beta  

###### inferência após seleção de variáveis ######

# necessidade de re-escalar lambda no pacote selectiveInference em relação a glmnet
#necessidade de estimar sigma2

sig <- estimateSigma(x,y)$sigmahat
b <- coef(lasso.model, s = l.lasso.min/263)[-1]
res <- fixedLassoInf(x,y,b,l.lasso.min, sigma = sig)
res






length(x)


fit <- lar(x,y, maxsteps = 20)
fit


larInf(fit, sigma = estimateSigma(x,y)$sigmahat, type = "aic", ntimes = 1 )



### dados de treino e de teste #############################
# para reprodutividade 

set.seed(123)

#### treino ####
# Separando dados de treino 
tr <- round(0.5*nrow(x)) # 50% das linhas 
treino <- sample(1:nrow(x), tr, replace = F)
x.treino <- x[treino,]
y.treino <- y[treino]

#### Teste ####
# separando os dados de teste
x.teste <- x[-treino,]
y.teste <- y[-treino]

#### Regressão Lasso ###################################################

grid <- 10^seq(10,-2, length = 100)
l1 <- glmnet(x.treino,y.treino, alpha = 1, lambda = grid) # alpha igual a 1, faz regressão Lasso

# plotando os coeficientes vs lambda
plot(l1, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x.treino), cex = .4)

# previsão com lambda igual a 10
#l1.pred <- predict(l1, s = 10,newx = x.teste)
#MSE(y.teste,l1.pred[,1])


# previsão com lambda igual a 0 (MQO)
#l2.pred <- predict(l1, s = 0, newx = x.teste)
#MSE(y.teste,l2.pred[,1])

# previsão com lambda --> inf (similar a media de y)
#l3.pred <- predict(l1, s = 1e10, newx = x.teste)
#MSE(y.teste,l3.pred[,1])

#### Validação Cruzada #### O pacote usa a metodologia K-fold 

# modelo de regessão Lasso via validação cruzada 
set.seed(123)
l1.cv <- cv.glmnet(x.treino,y.treino, alpha = 1)
plot(l1.cv)
l1.cv


# obtendo o lambda ótimo
bestlamb <- l1.cv$lambda.min
bestlamb

# previsão dados de teste com lambda ótimo
lasso <- predict(l1.cv, s = bestlamb, newx = x.teste)
MSE(y.teste, lasso[,1])

# coeficientes ótimos de regressão Lasso

predict(l1.cv, type = "coefficients", s = bestlamb)


###### inferência após seleção de variáveis ######

# necessidade de re-escalar lambda no pacote selectiveInference em relação a glmnet
#necessidade de estimar sigma2

xsigma <- std(x.teste)



b <- coef(l1.cv, s = bestlamb/)[-1]
b
res <- fixedLassoInf(x,y,b,bestlamb)



fit <- lar(x,y, maxsteps = 20)
fit


larInf(fit, sigma = estimateSigma(x,y)$sigmahat, type = "aic", ntimes = 1 )


















### MSE #### ################ Média  dos quadrados dos erros  
MSE <- function(y,y_hat) {
  
    mse <- mean((y-y_hat)^2)
    return(data.frame(MSE = mse))
}



### RegressÃo Linear ####
m1 <- lm(Salary~., dados[treino,])
summary(m1)

m1.prev <- predict(m1, newdata = dados[-treino,-19])
MSE(y.teste,m1.prev) # média dos quadrados dos erros para os dados de teste

## observando a Inflação da Variância (VIF)
library(car)
vif(m1)

# resíduos de m1
par(mfrow = c(2,2))
plot(m1)
par(mfrow = c(1,1))
shapiro.test(residuals(m1)) # verifica não possui normalidade nos resíduos 

#### Regressão Rigde #########################################################

# sequência de lambdas a serem testados
grid <- 10^seq(10,-2, length = 100)
r1 <- glmnet(x.treino, ytreino, alpha = 0, lambda = grid) # com alpha = 0 o comando faz a regressão Rigde

# plotando os coeficientes vs lambda
plot(r1, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x.treino), cex = .4)

# previsão com lambda igual a 10
r1.pred <- predict(r1, s = 10,newx = x.teste)
MSE(y.teste,r1.pred[,1])


# previsão com lambda igual a 0 (MQO)
r2.pred <- predict(r1, s = 0, newx = x.teste)
MSE(y.teste,r2.pred[,1])

# previsão com lambda --> inf (similar a media de y)
r3.pred <- predict(r1, s = 1e10, newx = x.teste)
MSE(y.teste,r3.pred[,1])

#### Validação Cruzada #### O pacote usa a metodologia K-fold 

# modelo de regessão rígida via validação cruzada 
set.seed(1)
r1.cv <- cv.glmnet(x.treino,ytreino, alpha = 0)
plot(r1.cv)

# obtendo o lambda ótimo
blamb <- r1.cv$lambda.min
blamb

# previsão dados de teste com lambda ótimo
rigde <- predict(r1.cv, s = blamb, newx = x.teste)
MSE(y.teste, rigde[,1])

# coeficientes ótimos de regressão rigde
c <- glmnet(x,y, alpha = 0)
predict(r1.cv, type = "coefficients", s = blamb)




