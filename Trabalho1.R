
## Bibliotecas Usadas

library(matrixStats)    ## Funções para Matrizes
library(corrplot)       ## Matriz de Correlação
library(fastDummies)    ## Criação de Variáveis Binárias
library(ggplot2)        ## Plotagens Gerais

## Normalização por Desvio Padrão

normalizationSD <- function(M){
  offset <- sweep(M,2,colMeans2(M))
  sd <- colSds(M)
  return (sweep(offset,2,sd,'/'))
}

## Normalização por Faixa de Valores

normalizationRange <- function(M){
  offset <- sweep(M,2,colMeans2(M))
  range <- colMaxs(M)-colMins(M)
  return (sweep(offset,2,range,'/'))
}

## Plotagem de Variância Mantida X (N° de Variáveis)

plotSVDVariance <- function(S,K){
  plotX <- 1:K
  plotY <- cumsum(S)/sum(S)
  ggplot()+geom_line(aes(plotX,plotY))
  return(NROW(S)-sum(plotY)+1)
}

## Redução de Dimensionalidade por PCA

dimReduction <- function(M,U,K){
  svdM <- M%*%U[,1:K]
  corrplot(cor(svdM), "color", tl.cex = 0.001)
  return(svdM)
}

## Remoção de Variáveis com Valores Ausentes

removeNA <- function(d){
  res <- d
  NANames <- names(res)[colSums(is.na(res)) > 0]
  if (length(NANames) > 0){
    res <- res[,-which(names(res) %in% NANames)]  
  }
  return(res)
}

## Gerador de Variáveis Binárias

dummyGenerator <- function(d){
  res <- d
  if (is.data.frame(res)){
    dummyNames <- names(res)[sapply(res, is.factor)]
    if (length(dummyNames) > 0){
      dataWithDummy <- dummy_cols(res, select_columns = dummyNames)
      res <- dataWithDummy[,-which(names(dataWithDummy) %in% dummyNames)]  
    }  
  }
  if (is.factor(res)){
    res <- model.matrix(~res-1)
  }
  return(res)
}

g <- function(z){
  return(1/(1+exp(-z)))  
}

train <- read.csv("all/train.csv")
test <- read.csv("all/test.csv")
Y <- train[,ncol(train)]
train = train[,-ncol(train)]

data <- rbind(train, test)
Y <- matrix(Y)

summary(data)
str(data)

## Removendo Variáveis com muitos valores ausentes

removeData <- colSums(is.na(data))/nrow(data) >= 0.2
pairs(cbind(data[1:nrow(Y),removeData], Y))
data <- data[,-which(removeData)]

shouldBeFactor <- c("MSSubClass", "OverallQual", "OverallCond")

data <- dummyGenerator(data)
data <- removeNA(data)
Y <- dummyGenerator(Y)

col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                           "cyan", "#007FFF", "blue", "#00007F"))

corrplot(cor(data), "color", tl.cex = 0.06, col = heat.colors(100)[100:-1:1])

M <- data.matrix(data)
M <- normalizationSD(M)
corrplot(cor(M), "color", tl.cex = 0.0011, col = heat.colors(100)[100:-1:1])

sigma = (t(M)%*%M)/(nrow(M))
SVD = svd(sigma)

nVar <- plotSVDVariance(SVD$d, ncol(sigma))
svdM <- dimReduction(M, SVD$u, nVar)

theta <- matrix(1, ncol(M)+1,ncol(Y))
X <- cbind(matrix(1,nrow(M),1), M)
alpha <- 0.05
Y <- log(1+Y)


logisticReg <- function(X, theta, alpha, K, Y){
  for (i in 1:K){
    h <- g(X%*%theta)
    theta <- theta - (alpha/nrow(X))*(t(X)%*%(h-Y))
  }
  return(theta)
}

computeCost <- function(theta,X,Y){
  return((1/(2*nrow(X)))*sum(((X%*%theta) - Y)^2))
}

linearReg <- function(X, theta, alpha, K, Y){
  J = vector(length = K)
  for (i in 1:K){
    h <- X%*%theta
    theta <- theta - (alpha/nrow(X))*(t(X)%*%(h-Y))
    J[i] <- computeCost(theta,X,Y)
  }
  res <- list(J,theta)
  names(res) <- c("J", "theta")
  return(res)
}

lr <- linearReg(X, theta, alpha, 1500, Y)

ggplot()+geom_smooth(aes(1:NROW(lr$J), lr$J))+
  scale_y_log10()

colMeans(Y)
colSds(Y)
qqnorm(Y); qqline(Y)
ggplot(data = data.frame(Y))+geom_histogram(aes(Y),binwidth = 0.1)

test <- read.csv("all/test.csv")
summary(test)
str(test)
dummyNames <- names(data)[sapply(data, is.factor)]

names(data)[!(names(data) %in% c(names(test)))]

str(test) <- dummyGenerator(test)
test <- removeNA(test)

hypotheses <- test%*%theta
