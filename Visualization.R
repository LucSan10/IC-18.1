library(BioStatR)
library(corrplot)
library(fastDummies)
library(matrixStats)
library(readr)
library(lattice)
library(ggplot2)
library(colorRamps)
library(reshape2)

normSD <- function(M){
  offset <- sweep(M,2,colMeans(M))
  sd <- colSds(M)
  return (sweep(offset,2,sd,'/'))
}

normRange <- function(M){
  offset <- sweep(M,2,colMeans(M))
  range <- colMaxs(M)-colMins(M)
  return (sweep(offset,2,range,'/'))
}

########################################################################################################################

#### Telecommunications Service Customer Churn
Telco_Customer_Churn <- read_csv("telco-customer-churn/WA_Fn-UseC_-Telco-Customer-Churn.csv")

telecom <- dummy_cols(Telco_Customer_Churn[,2:ncol(Telco_Customer_Churn)], remove_most_frequent_dummy = TRUE)
telecom <- telecom[,-which(sapply(telecom, is.character))]
telecom <- telecom[-which(rowSums(is.na(telecom)) > 0),]

###### Matriz de Correlação
corrplot(cor(telecom), "color", tl.pos = "n")

###### Matriz de Distância
n <- dim(telecom)[1]
u <- runif(2000, 1, n)
telecomDist <- as.matrix(dist(telecom, upper = TRUE, diag = TRUE))
telecomGrid <- expand.grid(x=u,y=u)
telecomGrid$z <- as.vector(telecomDist[u,u])
levelplot(z~x*y, telecomGrid, col.regions = matlab.like2(100))


telecomMeanDist <- data.frame(t(rbind((1:n),colMeans(telecomDist))))
colnames(telecomMeanDist) <- c("x","y")
telecomMeanDist <- telecomMeanDist[order(telecomMeanDist$y),]
roundN <- round(0.72*n)
ggplot(data = telecomMeanDist)+
  geom_point(aes((1:n),y))+geom_line(aes(roundN,y))


nonOutlier <- sort(telecomMeanDist[(1:roundN),1])
nonOutlierSort <- telecomMeanDist[(1:roundN), 1]
NROW(telecomDist) <- as.matrix(dist(telecom[nonOutlierSort,], upper = TRUE, diag = TRUE))

u <- runif(2000, 1, roundN)

telecomGrid <- expand.grid(x=(1:2500),y=(1:2500))
telecomGrid$z <- as.vector(telecomDist[(1:2500),(1:2500)])
levelplot(z~x*y, telecomGrid, col.regions = matlab.like2(100))

telecomGrid <- expand.grid(x=((roundN-2500):roundN),y=(1:2500))
telecomGrid$z <- as.vector(telecomDist[((roundN-2500):roundN),(1:2500)])
levelplot(z~x*y, telecomGrid, col.regions = matlab.like2(100))

telecomGrid <- expand.grid(x=(1:2500),y=((roundN-2500):roundN))
telecomGrid$z <- as.vector(telecomDist[(1:2500),((roundN-2500):roundN)])
levelplot(z~x*y, telecomGrid, col.regions = matlab.like2(100))

telecomGrid <- expand.grid(x=((roundN-2500):roundN),y=((roundN-2500):roundN))
telecomGrid$z <- as.vector(telecomDist[((roundN-2500):roundN),((roundN-2500):roundN)])
levelplot(z~x*y, telecomGrid, col.regions = matlab.like2(100))

########################################################################################################################

#### Absenteeism at Work
Absenteeism_at_work <- read_delim("Absenteeism_at_work_AAA/Absenteeism_at_work.csv", 
                                  ";", escape_double = FALSE)
Absenteeism_at_work[,c(2:5,13)] <- sapply(Absenteeism_at_work[,c(2:5,13)], as.factor)
Absenteeism_at_work <- dummy_cols(Absenteeism_at_work[,2:ncol(Absenteeism_at_work)], remove_most_frequent_dummy = TRUE)
Absenteeism_at_work <- Absenteeism_at_work[,-which(sapply(Absenteeism_at_work, is.character))]

###### Matriz de Correlação
corrplot(cor(Absenteeism_at_work), "color", tl.cex = 0.4)

###### Matriz de Distância


########################################################################################################################

#### Parkinson's Disease -- Data 1
parkinson <- read_csv("Parkinson_Multiple_Sound_Recording_Data/train_data.txt", 
                       col_names = FALSE)[,-1]

###### Matriz de Correlação
corrplot(cor(parkinson), "color")

###### Matriz de Distância
n <- dim(parkinson)[1]
parkinsonDist <- as.matrix(dist(parkinson, upper = TRUE, diag = TRUE))
parkinsonGrid <- expand.grid(x=(1:n),y=(1:n))
parkinsonGrid$z <- as.vector(parkinsonDist)
levelplot(z~y*x, parkinsonGrid, col.regions = matlab.like2(100))



########################################################################################################################

#### Parkinson's Disease -- Data 2
parkinson_updrs <- read_csv("parkinsons/parkinsons_updrs.data")[,-1]
boxplot(parkinson_updrs[,3])
boxplot(parkinson_updrs[,c(1,4:5,18)])
boxplot(parkinson_updrs[,-c(1,3:5,18)])
sum(parkinson_updrs$sex)

###### Matriz de Correlação
corrplot(cor(parkinson_updrs), "color", tl.cex = 0.6)

###### Matriz de Distância
n <- dim(parkinson_updrs)[1]
u <- runif(1000, 1, n)
parkinson_updrsDist <- as.matrix(dist(parkinson_updrs, upper = TRUE, diag = TRUE))
parkinson_updrsGrid <- expand.grid(x=u,y=u)
parkinson_updrsGrid$z <- as.vector(parkinson_updrsDist[u,u])
levelplot(z~y*x, parkinson_updrsGrid, col.regions = matlab.like2(200))


parkinson_updrsMeanDist <- data.frame(t(rbind((1:n),colMeans(parkinson_updrsDist))))
colnames(parkinson_updrsMeanDist) <- c("x","y")
parkinson_updrsMeanDist <- parkinson_updrsMeanDist[order(parkinson_updrsMeanDist$y),]
ggplot(data = parkinson_updrsMeanDist)+
  geom_point(aes((1:n),y))+geom_line(aes(0.9*n,y))


nonOutliers <- sort(parkinson_updrsMeanDist[(1:(0.9*n)),1])
parkinson_updrsDist <- as.matrix(dist(parkinson_updrs[nonOutliers,], upper = TRUE, diag = TRUE))
u <- runif(1000, 1, 0.9*n)
parkinson_updrsGrid <- expand.grid(x=u,y=u)
parkinson_updrsGrid$z <- as.vector(parkinson_updrsDist[u,u])
levelplot(z~y*x, parkinson_updrsGrid, col.regions = matlab.like2(100))

normalized <- data.frame(normSD(as.matrix(parkinson_updrs[nonOutliers,])))
summary(normalized)

boxplot()

corrplot(cor(normalized), "color", tl.cex = 0.6)


########################################################################################################################

#### Parkinsons's Disease -- Data 3
parkinsons <- read_csv("parkinsons/parkinsons.data")[,-1]
  
###### Matriz de Correlação
corrplot(cor(parkinsons), "color", tl.cex = 0.6)

###### Matriz de Distância
n <- dim(parkinsons)[1]
parkinsonsDist <- as.vector(as.matrix(dist(parkinsons, upper = TRUE, diag = TRUE)))
parkinsonsGrid <- expand.grid(x=(1:n),y=(1:n))
parkinsonsGrid$z <- parkinsonsDist
levelplot(z~y*x, parkinsonsGrid, col.regions = matlab.like2(100))

########################################################################################################################

#### Autism Adult Data
Autism_Adult_Data <- read_csv("Autism-Adult-Data Plus Description File/csv_result-Autism-Adult-Data.csv", na = "?")
Autism_Adult_Data <- Autism_Adult_Data[,-20]
Autism_Adult_Data <- dummy_cols(Autism_Adult_Data[,2:ncol(Autism_Adult_Data)], remove_most_frequent_dummy =  TRUE)
Autism_Adult_Data <- Autism_Adult_Data[-which(rowSums(is.na(Autism_Adult_Data)) > 0),]
Autism_Adult_Data <- Autism_Adult_Data[,-which(sapply(Autism_Adult_Data, is.character))]
corrplot(cor(Autism_Adult_Data), "color", tl.pos = "n")

########################################################################################################################

#### WDBC
wdbc <- read_csv("Breast Cancer/wdbc.data", col_names = FALSE)
wdbc <- dummy_cols(wdbc[,2:ncol(wdbc)], remove_most_frequent_dummy = TRUE)
wdbc <- wdbc[,-which(sapply(wdbc, is.character))]

###### Matriz de Correlação
corrplot(cor(wdbc), "color")

###### Matriz de Distância
n <- dim(wdbc)[1]
wdbcDist <- as.vector(as.matrix(dist(wdbc)))
wdbcGrid <- expand.grid(x=(1:n),y=(1:n))
wdbcGrid$z <- wdbcDist
levelplot(z~y*x, wdbcGrid, col.regions = matlab.like2(100))

########################################################################################################################

#### WPBC
wpbc <- read_csv("Breast Cancer/wpbc.data", col_names = FALSE, na = "?")
wpbc <- dummy_cols(wpbc[,2:ncol(wpbc)], remove_most_frequent_dummy = TRUE)
wpbc <- wpbc[,-which(sapply(wpbc, is.character))]
wpbc <- na.omit(wpbc)

###### Matriz de Correlação
corrplot(cor(wpbc), "color", tl.cex = 0.6)

###### Matriz de Distância
n <- dim(wpbc)[1]
wpbcDist <- as.vector(as.matrix(dist(wpbc)))
wpbcGrid <- expand.grid(x=(1:n),y=(1:n))
wpbcGrid$z <- wpbcDist
levelplot(z~x*y, wpbcGrid, col.regions = matlab.like2(100))

########################################################################################################################

#### Drug Consumption
drug_consumption <- read_csv("drug_consumption.data", 
                             col_names = FALSE)
drugNum <- data.frame(drug_consumption[,which(sapply(drug_consumption, is.numeric))])
drugNotNum <- data.frame(drug_consumption[,-which(sapply(drug_consumption, is.numeric))])
corrplot(cor(drugNum), "color", tl.cex = 0.6)

plot(GKtauDataframe(drugNotNum), diagSize = 0.4)
