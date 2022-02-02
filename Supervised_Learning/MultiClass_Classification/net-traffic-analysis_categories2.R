
#Cargamos las librerías que vamos a utilizar
#install.packages("ROSE")
#install.packages(c( "foreach", "iterators", "doParallel") )
#install.packages("chron")
#install.packages("randomForest")
#install.packages("ISLR")
#install.packages("InformationValue") 
library(InformationValue)
library(ISLR)
library(randomForest)
library(chron)
library(ROSE)
library(data.table)
library(plyr)
library(dplyr)
library(caret)
library(scales)
library(ggplot2)
library(plotly)
library(stringi)
library(stringr)
library(dataPreparation)
library(knitr)
library(kableExtra)
library(ggpubr)
library(tictoc)
library(ggeasy)
library(lubridate)
library(inspectdf)
library(questionr)
library(psych)
library(car)
library(Hmisc)
library(readxl)
library(mice)
library(xgboost)
library(GGally)
library(MASS)
library(dummies)
library(reshape)
library(pROC)
library(sas7bdat)
library(nnet)
library(parallel)
library(doParallel)
library(caret)
library(caretEnsemble)
########################################################################

#Limpieza y transformación del dataset Sem 1 y 2 CIDDS

#########################################################################

#Confirguramos el directorio de trabajo
setwd("C:/Users/A663876/Desktop/UCM Máster/8. Machine learning con R/CIDDS/Modelo Categorias/Categorias-muestra3")

# Data Loading
prot<- fread(file = 'protocol-numbers1.csv', stringsAsFactors = TRUE)
flags<-fread(file = 'TCPFlags1.csv', stringsAsFactors = TRUE)
load("sem1_1.Rda")
datIn<-sem1_1

# Depuracion del dataset
datIn$class <- NULL #no es util porque es una clasificación binaria
datIn$attackID <- NULL #no es util porque es una clasificación binaria
datIn$attackDescription <- NULL #no es util porque es una clasificación binaria
datIn$Flows <- NULL #todas las observaciones son iguales
datIn$`Src IP Addr` <-NULL #de momento no lo tomaremos en cuenta por ser una categoría con mucha cardinalidad 
datIn$`Dst IP Addr` <- NULL

# Renombramos algunas variables 
names(datIn)[1] <- "Date"
names(datIn)[4] <- "SrcPt"
names(datIn)[5] <- "DstPt"
names(datIn)[10] <- "class"


datIn$class<-car::recode(datIn$class, "c('---')='normal'")

#Transformamos Bytes con valores e.g., "8.3 M" a numérica 
datIn$Bytes <- gsub("M", "000000", datIn$Bytes)
datIn$Bytes <- gsub("[[:blank:]]", "", datIn$Bytes)
datIn$Bytes <- gsub("\\.", "", datIn$Bytes)
datIn$Bytes <- as.numeric(datIn$Bytes)

#Revisamos si tenemos NAs
sapply(datIn, function(x) sum(is.na(x)))
freq(datIn$class)

#Feature engineering
datIn$Packets_speed <- ifelse(datIn$Duration >0.000, datIn$Packets/datIn$Duration, datIn$Packets/0.0001)
datIn$Bytes_speed <- ifelse(datIn$Duration >0.000, datIn$Bytes/datIn$Duration, datIn$Bytes/0.0001)

#Convertimos factor en números
library('lubridate')
datIn$Hour<- hour(datIn$Date)
datIn$Hour<-format(as.POSIXct(datIn$Date), format="%H")
datIn$Hour <- as.numeric(datIn$Hour)
datIn$Minute<- minute(datIn$Date)
datIn$Minute<-format(as.POSIXct(datIn$Date), format="%M")
datIn$Minute <- as.numeric(datIn$Minute)
datIn$Second<- second(datIn$Date)
datIn$Second<-format(as.POSIXct(datIn$Date), format="%S")
datIn$Second <- as.numeric(datIn$Second)
#datIn$Year<- year(datIn$Date)
#datIn$Year<-format(as.POSIXct(datIn$Date), format="%Y")
#datIn$Year <- as.numeric(datIn$Year)
#datIn$Day<- day(datIn$Date)
datIn$Day<-format(as.POSIXct(datIn$Date), format="%d")
datIn$Day <- as.numeric(datIn$Day)

#eliminamos la columna Date que ya fue transformada
datIn$Date<-NULL

#Convertimos varibles Proto y Flags en numéricas
datIn$Proto<- with(datIn, prot$Decimal[match(Proto, prot$Keyword)])
datIn$Flags<- with(datIn, flags$Num[match(Flags, flags$TCPFlag)])
head(datIn,5)
summary(datIn)

#Estandarizamos el dataset
datIn2<-datIn
class<-datIn2[,"class"]
#listclass <-c("Proto", "Flags")
means_cat <- readRDS("./means_cat.rds")
sds_cat <- readRDS("./sds_cat.rds")
datIn3<-scale(datIn2[,c("Duration", "Proto", "SrcPt", "DstPt", "Packets", "Bytes", 
                        "Flags", "Tos", "Packets_speed", "Bytes_speed", "Hour", 
                        "Minute", "Second", "Day")], center = means_cat, scale = sds_cat) #escalamos cada vector

test1_1categ<-data.frame(cbind(datIn3, class)) #dataframe ya transformado, con variables estandarizadas, media cero y desviación típica 1
save(test1_1categ,file="test1_1categ.Rda")


#concatenamos los datasets semana 1
load("test1_1categ.Rda")
load("test1_2categ.Rda")
load("test1_3categ.Rda")
load("test1_4categ.Rda")
load("test1_5categ.Rda")
load("test1_6categ.Rda")
test_sem1<-data.frame(rbind(test1_1categ,test1_2categ,test1_3categ,test1_4categ,test1_5categ,test1_6categ))
save(test_sem1,file="test_sem1.Rda")

#concatenamos los datasets semana 2
load("test2_1categ.Rda")
load("test2_2categ.Rda")
load("test2_3categ.Rda")
load("test2_4categ.Rda")
load("test2_5categ.Rda")
load("test2_6categ.Rda")
load("test2_7categ.Rda")
test_sem2<-data.frame(rbind(test2_1categ,test2_2categ,test2_3categ,test2_4categ,test2_5categ,test2_6categ,test2_7categ))
save(test_sem2,file="test_sem2.Rda")

####################################################################################
#Creamos muestra para tuneado de modelos
setwd("C:/Users/A663876/Desktop/UCM Máster/8. Machine learning con R/CIDDS/Modelo Categorias")
library(dplyr)
library(tidyverse)

load("test_sem1.Rda") #de la semana 1 separamos cada categoria de la varibale dependiente
test_sem1<-as_tibble(test_sem1)
test_sem1

#categoría 1
bruteForce<-test_sem1 %>% filter(class == "bruteForce")
save(bruteForce,file="bruteForce.Rda")
load("bruteForce.Rda")
df1<-bruteForce %>% group_by(class) %>% sample_frac(0.2)
df1<-as.data.frame(df1)

#categoría 2
pingScan<-test_sem1 %>% filter(class == "pingScan")
save(pingScan,file="pingScan.Rda")
load("pingScan.Rda")
df2<-pingScan %>% group_by(class) %>% sample_frac(0.1)
df2<-as.data.frame(df2)

#categoría 3
portScan<-test_sem1 %>% filter(class == "portScan")
save(portScan,file="portScan.Rda")
load("portScan.Rda")
df3<-portScan %>% group_by(class) %>% sample_frac(0.004)
df3<-as.data.frame(df3)

#categoría 4
dos<-test_sem1 %>% filter(class == "dos")
save(dos,file="dos.Rda")
load("dos.Rda")
df4<-dos %>% group_by(class) %>% sample_frac(0.001)
df4<-as.data.frame(df4)

#categoría 5
normal<-test_sem1 %>% filter(class == "normal")
save(normal,file="normal.Rda")
load("normal.Rda")
df5<-normal %>% group_by(class) %>% sample_frac(0.0015)
df5<-as.data.frame(df5)

#Creamos una muestra con cada categoria, guardando un % similar al del dataset completo
muestra3<-rbind(df1,df2,df3,df4,df5)
freq(muestra3$class)
save(muestra3,file="muestra3.Rda")
#---------------------------------------------
###############################################################################

#Tuneado y Entrenamiento 

###############################################################################

setwd("C:/Users/A663876/Desktop/UCM Máster/8. Machine learning con R/CIDDS/Modelo Categorias/Categorias-muestra3")
load("muestra3.Rda")
freq(muestra3$class)
write.csv(muestra3, "C:\\Users\\A663876\\Desktop\\UCM Máster\\8. Machine learning con R\\CIDDS\\Modelo Categorias\\Categorias2\\muestra3.csv", row.names = TRUE)

datIn_bien_cat<-as.data.frame(muestra3)
sapply(datIn_bien_cat, function(x) sum(is.na(x)))

# Preparamos el dataset para el entrenamiento
class<-datIn_bien_cat[,"class"]
class<-as.data.frame(class)
class$class<-as.numeric(class$class)
dput(names(datIn_bien_cat))
listconti<-c("Duration", "Proto", "SrcPt", "DstPt", "Packets", "Bytes", 
             "Flags", "Tos", "Packets_speed", "Bytes_speed", "Hour", "Minute", 
             "Second", "Day")
vardep<-c("class")
data<-as.data.frame(datIn_bien_cat)
save(data,file="data.Rda")
load("data.Rda")


###############################################################################
# ***************************
# RED NEURONAL
# ***************************
###############################################################################

set.seed(123456)
control<-trainControl(method = "repeatedcv",number=4,repeats=5,savePredictions = "all",classProbs=TRUE) 
nnetgrid <-  expand.grid(size=c(5),decay=c(0.1),bag=FALSE)

red1<- train(class~.,data=data,
             method="avNNet",linout = FALSE,maxit=100,repeats=5,
             trControl=control,tuneGrid=nnetgrid)
saveRDS(red1, "./model_red1.rds")
#red1 <- readRDS("./model_red1.rds")
summary(red1)
# Accuracy   Kappa    
# 0.9876584  0.9493452
sal<-red1$pred

# La función confusionMatrix de caret calcula la matriz de confusión
salconfu<-confusionMatrix(sal$pred,sal$obs)
salconfu


# *********************************
# CRUZADA avNNet
#  ************************************

# TUNEANDO LA RED CON LOS  MODELOS CANDIDATOS USANDO CV


set.seed(12346)
# Validación cruzada repetida
control<-trainControl(method = "repeatedcv",number=4,repeats=5,savePredictions = "all",classProbs=TRUE) 

# ***************************************************************
# nnet: parámetros
#     Number of Hidden Units (size, numeric) --> numero de nodos de la red
#     Weight Decay (decay, numeric) --> dropout
#     Maxit --> indica el numero maximo de interacciones del proceso de estimacion-optimizacion
# ***************************************************************

nnetgrid <-  expand.grid(size=c(5,10,15,20),decay=c(0.01,0.1,0.001))

rednnet<- train(class~.,data=data,
                method="nnet",linout = FALSE,maxit=100,trControl=control,tuneGrid=nnetgrid)

rednnet #20    0.010  0.9873737  0.9639054
saveRDS(rednnet, "./model_rednnet.rds")
#rednnet <- readRDS("./model_rednnet.rds")

# ***************************************************************
# avNNet: parámetros
# Number of Hidden Units (size, numeric)
# Weight Decay (decay, numeric) --> regularization (e.g., L2, dropout)
# Bagging (bag, logical)--> bag=FALSE indica que queremos hacer subsampling en cada constiuccion de red
# ***************************************************************
avnnetgrid <-expand.grid(size=c(5,10,15,20),decay=c(0.01,0.1,0.001),bag=FALSE)

redavnnet<- train(class~Tos + Flags + Duration + Bytes_speed + SrcPt + Packets + Hour 
                  + Packets_speed,data=data,
                  method="avNNet",linout = FALSE,maxit=100,trControl=control,repeats=5,tuneGrid=avnnetgrid)

redavnnet #size = 20, decay = 0.1 and bag = FALSE
# Accuracy= 0.9911737 Kappa= 0.9640179
saveRDS(redavnnet, "./model_redavnnet.rds")
#redavnnet <- readRDS("./model_redavnnet.rds")



#Creamos modelo 1: model_avnnet fijando size = 20, maxit=100 y decay= 0.01 
control<-trainControl(method = "repeatedcv",number=4,repeats=5,savePredictions = "all",classProbs=TRUE) 

set.seed(123)
nnetgrid <- expand.grid(size=c(20),decay=c(0.01),bag=F)
model_nnet1 <- train(class~.,data=data,
                      method="avNNet",linout = FALSE,maxit=100,trControl=control,tuneGrid=nnetgrid)
model_nnet1
saveRDS(model_nnet1, "./nnet_cat.rds")
# Accuracy  Kappa   
# 0.926353  0.747929
#model_nnet1 <- readRDS("./model_nnet1.rds")

# La funcion confusionMatrix de caret calcula la matriz de confusion
summary(model_nnet1)
sal<-model_nnet1$pred
salconfu<-confusionMatrix(sal$pred,sal$obs)
salconfu

#Modelo Ganador

set.seed(123)
nnetgrid <- expand.grid(size=c(20),decay=c(0.01),bag=F)
model_nnet2 <- train(class~.,data=data,
                     method="avNNet",linout = FALSE,maxit=100,trControl=control,tuneGrid=nnetgrid)
model_nnet2
saveRDS(model_nnet2, "./model_nnet2.rds")
# Accuracy   Kappa    
#
#model_nnet2 <- readRDS("./model_nnet2.rds")

# La funcion confusionMatrix de caret calcula la matriz de confusion
summary(model_nnet2)
sal<-model_nnet2$pred
salconfu<-confusionMatrix(sal$pred,sal$obs)
salconfu

###############################################################################
# ***************************
# RANDOM FOREST
# ***************************
###############################################################################

set.seed(12345)
rfgrid<-expand.grid(mtry=c(3,4,5,6,7,8,9,10,11,12,13,14)) #se prueban con diferentes números de 3 a 14 puesto que tenemos 14 variables input posibles

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) #callsprobs= true se usa cuando la variable dependiente es binaria

rf<- train(factor(class)~.,data=data,#se usa como factor la variable dependiente
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = FALSE,ntree=450,nodesize=10,replace=TRUE, #linout=false indica que la variable dependiente es categórica
           importance=TRUE)#raplace=true indica que se va a utilizar el algoritmo clásico con reemplazamiento
#importance=true conserva las estadísticas de importancia de cada variable

rf# mtry= 4

# IMPORTANCIA DE VARIABLES
install.packages("randomForest")
library(randomForest)
final<-rf_cat$finalModel

tabla<-as.data.frame(importance(final))
tabla<-tabla[order(-tabla$MeanDecreaseAccuracy),]
tabla
par(cex.axis=0.8, las=2)
barplot(tabla$MeanDecreaseAccuracy,names.arg=rownames(tabla))

rfbis<-randomForest(factor(class)~.,
                    data=data,
                    mtry=4,ntree=450,nodesize=10,replace=TRUE)

plot(rfbis$err.rate[,1])#seleccionar ntree=300



#creamos modelo rf
library(randomForest)
set.seed(12345)
#class<-datIn_bien_cat[,"class"]
rfgrid<-expand.grid(mtry=c(6))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

rf_cat<- train(factor(class)~., data=data,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = FALSE,ntree=300,nodesize=10,replace=TRUE,
           importance=TRUE)
saveRDS(rf_cat, "./rf_cat.rds")

plot(varImp(rf_cat), main = "RandomForest Importancia de Variables")
#rf_cat <- readRDS("./rf_cat.rds")

rf_cat
final<-rf_cat$finalModel

tabla<-as.data.frame(importance(final))
tabla<-tabla[order(-tabla$MeanDecreaseAccuracy),]
tabla
par(cex.axis=0.8, las=2)
barplot(tabla$MeanDecreaseAccuracy,names.arg=rownames(tabla),main = "Random Forest Importancia de Variables")


###############################################################################
# ***************************
# GRADIENT BOOSTING 
# ***************************
###############################################################################

set.seed(12345)

gbmgrid<-expand.grid(shrinkage=c(0.1),
                     n.minobsinnode=c(20),
                     n.trees=c(100,200,300,500),
                     interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

gbm_cat<- train(factor(class)~.,data=data,
                method="gbm",trControl=control,tuneGrid=gbmgrid,distribution="multinomial", bag.fraction=0.5, verbose=TRUE)
saveRDS(gbm_cat, "./gbm_cat.rds")
gbm_cat # 'n.trees'= 1000, 'interaction.depth'= 2, 'shrinkage'=0.1, 'n.minobsinnode'=20
#gbm_cat <- readRDS("./gbm_cat.rds")
plot(gbm_cat)

# IMPORTANCIA DE VARIABLES
par(cex=0.8, las=2)
summary(gbm_cat2)#muestra que Tos y Hour son muy poco predictoras, probamos quitandolas
tabla<-summary(gbm_cat2)
par(cex=1.2,las=2)
barplot(tabla$rel.inf,names.arg=row.names(tabla))

plot(varImp(gbm_cat2), main = "RandomForest Importancia de Variables")

#creamos el modelo ganador con GBM

set.seed(12345)

gbmgrid<-expand.grid(shrinkage=c(0.1),
                     n.minobsinnode=c(20),
                     n.trees=c(500),
                     interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

gbm_cat2<- train(factor(class)~.,data=data,
            method="gbm",trControl=control,tuneGrid=gbmgrid,distribution="multinomial", bag.fraction=0.5, verbose=TRUE)
saveRDS(gbm_cat2, "./gbm_cat2.rds")
gbm_cat2 # 'n.trees'= 1000, 'interaction.depth'= 2, 'shrinkage'=0.1, 'n.minobsinnode'=20
gbm_cat2 <- readRDS("./gbm_cat2.rds")

# IMPORTANCIA DE VARIABLES
par(cex=0.8, las=2)
summary(gbm_cat2)#muestra que Tos y Hour son muy poco predictoras, probamos quitandolas
tabla<-summary(gbm_cat2)
par(cex=1.2,las=2)
barplot(tabla$rel.inf,names.arg=row.names(tabla),main = "Gradient Boosting Importancia de Variables")


################################################################################
# ***************************
# XGBOOST 
# ***************************
################################################################################
xgbmgrid<-expand.grid(min_child_weight=c(5), eta=c(0.1), nrounds=c(1000),
                      max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

xgbm_cat<- train(factor(class)~.,data=data,
                 method="xgbTree",trControl=control,
                 tuneGrid=xgbmgrid, verbose=FALSE)

saveRDS(xgbm_cat, "./xgbm_cat.rds")
#xgbm <- readRDS("./xgbm_cat.rds")
xgbm_cat

# IMPORTANCIA DE VARIABLES

varImp(xgbm_cat)
plot(varImp(xgbm_cat))
#eliminamos Hour, Minute y Second porque no aportan casi nada al modelo

#Tuneamos el algoritmo

set.seed(12345)
xgbmgrid<-expand.grid(
  min_child_weight=c(5,10,20),
  eta=c(0.1,0.01,0.001),
  nrounds=c(100,500,1000),
  max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

xgbm_cat1<- train(factor(class)~.,data=data,
             method="xgbTree",trControl=control,
             tuneGrid=xgbmgrid, verbose=FALSE)
saveRDS(xgbm_cat1, "./xgbm_cat1.rds")
xgbm_cat1 # The final values used for the model were nrounds = 1000, max_depth = 6, eta = 0.1, gamma = 0, colsample_bytree = 1, min_child_weight = 5 and subsample = 1.
plot(xgbm_cat1)


# ESTUDIO DE EARLY STOPPING
# Probamos a fijar algunos parámetros para ver como evoluciona
# en función de las iteraciones. Usamos eta=0.1, min_child_weight=5 ya que ofrecen resultados interesantes 

xgbmgrid<-expand.grid(eta=c(0.1),
                      min_child_weight=c(5),
                      nrounds=c(100,500,700,1000,2000),
                      max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

set.seed(12345)
control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

xgbm_cat2<- train(factor(class)~.,data=data,
             method="xgbTree",trControl=control,
             tuneGrid=xgbmgrid,verbose=FALSE)
saveRDS(xgbm_cat2, "./xgbm_cat2.rds")
plot(xgbm_cat2) # a partir de 1000 se vuelve constante

# Probamos con otras semillas para la validacion cruzada

set.seed(12367)
control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

xgbm_cat3<- train(factor(class)~.,data=data,
             method="xgbTree",trControl=control,
             tuneGrid=xgbmgrid,verbose=FALSE)
saveRDS(xgbm_cat3, "./xgbm_cat3.rds")
plot(xgbm_cat3)


# Creamos modelo ganador (sin la variable Second)
xgbmgrid<-expand.grid(min_child_weight=c(5), eta=c(0.1), nrounds=c(1000),
  max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

xgbm_cat4<- train(factor(class)~DstPt + Flags + Bytes + Packets + Duration + SrcPt + Packets_speed + 
                   Proto + Tos + Bytes_speed + Minute + Hour +  Day, data=data,
                 method="xgbTree",trControl=control,
                 tuneGrid=xgbmgrid, verbose=FALSE)

saveRDS(xgbm_cat4, "./xgbm_cat4.rds")
#xgbm_cat4 <- readRDS("./xgbm_cat4.rds")
xgbm_cat4




################################################################################
# ***************************
#   SVM  
# ***************************
################################################################################

set.seed(12345)
SVMgrid<-expand.grid(C=c(0.01,0.1,0.5,1,2,5,10,100))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

SVM_cat<- train(data=data,factor(class)~.,method="svmLinear",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM_cat$results #mejor resultado con C=10
plot(SVM_cat$results$C,SVM_cat$results$Accuracy)

# Rehago el grid para observar mejor el intervalo de C 
set.seed(12345)
SVMgrid<-expand.grid(C=c(100,200,500,1000))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

SVM_cat2<- train(data=data,factor(class)~.,method="svmLinear",trControl=control,
                 tuneGrid=SVMgrid,verbose=FALSE)

SVM_cat2$results#mejor resultado con C=500
plot(SVM_cat2$results$C,SVM_cat2$results$Accuracy)


#creamos modelo svm
set.seed(12345)
SVMgrid<-expand.grid(C=c(500))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

SVM_cat3<- train(data=data,factor(class)~., method="svmLinear",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)
SVM_cat3$results
saveRDS(SVM_cat3, "./svm_cat3.rds")



################################################################################
# ***************************
#  K-nearest Neighbors KNN 
# ***************************
################################################################################


#Install class package
install.packages('class')
# Load class package
library(class)

control<-trainControl(method = "cv",number=4,savePredictions = "all", classProbs=TRUE) 
KNNgrid<-expand.grid(k = c(1,3,5,11,21,35,55,85,102))
model_knn<- train(data=data,factor(class)~., method="knn",trControl=control,
            tuneGrid=KNNgrid)
#Mejor k= 1 con 0.9950003 de accuracy
model_knn

#creamos modelo KNN
set.seed(12345)
KNNgrid<-expand.grid(k=c(1))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

KNN_cat<- train(data=data,factor(class)~., method="knn",trControl=control,
                  tuneGrid=KNNgrid)
saveRDS(KNN_cat, "./knn_cat.rds")




################################################################################
# ***************************
# Naive Bayes NB 
# ***************************
################################################################################

library(e1071)

control<-trainControl(method = "cv",number=4,savePredictions = "all", classProbs=TRUE) 
model_NB1<- train(data=data,factor(class)~., method="nb",trControl=control)
model_NB1$results
# usekernel fL adjust  Accuracy     Kappa  AccuracySD    KappaSD
# 1     FALSE  0      1       NaN       NaN          NA         NA
# 2      TRUE  0      1 0.9895336 0.9598675 0.009940805 0.03637183

#Plot Variable performance
X <- varImp(NB_cat)
plot(X)

set.seed(12345)
control<-trainControl(method = "cv",number=4,savePredictions = "all") 
NBgrid<-expand.grid(usekernel = TRUE, fL = c(0), adjust =c(1))
NB_cat<- train(data=data,factor(class)~., method="nb",trControl=control,
                 tuneGrid=NBgrid)
saveRDS(NB_cat, "./NB_cat.rds")
NB_cat <- readRDS("./NB_cat.rds")



################################################################################
# ***************************
# Evaluamos los modelos creados con el dataset entero
# ***************************
################################################################################

setwd("C:/Users/A663876/Desktop/UCM Máster/8. Machine learning con R/CIDDS/Modelo Categorias/Categorias-muestra3")
load("test1_1categ.Rda")

datIn3<-test1_1categ
test1_1categ<-NULL
nnet_cat <- readRDS("./nnet_cat.rds")
start_time <- Sys.time()
pred_avnnet <- predict(nnet_cat, datIn3)
end_time <- Sys.time()
time<- end_time - start_time
time
nuevos_datos <-cbind(datIn3$class,pred_avnnet)
nuevos_datos<-as.data.frame(nuevos_datos)
names(nuevos_datos)[1] <- "class"
mc1<- table(nuevos_datos$class, nuevos_datos$pred_avnnet)
mc1




rf_cat<-readRDS("./rf_cat.rds")
start_time <- Sys.time()
pred_rf <- predict(rf_cat, datIn3)
end_time <- Sys.time()
time<- end_time - start_time
time
nuevos_datos <-cbind(nuevos_datos,pred_rf)
mc2<- table(nuevos_datos$class, nuevos_datos$pred_rf)
mc2



gbm_cat2<-readRDS("./gbm_cat2.rds")
start_time <- Sys.time()
pred_gbm <- predict(gbm_cat2, datIn3)
end_time <- Sys.time()
time<- end_time - start_time
time
nuevos_datos <-cbind(nuevos_datos,pred_gbm)
mc3 <- table(nuevos_datos$class, nuevos_datos$pred_gbm)
mc3



xgbm_cat4<-readRDS("./xgbm_cat4.rds")
start_time <- Sys.time()
pred_xgbm <- predict(xgbm_cat4, datIn3)
end_time <- Sys.time()
time<- end_time - start_time
time
nuevos_datos <-cbind(nuevos_datos,pred_xgbm)
mc4 <- table(nuevos_datos$class, nuevos_datos$pred_xgbm)
mc4


SVM_cat3<-readRDS("./svm_cat3.rds")
start_time <- Sys.time()
pred_svm <- predict(SVM_cat3, datIn3)
end_time <- Sys.time()
time<- end_time - start_time
time
nuevos_datos <-cbind(nuevos_datos,pred_svm)
mc5 <- table(nuevos_datos$class, nuevos_datos$pred_svm)
mc5


KNN_cat<-readRDS("./KNN_cat.rds")
start_time <- Sys.time()
pred_knn <- predict(KNN_cat, datIn3)
end_time <- Sys.time()
time<- end_time - start_time
time
nuevos_datos <-cbind(nuevos_datos,pred_knn)
mc6 <- table(nuevos_datos$class, nuevos_datos$pred_knn)
mc6


#Tarda demasiado, en 4 horas no logra hacer la predicción, descartamos este modelo para esta fase
# NB_cat<-readRDS("./NB_cat.rds")
# start_time <- Sys.time()
# pred_nb <- predict(NB_cat, datIn3)
# end_time <- Sys.time()
# time<- end_time - start_time
# time
# nuevos_datos <-cbind(nuevos_datos,pred_nb)
# mc7 <- table(nuevos_datos$class, nuevos_datos$pred_nb)
# mc7


################################################################################
# ***************************
#Mejor Modelo: ensamblado (XGBM,GBM,RF,NNET, SVM)
# ***************************
################################################################################

xgbm_cat <- readRDS("./xgbm_cat4.rds")
save(xgbm_cat,   file = "xgbm_cat.RData")
nnet_cat <- readRDS("./nnet_cat.rds")
save(nnet_cat,   file = "nnet_cat.RData")
rf_cat<-readRDS("./rf_cat.rds")
save(rf_cat,   file = "rf_cat.RData")
gbm_cat<-readRDS("./gbm_cat2.rds")
save(gbm_cat,   file = "gbm_cat.RData")
SVM_cat<-readRDS("./svm_cat3.rds")
save(SVM_cat,   file = "SVM_cat.RData")


# Crea  funcion my_mode (devuelve los valores más repetitivos)
my_mode <- function(x) {                      
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}
pred_ensemb <- apply(nuevos_datos,1,my_mode)
nuevos_datos$pred_ensemb <- pred_ensemb
nuevos_datos$class <- datIn3$class
mc<- table(nuevos_datos$class, nuevos_datos$pred_ensemb)
mc


#Función Mode, devuelve el valor más repetitivo. Usaremos esta para las evaluaciones
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

pred_ens <- apply(nuevos_datos,1,Mode) #aplicamos la función al dataset de test
nuevos_datos$pred_ens <- pred_ens #agregamos una columna con las predicciones del ensamblado
nuevos_datos$class <- datIn3$class #agregamos una columna con los valores reales de cada observación
mc<- table(nuevos_datos$class, nuevos_datos$pred_ens) #calculamos la matriz de confusión
mc #mostramos resultados


#ensemble1: xgbm,gbm,rf
nuevos_datos1<- cbind(pred_xgbm,pred_gbm, pred_rf)
nuevos_datos1<-as.data.frame(nuevos_datos1)
pred_ens1 <- apply(nuevos_datos1,1,Mode)
nuevos_datos1$pred_ens1 <- pred_ens1
nuevos_datos1$class <- datIn3$class
mc_ens1<- table(nuevos_datos1$class, nuevos_datos1$pred_ens1)
mc_ens1


#ensemble2: xgbm,gbm,rf,redes
nuevos_datos2<- cbind(pred_xgbm,pred_gbm, pred_rf, pred_avnnet)
nuevos_datos2<-as.data.frame(nuevos_datos2)
pred_ens2 <- apply(nuevos_datos2,1,Mode)
nuevos_datos2$pred_ens2 <- pred_ens2
nuevos_datos2$class <- datIn3$class
mc_ens2<- table(nuevos_datos2$class, nuevos_datos2$pred_ens2)
mc_ens2

#ensemble3: xgbm,gbm,rf,redes,svm
nuevos_datos3<- cbind(pred_xgbm,pred_gbm, pred_rf, pred_avnnet, pred_svm)
nuevos_datos3<-as.data.frame(nuevos_datos3)
pred_ens3 <- apply(nuevos_datos3,1,Mode)
nuevos_datos3$pred_ens3 <- pred_ens3
nuevos_datos3$class <- datIn3$class
mc_ens3<- table(nuevos_datos3$class, nuevos_datos3$pred_ens3)
mc_ens3

#ensemble4: xgbm,gbm,rf,redes,svm,knn
nuevos_datos4<- cbind(pred_xgbm,pred_gbm, pred_rf, pred_avnnet, pred_svm,pred_knn)
nuevos_datos4<-as.data.frame(nuevos_datos4)
pred_ens4 <- apply(nuevos_datos4,1,Mode)
nuevos_datos4$pred_ens4 <- pred_ens4
nuevos_datos4$class <- datIn3$class
save(nuevos_datos,file="nuevos_datos.Rda")
mc_ens4<- table(nuevos_datos4$class, nuevos_datos4$pred_ens4)
mc_ens4

#ensemble5: xgbm,gbm,rf,knn
nuevos_datos5<- cbind(pred_xgbm,pred_gbm, pred_rf, pred_knn)
nuevos_datos5<-as.data.frame(nuevos_datos5)
pred_ens5 <- apply(nuevos_datos5,1,Mode)
nuevos_datos5$pred_ens5 <- pred_ens5
nuevos_datos5$class <- datIn3$class
mc_ens5<- table(nuevos_datos5$class, nuevos_datos5$pred_ens5)
mc_ens5

#ensemble6: xgbm,gbm,rf,redes,knn
nuevos_datos6<- cbind(pred_xgbm,pred_gbm, pred_rf, pred_avnnet,pred_knn)
nuevos_datos6<-as.data.frame(nuevos_datos6)
pred_ens6 <- apply(nuevos_datos6,1,Mode)
nuevos_datos6$pred_ens6 <- pred_ens6
nuevos_datos6$class <- datIn3$class
mc_ens6<- table(nuevos_datos6$class, nuevos_datos6$pred_ens6)
mc_ens6

#ensemble7: rf,gbm,xgbm
nuevos_datos7<- cbind(pred_rf,pred_gbm,pred_xgbm)
#nuevos_datos7<- cbind(nuevos_datos$pred_rf,nuevos_datos$pred_gbm,nuevos_datos$pred_xgbm)
nuevos_datos7<-as.data.frame(nuevos_datos7)
pred_ens7 <- apply(nuevos_datos7,1,Mode)
nuevos_datos7$pred_ens7 <- pred_ens7
nuevos_datos7$class <- datIn3$class
mc_ens7<- table(nuevos_datos7$class, nuevos_datos7$pred_ens7)
mc_ens7


#ensemble8: gbm,xgbm,rf
nuevos_datos8<- cbind(pred_gbm,pred_xgbm,pred_rf)
nuevos_datos8<- cbind(nuevos_datos$pred_gbm,nuevos_datos$pred_xgbm,nuevos_datos$pred_rf)
nuevos_datos8<-as.data.frame(nuevos_datos8)
pred_ens8 <- apply(nuevos_datos8,1,Mode)
nuevos_datos8$pred_ens8 <- pred_ens8
nuevos_datos8$class <- datIn3$class
mc_ens8<- table(nuevos_datos8$class, nuevos_datos8$pred_ens8)
mc_ens8

#ensemble9: gbm,xgbm,rf
nuevos_datos9<- cbind(pred_xgbm,pred_rf,pred_gbm)
nuevos_datos9<-as.data.frame(nuevos_datos9)
pred_ens9 <- apply(nuevos_datos9,1,Mode)
nuevos_datos9$pred_ens9 <- pred_ens9
nuevos_datos9$class <- datIn3$class
mc_ens9<- table(nuevos_datos9$class, nuevos_datos9$pred_ens9)
mc_ens9

#ensemble10: rf,xgbm,gbm
nuevos_datos10<- cbind(pred_rf,pred_xgbm,pred_gbm)
nuevos_datos10<-as.data.frame(nuevos_datos10)
pred_ens10 <- apply(nuevos_datos10,1,Mode)
nuevos_datos10$pred_ens10 <- pred_ens10
nuevos_datos10$class <- datIn3$class
mc_ens10<- table(nuevos_datos10$class, nuevos_datos10$pred_ens10)
mc_ens10

#ensemble11: gbm,rf,xgbm
nuevos_datos11<- cbind(pred_gbm,pred_rf,pred_xgbm)
nuevos_datos11<-as.data.frame(nuevos_datos11)
pred_ens11 <- apply(nuevos_datos11,1,Mode)
nuevos_datos11$pred_ens11 <- pred_ens11
nuevos_datos11$class <- datIn3$class
mc_ens11<- table(nuevos_datos11$class, nuevos_datos11$pred_ens11)
mc_ens11


#Mejor ensamblado: ensemble3                                                                                                                                   

