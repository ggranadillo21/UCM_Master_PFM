#Cargamos las librerías que vamos a utilizar
#install.packages("ROSE")
#install.packages(c( "foreach", "iterators", "doParallel") )
#install.packages("chron")
#install.packages("randomForest")
#install.packages("ISLR")

install.packages("InformationValue") 
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
library(corrplot)
library(tidyr)
library(OneR)

#Confirguramos el directorio de trabajo
setwd("C:/Users/A663876/Desktop/UCM Máster/8. Machine learning con R/CIDDS")
#cargamos funciones
source ("funciones utiles.R")

# Data Loading
datIn <- fread(file = 'CIDDS-001-internal-week1.csv', stringsAsFactors = TRUE) #dataset semana1
head(datIn,5)
summary(datIn)


#Analizando el dataset
str(datIn)
head(datIn,5)
summary(datIn)
describe(datIn)


#Visualización del dataset
ggplot(datIn, aes(Flags, colour = class)) +
  geom_freqpoly(binwidth = 1) + labs(title="Flags Distribution by Class")
ggpairs(datIn)

#EDA

# categorical plot
x <- inspect_cat(datIn) 
show_plot(x)

# correlations in numeric columns
x <- inspect_cor(datIn)
show_plot(x)

# feature imbalance bar plot
x <- inspect_imb(datIn)
show_plot(x)

# memory usage barplot
x <- inspect_mem(datIn)
show_plot(x)

# missingness barplot
x <- inspect_na(datIn)
show_plot(x)

# histograms for numeric columns
x <- inspect_num(datIn)
show_plot(x)

# barplot of column types
x <- inspect_types(datIn)
show_plot(x)

#Veo gráficamente el efecto de las variables más importantes con respecto a class
mosaico(datIn$Flags,datIn$class,"Flags") 
mosaico(datIn$Proto,datIn$class,"Protocol")
mosaico(datIn$Bytes,datIn$class,"Bytes")
boxplot_cuantcuali(datIn$Packets,datIn$class,"Packets") 
boxplot_cuantcuali(datIn$Tos,datIn$class,"Tos") 
boxplot_cuantcuali(datIn$`Src Pt`,datIn$class,"Source Port") 
#Miramos otros gráficos
# ggplot(datIn, aes(Flags, class))+geom_point(color="red") 
# ggplot(datIn, aes(Bytes, class))+geom_point(color="red") 
# ggplot(datIn, aes(n000, yesno))+geom_point(color="red") 
# ggplot(datIn, aes(bang, yesno))+geom_point(color="red") 
# ggplot(datIn, aes(money, yesno))+geom_point(color="red") 
# ggplot(datIn, aes(make, yesno))+geom_point(color="red") 

hist(datIn$Bytes)
hist(datIn$Flags)
hist(datIn$`Src Pt`)
hist(datIn$`Dst IP Addr`)


plot_ly(datIn, x = ~water, y = ~age, z = ~cstrength, type ='mesh3d')


# Depuración del dataset

datIn$attackType <- NULL #no es util porque es una clasificación binaria
datIn$attackID <- NULL #no es util porque es una clasificación binaria
datIn$attackDescription <- NULL #no es util porque es una clasificación binaria
datIn$Flows <- NULL #todas las observaciones son iguales
datIn$`Src IP Addr` <-NULL #de momento no lo tomaremos en cuenta por ser una categoría con mucha cardinalidad 
datIn$`Dst IP Addr` <- NULL
head(datIn,5)

# transformamos a binaria numérica la variable objetivo 
datIn$class <- ifelse(datIn$class =="normal", 0, 1)

#Transformamos Bytes con valores e.g., "8.3 M" a numérico 
datIn$Bytes <- gsub("M", "000000", datIn$Bytes)
datIn$Bytes <- gsub("[[:blank:]]", "", datIn$Bytes)
datIn$Bytes <- gsub("\\.", "", datIn$Bytes)
datIn$Bytes <- as.numeric(datIn$Bytes)

#Revisamos si tenemos NAs
sapply(datIn, function(x) sum(is.na(x)))
freq(datIn$class)

# Renombramos algunas variables 
names(datIn)[1] <- "Date"
names(datIn)[4] <- "SrcPt"
names(datIn)[5] <- "DstPt"

#Feature engineering
datIn$Packets_speed <- ifelse(datIn$Duration >0.000, datIn$Packets/datIn$Duration, datIn$Packets/0.0001)
datIn$Bytes_speed <- ifelse(datIn$Duration >0.000, datIn$Bytes/datIn$Duration, datIn$Bytes/0.0001)

#Transformamos variable Date
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
#datIn$Year<- year(datIn$Date) #El año es igual para todas las observaciones, lo obviamos
#datIn$Year<-format(as.POSIXct(datIn$Date), format="%Y")
#datIn$Year <- as.numeric(datIn$Year)
datIn$Day<- day(datIn$Date)
datIn$Day<-format(as.POSIXct(datIn$Date), format="%d")
datIn$Day <- as.numeric(datIn$Day)

#Transformamos Proto y Flags en sus correspondientes valores numéricos
datIn$Proto<- with(datIn, prot$Decimal[match(Proto, prot$Keyword)])
datIn$Flags<- with(datIn, flags$Num[match(Flags, flags$TCPFlag)])
head(datIn,5)
summary(datIn)

#Estandarizamos variables 
datIn2<-datIn
class<-datIn2[,"class"]
means <-apply(datIn2[,c("Date", "Duration", "SrcPt", "DstPt", "Packets", "Bytes", "Tos", "Packets_speed", "Bytes_speed")],2,mean,na.rm=TRUE)#vector de medias 
sds<-sapply(datIn2[,c("Date", "Duration", "SrcPt", "DstPt", "Packets", "Bytes", "Tos", "Packets_speed", "Bytes_speed")],sd,na.rm=TRUE) #vector de desviaciones típicas
datIn3<-scale(datIn2[,c("Date", "Duration", "SrcPt", "DstPt", "Packets", "Bytes", "Tos", "Packets_speed", "Bytes_speed")], center = means, scale = sds) #escalamos cada vector
datIn3<-data.frame(cbind(datIn3,datIn2[,c("Proto", "Flags")], class)) #dataframe ya transformado, con variables estandarizadas, media cero y desviación típica 1


# Muestreo estratificado. Vamos a crear un subset del dataset creado anteriormente
# Tomamos sample_frac(0.01) para tener 10486 observaciones
muestra<- datIn3 %>% group_by(class) %>% sample_frac(0.01)
table(muestra$class) 
freq(muestra$class)
datIn_bien<-muestra

save(datIn_bien,file="datIn_bien.Rda")
write.csv(datIn_bien, "C:\\Users\\A663876\\Desktop\\UCM Máster\\8. Machine learning con R\\CIDDS\\dataset.csv", row.names = TRUE)

################################################################################
# ************************************
# REGRESION LOGISTICA 
# ************************************
################################################################################

setwd("C:/Users/A663876/Desktop/UCM Máster/8. Machine learning con R/CIDDS")
source ("funciones utiles.R")
load("datIn_bien.Rda")
write.csv(datIn_bien, "C:\\Users\\A663876\\Desktop\\UCM Máster\\8. Machine learning con R\\CIDDS\\datIn_bien.csv", row.names = TRUE)

datIn_bien2 <- datIn_bien
sapply(datIn_bien2, function(x) sum(is.na(x)))

# Preparamos el dataset para el entrenamiento
class<-datIn_bien2[,"class"]
datIn_bien2$class <- ifelse(datIn_bien2$class==0, "No","Yes")
datIn_bien2$class <- as.character(datIn_bien2$class)
datIn_bien2
vardep<-c("class")
data<-as.data.frame(datIn_bien2)
save(data,file="data.Rda")
load("data.Rda")
# Training test una sola vez
control<-trainControl(method = "LGOCV",p=0.8,number=1,
                      classProbs=TRUE,savePredictions = "all") 

logi<- train(class~.,data=data,method="glm",trControl=control)
saveRDS(logi, "./logi.rds")
logi
summary(logi)
logi <- readRDS("./logi.rds")
# Accuracy   Kappa    
# 0.8177481  0.4360027

# La función confusionMatrix de caret calcula la matriz de confusión
sal<-logi$pred
salconfu<-confusionMatrix(sal$pred,sal$obs)
salconfu
# Prediction   No  Yes
# No  1500   33
# Yes  349  214

# Para dibujar la curva roc y calcular el auc se usa el paquete pROC

curvaroc<-roc(response=sal$obs,predictor=sal$Yes)
auc<-curvaroc$auc
auc #Area under the curve: 0.8388
plot(roc(response=sal$obs,predictor=sal$Yes))


# SELECCIÓN DE VARIABLES EN CLASIFICACIÓN BINARIA LOGÍSTICA

full<-glm(factor(class)~.,data=data,family = binomial(link="logit"))
null<-glm(factor(class)~1,data=data,family = binomial(link="logit"))

seleccion<-stepAIC(null,scope=list(upper=full),direction="both")

# Para ver los efectos escogidos
dput(names(seleccion$coefficients))
# c("(Intercept)", "Tos", "Flags", "Duration", "Bytes_speed", "SrcPt", 
#   "Packets", "Hour", "Packets_speed")

# Esto si se quiere en versión formula
formula(seleccion)
# factor(class) ~ Tos + Flags + Duration + Bytes_speed + SrcPt + 
#   Packets + Hour + Packets_speed

dput(names(datIn_bien2))
# c("Duration", "Proto", "SrcPt", "DstPt", "Packets", "Bytes", 
#   "Flags", "Tos", "Packets_speed", "Bytes_speed", "Hour", "Min", 
#   "Sec", "class")

# APLICANDO steprepetido binaria 
#source("funcion steprepetido binaria.R")

listconti<-c("Tos", "Flags", "Duration", "Bytes_speed", "SrcPt",
             "Packets", "Hour", "Packets_speed")


lista<-steprepetidobinaria(data=data,
                           vardep=vardep,listconti=listconti,sinicio=12345,
                           sfinal=12355,porcen=0.8,criterio="AIC")

tabla<-lista[[1]]
dput(lista[[2]][[1]])
lista1<-c("Tos", "Flags", "Duration", "Bytes_speed", "SrcPt", "Packets", 
          "Packets_speed", "Hour")

dput(lista[[2]][[2]])
#no info

lista<-steprepetidobinaria(data=data,
                           vardep=vardep,listconti=listconti,sinicio=12345,
                           sfinal=12355,porcen=0.8,criterio="BIC")

tabla<-lista[[1]]
dput(lista[[2]][[1]])
lista2<-c("Tos", "Flags", "Duration", "Bytes_speed", "SrcPt", "Packets")
dput(lista[[2]][[2]])
lista3<-c("Tos", "Flags", "Duration", "Bytes_speed", "SrcPt")

# ************************************
# APLICANDO cruzadalogistica a los modelos candidatos
# ************************************

#source("cruzadas avnnet y log binaria.R")
#save(data,file="data.Rda")
# EVALUO LOS MODELOS CON LOGISTICA Y COMPARO USANDO BOXPLOT
#data<-as.data.frame(data)
medias1<-cruzadalogistica(data=data,
                          vardep="class",listconti=lista1,
                          listclass=c(""), grupos=4,sinicio=1234,repe=5)


medias1$modelo="Logística1"
saveRDS(medias1, "./model1_logistica.rds")
#medias1 <- readRDS("./model1_logistica.rds")

tablamedias1<-medias1 %>%
  group_by(modelo) %>%
  summarise(tasa=mean(tasa)) 
tablamedias1a<-medias1 %>%
  group_by(modelo) %>%
  summarise(auc=mean(auc)) 



medias2<-cruzadalogistica(data=data, vardep="class",listconti=lista2,
                          listclass=c(""), grupos=4,sinicio=1234,repe=5)

medias2$modelo="Logística2"
saveRDS(medias2, "./model2_logistica.rds")
#medias2 <- readRDS("./model2_logistica.rds")
tablamedias2<-medias2 %>%
  group_by(modelo) %>%
  summarise(tasa=mean(tasa)) 
tablamedias2a<-medias2 %>%
  group_by(modelo) %>%
  summarise(auc=mean(auc)) 

medias3<-cruzadalogistica(data=data, vardep="class",listconti=lista3,
                          listclass=c(""), grupos=4,sinicio=1234,repe=5)

medias3$modelo="Logística3"
saveRDS(medias3, "./model3_logistica.rds")
#medias3 <- readRDS("./model3_logistica.rds")
tablamedias3<-medias3 %>%
  group_by(modelo) %>%
  summarise(tasa=mean(tasa)) 
tablamedias3a<-medias3 %>%
  group_by(modelo) %>%
  summarise(auc=mean(auc)) 


union1<-rbind(medias1,medias2, medias3)

par(cex.axis=0.8, las=1)
union1$modelo <- with(union1,reorder(modelo,tasa, mean))
boxplot(data=union1,tasa~modelo,col="pink",main="TASA FALLOS")
union1$modelo <- with(union1,reorder(modelo,auc, mean))
boxplot(data=union1,auc~modelo,col="pink",main="AUC")

#Logistica1 (medias1) presenta mejor AUC y segunda mejor tasa de fallos. 
#Modelo ganador con las siguientes variables:
#c("Tos", "Flags", "Duration", "Bytes_speed", "SrcPt","Packets", "Hour", "Packets_speed")


#creamos y guardamos modelo ganador

control<-trainControl(method = "LGOCV",p=0.8,number=1,
                      classProbs=TRUE,savePredictions = "all") 

model_logi<- train(class~Tos + Flags + Duration + Bytes_speed + SrcPt + Packets + Hour 
                   + Packets_speed, data=data,method="glm",trControl=control)

model_logi
# Accuracy   Kappa    
# 0.9723282  0.864531
saveRDS(model_logi, "./logistica.rds")
logistica <- readRDS("./logistica.rds")

################################################################################
# ***************************
# RED
# ***************************
################################################################################

set.seed(123456)
nnetgrid <-  expand.grid(size=c(5),decay=c(0.1),bag=FALSE)

red1<- train(class~Tos + Flags + Duration + Bytes_speed + SrcPt + Packets + Hour 
             + Packets_speed,data=data,
             method="avNNet",linout = FALSE,maxit=100,repeats=5,
             trControl=control,tuneGrid=nnetgrid)
saveRDS(red1, "./model_red1.rds")
#red1 <- readRDS("./model_red1.rds")
summary(red1)
# Accuracy   Kappa    
# 0.9914122  0.9575027
sal<-red1$pred


# La función confusionMatrix de caret calcula la matriz de confusión

salconfu<-confusionMatrix(sal$pred,sal$obs)
salconfu
# No  1848   17
# Yes    1  230


# Para dibujar la curva roc y calcular el auc se usa el paquete pROC

curvaroc<-roc(response=sal$obs,predictor=sal$Yes)
auc<-curvaroc$auc
auc #Area under the curve: 0.9934
plot(roc(response=sal$obs,predictor=sal$Yes))


# *********************************
# CRUZADA avNNet
#  ************************************

# TUNEANDO LA RED CON LOS  MODELOS CANDIDATOS USANDO CV


set.seed(12346)
# Validación cruzada repetida
control<-trainControl(method = "repeatedcv",number=4,repeats=5,savePredictions = "all",classProbs=TRUE) 

# ***************************************************************
# nnet: parámetros
#     Number of Hidden Units (size, numeric) 
#     Weight Decay (decay, numeric) --> número de nodos de la red
#     Maxit --< indica el número máximo de interacciones del proceso de estimación-optimización
# ***************************************************************

nnetgrid <-  expand.grid(size=c(5,10,15,20),decay=c(0.01,0.1,0.001))

rednnet<- train(class~Tos + Flags + Duration + Bytes_speed + SrcPt + Packets + Hour 
                + Packets_speed,data=data,
                method="nnet",linout = FALSE,maxit=100,trControl=control,tuneGrid=nnetgrid)

rednnet #size = 20 and decay = 0.01
saveRDS(rednnet, "./model_rednnet.rds")
#rednnet <- readRDS("./model_rednnet.rds")

# ***************************************************************
# avNNet: parámetros
# Number of Hidden Units (size, numeric)
# Weight Decay (decay, numeric) --> regularization (e.g., L2)
# Bagging (bag, logical)--> bag=FALSE indica que queremos hacer subsampling en cada constiucción de red
# ***************************************************************
avnnetgrid <-expand.grid(size=c(5,10,15,20),decay=c(0.01,0.1,0.001),bag=FALSE)

redavnnet<- train(class~Tos + Flags + Duration + Bytes_speed + SrcPt + Packets + Hour 
                  + Packets_speed,data=data,
                  method="avNNet",linout = FALSE,maxit=100,trControl=control,repeats=5,tuneGrid=avnnetgrid)

redavnnet #size = 15, decay = 0.01 and bag = FALSE
saveRDS(redavnnet, "./model_redavnnet.rds")
#redavnnet <- readRDS("./model_redavnnet.rds")

# ************************************
# COMPARANDO LOS MODELOS FINALES
# ***********************************

medias5<-cruzadaavnnetbin(data=data,
                          vardep=vardep,listconti=listconti,
                          listclass=c(""),grupos=4,sinicio=1234,repe=5,
                          size=c(20),decay=c(0.01),repeticiones=5,itera=100)

medias5$modelo="avnnet1"
saveRDS(medias5, "./model1_nnet.rds")
#medias5 <- readRDS("./model1_nnet.rds")
tablamedias5<-medias5 %>%
  group_by(modelo) %>%
  summarise(tasa=mean(tasa)) 
tablamedias5a<-medias5 %>%
  group_by(modelo) %>%
  summarise(auc=mean(auc))

medias7<-cruzadaavnnetbin(data=data,
                          vardep=vardep,listconti=listconti,
                          listclass=c(""),grupos=4,sinicio=1234,repe=5,
                          size=c(15),decay=c(0.01),repeticiones=5,itera=100)

medias7$modelo="avnnet3"
saveRDS(medias7, "./model3_avnnet.rds")
#medias7 <- readRDS("./model3_avnnet.rds")
tablamedias7<-medias7 %>%
  group_by(modelo) %>%
  summarise(tasa=mean(tasa)) 
tablamedias7a<-medias7 %>%
  group_by(modelo) %>%
  summarise(auc=mean(auc))


union2<-rbind(medias5, medias7)
par(cex.axis=0.8, las=1)
union2$modelo <- with(union2,reorder(modelo,tasa, mean))
boxplot(data=union2,tasa~modelo,col="pink",main="TASA FALLOS")
union2$modelo <- with(union2,reorder(modelo,auc, mean))
boxplot(data=union2,auc~modelo,col="pink",main="AUC")
#mejor modelo avnnet3 (medias7) por poseer mejor AUC, aunque psoee la peor tasa de fallos

#Creamos modelo ganador: model_avnnet fijando size = 15, maxit=100 y decay= 0.01 
control<-trainControl(method = "repeatedcv",number=4,repeats=5,savePredictions = "all",classProbs=TRUE) 

set.seed(123)
nnetgrid <- expand.grid(size=c(15),decay=c(0.01),bag=F)
model_nnet <- train(class~ Tos + Flags + Duration + Bytes_speed + SrcPt + Packets + Hour 
                    + Packets_speed,data=data,
                    method="avNNet",linout = FALSE,maxit=100,trControl=control,tuneGrid=nnetgrid)
model_nnet
saveRDS(model_nnet, "./neuralnet.rds")
#neuralnet <- readRDS("./neuralnet.rds")

#### comparamos el mejor modelo de cada algoritmo
medias7$modelo="NNET"
union3<-rbind(medias1, medias7)
par(cex.axis=0.8, las=1)
union3$modelo <- with(union3,reorder(modelo,tasa, mean))
boxplot(data=union3,tasa~modelo,col="pink",main="TASA FALLOS")
union3$modelo <- with(union3,reorder(modelo,auc, mean))
boxplot(data=union3,auc~modelo,col="pink",main="AUC")


################################################################################
# ***************************
#  RANDOM FOREST 
# ***************************
################################################################################
set.seed(12345)
rfgrid<-expand.grid(mtry=c(5,6,7,8,9,10,11,12,13,14)) #se prueban con diferentes números de 3 a 14 puesto que tenemos 11 variables input posibles

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) #callsprobs= true se usa cuando la variable dependiente es binaria

rf<- train(factor(class)~.,data=data,#se usa como factor la variable dependiente
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = FALSE,ntree=450,nodesize=10,replace=TRUE, #linout=false indica que la variable dependiente es categórica
           importance=TRUE)#raplace=true indica que se va a utilizar el algoritmo clásico con reemplazamiento
#importance=true conserva las estadísticas de importancia de cada variable

rf# mtry= 7

# IMPORTANCIA DE VARIABLES
install.packages("randomForest")
library(randomForest)
final<-rf$finalModel

tabla<-as.data.frame(importance(final))
tabla<-tabla[order(-tabla$MeanDecreaseAccuracy),]
tabla

barplot(tabla$MeanDecreaseAccuracy,names.arg=rownames(tabla))

rfbis<-randomForest(factor(class)~.,
                    data=data,
                    mtry=7,ntree=450,nodesize=10,replace=TRUE)

plot(rfbis$err.rate[,1])#seleccionar ntree=150



# EL TUNEADO ANTERIOR LO HABÍAMOS REALIZADO CON TODAS LAS VARIABLES
# PERO SABEMOS QUE SOLO 8 SON IMPORTANTES, VAMOS A REALIZAR EL TUNEADO 
# UNA SEGUNDA VEZ CON SOLO LAS VARIABLES DE INTERÉS

set.seed(12345)
rfgrid<-expand.grid(mtry=c(3,4,5,6,7,8))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

rf<- train(factor(class)~Tos + Flags + Duration + Bytes_speed + SrcPt + Packets + Hour 
           + Packets_speed,data=data,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = FALSE,ntree=150,nodesize=10,replace=TRUE,
           importance=TRUE)

rf #mtry=7 es la que aporta mejores resultados

#Miramos las variables más predictoras

final<-rf$finalModel
tabla<-as.data.frame(importance(final))
tabla<-tabla[order(-tabla$MeanDecreaseAccuracy),]
tabla
par(cex.axis=0.8,las=2)
barplot(tabla$MeanDecreaseAccuracy,names.arg=rownames(tabla))
#Parece que Tos y Hour no son muy predictoras

#### Tuneamos sampsize en rf con los siguientes valores: mtry=7, ntree=150
# con 4 grupos de CV, maximo 9000 sampsize

mediasrf1<-cruzadarfbin(data=data, vardep=vardep,
                        listconti=listconti,
                        listclass=c(""),
                        grupos=4,sinicio=1234,repe=5,nodesize=10,
                        mtry=7,ntree=150,replace=TRUE,sampsize=50)
mediasrf1$modelo="rf50"

mediasrf2<-cruzadarfbin(data=data, vardep=vardep,
                        listconti=listconti,
                        listclass=c(""),
                        grupos=4,sinicio=1234,repe=5,nodesize=10,
                        mtry=7,ntree=150,replace=TRUE,sampsize=100)
mediasrf2$modelo="rf100"

mediasrf3<-cruzadarfbin(data=data, vardep=vardep,
                        listconti=listconti,
                        listclass=c(""),
                        grupos=4,sinicio=1234,repe=5,nodesize=10,
                        mtry=7,ntree=150,replace=TRUE,sampsize=250)
mediasrf3$modelo="rf250"

mediasrf4<-cruzadarfbin(data=data, vardep=vardep,
                        listconti=listconti,
                        listclass=c(""),
                        grupos=4,sinicio=1234,repe=5,nodesize=10,
                        mtry=7,ntree=150,replace=TRUE,sampsize=500)
mediasrf4$modelo="rf500"

mediasrf5<-cruzadarfbin(data=data, vardep=vardep,
                        listconti=listconti,
                        listclass=c(""),
                        grupos=4,sinicio=1234,repe=5,nodesize=10,
                        mtry=7,ntree=150,replace=TRUE,sampsize=1000)
mediasrf5$modelo="rf1000"

mediasrf6<-cruzadarfbin(data=data, vardep=vardep,
                        listconti=listconti,
                        listclass=c(""),
                        grupos=4,sinicio=1234,repe=5,nodesize=10,
                        mtry=7,ntree=150,replace=TRUE,sampsize=3000)
mediasrf6$modelo="rf3000"

mediasrf7<-cruzadarfbin(data=data, vardep=vardep,
                        listconti=listconti,
                        listclass=c(""),
                        grupos=4,sinicio=1234,repe=5,nodesize=10,
                        mtry=7,ntree=150,replace=TRUE,sampsize=5000)
mediasrf7$modelo="rf5000"

mediasrf8<-cruzadarfbin(data=data, vardep=vardep,
                        listconti=listconti,
                        listclass=c(""),
                        grupos=4,sinicio=1234,repe=5,nodesize=10,
                        mtry=7,ntree=150,replace=TRUE,sampsize=7000)
mediasrf8$modelo="rf7000"
mediasrf9<-cruzadarfbin(data=data, vardep=vardep,
                        listconti=listconti,
                        listclass=c(""),
                        grupos=4,sinicio=1234,repe=5,nodesize=10,
                        mtry=7,ntree=150,replace=TRUE)
mediasrf9$modelo="rfBASE"

union4<-rbind(mediasrf1,mediasrf2,mediasrf3,mediasrf4,mediasrf5,mediasrf6,mediasrf7,mediasrf8,mediasrf9)

par(cex.axis=0.8)
boxplot(data=union4,tasa~modelo,main="TASA FALLOS",col="pink")

uni<-union4
uni$modelo <- with(uni,reorder(modelo,tasa, mean))
par(cex.axis=0.8,las=2)
boxplot(data=uni,tasa~modelo,col="pink")
uni$modelo <- with(uni,reorder(modelo,auc, mean))
boxplot(data=uni,auc~modelo,col="pink",main="AUC")
#seleccionamos sampsize=rf7000

#Comparamos los siguientes modelos: 1) con todas las variables, 2) eliminando Hour, 3) eliminando Hour y Tos

listconti1<-c("Flags", "Duration", "Bytes_speed", "SrcPt",
              "Packets","Packets_speed")
listconti2<-c("Tos", "Flags", "Duration", "Bytes_speed", "SrcPt",
              "Packets", "Packets_speed")


#Evaluamos el modelo y lo comparamos con los anteriores
set.seed(12345)
medias9<-cruzadarfbin(data=data, vardep=vardep,
                      listconti=listconti,
                      listclass=c(""),
                      grupos=4,sinicio=1234,repe=5,nodesize=10,
                      mtry=7,ntree=150,replace=TRUE,sampsize=7000)

medias9$modelo="RF"
saveRDS(medias9, "./model_rf.rds")
#medias9 <- readRDS("./model_rf.rds")
tablamedias9<-medias9 %>%
  group_by(modelo) %>%
  summarise(tasa=mean(tasa)) 
tablamedias9a<-medias9 %>%
  group_by(modelo) %>%
  summarise(auc=mean(auc))

medias9a<-cruzadarfbin(data=data, vardep=vardep,
                       listconti=listconti1,
                       listclass=c(""),
                       grupos=4,sinicio=1234,repe=5,nodesize=10,
                       mtry=7,ntree=150,replace=TRUE,sampsize=7000)

medias9a$modelo="RF1"
saveRDS(medias9a, "./model_rf1.rds")
#medias9a <- readRDS("./model_rf1.rds")


medias9b<-cruzadarfbin(data=data, vardep=vardep,
                       listconti=listconti2,
                       listclass=c(""),
                       grupos=4,sinicio=1234,repe=5,nodesize=10,
                       mtry=7,ntree=150,replace=TRUE,sampsize=7000)

medias9b$modelo="RF2"
saveRDS(medias9b, "./model_rf2.rds")
#medias9b <- readRDS("./model_rf2.rds")

union5<-rbind(medias9,medias9a, medias9b)
par(cex.axis=0.8, las=1)
union5$modelo <- with(union5,reorder(modelo,tasa, mean))
boxplot(data=union5,tasa~modelo,col="pink",main="TASA FALLOS")
union5$modelo <- with(union5,reorder(modelo,auc, mean))
boxplot(data=union5,auc~modelo,col="pink",main="AUC")


union_5<-rbind(medias1,medias7, medias9)
par(cex.axis=0.8, las=1)
union_5$modelo <- with(union_5,reorder(modelo,tasa, mean))
boxplot(data=union_5,tasa~modelo,col="pink",main="TASA FALLOS")
union_5$modelo <- with(union_5,reorder(modelo,auc, mean))
boxplot(data=union_5,auc~modelo,col="pink",main="AUC")

#modelo ganador rf
set.seed(12345)
rfgrid<-expand.grid(mtry=c(7))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

rf<- train(factor(class)~Tos + Flags + Duration + Bytes_speed + SrcPt + Packets + Hour 
           + Packets_speed, data=data,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = FALSE,ntree=150,nodesize=10,replace=TRUE,sampsize=7000,
           importance=TRUE)
saveRDS(rf, "./randomforest.rds")
#randomforest <- readRDS("./randomforest.rds")


################################################################################
# ***************************
# GRADIENT BOOSTING 
# ***************************
################################################################################

set.seed(12345)

gbmgrid<-expand.grid(shrinkage=c(0.2,0.1,0.05,0.03,0.01,0.001),
                     n.minobsinnode=c(5,10,20),
                     n.trees=c(100,500,1000,3000),
                     interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

gbm<- train(factor(class)~Tos + Flags + Duration + Bytes_speed + SrcPt + Packets + Hour 
            + Packets_speed,data=data,
            method="gbm",trControl=control,tuneGrid=gbmgrid,
            distribution="bernoulli", bag.fraction=1,verbose=FALSE)

gbm #n.trees = 3000, interaction.depth = 2, shrinkage = 0.1, and n.minobsinnode = 20

plot(gbm)


# ESTUDIO DE EARLY STOPPING
# Probamos a fijar algunos parámetros para ver como evoluciona
# en función de las iteraciones
set.seed(12345)
gbmgrid<-expand.grid(shrinkage=c(0.1),
                     n.minobsinnode=c(20),
                     n.trees=c(100,500,1000,3000,5000),
                     interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

gbm<- train(factor(class)~Tos + Flags + Duration + Bytes_speed + SrcPt + Packets + Hour 
            + Packets_speed,data=data,
            method="gbm",trControl=control,tuneGrid=gbmgrid,
            distribution="bernoulli", bag.fraction=1,verbose=FALSE)

gbm
plot(gbm)#early stopping en ntree=3000

# IMPORTANCIA DE VARIABLES
par(cex=0.8)
summary(gbm)#muestra que Tos y Hour son muy poco predictoras, probamos quitándolas
#variables a retener:
listconti1<-c("Flags", "Duration", "Bytes_speed", "SrcPt",
              "Packets","Packets_speed")
listconti2<-c("Tos", "Flags", "Duration", "Bytes_speed", "SrcPt",
              "Packets", "Packets_speed")

tabla<-summary(gbm)
par(cex=1.2,las=2)
barplot(tabla$rel.inf,names.arg=row.names(tabla))

#source ("cruzada gbm binaria.R")

medias10<-cruzadagbmbin(data=data, vardep=vardep,
                        listconti=listconti1,
                        listclass=c(""),
                        grupos=4,sinicio=1234,repe=5,
                        n.minobsinnode=20,shrinkage=0.1,n.trees=3000,interaction.depth=2)
medias10$modelo="GBM"
saveRDS(medias10, "./model_gbm.rds")
#medias10 <- readRDS("./model_gbm.rds")

medias11<-cruzadagbmbin(data=data, vardep=vardep,
                        listconti=listconti2,
                        listclass=c(""),
                        grupos=4,sinicio=1234,repe=5,
                        n.minobsinnode=20,shrinkage=0.1,n.trees=3000,interaction.depth=2)
medias11$modelo="GBM1"
saveRDS(medias11, "./model_gbm1.rds")
#medias11 <- readRDS("./model_gbm1.rds")

medias12<-cruzadagbmbin(data=data, vardep=vardep,
                        listconti=listconti,
                        listclass=c(""),
                        grupos=4,sinicio=1234,repe=5,
                        n.minobsinnode=20,shrinkage=0.1,n.trees=3000,interaction.depth=2)
medias12$modelo="GBM2"
saveRDS(medias12, "./model_gbm2.rds")
#medias12 <- readRDS("./model_gbm2.rds")

tablamedias12<-medias12 %>%
  group_by(modelo) %>%
  summarise(tasa=mean(tasa)) 
tablamedias12a<-medias12 %>%
  group_by(modelo) %>%
  summarise(auc=mean(auc))

union6<-rbind(medias10, medias11, medias12)
par(cex.axis=0.8, las=1)
union6$modelo <- with(union6,reorder(modelo,tasa, mean))
boxplot(data=union6,tasa~modelo,col="pink",main="TASA FALLOS")
union6$modelo <- with(union6,reorder(modelo,auc, mean))
boxplot(data=union6,auc~modelo,col="pink",main="AUC")
#mejor modelo GBM3 (medias13), tiene mejor AUC aunque peor tasa de fallos que los otros

medias12$modelo="GBM"
union7<-rbind(medias1, medias7, medias9, medias12)
par(cex.axis=0.8, las=1)
union7$modelo <- with(union7,reorder(modelo,tasa, mean))
boxplot(data=union7,tasa~modelo,col="pink",main="TASA FALLOS")
union7$modelo <- with(union7,reorder(modelo,auc, mean))
boxplot(data=union7,auc~modelo,col="pink",main="AUC")

#creamos el modelo GBM

set.seed(12345)

gbmgrid<-expand.grid(shrinkage=c(0.1),
                     n.minobsinnode=c(20),
                     n.trees=c(3000),
                     interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

gbm<- train(factor(class)~Tos + Flags + Duration + Bytes_speed + SrcPt + Packets + Hour 
            + Packets_speed,data=data,
            method="gbm",trControl=control,tuneGrid=gbmgrid,
            distribution="bernoulli", bag.fraction=1,verbose=FALSE)

saveRDS(gbm, "./gradientb.rds")
gbm # 'n.trees'= 3000, 'interaction.depth'= 2, 'shrinkage'=0.1, 'n.minobsinnode'=20
#gradientb <- readRDS("./gradientb.rds")


################################################################################
# ***************************
#  XGBOOST 
# ***************************
################################################################################

set.seed(12345)
xgbmgrid<-expand.grid(
  min_child_weight=c(5,10,20),
  eta=c(0.1,0.05,0.03,0.01,0.001),
  nrounds=c(100,500,1000,3000),
  max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

xgbm<- train(factor(class)~Tos + Flags + Duration + Bytes_speed + SrcPt + Packets + Hour 
             + Packets_speed,data=data,
             method="xgbTree",trControl=control,
             tuneGrid=xgbmgrid,verbose=FALSE)

xgbm # mejor config eta= 0.1, nrounds=3000, min_child_weight=5, gamma =0, colsample_bytree = 1,and subsample = 1

plot(xgbm)


# ESTUDIO DE EARLY STOPPING
# Probamos a fijar algunos parámetros para ver como evoluciona
# en función de las iteraciones. Usamos eta=0.1, min_child_weight=5 ya que ofrecen resultados interesantes 

xgbmgrid<-expand.grid(eta=c(0.1),
                      min_child_weight=c(5),
                      nrounds=c(1000,3000,5000,7000,10000),
                      max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

set.seed(12345)
control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

xgbm<- train(factor(class)~Tos + Flags + Duration + Bytes_speed + SrcPt + Packets + Hour 
             + Packets_speed,data=data,
             method="xgbTree",trControl=control,
             tuneGrid=xgbmgrid,verbose=FALSE)

plot(xgbm) # a partir de 5000 se vuelve constante

# Probamos con otras semillas para la validación cruzada

set.seed(12367)
control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

xgbm<- train(factor(class)~Tos + Flags + Duration + Bytes_speed + SrcPt + Packets + Hour 
             + Packets_speed,data=data,
             method="xgbTree",trControl=control,
             tuneGrid=xgbmgrid,verbose=FALSE)

plot(xgbm)

# IMPORTANCIA DE VARIABLES

varImp(xgbm)
plot(varImp(xgbm))
#nos damos cuenta que 1 variable no aportan casi nada, así que creamos una variable para simplificar el análisis

# PRUEBO PARÁMETROS CON VARIABLES SELECCIONADAS

xgbmgrid<-expand.grid(
  min_child_weight=c(5,10,20),
  eta=c(0.1,0.05,0.03,0.01,0.001),
  nrounds=c(100,500,1000,3000),
  max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

xgbm<- train(factor(class)~Tos + Flags + Duration + Bytes_speed + SrcPt + Packets 
             + Packets_speed,data=data,
             method="xgbTree",trControl=control,
             tuneGrid=xgbmgrid,verbose=FALSE)

xgbm #nrounds = 1000, max_depth = 6, eta = 0.05, gamma =0, colsample_bytree = 1, min_child_weight = 5 and subsample = 1.

plot(xgbm)

#evaluamos los modelos y comparamos con Boxplot

medias14<-cruzadaxgbmbin(data=data, vardep=vardep,
                         listconti=listconti2,#tiene 7 variables (se ha eliminado Hours)
                         listclass=c(""),
                         grupos=4,sinicio=1234,repe=5,
                         min_child_weight=5,eta=0.05,nrounds=1000,max_depth=6,
                         gamma=0,colsample_bytree=1,subsample=1,
                         alpha=0,lambda=0,lambda_bias=0)
medias14$modelo="xgbm"
saveRDS(medias14, "./model_xgbm.rds")
#medias14 <- readRDS("./model_xgbm.rds")

medias15<-cruzadaxgbmbin(data=data, vardep=vardep,
                         listconti=listconti,#probamos con todas las variables y config inicial
                         listclass=c(""),
                         grupos=4,sinicio=1234,repe=5,
                         min_child_weight=5,eta=0.1,nrounds=3000,max_depth=6,
                         gamma=0,colsample_bytree=1,subsample=1,
                         alpha=0,lambda=0,lambda_bias=0)
medias15$modelo="xgbm2"
saveRDS(medias15, "./model_xgbm2.rds")
#medias15 <- readRDS("./model_xgbm2.rds")
tablamedias15<-medias15 %>%
  group_by(modelo) %>%
  summarise(tasa=mean(tasa)) 
tablamedias15a<-medias15 %>%
  group_by(modelo) %>%
  summarise(auc=mean(auc))


medias16<-cruzadaxgbmbin(data=data, vardep=vardep,
                         listconti=listconti1,
                         listclass=c(""),
                         grupos=4,sinicio=1234,repe=5,#probamos con 6 variables (eliminamos Tos y Hour)
                         min_child_weight=5,eta=0.1,nrounds=3000,max_depth=6,
                         gamma=0,colsample_bytree=1,subsample=1,
                         alpha=0,lambda=0,lambda_bias=0)
medias16$modelo="xgbm3"
saveRDS(medias16, "./model_xgbm3.rds")
#medias16 <- readRDS("./model_xgbm3.rds")

medias16a<-cruzadaxgbmbin(data=data, vardep=vardep,
                          listconti=listconti1,
                          listclass=c(""),
                          grupos=4,sinicio=1234,repe=5,#probamos con 6 variables (eliminamos Tos y Hour)
                          min_child_weight=5,eta=0.05,nrounds=1000,max_depth=6,
                          gamma=0,colsample_bytree=1,subsample=1,
                          alpha=0,lambda=0,lambda_bias=0)
medias16a$modelo="xgbm4"
saveRDS(medias16a, "./model_xgbm4.rds")
#medias16a <- readRDS("./model_xgbm4.rds")

union8<-rbind(medias14,medias15,medias16,medias16a)
par(cex.axis=0.8, las=1)
union8$modelo <- with(union8,reorder(modelo,tasa, mean))
boxplot(data=union8,tasa~modelo,col="pink",main="TASA FALLOS")
union8$modelo <- with(union8,reorder(modelo,auc, mean))
boxplot(data=union8,auc~modelo,col="pink",main="AUC")

#Mejor modelo XGBoost (medias15). Creamos modelo ganador XGBoost
xgbmgrid<-expand.grid(min_child_weight=c(5), eta=c(0.1), nrounds=c(3000),
                      max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

xgbm<- train(factor(class)~Tos + Flags + Duration + Bytes_speed + SrcPt + Packets + Hour 
             + Packets_speed,data=data,
             method="xgbTree",trControl=control,
             tuneGrid=xgbmgrid,verbose=FALSE)

saveRDS(xgbm, "./xgbm.rds")
#xgbm <- readRDS("./xgbm.rds")

#Comparamos con los modelos creados anteriormente
medias15$modelo="XGBM"
union9<-rbind(medias1, medias7, medias9, medias12, medias15)
par(cex.axis=0.8, las=1)
union9$modelo <- with(union9,reorder(modelo,tasa, mean))
boxplot(data=union9,tasa~modelo,col="pink",main="TASA FALLOS")
union9$modelo <- with(union9,reorder(modelo,auc, mean))
boxplot(data=union9,auc~modelo,col="pink",main="AUC")



################################################################################
# ***************************
# SVM 
# ***************************
################################################################################


set.seed(12345)
SVMgrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

SVM<- train(data=data,factor(class)~Tos + Flags + Duration + Bytes_speed + SrcPt + Packets + Hour 
            + Packets_speed,method="svmLinear",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM$results #mejor resultado con C=10
plot(SVM$results$C,SVM$results$Accuracy)

# Rehago el grid para observar mejor el intervalo de C 
set.seed(12345)
SVMgrid<-expand.grid(C=c(10,15,20,50,100))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

SVM<- train(data=data,factor(class)~Tos + Flags + Duration + Bytes_speed + SrcPt + Packets + Hour 
            + Packets_speed,method="svmLinear",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM$results#mejor resultado con C=100
plot(SVM$results$C,SVM$results$Accuracy)



#COMPARAR CV REPETIDA Y BOXPLOT
medias17<-cruzadaSVMbin(data=data, vardep=vardep,
                        listconti=listconti,
                        listclass=c(""),
                        grupos=4,sinicio=1234,repe=5,C=100)
medias17$modelo="SVM"
saveRDS(medias17, "./model_svm.rds")
#medias17 <- readRDS("./model_svm.rds")
tablamedias17<-medias17 %>%
  group_by(modelo) %>%
  summarise(tasa=mean(tasa)) 
tablamedias17a<-medias17 %>%
  group_by(modelo) %>%
  summarise(auc=mean(auc))


#creamos modelo svm
set.seed(12345)
SVMgrid<-expand.grid(C=c(100))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

SVM<- train(data=data,factor(class)~Tos + Flags + Duration + Bytes_speed + SrcPt + 
              Packets + Hour + Packets_speed, method="svmLinear",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)
saveRDS(SVM, "./svm.rds")

#Comparamos todos los mejores modelos de cada algoritmo
union10<-rbind(medias1, medias7, medias9, medias12, medias15, medias17)
par(cex.axis=0.8, las=2)
union10$modelo <- with(union10,reorder(modelo,tasa, mean))
boxplot(data=union10,tasa~modelo,col="pink",main="TASA FALLOS")
union10$modelo <- with(union10,reorder(modelo,auc, mean))
boxplot(data=union10,auc~modelo,col="pink",main="AUC")


################################################################################
# ***************************
# K-nearest Neighbors KNN 
# ***************************
################################################################################
 

#Install class package
install.packages('class')
# Load class package
library(class)

control<-trainControl(method = "cv",number=4,savePredictions = "all", classProbs=TRUE) 
KNNgrid<-expand.grid(k = c(1,3,5,11,21,35,55,85,102))
model_knn<- train(data=data,factor(class)~Tos + Flags + Duration + Bytes_speed + SrcPt + 
                    Packets + Hour + Packets_speed, method="knn",trControl=control,
                  tuneGrid=KNNgrid)
#Mejor k= 1 con 0.9962807 de accuracy


#COMPARAR CV REPETIDA Y BOXPLOT
medias18<-cruzadaKNNbin(data=data, vardep=vardep,
                        listconti=listconti,
                        listclass=c(""),
                        grupos=4,sinicio=1234,repe=5,k=1)
medias18$modelo="KNN"
saveRDS(medias18, "./model_knn.rds")
#medias18 <- readRDS("./model_knn.rds")
# k  Accuracy     Kappa  AccuracySD     KappaSD
# 1 0.9960137 0.9808717 0.001024274 0.004901051
tablamedias18<-medias18 %>%
  group_by(modelo) %>%
  summarise(tasa=mean(tasa)) 
tablamedias18a<-medias18 %>%
  group_by(modelo) %>%
  summarise(auc=mean(auc))

#Comparamos todos los mejores modelos de cada algoritmo
medias1$modelo="LR"
medias7$modelo="NNET"
medias12$modelo="GBM"
medias15$modelo="XGBM"
union11<-rbind(medias1, medias7, medias9, medias12, medias15, medias17, medias18)
par(cex.axis=0.8, las=2)
union11$modelo <- with(union11,reorder(modelo,tasa, mean))
boxplot(data=union11,tasa~modelo,col="pink",main="TASA FALLOS")
union11$modelo <- with(union11,reorder(modelo,auc, mean))
boxplot(data=union11,auc~modelo,col="pink",main="AUC")


#creamos modelo KNN
set.seed(12345)
KNNgrid<-expand.grid(k=c(1))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

KNN<- train(data=data,factor(class)~Tos + Flags + Duration + Bytes_speed + SrcPt + 
              Packets + Hour + Packets_speed, method="knn",trControl=control,
            tuneGrid=KNNgrid)
saveRDS(KNN, "./knn.rds")
KNN <- readRDS("./knn.rds")



################################################################################
# ***************************
# Naive Bayes NB 
# ***************************
################################################################################


library(e1071)

setwd("C:/Users/A663876/Desktop/UCM Máster/8. Machine learning con R/CIDDS")
source ("funciones utiles.R")

load("datIn_bien.Rda")
datIn_bien2 <- datIn_bien
sapply(datIn_bien2, function(x) sum(is.na(x)))
class<-datIn_bien2[,"class"]

load("data.Rda")

control<-trainControl(method = "cv",number=4,savePredictions = "all", classProbs=TRUE) 
model_NB1<- train(data=data,factor(class)~Tos + Flags + Duration + Bytes_speed + SrcPt + 
                    Packets + Hour + Packets_speed, method="nb",trControl=control)
model_NB1$results
# usekernel fL adjust  Accuracy     Kappa  AccuracySD     KappaSD
# 1     FALSE  0      1 0.4926494 0.1447464 0.076438588 0.041664300
# 2      TRUE  0      1 0.9804501 0.9021085 0.001688491 0.008539092

#Plot Variable performance
X <- varImp(NB)
plot(X)

#Hacemos tuneado de NB
NBgrid<-expand.grid(usekernel = c(TRUE, FALSE), fL = 0:5, adjust = seq(0, 5, by = 1))
model_NB<- train(data=data,factor(class)~Tos + Flags + Duration + Bytes_speed + SrcPt + 
                   Packets + Hour + Packets_speed, method="nb",trControl=control,
                 tuneGrid=NBgrid, preProc = c("BoxCox", "center", "scale", "pca"))



# Mostramos el top 5 de modelos
model_NB$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))


# usekernel fL adjust  Accuracy     Kappa  AccuracySD    KappaSD
# 1      TRUE  0      1 0.9294302 0.7051488 0.009722439 0.03421885
# 2      TRUE  1      1 0.9294302 0.7051488 0.009722439 0.03421885
# 3      TRUE  2      1 0.9294302 0.7051488 0.009722439 0.03421885
# 4      TRUE  3      1 0.9294302 0.7051488 0.009722439 0.03421885
# 5      TRUE  4      1 0.9294302 0.7051488 0.009722439 0.03421885
# 6      TRUE  5      1 0.9294302 0.7051488 0.009722439 0.03421885


#Creamos el modelo con mejor resultado
listconti<-c("Tos", "Flags", "Duration", "Bytes_speed", "SrcPt",
             "Packets", "Hour", "Packets_speed")
vardep<-c("class")

medias19<-cruzadaNBbin(data=data, vardep=vardep,
                       listconti=listconti,
                       listclass=c(""),
                       grupos=4,sinicio=1234,repe=5,
                       usekernel=TRUE,fL=0, adjust=1)
medias19$modelo="NB"

saveRDS(medias19, "./model_NB.rds")
medias19 <- readRDS("./model_NB.rds")
tablamedias19<-medias19 %>%
  group_by(modelo) %>%
  summarise(tasa=mean(tasa)) 
tablamedias19a<-medias19 %>%
  group_by(modelo) %>%
  summarise(auc=mean(auc))


#creamos modelo NB
set.seed(12345)
control<-trainControl(method = "cv",number=4,savePredictions = "all") 
NBgrid<-expand.grid(usekernel = c(TRUE), fL = c(0), adjust =c(1))
NB<- train(data=data,factor(class)~Tos + Flags + Duration + Bytes_speed + SrcPt + 
             Packets + Hour + Packets_speed, method="nb",trControl=control,
           tuneGrid=NBgrid)
saveRDS(NB, "./NB.rds")
NB <- readRDS("./NB.rds")

#Comparamos todos los mejores modelos de cada algoritmo
medias1$modelo="LR"
medias7$modelo="NNET"
medias12$modelo="GBM"
medias15$modelo="XGBM"
union12<-rbind(medias1, medias7, medias9, medias12, medias15, medias17, medias18, medias19)
par(cex.axis=0.8, las=2)
union12$modelo <- with(union12,reorder(modelo,tasa, mean))
boxplot(data=union12,tasa~modelo,col="pink",main="TASA FALLOS")
union12$modelo <- with(union12,reorder(modelo,auc, mean))
boxplot(data=union12,auc~modelo,col="pink",main="AUC")
#mejor modelo individual RF


################################################################################
# ***************************
#  ENSEMBLADOS
# ***************************
################################################################################


#Confirguramos el directorio de trabajo
setwd("C:/Users/A663876/Desktop/UCM Máster/8. Machine learning con R/CIDDS")
source ("funciones utiles.R")
load("data.Rda")

library(caret)
library(caretEnsemble)

# PARÁMETROS EXTRAÍDOS DE TUNING CON CARET de ejemplos anteriores
# A veces hay que cambiar el nombre de los parámetros
# Los de grid hay que ponerlos en un grid

# ************************************************************
# Estas tres líneas son importantes, sustituir por el nombre
# de variable y archivo
# ************************************************************

formula1<-as.formula(paste("factor(","class",")","~."))
data$class<-as.factor(data$class)
levels(data$class) <- make.names(levels(factor(data$class)))

# Aquí se fijan el número de repeticiones de validación cruzada 
# y la semilla
set.seed(3005)
repeticiones=5


# Manera de evaluar los modelos
stackControl <- trainControl(method="repeatedcv", 
                             number=4, repeats=repeticiones, savePredictions=TRUE, classProbs=TRUE)

# Parámetros para caret, tunear antes

# Muy importante considerar esto:

# Cada method (algoritmo) tiene parámetros a tunear con Grid
# y parámetros específicos que no se pueden tunear.
# --Los que se pueden tunear hay que ponerlos en un Grid aunque 
# solo se les de un valor (ver por ejemplo gbmGrid).
# --Los que no se pueden tunear hay que nombrarlos directamente
# en train (ver por ejemplo rf)

#variables de la regresión logística
#c("Flags..A..SF", "Flags..AP.SF", "Bytes", "Flags..A....", "Duration", "Flags.....S.", "Flags..A.R..", "Proto.ICMP", "Tos")

#model_avnnet size = 15, maxit=100 y decay= 0.001 
gbmGrid <- expand.grid(n.trees = c(3000),
                       interaction.depth = c(2), shrinkage =c(0.1), n.minobsinnode = c(20))
xgbmGrid<-expand.grid(eta=c(0.1),
                      min_child_weight=c(5),
                      nrounds=c(3000),
                      max_depth=6,gamma=0,colsample_bytree=1,subsample=1)
rfGrid <- expand.grid(mtry=c(3))
svmlinGrid <- expand.grid(C=c(100))
knnGrid <- expand.grid(k=c(1))

set.seed(3005)
models <- caretList(class~Tos + Flags + Duration + Bytes_speed + SrcPt + 
                      Packets + Hour + Packets_speed, 
                    data=data, trControl=stackControl,tuneList=list(
                      parrf=caretModelSpec(method="rf",linout = FALSE,ntree=150,nodesize=10,replace=TRUE,
                                           sampsize=7000,importance=TRUE,tuneGrid=rfGrid), 
                      glm=caretModelSpec(method="glm"),
                      gbm=caretModelSpec(method="gbm",tuneGrid=gbmGrid),
                      xgbm=caretModelSpec(method="xgbTree",tuneGrid=xgbmGrid),
                      svmlinear=caretModelSpec(method="svmLinear",tuneGrid=svmlinGrid),
                      knn=caretModelSpec(method="knn",tuneGrid=knnGrid)
                    ))

saveRDS(models, "./models.rds")
models <- readRDS("./models.rds")
results <- resamples(models)
summary(results)
dotplot(results)

modelCor(results)
splom(results)
results[[2]]

ense <- caretEnsemble(models)

# Aquí se recomiendan los pesos para el ensamblado 
# de todos los modelos y se ve la tasa de aciertos
# de cada modelo y ensamblado
summary(ense)


# PRUEBAS DE ENSAMBLADO


# **************************************
# IMPORTANTE: AQUÍ HAY QUE DECIDIR ANTES LOS PARÁMETROS A UTILIZAR
# EN CADA ALGORITMO, NO VALE GRID
# Importante, la dependiente en letras Yes, No
# Preparación de archivo, variables y CV. 
# Esto se cambia para cada archivo.
# Necesario haber cambiado la var dep a Yes,No.
# **************************************

# LEER LAS CRUZADAS DE ENSAMBLADO, SON LIGERAMENTE DIFERENTES
# A LAS UTILIZADAS ANTERIORMENTE AUNQUE SE LLAMAN IGUAL


#leemos los modelos guardados
medias1 <- readRDS("./model1_logistica.rds")
medias2 <- readRDS("./model2_logistica.rds")
medias3 <- readRDS("./model3_logistica.rds")
medias4 <- readRDS("./model4_logistica.rds")
medias5 <- readRDS("./model1_nnet.rds")
medias6 <- readRDS("./model2_avnnet.rds")
medias7 <- readRDS("./model3_avnnet.rds")
medias8 <- readRDS("./model4_avnnet.rds")
medias9 <- readRDS("./model_rf.rds")
medias10 <- readRDS("./model_gbm.rds")
medias11 <- readRDS("./model_gbm1.rds")
medias12 <- readRDS("./model_gbm2.rds")
medias13 <- readRDS("./model_gbm3.rds")
medias14 <- readRDS("./model_xgbm.rds")
medias15 <- readRDS("./model_xgbm2.rds")
medias16 <- readRDS("./model_xgbm3.rds")
medias17 <- readRDS("./model_svm.rds")

source("cruzadas ensamblado binaria fuente.R")

load ("data.Rda")
#data$class <- ifelse(data$class==1, "Yes","No")
#data$class <- as.character(data$class)
dput(names(data))
set.seed(12345)

archivo<-data


vardep<-"class"
listconti<-c("Tos", "Flags", "Duration", "Bytes_speed", "SrcPt",
             "Packets", "Hour", "Packets_speed")
listclass<-c("")
grupos<-4
sinicio<-1234
repe<-10


# APLICACIÓN CRUZADAS PARA ENSAMBLAR

medias_1<-cruzadalogistica(data=archivo,
                           vardep=vardep,listconti=listconti,
                           listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe)
saveRDS(medias_1, "./medias_1.rds")
#medias_1 <- readRDS("./medias_1.rds")
medias1bis<-as.data.frame(medias_1[1])
medias1bis$modelo<-"Logistica"
predi1<-as.data.frame(medias_1[2])
predi1$logi<-predi1$Yes
predi1$pred<-as.factor(predi1$pred)
predi1$obs<-as.factor(predi1$obs)
salconfu1<-confusionMatrix(predi1$pred,predi1$obs)
salconfu1
#Accuracy : 0.964, Kappa : 0.8258
#Prediction    No    Yes
#       No  90722  2003
#      Yes   1768 10367

medias_2<-cruzadaavnnetbin(data=archivo,
                           vardep=vardep,listconti=listconti,
                           listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                           size=c(15),decay=c(0.01),repeticiones=5,itera=100)
saveRDS(medias_2, "./medias_2.rds")
#medias_2 <- readRDS("./medias_2.rds")

medias2bis<-as.data.frame(medias_2[1])
medias2bis$modelo<-"avnnet"
predi2<-as.data.frame(medias_2[2])
predi2$avnnet<-predi2$Yes
predi2$pred<-as.factor(predi2$pred)
predi2$obs<-as.factor(predi2$obs)
salconfu2<-confusionMatrix(predi2$pred,predi2$obs)
salconfu2
# size decay   bag  Accuracy     Kappa AccuracySD    KappaSD
# 1   15  0.01 FALSE 0.9898723 0.9496654 0.00294175 0.01503916
#Prediction    No   Yes
#       No  12323    81
#       Yes    47 12289


medias_3<-cruzadarfbin(data=archivo,
                       vardep=vardep,listconti=listconti,
                       listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                       mtry=3,ntree=150,nodesize=10,replace=TRUE)
saveRDS(medias_3, "./medias_3.rds")
#medias_3 <- readRDS("./medias_3.rds")
medias3bis<-as.data.frame(medias_3[1])
medias3bis$modelo<-"rf"
predi3<-as.data.frame(medias_3[2])
predi3$rf<-predi3$Yes
predi3$pred<-as.factor(predi3$pred)
predi3$obs<-as.factor(predi3$obs)
salconfu3<-confusionMatrix(predi3$pred,predi3$obs)
salconfu3
# mtry  Accuracy     Kappa  AccuracySD     KappaSD
# 1    3 0.9972249 0.9865186 0.001082362 0.005313481

#Prediction    No   Yes
#       No  12325    20
#       Yes    45 12350


medias_4<-cruzadagbmbin(data=archivo,
                        vardep=vardep,listconti=listconti,
                        listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                        n.minobsinnode=20,shrinkage=0.1,n.trees=3000,interaction.depth=2)

saveRDS(medias_4, "./medias_4.rds")
#medias_4 <- readRDS("./medias_4.rds")
medias4bis<-as.data.frame(medias_4[1])
medias4bis$modelo<-"gbm"
predi4<-as.data.frame(medias_4[2])
predi4$gbm<-predi4$Yes
predi4$pred<-as.factor(predi4$pred)
predi4$obs<-as.factor(predi4$obs)
salconfu4<-confusionMatrix(predi4$pred,predi4$obs)
salconfu4
# n.minobsinnode shrinkage n.trees interaction.depth  Accuracy     Kappa  AccuracySD     KappaSD
# 1             20       0.1    3000                 2 0.9984264 0.9924114 0.000708139 0.003429995

#Prediction    No   Yes
#       No  12309    44
#      Yes    61 12326

medias_5<-cruzadaxgbmbin(data=archivo,
                         vardep=vardep,listconti=listconti,
                         listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                         min_child_weight=5,eta=0.1,nrounds=3000,max_depth=6,
                         gamma=0,colsample_bytree=1,subsample=1,
                         alpha=0,lambda=0,lambda_bias=0)
saveRDS(medias_5, "./medias_5.rds")
#medias_5 <- readRDS("./medias_5.rds")

medias5bis<-as.data.frame(medias_5[1])
medias5bis$modelo<-"xgbm"
predi5<-as.data.frame(medias_5[2])
predi5$xgbm<-predi5$Yes
predi5$pred<-as.factor(predi5$pred)
predi5$obs<-as.factor(predi5$obs)
salconfu5<-confusionMatrix(predi5$pred,predi5$obs)
salconfu5

# min_child_weight eta nrounds max_depth gamma colsample_bytree subsample  Accuracy     Kappa    AccuracySD     KappaSD
#               5 0.1    3000         6     0                1         1 0.9980259 0.9904489 0.0009419836 0.004582125

#Prediction    No   Yes
#       No  12325    58
#       Yes    45 12312


medias_6<-cruzadaSVMbin(data=archivo,
                        vardep=vardep,listconti=listconti,
                        listclass=listclass,grupos=grupos,
                        sinicio=sinicio,repe=repe,C=100)
saveRDS(medias_6, "./medias_6.rds")
#medias_6 <- readRDS("./medias_6.rds")
medias6bis<-as.data.frame(medias_6[1])
medias6bis$modelo<-"svmLinear"
predi6<-as.data.frame(medias_6[2])
predi6$svmLinear<-predi6$Yes
predi6$pred<-as.factor(predi6$pred)
predi6$obs<-as.factor(predi6$obs)
salconfu6<-confusionMatrix(predi6$pred,predi6$obs)
salconfu6
# C  Accuracy     Kappa  AccuracySD    KappaSD
# 1 100 0.9647912 0.8286771 0.003498676 0.01769053

#Prediction    No   Yes
#       No  12224    50
#       Yes   146 12320

medias_7<-cruzadaKNNbin(data=archivo,
                        vardep=vardep,listconti=listconti,
                        listclass=listclass,grupos=grupos,
                        sinicio=sinicio,repe=repe,k=1)
saveRDS(medias_7, "./medias_7.rds")
#medias_7 <- readRDS("./medias_7.rds")
medias7bis<-as.data.frame(medias_7[1])
medias7bis$modelo<-"knn"
predi7<-as.data.frame(medias_7[2])
predi7$knn<-predi7$Yes
predi7$pred<-as.factor(predi7$pred)
predi7$obs<-as.factor(predi7$obs)
salconfu7<-confusionMatrix(predi7$pred,predi7$obs)
salconfu7

# k  Accuracy     Kappa  AccuracySD     KappaSD
# 1 1 0.9959947 0.9807707 0.001061132 0.005092724

medias_8<-cruzadaNBbin(data=archivo,
                       vardep=vardep,listconti=listconti,
                       listclass=listclass,grupos=grupos,
                       sinicio=sinicio,repe=repe,usekernel=TRUE,fL=0,adjust=1)
saveRDS(medias_8, "./medias_8.rds")
#medias_8 <- readRDS("./medias_8.rds")
medias8bis<-as.data.frame(medias_8[1])
medias8bis$modelo<-"NB"
predi8<-as.data.frame(medias_8[2])
predi8$NB<-predi8$Yes
predi8$pred<-as.factor(predi8$pred)
predi8$obs<-as.factor(predi8$obs)
salconfu8<-confusionMatrix(predi8$pred,predi8$obs)
salconfu8


union1<-rbind(medias1bis,medias2bis,
              medias3bis,medias4bis,medias5bis,medias6bis,medias7bis,medias8bis)
par(cex.axis=0.8, las=2)
union1$modelo <- with(union1,reorder(modelo,tasa, mean))
boxplot(data=union1,tasa~modelo,col="pink",main="TASA FALLOS")
union1$modelo <- with(union1,reorder(modelo,auc, mean))
boxplot(data=union1,auc~modelo,col="pink",main="AUC")


# CONSTRUCCIÓN DE TODOS LOS ENSAMBLADOS
# SE UTILIZARÁN LOS ARCHIVOS SURGIDOS DE LAS FUNCIONES LLAMADOS predi1,...
unipredi<-cbind(predi1,predi2,predi3,predi4,predi5,predi6,predi7,predi8)

# Esto es para eliminar columnas duplicadas
unipredi<- unipredi[, !duplicated(colnames(unipredi))]

# Construccion de ensamblados, cambiar al gusto

#unipredi$predi8<-(unipredi$logi+unipredi$avnnet)/2
unipredi$predi9<-(unipredi$logi+unipredi$rf)/2
unipredi$predi10<-(unipredi$logi+unipredi$gbm)/2
unipredi$predi11<-(unipredi$logi+unipredi$xgbm)/2
unipredi$predi12<-(unipredi$logi+unipredi$svmLinear)/2
unipredi$predi13<-(unipredi$logi+unipredi$knn)/2
unipredi$predi14<-(unipredi$avnnet+unipredi$rf)/2
unipredi$predi15<-(unipredi$avnnet+unipredi$gbm)/2
unipredi$predi16<-(unipredi$avnnet+unipredi$xgbm)/2
unipredi$predi17<-(unipredi$avnnet+unipredi$svmLinear)/2
unipredi$predi18<-(unipredi$avnnet+unipredi$knn)/2
unipredi$predi19<-(unipredi$rf+unipredi$gbm)/2
unipredi$predi20<-(unipredi$rf+unipredi$xgbm)/2
unipredi$predi21<-(unipredi$rf+unipredi$svmLinear)/2
unipredi$predi22<-(unipredi$rf+unipredi$knn)/2
unipredi$predi23<-(unipredi$gbm+unipredi$xgbm)/2
unipredi$predi24<-(unipredi$gbm+unipredi$svmLinear)/2
unipredi$predi25<-(unipredi$gbm+unipredi$knn)/2
unipredi$predi26<-(unipredi$xgbm+unipredi$svmLinear)/2
unipredi$predi27<-(unipredi$xgbm+unipredi$knn)/2
unipredi$predi28<-(unipredi$svmLinear+unipredi$knn)/2

unipredi$predi29<-(unipredi$logi+unipredi$avnnet+unipredi$rf)/3
unipredi$predi30<-(unipredi$logi+unipredi$avnnet+unipredi$gbm)/3
unipredi$predi31<-(unipredi$logi+unipredi$avnnet+unipredi$xgbm)/3
unipredi$predi32<-(unipredi$logi+unipredi$avnnet+unipredi$svmLinear)/3
unipredi$predi33<-(unipredi$logi+unipredi$avnnet+unipredi$knn)/3
unipredi$predi34<-(unipredi$logi+unipredi$rf+unipredi$gbm)/3
unipredi$predi35<-(unipredi$logi+unipredi$rf+unipredi$xgbm)/3
unipredi$predi36<-(unipredi$logi+unipredi$rf+unipredi$svmLinear)/3
unipredi$predi37<-(unipredi$logi+unipredi$rf+unipredi$knn)/3
unipredi$predi38<-(unipredi$logi+unipredi$gbm+unipredi$xgbm)/3
unipredi$predi39<-(unipredi$logi+unipredi$gbm+unipredi$svmLinear)/3
unipredi$predi40<-(unipredi$logi+unipredi$gbm+unipredi$knn)/3
unipredi$predi41<-(unipredi$logi+unipredi$xgbm+unipredi$svmLinear)/3
unipredi$predi42<-(unipredi$logi+unipredi$xgbm+unipredi$knn)/3
unipredi$predi43<-(unipredi$logi+unipredi$svmLinear+unipredi$knn)/3

unipredi$predi44<-(unipredi$rf+unipredi$avnnet+unipredi$gbm)/3
unipredi$predi45<-(unipredi$rf+unipredi$avnnet+unipredi$xgbm)/3
unipredi$predi46<-(unipredi$rf+unipredi$avnnet+unipredi$svmLinear)/3
unipredi$predi47<-(unipredi$rf+unipredi$avnnet+unipredi$knn)/3
unipredi$predi48<-(unipredi$rf+unipredi$gbm+unipredi$xgbm)/3
unipredi$predi49<-(unipredi$rf+unipredi$gbm+unipredi$svmLinear)/3
unipredi$predi50<-(unipredi$rf+unipredi$gbm+unipredi$knn)/3
unipredi$predi51<-(unipredi$rf+unipredi$xgbm+unipredi$svmLinear)/3
unipredi$predi52<-(unipredi$rf+unipredi$xgbm+unipredi$knn)/3
unipredi$predi53<-(unipredi$rf+unipredi$svmLinear+unipredi$knn)/3

unipredi$predi54<-(unipredi$avnnet+unipredi$gbm+unipredi$svmLinear)/3
unipredi$predi55<-(unipredi$avnnet+unipredi$gbm+unipredi$xgbm)/3
unipredi$predi56<-(unipredi$avnnet+unipredi$gbm+unipredi$knn)/3
unipredi$predi57<-(unipredi$avnnet+unipredi$xgbm+unipredi$knn)/3
unipredi$predi58<-(unipredi$avnnet+unipredi$xgbm+unipredi$svmLinear)/3
unipredi$predi59<-(unipredi$avnnet+unipredi$svmLinear+unipredi$knn)/3

unipredi$predi60<-(unipredi$gbm+unipredi$xgbm+unipredi$svmLinear)/3
unipredi$predi61<-(unipredi$gbm+unipredi$xgbm+unipredi$knn)/3
unipredi$predi62<-(unipredi$gbm+unipredi$svmLinear+unipredi$knn)/3
unipredi$predi63<-(unipredi$xgbm+unipredi$svmLinear+unipredi$knn)/3

unipredi$predi64<-(unipredi$logi+unipredi$avnnet+unipredi$rf+unipredi$gbm)/4
unipredi$predi65<-(unipredi$logi+unipredi$avnnet+unipredi$rf+unipredi$xgbm)/4
unipredi$predi66<-(unipredi$logi+unipredi$avnnet+unipredi$rf+unipredi$svmLinear)/4
unipredi$predi67<-(unipredi$logi+unipredi$avnnet+unipredi$rf+unipredi$knn)/4
unipredi$predi68<-(unipredi$logi+unipredi$avnnet+unipredi$gbm+unipredi$xgbm)/4
unipredi$predi69<-(unipredi$logi+unipredi$avnnet+unipredi$gbm+unipredi$svmLinear)/4
unipredi$predi70<-(unipredi$logi+unipredi$avnnet+unipredi$gbm+unipredi$knn)/4
unipredi$predi71<-(unipredi$logi+unipredi$avnnet+unipredi$xgbm+unipredi$svmLinear)/4
unipredi$predi72<-(unipredi$logi+unipredi$avnnet+unipredi$xgbm+unipredi$knn)/4
unipredi$predi73<-(unipredi$logi+unipredi$avnnet+unipredi$svmLinear+unipredi$knn)/4
unipredi$predi74<-(unipredi$logi+unipredi$rf+unipredi$gbm+unipredi$xgbm)/4
unipredi$predi75<-(unipredi$logi+unipredi$rf+unipredi$gbm+unipredi$svmLinear)/4
unipredi$predi76<-(unipredi$logi+unipredi$rf+unipredi$gbm+unipredi$knn)/4
unipredi$predi77<-(unipredi$logi+unipredi$rf+unipredi$xgbm+unipredi$svmLinear)/4
unipredi$predi78<-(unipredi$logi+unipredi$rf+unipredi$xgbm+unipredi$knn)/4
unipredi$predi79<-(unipredi$logi+unipredi$gbm+unipredi$xgbm+unipredi$svmLinear)/4
unipredi$predi80<-(unipredi$logi+unipredi$gbm+unipredi$xgbm+unipredi$knn)/4
unipredi$predi81<-(unipredi$logi+unipredi$xgbm+unipredi$svmLinear+unipredi$knn)/4
unipredi$predi82<-(unipredi$avnnet+unipredi$rf+unipredi$gbm+unipredi$xgbm)/4
unipredi$predi83<-(unipredi$avnnet+unipredi$rf+unipredi$gbm+unipredi$svmLinear)/4
unipredi$predi84<-(unipredi$avnnet+unipredi$rf+unipredi$gbm+unipredi$knn)/4

# Listado de modelos a considerar, cambiar al gusto

dput(names(unipredi))

listado<-c("logi", "avnnet", "rf", "gbm", "xgbm", "svmLinear", "knn", 
           "NB", "predi9", "predi10", "predi11", "predi12", "predi13", 
           "predi14", "predi15", "predi16", "predi17", "predi18", "predi19", 
           "predi20", "predi21", "predi22", "predi23", "predi24", "predi25", 
           "predi26", "predi27", "predi28", "predi29", "predi30", "predi31", 
           "predi32", "predi33", "predi34", "predi35", "predi36", "predi37", 
           "predi38", "predi39", "predi40", "predi41", "predi42", "predi43", 
           "predi44", "predi45", "predi46", "predi47", "predi48", "predi49", 
           "predi50", "predi51", "predi52", "predi53", "predi54", "predi55", 
           "predi56", "predi57", "predi58", "predi59", "predi60", "predi61", 
           "predi62", "predi63", "predi64", "predi65", "predi66", "predi67", 
           "predi68", "predi69", "predi70", "predi71", "predi72", "predi73", 
           "predi74", "predi75", "predi76", "predi77", "predi78", "predi79", 
           "predi80", "predi81", "predi82", "predi83", "predi84")

# Cambio a Yes, No, todas las predicciones

# Defino funcion tasafallos

tasafallos<-function(x,y) {
  confu<-confusionMatrix(x,y)
  tasa<-confu[[3]][1]
  return(tasa)
}

auc<-function(x,y) {
  curvaroc<-roc(response=x,predictor=y)
  auc<-curvaroc$auc
  return(auc)
}

# Se obtiene el numero de repeticiones CV y se calculan las medias por repe en
# el data frame medias0

repeticiones<-nlevels(factor(unipredi$Rep))
unipredi$Rep<-as.factor(unipredi$Rep)
unipredi$Rep<-as.numeric(unipredi$Rep)


medias0<-data.frame(c())
for (prediccion in listado)
{
  unipredi$proba<-unipredi[,prediccion]
  unipredi[,prediccion]<-ifelse(unipredi[,prediccion]>0.5,"Yes","No")
  for (repe in 1:repeticiones)
  {
    paso <- unipredi[(unipredi$Rep==repe),]
    pre<-factor(paso[,prediccion])
    archi<-paso[,c("proba","obs")]
    archi<-archi[order(archi$proba),]
    obs<-paso[,c("obs")]
    tasa=1-tasafallos(pre,obs)
    t<-as.data.frame(tasa)
    t$modelo<-prediccion
    auc<-suppressMessages(auc(archi$obs,archi$proba))
    t$auc<-auc
    medias0<-rbind(medias0,t)
  }
}

# PRESENTACION TABLA MEDIAS

library(dplyr)
tablamedias<-medias0 %>%
  group_by(modelo) %>%
  summarise(tasa=mean(tasa))       

tablamedias<-as.data.frame(tablamedias[order(tablamedias$tasa),])


# ORDENACIÓN DEL FACTOR MODELO POR LAS MEDIAS EN TASA
# PARA EL GRAFICO

medias0$modelo <- with(medias0,
                       reorder(modelo,tasa, mean))
par(cex.axis=0.3,las=2)
boxplot(data=medias0,tasa~modelo,col="pink", main='TASA FALLOS')
medias0$modelo <- with(medias0,
                       reorder(modelo,auc, mean))
par(cex.axis=0.35,las=2)
boxplot(data=medias0,auc~modelo,col="pink", main='AUC')

# ************************************
# PARA AUC
# ************************************

# PRESENTACION TABLA MEDIAS

tablamedias2<-medias0 %>%
  group_by(modelo) %>%
  summarise(auc=mean(auc))     

tablamedias2<-tablamedias2[order(-tablamedias2$auc),]


# ORDENACIÓN DEL FACTOR MODELO POR LAS MEDIAS EN AUC
# PARA EL GRAFICO

medias0$modelo <- with(medias0,
                       reorder(modelo,auc, mean))
par(cex.axis=0.5,las=2)
boxplot(data=medias0,auc~modelo,col="pink", main='AUC')



# Se pueden escoger listas pero el factor hay que pasarlo a character
# para que no salgan en el boxplot todos los niveles del factor

listadobis<-c("logi", "avnnet", "rf","gbm",  "xgbm", "svmLinear", "knn",
              "predi48", "predi52", "predi20", "predi19", "predi61", "predi23", "predi50") 

medias0$modelo<-as.character(medias0$modelo)
mediasver<-medias0[medias0$modelo %in% listadobis,]
mediasver$modelo <- with(mediasver,
                         reorder(modelo,auc, median))
par(cex.axis=0.9,las=2)
boxplot(data=mediasver,auc~modelo,col="pink",main='AUC')

medias0$modelo<-as.character(medias0$modelo)
mediasver<-medias0[medias0$modelo %in% listadobis,]
mediasver$modelo <- with(mediasver,
                         reorder(modelo,tasa, median))
par(cex.axis=0.9,las=2)
boxplot(data=mediasver,tasa~modelo,col="pink",main='TASA FALLOS')



################################################################################
# ***************************
# Evaluamos los modelos creados con el dataset entero
# ***************************
################################################################################


install.packages("InformationValue") 
#install.packages("ISLR")
library(caret)
library(InformationValue)
library(ISLR)
models<-readRDS("./models.rds")
load("test_sem1.Rda")
datIn_test<-test_sem1
test_sem1<-NULL

logistica <- readRDS("./logistica.rds")
start_time <- Sys.time()
pred_logi <- predict(logistica, datIn_test)
end_time <- Sys.time()
time<- end_time - start_time
time
nuevos_datos <-cbind(datIn_test$class,pred_logi)
nuevos_datos<-as.data.frame(nuevos_datos)
names(nuevos_datos)[1] <- "class"
#nuevos_datos$class <- ifelse(nuevos_datos$class=="Yes", 1, 0)
nuevos_datos$pred_logi <- ifelse(nuevos_datos$pred_logi=="2", 1, 0)
mc <- confusionMatrix(nuevos_datos$class, nuevos_datos$pred_logi)
mc

neuralnet <- readRDS("./neuralnet.rds")
start_time <- Sys.time()
pred_avnnet <- predict(neuralnet, datIn_test)
end_time <- Sys.time()
time<- end_time - start_time
time
nuevos_datos <-cbind(nuevos_datos,pred_avnnet)
nuevos_datos$pred_avnnet <- ifelse(nuevos_datos$pred_avnnet=="Yes", 1, 0)
mc2 <- confusionMatrix(nuevos_datos$class, nuevos_datos$pred_avnnet)
mc2

randomforest <- readRDS("./randomforest.rds")
start_time <- Sys.time()
pred_rf1 <- predict(randomforest, datIn_test)
end_time <- Sys.time()
time<- end_time - start_time
time
nuevos_datos <-cbind(nuevos_datos,pred_rf1)
nuevos_datos$pred_rf1 <- ifelse(nuevos_datos$pred_rf1=="Yes", 1, 0)
mc3 <- confusionMatrix(nuevos_datos$class, nuevos_datos$pred_rf1)
mc3


gradientb <- readRDS("./gradientb.rds")
start_time <- Sys.time()
pred_gbm1 <- predict(gradientb, datIn_test)
end_time <- Sys.time()
time<- end_time - start_time
time
nuevos_datos <-cbind(nuevos_datos,pred_gbm1)
nuevos_datos$pred_gbm1 <- ifelse(nuevos_datos$pred_gbm1=="Yes", 1, 0)
mc4 <- confusionMatrix(nuevos_datos$class, nuevos_datos$pred_gbm1)
mc4


xgbm <- readRDS("./xgbm.rds")
start_time <- Sys.time()
pred_xgbm <- predict(xgbm, datIn_test)
end_time <- Sys.time()
time<- end_time - start_time
time
nuevos_datos <-cbind(nuevos_datos,pred_xgbm)
nuevos_datos$pred_xgbm <- ifelse(nuevos_datos$pred_xgbm=="Yes", 1, 0)
mc5 <- confusionMatrix(nuevos_datos$class, nuevos_datos$pred_xgbm)
mc5


svm<-readRDS("./svm.rds")
start_time <- Sys.time()
pred_svm1 <- predict(svm, datIn_test)
end_time <- Sys.time()
time<- end_time - start_time
time
nuevos_datos <-cbind(nuevos_datos,pred_svm1)
nuevos_datos$pred_svm1 <- ifelse(nuevos_datos$pred_svm1=="Yes", 1, 0)
mc6 <- confusionMatrix(nuevos_datos$class, nuevos_datos$pred_svm1)
mc6


KNN<-readRDS("./knn.rds")
start_time <- Sys.time()
pred_knn <- predict(KNN, datIn_test)
end_time <- Sys.time()
time<- end_time - start_time
time
nuevos_datos <-cbind(nuevos_datos,pred_knn)
nuevos_datos$pred_knn <- ifelse(nuevos_datos$pred_knn=="Yes", 1, 0)
mc7 <- confusionMatrix(nuevos_datos$class, nuevos_datos$pred_knn)
mc7


NB <- readRDS("./NB.rds")
start_time <- Sys.time()
pred_nb <- predict(NB, datIn_test)
end_time <- Sys.time()
time<- end_time - start_time
time
nuevos_datos <-cbind(nuevos_datos,pred_nb)
nuevos_datos$pred_nb <- ifelse(nuevos_datos$pred_nb=="Yes", 1, 0)
mc8 <- confusionMatrix(nuevos_datos$class, nuevos_datos$pred_nb)
mc8


################################################################################
# ***************************
# verficamos con el dataset de las semanas 2, 3 y 4
# ***************************
################################################################################

setwd("C:/Users/A663876/Desktop/UCM Máster/8. Machine learning con R/CIDDS")
source ("funciones utiles.R")

# Datataset external servers
test1 <- fread(file = 'CIDDS-001-internal-week2.csv',header=T)

#Limpiamos y preparamos el dataset
test1$attackType <- NULL #no es util porque es una clasificación binaria
test1$attackID <- NULL #no es util porque es una clasificación binaria
test1$attackDescription <- NULL #no es util porque es una clasificación binaria
test1$Flows <- NULL #todas las observaciones son iguales
test1$`Src IP Addr` <-NULL #de momento no lo tomaremos en cuenta por ser una categoría con mucha cardinalidad 
test1$`Dst IP Addr` <- NULL
test1$class <- ifelse(datIn$class =="normal", 0, 1)

#Transformamos Bytes con valores e.g., "8.3 M" a numérica 
test1$Bytes <- gsub("M", "000000", test1$Bytes)
test1$Bytes <- gsub("[[:blank:]]", "", test1$Bytes)
test1$Bytes <- gsub("\\.", "", test1$Bytes)
test1$Bytes <- as.numeric(test1$Bytes)

# Renombramos algunas variables 
names(test1)[1] <- "Date"
names(test1)[4] <- "SrcPt"
names(test1)[5] <- "DstPt"

#Feature engineering
test1$Packets_speed <- ifelse(test1$Duration >0.000, test1$Packets/test1$Duration, test1$Packets/0.0001)
test1$Bytes_speed <- ifelse(test1$Duration >0.000, test1$Bytes/test1$Duration, test1$Bytes/0.0001)

library('lubridate')
datIn$Hour<- hour(datIn$Date)
datIn$Hour<-format(as.POSIXct(datIn$Date), format="%H")
datIn$Hour <- as.numeric(datIn$Hour)
datIn$Min<- minute(datIn$Date)
datIn$Min<-format(as.POSIXct(datIn$Date), format="%M")
datIn$Min <- as.numeric(datIn$Minute)
datIn$Sec<- second(datIn$Date)
datIn$Sec<-format(as.POSIXct(datIn$Date), format="%S")
datIn$Sec <- as.numeric(datIn$Second)

#Transformamos Proto y Flags en numéricas
test1$Proto<- with(test1, prot$Decimal[match(Proto, prot$Keyword)])
test1$Flags<- with(test1, flags$Num[match(Flags, flags$TCPFlag)])

#Estandarización de variables
datIn2<-test1
class<-datIn2[,"class"]
means <- readRDS("./means.rds")
sds <- readRDS("./sds.rds")
datIn3<-scale(datIn2[,c("Duration", "Proto", "SrcPt", "DstPt", "Packets", "Bytes", 
                        "Flags", "Tos", "Packets_speed", "Bytes_speed", "Hour", 
                        "Min", "Sec")], center = means, scale = sds) #escalamos cada vector

test_dataset<-data.frame(cbind(datIn3, class)) #dataframe ya transformado, con variables estandarizadas, media cero y desviación típica 1
save(test_dataset,file="test_dataset.Rda")