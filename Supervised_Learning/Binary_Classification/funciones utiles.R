steprepetido<- function(data=data,vardep="vardep",
  listconti="listconti",sinicio=12345,sfinal=12355,porcen=0.8,criterio="AIC")
 {

library(MASS)
library(dplyr)

resultados<-data.frame(c())
data<-data[,c(listconti,vardep)]
formu1<-formula(paste(vardep,"~.",sep=""))
formu2<-formula(paste(vardep,"~1",sep=""))
listamodelos<-list()

for (semilla in sinicio:sfinal)
{

set.seed(semilla)
sample <- sample.int(n = nrow(data),
 size = floor(porcen*nrow(data)), replace = F)

train <- data[sample, ]
test  <- data[-sample, ]


full<-lm(formu1,data=train)
null<-lm(formu2,data=train)


if  (criterio=='AIC')
  {
  selec1<-stepAIC(null,scope=list(upper=full),direction="both",trace=FALSE)
  } 
else   if  (criterio=='BIC')
  {
 k1=log(nrow(train))
 selec1<-stepAIC(null,scope=list(upper=full),direction="both",k=k1,trace=FALSE)
  }

vec<-(names(selec1[[1]]))


# CAMBIOS

cosa<-as.data.frame(table(vec))
cosa<-as.data.frame(t(cosa))
colnames(cosa)<-vec

# 1) creo un vector con todas las variables input y ceros
# 2) voy añadiendo

cosa<-cosa[2,]
cosa<-cosa[,-c(1)]
cosa<- data.frame(lapply(cosa, function(x) as.numeric(as.character(x))))
cosa$id<-semilla

vectormodelo<-list(names(cosa),semilla)
listamodelos<-append(listamodelos,vectormodelo)  

if (semilla==sinicio)
{
listamod<-cosa
}

else if (semilla!=sinicio)
{
 listamod<-suppressMessages(full_join(cosa,listamod,by = NULL, copy =TRUE))
}

}

listamod[is.na(listamod)] <- 0

nom<-names(listamod)
listamod$modelo<-""
for (i in 1:nrow(listamod))
{
 listamod[i,c("modelo")]<-""
 listamod[i,c("contador")]=0

  for (vari in nom)
  { 
   if (listamod[i,vari]==1)
   {
   listamod[i,c("modelo")]<-paste(listamod[i,c("modelo")],vari,collapse="",sep="+")
   listamod[i,c("contador")]=listamod[i,c("contador")]+1
   }
  
   }

}
 
listamod$modelo<-substring(listamod$modelo, 2)

tablamod<-as.data.frame(table(listamod$modelo))
names(tablamod)<-c("modelo","Freq")

tablamod<-tablamod[order(-tablamod$Freq,tablamod$modelo),]

nuevo<-listamod[,c("modelo","id","contador")]

uni<-full_join(tablamod,nuevo,by ="modelo", copy =TRUE)

uni= uni[!duplicated(uni$modelo),]
uni$semilla<-semilla

li1<-list()
# str(listamodelos)
for (i in 1:nrow(uni))
{
 for (j in 1:length(listamodelos))
 {
    if (uni[i,c("id")]==listamodelos[j][[1]])
  {
   k<-as.vector(listamodelos[j-1][[1]])
   length(k)<-length(k)-1
   li1<-c(li1,list(k))
   j=length(listamodelos)
  }
 } 

}

 uni$semilla<-NULL
 uni$id<-NULL
 return(list(uni,li1))

}


steprepetidobinaria<- function(data=data,vardep="vardep",
                               listconti="listconti",
                               sinicio=12345,sfinal=12355,porcen=0.8,criterio="BIC")
{
  
  library(MASS)
  library(dplyr)
  
  resultados<-data.frame(c())
  data<-data[,c(listconti,vardep)]
  formu1<-formula(paste("factor(",vardep,")~.",sep=""))
  formu2<-formula(paste("factor(",vardep,")~1",sep=""))
  listamodelos<-list()
  
  for (semilla in sinicio:sfinal)
  {
    set.seed(semilla)
    sample <- sample.int(n = nrow(data),
                         size = floor(porcen*nrow(data)), replace = F)
    
    train <- data[sample, ]
    test  <- data[-sample, ]
    
    
    full<-glm(formu1,data=train,family = binomial(link="logit"))
    null<-glm(formu2,data=train,family = binomial(link="logit"))
    
    
    if  (criterio=='AIC')
    {
      selec1<-stepAIC(null,scope=list(upper=full),
                      direction="both",family = binomial(link="logit"),trace=FALSE)
    } 
    else   if  (criterio=='BIC')
    {
      k1=log(nrow(train))
      selec1<-stepAIC(null,scope=list(upper=full),
                      direction="both",family = binomial(link="logit"),k=k1,trace=FALSE)
    }
    
    vec<-(names(selec1[[1]]))
    
    if (length(vec)!=1)
      
    {
      # CAMBIOS
      
      cosa<-as.data.frame(table(vec))
      cosa<-as.data.frame(t(cosa))
      colnames(cosa)<-vec
      
      # 1) creo un vector con todas las variables input y ceros
      # 2) voy añadiendo
      
      cosa<-cosa[2,,drop=FALSE]
      cosa<-cosa[,-(1),drop=FALSE]
      cosa<- data.frame(lapply(cosa, function(x) as.numeric(as.character(x))))
      cosa$id<-semilla
    }
    
    if (length(vec)==1)
    {
      cosa<-data.frame()
      cosa[1,"id"]<-semilla
      cosa$id<-as.integer(cosa$id)
    }
    vectormodelo<-list(names(cosa),semilla)
    listamodelos<-append(listamodelos,vectormodelo)  
    
    if (semilla==sinicio)
    {
      listamod<-cosa
    }
    
    else if (semilla!=sinicio)
    {
      listamod<-suppressMessages(full_join(cosa,listamod,by = NULL, copy =TRUE))
    }
    
  }
  
  listamod[is.na(listamod)] <- 0
  
  nom<-names(listamod)
  listamod$modelo<-""
  for (i in 1:nrow(listamod))
  {
    listamod[i,c("modelo")]<-""
    listamod[i,c("contador")]=0
    
    for (vari in nom)
    { 
      if (listamod[i,vari]==1)
      {
        listamod[i,c("modelo")]<-paste(listamod[i,c("modelo")],vari,collapse="",sep="+")
        listamod[i,c("contador")]=listamod[i,c("contador")]+1
      }
      
    }
    
  }
  
  listamod$modelo<-substring(listamod$modelo, 2)
  
  tablamod<-as.data.frame(table(listamod$modelo))
  names(tablamod)<-c("modelo","Freq")
  
  tablamod<-tablamod[order(-tablamod$Freq,tablamod$modelo),]
  
  nuevo<-listamod[,c("modelo","id","contador")]
  
  uni<-full_join(tablamod,nuevo,by ="modelo", copy =TRUE)
  
  uni= uni[!duplicated(uni$modelo),]
  uni$semilla<-semilla
  
  li1<-list()
  # str(listamodelos)
  for (i in 1:nrow(uni))
  {
    for (j in 1:length(listamodelos))
    {
      if (uni[i,c("id")]==listamodelos[j][[1]])
      {
        k<-as.vector(listamodelos[j-1][[1]])
        length(k)<-length(k)-1
        li1<-c(li1,list(k))
        j=length(listamodelos)
      }
    } 
    
  }
  
  uni$semilla<-NULL
  uni$id<-NULL
  return(list(uni,li1))
  
}

cruzadaavnnet<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5,
           size=c(5),decay=c(0.01),repeticiones=5,itera=100,trace=TRUE)
    
  { 
    library(caret)
    
    
    
    library(dummies)
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",
                          number=grupos,repeats=repe,
                          savePredictions = "all") 
    
    # Aplico caret y construyo modelo
    
    avnnetgrid <-  expand.grid(size=size,decay=decay,bag=FALSE)
    
    avnnet<- train(formu,data=databis,
                   method="avNNet",linout = TRUE,maxit=itera,repeats=repeticiones,
                   trControl=control,tuneGrid=avnnetgrid,trace=trace)
    
    print(avnnet$results)
    
    preditest<-avnnet$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    preditest$error<-(preditest$pred-preditest$obs)^2
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      error=mean(paso1$error)  
      medias<-rbind(medias,error)
    }
    names(medias)<-"error"
    
    
    
    return(medias)
    
  }

cruzadalin<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5)
    
  { 
    
    library(caret)
    
    
    library(dummies)
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",
                          number=grupos,repeats=repe,
                          savePredictions = "all") 
    
    # Aplico caret y construyo modelo
    
    lineal<- train(formu,data=databis,
                   method="lm",trControl=control)
    
    print(lineal$results)
    
    preditest<-lineal$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    preditest$error<-(preditest$pred-preditest$obs)^2
    
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      error=mean(paso1$error)  
      medias<-rbind(medias,error)
    }
    names(medias)<-"error"
    
    
    
    return(medias)
    
  }


cruzadalogistica <- function(data=data,vardep=NULL,
                             listconti=NULL,listclass=NULL,grupos=4,sinicio=1234,repe=5)
{
  
  if (any(listclass==c(""))==FALSE)
  {
    for (i in 1:dim(array(listclass))) {
      numindi<-which(names(data)==listclass[[i]])
      data[,numindi]<-as.character(data[,numindi])
      data[,numindi]<-as.factor(data[,numindi])
    }
  }   
  
  data[,vardep]<-as.factor(data[,vardep])
  
  # Creo la formula para la logistica
  
  if (any(listclass==c(""))==FALSE)
  {
    koko<-c(listconti,listclass)
  }  else   {
    koko<-c(listconti)
  }
  
  modelo<-paste(koko,sep="",collapse="+")
  formu<-formula(paste(vardep,"~",modelo,sep=""))
  
  formu 
  # Preparo caret   
  
  set.seed(sinicio)
  control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                        savePredictions = "all",classProbs=TRUE) 
  
  # Aplico caret y construyo modelo
  
  regresion <- train(formu,data=data,
                     trControl=control,method="glm",family = binomial(link="logit"))                  
  preditest<-regresion$pred
  
  preditest$prueba<-strsplit(preditest$Resample,"[.]")
  preditest$Fold <- sapply(preditest$prueba, "[", 1)
  preditest$Rep <- sapply(preditest$prueba, "[", 2)
  preditest$prueba<-NULL
  
  tasafallos<-function(x,y) {
    confu<-confusionMatrix(x,y)
    tasa<-confu[[3]][1]
    return(tasa)
  }
  
  # Aplicamos función sobre cada Repetición
  
  
  
  tabla<-table(preditest$Rep)
  listarep<-c(names(tabla))
  medias<-data.frame()
  for (repi in listarep) {
    paso1<-preditest[which(preditest$Rep==repi),]
    tasa=1-tasafallos(paso1$pred,paso1$obs)  
    medias<-rbind(medias,tasa)
  }
  names(medias)<-"tasa"
  
  
  # CalculamoS AUC  por cada Repetición de cv 
  # Definimnos función
  
  auc<-function(x,y) {
    curvaroc<-roc(response=x,predictor=y)
    auc<-curvaroc$auc
    return(auc)
  }
  
  # Aplicamos función sobre cada Repetición
  
  
  
  mediasbis<-data.frame()
  for (repi in listarep) {
    paso1<-preditest[which(preditest$Rep==repi),]
    auc=suppressMessages(auc(paso1$obs,paso1$Yes))
    mediasbis<-rbind(mediasbis,auc)
  }
  names(mediasbis)<-"auc"
  
  
  # Unimos la info de auc y de tasafallos
  
  medias$auc<-mediasbis$auc
  
  return(medias)
  
}




# *********************************
# CRUZADA avNNet
# **************


cruzadaavnnetbin<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",grupos=4,sinicio=1234,repe=5,
           size=c(5),decay=c(0.01),repeticiones=5,itera=100,trace=TRUE)
  { 
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    databis[,vardep]<-as.factor(databis[,vardep])
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                          savePredictions = "all",classProbs=TRUE) 
    
    # Aplico caret y construyo modelo
    
    avnnetgrid <-  expand.grid(size=size,decay=decay,bag=FALSE)
    
    avnnet<- train(formu,data=databis,
                   method="avNNet",linout = FALSE,maxit=itera,repeats=repeticiones,
                   trControl=control,tuneGrid=avnnetgrid,trace=trace)
    
    print(avnnet$results)
    
    preditest<-avnnet$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    tasafallos<-function(x,y) {
      confu<-confusionMatrix(x,y)
      tasa<-confu[[3]][1]
      return(tasa)
    }
    
    # Aplicamos función sobre cada Repetición
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      tasa=1-tasafallos(paso1$pred,paso1$obs)  
      medias<-rbind(medias,tasa)
    }
    names(medias)<-"tasa"
    
    
    # CalculamoS AUC  por cada Repetición de cv 
    # Definimnos función
    
    auc<-function(x,y) {
      curvaroc<-roc(response=x,predictor=y)
      auc<-curvaroc$auc
      return(auc)
    }
    
    # Aplicamos función sobre cada Repetición
    
    
    
    mediasbis<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      auc=suppressMessages(auc(paso1$obs,paso1$Yes))
      mediasbis<-rbind(mediasbis,auc)
    }
    names(mediasbis)<-"auc"
    
    
    # Unimos la info de auc y de tasafallos
    
    medias$auc<-mediasbis$auc
    
    return(medias)
    
  }


cruzadaarbol<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5,
           cp=c(0),minbucket =20)
    
  { 
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",
                          number=grupos,repeats=repe,
                          savePredictions = "all") 
    
    # Aplico caret y construyo modelo
    
    
    arbolgrid <-  expand.grid(cp=cp)
    
    arbol<- train(formu,data=databis,
                  method="rpart",trControl=control,
                  tuneGrid=arbolgrid,minbucket=minbucket)
    
    print(arbol$results)
    
    preditest<-arbol$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    preditest$error<-(preditest$pred-preditest$obs)^2
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      error=mean(paso1$error)  
      medias<-rbind(medias,error)
    }
    names(medias)<-"error"
    
    
    
    return(medias)
    
  }

cruzadaarbolbin<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5,
           cp=c(0),minbucket =20)
  { 
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    databis[,vardep]<-as.factor(databis[,vardep])
    
    formu<-formula(paste("factor(",vardep,")~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                          savePredictions = "all",classProbs=TRUE) 
    
    # Aplico caret y construyo modelo
    
    arbolgrid <-  expand.grid(cp=cp)
    
    arbol<- train(formu,data=databis,
                  method="rpart",trControl=control,
                  tuneGrid=arbolgrid,minbucket=minbucket)
    
    print(arbol$results)
    
    preditest<-arbol$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    tasafallos<-function(x,y) {
      confu<-confusionMatrix(x,y)
      tasa<-confu[[3]][1]
      return(tasa)
    }
    
    # Aplicamos función sobre cada Repetición
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      tasa=1-tasafallos(paso1$pred,paso1$obs)  
      medias<-rbind(medias,tasa)
    }
    names(medias)<-"tasa"
    
    
    # CalculamoS AUC  por cada Repetición de cv 
    # Definimnos función
    
    auc<-function(x,y) {
      curvaroc<-roc(response=x,predictor=y)
      auc<-curvaroc$auc
      return(auc)
    }
    
    # Aplicamos función sobre cada Repetición
    
    
    
    mediasbis<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      auc=suppressMessages(auc(paso1$obs,paso1$Yes))
      mediasbis<-rbind(mediasbis,auc)
    }
    names(mediasbis)<-"auc"
    
    
    # Unimos la info de auc y de tasafallos
    
    medias$auc<-mediasbis$auc
    
    return(medias)
    
  }


cruzadagbmbin<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5,
           n.minobsinnode=20,shrinkage=0.1,n.trees=100,interaction.depth=2)
  { 
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    databis[,vardep]<-as.factor(databis[,vardep])
    
    formu<-formula(paste("factor(",vardep,")~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                          savePredictions = "all",classProbs=TRUE) 
    
    # Aplico caret y construyo modelo
    
    
    
    gbmgrid <-expand.grid(n.minobsinnode=n.minobsinnode,
                          shrinkage=shrinkage,n.trees=n.trees,
                          interaction.depth=interaction.depth)
    
    gbm<- train(formu,data=databis,
                method="gbm",trControl=control,
                tuneGrid=gbmgrid,distribution="bernoulli",verbose=FALSE)
    
    print(gbm$results)
    
    preditest<-gbm$pred
    
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    tasafallos<-function(x,y) {
      confu<-confusionMatrix(x,y)
      tasa<-confu[[3]][1]
      return(tasa)
    }
    
    # Aplicamos función sobre cada Repetición
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      tasa=1-tasafallos(paso1$pred,paso1$obs)  
      medias<-rbind(medias,tasa)
    }
    names(medias)<-"tasa"
    
    
    # CalculamoS AUC  por cada Repetición de cv 
    # Definimnos función
    
    auc<-function(x,y) {
      curvaroc<-roc(response=x,predictor=y)
      auc<-curvaroc$auc
      return(auc)
    }
    
    # Aplicamos función sobre cada Repetición
    
    
    
    mediasbis<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      auc=suppressMessages(auc(paso1$obs,paso1$Yes))
      mediasbis<-rbind(mediasbis,auc)
    }
    names(mediasbis)<-"auc"
    
    
    # Unimos la info de auc y de tasafallos
    
    medias$auc<-mediasbis$auc
    
    return(medias)
    
  }


cruzadagbm<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5,
           n.minobsinnode=20,shrinkage=0.1,n.trees=100,interaction.depth=2)
    
  { 
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",
                          number=grupos,repeats=repe,
                          savePredictions = "all") 
    
    # Aplico caret y construyo modelo
    
    # n.minobsinnode=20,shrinkage=0.1,n.trees=100,interaction.depth=2
    
    gbmgrid <-expand.grid(n.minobsinnode=n.minobsinnode,
                          shrinkage=shrinkage,n.trees=n.trees,
                          interaction.depth=interaction.depth)
    
    gbm<- train(formu,data=databis,
                method="gbm",trControl=control,
                tuneGrid=gbmgrid,distribution="gaussian",verbose=FALSE)
    
    print(gbm$results)
    
    preditest<-gbm$pred
    
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    preditest$error<-(preditest$pred-preditest$obs)^2
    
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      error=mean(paso1$error)  
      medias<-rbind(medias,error)
    }
    names(medias)<-"error"
    
    
    
    return(medias)
    
  }


cruzadarfbin<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5,nodesize=20,
           mtry=2,ntree=50,replace=TRUE,sampsize=1)
  { 
    # if  (sampsize==1)
    # {
    #  sampsize=floor(nrow(data)/(grupos-1))
    # }
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    databis[,vardep]<-as.factor(databis[,vardep])
    
    formu<-formula(paste("factor(",vardep,")~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                          savePredictions = "all",classProbs=TRUE) 
    
    # Aplico caret y construyo modelo
    
    rfgrid <-expand.grid(mtry=mtry)
    
    if  (sampsize==1)
    {
      rf<- train(formu,data=databis,
                 method="rf",trControl=control,
                 tuneGrid=rfgrid,nodesize=nodesize,replace=replace,ntree=ntree)
    }
    
    else  if  (sampsize!=1)
    {
      rf<- train(formu,data=databis,
                 method="rf",trControl=control,
                 tuneGrid=rfgrid,nodesize=nodesize,replace=replace,sampsize=sampsize,
                 ntree=ntree)
    }
    
    
    print(rf$results)
    
    preditest<-rf$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    tasafallos<-function(x,y) {
      confu<-confusionMatrix(x,y)
      tasa<-confu[[3]][1]
      return(tasa)
    }
    
    # Aplicamos función sobre cada Repetición
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      tasa=1-tasafallos(paso1$pred,paso1$obs)  
      medias<-rbind(medias,tasa)
    }
    names(medias)<-"tasa"
    
    
    # CalculamoS AUC  por cada Repetición de cv 
    # Definimnos función
    
    auc<-function(x,y) {
      curvaroc<-roc(response=x,predictor=y)
      auc<-curvaroc$auc
      return(auc)
    }
    
    # Aplicamos función sobre cada Repetición
    
    
    
    mediasbis<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      auc=suppressMessages(auc(paso1$obs,paso1$Yes))
      mediasbis<-rbind(mediasbis,auc)
    }
    names(mediasbis)<-"auc"
    
    
    # Unimos la info de auc y de tasafallos
    
    medias$auc<-mediasbis$auc
    
    return(medias)
    
  }

cruzadarf<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5,
           nodesize=20,replace=TRUE,ntree=100,mtry=2,sampsize=1)
    
  { 
    library(caret)
    
    library(dummies)
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",
                          number=grupos,repeats=repe,
                          savePredictions = "all") 
    
    # Aplico caret y construyo modelo
    
    rfgrid <-expand.grid(mtry=mtry)
    
    if  (sampsize==1)
    {
      rf<- train(formu,data=databis,
                 method="rf",trControl=control,
                 tuneGrid=rfgrid,nodesize=nodesize,replace=replace,ntree=ntree)
    }
    
    else  if  (sampsize!=1)
    {
      rf<- train(formu,data=databis,
                 method="rf",trControl=control,
                 tuneGrid=rfgrid,nodesize=nodesize,replace=replace,sampsize=sampsize,
                 ntree=ntree)
    }
    
    
    print(rf$results)
    
    preditest<-rf$pred
    
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    preditest$error<-(preditest$pred-preditest$obs)^2
    
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      error=mean(paso1$error)  
      medias<-rbind(medias,error)
    }
    names(medias)<-"error"
    
    
    
    
    return(medias)
    
  }


cruzadaxgbmbin<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5,
           min_child_weight=20,eta=0.1,nrounds=100,max_depth=2,
           gamma=0,colsample_bytree=1,subsample=1,alpha=0,lambda=0,lambda_bias=0)
  { 
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    databis[,vardep]<-as.factor(databis[,vardep])
    
    formu<-formula(paste("factor(",vardep,")~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                          savePredictions = "all",classProbs=TRUE) 
    
    # Aplico caret y construyo modelo
    
    xgbmgrid <-expand.grid( min_child_weight=min_child_weight,
                            eta=eta,nrounds=nrounds,max_depth=max_depth,
                            gamma=gamma,colsample_bytree=colsample_bytree,subsample=subsample)
    
    xgbm<- train(formu,data=databis,
                 method="xgbTree",trControl=control,
                 tuneGrid=xgbmgrid,verbose=FALSE,
                 alpha=alpha,lambda=lambda,lambda_bias=lambda_bias)
    
    print(xgbm$results)
    
    preditest<-xgbm$pred
    
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    tasafallos<-function(x,y) {
      confu<-confusionMatrix(x,y)
      tasa<-confu[[3]][1]
      return(tasa)
    }
    
    # Aplicamos función sobre cada Repetición
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      tasa=1-tasafallos(paso1$pred,paso1$obs)  
      medias<-rbind(medias,tasa)
    }
    names(medias)<-"tasa"
    
    
    # CalculamoS AUC  por cada Repetición de cv 
    # Definimnos función
    
    auc<-function(x,y) {
      curvaroc<-roc(response=x,predictor=y)
      auc<-curvaroc$auc
      return(auc)
    }
    
    # Aplicamos función sobre cada Repetición
    
    
    
    mediasbis<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      auc=suppressMessages(auc(paso1$obs,paso1$Yes))
      mediasbis<-rbind(mediasbis,auc)
    }
    names(mediasbis)<-"auc"
    
    
    # Unimos la info de auc y de tasafallos
    
    medias$auc<-mediasbis$auc
    
    return(medias)
    
  }



cruzadaxgbm<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5,
           min_child_weight=20,eta=0.1,nrounds=100,max_depth=2,
           gamma=0,colsample_bytree=1,subsample=1,alpha=0,lambda=0,lambda_bias=0)  
  { 
    library(caret)
    
    library(dummies)
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",
                          number=grupos,repeats=repe,
                          savePredictions = "all") 
    
    # Aplico caret y construyo modelo
    
    xgbmgrid <-expand.grid( min_child_weight=min_child_weight,
                            eta=eta,nrounds=nrounds,max_depth=max_depth,
                            gamma=gamma,colsample_bytree=colsample_bytree,subsample=subsample)
    
    xgbm<- train(formu,data=databis,
                 method="xgbTree",trControl=control,
                 tuneGrid=xgbmgrid,verbose=FALSE,
                 alpha=alpha,lambda=lambda,lambda_bias=lambda_bias)
    
    print(xgbm$results)
    
    preditest<-xgbm$pred
    
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    preditest$error<-(preditest$pred-preditest$obs)^2
    
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      error=mean(paso1$error)  
      medias<-rbind(medias,error)
    }
    names(medias)<-"error"
    
    
    
    
    return(medias)
    
  }



cruzadaSVMbin<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5,
           C=1,replace=TRUE)
  { 
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    databis[,vardep]<-as.factor(databis[,vardep])
    
    formu<-formula(paste("factor(",vardep,")~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                          savePredictions = "all",classProbs=TRUE) 
    
    # Aplico caret y construyo modelo
    
    SVMgrid <-expand.grid(C=C)
    
    SVM<- train(formu,data=databis,
                method="svmLinear",trControl=control,
                tuneGrid=SVMgrid,replace=replace)
    
    print(SVM$results)
    
    preditest<-SVM$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    tasafallos<-function(x,y) {
      confu<-confusionMatrix(x,y)
      tasa<-confu[[3]][1]
      return(tasa)
    }
    
    # Aplicamos función sobre cada Repetición
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      tasa=1-tasafallos(paso1$pred,paso1$obs)  
      medias<-rbind(medias,tasa)
    }
    names(medias)<-"tasa"
    
    
    # CalculamoS AUC  por cada Repetición de cv 
    # Definimnos función
    
    auc<-function(x,y) {
      curvaroc<-roc(response=x,predictor=y)
      auc<-curvaroc$auc
      return(auc)
    }
    
    # Aplicamos función sobre cada Repetición
    
    
    
    mediasbis<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      auc=suppressMessages(auc(paso1$obs,paso1$Yes))
      mediasbis<-rbind(mediasbis,auc)
    }
    names(mediasbis)<-"auc"
    
    
    # Unimos la info de auc y de tasafallos
    
    medias$auc<-mediasbis$auc
    
    return(medias)
    
  }

cruzadaSVMbinPoly<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5,
           C=1,degree=2,scale=1)
  { 
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    databis[,vardep]<-as.factor(databis[,vardep])
    
    formu<-formula(paste("factor(",vardep,")~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                          savePredictions = "all",classProbs=TRUE) 
    
    # Aplico caret y construyo modelo
    
    SVMgrid <-expand.grid(C=C,degree=degree,scale=scale)
    
    SVM<- train(formu,data=databis,
                method="svmPoly",trControl=control,
                tuneGrid=SVMgrid,replace=replace)
    
    print(SVM$results)
    
    preditest<-SVM$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    tasafallos<-function(x,y) {
      confu<-confusionMatrix(x,y)
      tasa<-confu[[3]][1]
      return(tasa)
    }
    
    # Aplicamos función sobre cada Repetición
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      tasa=1-tasafallos(paso1$pred,paso1$obs)  
      medias<-rbind(medias,tasa)
    }
    names(medias)<-"tasa"
    
    
    # CalculamoS AUC  por cada Repetición de cv 
    # Definimnos función
    
    auc<-function(x,y) {
      curvaroc<-roc(response=x,predictor=y)
      auc<-curvaroc$auc
      return(auc)
    }
    
    # Aplicamos función sobre cada Repetición
    
    
    
    mediasbis<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      auc=suppressMessages(auc(paso1$obs,paso1$Yes))
      mediasbis<-rbind(mediasbis,auc)
    }
    names(mediasbis)<-"auc"
    
    
    # Unimos la info de auc y de tasafallos
    
    medias$auc<-mediasbis$auc
    
    return(medias)
    
  }


cruzadaSVMbinRBF<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5,
           C=1,sigma=1)
  { 
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    databis[,vardep]<-as.factor(databis[,vardep])
    
    formu<-formula(paste("factor(",vardep,")~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                          savePredictions = "all",classProbs=TRUE) 
    
    # Aplico caret y construyo modelo
    
    SVMgrid <-expand.grid(C=C,sigma=sigma)
    
    SVM<- train(formu,data=databis,
                method="svmRadial",trControl=control,
                tuneGrid=SVMgrid,replace=replace)
    
    print(SVM$results)
    
    preditest<-SVM$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    tasafallos<-function(x,y) {
      confu<-confusionMatrix(x,y)
      tasa<-confu[[3]][1]
      return(tasa)
    }
    
    # Aplicamos función sobre cada Repetición
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      tasa=1-tasafallos(paso1$pred,paso1$obs)  
      medias<-rbind(medias,tasa)
    }
    names(medias)<-"tasa"
    
    
    # CalculamoS AUC  por cada Repetición de cv 
    # Definimnos función
    
    auc<-function(x,y) {
      curvaroc<-roc(response=x,predictor=y)
      auc<-curvaroc$auc
      return(auc)
    }
    
    # Aplicamos función sobre cada Repetición
    
    
    
    mediasbis<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      auc=suppressMessages(auc(paso1$obs,paso1$Yes))
      mediasbis<-rbind(mediasbis,auc)
    }
    names(mediasbis)<-"auc"
    
    
    # Unimos la info de auc y de tasafallos
    
    medias$auc<-mediasbis$auc
    
    return(medias)
    
  }



cruzadaSVM<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5, C=1)  
  { 
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",
                          number=grupos,repeats=repe,
                          savePredictions = "all") 
    
    # Aplico caret y construyo modelo
    
    SVMgrid <-expand.grid(C=C)
    
    SVM<- train(formu,data=databis,
                method="svmLinear",trControl=control,
                tuneGrid=SVMgrid,verbose=FALSE)
    
    print(SVM$results)
    
    preditest<-SVM$pred
    
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    preditest$error<-(preditest$pred-preditest$obs)^2
    
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      error=mean(paso1$error)  
      medias<-rbind(medias,error)
    }
    names(medias)<-"error"
    
    
    
    
    return(medias)
    
  }



cruzadaSVMpoly<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5, C=1,degree=2,scale=1)  
  { 
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",
                          number=grupos,repeats=repe,
                          savePredictions = "all") 
    
    # Aplico caret y construyo modelo
    
    SVMgrid <-expand.grid(C=C,degree=degree,scale=scale)
    
    SVM<- train(formu,data=databis,
                method="svmPoly",trControl=control,
                tuneGrid=SVMgrid,verbose=FALSE)
    
    print(SVM$results)
    
    preditest<-SVM$pred
    
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    preditest$error<-(preditest$pred-preditest$obs)^2
    
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      error=mean(paso1$error)  
      medias<-rbind(medias,error)
    }
    names(medias)<-"error"
    
    
    
    
    return(medias)
    
  }


cruzadaSVMRBF<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5, C=1,sigma=1)  
  { 
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",
                          number=grupos,repeats=repe,
                          savePredictions = "all") 
    
    # Aplico caret y construyo modelo
    
    SVMgrid <-expand.grid(C=C,sigma=sigma)
    
    SVM<- train(formu,data=databis,
                method="svmRadial",trControl=control,
                tuneGrid=SVMgrid,verbose=FALSE)
    
    print(SVM$results)
    
    preditest<-SVM$pred
    
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    preditest$error<-(preditest$pred-preditest$obs)^2
    
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      error=mean(paso1$error)  
      medias<-rbind(medias,error)
    }
    names(medias)<-"error"
    
    
    
    
    return(medias)
    
  }



resultadosgbm<-function(dataf=dataf,vardep=vardep,shrink=0.01,n.trees =100,n.minobsinnode =20,corte=0.5)
  
{
  library(caret)
  library(pROC)
  
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
  
  set.seed(12345)
  control<-trainControl(method = "cv",number=4,savePredictions = "all",classProbs=TRUE)
  
  # PREPARACIÓN DATOS
  
  
  tabla1<-as.data.frame(table(dataf[,vardep]))
  tabla1<-tabla1[order(tabla1$Freq),]
  minoritaria<-as.character(tabla1[1,c("Var1")])
  tabla1<-tabla1[order(-tabla1$Freq),]
  mayoritaria<-as.character(tabla1[1,c("Var1")])
  if (minoritaria==mayoritaria)
  {
    tabla1<-tabla1[order(tabla1$Freq),]
    mayoritaria<-as.character(tabla1[2,c("Var1")])
  }
  cosa<-as.data.frame(prop.table(table(dataf[[vardep]])))
  fremin<-100*round(min(cosa$Freq),2)
  totalobs=nrow(dataf)
  
  cosa<-as.data.frame(table(dataf[[vardep]]))
  totalmin<-round(min(cosa$Freq),2)
  
  dataf[vardep]<-ifelse(dataf[vardep]==minoritaria,"Yes","No")
  
  formu1<-paste("factor(",vardep,")~.")
  
  gbmGrid <- expand.grid(interaction.depth = 2,shrinkage =shrink,
                         n.trees =n.trees,n.minobsinnode = n.minobsinnode )
  
  gbm <- train(formula(formu1), data=dataf,trControl=control,
               method="gbm", distribution="bernoulli",tuneGrid=gbmGrid)
  
  preditest<-gbm$pred
  
  preditest$pred<-ifelse(preditest$Yes>corte,"Yes","No")
  preditest$pred<-as.factor(preditest$pred)
  
  tasa=1-tasafallos(preditest$pred,preditest$obs)
  auc=auc(preditest$obs,preditest$Yes)
  a<-as.data.frame(table(preditest$pred))
  nYes<-a[2,2]
  
  if (is.na(nYes)==T)
  {nYes=0}
  confu<-confusionMatrix(preditest$pred,preditest$obs)
  FP<-confu[[2]][2]
  FN<-confu[[2]][3]
  VP<-confu[[2]][4]
  VN<-confu[[2]][1]
  
  
  sink("gbm.txt",append=FALSE)
  cat("obs: ",totalobs,"\n")
  cat("obs clase minoritaria: ",totalmin,"\n")
  cat("% clase minoritaria: ",fremin,"%","\n")
  cat("tasa fallos: ",tasa,"\n")
  cat("auc: ",auc,"\n")
  cat("sensitivity: ", sensitivity(preditest$pred,preditest$obs,"Yes"),"\n")
  cat("specificity: ",specificity(preditest$pred,preditest$obs,"No"),"\n")
  cat("numero de Yes predichos: ", nYes,"\n")
  cat("FP: ", FP,"\n")
  cat("FN: ", FN,"\n")
  cat("VP: ", VP,"\n")
  cat("VN: ", VN,"\n")
  sink()
  
  return(preditest)
}



resultadosglm<-function(dataf=dataf,vardep=vardep,corte=0.5)
  
{
  library(caret)
  library(pROC)
  
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
  set.seed(12345)
  control<-trainControl(method = "cv",number=4,savePredictions = "all",classProbs=TRUE)
  
  # PREPARACIÓN DATOS
  
  
  tabla1<-as.data.frame(table(dataf[,vardep]))
  tabla1<-tabla1[order(tabla1$Freq),]
  minoritaria<-as.character(tabla1[1,c("Var1")])
  tabla1<-tabla1[order(-tabla1$Freq),]
  mayoritaria<-as.character(tabla1[1,c("Var1")])
  if (minoritaria==mayoritaria)
  {
    tabla1<-tabla1[order(tabla1$Freq),]
    mayoritaria<-as.character(tabla1[2,c("Var1")])
  }
  cosa<-as.data.frame(prop.table(table(dataf[[vardep]])))
  fremin<-100*round(min(cosa$Freq),2)
  totalobs=nrow(dataf)
  
  cosa<-as.data.frame(table(dataf[[vardep]]))
  totalmin<-round(min(cosa$Freq),2)
  
  dataf[vardep]<-ifelse(dataf[vardep]==minoritaria,"Yes","No")
  
  formu1<-paste("factor(",vardep,")~.")
  
  glm <- train(formula(formu1),data=dataf,trControl=control,method="glm",family = binomial(link="logit"))
  preditest<-glm$pred
  
  preditest$pred<-ifelse(preditest$Yes>corte,"Yes","No")
  preditest$pred<-as.factor(preditest$pred)
  
  
  tasa=1-tasafallos(preditest$pred,preditest$obs)
  auc=auc(preditest$obs,preditest$Yes)
  a<-as.data.frame(table(preditest$pred))
  nYes<-a[2,2]
  
  if (is.na(nYes)==T)
  {nYes=0}
  
  confu<-confusionMatrix(preditest$pred,preditest$obs)
  
  FP<-confu[[2]][2]
  FN<-confu[[2]][3]
  VP<-confu[[2]][4]
  VN<-confu[[2]][1]
  
  
  sink("glm.txt",append=FALSE)
  cat("obs: ",totalobs,"\n")
  cat("obs clase minoritaria: ",totalmin,"\n")
  cat("% clase minoritaria: ",fremin,"%","\n")
  cat("tasa fallos: ",tasa,"\n")
  cat("auc: ",auc,"\n")
  cat("sensitivity: ", sensitivity(preditest$pred,preditest$obs,"Yes"),"\n")
  cat("specificity: ",specificity(preditest$pred,preditest$obs,"No"),"\n")
  cat("numero de Yes predichos: ", nYes,"\n")
  cat("FP: ", FP,"\n")
  cat("FN: ", FN,"\n")
  cat("VP: ", VP,"\n")
  cat("VN: ", VN,"\n")
  sink()
  
  return(preditest)
}




resultadosnnet<-function(dataf=dataf,vardep=vardep,size=5,decay=0.01,maxit=200,corte=0.5)
  
{
  library(caret)
  library(pROC)
  
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
  
  set.seed(12345)
  control<-trainControl(method = "cv",number=4,savePredictions = "all",classProbs=TRUE)
  
  # PREPARACIÓN DATOS
  
  
  tabla1<-as.data.frame(table(dataf[,vardep]))
  tabla1<-tabla1[order(tabla1$Freq),]
  minoritaria<-as.character(tabla1[1,c("Var1")])
  tabla1<-tabla1[order(-tabla1$Freq),]
  mayoritaria<-as.character(tabla1[1,c("Var1")])
  if (minoritaria==mayoritaria)
  {
    tabla1<-tabla1[order(tabla1$Freq),]
    mayoritaria<-as.character(tabla1[2,c("Var1")])
  }
  cosa<-as.data.frame(prop.table(table(dataf[[vardep]])))
  fremin<-100*round(min(cosa$Freq),2)
  totalobs=nrow(dataf)
  
  cosa<-as.data.frame(table(dataf[[vardep]]))
  totalmin<-round(min(cosa$Freq),2)
  
  dataf[vardep]<-ifelse(dataf[vardep]==minoritaria,"Yes","No")
  
  formu1<-paste("factor(",vardep,")~.")
  
  
  netgrid <-expand.grid(size=size,decay=decay)
  net <- train(formula(formu1),data=dataf,trControl=control,method="nnet",
               tuneGrid=netgrid,linout=FALSE,maxit=maxit)
  preditest<-net$pred
  
  preditest$pred<-ifelse(preditest$Yes>corte,"Yes","No")
  preditest$pred<-as.factor(preditest$pred)
  
  tasa=1-tasafallos(preditest$pred,preditest$obs)
  auc=auc(preditest$obs,preditest$Yes)
  a<-as.data.frame(table(preditest$pred))
  
  nYes<-a[2,2]
  
  if (is.na(nYes)==T)
  {nYes=0}
  confu<-confusionMatrix(preditest$pred,preditest$obs)
  FP<-confu[[2]][2]
  FN<-confu[[2]][3]
  VP<-confu[[2]][4]
  VN<-confu[[2]][1]
  
  
  sink("nnet.txt",append=FALSE)
  cat("obs: ",totalobs,"\n")
  cat("obs clase minoritaria: ",totalmin,"\n")
  cat("% clase minoritaria: ",fremin,"%","\n")
  cat("tasa fallos: ",tasa,"\n")
  cat("auc: ",auc,"\n")
  cat("sensitivity: ", sensitivity(preditest$pred,preditest$obs,"Yes"),"\n")
  cat("specificity: ",specificity(preditest$pred,preditest$obs,"No"),"\n")
  cat("numero de Yes predichos: ", nYes,"\n")
  cat("FP: ", FP,"\n")
  cat("FN: ", FN,"\n")
  cat("VP: ", VP,"\n")
  cat("VN: ", VN,"\n")
  sink()
  
  return(preditest)
}




resultadosrf<-function(dataf=dataf,vardep=vardep,mtry=2,ntree=100,sampsize=400,corte=0.5)
  
{
  library(caret)
  library(pROC)
  
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
  set.seed(12345)
  control<-trainControl(method = "cv",number=4,savePredictions = "all",classProbs=TRUE)
  
  # PREPARACIÓN DATOS
  
  
  tabla1<-as.data.frame(table(dataf[,vardep]))
  tabla1<-tabla1[order(tabla1$Freq),]
  minoritaria<-as.character(tabla1[1,c("Var1")])
  tabla1<-tabla1[order(-tabla1$Freq),]
  mayoritaria<-as.character(tabla1[1,c("Var1")])
  if (minoritaria==mayoritaria)
  {
    tabla1<-tabla1[order(tabla1$Freq),]
    mayoritaria<-as.character(tabla1[2,c("Var1")])
  }
  cosa<-as.data.frame(prop.table(table(dataf[[vardep]])))
  fremin<-100*round(min(cosa$Freq),2)
  totalobs=nrow(dataf)
  
  cosa<-as.data.frame(table(dataf[[vardep]]))
  totalmin<-round(min(cosa$Freq),2)
  
  dataf[vardep]<-ifelse(dataf[vardep]==minoritaria,"Yes","No")
  
  formu1<-paste("factor(",vardep,")~.")
  
  
  if (sampsize>nrow(dataf)/4) {sampsize=floor(nrow(dataf)/4-1)}
  
  rfgrid <-expand.grid(mtry=mtry)
  rf <- train(formula(formu1),data=dataf,trControl=control,method="rf",
              tuneGrid=rfgrid,linout=FALSE,ntree=ntree,sampsize=sampsize)
  preditest<-rf$pred
  
  preditest$pred<-ifelse(preditest$Yes>corte,"Yes","No")
  preditest$pred<-as.factor(preditest$pred)
  
  tasa=1-tasafallos(preditest$pred,preditest$obs)
  auc=auc(preditest$obs,preditest$Yes)
  a<-as.data.frame(table(preditest$pred))
  nYes<-a[2,2]
  
  if (is.na(nYes)==T)
  {nYes=0}
  confu<-confusionMatrix(preditest$pred,preditest$obs)
  FP<-confu[[2]][2]
  FN<-confu[[2]][3]
  VP<-confu[[2]][4]
  VN<-confu[[2]][1]
  
  
  
  sink("rf.txt",append=FALSE)
  cat("obs: ",totalobs,"\n")
  cat("obs clase minoritaria: ",totalmin,"\n")
  cat("% clase minoritaria: ",fremin,"%","\n")
  cat("tasa fallos: ",tasa,"\n")
  cat("auc: ",auc,"\n")
  cat("sensitivity: ", sensitivity(preditest$pred,preditest$obs,"Yes"),"\n")
  cat("specificity: ",specificity(preditest$pred,preditest$obs,"No"),"\n")
  cat("numero de Yes predichos: ", nYes,"\n")
  cat("FP: ", FP,"\n")
  cat("FN: ", FN,"\n")
  cat("VP: ", VP,"\n")
  cat("VN: ", VN,"\n")
  sink()
  
  return(preditest)
}


resultadossvm<-function(dataf=dataf,vardep=vardep,C=100,sigma=10,corte=0.5)
  
{
  library(caret)
  library(pROC)
  
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
  set.seed(12345)
  control<-trainControl(method = "cv",number=4,savePredictions = "all",classProbs=TRUE)
  
  # PREPARACIÓN DATOS
  
  
  tabla1<-as.data.frame(table(dataf[,vardep]))
  tabla1<-tabla1[order(tabla1$Freq),]
  minoritaria<-as.character(tabla1[1,c("Var1")])
  tabla1<-tabla1[order(-tabla1$Freq),]
  mayoritaria<-as.character(tabla1[1,c("Var1")])
  if (minoritaria==mayoritaria)
  {
    tabla1<-tabla1[order(tabla1$Freq),]
    mayoritaria<-as.character(tabla1[2,c("Var1")])
  }
  cosa<-as.data.frame(prop.table(table(dataf[[vardep]])))
  fremin<-100*round(min(cosa$Freq),2)
  totalobs=nrow(dataf)
  
  cosa<-as.data.frame(table(dataf[[vardep]]))
  totalmin<-round(min(cosa$Freq),2)
  
  dataf[vardep]<-ifelse(dataf[vardep]==minoritaria,"Yes","No")
  
  formu1<-paste("factor(",vardep,")~.")
  
  SVMgrid <-expand.grid(C=C,sigma=sigma)
  svm <- train(formula(formu1),data=dataf,trControl=control,method="svmRadial",tuneGrid=SVMgrid,replace=replace)
  preditest<-svm$pred
  
  preditest$pred<-ifelse(preditest$Yes>corte,"Yes","No")
  preditest$pred<-as.factor(preditest$pred)
  
  tasa=1-tasafallos(preditest$pred,preditest$obs)
  auc=auc(preditest$obs,preditest$Yes)
  a<-as.data.frame(table(preditest$pred))
  nYes<-a[2,2]
  
  if (is.na(nYes)==T)
  {nYes=0}
  confu<-confusionMatrix(preditest$pred,preditest$obs)
  FP<-confu[[2]][2]
  FN<-confu[[2]][3]
  VP<-confu[[2]][4]
  VN<-confu[[2]][1]
  
  
  sink("svm.txt",append=FALSE)
  cat("obs: ",totalobs,"\n")
  cat("obs clase minoritaria: ",totalmin,"\n")
  cat("% clase minoritaria: ",fremin,"%","\n")
  cat("tasa fallos: ",tasa,"\n")
  cat("auc: ",auc,"\n")
  cat("sensitivity: ", sensitivity(preditest$pred,preditest$obs,"Yes"),"\n")
  cat("specificity: ",specificity(preditest$pred,preditest$obs,"No"),"\n")
  cat("numero de Yes predichos: ", nYes,"\n")
  cat("FP: ", FP,"\n")
  cat("FN: ", FN,"\n")
  cat("VP: ", VP,"\n")
  cat("VN: ", VN,"\n")
  sink()
  
  return(preditest)
}



seleccionar<-function(dataf,listconti,listclass,vardep)
  
{
  library(MASS)  
  library(dplyr)
  
  archivo<-dataf
  
  if (any(listclass==c(""))==TRUE)
  { 
    archivo<-archivo[,c(listconti,vardep)]
  } 
  
  if (any(listclass==c(""))==FALSE)
  { 
    if (any(listconti==c(""))==FALSE)
    {archivo<-archivo[,c(listconti,listclass,vardep)]}
    
    if (any(listconti==c(""))==TRUE)
    {archivo<-archivo[,c(listclass,vardep)]}
  } 
  
  # Estandarización a 0-1
  if (any(listconti==c(""))==FALSE)
  {
    normFunc <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}
    archivo[c(listconti)] <- apply(archivo[c(listconti)], 2, normFunc)
  }
  # LAS DE CLASE A FACTOR
  if (any(listclass==c(""))==FALSE)
  {
    archivo[c(listclass,vardep)]<-
      lapply(archivo[c(listclass,vardep)],factor)
  }
  
  
  formu1<-paste("factor(",vardep,")~.")
  formu2<-paste("factor(",vardep,")~1")
  
  # full.model <- glm(formula(formu1), data = archivo, family = binomial)
  #  step.model <- full.model %>% stepAIC(trace = TRUE)
  
  full<-glm(formu1,data=archivo,family = binomial(link="logit"))
  null<-glm(formu2,data=archivo,family = binomial(link="logit"))
  
  seleccion<-stepAIC(null,scope=list(upper=full),direction="both")
  cosa<-attr(terms(seleccion), "term.labels")
  
  # cosa<-attr(terms(step.model), "term.labels")
  
  # Para ver los efectos escogidos
  if (any(listclass==c(""))==FALSE)
  {
    listclass <- listclass[listclass %in%cosa]
    
    if (any(listconti==c(""))==FALSE)
    {
      listconti <- listconti[listconti %in%cosa]
    }
    
  }
  
  
  if (any(listclass==c(""))==TRUE)
  {
    listconti <- listconti[listconti %in%cosa]
  }
  
  if (any(listclass==c(""))==TRUE)
  {
    archivo<-archivo[,c(listconti,vardep)]
  }
  
  if (any(listclass==c(""))==FALSE)
  {
    if (any(listconti==c(""))==FALSE)
    {archivo<-archivo[,c(listconti,listclass,vardep)]}
    if (any(listconti==c(""))==TRUE)
    {archivo<-archivo[,c(listclass,vardep)]}
  }
  
  
  detach("package:dplyr", unload=TRUE)
  
  return(list(archivo,listconti,listclass,vardep))  
}


cruzadaKNNbin<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5,
           k=1)
  { 
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    databis[,vardep]<-as.factor(databis[,vardep])
    
    formu<-formula(paste("factor(",vardep,")~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                          savePredictions = "all",classProbs=TRUE) 
    
    # Aplico caret y construyo modelo
    
    KNNgrid <-expand.grid(k=k)
    
    KNN<- train(formu,data=databis,
                method="knn",trControl=control,
                tuneGrid=KNNgrid)
    
    print(KNN$results)
    
    preditest<-KNN$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    tasafallos<-function(x,y) {
      confu<-confusionMatrix(x,y)
      tasa<-confu[[3]][1]
      return(tasa)
    }
    
    # Aplicamos función sobre cada Repetición
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      tasa=1-tasafallos(paso1$pred,paso1$obs)  
      medias<-rbind(medias,tasa)
    }
    names(medias)<-"tasa"
    
    
    # CalculamoS AUC  por cada Repetición de cv 
    # Definimnos función
    
    auc<-function(x,y) {
      curvaroc<-roc(response=x,predictor=y)
      auc<-curvaroc$auc
      return(auc)
    }
    
    # Aplicamos función sobre cada Repetición
    
    
    
    mediasbis<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      auc=suppressMessages(auc(paso1$obs,paso1$Yes))
      mediasbis<-rbind(mediasbis,auc)
    }
    names(mediasbis)<-"auc"
    
    
    # Unimos la info de auc y de tasafallos
    
    medias$auc<-mediasbis$auc
    
    return(medias)
    
  }

cruzadaNBbin<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5,
           usekernel=TRUE,fL=0, adjust=1)
  { 
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    databis[,vardep]<-as.factor(databis[,vardep])
    
    formu<-formula(paste("factor(",vardep,")~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                          savePredictions = "all",classProbs=TRUE) 
    
    # Aplico caret y construyo modelo
    
    NBgrid <-expand.grid(usekernel=usekernel, fL=fL, adjust=adjust)
    
    NB<- train(formu,data=databis, method="nb",trControl=control, tuneGrid=NBgrid)
    
    print(NB$results)
    
    preditest<-NB$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    tasafallos<-function(x,y) {
      confu<-confusionMatrix(x,y)
      tasa<-confu[[3]][1]
      return(tasa)
    }
    
    # Aplicamos función sobre cada Repetición
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      tasa=1-tasafallos(paso1$pred,paso1$obs)  
      medias<-rbind(medias,tasa)
    }
    names(medias)<-"tasa"
    
    
    # CalculamoS AUC  por cada Repetición de cv 
    # Definimnos función
    
    auc<-function(x,y) {
      curvaroc<-roc(response=x,predictor=y)
      auc<-curvaroc$auc
      return(auc)
    }
    
    # Aplicamos función sobre cada Repetición
    
    
    
    mediasbis<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      auc=suppressMessages(auc(paso1$obs,paso1$Yes))
      mediasbis<-rbind(mediasbis,auc)
    }
    names(mediasbis)<-"auc"
    
    
    # Unimos la info de auc y de tasafallos
    
    medias$auc<-mediasbis$auc
    
    return(medias)
    
  }


