# script name:
# plumber.R

# set API title and description to show up in http://localhost:8080/__swagger__/

#' @apiTitle Evalúa tráfico de red con un ensamblado entre los modelos XGBM, GBM, RF, NNET y SVM para detectar ataques
#' @apiDescription Esta API toma como entrada tráfico de red en formato Netflow y predice si el tráfico es normal
#' o es un ataque de tipo Brute Force (fuerza bruta), DoS (negación de servicio), Ping Scan (escaneo de red) o Port Scan (escaneo de puerto).

# llamamos a los 5 modelos que conforman el ensamblado
load("xgbm_cat.RData")
load("gbm_cat.RData")
load("rf_cat.RData")
load("nnet_cat.RData")
load("SVM_cat.RData")

#' Log system time, request method and HTTP user agent of the incoming request
#' @filter logger
function(req){
  cat("System time:", as.character(Sys.time()), "\n",
      "Request method:", req$REQUEST_METHOD, req$PATH_INFO, "\n",
      "HTTP user agent:", req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}

# core function follows below:
# define parameters with type and description
# name endpoint
# return output as html/text
# specify 200 (okay) return

#' Predice tráfico malicioso en una red con el ensamblado de modelos.
#' @param Duration:numeric Indica la duración del Flow en segundos, integer (scaled and centered to be btw -3 and 3)
#' @param Proto:numeric Indica el protocolo de transporte usado (TCP, UDP, etc.), integer (scaled and centered to be btw -3 and 3)
#' @param SrcPt:numeric Indica el número de puerto de la máquina origen, numeric (scaled and centered to be btw -3 and 3)
#' @param DstPt:numeric Indica el número de puerto de la máquina destino, numeric (scaled and centered to be btw -3 and 3)
#' @param Packets:numeric Indica el número de paquetes transmitidos en el flow, integer (scaled and centered to be btw -3 and 3)
#' @param Bytes:numeric Indica el número de bytes transmitidos (e.g., 21378), numeric (scaled and centered to be btw -3 and 3)
#' @param Flags:numeric Indica un estado particular en la conexión TCP (entre 0 y 254 opciones), numeric (scaled and centered to be btw -3 and 3)
#' @param Tos:numeric indica el tipo de servicio, numeric (scaled and centered to be btw -3 and 3)
#' @param Packets_speed:numeric Indica el número de paquetes transmitidos entre la duración del flow, integer (scaled and centered to be btw -3 and 3)
#' @param Bytes_speed:numeric Indica el número de bytes transmitidos entre la duración el flow, integer (scaled and centered to be btw -3 and 3)
#' @param Hour:numeric Indica la hora en la que se capturó el flow, integer (scaled and centered to be btw -3 and 3)
#' @param Minute:numeric Indica el minuto en el que se capturó el flow, integer (scaled and centered to be btw -3 and 3)
#' @param Second:numeric Indica el segundo en el que se capturó el flow, integer (scaled and centered to be btw -3 and 3)
#' @param Day:numeric Indica el día en el que se capturó el flow, integer (scaled and centered to be btw -3 and 3)
#' @get /predict
#' @html
#' @response 200 Retorna la clase del tráfico de red (bruteforce, dos, normal, pingScan o portScan) usando el ensamblado de modelos
calculate_prediction <- function(DstPt, Flags, Bytes, Packets, Duration, SrcPt, Packets_speed, Proto, Tos, Bytes_speed, Minute, Hour, Second, Day) {
  
  # make data frame from numeric parameters
  input_data_num <<- data.frame(DstPt, Flags, Bytes, Packets, Duration, SrcPt, Packets_speed, Proto, Tos, Bytes_speed, Minute, Hour, Second, Day, 
                                stringsAsFactors = FALSE)
  
  # and make sure they really are numeric
  input_data_num <<- as.data.frame(t(sapply(input_data_num, as.numeric)))

  # combine into one data frame
  input_data <<- as.data.frame(input_data_num)
  
  # validation for parameter
  if (any(is.na(input_data))) {
    res$status <- 400
    res$body <- "Parameters have to be numeric"
  }
  
  if (any(input_data < -3) || any(input_data > 3)) {
    res$status <- 400
    res$body <- "Parameters have to be between -3 and 3"
  }
  
  # realiza predicciones con cada uno de los modelos por separado
  pred_xgbm <<- predict(xgbm_cat, input_data)
  pred_gbm <<- predict(gbm_cat, input_data)
  pred_rf <<- predict(rf_cat, input_data)
  pred_nnet <<- predict(nnet_cat, input_data)
  pred_svm <<- predict(SVM_cat, input_data)
  
  #función Mode que calcula el valor más repetitivo de una lista
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
 #Creamos la lista con los valores de predicción de cada modelo
  nuevos_datos3<- cbind(pred_xgbm,pred_gbm, pred_rf, pred_nnet, pred_svm)
  nuevos_datos3<-as.data.frame(nuevos_datos3) #convertimos la lista en un dataframe
  pred_ens3 <- apply(nuevos_datos3,1,Mode) #aplicamos la función Mode a la lista y la asignamos a una variable nueva
  
  #la variable pred_ens3 posee la predicción del ensamblado en valores numéricos. Los transformamos en categóricos
  pred_ens <- ifelse(pred_ens3 ==1, 'DoS Attack', #si el valor es 1, es un ataque DoS
                     ifelse(pred_ens3 ==2, 'Normal Traffic', #si el valor es 2, es tráfico normal
                            ifelse(pred_ens3 ==3, 'Port Scan', #si el valor es 3, es un ataque PortScan
                                   ifelse(pred_ens3 ==4, 'Ping scan', 'Brute Force Attack')))) #si el valor es 4, es un ataque PingScan, sino es un bruteForce
  #Mostramos la predicción del ensamblado
  paste("----------------\nTest Traffic flow predicted to be", as.character(pred_ens), "\n----------------\n")
}