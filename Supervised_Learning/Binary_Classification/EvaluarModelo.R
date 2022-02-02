# script name:
# plumber.R

# set API title and description to show up in http://localhost:8000/__swagger__/

#' @apiTitle Evalúa tráfico de red para detectar anomalías con el modelo Random Forest
#' @apiDescription Esta API toma como entrada tráfico de red en formato netflow y retorna una predicción del tipo
#' de tráfico: normal (No) o ataque (Yes).

# load model
#load("randomforest.rds")
load("model_rf.RData")

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

#' Predice tráfico de red maligno usnado el modelo Random Forest
#' @param Tos:numeric indica el tipo de servicio, numeric (scaled and centered to be btw -3 and 3)
#' @param Flags:numeric Indica un estado particular en la conexión TCP (entre 0 y 254 opciones), numeric (scaled and centered to be btw -3 and 3)
#' @param Duration:numeric Indica la duración del Flow en segundos, integer (scaled and centered to be btw -3 and 3)
#' @param Bytes_speed:numeric Indica el número de bytes transmitidos entre la duración el flow, integer (scaled and centered to be btw -3 and 3)
#' @param SrcPt:numeric Indica el número de puerto de origen, numeric (scaled and centered to be btw -3 and 3)
#' @param Packets:numeric Indica el número de paquetes transmitidos en el flow, integer (scaled and centered to be btw -3 and 3)
#' @param Hour:numeric Indica la hora en la que se capturó el flow, integer (scaled and centered to be btw -3 and 3)
#' @param Packets_speed:numeric Indica el número de paquetes transmitidos entre la duración del flow, integer (scaled and centered to be btw -3 and 3)
#' @get /predict
#' @html
#' @response 200 Retorna la clase (Yes o No) que predice el modelo Random Forest; Yes = tráfico malicioso
calculate_prediction <- function(Tos, Flags, Duration, Bytes_speed, SrcPt, Packets, Hour, Packets_speed) {
  
  # make data frame from numeric parameters
  input_data_num <<- data.frame(Tos, Flags, Duration, Bytes_speed, SrcPt, Packets, Hour, Packets_speed,
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
  
  # predict and return result
  pred_rf <<- predict(randomforest, input_data)
  paste("----------------\nTest case predicted to be", as.character(pred_rf), "\n----------------\n")
}