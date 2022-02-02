setwd("C:/Users/A663876/Desktop/UCM Máster/8. Machine learning con R/CIDDS/Modelo Categorias/Categorias-muestra3")
install.packages(c("data.table", "caret", "plumber"))
#-------------------------
# Run in Console
library(plumber)
r <- plumb("Evaluar_Modelo_Categorias.R") #llamamos al fichero que evalúa el ensamblado
r$run(port = 8080) #corremos la aplicación en el puerto 8080

#-------------------------
# Run in Terminal
#curl -H "Content-Type: application/json" -X GET -d '{"Tos":-0.6246636,"Flags":0.80241017,"Duration":-0.2036846553,"Bytes_speed":0.062501147,"SrcPt":-0.9710072,"Packets":-0.014895205,"Hour":-1.76092,"Packets_speed":0.4378843}' "http://localhost:8000/predict"

#Caso PortScan
curl -X GET "http://127.0.0.1:8080/predict?Duration=-0.1344098&Proto=-0.3877973&SrcPt=1.48104286&DstPt=0.2692100&Packets=-0.01303201&Bytes=-0.007119512&Flags=-1.8589723&Tos=-0.6041271&Packets_speed=0.4569481&Bytes_speed=-0.003203314&Hour=0.28122244&Minute=0.6740417527&&Second=1.37341712&Day=-1.5183579" -H "accept: */*"


#Caso BruteForce
curl -X GET "http://127.0.0.1:8080/predict?Duration=0.063709297&Proto=-0.3877973&SrcPt=-0.9830600&DstPt=1.1216123&Packets=-0.0016666564&Bytes=-0.007006940&Flags=1.01205348&Tos=-0.6041271&Packets_speed=-0.7105060&Bytes_speed=-0.055986689&Hour=0.04373371&Minute=0.56166130&Second=-0.75967119&Day=0.8906744" -H "accept: */*"


#Caso Normal
curl -X GET "http://127.0.0.1:8080/predict?Duration=1&Proto=1&SrcPt=1&DstPt=1&Packets=1&Bytes=1&Flags=1&Tos=1&Packets_speed=1&Bytes_speed=1&Hour=1&Minute=1&Second=1&Day=1" -H "accept: */*"

#Caso DoS
curl -X GET "http://127.0.0.1:8080/predict?Duration=-0.1274173&Proto=-0.3877973&SrcPt=1.1022955&DstPt=-0.9677811&Packets=-0.008485869&Bytes=-0.007096131&Flags=1.131680&Tos=-0.6041271&Packets_speed=-0.62018785&Bytes_speed=-0.049704712&Hour=-0.19375503&Minute=0.9549929&Second=-1.16322844&Day=0.8906744" -H "accept: */*"

#Caso PingScan
curl -X GET "http://127.0.0.1:8080/predict?Duration=-0.0132075109&Proto=-1.7285734&SrcPt=-0.9839421&DstPt=-0.9706661&Packets=-0.0118954746&Bytes=-0.007118068&Flags=-2.09822441&Tos=-0.6041271&Packets_speed=-0.7158496&Bytes_speed=-0.057051868&Hour=0.04373371&Minute=-0.05643121&Second=-0.5290670&Day=1.6936852" -H "accept: */*"

