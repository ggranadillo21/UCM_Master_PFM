setwd("C:/Users/A663876/Desktop/UCM Máster/8. Machine learning con R/CIDDS")
install.packages(c("data.table", "caret", "plumber"))
#-------------------------
# Run in Console
library(plumber)
r <- plumb("EvaluarModelo.R")
r$run(port = 8000)

#-------------------------
# Run in Terminal
curl -H "Content-Type: application/json" -X GET -d '{"Tos":-0.6246636,"Flags":0.80241017,"Duration":-0.2036846553,"Bytes_speed":0.062501147,"SrcPt":-0.9710072,"Packets":-0.014895205,"Hour":-1.76092,"Packets_speed":0.4378843}' "http://localhost:8000/predict"

#Caso No Anomalia
curl -X GET "http://127.0.0.1:8000/predict?Tos=-0.6246636&Flags=0.80241017&Duration=-0.2036846553&Bytes_speed=0.062501147&SrcPt=-0.9710072&Packets=-0.014895205&Hour=-1.76092&Packets_speed=0.4378843" -H "accept: */*"


#Caso Si Anomalia
curl -X GET "http://127.0.0.1:8000/predict?Tos=-0.6246636&Flags=0.31485104&Duration=-0.2036846553&Bytes_speed=-0.013888437&SrcPt=-0.9879173&Packets=-0.014895205&Hour=-1.76092&Packets_speed=0.4378843" -H "accept: */*"
