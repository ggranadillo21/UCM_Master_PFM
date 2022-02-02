# UCM_Master_PFM
Información y código en R y Python de los modelos creados en el proyecto de fin de master de la UCM (Big data y Business Analytics)

El presente estudio consiste en analizar un dataset de tráfico de redes IT, que contiene información en formato Netflow de las comunicaciones que ocurren en la red (e.g., protocolos, IP fuente y destino, puerto fuente y destino, paquetes, etc.), con lo que se pretende detectar si la comunicación es benigna o no. El dataset usado es público, ha sido creado por un instituto de investigación en ciberseguridad en Alemania llamado Hochschule Coburg en el 2017 y consta de 4 semanas de comunicaciones internas y externas etiquetadas, que contienen más de 31 millones de observaciones de tráfico normal y ataques de tipo escaneo de puertos, escaneo de hosts, fuerza bruta y denegación de servicio. El dataset puede descargarse en el siguiente enlace: https://www.hs-coburg.de/forschung/forschungsprojekte-oeffentlich/informationstechnologie/cidds-coburg-intrusion-detection-data-sets.html

![image](https://user-images.githubusercontent.com/33121371/152155166-d97e5a3d-e387-4c74-96e2-206788c24f76.png)

El trabajo se ha desarrollado en tres etapas: 

(i)	La primera de ellas corresponde a un análisis de clasificación binaria utilizando técnicas de aprendizaje supervisado en la que se comparan 8 algoritmos (i.e., Regresión Logística, Redes Neuronales, Random Forest, Stochastic Gradient Boosting, eXtreme Gradient Boosting, Support Vector Machine, k-Nearest Neighbors, y Naive Bayes) para seleccionar el que mejor clasifique el tráfico benigno y maligno. 

(ii)	La siguiente etapa corresponde a un análisis multi clase utilizando técnicas de aprendizaje supervisado para crear un modelo que sea capaz, no solamente de predecir cuando el tráfico es benigno o maligno, sino que también pueda clasificar el tipo de ataque al que corresponde el tráfico maligno e.g., ataque de fuerza bruta (BruteForce), ataque de negación de servicio (DoS), escaneo de puertos (PortScan) o escaneo de la red (PingScan). Para esta segunda etapa se comparan 6 algoritmos (i.e., Redes Neuronales, Random Forest, Stochastic Gradient Boosting, eXtreme Gradient Boosting, Support Vector Machine, y k-Nearest Neighbors). 

(iii)	La tercera y última etapa corresponde a un análisis de clasificación binaria utilizando técnicas de aprendizaje no supervisado en el que se comparan 5 algoritmos (i.e., Local Outlier Factor, Isolation Forest, OneClass-SVM, Autoencoders, y K-Means) para crear un modelo que sea capaz de detectar anomalías en el tráfico de red sin tener el dataset etiquetado. 
