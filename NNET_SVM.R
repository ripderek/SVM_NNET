#Instalar le paquete MASS, donde se encuentra el conjunto de datos de Boston.
if (!requireNamespace("MASS", quietl = TRUE)) install.packages("MASS")
if (!requireNamespace("e1071", quietl = TRUE)) install.packages("e1071")
if (!requireNamespace("nnet", quietl = TRUE)) install.packages("nnet")
if (!requireNamespace("neuralnet", quietl = TRUE)) install.packages("neuralnet")
if (!requireNamespace("class", quietl = TRUE)) install.packages("class")

#Cargar librerías instaladas
library("MASS")
library("e1071")
library("nnet")
library("neuralnet")
library("class")

#Cargar el conjunto de datos de Boston de la liberería MASS 
dfBoston = Boston

#El preprocesado de datos constará de la división de los registros del cojunto de datos de Boston.
#Para ello, se dividirá en conjunto de entrenamiento y prueba (train, test).
N_OBSERVACIONES = NROW(dfBoston)
PORCENTAJE_ENTRENAMIENTO = 70
N_DATOS_ENTRENAMIENTO = as.integer(NROW(dfBoston) * PORCENTAJE_ENTRENAMIENTO/100)

#Ya que la variable explicativa que será considerada para el desarrollo es el precio medio de la 
#vivienda equivale a x. Obteniendo así, lo siguiente 
boston.train = dfBoston[indice.entrenamiento, ]
boston.test = dfBoston[-indice.entrenamiento, ]

#Debido a que se requiere transformar el conjunto de datos a una matriz para utilizarla en la red
#neuronal, se procede a realizarlo
mtBoston.train = as.matrix(boston.train)
mtBoston.test = as.matrix(boston.test)


#Se procede a seleccionar los algoritmos que serán utilizados. Estos serán SVM (Support Vector Machine) 
#y Redes neuronales.

#SVM (Support Vector Machine)
#La primera prueba que será realizada haciendo uso de SVM, será con un modelo de kernel lineal.
#El objetivo es determinar si una función lineal permite representar y predecir los datos correctamente.
x.svm = svm(boston.train[, 14]~ ., data=boston.train, type="eps-regression", kernel="linear", epsilon=0.02,cost=100,scale=F)
prediccion.svm = predict(x.svm, boston.test)

#Así, al aplicar un modelo lineal mediante SVM se obtiene una correlación equivalente a 0.99
#lo que significa que la predicción es bastante buena.
corr.svm = cor(prediccion.svm, boston.test[, 14])
plot(boston.test[,14], prediccion.svm)

#Los resultados obtenidos al aplicar SVM logran satisfacer una función lineal, donde existe una relación
#notable entre los valores de x e y.
mean(abs(boston.test[, 14] - prediccion.svm))
ord=sort(c(boston.test[, 14], boston.train[, 14]),index.return=T)
ent.x=ord$x
ent.ind=ord$ix
ent.y=c(prediccion.svm, boston.train[, 14])[ent.ind]
plot(ent.x,ent.y,pch=".",type="l")

#Se procede a utilzar la red neuronal. RED NEURONAL
#Al aplicar cinco neuronas escondidas se obtiene una aproximación x = y. Se debe realizar varias
#pruebas puesto que el modelo cambia con cada ejecución.
boston.nnet = nnet(mtBoston.train[,14]~mtBoston.train[,1:13],rang=0.1,size=5,linout=T,maxit = 100)

#Al graficar el modelo neuronal se visualiza una relación entre las dimensiones x e y que no es tan clara
#o defindia. Sin embargo, al calcular a correlación entre los valores originales y los predecidos con la
#alicación de red nueronal, se obtiene un valor cercano al 1. Siendo esta equivalente a 0.90.
prediccion.nnet = predict(boston.nnet, mtBoston.test)
corr.rn = cor(prediccion.nnet, boston.test[, 14])

plot(mtBoston.test[,14], prediccion.nnet)


#Para comprobar si es posible obtener un mejor modelo se aplica un decaimiento de 10e-2 junto
#a nueve neuronas ocultas  y un límite máximo de 2000 ciclos o iteraciones. 
boston.nnetdec =  nnet(mtBoston.train[,14]~mtBoston.train[,1:13],rang=0.5,size=9,linout=T,decay
       =10e-2,maxit=2000)

prediccion.nnetdecay = predict(boston.nnetdec, boston.test)

#Al entrenar esta red neuronal se obtienen mejores resultados, que se pueden visualizar
# de forma gráfica y por el resultado hayado en la correlación siendo equivalente a 0.96.
corr.rndecay = cor(prediccion.nnetdecay, mtBoston.test[, 14])
plot(boston.test[, 14], prediccion.nnetdecay)

#Formar un dataframe con la correlación obtenida por la aplicación de los algoritmos.
dfCorrelacion = data.frame(algoritmo = c("SVM", "RN", "RNDecay"), correlacion = c(corr.svm, corr.rn, corr.rndecay))

barplot(dfCorrelacion$correlacion, names.arg = c("SVM", "RN", "RNDecay"), 
        xlab = "Algoritmo", 
        ylab = "Correlación", 
        main = "Comparación de algoritmos",
        col= "#39BFD8")