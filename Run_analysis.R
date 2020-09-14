################################################################################
##Descripción de la actividad.
################################################################################
##El siguiente script hace lo que se expone a continuación:
##1. Fusiona los conjuntos de datos de entrenamiento y prueba de 
##   dos grupos de individuos, para reconocer la actividad humana con el
##   uso de un teléfono inteligente.
##2. Extrae las medidas de desviación estándar y media de cada variable.
##3. Utiliza nombres de actividades descriptivos para nombrar las actividades
##   en el conjunto de datos.
##4. Etiqueta el conjunto de datos con nombres de variables descriptivas.
##5. Crea un segundo conjunto de datos ordenado e independiente, con los datos 
##   del numeral 4, con el promedio de cada variable, para cada actividad y
##   tema.
################################################################################
##Para lograr lo anterior, procedemos así:
################################################################################
##0. Descargar y leer los archivos.
################################################################################
##0.1 Traemos el conjunto de datos arrojado en la web.Para esto le asignamos un nombre.
dataurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
##0.2 Ubicamos el directorio en el que estamos trabajando y le asignamos un nombre
##    para llamarlo más rápidamente cuanto descarguemos el archivo.
path <- getwd()
##0.3 Descargamos el arhivo y lo ubicamos en el directorio donde vamos a trabajar.
download.file(dataurl, file.path(path, "getdata_projectfiles_UCI HAR Dataset.zip"))
##0.4 Descomprimimos el archivo en el directorio donde se está trabajando.
unzip(zipfile = "getdata_projectfiles_UCI HAR Dataset.zip", exdir = "./getdata_projectfiles_UCI HAR Dataset")
################################################################################
##1. Fusionar los datos.
################################################################################
##1.1 Creamos las variables para los datos de los sujetos de entrenamiento.
xtrain <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
ytrain <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
strain <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
##1.2 Creamos las variables para los datos de los sujetos de prueba.
xtest <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
ytest <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
stest <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
##1.3 Creamos la variable para los datos de características.
features <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt")
colnames(features)
names <- features[,2]
##1.4 Creamos la variable para los datos de los niveles de actividades.
activityLabels <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")
#1.5 Asignamos nombres a las columnas de las variables.
colnames(xtrain)
colnames(xtrain) <- names
colnames(ytrain)
colnames(ytrain) <- "activityID"
colnames(strain) <- "subjectID"
colnames(xtest) <- features[,2]
colnames(ytest) <- "activityID"
colnames(stest) <- "subjectID"
colnames(activityLabels) <- c("activityID", "activityType")
#1.6 Comparamos los datos de entrenamiento
datatrain <- cbind(ytrain, strain, xtrain)
dim(datatrain)
colnames(datatrain)
#1.7 Comparamos los datos de prueba
datatest <- cbind(ytest, stest, xtest)
dim(datatest)
colnames(datatest)
#1.8 Creamos un arreglo con los datos de entrenamiento y prueba.
data <- rbind(datatrain, datatest)
dim(data)
colnames(data)
################################################################################
##2. Extraer las medidas de desviación estándar y media de cada variable.
################################################################################
#2.1 Asignamos los nombres de las columnas a una variable
colNames <- colnames(data)
#2.2 Creamos un vector para definir ID, media y sd.
meanystd <- (grepl("activityID", colNames) | grepl("subjectID", colNames) | grepl("mean..", colNames) | grepl("std...", colNames))
################################################################################
##3.Utilizamos nombres de actividades descriptivos 
################################################################################
#3.1 Tomamos los datos que tienen "activityID", "subjectID", "mean..", "std..." (TRUE). 
dataMeanStd <- data[ , meanystd == TRUE] 
dim(dataMeanStd)
colnames(dataMeanStd) 
#3.2 Unimos "dataMeanStd" y "activityLabels" por "activityID"
M <- merge(dataMeanStd, activityLabels,by = "activityID", all.x = TRUE)
colnames(M)
################################################################################
##4. y 5. Etiquetamos el conjunto de datos y creamos un segundo conjunto de 
##        datos ordenado
################################################################################
##4.1 Instalamos el paquete que facilita la transformación de datos según el 
##    ancho y alto, y ejecutamos la librería.
install.packages("reshape2")
library(reshape2)
#4.2 Seleccionamos Los elementos que concuerdan.
average_columns<-colnames(M[,3:68])
#5.1 Organizamos los datos con "melt"
M_ordering<- melt(M,id=c("subjectID","activityID"),measure.vars=average_columns)
#5.2 Preparamos el archivo final.
M_final <- dcast(M_ordering, subjectID + activityID ~ variable, mean)
#5.3 Escribimos el archivo final.
write.table(M_final, "M_final.txt", row.names = FALSE, quote = FALSE)

