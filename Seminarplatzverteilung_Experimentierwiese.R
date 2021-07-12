#######################Erstellen eines Testdatensatzes######################################################

#Größe des Datensatz
n<- 300

#leerer Datensatz
matrikelnummer <- seq(1,n,by=1)
erstw <-seq(1,n,by=1)
zweitw <- seq(1,n,by=1)
drittw <- seq(1,n,by=1)
studiengang <- seq(1,n,by=1)
fachsemester <- seq(1,n,by=1)
data <- data.frame(matrikelnummer,erstw,zweitw,drittw,studiengang,fachsemester)
rm(drittw,erstw,fachsemester,matrikelnummer,studiengang,zweitw)

#Füllen mit Zufallswerten
for (i in 1:n){
  data$matrikelnummer[i]<- 41372+i
  data$erstw[i] <- floor(runif(1,1,11))
  data$zweitw[i] <- floor(runif(1,1,11))
  data$drittw[i] <- floor(runif(1,1,11))
  data$studiengang[i] <- floor(runif(1,0,2))
  data$staatsexamen[i] <- floor(runif(1,0,2))
  data$fachsemester[i] <- floor(runif(1,0,7))
}
rm(i)

