###############packages############################################################
library(dplyr)

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

#hilfsvektor für staatsexamen und studiengang, Wahrscheinlichkeit kann hier adjustiert werden
hilf_studiengang <-c(0,0,0,0,0,0,0,0,0,0,0,1,1,1)
hilf_staatsexamen <-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1)

#Füllen mit Zufallswerten
for (i in 1:n){
  data$matrikelnummer[i]<- 41372+i
  data$erstw[i] <- floor(runif(1,1,11))
  data$zweitw[i] <- floor(runif(1,1,11))
  data$drittw[i] <- floor(runif(1,1,11))
  data$studiengang[i] <- sample(x=hilf_studiengang,size=1,replace=TRUE)
  data$staatsexamen[i] <- sample(x=hilf_staatsexamen,size=1,replace=TRUE)
  data$fachsemester[i] <- floor(runif(1,0,7))
}
rm(i,hilf_staatsexamen,hilf_studiengang)


#####################Erstellen der leeren Seminardatensätze###############################

#hilfsweise manuell erstellt
names <- c("matrikelnummer","erstw","zweitw","drittw","studiengang","staatsexamen","fachsemester")

Seminar_1 <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(seminar_1) <- names

Seminar_2 <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(seminar_2) <- names

Seminar_3 <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(seminar_3) <- names

Seminar_4 <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(seminar_4) <- names

Seminar_5 <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(seminar_5) <- names

Seminar_6 <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(seminar_6) <- names

Seminar_7 <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(seminar_7) <- names

Seminar_8 <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(Seminar_8) <- names

Seminar_9 <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(seminar_9) <- names

Seminar_10 <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(seminar_10) <- names

rm(names)

#################Zuteilung der Master-Studierenden#####################################################