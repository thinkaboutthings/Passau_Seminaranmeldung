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
hilf_studiengang <-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1)
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
seminaranzahl <- 10
#hilfsweise manuell erstellt
names <- c("matrikelnummer","erstw","zweitw","drittw","studiengang","staatsexamen","fachsemester")
raw <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(raw) <- names

for (i in 1:seminaranzahl){
  assign(paste("Seminar",i, sep = "_"),raw)
}
rm(i,names,raw)
#################Zuteilung der Master-Studierenden#####################################################

data_wm_1 <-filter(data, data$studiengang==1)

if (data$studiengang==1){
  for (i in 1:seminaranzahl){
    cat("seminar",i,sep="_")
    
  } 
}

cat("seminar",i,sep="_")

aber<- c(1,2,3,4,4,5,6,4,5,6,3,4)
doch<- sample(aber, size=7, replace=FALSE)
