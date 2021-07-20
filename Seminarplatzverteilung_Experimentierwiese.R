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

#####Nur Masterstudierende#####
data_master <- filter(data, studiengang==1)

##weniger oder mehr als 30 pro Seminar? bei Erstwunsch##
apply(data_master,MARGIN=2,table)

###mit if noch als String formulieren?###

##wenn mehr als 30##
Master_Seminar1 <- subset(data_master, erstw==1)
Seminar_1 <- sample (Master_Seminar1, size=30)

Master_Seminar2 <- subset(data_master, erstw==2)
Seminar_2 <- sample (Master_Seminar2, size=30)

Master_Seminar3 <- subset(data_master, erstw==3)
Seminar_3 <- sample (Master_Seminar3, size=30)

Master_Seminar4 <- subset(data_master, erstw==4)
Seminar_4 <- sample (Master_Seminar4, size=30)

Master_Seminar5 <- subset(data_master, erstw==5)
Seminar_5 <- sample (Master_Seminar5, size=30)

Master_Seminar6 <- subset(data_master, erstw==6)
Seminar_6 <- sample (Master_Seminar6, size=30)

Master_Seminar7 <- subset(data_master, erstw==7)
Seminar_7 <- sample (Master_Seminar7, size=30)

Master_Seminar8 <- subset(data_master, erstw==8)
Seminar_8 <- sample (Master_Seminar8, size=30)

Master_Seminar9 <- subset(data_master, erstw==9)
Seminar_9 <- sample (Master_Seminar9, size=30)

Master_Seminar10 <- subset(data_master, erstw==10)
Seminar_10 <- sample (Master_Seminar10, size=30)

##wenn weniger als 30##

Seminar_1 <- subset(data_master, data_master$erstw==1)
Seminar_2 <- subset(data_master, data_master$erstw==2)
Seminar_3 <- subset(data_master, data_master$erstw==3)
Seminar_4 <- subset(data_master, data_master$erstw==4)
Seminar_5 <- subset(data_master, data_master$erstw==5)
Seminar_6 <- subset(data_master, data_master$erstw==6)
Seminar_7 <- subset(data_master, data_master$erstw==7)
Seminar_8 <- subset(data_master, data_master$erstw==8)
Seminar_9 <- subset(data_master, data_master$erstw==9)
Seminar_10 <- subset(data_master, data_master$erstw==10)

##In Originaltabelle vermerken##



#####Nur Staatsexamen nächstes Semester#####

data_staatsexamen <- filter(data, data$staatsexamen==1)


#####Sortieren nach Semesteranzahl#####

#hier noch filtern? mit data_semester#

arrange (data_semester, -data$fachsemester)







data_wm_1 <-filter(data, data$studiengang==1)

if (data$studiengang==1){
  for (i in 1:seminaranzahl){
    cat("seminar",i,sep="_")
    
  } 
}

cat("seminar",i,sep="_")

aber<- c(1,2,3,4,4,5,6,4,5,6,3,4)
doch<- sample(aber, size=7, replace=FALSE)
