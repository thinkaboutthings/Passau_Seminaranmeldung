###############packages############################################################
library(dplyr)
library(plyr)

#######################Erstellen eines Testdatensatzes######################################################

#Gr??e des Datensatz
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

####Pro Seminar####
###Erstwunsch###
##Seminar1##
if ((length(data_master$erstw[data_master$erstw==1]))>30) {
  Master_Seminar1 <- subset(data_master, erstw==1)
  Seminar_1 <- sample (Master_Seminar1, size=30)
} else {
  Seminar_1 <- subset(data_master, data_master$erstw==1)
}

##Seminar2##
if ((length(data_master$erstw[data_master$erstw==2]))>30) {
  Master_Seminar2 <- subset(data_master, erstw==2)
  Seminar_2 <- sample (Master_Seminar2, size=30)
} else {
  Seminar_2 <- subset(data_master, data_master$erstw==2)
}

##Seminar3##
if ((length(data_master$erstw[data_master$erstw==3]))>30) {
  Master_Seminar3 <- subset(data_master, erstw==3)
  Seminar_3 <- sample (Master_Seminar3, size=30)
} else {
  Seminar_3 <- subset(data_master, data_master$erstw==3)
}

##Seminar4##
if ((length(data_master$erstw[data_master$erstw==4]))>30) {
  Master_Seminar4 <- subset(data_master, erstw==4)
  Seminar_4 <- sample (Master_Seminar4, size=30)
} else {
  Seminar_4 <- subset(data_master, data_master$erstw==4)
}

##Seminar5##
if ((length(data_master$erstw[data_master$erstw==5]))>30) {
  Master_Seminar5 <- subset(data_master, erstw==5)
  Seminar_5 <- sample (Master_Seminar5, size=30)
} else {
  Seminar_5 <- subset(data_master, data_master$erstw==5)
}

##Seminar6##
if ((length(data_master$erstw[data_master$erstw==6]))>30) {
  Master_Seminar6 <- subset(data_master, erstw==6)
  Seminar_6 <- sample (Master_Seminar6, size=30)
} else {
  Seminar_6 <- subset(data_master, data_master$erstw==6)
}

##Seminar7##
if ((length(data_master$erstw[data_master$erstw==7]))>30) {
  Master_Seminar7 <- subset(data_master, erstw==7)
  Seminar_7 <- sample (Master_Seminar7, size=30)
} else {
  Seminar_7 <- subset(data_master, data_master$erstw==7)
}

##Seminar8##
if ((length(data_master$erstw[data_master$erstw==8]))>30) {
  Master_Seminar8 <- subset(data_master, erstw==8)
  Seminar_8 <- sample (Master_Seminar8, size=30)
} else {
  Seminar_8 <- subset(data_master, data_master$erstw==8)
}

##Seminar9##
if ((length(data_master$erstw[data_master$erstw==9]))>30) {
  Master_Seminar9 <- subset(data_master, erstw==9)
  Seminar_9 <- sample (Master_Seminar9, size=30)
} else {
  Seminar_9 <- subset(data_master, data_master$erstw==9)
}

##Seminar10##
if ((length(data_master$erstw[data_master$erstw==10]))>30) {
  Master_Seminar10 <- subset(data_master, erstw==10)
  Seminar_10 <- sample (Master_Seminar10, size=30)
} else {
  Seminar_10 <- subset(data_master, data_master$erstw==10)
}


###In Originaltabelle vermerken###
##neue Variable Platz_erhalten##

Seminar_1 <- Seminar_1 %>% mutate(Platz_erhalten = 1)
Seminar_2 <- Seminar_2 %>% mutate(Platz_erhalten = 2)
Seminar_3 <- Seminar_3 %>% mutate(Platz_erhalten = 3)
Seminar_4 <- Seminar_4 %>% mutate(Platz_erhalten = 4)
Seminar_5 <- Seminar_5 %>% mutate(Platz_erhalten = 5)
Seminar_6 <- Seminar_6 %>% mutate(Platz_erhalten = 6)
Seminar_7 <- Seminar_7 %>% mutate(Platz_erhalten = 7)
Seminar_8 <- Seminar_8 %>% mutate(Platz_erhalten = 8)
Seminar_9 <- Seminar_9 %>% mutate(Platz_erhalten = 9)
Seminar_10 <- Seminar_10 %>% mutate(Platz_erhalten = 10)


Zuteilung_master1 <- join_all(list(Seminar_1,Seminar_2,Seminar_3,Seminar_4, Seminar_5, Seminar_6, Seminar_7, Seminar_8, Seminar_9, Seminar_10), by = 'matrikelnummer', type = 'full')


###Zweitwunsch###
##Seminar1## 


30-(length(Seminar_1)) #einsetzen#
Master_Seminar1 #umbennen da Zweitwunsch#

if ((length(data_master$erstw[data_master$erstw==1]))>30) {
  Master_Seminar1 <- subset(data_master, erstw==1)
  Seminar_1 <- sample (Master_Seminar1, size=30)
} else {
  Seminar_1 <- subset(data_master, data_master$erstw==1)
}

#####Nur Staatsexamen nächstes Semester#####

data_staatsexamen <- filter(data, data$staatsexamen==1)


#####Sortieren nach Semesteranzahl#####

#hier noch filtern? mit data_semester#

arrange (data_semester, -data$fachsemester)


#offene Seminarplätze#
30-(length(Seminar_1)) 
30-(length(Seminar_2)) 
30-(length(Seminar_3)) 
30-(length(Seminar_4)) 
30-(length(Seminar_5)) 
30-(length(Seminar_6)) 
30-(length(Seminar_7)) 
30-(length(Seminar_8)) 
30-(length(Seminar_9)) 
30-(length(Seminar_10)) 

