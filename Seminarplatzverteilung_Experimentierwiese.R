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

#################Zuteilung Studierenden###############

####Datensätze für Studiengänge ###
data_master <- filter(data, studiengang==1)
data_staatsexamen <- filter(data, data$staatsexamen==1)


##Funktion für Zuteilung## 
###MASTER###

#ERSTWUNSCH#

Zuteilung_erstw_m <- function(Dataframe, x) { 
  
  Seminar <- subset (Dataframe)
  if ((length(Dataframe$erstw[Dataframe$erstw==x]))>(30-(length(Seminar)))) {
    helper_1 <- subset(Dataframe, erstw==x)
    Seminar <- sample (helper_1, size=(30-(length(Seminar))))
  } else {
    Seminar <- subset(Dataframe, Dataframe$erstw==x)
  } 
  
  Seminar <- Seminar %>% mutate(Platz_erhalten = x)
  data_master <- join_all(list(Seminar, data_master) , by = 'matrikelnummer', type = 'full')
  
  
  
  return (data_master)
  
}

######
sum(data_master$Platz_erhalten>0)

#ZWEITWUNSCH#
Zuteilung_zweitw_m <- function(Dataframe, x) { 
  Seminar <- subset (Dataframe)
  
  if ((sum(data_master$Platz_erhalten<1)))>(0) {
    helper_1 <- subset(Dataframe, zweitw==x)
    Seminar <- sample (helper_1, size=(30-(length(Seminar))))
  } else {
    Seminar <- subset(Dataframe, Dataframe$zweitw==x)
  } 
  
  Seminar <- Seminar %>% mutate(Platz_erhalten = x)
  data_master <- join_all(list(Seminar, data_master) , by = 'matrikelnummer', type = 'full')
  
  
  
  return (data_master)
}

#DRITTWUNSCH#
Zuteilung_drittw_m <- function(Dataframe, x) { 
  Seminar <- subset (Dataframe)
  
  if ((length(Dataframe$drittw[Dataframe$drittw==x]))>(30-(length(Seminar)))) {
    helper_1 <- subset(Dataframe, drittw==x)
    Seminar <- sample (helper_1, size=(30-(length(Seminar))))
  } else {
    Seminar <- subset(Dataframe, Dataframe$drittw==x)
  } 
  
  Seminar <- Seminar %>% mutate(Platz_erhalten = x)
  data_master <- join_all(list(Seminar, data_master) , by = 'matrikelnummer', type = 'full')
  
  
  
  return (data_master)
}

###STAATSEXAMEN###

#ERSTWUNSCH#

Zuteilung_erstw_s <- function(Dataframe, x) { 
  
  Seminar <- subset (Dataframe)
  if ((length(Dataframe$erstw[Dataframe$erstw==x]))>(30-(length(Seminar)))) {
    helper_1 <- subset(Dataframe, erstw==x)
    Seminar <- sample (helper_1, size=(30-(length(Seminar))))
  } else {
    Seminar <- subset(Dataframe, Dataframe$erstw==x)
  } 
  
  Seminar <- Seminar %>% mutate(Platz_erhalten = x)
  data_master <- join_all(list(Seminar, data_master) , by = 'matrikelnummer', type = 'full')
  
  
  
  return (data_master)
  
}

#ZWEITWUNSCH#
Zuteilung_zweitw_s <- function(Dataframe, x) { 
  Seminar <- subset (Dataframe)
  
  if ((length(Dataframe$zweitw[Dataframe$zweitw==x]))>(30-(length(Seminar)))) {
    helper_1 <- subset(Dataframe, zweitw==x)
    Seminar <- sample (helper_1, size=(30-(length(Seminar))))
  } else {
    Seminar <- subset(Dataframe, Dataframe$zweitw==x)
  } 
  
  Seminar <- Seminar %>% mutate(Platz_erhalten = x)
  data_master <- join_all(list(Seminar, data_master) , by = 'matrikelnummer', type = 'full')
  
  
  
  return (data_master)
}

#DRITTWUNSCH#
Zuteilung_drittw_s <- function(Dataframe, x) { 
  Seminar <- subset (Dataframe)
  
  if ((length(Dataframe$drittw[Dataframe$drittw==x]))>(30-(length(Seminar)))) {
    helper_1 <- subset(Dataframe, drittw==x)
    Seminar <- sample (helper_1, size=(30-(length(Seminar))))
  } else {
    Seminar <- subset(Dataframe, Dataframe$drittw==x)
  } 
  
  Seminar <- Seminar %>% mutate(Platz_erhalten = x)
  data_master <- join_all(list(Seminar, data_master) , by = 'matrikelnummer', type = 'full')
  
  
  
  return (data_master)
}


###Anwendung###
#Master#
for (i in 1:10){
  data_master <-  Zuteilung_erstw_m (data_master, i)
}

for (i in 1:10){
  data_master <-  Zuteilung_zweitw_m (data_master, i)
}

for (i in 1:10){
  data_master <-  Zuteilung_drittw_m (data_master, i)
}

#Staatsexamen#
for (i in 1:10){
  data_staatsexamen <-  Zuteilung_erstw_s (data_master, i)
}

for (i in 1:10){
  data_staatsexamen <-  Zuteilung_zweitw_s (data_master, i)
}

for (i in 1:10){
  data_staatsexamen <-  Zuteilung_drittw_s (data_master, i)
}






#####Sortieren nach Semesteranzahl#####
 
#hier noch filtern? mit data_semester#

arrange (data_semester, -data$fachsemester)



