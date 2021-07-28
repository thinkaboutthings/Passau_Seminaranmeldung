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

##Funktion für Zuteilung## 
#erstw ersetzen funktioniert aber nicht##

#ERSTWUNSCH#
Zuteilung_erstw <- function(Dataframe, Seminar, x) { 
  
  if ((length(Dataframe$erstw[Dataframe$erstw==x]))>(30-(length(Seminar)))) {
    helper <- subset(Dataframe, erstw==x)
    Seminar <- sample (helper, size=(30-(length(Seminar))))
  } else {
    Seminar <- subset(Dataframe, Dataframe$erstw==x)
  } 
  return (Seminar)
}


#ZWEITWUNSCH#
Zuteilung_zweitw <- function(Dataframe, Seminar, x) { 
  
  if ((length(Dataframe$zweitw[Dataframe$zweitw==x]))>(30-(length(Seminar)))) {
    helper <- subset(Dataframe, zweitw==x)
    Seminar <- sample (helper, size=(30-(length(Seminar))))
  } else {
    Seminar <- subset(Dataframe, Dataframe$zweitw==x)
  } 
  return (Seminar)
}

#DRITTWUNSCH#
Zuteilung_drittw <- function(Dataframe, Seminar, x) { 
  
  if ((length(Dataframe$drittw[Dataframe$drittw==x]))>(30-(length(Seminar)))) {
    helper <- subset(Dataframe, drittw==x)
    Seminar <- sample (helper, size=(30-(length(Seminar))))
  } else {
    Seminar <- subset(Dataframe, Dataframe$drittw==x)
  } 
  return (Seminar)
}

#Anwendung#
#Erstwunsch#

  Seminar_1 <- Zuteilung_erstw (data_master, Seminar_1, 1)
  Seminar_2 <- Zuteilung_erstw (data_master, Seminar_2, 2)
  Seminar_3 <- Zuteilung_erstw (data_master, Seminar_3, 3)
  Seminar_4 <- Zuteilung_erstw (data_master, Seminar_4, 4)
  Seminar_5 <- Zuteilung_erstw (data_master, Seminar_5, 5)
  Seminar_6 <- Zuteilung_erstw (data_master, Seminar_6, 6)    
  Seminar_7 <- Zuteilung_erstw (data_master, Seminar_7, 7)
  Seminar_8 <- Zuteilung_erstw (data_master, Seminar_8, 8)
  Seminar_9 <- Zuteilung_erstw (data_master, Seminar_9, 9)
  Seminar_10 <- Zuteilung_erstw (data_master, Seminar_10, 10)
  
#Vermerken#
  
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
  
  # Vermerk in data_master #
  Zuteilung_master1 <- join_all(list(Seminar_1,Seminar_2,Seminar_3,Seminar_4, Seminar_5, Seminar_6, Seminar_7, Seminar_8, Seminar_9, Seminar_10, data_master), by = 'matrikelnummer', type = 'full')
  data_master <- merge (data_master [, 'matrikelnummer', drop = FALSE], Zuteilung_master1, by='matrikelnummer', all.x = TRUE) 
  
  

#ZWEITWUNSCH#
  
#DRITTWUNSCH#


###In Tabelle vermerken###
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

# Vermerk in data_master #
Zuteilung_master1 <- join_all(list(Seminar_1,Seminar_2,Seminar_3,Seminar_4, Seminar_5, Seminar_6, Seminar_7, Seminar_8, Seminar_9, Seminar_10, data_master), by = 'matrikelnummer', type = 'full')
data_master <- merge (data_master [, 'matrikelnummer', drop = FALSE], Zuteilung_master1, by='matrikelnummer', all.x = TRUE) 



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

