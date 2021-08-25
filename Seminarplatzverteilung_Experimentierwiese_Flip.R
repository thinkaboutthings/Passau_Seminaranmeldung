###############packages############################################################
library(plyr)
library(dplyr)

#######################Erstellen eines Testdatensatzes######################################################

#Groesse des Datensatz
n<- 300

#leerer Datensatz
matrikelnummer <- seq(1,n,by=1)
erstw <-seq(1,n,by=1)
zweitw <- seq(1,n,by=1)
drittw <- seq(1,n,by=1)
studiengang <- seq(1,n,by=1)
fachsemester <- seq(1,n,by=1)
Platz_erhalten<- rep(0,n)
data <- data.frame(matrikelnummer,erstw,zweitw,drittw,studiengang,fachsemester,Platz_erhalten)
rm(drittw,erstw,fachsemester,matrikelnummer,studiengang,zweitw,Platz_erhalten)

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
rm(i,hilf_staatsexamen,hilf_studiengang,n)


############################################Funktionen#####################################

Zuteilung_1 <- function(Dataframe,gruppe, x) {
  helper_all<- Dataframe
  helper_group <- subset(helper_all,sort==gruppe)
  
  #erstwunsch 
  helper_1 <- subset(helper_group, Platz_erhalten==0)
  helper_1 <- subset(helper_1, erstw==x)
  
  if ((length(helper_1$matrikelnummer))>(30-(sum(helper_all$Platz_erhalten==x)))) {
    helper_1$Platz_erhalten[sample(length(helper_1$matrikelnummer),(30-sum(helper_all$Platz_erhalten==x)))] <- x
  } else {
    helper_1 <- helper_1 %>% mutate(Platz_erhalten = x)
  } 
  
  
  helper_all <- join_all(list(helper_1, helper_all) , by = 'matrikelnummer', type = 'full')

  return (helper_all)
  
}

Zuteilung_2 <- function(Dataframe,gruppe, x) {
  helper_all<- Dataframe
  helper_group <- subset(helper_all,sort==gruppe)
  
  #zweitwunsch
  helper_1 <- subset(helper_group, Platz_erhalten==0)
  helper_1 <- subset(helper_1, zweitw==x)
  
  if ((length(helper_1$matrikelnummer))>(30-(sum(helper_all$Platz_erhalten==x)))) {
    helper_1$Platz_erhalten[sample(length(helper_1$matrikelnummer),(30-sum(helper_all$Platz_erhalten==x)))] <- x
  } else {
    helper_1 <- helper_1 %>% mutate(Platz_erhalten = x)
  } 
  
  helper_all <- join_all(list(helper_1, helper_all) , by = 'matrikelnummer', type = 'full')

    return (helper_all)
  
}

Zuteilung_3 <- function(Dataframe,gruppe, x) {
  helper_all<- Dataframe
  helper_group <- subset(helper_all,sort==gruppe)
  
  #drittwunsch
  helper_1 <- subset(helper_group, Platz_erhalten==0)
  helper_1 <- subset(helper_1, drittw==x)
  
  if ((length(helper_1$matrikelnummer))>(30-(sum(helper_all$Platz_erhalten==x)))) {
    helper_1$Platz_erhalten[sample(length(helper_1$matrikelnummer),(30-sum(helper_all$Platz_erhalten==x)))] <- x
  } else {
    helper_1 <- helper_1 %>% mutate(Platz_erhalten = x)
  } 
  
  
  helper_all <- join_all(list(helper_1, helper_all) , by = 'matrikelnummer', type = 'full')
  
  
  return (helper_all)
  
}
#test: data<-Zuteilung(data,1,1)



#######################################Sortierung Erstellen#############################################

data$sort<-data$fachsemester
helper_1<-subset(data,studiengang==1)
helper_1$sort<-"m"
helper_2<-subset(data, staatsexamen==1)
helper_2$sort<-"s"
data<- join_all(list(helper_1,helper_2,data),by='matrikelnummer',type='full')
rm(helper_1,helper_2)


##################################Anwendung#########################################
#Master

for (i in 1:10){
  data <-  Zuteilung_1(data,"m",i)
}
rm(i)
for (i in 1:10){
  data <-  Zuteilung_2(data,"m",i)
}
rm(i)
for (i in 1:10){
  data <-  Zuteilung_3(data,"m",i)
}
rm(i)
#Staatsexamen
for (i in 1:10){
  data <-  Zuteilung_1(data,"s",i)
}
rm(i)

for (i in 1:10){
  data <-  Zuteilung_2(data,"s",i)
}
rm(i)

for (i in 1:10){
  data <-  Zuteilung_3(data,"s",i)
}
rm(i)


#nach Fachsemester
for (k in 20:2){
  for (i in 1:10){
    data <-  Zuteilung_1(data,k,i)
  }
}
for (k in 20:2){
  for (i in 1:10){
    data <-  Zuteilung_2(data,k,i)
  }
}
for (k in 20:2){
  for (i in 1:10){
    data <-  Zuteilung_3(data,k,i)
  }
}
rm(i,k)

#####################################Zusammenfassung######################################
Seminar<-c(0:10)
Teilnehmendenzahl<- c(0:10)
kontrolle<- data.frame(Seminar,Teilnehmendenzahl)
rm(Seminar, Teilnehmendenzahl)
for (i in 1:11){
  kontrolle$Seminar[i]<-i-1
  kontrolle$Teilnehmendenzahl[i]<- sum(data$Platz_erhalten==i-1)
}
rm(i)
sum(kontrolle$Teilnehmendenzahl)
