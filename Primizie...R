library("zoo")
library("plot3D")
library("magick")
library("rgl")
library("tseries")
library("lmtest")
library(car)
library(DAAG)
library(corrplot)
library(tidyverse)
library(corrgram)

#DATI

####Daset utilizzato

(campione_CD <- read.csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/UFFICIALE/Consumo-main/durevoli.csv")[18:56])
(campione_C_ND <- read.csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/UFFICIALE/Consumo-main/non_durevoli.csv")[18:56])

(campione_CD <- colSums(campione_CD))
(campione_C_ND <- colSums(campione_C_ND))
(campione_C <- campione_C_ND + campione_CD)

(campione_Y <- read.csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/UFFICIALE/Consumo-main/gdp_ita.csv")[18:56])
(campione_T <- read.csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/UFFICIALE/Consumo-main/gettito2.csv")[2:40])
(campione_DP <- read.csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/UFFICIALE/Consumo-main/inflazione.csv")[14:130])
#AGG: Wealth..
#AGG: Real interest rate..

####Le variabili di interesse.

#Consumi

(C = as.numeric(campione_C))

#Reddito

(Y = as.numeric(campione_Y))
#Unica variabile, reddito disponibile?
#(...)

#Pressione fiscale
#completare la tassazione.

(Tax = as.numeric(campione_T))
t_perc <- Tax/Y

#Prezzi

a = c()
infl <- c()
counter <- 0
for(i in 1:length(campione_DP)){
  counter <- counter+1
  if(counter%%3==0){
    counter <- 0
    infl <- c(infl, prod(as.numeric(a)))
    a <- c()
  }
  else{
    a <- c(a,campione_DP[i])
  }
}
infl <- infl+1

#Tempo

Time <- 1:length(Y)
Dtime = 1:length(DC)

####La gestione delle variabili d'interesse.

##La trasformazione in variabili reali.

(C = C/infl)
(Y = Y/infl)
(Tax = Tax/infl)

##La trasformazione in variabili logaritmiche.

logC = log(C)
logY = log(Y)
logT = log(Tax)

##Il calcolo delle differenze prime di ciascuna variabile.

DC = c()
DY = c()
DT = c()

n = length(logY)

for(i in 1:n-1){
  DC <- c(DC, c(logC[i+1]-logC[i]))
}
for(i in 1:n-1){
  DY <- c(DY, c(logY[i+1]-logY[i]))
}
for(i in 1:n-1){
  DT <- c(DT, c(logT[i+1]-logT[i]))
}

#diff.ts() ..verificare 

Dtime = 1:length(DC)

##Il calcolo delle medie mobili di ciascuna variabile.

DCM = c(rollmean(DC, 4),rollmean(DC, 4)[32:34] )
DTM = c(rollmean(DT, 4),rollmean(DT, 4)[32:34] )
DYM = c(rollmean(DY, 4),rollmean(DY, 4)[32:34] )

####Rappresentazione delle variabili di interesse.

##Rappresentazione tabellare

#Adattare dataframe come modellato dal documento..
(dati <- data.frame(consumo=c(C), reddito=c(Y), tasse=c(Tax)))
(dati7 <- data.frame(consumo=c(C), reddito=c(Y), tasse=c(t_perc)))
(dati1=data.frame(VariazioniC=DC, VariazioniY=DY, VariazioniT=DT))

#matrici
(datimat <- as.matrix(dati))

#liste

##Rappresentazioni grafiche

#Valori assoluti
plot(Time, C)
plot(Time,Y)
plot(Time, Tax)

#Valori logaritmici
plot(Time, logC)
plot(Time, logY)
plot(Time, logT)

#Differenze prime

plot(Dtime, DCM, type="l", col="red")
abline(a=0, b=0, col = "black")

plot(Dtime, DYM, type = "l", col='green')
abline(a=0, b=0, col = "black")

plot(Dtime, DTM, type="l", col='blue')
abline(a=0, b=0, col = "black")


#LA COSTRUZIONE DEL MODELLO

#I FASE: la determinazione della forma funzionale.
#--
#1. DC=f(DY,DTax) 

####Lo studio della funzione ipotizzata

eq = function(b,b1,b2,x=0,z=0){b+b1*log(x)+2*b1*log(x)+b2*log((1-z))}
eq1 = function(b,b1,x){b+b1*log(x)}
der <- function(b1,b2,x,z){b1*(1/x)-b2*(1/(1-z))}
plot(eq(b=3.552e+04,b1=2.523e-01,b2=1.803e+00,x=Y,z=t_perc), type='l')
plot(eq1(b=3.552e+04,b1=2.523e-01,x=Y), type="l", col="red")
plot(der(b1=2.523e-01,b2=1.803e+00,x=Y,z=t_perc), type="l", col="green")
plot3d(Y, t_perc, C)
play3d(spin3d(axis = c(0, 0, 1), rpm = 1), duration = 100000)

#2. retta di regressione
#--
#1. DC=f(DY,DTax)
#2. curva di regressione
#--

#II FASE: la stima dei parametri.

###Regressione in termini assoluti

plot(Y, C)
AregrY <- lm(C ~ Y)
summary(AregrY)
abline(c(AregrY$coefficients[1],AregrY$coefficients[2]))

plot(Tax, C)
AregrT <- lm(C ~ Tax)
summary(AregrT)
abline(c(AregrT$coefficients[1],AregrT$coefficients[2]))

plot(t_perc, C)
Aregrt <- lm(C ~ t_perc)
summary(Aregrt)
abline(c(Aregrt$coefficients[1],Aregrt$coefficients[2]))

AregrMT <- lm(C ~ Y + Tax)
summary(AregrMT)

(Ab00=AregrMT$coefficients[1])
(Ab11=AregrMT$coefficients[2])
(Ab22=AregrMT$coefficients[3])

plot3d(Y, Tax, C)
play3d(spin3d(axis = c(0, 0, 1), rpm = 3), duration = 100000)

AregrMt <- lm(C ~ Y + t_perc)
summary(AregrMt)

(Ab00=AregrMt$coefficients[1])
(Ab11=AregrMt$coefficients[2])
(Ab22=AregrMt$coefficients[3])

plot3d(Y, t_perc, C)
play3d(spin3d(axis = c(0, 0, 1), rpm = 3), duration = 100000)


###Regressione in logaritmo
plot(logY, logC)
LregrY <- lm(logC ~ logY)
summary(LregrY)
abline(c(LregrY$coefficients[1],LregrY$coefficients[2]))

plot(logT, logC)
LregrT <- lm(logC ~ logT)
summary(LregrT)
abline(c(LregrT$coefficients[1],LregrT$coefficients[2]))

logt=log(t_perc)
plot(logt, C)
Lregrt <- lm(logC ~logt)
summary(Lregrt)
abline(c(Lregrt$coefficients[1],Lregrt$coefficients[2]))

LregrM <- lm(logC ~ logY + logT)
summary(Lregr)

Lb00=LregrM$coefficients[1]
Lb11=LregrM$coefficients[2]
Lb22=LregrM$coefficients[3]

plot3d(logY, logT, logC)
play3d(spin3d(axis = c(0, 0, 1), rpm = 3), duration = 100000)

LregrMt <- lm(C ~ Y + t_perc)
summary(LregrMt)

(Ab00=AregrMt$coefficients[1])
(Ab11=AregrMt$coefficients[2])
(Ab22=AregrMt$coefficients[3])

plot3d(Y, t_perc, C)
play3d(spin3d(axis = c(0, 0, 1), rpm = 3), duration = 100000)

###Regressione in variazioni

#Consumo-Reddito

VregrCR <- lm(DC~ DY)
summary(VregrCR)

VCRb00=VregrCR$coefficients[1]
VCRb11=VregrCR$coefficients[2]

plot(DY,DC)
abline(c(VCRb00,VCRb11))
#abline(c(a=0, b=0))
#abline(c(a=0, b=mean(Y)))

#Consumo-TasseM

VregrCTM <- lm(DC ~ DTM)
summary(VregrCTM)

VCTMb00=VregrCTM$coefficients[1]
VCTMb11=VregrCTM$coefficients[2]

plot(DTM,DC)
abline(c(VCTMb00,VCTMb11))
#abline(c(a=0, b=0))
#abline(c(a=0, b=mean(Y)))

#Consumo-Reddito-TasseM

VregrMCRTM <- lm(DC~ DY + DTM)
summary(VregrMCRTM)

#(..i plot cambiano poco..)

#Consumo-Consumo(t-1)-Reddito-TasseM
#(DCC <- c(DC[[1]], DC[1:37]))
#(DCCC <- c(DC[1:2], DC[1:36]))
#(DCCCC <- c(DC[1:3], DC[1:35]))
#(DTT <- c(DT[[1]], DT[1:37]))
#(DYY <- c(DY[[1]], DY[1:37]))

#Vregr <- lm(DC~ DY + DYY + DT + DTT + DCC+ DCCC + DCCCC)
#summary(Vregr)

#plot(DY,DC)
#Vb000=Vregr$coefficients[1]
#Vb111=Vregr$coefficients[2]
#abline(c(Vb000,Vb111))

#(DTT <- c(DT[[1]], DT[1:37]))
#(DYY <- c(DY[[1]], DY[1:37]))

#(...)

#commento dell'output:

#- analizzare la variabilita' degli stimatori b00 e b11 misurata dallo St.Error;
#- notiamo che, nel caso di b00, non rifiutiamo l'ipotesi che questa assuma il valore stimato;
#- notiamo che, nel caso di b11, rifiutiamo l'ipotesi che questa assuma il valore stimato;
#- come si interpreta lo standard error residuo?
#- notiamo che l'R^2 e' molto basso, sara' necessario aumentare il numero di variabili indipendenti per spiegare meglio il modello.

#Test ANOVA su ciascuna variabile indipendente

#- testata la normalita', calcolarsi e riportare gli intervalli di confidenza di b00, b11 e Y;
#- plottare le rispettive distribuzioni normali
#- calcolare l'intervallo predittivo di Yn+1 e analizzarlo.


#III FASE: la validazione del modello.

###Test Anova sui parametri della regressione

anova(VregrMCRTM)

#commento dei risultati

###L'analisi dei residui

residui=summary(Vregr)$residuals

#errori a media nulla

t.test(residui)

plot(VregrMCRTM$fitted.values,VregrMCRTM$residuals)
abline(c(a=0,b=0))

#errori omoschedastici

bptest(Vregr)

plot(VregrMCRTM$fitted.values,VregrMCRTM$residuals)
abline(c(a=0,b=0))

#errori con distribuzione normale

shapiro.test(residui)

qqnorm(residui)
qqline(residui)

jarque.bera.test(residui)

hist(residui, prob=T, main="Distribuzione dei residui", xlab="Residui")
plot(density(residui,kernel="gaussian"),main="Distribuzione dei residui:
lisciamento")

#errori non autocorrelati

dwtest(Vregr)

durbinWatsonTest(Vregr)

###Analisi del valore informativo di ciascuna variabile

#VIF
#come si interpreta?

vif(Vregr)

###Gestione degli outliers
#Come si interpreta?

outlierTest(Vregr)

#ANALISI DEI RISULTATI DEL MODELLO.

###analisi su caratteristiche e relazione tra C e Y

#Y prociclica
(r = cor(DC,DY))
(r^2)

#bozza: Y leading o logging?
#ciclo for

#bozza: persistenza di C e Y attraverso l'autocorrelazione
plot(c(1:length(C)), C)
plot(c(1:length(Y)), Y)

#bozza: variabilita' di C e Y
var(DC)
var(DY)
var(DC)/var(DY)

###bozza analisi della correlazione nel tempo

(cor(dati1))
corrplot(cor(dati1), method = "pie")

##bozza organizzazione risultati
#dataframe riassuntivo

(dati2=data.frame(VariazioniC=DC, VariazioniY=DY, Variazioni_stimateC=Vregr$fitted.values, Residui=residui))
