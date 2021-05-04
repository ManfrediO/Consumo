
#DATI

####Daset utilizzato
campione_C <- read_csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/UFFICIALE/estat_namq_10_fcs_filtered.csv")
campione_Y <- read_csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/UFFICIALE/estat_namq_10_gdp_filtered(1).csv")
###Dati di interesse
(C = as.numeric(campione_C[5,]))
(Y = as.numeric(campione_Y[8,]))
(C = C[2:89])
(Y = Y[2:89])
logC = log(C)
logY = log(Y)
DC = c()
DY = c()
nC = length(logC)
nY = length(logY)
for(i in 1:nC-1){
  DC <- c(DC, c(logC[i+1]-logC[i]))
}
for(i in 1:nY-1){
  DY <- c(DY, c(logY[i+1]-logY[i]))
}
DC=DC
DY=DY
###Rappresentazione dei dati di interesse
(dati1=data.frame(VariazioniC=DC, VariazioniY=DY))


#LA COSTRUZIONE DEL MODELLO

#I FASE: la determinazione della forma funzionale.
#--
#1. DC=f(DY) 
#2. retta di regressione
#--
#1. DC=f(DY)
#2. curva di regressione
#--
#1. DC=f(DY,Dr) 
#2. retta di regressione
#--
#(...)

#II FASE: la stima dei parametri.

###Regressione

lm(DC~DY)
summary(lm(DC~DY))
names(summary(lm(DC~DY)))
regressione=lm(DC~DY)

b00=lm(DC~DY)$coefficients[1]
b11=lm(DC~DY)$coefficients[2]

plot(DY,DC)
abline(c(b00,b11))

#commento dell'output, in particolare:
#-b00 e b11 sono stimatori corretti sotto ipotesi di una certa distribuzione dei residui ancora da verificare;; 
#-analizzare la variabilita' degli stimatori b00 e b11 misurata dallo St.Error;
#-notiamo che, nel caso di b00, non rifiutiamo l'ipotesi che questa assuma il valore stimato;
#-notiamo che, nel caso di b11, rifiutiamo l'ipotesi che questa assuma il valore stimato;
#-come si interpreta lo standard error residuo?
#-notiamo che l'R^2 e' molto basso, sara' necessario aumentare il numero di variabili indipendenti per spiegare meglio il modello.

#III FASE: la validazione del modello.
##Come trattare gli outliers?
##L'analisi dei residui
residui=summary(lm(DC~DY))$residuals
t.test(residui)
ks.test(residui, pnorm)
qqnorm(residui)
qqline(residui)
??
library(tseries)
jarque.bera.test(residui)
??
library(lmtest)
bptest()
ks.test(residui, pt)
ks.test(residui, pchisq)


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
library(corrplot)
corrplot(cor(dati1), method = "pie")

##bozza organizzazione risultati
#dataframe riassuntivo
(dati2=data.frame(VariazioniC=DC, VariazioniY=DY, Variazioni_stimateC=regressione$fitted.values, Residui=residui))

#confronto regressioni
par(mfrow=c(2,1))
plot(Y,C)
abline(c(b0,b1))
plot(DY,DC)
abline(c(b00,b11))


