library("zoo")
library("plot3D")
library("magick")
library("rgl")
par(bg="white")
#DA
####Daset utilizzato

#campione_C <- read_xlsx("/home/jenito/Documenti/Consumo/consumo_procapiteHH.xlsx")[1,]
#campione_Y <- read_xlsx("/home/jenito/Documenti/Consumo/gdp_procap.xlsx")[1,]
#campione_T <- read_xlsx("/home/jenito/Documenti/Consumo/taxes.xlsx")[1,]
(campione_CD <- read.csv("C:/Users/andre/Documents/GitHub/Consumo/durevoli.csv")[18:56])
(campione_C_ND <- read.csv("C:/Users/andre/Documents/GitHub/Consumo/non_durevoli.csv")[18:56])
(campione_T <- read.csv("C:/Users/andre/Documents/GitHub/Consumo/gettito2.csv")[2:40])
(campione_Y <- read.csv("C:/Users/andre/Documents/GitHub/Consumo/gdp_ita.csv")[18:56])
(campione_DP <- read.csv("C:/Users/andre/Documents/GitHub/Consumo/inflazione.csv")[14:130])
# per sapere in che riga si trovavano i dati italiani ho messo come indice di colonna ['Country]
# su tutti i file qui sopra e ho runnato questi comandi, cosi ho trovato la riga corrispondente


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
campione_CD <- colSums(campione_CD)
campione_CD

campione_C_ND <- colSums(campione_C_ND)

campione_C <- campione_C_ND + campione_CD
campione_C <- campione_C



#which(campione_Y=='Italy')
#which(campione_T=='Italy')
head(campione_C)
head(campione_Y)
#head(campione_T)
###Dati di interesse
(C = as.numeric(campione_C)/infl)
(Y = as.numeric(campione_Y)/infl)
(Tax = as.numeric(campione_T)/infl)
Tax = rollmean(Tax, 4)
Tax = c(Tax, Tax[33:35])


dati <- data.frame(reddito=c(Y), consumo=c(C), tasse=c(t_perc))
head(dati)
datimat <- as.matrix(dati)
datimat
Time <- 1:length(Y)
plot(Time, Tax, col = "green", bg="white")
t_perc <- Tax/Y

logC = log(C)
logY = log(Y)
logT = log(Tax)
plot(Time, C, col="black")

plot(Time,Y)
plot(Time, C)
plot(Time, Tax)

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

###Rappresentazione dei dati di interesse
(dati1=data.frame(VariazioniC=DC, VariazioniY=DY, VariazioniT=DT))
par(mfrow=c(1,1))
Dtime = 1:length(DC)
DCM = c(rollmean(DC, 4),rollmean(DC, 4)[32:34] )
DTM = c(rollmean(DT, 4),rollmean(DT, 4)[32:34] )
DYM = c(rollmean(DY, 4),rollmean(DY, 4)[32:34] )
plot(Dtime, DCM, type="l", col="red")
#abline(a=mean(DCM), b=0, col = "red")
par(new=TRUE)
plot(Dtime, DYM, type = "l", col='green')
#abline(a=mean(DYM), b=0, col = "red")
par(new=TRUE)
plot(Dtime, DTM, type="l", col='blue')
#abline(a=mean(DTM), b=0, col = "red")
line(Dtime, DCM)
eq = function(b,b1,b2,x=0,z=0){b+b1*log(x)+2*b1*log(x)+b2*log((1-z))}
eq1 = function(b,b1,x){b+b1*log(x)}
plot(eq(b=3.552e+04,b1=2.523e-01,b2=1.803e+00,x=Y,z=t_perc), type='l')
par(new=T)
plot(eq1(b=3.552e+04,b1=2.523e-01,x=Y), type="l", col="red")
der <- function(b1,b2,x,z){b1*(1/x)-b2*(1/(1-z))}
derr <- D(eq, "x")
par(new=T)
plot(der(b1=2.523e-01,b2=1.803e+00,x=Y,z=t_perc), type="l", col="green")
scatter3D(Y, t_perc,  C,phi = 0, bty = "g", col = gg.col(100), 
          pch = 18)



plot3d(Y, t_perc, C)


# We can indicate the axis and the rotation velocity
play3d( spin3d( axis = c(0, 0, 1), rpm = 1), duration = 1000 )

# Save like gif
movie3d(
  movie="3dAnimatedScatterplot", 
  spin3d( axis = c(0, 0, 1), rpm = 50000),
  duration = 1000, 
  dir = "~/Desktop",
  type = "gif", 
  clean = TRUE
)







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





###Regressione in termini assouluti
plot(Y, C)

Aregr <- lm(C ~ Y + Tax)
summary(Aregr)
names(summary(Aregr))
max(Aregr$residuals)
min(Aregr$residuals)

Ab00=Aregr$coefficients[1]
Ab11=Aregr$coefficients[2]
Ab22=Aregr$coefficients[3]

plot(Y,C)
abline(c(Ab00,Ab11))


###Regressione in logaritmo
plot(Y, C)

Lregr <- lm(logC ~ logY + logT)
summary(Lregr)
names(summary(Lregr))
max(Lregr$residuals)
min(Lregr$residuals)

Lb00=Lregr$coefficients[1]
Lb11=Lregr$coefficients[2]
Lb22=Lregr$coefficients[3]
plot(logY,logC)
abline(c(Lb00,Lb11))


###Regressione in variazioni
plot(DY, DC)
DCC <- c(DC[[1]], DC[1:37])
DCCC <- c(DC[1:2], DC[1:36])
DCCCC <- c(DC[1:3], DC[1:35])
DCC
DCCC
DCCCC
DTT <- c(DT[[1]], DT[1:37])
DYY <- c(DY[[1]], DY[1:37])
Vregr <- lm(DC~ DY + DYY + DT + DTT + DCC+ DCCC + DCCCC)
summary(Vregr)
names(summary(Vregr))
max(Vregr$residuals)
min(Vregr$residuals)

Vb00=Vregr$coefficients[1]
Vb11=Vregr$coefficients[2]

plot(DY,DC)
abline(c(Vb00,Vb11))
abline(c(a=mean(DC), b=0))
abline(c(a=0, b=mean(Y)))


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
residui=summary(Vregr)$residuals
residui <- c(residui)
t.test(residui)
ks.test(rnorm, residui)
qqnorm(residui)
qqline(residui)
library("tseries")
jarque
jarque.bera.test(residui)

library("lmtest")
bptest(residui)
ks.test(residui, pt)
ks.test(residui, pchisq)
bartlett.test(residui)

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


