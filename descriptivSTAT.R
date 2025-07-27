library(readxl)
library(moments)
library(tseries)

setwd('C:/Users/njah_/Documents/A_RECHERCHE/Submitted_Revision_Accepted/RIBAF/RIBAF_Accepted')


data <- read_excel("base.xlsx")


attach(data)

head(data)
###################DESCRIPTIVE statistics
summary(data)

mean(ln_BTC)
sd(ln_BTC)
skewness(ln_BTC)
kurtosis(ln_BTC)
jarque.bera.test(ln_BTC)


mean(`ln(ETH)`)
sd(`ln(ETH)`)
skewness(`ln(ETH)`)
kurtosis(`ln(ETH)`)
jarque.bera.test(`ln(ETH)`)


mean(T5YIE)
sd(T5YIE)
skewness(T5YIE)
kurtosis(T5YIE)
jarque.bera.test(T5YIE)

mean(OFR_FSI)
sd(OFR_FSI)
skewness(OFR_FSI)
kurtosis(OFR_FSI)
jarque.bera.test(OFR_FSI)

##################plots

Data <- ts(data, start = c(2018, 4), frequency = 256)
plot.ts(Data)




##################plots

DataX <- ts(data, start = c(2018, 4), frequency = 256)

################# PLOT in the same graph
#############################################################################################
DataX_subset2 <- DataX[, 2] #btc
DataX_subset3 <- DataX[, 3] #T5
DataX_subset4 <- DataX[, 4] #fsi
DataX_subset5 <- DataX[, 5] #eth

# Calcul des échelles pour chaque série
yrange1 <- range(DataX_subset2)
yrange2 <- range(DataX_subset5)
# Définition de la taille de l'espace graphique
par(mar=c(5,4,4,4)+0.1)
# Tracer la première série avec l'axe des y à gauche
plot(DataX_subset2, col="blue", xlab="", ylab="",yaxt="n")
axis(2, at=pretty(yrange1), labels=pretty(yrange1), col.axis="blue")
mtext("ln(BTC)", side=2, line=2.5, col="blue")
# Ajouter une deuxième série avec l'axe des y à droite
par(new=TRUE)
plot(DataX_subset5, col="red", xlab="", ylab="", yaxt="n")
axis(4, at=pretty(yrange2), labels=pretty(yrange2), col.axis="red")
mtext("ln(ETH)", side=4, line=2.5, col="red")
# Ajouter une légende
legend("topright", c("ln(BTC)", "ln(ETH)"), col=c("blue", "red"), lty=1, bty="n")
# Restaurer les paramètres graphiques par défaut
par(new=FALSE)

####################################################################

# Calcul des échelles pour chaque série
yrange1 <- range(DataX_subset3)
yrange2 <- range(DataX_subset4)
# Définition de la taille de l'espace graphique
par(mar=c(5,4,4,4)+0.1)
# Tracer la première série avec l'axe des y à gauche
plot(DataX_subset3, col="blue", xlab="", ylab="",yaxt="n")
axis(2, at=pretty(yrange1), labels=pretty(yrange1), col.axis="blue")
mtext("T5YIE", side=2, line=2.5, col="blue")
# Ajouter une deuxième série avec l'axe des y à droite
par(new=TRUE)
plot(DataX_subset4, col="red", xlab="", ylab="", yaxt="n")
axis(4, at=pretty(yrange2), labels=pretty(yrange2), col.axis="red")
mtext("FSI", side=4, line=2.5, col="red")
# Ajouter une légende
legend("topright", c("T5YIE", "FSI"), col=c("blue", "red"), lty=1, bty="n")
# Restaurer les paramètres graphiques par défaut
par(new=FALSE)




#####################################################################


############## BDS NON linear tests
library(fNonlinear)

x<-OFR_FSI
x<-as.numeric(x)

bdsTest(x, m = 5, eps = NULL, title = NULL, description = NULL)

###### Kruse(2011) nonlinear unit root test function


library(NonlinearTSA)
Kruse_Unit_Root(`ln(ETH)`, 3, 4, 1)

Kruse_Unit_Root(CVIX, 2, 4, 1)

Kruse_Unit_Root(CVIX, 3, 4, 1)

#### Cuestas and Ordonez(2014) nonlinear unit root test

Cuestas_Ordonez_2014_unit_root(CVIX, 4)

### Hu and Chen(2016) nonlinear unit root test 


Hu_Chen_Unit_Root(`ln(ETH)`, 3, 4, 1)

Hu_Chen_Unit_Root(UCRY_price, 2, 4, 1)

Hu_Chen_Unit_Root(UCRY_price, 3, 4, 1)


###################################################"
dFsi= diff(OFR_FSI)

dT5=diff(T5YIE)

dBTC=diff(ln_BTC)

dETH=diff(`ln(ETH)`)

#####################################################
Kruse_Unit_Root(dBTC, 1, 4, 1)
Hu_Chen_Unit_Root(dBTC, 1, 4, 1)

Kruse_Unit_Root(dETH, 1, 4, 1)
Hu_Chen_Unit_Root(dETH, 1, 4, 1)

Kruse_Unit_Root(dT5, 1, 4, 1)
Hu_Chen_Unit_Root(dT5, 1, 4, 1)

Kruse_Unit_Root(dFsi, 1, 4, 1)
Hu_Chen_Unit_Root(dFsi, 1, 4, 1)








