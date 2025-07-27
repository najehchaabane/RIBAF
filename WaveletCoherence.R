#install.packages('biwavelet')
#install.packages("lubridate")
library(biwavelet)
library(readxl)
library(lubridate)



data <- read_excel("../Data/base.xlsx")
attach(data)




#### WAVELET COHERENCE ###### FIGURE 2

t1 = cbind(1:1261, T5YIE)
t2 = cbind(1:1261, ln_BTC)

nrands = 50

wtc.AB = wtc(t1, t2, nrands = nrands)

# Plotting a graph
par(oma = c(0, 0, 0, 1), mar = c(6, 5, 6, 6) + 0.1)
plot(wtc.AB, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.1, arrow.len = 0.1, ylab = "", xlab = "", 
     plot.cb = TRUE, main = "T5YIE vs BTC", xaxt='n')

axis(side = 1, at = seq(1, length(Date), by = 40), labels = Date[seq(1, length(Date), by = 40)], las = 2)



#### WAVELET COHERENCE ###### FIGURE 3

t1 = cbind(1:1261, T5YIE)
t2 = cbind(1:1261, `ln(ETH)`)

nrands = 50

wtc.AB = wtc(t1, t2, nrands = nrands)

# Plotting a graph
par(oma = c(0, 0, 0, 1), mar = c(6, 5, 6, 6) + 0.1)
plot(wtc.AB, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.1, arrow.len = 0.1, ylab = "", xlab = "", 
     plot.cb = TRUE, main = "T5YIE vs ETH", xaxt='n')

axis(side = 1, at = seq(1, length(Date), by = 40), labels = Date[seq(1, length(Date), by = 40)], las = 2)



#### WAVELET COHERENCE ###### FIGURE 4

t1 = cbind(1:1261, OFR_FSI)
t2 = cbind(1:1261, ln_BTC)

nrands = 50

wtc.AB = wtc(t1, t2, nrands = nrands)

# Plotting a graph
par(oma = c(0, 0, 0, 1), mar = c(6, 5, 6, 6) + 0.1)
plot(wtc.AB, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.1, arrow.len = 0.1, ylab = "", xlab = "", 
     plot.cb = TRUE, main = "FSI vs BTC", xaxt='n')

axis(side = 1, at = seq(1, length(Date), by = 40), labels = Date[seq(1, length(Date), by = 40)], las = 2)




#### WAVELET COHERENCE ###### FIGURE 5

t1 = cbind(1:1261, OFR_FSI)
t2 = cbind(1:1261, `ln(ETH)`)

nrands = 50

wtc.AB = wtc(t1, t2, nrands = nrands)

# Plotting a graph
par(oma = c(0, 0, 0, 1), mar = c(6, 5, 6, 6) + 0.1)
plot(wtc.AB, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.1, arrow.len = 0.1, ylab = "", xlab = "", 
     plot.cb = TRUE, main = "FSI vs ETH", xaxt='n')

axis(side = 1, at = seq(1, length(Date), by = 40), labels = Date[seq(1, length(Date), by = 40)], las = 2)


