


setwd("C:/Users/..")

source("quantile_coherency_replication_pack.R")
library(readxl)

y <- read_excel("base.xlsx", col_names = TRUE);




Y1 <- y$OFR_FSI
Y2 <- y$`ln(ETH)`

Y <- matrix(c(Y1, Y2), ncol = 2)
n <- dim(Y)[1]

## Compute Quantile cross-spectral densities on data

# chose quantile levels
quantile <- c(0.5, 0.05, 0.95)
sPG <- smoothedPG(Y, levels.1 = quantile,
                  weight = kernelWeight(W = W1, b = 0.5 * n^(-1/4)))

# Choose frequencies
cut = (0:123) / 124
cut = round(cut * 23385) / 23385 

# Compute Quantile-Coherency and confidence intervals
Coh <- getCoherency(sPG, frequencies = 2 * pi * cut)

# NOTE: this takes few minutes
CI <- getPointwiseCIs(sPG, quantity = "coherency", frequencies = 2 * pi * cut)

# FIGURE 3 
pdf(file=paste("FSI_ETH_re.pdf", sep = ''), width = 8, height = 5.3)

par(mar = c(4, 2, 5.5, 0.5) + 0.1)
par(mfrow = c(1, 2))

freq <- cut[1:(length(cut)/2)]
le <- (length(cut) / 2)

clr <- gray.colors(length(quantile) + 2,start = 0.1, end = 0.8)
clr <- c(clr[2], clr[1], clr[5])
d <- c(10, 20, 30)
a <- c(90, 90, 90)

plot(x = freq, xlim = c(0,0.5), ylim = c(-0.6, 0.6), type = "l",
     xlab = expression(omega / 2*pi), ylab = "", main = "QC (Re) FSI vs ETH ")
for (i1 in 1:length(quantile)) {
  polygon(x = c(rev(freq), freq), y = c(rev(Re(CI$lo[1:le,1,i1,2,i1])),
                                        Re(CI$up[1:le,1,i1,2,i1])),
          col = clr[i1], density = d[i1], angle = a[i1],
          lty = 3, lwd = c(0.8, 0.8, 0.8))
}

for (i1 in 1:length(quantile)) {
  lines(x = freq, y = Re(Coh[1:le,1,i1,2,i1,1]),
        col = "black", lty = i1, lwd = c(1.5, 1.5, 1.5))
}
axis(side = 3, at = c(1/5, 1/22, 1/250), labels = c("W", "M", "Y"))
abline(v = c(1/5, 1/22, 1/250), col = "gray")
legend("bottom", inset = .03, "center",
       c("0.5 | 0.5", "0.05 | 0.05", "0.95 | 0.95"), cex = 0.65,
       lwd = c(1, 1, 1), lty = c(1:length(quantile)),
       horiz = TRUE, bg = "white")

# FIGURE 3 (middle part for tau1 \ne tau2)
plot(x = freq, xlim = c(0, 0.5), ylim = c(-0.6, 0.6), type = "l",
     xlab = expression(omega / 2 * pi), ylab = "", main = "QC (Re) FSI vs ETH")
i1 <- 1
i2 <- 2
i3 <- 3
polygon(x = c(rev(freq), freq), y = c(rev(Re(CI$lo[1:le,1,i2,2,i3])),
                                      Re(CI$up[1:le,1,i2,2,i3])),
        col = clr[i1], density = d[i1], angle = a[i1],
        lty = 3,lwd = c(0.8,0.8,0.8))
lines(x = freq, y = Re(Coh[1:le,1,i2,2,i3,1]),
      col = "black", lty = i1, lwd = c(1.5, 1.5, 1.5))
axis(side = 3, at = c(1/5, 1/22, 1/250),
     labels = c("W", "M", "Y"))
abline(v = c(1/5, 1/22, 1/250), col = "gray")
legend("bottom", inset = .03, "center", c("0.05 | 0.95"),
       cex = 0.8, lwd = c(1, 1, 1), lty = c(1:length(quantile)),
       horiz = TRUE, bg = "white")
dev.off()

