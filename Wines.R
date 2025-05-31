install.packages("mclust")
install.packages("ggplot2")

library(mclust)
library(ggplot2)

data <- read.csv("Wines.csv", header = TRUE, sep = ";")
attach(data)

# Histograms e EDA

par(mfrow = c(2, 2))
hist(Myo, nclass=35, freq = FALSE, main="Histogram of Myo-inostitol (mg/kg sugars)",
     xlim=c(0,3000)) #, ylim=c(0,40))
lines(density(Myo), col="red")

hist(Scyllo, nclass=35, freq = FALSE, main="Histogram of Scyllo-inostitol (mg/kg sugars)",
     xlim=c(0,600)) #, ylim=c(0,40))
lines(density(Scyllo), col="red")

hist(DH.I, nclass=35, freq = FALSE, main="Histogram of DH.I (ppm)",
     xlim=c(90, 107))#, ylim=c(0,40))
lines(density(DH.I), col="red")

hist(DH.II, nclass=35, freq = FALSE, main="Histogram of DH.II (ppm)",
     xlim=c(120, 132)) #, ylim=c(0,40))
lines(density(DH.II), col="red")

# Trsformazioni logaritmiche e preparazione del data.frame

DH.I=data[,3]
log.Myo=log(data[,1])
log.Scyllo=log(data[,2])

wines=data.frame(DH.I, log.Myo, log.Scyllo)

# Scelta del modello via BIC (Bayesian Information Criterion)

BIC <- mclustBIC(wines, G=2)

par(mfrow = c(1,1))
plot(BIC)

summary(BIC)

# Stima dei parametri della mistura a tre componenti (log(Myo), log(Scyllo) e DH.I) via EM-algorithm

winesMclust <- Mclust(wines, x=BIC, G=2)
summary(winesMclust, parameters = TRUE)

# Identificazione delle sottopopolazioni (Genuine=1 vs. Adulterated=2 samples)

class=winesMclust$classification

wines.class=data.frame(wines, class, winesMclust$z)
colnames(wines.class)[c(5,6)] <- c("z.i1","z.i2")
wines.class
plot(winesMclust, what = "classification")

# Sotto-analisi con differenti combinazioni di coppie di variabili o singole variabili

# log(Myo) vs. DH.I
data.1 = data.frame(log.Myo, DH.I)

mod.1 = densityMclust(data.1, G=2, x=BIC, plot=FALSE)
summary(mod.1, parameter=TRUE)

par(mfrow = c(2,1))
plot(mod.1, what = "density", data = data.1, drawlabels = TRUE, points.pch = 20, col="red", nlevels=30)
plot(mod.1, what = "density", type = "hdr", data=data.1)

par(mfrow = c(1,1))
plot(mod.1, what = "density", type = "persp")  # Perspective plot

# log(Myo) vs.log(Scyllo)

data.2=data.frame(log.Scyllo, log.Myo)

mod.2 = densityMclust(data.2, G=2, x=BIC, plot=FALSE)
summary(mod.2, parameter=TRUE)

par(mfrow = c(2,1))
plot(mod.2, what = "density", data = data.2, drawlabels = TRUE, points.pch = 20, col="red", nlevels=20)
plot(mod.2, what = "density", type = "hdr", data=data.2)

par(mfrow = c(1,1))
plot(mod.2, what = "density", type = "persp")

# log(Scyllo) vs. DH.I

data.3 = data.frame(log.Scyllo, DH.I)

mod.3 = densityMclust(data.3, G=2, x=BIC, plot=FALSE)
summary(mod.3, parameter=TRUE)

par(mfrow = c(2,1))
plot(mod.3, what = "density", data = data.3, drawlabels = TRUE, points.pch = 20, col="red", nlevels=30)
plot(mod.1, what = "density", type = "hdr", data=data.1)

par(mfrow = c(1,1))
plot(mod.3, what = "density", type = "persp")  # Perspective plot


# Per qualche ulteriore dettaglio sui modelli implementati da Mclust o sul criterio di selezione BIC vedere:
# https://rdrr.io/cran/mclust/man/mclustModelNames.html
# https://stats.stackexchange.com/questions/237220/mclust-model-selection


# DH.I

mod.4 <- densityMclust(data.matrix(DH.I), modelNames = "V", G=2, plot=FALSE)
summary(mod.4, parameter=TRUE)

par(mfrow = c(1,1))
plot(mod.4, what = "density", data = DH.I, col="red")
plot(mod.4, what = "density", type = "persp")

# log(Myo)

mod.5 <- densityMclust(log.Myo, modelNames = "V", G=2, plot=FALSE)
summary(mod.5, parameter=TRUE)

plot(mod.5, what = "density", data = log.Myo, col="red")
plot(mod.5, what = "density", type = "persp")

# log(Scyllo)

mod.6 <- densityMclust(log.Scyllo, modelNames = "V", G=2, plot=FALSE)
summary(mod.6, parameter=TRUE)

plot(mod.6, what = "density", data = log.Scyllo, col="red")
plot(mod.6, what = "density", type = "persp")

