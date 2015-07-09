getwd();
setwd("../../4-exploratory-data-analysis");

pollution <-
  read.csv(
    "data/avgpm25.csv", 
    colClasses = c("numeric", "character", "factor", "numeric", "numeric"))
#annual mean, countie identifier, region of countie, location of sensor
head(pollution)

# ONE DIMENSIONAL
summary(pollution$pm25)

boxplot(pollution$pm25, col = "blue")
abline(h = 12)

hist(pollution$pm25, col = "green", breaks = 200)# Sense of distribution
rug(pollution$pm25)# Density of data
abline(v = 12, lwd = 2)# highlight the standard reference
abline(v = median(pollution$pm25), col = "magenta", lwd = 4)

barplot(
  table(pollution$region), col = "wheat",
  main = "Number of COUNTIES in Each Region")

# TWO DIMENSIONAL
boxplot(pm25 ~ region, data = pollution, col = "red")

?par
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1)) # Draw plots on x by y
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")

?with
plot(pollution$latitude, pollution$pm25, col = pollution$region)# This is
with(pollution, plot(latitude, pm25, col = region))# same as this
?abline
abline(h = 12, lwd = 2, lty = 2)


# PLOTTING SYSTEMS

# BASE PLOT
library(datasets)
data(cars)
with(cars, plot(speed, dist))

# LATTICE SYSTEM
library(lattice)
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4, 1))


# GGPLOT2 SYSTEM
install.packages("ggplot2")
library(ggplot2)
data(mpg)
qplot(displ, hwy, data = mpg)

library(datasets)
hist(airquality$Ozone)
with(airquality, plot(Wind, Ozone))

airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)")

par("lty")
par("col")
par("pch")

# BASE PLOT SYSTEMS

with(airquality, plot(Wind, Ozone, main = "A Title", type = "n"))
title(main = "Hello World !")
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1, col = c("blue", "red"), legend = c("May", "Other Months"))

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", pch = 20))
model <- lm(Ozone ~ Wind, airquality)
abline(model, lwd = 2)

par(mfrow = c(1, 2))
with(
  airquality, {
  plot(Wind, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
})

par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
with(airquality, {
  plot(Wind, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
  plot(Temp, Ozone, main = "Ozone and Temperature")
  mtext("Huge Title", outer = TRUE)
})

# Demo

x <- rnorm(100)
hist(x)
y <- rnorm(100)
plot(x, y)
z <- rnorm(100)
plot(x, z)
par(mar = c(5, 5, 5, 5))
plot(x, z)
plot(x, y, pch = 20)
plot(x, y, pch = 4)
title("ScatterPlot")
text(-2, -2, "Label")
legend("topright", legend = "Data", pch = 20)
fit <- lm(y ~x)
abline(fit)
abline(fit, lwd = 3, col = "red")
plot(x, y, xlab = "weight", ylab = "height", main = "XYZ", pch = 20)

par(mfrow = c(2, 1))
plot(x, y, pch = 20)
plot(x, z, pch = 19)
par("mar")
par(mar = c(4, 4, 4, 4))

x <- rnorm(100)
y <- x + rnorm(100)

?gl
g <- gl(2, 50, labels = c("Male", "Women"))
str(g)
plot(x, y)

plot(x, y, type = "n")
points(x[g == "Male"], y[g == "Male"], col = "green")
points(x[g == "Women"], y[g == "Women"], col = "blue", pch = 19)

# GRAPHIC DEVICES
# MAC
# quartz()
# WINDOWS
# windows()
# Linux
# x11()

with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful Geyser data")

getwd()
pdf(file = "./myplot.pdf")
with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful Geyser data")
dev.off()

windows()
dev.cur()
dev.set(4)

with(faithful, plot(eruptions, waiting))
title(main = "Copied from Screen")
dev.copy(png, file = "myplot.png")
dev.off()

# TESTING

