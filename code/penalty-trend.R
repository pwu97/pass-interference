## Get data from Pro Football Reference website and then
## put into dataframes. View the results by using line charts
## and adding suitable colors to differentiate DPI and OPI.

def.pen.g <- c(1.02, 1.08, 1.02, 0.88, 0.93, 0.87, 0.69, 0.78, 0.64, 
               0.57, 0.62, 0.69, 0.76, 0.81, 0.96, 0.94, 0.80, 0.90,
               1.09, 1.04, 0.89)
mean(def.pen.g)
off.pen.g <- c(0.26, 0.31, 0.26, 0.24, 0.15, 0.26, 0.23, 0.27, 0.23,
               0.20, 0.25, 0.28, 0.26, 0.27, 0.29, 0.24, 0.40, 0.40,
               0.34, 0.45, 0.31)
length(def.pen.g)
rep(mean(off.pen.g), 21)
years <- c(1998:2018)

plot(years, def.pen.g, type = "o", col = "red", xlab = "Year", 
     ylim=c(0,1.2), ylab = "Penalties/Year", main = "Pass Interference (PI) Calls 1998-2018")
legend("top", legend=c("Defensive PI", "Offensive PI"),
       col=c("red", "blue"), lty=1:1, cex=0.8)
lines(years, off.pen.g, type = "o", col = "blue")
lines(years, rep(mean(off.pen.g), 21), lty=2, col="blue")
lines(years, rep(mean(def.pen.g), 21), lty=2, col="red")

