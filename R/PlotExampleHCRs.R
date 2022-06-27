x <- c(0, seq(150000, 5000000, by = 50000))
y <- function(x, Fmsy){
  HG <- (x-150000)*Fmsy
  if(HG < 0) HG <- 0
  if(HG > 200000) HG <- 200000
  
  return(HG)
}

Emsy <- function(Temp){
  -18.46452 + 3.25209*(Temp) - 0.19723*(Temp^2) + 0.0041863*(Temp^3)
} 

xTemp <- seq(13, 17, by = 0.1)
plot(xTemp, Emsy(Temp = xTemp), type = "l")
abline(h = c(0.05, 0.2), lty = 2, col = "grey")

y5 <- sapply(x, y, Fmsy = 0.05)
y20 <- sapply(x, y, Fmsy = 0.2)  

plot(x, y5, type = "l", xlab = "Biomass (mt)", ylab = "Catch (mt)", lwd = 4)
abline(h = 200000, col = 2, lwd = 4)
lines(x, y20, col = "blue", lwd = 4)
