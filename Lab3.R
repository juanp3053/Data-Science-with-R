#LAB 3
library(lattice)
data("USArrests")

AssignLevel <- function(p, quartiles)
{
  if (p < quartiles[1])
    rlevel <- "LOW"
  else if (p < quartiles[2])
    rlevel <- "MODERATE"
  else if (p < quartiles[3])
    rlevel <- "HIGH"
  else
    rlevel <- "VERY HIGH"
  
  return (rlevel)
}

p <- USArrests[,3]
q <- quantile(p, c(.25, .5, .75))

UrbanPopRating <- sapply(p,  AssignLevel, q)
UrbanPopRating <- factor(UrbanPopRating, levels = c("LOW","MODERATE","HIGH","VERY HIGH"))
cbind(USArrests, UrbanPopRating)

histogram(~Assault | UrbanPopRating, data =USArrests, xlab = "State Arrests per 100,000", layout=c(1,4), panel = function(x, ...){
  panel.histogram(x, ...)
  panel.abline(v = median(x), lwd = 3, lty = 2, col = "red")
})
bwplot(~Assault | UrbanPopRating, data =USArrests, xlab = "State Arrests per 100,000", layout=c(1,4), panel = function(x, ...){
  panel.bwplot(x, ...)
  panel.abline(v = mean(x), lwd = 2, lty = 3, col = "blue")
})
xyplot(Murder~ Assault | UrbanPopRating, data =USArrests, xlab = "State Arrests per 100,000", layout=c(1,4), panel = function(x,y, ...){
  panel.xyplot(x,y, ...)
  panel.lmline(x,y, lwd = 3)
})

