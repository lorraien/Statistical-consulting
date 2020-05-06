#install.packages("pwr")
library(pwr)
sp=sqrt((6*(1.971619015)^2+5*(1.431185485)^2)/9)
d=(6.444248848-5.593836676)/sp
pwr.t2n.test(n1 =7 , n2=6 , d = d , sig.level =0.05,alternative="greater")
r<-pwr.t.test(power = 0.8,d=d,sig.level=.1,alternative="greater")


#Creating Power or Sample Size Plots

#Effect size is a quantitative measure of the magnitude of the experimenter effect. 
#The larger the effect size the stronger the relationship between two variables.

# Plot sample size curves for detecting correlations of
# various sizes.

# lin neg
library(pwr)

d <- seq(.3,.55,.05)
nd <- length(d)

# power values
p <- c(seq(.5,.9,.1),0.95)
np <- length(p)

# obtain sample sizes
samsize <- array(numeric(nd*np), dim=c(nd,np))
for (i in 1:np){
  for (j in 1:nd){
    result <- pwr.t.test(n = NULL, d = d[j],
                         sig.level = .05, power = p[i],
                         alternative = "greater")
    samsize[j,i] <- ceiling(result$n)
  }
}

# set up graph
xrange <- range(d)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type="n",
     xlab="Effect Size (d)",
     ylab="Sample Size (n)" )

# add power curves
for (i in 1:np){
  lines(d, samsize[,i], type="l", lwd=2, col=colors[i])
}

# add annotation (grid lines, title, legend)
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
       col="grey89")
title("Sample Size Estimation for Correlation Studies\n
  Sig=0.05 (one-side)")
legend("topright", title="Power", as.character(p),
       fill=colors)




library(pwr)

# LK
library(pwr)

d <- seq(1.7,2.2,0.1)
nd <- length(d)

# power values
p <- c(seq(.5,.9,.1),0.95)
np <- length(p)

# obtain sample sizes
samsize <- array(numeric(nd*np), dim=c(nd,np))
for (i in 1:np){
  for (j in 1:nd){
    result <- pwr.t.test(n = NULL, d = d[j],
                         sig.level = .016, power = p[i],
                         alternative = "two.sided")
    samsize[j,i] <- ceiling(result$n)
  }
}

# set up graph
xrange <- range(d)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type="n",
     xlab="Effect Size (d)",
     ylab="Sample Size (n)" )

# add power curves
for (i in 1:np){
  lines(d, samsize[,i], type="l", lwd=2, col=colors[i])
}

# add annotation (grid lines, title, legend)
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
       col="grey89")
title("Sample Size Estimation for Correlation Studies\n
  Sig=0.05 (two-side)")
legend("topright", title="Power", as.character(p),
       fill=colors)

par(mfrow=c(1,1))

# CMP
library(pwr)

d <- seq(1.3,1.8,0.1)
nd <- length(d)

# power values
p <- c(seq(.7,.9,.1))
np <- length(p)

# obtain sample sizes
samsize <- array(numeric(nd*np), dim=c(nd,np))
for (i in 1:np){
  for (j in 1:nd){
    result <- pwr.t.test(n = NULL, d = d[j],
                         sig.level = .05, power = p[i],
                         alternative = "two.sided")
    samsize[j,i] <- ceiling(result$n)
  }
}

# set up graph
xrange <- range(d)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type="n",
     xlab="Effect Size (d)",
     ylab="Sample Size (n)" )

# add power curves
for (i in 1:np){
  lines(d, samsize[,i], type="l", lwd=2, col=colors[i])
}

# add annotation (grid lines, title, legend)
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
       col="grey89")
title("Sample Size Estimation for CMP\n
  Sig=0.05 (two-side)")
legend("topright", title="Power", as.character(p),bty="n",
       lty=rep(1,5),lwd=rep(2,5),col=colors,cex=0.9)



library(pwr)

d <- seq(1.3,1.8,0.1)
nd <- length(d)

# power values
p <- c(seq(.7,.9,.1))
np <- length(p)

# obtain sample sizes
samsize <- array(numeric(nd*np), dim=c(nd,np))
for (i in 1:np){
  for (j in 1:nd){
    result <- pwr.t.test(n = NULL, d = d[j],
                         sig.level = .1, power = p[i],
                         alternative = "two.sided")
    samsize[j,i] <- ceiling(result$n)
  }
}

# set up graph
xrange <- range(d)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type="n",
     xlab="Effect Size (d)",
     ylab="Sample Size (n)" )

# add power curves
for (i in 1:np){
  lines(d, samsize[,i], type="l", lwd=2, col=colors[i])
}

# add annotation (grid lines, title, legend)
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
       col="grey89")
title("Sample Size Estimation for CMP\n
  Sig=0.1 (two-side)")
legend("topright", title="Power", as.character(p),bty="n",
       lty=rep(1,5),lwd=rep(2,5),col=colors,cex=0.9)



# GMP
library(pwr)

d <- seq(1.0,1.5,0.1)
nd <- length(d)

# power values
p <- c(seq(.7,.9,.1))
np <- length(p)

# obtain sample sizes
samsize <- array(numeric(nd*np), dim=c(nd,np))
for (i in 1:np){
  for (j in 1:nd){
    result <- pwr.t.test(n = NULL, d = d[j],
                         sig.level = .05, power = p[i],
                         alternative = "two.sided")
    samsize[j,i] <- ceiling(result$n)
  }
}

# set up graph
xrange <- range(d)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type="n",
     xlab="Effect Size (d)",
     ylab="Sample Size (n)" )

# add power curves
for (i in 1:np){
  lines(d, samsize[,i], type="l", lwd=2, col=colors[i])
}

# add annotation (grid lines, title, legend)
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
       col="grey89")
title("Sample Size Estimation for GMP\n
  Sig=0.05 (two-side)")
legend("topright", title="Power", as.character(p),bty="n",
       lty=rep(1,5),lwd=rep(2,5),col=colors,cex=0.9)

d <- seq(1.0,1.5,0.1)
nd <- length(d)

# power values
p <- c(seq(.7,.9,.1))
np <- length(p)

# obtain sample sizes
samsize <- array(numeric(nd*np), dim=c(nd,np))
for (i in 1:np){
  for (j in 1:nd){
    result <- pwr.t.test(n = NULL, d = d[j],
                         sig.level = .1, power = p[i],
                         alternative = "two.sided")
    samsize[j,i] <- ceiling(result$n)
  }
}

# set up graph
xrange <- range(d)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type="n",
     xlab="Effect Size (d)",
     ylab="Sample Size (n)" )

# add power curves
for (i in 1:np){
  lines(d, samsize[,i], type="l", lwd=2, col=colors[i])
}

# add annotation (grid lines, title, legend)
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
       col="grey89")
title("Sample Size Estimation for GMP\n
  Sig=0.1 (two-side)")
legend("topright", title="Power", as.character(p),bty="n",
       lty=rep(1,5),lwd=rep(2,5),col=colors,cex=0.9)




# MEP
library(pwr)

d <- seq(0.8,1.3,0.1)
nd <- length(d)

# power values
p <- c(seq(.7,.9,.1))
np <- length(p)

# obtain sample sizes
samsize <- array(numeric(nd*np), dim=c(nd,np))
for (i in 1:np){
  for (j in 1:nd){
    result <- pwr.t.test(n = NULL, d = d[j],
                         sig.level = .05, power = p[i],
                         alternative = "two.sided")
    samsize[j,i] <- ceiling(result$n)
  }
}

# set up graph
xrange <- range(d)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type="n",
     xlab="Effect Size (d)",
     ylab="Sample Size (n)" )

# add power curves
for (i in 1:np){
  lines(d, samsize[,i], type="l", lwd=2, col=colors[i])
}

# add annotation (grid lines, title, legend)
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
       col="grey89")
title("Sample Size Estimation for MEP\n
  Sig=0.05 (two-side)")
legend("topright", title="Power", as.character(p),bty="n",
       lty=rep(1,5),lwd=rep(2,5),col=colors,cex=0.9)


d <- seq(0.8,1.3,0.1)
nd <- length(d)

# power values
p <- c(seq(.7,.9,.1))
np <- length(p)

# obtain sample sizes
samsize <- array(numeric(nd*np), dim=c(nd,np))
for (i in 1:np){
  for (j in 1:nd){
    result <- pwr.t.test(n = NULL, d = d[j],
                         sig.level = .1, power = p[i],
                         alternative = "two.sided")
    samsize[j,i] <- ceiling(result$n)
  }
}

# set up graph
xrange <- range(d)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type="n",
     xlab="Effect Size (d)",
     ylab="Sample Size (n)" )

# add power curves
for (i in 1:np){
  lines(d, samsize[,i], type="l", lwd=2, col=colors[i])
}

# add annotation (grid lines, title, legend)
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
       col="grey89")
title("Sample Size Estimation for MEP\n
  Sig=0.1 (two-side)")
legend("topright", title="Power", as.character(p),bty="n",
       lty=rep(1,5),lwd=rep(2,5),col=colors,cex=0.9)



# 5 variable sample size
library(pwr)

d <- c(0.486834696,	2.122190405,	1.759898929,	1.537730305,	1.169996316,	1.510089857,	1.136640736)
nd <- length(d)

# power values
p <- c(seq(.6,.9,.1),0.95)
np <- length(p)

# obtain sample sizes
samsize <- array(numeric(nd*np), dim=c(nd,np))
for (i in 1:np){
  for (j in 1:nd){
    result <- pwr.t.test(n = NULL, d = d[j],
                         sig.level = .1, power = p[i],
                         alternative = "two.sided")
    samsize[j,i] <- ceiling(result$n)
  }
}


