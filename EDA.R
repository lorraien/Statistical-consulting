rm(list = ls())
library(gee)
library(nlme)
library(splines)
library(psych)
source( "http://www.ics.uci.edu/~dgillen/STAT212/Handouts/Stat212Functions.R")
load("D://Academic/STAT275/mouse/mouse_new.Rdata")
mouse <- mouse[mouse$trt != 'Control',]
mouse$t <- mouse$wks
mouse$x <- rep(0,nrow(mouse))
mouse[mouse$trt=='E-cig',"x"] = 1
mouse$x <- as.factor(mouse$x)
mouse <- mouse[order(mouse$id),]
mouse$y <- mouse$percent_45.2/(mouse$percent_45.1+mouse$percent_45.2)


mousegrouped <- groupedData(y ~ t | id, outer = ~trt,
                            data = mouse, labels = list( x = "t", y = "y" ) )
plot(mousegrouped, outer = ~ trt, aspect=1, key=FALSE,xlab="weeks",
     ylab="percentage_45.2" )

plot( mouse$wks, mouse$y, pch=".", xlab="weeks",
      ylab="percentage_45.2" )
lines( smooth.spline( mouse[mouse$x==0,"wks"], mouse[mouse$x==0,"y"], df=4),col='red' )
lines( smooth.spline( mouse[mouse$x==1,"wks"], mouse[mouse$x==1,"y"], df=4),col='blue' )
legend("topleft", legend=c("Air", "E-cig"),
       col=c("red", "blue"), lty=1, cex=0.8)


fit <- lm( y ~ ns( t, knots=c(6,10,14,18) ), data=mouse )
resids <- mouse$y - fitted( fit )
nobs <- length( mouse$y )
nsubjects <- length( table( mouse$id ) )
rmat <- matrix( NA, nsubjects, 4 )
ycat <- c(6,10,15,19)
nj <- unlist( lapply( split( mouse$id, mouse$id ), length ) )
mymin <- function(x){ifelse(sum(!is.na(x) )==0,
                            NA, min(x, na.rm=TRUE))}
for( j in 1:4 ){
  legal <- ( mouse$t >= ycat[j]-3.5 )&( mouse$t < ycat[j]+3.5 )
  jtime <- mouse$t + 0.1*rnorm(nobs)
  t0 <- unlist( lapply( split(abs(jtime - ycat[j]) , mouse$id), min))
  tj <- rep( t0, nj )
  keep <- ( abs( jtime - ycat[j] )==tj ) & ( legal )
  yj <- rep( NA, nobs )
  yj[keep] <- resids[keep]
  yj <- unlist( lapply( split( yj, mouse$id), mymin ) )
  rmat[ , j ] <- yj
}
dimnames( rmat ) <- list( NULL, paste("weeks",c("4-8","8-12","12-18","18-20")) )
pairs( rmat )


cmat <- matrix( 0, 4, 4 )
nmat <- matrix( 0, 4, 4 )
for( j in 1:4 ){
  for( k in j:4){
    njk <- sum( !is.na( rmat[,j]*rmat[,k] ) )
    sjk <- sum( rmat[,j]*rmat[,k], na.rm=T )/njk
    cmat[j,k] <- sjk
    nmat[j,k] <- njk
  }
}
vvec <- diag(cmat)
cormat <- cmat/( outer( sqrt(vvec), sqrt(vvec) ) )
print( round( cormat, 2 ) )

pairs.panels(rmat,smooth = FALSE, ellipses = FALSE, density = FALSE)

lda.variogram <- function( id, y, x ){
  uid <- unique( id )
  m <- length( uid )
  delta.y <- NULL
  delta.x <- NULL
  did <- NULL
  for( i in 1:m ){
    yi <- y[ id==uid[i] ]
    xi <- x[ id==uid[i] ]
    n <- length(yi)
    expand.j <- rep( c(1:n), n )
    expand.k <- rep( c(1:n), rep(n,n) )
    keep <- expand.j > expand.k
    if( sum(keep)>0 ){
      expand.j <- expand.j[keep]
      expand.k <- expand.k[keep]
      delta.yi <- 0.5*( yi[expand.j] - yi[expand.k] )^2
      delta.xi <- abs( xi[expand.j] - xi[expand.k] )
      didi <- rep( uid[i], length(delta.yi) )
      delta.y <- c( delta.y, delta.yi )
      delta.x <- c( delta.x, delta.xi )
      did <- c( did, didi )
    }
  }
  out <- list( id = did, delta.y = delta.y, delta.x = delta.x )
  out
}

fit <- lm( y ~ ns( t, knots=c(6,10,14,18) ), data=mouse )
resids <- mouse$y - fitted( fit )
out <- lda.variogram( id=mouse$id, y=resids, x=mouse$t )
dr <- out$delta.y
dt <- out$delta.x
var.est <- var( resids )
plot( dt, dr, pch=".", ylim=c(0, 1.2*var.est) )
lines( smooth.spline( dt, dr, df=5 ), lwd=3 )
abline( h=var.est, lty=2, lwd=2 )
title("residual variogram")
