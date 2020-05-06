library(pwr)
#LKS
LKSair = c(0.170490732,0.166594092,0.12722457,0.121989762,0.133970801,0.094948229,0.132664101)
LKSecig = c(0.082141029,0.103628354,0.104122283,0.065893723,0.105368548,0.130310752)

#LKS-SLAM
LKS_SLAMair = c(0.008320311,0.017839334,0.011457651,0.010042741,0.006789061,0.009569881,0.01347924)
LKS_SLAMecig = c(0.007558115,0.004841033,0.008870753,0.005562457,0.004971517,0.012047859)

# # of BMCs/femur (^6)
BMCair = c(18.4,20.3,19,17.7)
BMCecig = c(17.8,19.3,19.6,16.2)

# # Lin neg/femur (^6)
Lin_negair = c(1.641192497,1.542995042,1.46062288,1.169271872)
Lin_negecig = c(1.150015758,1.426355187,1.232971862,0.907402775)
  
# LK/femur (^4)  
LKair = c(4.553842244,5.20680985,4.271918338,2.914683964)
LKecig = c(2.439388905,2.668649473,3.086847582,1.975528041)

airdata1 = rbind(LKSair,LKS_SLAMair)
ecigdata1 = rbind(LKSecig,LKS_SLAMecig)
airdata2 = rbind(BMCair,Lin_negair,LKair)
ecigdata2 = rbind(BMCecig,Lin_negecig,LKecig)

func = function(powervalue, alpha){
  samplesize = matrix(nrow = 5, ncol = length(powervalue))
  for(i in 1:2){
    for(j in 1:length(powervalue)){
      n1 = length(airdata1[i,])
      n2 = length(ecigdata1[i,])
      buttom = sqrt(((n1-1)*var((airdata1[i,])) + sqrt((n2-1)*var(ecigdata1[i,])))/(n1+n2-2))
      h = abs(mean(airdata1[i,])-mean(ecigdata1[i,]))/buttom
      result = pwr.t.test(d = h, power = powervalue[j], sig.level = alpha)
      samplesize[i,j] = result$n
    }
  }
  
  for(i in 1:3){
    for(j in 1:length(powervalue)){
      n1 = length(airdata2[i,])
      n2 = length(ecigdata2[i,])
      buttom = sqrt((var((airdata2[i,])) + sqrt(var(ecigdata2[i,])))/2)
      h = abs(mean(airdata2[i,])-mean(ecigdata2[i,]))/buttom
      result = pwr.t.test(d = h, power = powervalue[j], sig.level = alpha)
      samplesize[i+2,j] = result$n
    }
  }
  rownames(samplesize) = c("LKS","LKS-SLAM","BMCs","Lin_neg","LK")
  colnames(samplesize) = powervalue
  print(ceiling(samplesize))
}

powervalue = c(seq(0.6,0.9,0.1),0.95)
alpha = 0.05
samsize = func(powervalue = powervalue, alpha = alpha)

xrange <- range(powervalue)
yrange <- round(range(samsize))
colors <- rainbow(dim(samsize)[2])
plot(xrange, yrange, type="n",
     xlab="powervalue",
     ylab="Sample Size (n)" )
for (i in 1:dim(samsize)[1]){
  lines(powervalue, samsize[i,], type="l", lwd=2, col=colors[i])
}
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
       col="grey89")
title("Sample Size Estimation (Sig=0.05 (Two-tailed))")
legend("topleft", title="Power", as.character(powervalue),
       bty="n",lty=rep(1,5),lwd=rep(2,5),col=colors,cex=0.9)
