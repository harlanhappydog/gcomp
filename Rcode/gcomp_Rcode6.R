######################################################################## Design of experiment
lvls <- list(NA, NA, NA, NA)

fixlev<-c(0, 0.25, 0.5)
seqlev<-seq(0,3,0.6)

lvls[[1]] <- fixlev

lvls[[2]] <- fixlev

lvls[[3]] <- seqlev

lvls[[4]] <- fixlev

lvls[[5]] <- seqlev

lvls[[6]] <-seqlev

lvls[[7]] <- fixlev

lvls[[8]] <- c(0)

lvls[[9]] <- c(0)

lvls[[10]] <- c(0)

lvls[[11]] <- fixlev

interceptc<-(0.5)
intercepty<-(-3)


### factorial design
dsgn <- as.matrix(expand.grid(lvls[[1]], lvls[[2]], lvls[[3]], lvls[[4]], lvls[[5]], lvls[[6]], lvls[[7]], lvls[[8]], lvls[[9]], lvls[[10]], lvls[[11]]))
dim(dsgn)




######################################################################## CALCULATE TRUE VALUES OF THETA00 and THETA11
matt <- matrix(0, dim(dsgn)[1], 2)
for (i in c(1:dim(dsgn)[1])) {
truetheta11 <- 0
kk <- 1
individual <- matrix(0, 8, 8)
 myH<-0
 myZ<-0
 myX1<-1
 myX2<-1
    for (c2i in 0:1) {
        for (y1i in 0:1) {
            for (c1i in 0:1) {
                individual[kk, 1:3] <- c(y1i, c1i, c2i)
 
                # P(C1=1 or =0)
                pc1<-1/(1+exp(-(interceptc )))
                aa <- (as.numeric(c1i == 1) * (pc1) + as.numeric(c1i == 0) * (1-pc1))

                # P(Y1=1 or =0|C1,X1)
                py1<-1/(1+exp(-(intercepty + dsgn[i, 5]*c1i + dsgn[i, 3]*myX1 + dsgn[i, 8]*myH)))
                bb <- as.numeric(y1i == 1) * (py1) + as.numeric(y1i == 0) * (1 - py1)

                # P(C2=1 or =0|C1,X1)
                pc2<-(1/(1+exp(-(interceptc + dsgn[i, 4]*c1i + dsgn[i, 6]*myX1))))
                cc <- as.numeric(c2i == 1) * (pc2)  + as.numeric(c2i == 0)*(1-pc2)
                
                # P(Y2=1|C2,X2,Y1)
                dd <- (as.numeric(y1i == 1) * 1 + as.numeric(y1i == 0) * ( (1/(1+exp(-(intercepty+dsgn[i, 9]*myZ + dsgn[i, 5]*c2i + dsgn[i, 3]*myX2 + dsgn[i, 8]*myH)))) ))                

                individual[kk, 5:8] <- c(aa, bb, cc, dd)

                # Product
                individual[kk, 4] <- aa * bb * cc * dd
                truetheta11 <- truetheta11 + individual[kk, 4]
                kk <- kk + 1
            }
        }
    }
    
    truetheta00<- 0
    kk <- 1
    individual <- matrix(0, 8, 8)
    myH<-0
    myZ<-0
    myX1<-0
    myX2<-0
    for (c2i in 0:1) {
        for (y1i in 0:1) {
            for (c1i in 0:1) {
                individual[kk, 1:3] <- c(y1i, c1i, c2i)
 
                # P(C1=1 or =0)
                pc1<-1/(1+exp(-(interceptc )))
                aa <- (as.numeric(c1i == 1) * (pc1) + as.numeric(c1i == 0) * (1-pc1))

                # P(Y1=1 or =0|C1,X1)
                py1<-1/(1+exp(-(intercepty + dsgn[i, 5]*c1i + dsgn[i, 3]*myX1 + dsgn[i, 8]*myH)))
                bb <- as.numeric(y1i == 1) * (py1) + as.numeric(y1i == 0) * (1 - py1)

                # P(C2=1 or =0|C1,X1)
                pc2<-(1/(1+exp(-(interceptc + dsgn[i, 4]*c1i + dsgn[i, 6]*myX1))))
                cc <- as.numeric(c2i == 1) * (pc2)  + as.numeric(c2i == 0)*(1-pc2)
                
                # P(Y2=1|C2,X2,Y1)
                dd <- (as.numeric(y1i == 1) * 1 + as.numeric(y1i == 0) * ( (1/(1+exp(-(intercepty + dsgn[i, 9]*myZ + dsgn[i, 5]*c2i + dsgn[i, 3]*myX2 + dsgn[i, 8]*myH)))) ))                

                individual[kk, 5:8] <- c(aa, bb, cc, dd)

                # Product
                individual[kk, 4] <- aa * bb * cc * dd
                truetheta00 <- truetheta00 + individual[kk, 4]
                kk <- kk + 1
            }
        }
    }
    matt[i, ] <- c(truetheta00, truetheta11)
}
######################################################################## 

colMeans(matt)

diff<-round(matt[,2]-matt[,1],5)

library(ggplot2)

qplot(dsgn[,3], diff,  color=as.factor(dsgn[,5]), shape=as.factor(dsgn[,6]))

qplot(dsgn[,5], diff,  color=as.factor(dsgn[,3]))
qplot(dsgn[,6], diff,  color=as.factor(dsgn[,3]))

qplot(dsgn[dsgn[,5]==0,][,6], diff[dsgn[,5]==0],  color=as.factor(dsgn[dsgn[,5]==0,][,3]))
qplot(dsgn[dsgn[,6]==0,][,5], diff[dsgn[,6]==0],  color=as.factor(dsgn[dsgn[,6]==0,][,3]))


qplot(dsgn[dsgn[,5]!=0,][,6], diff[dsgn[,5]!=0],  color=as.factor(dsgn[dsgn[,5]!=0,][,3]))
qplot(dsgn[dsgn[,6]!=0,][,5], diff[dsgn[,6]!=0],  color=as.factor(dsgn[dsgn[,6]!=0,][,3]))



qplot(dsgn[,1], diff,  color=as.factor(dsgn[,3]), shape=as.factor(dsgn[,6]))
qplot(dsgn[,2], diff,  color=as.factor(dsgn[,3]), shape=as.factor(dsgn[,6]))
qplot(dsgn[,4], diff,  color=as.factor(dsgn[,3]), shape=as.factor(dsgn[,6]))
qplot(dsgn[,7], diff,  color=as.factor(dsgn[,3]), shape=as.factor(dsgn[,6]))
qplot(dsgn[,11], diff,  color=as.factor(dsgn[,3]), shape=as.factor(dsgn[,6]))


######################################################################## 


######################################################################## 
ppp <- 1
est <- matrix(0, dim(dsgn)[1], 2)
kkk <- 1
result <- matrix(0, dim(dsgn)[1], 2)
numsim <- 100  ### number of datasets generated under each scenario
Bayesresult <- matrix(0, dim(dsgn)[1] * numsim, 8)
n <- 1000  ### size of each dataset
set.seed(13)  ### for reproducibility
seedlist <- sample(1:250000, size = numsim)

######################################################################## 

for (i in c(1:dim(dsgn)[1])) {
    ### loop over datasets
    for (j in 1:numsim) {
        print(c(i, j))
        
        set.seed(seedlist[j])

        H <- rbinom(n, size = 1, prob = 0.5)
        Z<-rbinom(n, 1, (1/(1+exp(-(0.5 + dsgn[i, 10]*H)))) )
        C1 <- rbinom(n, size = 1, prob = 0.5)
        X1<- rbinom(n, 1, (1/(1+exp(-(0.5 + dsgn[i, 1]*C1 + dsgn[i, 2]*H)))) ) 
        C2 <-rbinom(n, 1, (1/(1+exp(-(0.5 + dsgn[i, 4]*C1 + dsgn[i, 6]*X1)))) )         
        Y1 <-rbinom(n, 1, (1/(1+exp(-(0.5 + dsgn[i, 5]*C1 + dsgn[i, 3]*X1 + dsgn[i, 8]*H)))) ) 
        X2 <-rbinom(n, 1, (1/(1+exp(-(0.5 + dsgn[i, 1]*C2  + dsgn[i, 2]*H + dsgn[i, 7]*Y1 + dsgn[i, 9]*Z + dsgn[i, 11]*X1)))) )
        Y2 <- rep(1, n)  
        Y2[Y1 == 0]<-rbinom(n, 1, (1/(1+exp(-(0.5 + dsgn[i, 9]*Z + dsgn[i, 5]*C2 + dsgn[i, 3]*X2 + dsgn[i, 8]*H)))) )     
        simdata <- data.frame(H,Z,C1,X1,C2,Y1,X2,Y2)
        print(head(simdata))

######################################################################## 
        HprobB <- function(h, nm = 1) {
            ss <- sum(simdata$H)
            nn <- length(simdata$H)
            pp <- (rbeta(nm, (ss + 1), (nn - ss + 1)))
            if (h == 0) {
                return(1 - as.numeric(pp))
            }
            if (h == 1) {
                return(as.numeric(pp))
            }
        }
        
        
        C1probB <- function(h, c1, nm = 1) {
            ss <- sum(simdata$C1[simdata$H == h])
            nn <- length(simdata$C1[simdata$H == h])
            pp <- (rbeta(nm, (ss + 1), (nn - ss + 1)))
            if (c1 == 0) {
                return(1 - as.numeric(pp))
            }
            if (c1 == 1) {
                return(as.numeric(pp))
            }
        }
        
        C2probB <- function(h, c2, c1, x1, nm = 1) {
            ss <- sum(simdata$C2[simdata$H == h & simdata$C1 == c1 & simdata$X1 == x1])
            nn <- length(simdata$C2[simdata$H == h & simdata$C1 == c1 & simdata$X1 == x1])
            pp <- (rbeta(nm, (ss + 1), (nn - ss + 1)))
            if (c2 == 0) {
                return(1 - as.numeric(pp))
            }
            if (c2 == 1) {
                return(as.numeric(pp))
            }
        }
        
        Y2probB <- function(h, y2, c1, c2, x1, x2, nm = 1) {
            ss <- sum(simdata$Y2[simdata$H == h & simdata$C2 == c2 & simdata$X2 == x2 & simdata$C1 == c1 & simdata$X1 == x1])
            nn <- length(simdata$Y2[simdata$H == h & simdata$C2 == c2 & simdata$X2 == x2 & simdata$C1 == c1 & 
                simdata$X1 == x1])
            pp <- (rbeta(nm, (ss + 1), (nn - ss + 1)))
            if (y2 == 0) {
                return(1 - as.numeric(pp))
            }
            if (y2 == 1) {
                return(as.numeric(pp))
            }
        }
        
        
        nB <- 500
        sumtheta11B <- 0
        sumtheta00B <- 0
        for (c2i in 0:1) {
            for (c1i in 0:1) {
                for (hi in 0:1) {
                  
                  sumtheta11B <- sumtheta11B + HprobB(h = hi, nB) * C1probB(h = hi, c1 = c1i, nm = nB) * C2probB(h = hi, 
                    c2 = c2i, c1 = c1i, x1 = 1, nm = nB) * Y2probB(h = hi, y2 = 1, c1 = c1i, c2 = c2i, x1 = 1, 
                    x2 = 1, nm = nB)
                  
                  sumtheta00B <- sumtheta00B + HprobB(h = hi, nB) * C1probB(h = hi, c1 = c1i, nm = nB) * C2probB(h = hi, 
                    c2 = c2i, c1 = c1i, x1 = 0, nm = nB) * Y2probB(h = hi, y2 = 1, c1 = c1i, c2 = c2i, x1 = 0, 
                    x2 = 0, nm = nB)
                  
                }
            }
        }
        
        sumtheta00Bvec <- sumtheta00B
        sumtheta11Bvec <- sumtheta11B
        Bpval <- 1 - mean((sumtheta00Bvec - sumtheta11Bvec) > 0)
        
        Hpval <- summary(glm(Y2 ~ H, data = simdata, family = "binomial"))$coefficients[2, 4]
        
        Bayesresult[kkk, ] <- c(i, j, c(1, 1), mean(sumtheta00Bvec), mean(sumtheta11Bvec), Bpval, Hpval)
        kkk <- kkk + 1
    }
}

######################################################################## 



AA <- Bayesresult
Bayesresult <- AA


power <- matrix(0, dim(dsgn)[1], 3)
for (i in (dim(dsgn)[1]:1)) {
    power[i, ] <- c(i, mean(Bayesresult[Bayesresult[, 1] == i, ][, 7] <= 0.15), mean(Bayesresult[Bayesresult[, 
        1] == i, ][, 8] <= 0.15))
}

par(mfrow = c(2, 2), mar = c(6, 3, 3, 3))
plot(power[, 2] ~ dsgn[,3], mar = c(9, 3, 3, 3), col = as.numeric(as.factor(dsgn[, 2])) + 1, pch = as.numeric(as.factor(dsgn[, 
    2])), lwd = 2, xlim = c(0, 0.1), ylim = c(0, 1))

plot(power[, 3] ~ dsgn[,3], mar = c(9, 3, 3, 3), col = as.numeric(as.factor(dsgn[, 2])) + 1, pch = as.numeric(as.factor(dsgn[, 
    2])), lwd = 2, xlim = c(0, 0.1), ylim = c(0, 1))

plot(power[, 3] ~ power[, 2], mar = c(9, 3, 3, 3), col = as.numeric(as.factor(dsgn[, 2])) + 1, pch = as.numeric(as.factor(dsgn[, 
    2])), lwd = 2, xlim = c(0, 1), ylim = c(0, 1))
abline(0, 1)

plot(log(Bayesresult[Bayesresult[, 1] == 12, 7:8]))
lines(rep(log(0.25), 2), y = c(-100, 100))
lines(y = rep(log(0.25), 2), x = c(-100, 100))