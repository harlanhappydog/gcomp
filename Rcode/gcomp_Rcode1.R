######################################################################## Design of experiment
lvls <- list(NA, NA, NA, NA)


### extent to which doing badly encourages going on treatment (hallmark of 'confounding by indication')
lvls[[1]] <- c(0.1, 0.25)

### extent to which going on treatment is more likely in the later time period (i.e., strength of instrument)
lvls[[2]] <- c(0.2, 0.4, 0.6)

### extent to which treatment *directly* reduces outcome risk
lvls[[3]] <- c(0, 0.02, 0.04)

### extent to which treatment inhibits transition of intermediate variable from better to worse state (i.e.,
### indirect benefit of treatment)
lvls[[4]] <- c(0, 0.09, 0.18)

### 
lvls[[5]] <- c(0.15)

### factorial design
dsgn <- as.matrix(expand.grid(lvls[[1]], lvls[[2]], lvls[[3]], lvls[[4]], lvls[[5]]))

ppp <- 1
est <- matrix(0, dim(dsgn)[1], 2)
kkk <- 1
result <- matrix(0, dim(dsgn)[1], 2)




######################################################################## CALCULATE TRUE VALUES OF THETA00 and THETA11
matt <- matrix(0, dim(dsgn)[1], 2)
for (i in c(1:dim(dsgn)[1])) {
    truetheta11 <- 0
    kk <- 1
    individual <- matrix(0, 8, 8)
    for (c2i in 0:1) {
        for (y1i in 0:1) {
            for (c1i in 0:1) {
                individual[kk, 1:3] <- c(y1i, c1i, c2i)
                # P(C1)
                aa <- (as.numeric(c1i == 0) * (0.75) + as.numeric(c1i == 1) * (0.25))
                # P(Y1|C1,X1)
                bb <- as.numeric(y1i == 1) * (0.2 - dsgn[i, 3] * 1 + dsgn[i, 5] * c1i) + as.numeric(y1i == 0) * 
                  (1 - (0.2 - dsgn[i, 3] * 1 + dsgn[i, 5] * c1i))
                # P(C2|C1,X1)
                cc <- as.numeric(c2i == 1) * (as.numeric(c1i == 1) * (0.95) + as.numeric(c1i == 0) * (0.25 - dsgn[i, 
                  4] * 1)) + as.numeric(c2i == 0) * (as.numeric(c1i == 1) * (1 - 0.95) + as.numeric(c1i == 0) * 
                  (1 - (0.25 - dsgn[i, 4] * 1)))
                # P(Y2|C2,X2,Y1)
                dd <- (as.numeric(y1i == 1) * 1 + as.numeric(y1i == 0) * (0.2 - dsgn[i, 3] * 1 + dsgn[i, 5] * 
                  c2i))
                
                individual[kk, 5:8] <- c(aa, bb, cc, dd)
                # Product
                individual[kk, 4] <- aa * bb * cc * dd
                truetheta11 <- truetheta11 + individual[kk, 4]
                kk <- kk + 1
            }
        }
    }
    
    truetheta00 <- 0
    kk <- 1
    individual <- matrix(0, 8, 8)
    for (c2i in 0:1) {
        for (y1i in 0:1) {
            for (c1i in 0:1) {
                individual[kk, 1:3] <- c(y1i, c1i, c2i)
                # P(C1)
                aa <- (as.numeric(c1i == 0) * (0.75) + as.numeric(c1i == 1) * (0.25))
                # P(Y1|C1,X1)
                bb <- as.numeric(y1i == 1) * (0.2 - dsgn[i, 3] * 0 + dsgn[i, 5] * c1i) + as.numeric(y1i == 0) * 
                  (1 - (0.2 - dsgn[i, 3] * 0 + dsgn[i, 5] * c1i))
                # P(C2|C1,X1)
                cc <- as.numeric(c2i == 1) * (as.numeric(c1i == 1) * (0.95) + as.numeric(c1i == 0) * (0.25 - dsgn[i, 
                  4] * 0)) + as.numeric(c2i == 0) * (as.numeric(c1i == 1) * (1 - 0.95) + as.numeric(c1i == 0) * 
                  (1 - (0.25 - dsgn[i, 4] * 0)))
                # P(Y2|C2,X2,Y1)
                dd <- (as.numeric(y1i == 1) * 1 + as.numeric(y1i == 0) * (0.2 - dsgn[i, 3] * 0 + dsgn[i, 5] * 
                  c2i))
                
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

numsim <- 50  ### number of datasets generated under each scenario
Bayesresult <- matrix(0, dim(dsgn)[1] * numsim, 8)
n <- 5000  ### size of each dataset
set.seed(13)  ### for reproducibility
seedlist <- sample(1:250000, size = numsim)



######################################################################## 

for (i in c(1:dim(dsgn)[1])) {
    ### loop over datasets
    for (j in 1:numsim) {
        print(c(i, j))
        
        set.seed(seedlist[j])

        H <- rbinom(n, size = 1, prob = 0.5)
        pr.h <- 0.5
        
        C1 <- rbinom(n, size = 1, prob = 0.25)
        pr.c1.h <- c(0.25, 0.25)
        
        X1 <- rbinom(n, size = 1, prob = 0.05 + dsgn[i, 1] * C1 + dsgn[i, 2] * H)
        
        C2 <- rep(NA, n)
        ### unlikely for C to go from bad to good state
        C2[C1 == 1] <- rbinom(sum(C1 == 1), size = 1, prob = 0.95)
        ### treatment may inhibit the good-to-bad transition
        C2[C1 == 0] <- rbinom(sum(C1 == 0), size = 1, prob = 0.25 - dsgn[i, 4] * X1[C1 == 0])
        
        tmp <- expand.grid(0:1, 0:1, 0:1)
        pr.c2.c1x1h <- tmp[, 1] * 0.95 + (1 - tmp[, 1]) * (0.25 - dsgn[i, 4] * tmp[, 2])
        
        X2 <- rep(NA, n)
        ### a few on treatment drop it
        X2[X1 == 1] <- rbinom(sum(X1 == 1), size = 1, prob = 0.9)
        ### same mechanism for going on treatment as at timetpoint 1
        X2[X1 == 0] <- rbinom(sum(X1 == 0), size = 1, prob = 0.05 + dsgn[i, 1] * C2 + dsgn[i, 2] * H)
        ### same mechanism for outcome as per timepoint 1
        Y1 <- rbinom(n, size = 1, prob = 0.2 - dsgn[i, 3] * X1 + 0.15 * C1)
        Y <- pmax(Y1, rbinom(n, size = 1, prob = 0.2 - dsgn[i, 3] * X2 + 0.15 * C2))
        
        Y2 <- Y
        simdata <- data.frame(Y2, Y1, X2, X1, C2, C1, H)
        
        # write.csv(simdata, paste('~/Desktop/UBC/Paul Gustafson/Data2/anothersimdata',i,'_',j,'.csv',sep='' ))
        
        # simdata<-read.csv(paste('~/Desktop/UBC/Paul Gustafson/Data2/anothersimdata',i,'_',j,'.csv',sep='' ))
        
        
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
            ss <- sum(simdata$Y2[simdata$H == h & simdata$C2 == c2 & simdata$X2 == x2 & simdata$C1 == c1 & simdata$X1 == 
                x1])
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
        
        
        nB <- 5e+05
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

AA <- Bayesresult

# save(AA,file='~/Desktop/UBC/Paul Gustafson/Rcode/AA.RData') AA<- read.csv('~/Desktop/UBC/Paul
# Gustafson/AA.csv')[,-1]

Bayesresult <- AA
diff <- (matt[, 1] - matt[, 2])

power <- matrix(0, dim(dsgn)[1], 3)
for (i in (dim(dsgn)[1]:1)) {
    power[i, ] <- c(i, mean(Bayesresult[Bayesresult[, 1] == i, ][, 7] <= 0.05), mean(Bayesresult[Bayesresult[, 
        1] == i, ][, 8] <= 0.05))
}

par(mfrow = c(2, 2), mar = c(6, 3, 3, 3))
plot(power[, 2] ~ diff, mar = c(9, 3, 3, 3), col = as.numeric(as.factor(dsgn[, 4])) + 7, pch = as.numeric(as.factor(dsgn[, 
    2])), lwd = 2, xlim = c(0, 0.1), ylim = c(0, 1))

plot(power[, 3] ~ diff, mar = c(9, 3, 3, 3), col = as.numeric(as.factor(dsgn[, 4])) + 7, pch = as.numeric(as.factor(dsgn[, 
    2])), lwd = 2, xlim = c(0, 0.1), ylim = c(0, 1))

plot(power[, 3] ~ power[, 2], mar = c(9, 3, 3, 3), col = as.numeric(as.factor(dsgn[, 4])) + 7, pch = as.numeric(as.factor(dsgn[, 
    2])), lwd = 2, xlim = c(0, 1), ylim = c(0, 1))
abline(0, 1)

plot(log(Bayesresult[Bayesresult[, 1] == 12, 7:8]))
lines(rep(log(0.25), 2), y = c(-100, 100))
lines(y = rep(log(0.25), 2), x = c(-100, 100))