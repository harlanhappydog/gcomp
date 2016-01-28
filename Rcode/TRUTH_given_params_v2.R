####
# This code sets determines how different parameter settings influence diff= P(y2=1|X1=X2=1)-P(y2=1|X1=X2=0)
#
#
######################################################################## Design of experiment
lvls <- list(NA)

fixlev<-c(0, 0.25, 0.5)  # levels for not interesting parameters
seqlev<-seq(0,3,0.6)  # levels for  interesting parameters

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

interceptc<-(0.5)  # intercept for C1 and C2
intercepty<-(-2)   # intercept for Y1 and Y2


### factorial design
dsgn <- as.matrix(expand.grid(lvls[[1]], lvls[[2]], lvls[[3]], lvls[[4]], lvls[[5]], lvls[[6]], lvls[[7]], lvls[[8]], lvls[[9]], lvls[[10]], lvls[[11]]))
dim(dsgn)

######################################################################## CALCULATE TRUE VALUES OF diff
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
                dd <- (as.numeric(y1i == 1) * 1 + as.numeric(y1i == 0) * ( (1/(1+exp(-(intercepty+dsgn[i, 9]*myZ + dsgn[i, 5]*c2i + dsgn[i, 3]*myX2 + dsgn[i, 8]*myH)))) ))                

                individual[kk, 5:8] <- c(aa, bb, cc, dd)
            

                # Product
                individual[kk, 4] <- aa * bb * cc * dd
                truetheta00 <- truetheta00 + individual[kk, 4]
                
                kk <- kk + 1
            }
        }
    }
  
    matt[i, ] <- c(truetheta00, truetheta11)
    print(i/dim(dsgn)[1])
}
######################################################################## 


# Marginal probabilities of y2:
colMeans(matt)

diff<-round(matt[,2]-matt[,1],5)

library(ggplot2)

# Plots of interest:

#Theta3 has noticiable effect
qplot(dsgn[,3], diff,  color=as.factor(dsgn[,5]), shape=as.factor(dsgn[,6]))

# Theta5 and Theta6 also have effect
qplot(dsgn[,5], diff,  color=as.factor(dsgn[,3]))
qplot(dsgn[,6], diff,  color=as.factor(dsgn[,3]))

#However, theta6 only has effect if Theta5!=0
qplot(dsgn[dsgn[,5]==0,][,6], diff[dsgn[,5]==0],  color=as.factor(dsgn[dsgn[,5]==0,][,3]))
qplot(dsgn[dsgn[,5]!=0,][,6], diff[dsgn[,5]!=0],  color=as.factor(dsgn[dsgn[,5]!=0,][,3]))

#Whereas, Theta5 has effect regardless of Theta6
qplot(dsgn[dsgn[,6]==0,][,5], diff[dsgn[,6]==0],  color=as.factor(dsgn[dsgn[,6]==0,][,3]))
qplot(dsgn[dsgn[,6]!=0,][,5], diff[dsgn[,6]!=0],  color=as.factor(dsgn[dsgn[,6]!=0,][,3]))


# Other parameters do not have any noticable effect
qplot(dsgn[,1], diff,  color=as.factor(dsgn[,3]), shape=as.factor(dsgn[,6]))
qplot(dsgn[,2], diff,  color=as.factor(dsgn[,3]), shape=as.factor(dsgn[,6]))
qplot(dsgn[,4], diff,  color=as.factor(dsgn[,3]), shape=as.factor(dsgn[,6]))
qplot(dsgn[,7], diff,  color=as.factor(dsgn[,3]), shape=as.factor(dsgn[,6]))
qplot(dsgn[,11], diff,  color=as.factor(dsgn[,3]), shape=as.factor(dsgn[,6]))


mod1<-glm(diff~dsgn[,c(1:7,11)] + .*.*.,data=data.frame(dsgn[,c(1:7,11)]), family="binomial")

names(summary(mod1))
summary(mod1)$coefficients[summary(mod1)$coefficients[,4]<0.1,]


######################################################################## 