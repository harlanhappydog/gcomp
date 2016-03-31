####
# This code sets determines how different parameter settings influence diff= P(y2=1|X1=X2=1)-P(y2=1|X1=X2=0)
#
#

# 1 to be determined
# 2 no effect (good at zero)
# 3 must be zero
# 4 ==0.5 (high)
# 5 ==2    (high)
# 6 ==0    (zero)
# 7 ==0.5 (high)

######################################################################## Design of experiment
lvls <- list(NA)

fixlev<-c(0,0.5)  # levels for not interesting parameters
#seqlev<-round(seq(0,4,0.3333),3)  # levels for  interesting parameters
seqlev<-c(0,1,2) # levels for  interesting parameters

lvls[[1]] <- c(0)  # violation

lvls[[2]] <- c(0, 1, 2)

lvls[[3]] <- c(0) 

lvls[[4]] <- c(0, 1, 2) 

lvls[[5]] <- c(0, 1, 2)

lvls[[6]] <-c(0, 1, 2)

lvls[[7]] <- c(0) # violation

lvls[[8]] <- c(0)

lvls[[9]] <- c(0)

lvls[[10]] <- c(0)

lvls[[11]] <- c(0,  1,  2)

interceptc<-(0)  # intercept for C1 and C2
intercepty<-(-1)   # intercept for Y1 and Y2
intercepth<-(-1)  #intercept for H
interceptz<-0.5
interceptx<-0.5

### factorial design
dsgn <- as.matrix(expand.grid(lvls[[1]], lvls[[2]], lvls[[3]], lvls[[4]], lvls[[5]], lvls[[6]], lvls[[7]], lvls[[8]], lvls[[9]], lvls[[10]], lvls[[11]]))
dim(dsgn)
dim(dsgn)[1]
######################################################################## 
### CALCULATE TRUE VALUES OF diff
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

library(ggplot2)
# Marginal probabilities of y2:
colMeans(matt)

diff_theta<-round(matt[,2]-matt[,1],5)
summary(diff_theta)

plot1<-qplot(matt[,2],matt[,1],  color=as.factor(dsgn[,5]), shape=as.factor(dsgn[,6]), ylim=c(0,1), xlim=c(0,1))+geom_abline(slope=1,intercept=0)
plot2<-qplot(matt[,2],matt[,1],  color=as.factor(dsgn[,3]), shape=as.factor(dsgn[,6]), ylim=c(0,1), xlim=c(0,1))+geom_abline(slope=1,intercept=0)
grid.arrange(plot1, plot2, ncol=1)

# Plots of interest:

#Theta3 has noticiable effect
qplot(dsgn[,3]+(dsgn[,6])/20, diff_theta,  color=as.factor(dsgn[,5]), shape=as.factor(dsgn[,4]))

# Theta5 and Theta6 also have effect
qplot(dsgn[,5]+(dsgn[,6])/20, diff_theta,  color=as.factor(dsgn[,3]), shape=as.factor(dsgn[,4]))
qplot(dsgn[,6]+(dsgn[,5])/20, diff_theta,  color=as.factor(dsgn[,3]), shape=as.factor(dsgn[,4]))

######################################################################
mod1<-glm(diff_theta~dsgn[,c(1:7,11)] + .*.*.,data=data.frame(dsgn[,c(1:7,11)]), family="binomial")
summary(mod1)
summary(mod1)$coefficients[summary(mod1)$coefficients[,4]<0.99,]



######################################################################## 
# SIMULATIONS STUDY
######################################################################## 
ppp <- 1
est <- matrix(0, dim(dsgn)[1], 2)
kkk <- 1
result <- matrix(0, dim(dsgn)[1], 2)
numsim <- 500  ### number of datasets generated under each scenario
Bayesresult <- matrix(0, dim(dsgn)[1] * numsim, 8)
n <- 200  ### size of each dataset
#set.seed(13)  ### for reproducibility
seedlist <- sample(1:250000, size = numsim)

######################################################################## 
simdatasum<-matrix(0,dim(dsgn)[1],8)
colnames(simdatasum)<-c( "H",     "Z",    "C1",    "X1",    "C2",    "Y1",    "X2",    "Y2") 
for (i in c(1:dim(dsgn)[1])) {
    ### loop over datasets
    for (j in 1:numsim) {
        print(c(i, j))
print(        kkk/(dim(dsgn)[1] * numsim))
#        set.seed(seedlist[j])


	    H <- rbinom(n, size = 1, prob = (1/(1+exp(-(intercepth)))) )
        Z  <- rbinom(n, 1, (1/(1+exp(-(interceptz + dsgn[i, 10]*H)))) )
        C1 <- rbinom(n, size = 1, prob = (1/(1+exp(-(interceptc)))) )
        X1<- rbinom(n, 1, (1/(1+exp(-(interceptx + dsgn[i, 1]*C1 + dsgn[i, 2]*H)))) ) 
        C2 <-rbinom(n, 1, (1/(1+exp(-(interceptc + dsgn[i, 4]*C1 + dsgn[i, 6]*X1)))) )         
        Y1 <-rbinom(n, 1, (1/(1+exp(-(intercepty + dsgn[i, 5]*C1 + dsgn[i, 3]*X1 + dsgn[i, 8]*H)))) ) 
        X2 <-rbinom(n, 1, (1/(1+exp(-(interceptx + dsgn[i, 1]*C2  + dsgn[i, 2]*H + dsgn[i, 7]*Y1 + dsgn[i, 9]*Z + dsgn[i, 11]*X1)))) )
        Y2 <- rep(1, n)  
        Y2[Y1 == 0]<-rbinom(length(Y2[Y1 == 0]), 1, (1/(1+exp(-(intercepty + dsgn[i, 9]*Z + dsgn[i, 5]*C2 + dsgn[i, 3]*X2 + dsgn[i, 8]*H)))) )     
 
        simdata <- data.frame(H,Z,C1,X1,C2,Y1,X2,Y2)
		simdatasum[i,]<-colMeans(simdata)
        print(colMeans(simdata))
             
####################################################################### 
# ## BAYESIAN APPROACH:  WITHOUT Y1, WITHOUT H (uniform priors):
        
        C1probB <- function( c1, nm = 1) {
            ss <- sum(simdata$C1)
            nn <- length(simdata$C1)
            pp <- (rbeta(nm, (ss + 1), (nn - ss + 1)))
            if (c1 == 0) { return(1 - as.numeric(pp)) }
            if (c1 == 1) { return(as.numeric(pp))}}
        
        C2probB <- function(c2, c1, x1, nm = 1) {
            ss <- sum(simdata$C2[ simdata$C1 == c1 & simdata$X1 == x1])
            nn <- length(simdata$C2[ simdata$C1 == c1 & simdata$X1 == x1])
            pp <- (rbeta(nm, (ss + 1), (nn - ss + 1)))
            if (c2 == 0) { return(1 - as.numeric(pp)) }
            if (c2 == 1) { return(as.numeric(pp))}}
        
        Y2probB <- function( y2, c1, c2, x1, x2, nm = 1) {
            ss <- sum(simdata$Y2[ simdata$C2 == c2 & simdata$X2 == x2 & simdata$C1 == c1 & simdata$X1 == x1])
            nn <- length(simdata$Y2[ simdata$C2 == c2 & simdata$X2 == x2 & simdata$C1 == c1 & simdata$X1 == x1])
            pp <- (rbeta(nm, (ss + 1), (nn - ss + 1)))
            if (y2 == 0) { return(1 - as.numeric(pp))}
            if (y2 == 1) {return(as.numeric(pp)) } }
        
        
        nB <- 200
        sumtheta11B <- 0
        sumtheta00B <- 0
        for (c2i in 0:1) {
            for (c1i in 0:1) {
                  
                  sumtheta11B <- sumtheta11B + C1probB( c1 = c1i, nm = nB) * 
                  C2probB(   c2 = c2i, c1 = c1i, x1 = 1, nm = nB) * 
                  Y2probB( y2 = 1, c1 = c1i, c2 = c2i, x1 = 1,  x2 = 1, nm = nB)
                  
                  sumtheta00B <- sumtheta00B + C1probB( c1 = c1i, nm = nB) * C2probB( 
                    c2 = c2i, c1 = c1i, x1 = 0, nm = nB) * Y2probB( y2 = 1, c1 = c1i, c2 = c2i, x1 = 0, 
                    x2 = 0, nm = nB)
                  
                }
            }

        
        sumtheta00Bvec_wY1_wH <- sumtheta00B
        sumtheta11Bvec_wY1_wH <- sumtheta11B

# ################################################################
# ## BAYESIAN APPROACH WITH Y1, WITHOUT H (uniform priors):

C1probB<-function(c1,nm=1){
	ss<-sum(simdata$C1)
	nn<-length(simdata$C1)
	pp<-(rbeta(nm,(ss+1),(nn-ss+1)))
	if(c1==0){return(1-as.numeric(pp))}
	if(c1==1){return(as.numeric(pp))}}

Y1probB<-function(y1,c1,x1,nm=1){
	ss<-sum(simdata$Y1[simdata$X1==x1 & simdata$C1==c1])
	nn<-length(simdata$Y1[simdata$X1==x1 & simdata$C1==c1])
	pp<-(rbeta(nm,(ss+1),(nn-ss+1)))
	if(y1==0){return(1-as.numeric(pp))}
	if(y1==1){return(as.numeric(pp))}}

C2probB<-function(c2,c1,x1,nm=1){
	ss<-sum(simdata$C2[simdata$C1==c1 & simdata$X1==x1])
	nn<-length(simdata$C2[simdata$C1==c1 & simdata$X1==x1])
	pp<-(rbeta(nm,(ss+1),(nn-ss+1)))
	if(c2==0){return(1-as.numeric(pp))}
	if(c2==1){return(as.numeric(pp))}}

Y2probB<-function(y2,c2,x2,y1,nm=1){
	ss<-sum(simdata$Y2[simdata$C2==c2 & simdata$X2==x2 & simdata$Y1==y1])
	nn<-length(simdata$Y2[simdata$C2==c2 & simdata$X2==x2 & simdata$Y1==y1])
	pp<-(rbeta(nm,(ss+1),(nn-ss+1)))
	if(y2==0){return(1-as.numeric(pp))}
	if(y2==1){return(as.numeric(pp))}}

nB<-200
# Integrate over the values of C1 and C2: Prob(Y2=1 | X1=1 and X2=1)
sumtheta11B<-0
sumtheta00B<-0
for(c2i in 0:1){
for(y1i in 0:1){
for(c1i in 0:1){
sumtheta11B<-sumtheta11B+
C1probB(c1=c1i,nm=nB)*
Y1probB(y1=y1i,c1=c1i,x1=1,nm=nB)*
C2probB(c2=c2i,c1=c1i,x1=1,nm=nB)*
Y2probB(y2=1,c2=c2i,x2=1,y1=y1i,nm=nB)

sumtheta00B<-sumtheta00B +
C1probB(c1=c1i,nm=nB)*
Y1probB(y1=y1i,c1=c1i,x1=0,nm=nB)*
C2probB(c2=c2i,c1=c1i,x1=0,nm=nB)*
Y2probB(y2=1,c2=c2i,x2=0,y1=y1i,nm=nB)
}}}

        sumtheta00Bvec_Y1_wH <- sumtheta00B
        sumtheta11Bvec_Y1_wH <- sumtheta11B


######################################################################## 
# ## BAYESIAN APPROACH WITHOUT Y1 WITH H  (uniform priors):
        HprobB <- function(h, nm = 1) {
            ss <- sum(simdata$H)
            nn <- length(simdata$H)
            pp <- (rbeta(nm, (ss + 1), (nn - ss + 1)))
            if (h == 0) { return(1 - as.numeric(pp)) }
            if (h == 1) {  return(as.numeric(pp))}}
           
        C1probB <- function(h, c1, nm = 1) {
            ss <- sum(simdata$C1[simdata$H == h])
            nn <- length(simdata$C1[simdata$H == h])
            pp <- (rbeta(nm, (ss + 1), (nn - ss + 1)))
            if (c1 == 0) {return(1 - as.numeric(pp))}
            if (c1 == 1) {return(as.numeric(pp))}}
        
        C2probB <- function(h, c2, c1, x1, nm = 1) {
            ss <- sum(simdata$C2[simdata$H == h & simdata$C1 == c1 & simdata$X1 == x1])
            nn <- length(simdata$C2[simdata$H == h & simdata$C1 == c1 & simdata$X1 == x1])
            pp <- (rbeta(nm, (ss + 1), (nn - ss + 1)))
            if (c2 == 0) {return(1 - as.numeric(pp))}
            if (c2 == 1) {return(as.numeric(pp))} }
        
        Y2probB <- function(h, y2, c1, c2, x1, x2, nm = 1) {
            ss <- sum(simdata$Y2[simdata$H == h & simdata$C2 == c2 & simdata$X2 == x2 & simdata$C1 == c1 & simdata$X1 == x1])
            nn <- length(simdata$Y2[simdata$H == h & simdata$C2 == c2 & simdata$X2 == x2 & simdata$C1 == c1 & simdata$X1 == x1])
            pp <- (rbeta(nm, (ss + 1), (nn - ss + 1)))
            if (y2 == 0) {return(1 - as.numeric(pp))}
            if (y2 == 1) {return(as.numeric(pp))}}
        
        
        nB <- 200
        sumtheta11B <- 0
        sumtheta00B <- 0
        for (c2i in 0:1) {
            for (c1i in 0:1) {
                for (hi in 0:1) {
                  
                  sumtheta11B <- sumtheta11B + HprobB(h = hi, nB) * C1probB(h = hi, c1 = c1i, nm = nB) * 
                  C2probB(h = hi,   c2 = c2i, c1 = c1i, x1 = 1, nm = nB) * 
                  Y2probB(h = hi, y2 = 1, c1 = c1i, c2 = c2i, x1 = 1,  x2 = 1, nm = nB)
                  
                  sumtheta00B <- sumtheta00B + HprobB(h = hi, nB) * C1probB(h = hi, c1 = c1i, nm = nB) * C2probB(h = hi, 
                    c2 = c2i, c1 = c1i, x1 = 0, nm = nB) * Y2probB(h = hi, y2 = 1, c1 = c1i, c2 = c2i, x1 = 0, 
                    x2 = 0, nm = nB)
                  
                }
            }
        }
        
        sumtheta00Bvec_wY1_H <- sumtheta00B
        sumtheta11Bvec_wY1_H <- sumtheta11B

# ################################################################
# ## BAYESIAN APPROACH WITH Y1 WITH H (uniform priors):
     HprobB <- function(h, nm = 1) {
            ss <- sum(simdata$H)
            nn <- length(simdata$H)
            pp <- (rbeta(nm, (ss + 1), (nn - ss + 1)))
            if (h == 0) {return(1 - as.numeric(pp))}
            if (h == 1) {return(as.numeric(pp))}}
        
      C1probB <- function(h, c1, nm = 1) {
            ss <- sum(simdata$C1[simdata$H == h])
            nn <- length(simdata$C1[simdata$H == h])
            pp <- (rbeta(nm, (ss + 1), (nn - ss + 1)))
            if (c1 == 0) {return(1 - as.numeric(pp))}
            if (c1 == 1) {return(as.numeric(pp))}}

	 Y1probB<-function(h, y1,c1,x1,nm=1){
			ss<-sum(simdata$Y1[simdata$H == h & simdata$X1==x1 & simdata$C1==c1])
			nn<-length(simdata$Y1[simdata$H == h & simdata$X1==x1 & simdata$C1==c1])
			pp<-(rbeta(nm,(ss+1),(nn-ss+1)))
			if(y1==0){return(1-as.numeric(pp))}
			if(y1==1){return(as.numeric(pp))}}

    C2probB <- function(h, c2, c1, x1, nm = 1) {
            ss <- sum(simdata$C2[simdata$H == h & simdata$C1 == c1 & simdata$X1 == x1])
            nn <- length(simdata$C2[simdata$H == h & simdata$C1 == c1 & simdata$X1 == x1])
            pp <- (rbeta(nm, (ss + 1), (nn - ss + 1)))
            if (c2 == 0) {return(1 - as.numeric(pp))}
            if (c2 == 1) {return(as.numeric(pp))}}

	Y2probB<-function(h, y2,c2,x2,y1,nm=1){
			ss<-sum(simdata$Y2[simdata$H == h & simdata$C2==c2 & simdata$X2==x2 & simdata$Y1==y1])
			nn<-length(simdata$Y2[simdata$H == h & simdata$C2==c2 & simdata$X2==x2 & simdata$Y1==y1])
			pp<-(rbeta(nm,(ss+1),(nn-ss+1)))
			if(y2==0){return(1-as.numeric(pp))}
			if(y2==1){return(as.numeric(pp))}}

nB<-200
# Integrate over the values of C1 and C2: Prob(Y2=1 | X1=1 and X2=1)
sumtheta11B<-0
sumtheta00B<-0
for(c2i in 0:1){
for(y1i in 0:1){
for(c1i in 0:1){
	                for (hi in 0:1) {
sumtheta11B<-sumtheta11B+C1probB(h= hi , c1=c1i,nm=nB)*Y1probB(h= hi, y1=y1i,c1=c1i,x1=1,nm=nB)*C2probB(h= hi, c2=c2i,c1=c1i,x1=1,nm=nB)*Y2probB(h=hi, y2=1,c2=c2i,x2=1,y1=y1i,nm=nB)
sumtheta00B<-sumtheta00B +C1probB(h=hi,c1=c1i,nm=nB)*Y1probB(h=hi,y1=y1i,c1=c1i,x1=0,nm=nB)*C2probB(h=hi,c2=c2i,c1=c1i,x1=0,nm=nB)*Y2probB(h=hi,y2=1,c2=c2i,x2=0,y1=y1i,nm=nB)
}}}}

        sumtheta00Bvec_Y1_H <- sumtheta00B
        sumtheta11Bvec_Y1_H <- sumtheta11B

##################################
##################################
##################################


#one-sided p-values:
        Bpval_Y1_H <- 1 - mean((sumtheta11Bvec_Y1_H - sumtheta00Bvec_Y1_H) > 0)
        Bpval_wY1_H <- 1 - mean((sumtheta11Bvec_wY1_H - sumtheta00Bvec_wY1_H) > 0)
        Bpval_Y1_wH <- 1 - mean((sumtheta11Bvec_Y1_wH - sumtheta00Bvec_Y1_wH) > 0)
		Bpval_wY1_wH <- 1 - mean((sumtheta11Bvec_wY1_wH - sumtheta00Bvec_wY1_wH) > 0)

        Hpval <- (1-pnorm((summary(glm(Y2 ~ H, data = simdata, family = "binomial"))$coefficients[2, 3])))

        Bayesresult[kkk, ] <- c(i, j, diff_theta[i], Bpval_Y1_H, Bpval_wY1_H, Bpval_Y1_wH, Bpval_wY1_wH, Hpval)
        kkk <- kkk + 1
    }
}

######################################################################## 


alpha<-0.1
AA <- Bayesresult
Bayesresult <- AA

colnames(Bayesresult)<-c("i","j","diff_theta","with Y1 with H","without Y1 with H","with Y1 without H","without Y1 without H", "IV")
power <- matrix(0, dim(dsgn)[1], 7)
for (i in (dim(dsgn)[1]:1)) {
    power[i, ] <- c(i, 
    mean(Bayesresult[Bayesresult[, 1] == i, ][, 3]), 
    mean(Bayesresult[Bayesresult[, 1] == i, ][, 4] <= alpha), 
    mean(Bayesresult[Bayesresult[, 1] == i, ][, 5] <= alpha), 
    mean(Bayesresult[Bayesresult[,1] == i, ][, 6] <= alpha), 
    mean(Bayesresult[Bayesresult[,1] == i, ][, 7] <= alpha), 
    mean(Bayesresult[Bayesresult[,1] == i, ][, 8] <= alpha))
}

colnames(power)<-c("i","diff_theta","with Y1 with H","without Y1 with H","with Y1 without H","without Y1 without H", "IV")

require(gridExtra)
plot1 <- qplot(diff_theta,    power[, 5], col = as.factor(dsgn[, 3]), shape=as.factor(dsgn[, 5]), ylim=c(0,1))+geom_abline(slope=0,intercept=alpha)+  geom_jitter(width = 0.05, height = 0)+ggtitle(paste(colnames(power)[5]))
plot2 <- qplot(diff_theta,    power[, 6], col = as.factor(dsgn[, 2]), shape=as.factor(dsgn[, 3]), ylim=c(0,1))+geom_abline(slope=0,intercept=alpha)+  geom_jitter(width = 0.05, height = 0)+ggtitle(paste(colnames(power)[6]))
grid.arrange(plot1, plot2, ncol=1) 

nullpower<-power[diff_theta==0,]

require(gridExtra)
plot1 <- qplot((dsgn[, 5][diff_theta==0]),    nullpower[, 3], col = as.factor(dsgn[, 2][diff_theta==0]), shape=as.factor(dsgn[, 6][diff_theta==0]), ylim=c(0,0.5))+geom_abline(slope=0,intercept=alpha)+  geom_jitter(width = 0.2, height = 0)+ggtitle(paste(colnames(power)[3]))
plot2 <- qplot((dsgn[, 5][diff_theta==0]),    nullpower[, 4], col = as.factor(dsgn[, 2][diff_theta==0]), shape=as.factor(dsgn[, 6][diff_theta==0]), ylim=c(0,0.5))+geom_abline(slope=0,intercept=alpha)+  geom_jitter(width = 0.2, height = 0)+ggtitle(paste(colnames(power)[4]))
grid.arrange(plot1, plot2, ncol=1)


require(gridExtra)
plot1 <- qplot((dsgn[, 5][diff_theta==0]),    nullpower[, 5], col = as.factor(dsgn[, 2][diff_theta==0]), shape=as.factor(dsgn[, 6][diff_theta==0]), ylim=c(0,0.5))+geom_abline(slope=0,intercept=alpha)+  geom_jitter(width = 0.2, height = 0)+ggtitle(paste(colnames(power)[5]))
plot2 <- qplot((dsgn[, 5][diff_theta==0]),    nullpower[, 6], col = as.factor(dsgn[, 2][diff_theta==0]), shape=as.factor(dsgn[, 6][diff_theta==0]), ylim=c(0,0.5))+geom_abline(slope=0,intercept=alpha)+  geom_jitter(width = 0.2, height = 0)+ggtitle(paste(colnames(power)[6]))
grid.arrange(plot1, plot2, ncol=1)
