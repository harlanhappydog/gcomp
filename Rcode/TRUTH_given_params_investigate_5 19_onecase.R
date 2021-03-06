
######################################################################## Design of experiment
lvls <- list(NA)

fixlev<-c(0,0.5)  # levels for not interesting parameters
seqlev<-c(1, 2, 3) # levels for  interesting parameters

lvls[[1]] <- c(0)  # violation
lvls[[2]] <- seqlev
lvls[[3]] <- c(0) 
lvls[[4]] <- 0.5
lvls[[5]] <- seqlev
lvls[[6]] <- 0
lvls[[7]] <- c(0) # violation
lvls[[8]] <- c(0)
lvls[[9]] <- c(0)
lvls[[10]] <- c(0)
lvls[[11]] <- seqlev

interceptc<-(0)  # intercept for C1 and C2
intercepty<-(-1)   # intercept for Y1 and Y2
intercepth<-(-1)  #intercept for H
interceptz<-0.5
interceptx<-0.5

### factorial design
dsgn <- as.matrix(expand.grid(lvls[[1]], lvls[[2]], lvls[[3]], lvls[[4]], lvls[[5]], lvls[[6]], lvls[[7]], lvls[[8]], lvls[[9]], lvls[[10]], lvls[[11]]))
dim(dsgn)
dim(dsgn)[1]

# only null
#dsgn<-dsgn[dsgn[,5]==0|dsgn[,6]==0,]
diff_theta<-rep(0,dim(dsgn)[1])
######################################################################## 
### CALCULATE TRUE VALUES OF diff
matt <- matrix(0, dim(dsgn)[1], 2)
true_gammas<-list()
for (i in c(1:dim(dsgn)[1])) {
truetheta11 <- 0
kk <- 1
individual <- matrix(0, 8, 8)

true_gammas[[i]]<-data.frame(expand.grid(c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(1), c("H","C1","C2","Y1","Y2"), c(NA)))
colnames(true_gammas[[i]])<-c("H","C1","C2","X1","X2","Y1","Y2", "cond", "gamma")

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
true_gammas[[i]][true_gammas[[i]]$cond=="C1" & true_gammas[[i]]$C1== c1i,]$gamma<-aa

                # P(Y1=1 or =0|C1,X1)
                py1<-1/(1+exp(-(intercepty + dsgn[i, 5]*c1i + dsgn[i, 3]*myX1 + dsgn[i, 8]*myH)))
                bb <- as.numeric(y1i == 1) * (py1) + as.numeric(y1i == 0) * (1 - py1)
true_gammas[[i]][true_gammas[[i]]$cond=="Y1" & true_gammas[[i]]$Y1== y1i & true_gammas[[i]]$X1==myX1 & true_gammas[[i]]$C1==c1i,]$gamma<-bb

                # P(C2=1 or =0|C1,X1)
                pc2<-(1/(1+exp(-(interceptc + dsgn[i, 4]*c1i + dsgn[i, 6]*myX1))))
                cc <- as.numeric(c2i == 1) * (pc2)  + as.numeric(c2i == 0)*(1-pc2)
true_gammas[[i]][true_gammas[[i]]$cond=="C2" & true_gammas[[i]]$C1== c1i & true_gammas[[i]]$C2== c2i & true_gammas[[i]]$X1== myX1,]$gamma<-cc

                # P(Y2=1|C2,X2,Y1)
                dd <- (as.numeric(y1i == 1) * 1 + as.numeric(y1i == 0) * ( (1/(1+exp(-(intercepty+dsgn[i, 9]*myZ + dsgn[i, 5]*c2i + dsgn[i, 3]*myX2 + dsgn[i, 8]*myH)))) ))                
true_gammas[[i]][true_gammas[[i]]$cond=="Y2" & true_gammas[[i]]$Y2== 1  & true_gammas[[i]]$C2== c2i & true_gammas[[i]]$X2== myX2 & true_gammas[[i]]$Y1== y1i,]$gamma<-dd
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
true_gammas[[i]][true_gammas[[i]]$cond=="C1" & true_gammas[[i]]$C1== c1i,]$gamma<-aa

                # P(Y1=1 or =0|C1,X1)
                py1<-1/(1+exp(-(intercepty + dsgn[i, 5]*c1i + dsgn[i, 3]*myX1 + dsgn[i, 8]*myH)))
                bb <- as.numeric(y1i == 1) * (py1) + as.numeric(y1i == 0) * (1 - py1)
true_gammas[[i]][true_gammas[[i]]$cond=="Y1" & true_gammas[[i]]$Y1== y1i & true_gammas[[i]]$X1==myX1 & true_gammas[[i]]$C1==c1i,]$gamma<-bb

                # P(C2=1 or =0|C1,X1)
                pc2<-(1/(1+exp(-(interceptc + dsgn[i, 4]*c1i + dsgn[i, 6]*myX1))))
                cc <- as.numeric(c2i == 1) * (pc2)  + as.numeric(c2i == 0)*(1-pc2)
true_gammas[[i]][true_gammas[[i]]$cond=="C2" & true_gammas[[i]]$C1== c1i & true_gammas[[i]]$C2== c2i & true_gammas[[i]]$X1== myX1,]$gamma<-cc
                
                # P(Y2=1|C2,X2,Y1)
                dd <- (as.numeric(y1i == 1) * 1 + as.numeric(y1i == 0) * ( (1/(1+exp(-(intercepty+dsgn[i, 9]*myZ + dsgn[i, 5]*c2i + dsgn[i, 3]*myX2 + dsgn[i, 8]*myH)))) ))                
true_gammas[[i]][true_gammas[[i]]$cond=="Y2" & true_gammas[[i]]$Y2== 1  & true_gammas[[i]]$C2== c2i & true_gammas[[i]]$X2== myX2 & true_gammas[[i]]$Y1== y1i,]$gamma<-dd
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

# For H marginals
for (h1i in 0:1) {
       # P(H=1 or =0)
                ph1<-1/(1+exp(-(intercepth )))
                hh <- (as.numeric(h1i == 1) * (ph1) + as.numeric(h1i == 0) * (1-ph1))
true_gammas[[i]][true_gammas[[i]]$cond=="H" & true_gammas[[i]]$H== h1i,]$gamma<-hh
}
}

diff_theta<-round(matt[,2]-matt[,1],5)


for(i in 1:dim(dsgn)[1]) {print(dim(true_gammas[[i]][true_gammas[[i]]$gamma>0.95 | true_gammas[[i]]$gamma<0.05,])[1]-32)
	
if((dim(true_gammas[[i]][true_gammas[[i]]$gamma>0.95 | true_gammas[[i]]$gamma<0.05,])[1]-32)>0){print(dsgn[i,])}
	}

diff_theta
######################################################################## 
# SIMULATIONS STUDY
######################################################################## 
ppp <- 1
est <- matrix(0, dim(dsgn)[1], 2)
kkk <- 1
result <- matrix(0, dim(dsgn)[1], 2)
numsim <- 100  ### number of datasets generated under each scenario
Bayesresult <- matrix(0, dim(dsgn)[1] * numsim, 8)
n <- 10000  ### size of each dataset
#set.seed(13)  ### for reproducibility
seedlist <- sample(1:250000, size = numsim)

######################################################################## 
simdatasum<-matrix(0,dim(dsgn)[1],8)
colnames(simdatasum)<-c( "H",     "Z",    "C1",    "X1",    "C2",    "Y1",    "X2",    "Y2") 

my_Fgammas_wY1_wH<-list()
my_Fgammas_wY1_H<-list()
my_Fgammas_Y1_wH<-list()
my_Fgammas_Y1_H<-list()

my_gammas_wY1_wH<-list()
my_gammas_wY1_H<-list()
my_gammas_Y1_wH<-list()
my_gammas_Y1_H<-list()

for (i in c(1:1)) {
#for (i in c(1:dim(dsgn)[1])) {
my_gammas_wY1_wH[[i]]<-matrix(0, dim(true_gammas[[1]])[1],numsim)
my_gammas_wY1_H[[i]]<-matrix(0, dim(true_gammas[[1]])[1],numsim)
my_gammas_Y1_wH[[i]]<-matrix(0, dim(true_gammas[[1]])[1],numsim)
my_gammas_Y1_H[[i]]<-matrix(0, dim(true_gammas[[1]])[1],numsim)

my_Fgammas_wY1_wH[[i]]<-matrix(0, dim(true_gammas[[1]])[1],numsim)
my_Fgammas_wY1_H[[i]]<-matrix(0, dim(true_gammas[[1]])[1],numsim)
my_Fgammas_Y1_wH[[i]]<-matrix(0, dim(true_gammas[[1]])[1],numsim)
my_Fgammas_Y1_H[[i]]<-matrix(0, dim(true_gammas[[1]])[1],numsim)

	for (j in 1:numsim) {
    ### loop over datasets
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
 #       Y2[Y1 == 0]<-rbinom(length(Y2[Y1 == 0]), 1, (1/(1+exp(-(intercepty + dsgn[i, 9]*Z + dsgn[i, 5]*C2 + dsgn[i, 3]*X2 + dsgn[i, 8]*H)))) )     

        Y2<-rbinom(n, 1, (1/(1+exp(-(intercepty + dsgn[i, 9]*Z + dsgn[i, 5]*C2 + dsgn[i, 3]*X2 + dsgn[i, 8]*H)))) )     
        Y2[Y1 == 1]<-1
 
        simdata <- data.frame(H,Z,C1,X1,C2,Y1,X2,Y2)
		simdatasum[i,]<-colMeans(simdata)
        print(colMeans(simdata))
      # ################################################################
# ## 
print("BAYESIAN APPROACH WITH Y1, WITHOUT H (uniform priors):")
nB<-100
sumtheta11Bvec<-rep(0,nB)
sumtheta00Bvec<-rep(0,nB)

gammas_Y1_wH<-data.frame(expand.grid(c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(1), c("H","C1","C2","Y1","Y2"), c(NA)))
colnames(gammas_Y1_wH)<-c("H","C1","C2","X1","X2","Y1","Y2", "cond", "gamma")

gammas<-matrix(0,dim(gammas_Y1_wH)[1], nB)

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
		
for(sw in 1:nB){

y2i<-1
 for (x2i in 0:1) {
x1i<-x2i
    for (y1i in 0:1) {
      for (c2i in 0:1) {
            for (c1i in 0:1) {
gammas_Y1_wH[gammas_Y1_wH$cond=="C1" & gammas_Y1_wH$C1== c1i,]$gamma<-C1probB(c1i)	
gammas_Y1_wH[gammas_Y1_wH$cond=="Y1" & gammas_Y1_wH$Y1== y1i  & gammas_Y1_wH$C1== c1i  & gammas_Y1_wH$X1== x1i,]$gamma<-Y1probB(y1i, c1i, x1i)
gammas_Y1_wH[gammas_Y1_wH$cond=="C2" & gammas_Y1_wH$C2== c2i  & gammas_Y1_wH$C1== c1i  & gammas_Y1_wH$X1== x1i,]$gamma<-C2probB(c2i, c1i, x1i)
gammas_Y1_wH[gammas_Y1_wH$cond=="Y2" & gammas_Y1_wH$Y2== y2i  & gammas_Y1_wH$C2== c2i  & gammas_Y1_wH$X2== x2i  & gammas_Y1_wH$Y1== y1i,]$gamma<-Y2probB(y2i, c2i, x2i, y1i)
}}}}


sumtheta11B<-0
sumtheta00B<-0
        x1i<-1
        x2i<-1
        y2i<-1
for(c2i in 0:1){
for(y1i in 0:1){
for(c1i in 0:1){
sumtheta11B<-sumtheta11B+
unique(gammas_Y1_wH[gammas_Y1_wH$cond=="C1" & gammas_Y1_wH$C1== c1i,]$gamma*
gammas_Y1_wH[gammas_Y1_wH$cond=="Y1" & gammas_Y1_wH$Y1== y1i  & gammas_Y1_wH$C1== c1i  & gammas_Y1_wH$X1== x1i,]$gamma*
gammas_Y1_wH[gammas_Y1_wH$cond=="C2" & gammas_Y1_wH$C2== c2i  & gammas_Y1_wH$C1== c1i  & gammas_Y1_wH$X1== x1i,]$gamma*
gammas_Y1_wH[gammas_Y1_wH$cond=="Y2" & gammas_Y1_wH$Y2== y2i  & gammas_Y1_wH$C2== c2i  & gammas_Y1_wH$X2== x2i  & gammas_Y1_wH$Y1== y1i,]$gamma)
}}}

        x1i<-0
        x2i<-0
        y2i<-1
for(c2i in 0:1){
for(y1i in 0:1){
for(c1i in 0:1){
sumtheta00B<-sumtheta00B+
unique(gammas_Y1_wH[gammas_Y1_wH$cond=="C1" & gammas_Y1_wH$C1== c1i,]$gamma*
gammas_Y1_wH[gammas_Y1_wH$cond=="Y1" & gammas_Y1_wH$Y1== y1i  & gammas_Y1_wH$C1== c1i  & gammas_Y1_wH$X1== x1i,]$gamma*
gammas_Y1_wH[gammas_Y1_wH$cond=="C2" & gammas_Y1_wH$C2== c2i  & gammas_Y1_wH$C1== c1i  & gammas_Y1_wH$X1== x1i,]$gamma*
gammas_Y1_wH[gammas_Y1_wH$cond=="Y2" & gammas_Y1_wH$Y2== y2i  & gammas_Y1_wH$C2== c2i  & gammas_Y1_wH$X2== x2i  & gammas_Y1_wH$Y1== y1i,]$gamma)
}}}

sumtheta11Bvec[sw]<-sumtheta11B
sumtheta00Bvec[sw]<-sumtheta00B
gammas[,sw]<-gammas_Y1_wH$gamma
}
	   my_gammas_Y1_wH[[i]][,j]<-rowMeans(gammas[,1:nB])   
        sumtheta00Bvec_Y1_wH <- sumtheta00Bvec
        sumtheta11Bvec_Y1_wH <- sumtheta11Bvec

Bpval_Y1_wH <- 1 - mean((sumtheta11Bvec_Y1_wH - sumtheta00Bvec_Y1_wH) > 0)
########################################


##############################################################

# print('FREQUENTIST APPROACH WITH Y1 WITHOUT H')
Fgammas_Y1_wH<-data.frame(expand.grid(c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(1), c("H","C1","C2","Y1","Y2"), c(NA)))
colnames(Fgammas_Y1_wH)<-c("H","C1","C2","X1","X2","Y1","Y2", "cond", "gamma")

 C1prob<-function(c1){
pp<-predict(glm(C1~1, family="binomial",data=simdata), newdata=data.frame(X="ZZ"),type="response")
if(c1==0){return(1-as.numeric(pp))}
if(c1==1){return(as.numeric(pp))}}

Y1prob<-function(y1,c1,x1){
pp<-predict(glm(Y1~1,data=simdata[simdata$X1==x1 & simdata$C1==c1, ], family="binomial"), newdata=data.frame(X="ZZ"),type="response")
if(y1==0){return(1-as.numeric(pp))}
if(y1==1){return(as.numeric(pp))}}

C2prob<-function(c2,c1,x1){
pp<-predict(glm(C2~1,data=simdata[simdata$C1==c1 & simdata$X1==x1, ], family="binomial"), newdata=data.frame(X="ZZ"),type="response")
if(c2==0){return(1-as.numeric(pp))}
if(c2==1){return(as.numeric(pp))}}

Y2prob<-function(y2,c2,x2,y1){
pp<-predict(glm(Y2~1,data=simdata[simdata$C2==c2 & simdata$X2==x2 & simdata$Y1==y1, ], family="binomial"), newdata=data.frame(X="ZZ"),type="response")
if(y2==0){return(1-as.numeric(pp))}
if(y2==1){return(as.numeric(pp))}}

################################################################
# Since these are all binary, we can compute theta00 and theta11 explicitly by summation

y2i<-1
 for (x2i in 0:1) {
x1i<-x2i
    for (y1i in 0:1) {
      for (c2i in 0:1) {
            for (c1i in 0:1) {
Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="C1" & Fgammas_Y1_wH$C1== c1i,]$gamma<-C1prob(c1i)	
Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="Y1" & Fgammas_Y1_wH$Y1== y1i  & Fgammas_Y1_wH$C1== c1i  & Fgammas_Y1_wH$X1== x1i,]$gamma<-Y1prob(y1i, c1i, x1i)
Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="C2" & Fgammas_Y1_wH$C2== c2i  & Fgammas_Y1_wH$C1== c1i  & Fgammas_Y1_wH$X1== x1i,]$gamma<-C2prob(c2i, c1i, x1i)
Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="Y2" & Fgammas_Y1_wH$Y2== y2i  & Fgammas_Y1_wH$C2== c2i  & Fgammas_Y1_wH$X2== x2i  & Fgammas_Y1_wH$Y1== y1i,]$gamma<-Y2prob(y2i, c2i, x2i, y1i)
}}}}


        x1i<-1
        x2i<-1
        y2i<-1
        sumtheta11<-0
# Integrate over the values of C1 and C2: Prob(Y2=1 | X1=1 and X2=1)
sumtheta11<-0
for(c2i in 0:1){
for(y1i in 0:1){
for(c1i in 0:1){
sumtheta11<-sumtheta11+unique(Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="C1" & Fgammas_Y1_wH$C1== c1i,]$gamma*
Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="Y1" & Fgammas_Y1_wH$Y1== y1i  & Fgammas_Y1_wH$C1== c1i  & Fgammas_Y1_wH$X1== x1i,]$gamma*
Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="C2" & Fgammas_Y1_wH$C2== c2i  & Fgammas_Y1_wH$C1== c1i  & Fgammas_Y1_wH$X1== x1i,]$gamma*
Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="Y2" & Fgammas_Y1_wH$Y2== y2i  & Fgammas_Y1_wH$C2== c2i  & Fgammas_Y1_wH$X2== x2i  & Fgammas_Y1_wH$Y1== y1i,]$gamma)
}
}}
        x1i<-0
        x2i<-0
        y2i<-1
# Integrate over the values of C1 and C2: Prob(Y2=1 | X1=0 and X2=0)
sumtheta00<-0
for(c2i in 0:1){
for(y1i in 0:1){
for(c1i in 0:1){
sumtheta00<-sumtheta00 +unique(Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="C1" & Fgammas_Y1_wH$C1== c1i,]$gamma*
Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="Y1" & Fgammas_Y1_wH$Y1== y1i  & Fgammas_Y1_wH$C1== c1i  & Fgammas_Y1_wH$X1== x1i,]$gamma*
Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="C2" & Fgammas_Y1_wH$C2== c2i  & Fgammas_Y1_wH$C1== c1i  & Fgammas_Y1_wH$X1== x1i,]$gamma*
Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="Y2" & Fgammas_Y1_wH$Y2== y2i  & Fgammas_Y1_wH$C2== c2i  & Fgammas_Y1_wH$X2== x2i  & Fgammas_Y1_wH$Y1== y1i,]$gamma)}
}}

print(c(mean(sumtheta00Bvec_Y1_wH ), sumtheta00))
print(c(mean(sumtheta11Bvec_Y1_wH ), sumtheta11))
 my_Fgammas_Y1_wH[[i]][,j]<-Fgammas_Y1_wH[,9]  





#########################################
    Hpval <- (1-pnorm((summary(glm(Y2 ~ H, data = simdata, family = "binomial"))$coefficients[2, 3])))
#one-sided p-values:
        Bpval_wY1_wH <- 1 - mean((sumtheta11Bvec_wY1_wH - sumtheta00Bvec_wY1_wH) > 0)
         Bpval_Y1_wH <- 1 - mean((sumtheta11Bvec_Y1_wH - sumtheta00Bvec_Y1_wH) > 0)
         Bpval_wY1_H <- 1 - mean((sumtheta11Bvec_wY1_H - sumtheta00Bvec_wY1_H) > 0)
         Bpval_Y1_H <- 1 - mean((sumtheta11Bvec_Y1_H - sumtheta00Bvec_Y1_H) > 0)
         Hpval <- (1-pnorm((summary(glm(Y2 ~ H, data = simdata, family = "binomial"))$coefficients[2, 3])))
         Bayesresult[kkk, ] <- c(i, j, diff_theta[i], Bpval_Y1_H, Bpval_wY1_H, Bpval_Y1_wH, Bpval_wY1_wH, Hpval)
         print(Bayesresult[kkk, ])
         kkk <- kkk + 1
    }
  
   	   true_gammas[[i]]$simgamma_Y1_wH<-rowMeans(my_gammas_Y1_wH[[i]][,1:j])   
   	   true_gammas[[i]]$Fsimgamma_Y1_wH<-rowMeans(my_Fgammas_Y1_wH[[i]][,1:j])   
   	   
	   true_gammas[[i]]$simgamma_wY1_H<-rowMeans(my_gammas_wY1_H[[i]][,1:j])   
	   true_gammas[[i]]$Fsimgamma_wY1_H<-rowMeans(my_Fgammas_wY1_H[[i]][,1:j])   
	   	   
	   true_gammas[[i]]$simgamma_Y1_H<-rowMeans(my_gammas_Y1_H[[i]][,1:j])   
	   true_gammas[[i]]$Fsimgamma_Y1_H<-rowMeans(my_Fgammas_Y1_H[[i]][,1:j])   	   
}

######################################################################## 
par(mfrow=c(2,2))
hist(Bayesresult[1:75,4])
hist(Bayesresult[1:75,5])
hist(Bayesresult[1:75,6])
hist(Bayesresult[1:75,7])

options(digits=2)
na.omit(subset(true_gammas[[1]][,-c(11:13)], !duplicated(true_gammas[[1]][,-c(11:13)][,10])))
na.omit(subset(true_gammas[[1]][,-c(10,12,13)], !duplicated(true_gammas[[1]][,-c(10,12,13)][,10])))
na.omit(subset(true_gammas[[1]][,-c(10,11,13)], !duplicated(true_gammas[[1]][,-c(10,11,13)][,10])))
na.omit(subset(true_gammas[[1]][,-c(10,11,12)], !duplicated(true_gammas[[1]][,-c(10,11,12)][,10])))

plot(na.omit(true_gammas[[1]][,9]-true_gammas[[1]][,10:13][,1]))
plot(na.omit(true_gammas[[1]][,9]-true_gammas[[1]][,10:13][,2]))
plot(na.omit(true_gammas[[1]][,9]-true_gammas[[1]][,10:13][,3]))
plot(na.omit(true_gammas[[1]][,9]-true_gammas[[1]][,10:13][,4]))
mean(na.omit(cbind(true_gammas[[1]][,c(1:9,10)],true_gammas[[1]][,9]-true_gammas[[1]][,10:13][,1]))[,11]>0.2)
na.omit(cbind(true_gammas[[1]][,c(1:9,11)],true_gammas[[1]][,9]-true_gammas[[1]][,10:13][,2]))
na.omit(cbind(true_gammas[[1]][,c(1:9,12)],true_gammas[[1]][,9]-true_gammas[[1]][,10:13][,3]))
na.omit(cbind(true_gammas[[1]][,c(1:9,13)],true_gammas[[1]][,9]-true_gammas[[1]][,10:13][,4]))

colnames(Bayesresult)<-c("i","j","diff_theta","with Y1 with H","without Y1 with H","with Y1 without H","without Y1 without H", "IV")
alpha<-0.1
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

head(power)


i<-3
mean(na.omit(cbind(true_gammas[[i]][,c(1:9,10)],true_gammas[[i]][,9]-true_gammas[[i]][,10:13][,1]))[,11]>0.2)


