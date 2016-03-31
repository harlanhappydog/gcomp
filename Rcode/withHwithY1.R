n <- 10000
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

###############################################################
# ## 
print("BAYESIAN APPROACH WITH Y1 WITH H (uniform priors):")

nB<-100
sumtheta11Bvec<-rep(0,nB)
sumtheta00Bvec<-rep(0,nB)

gammas_Y1_H<-data.frame(expand.grid(c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(1), c("H","C1","C2","Y1","Y2"), c(NA)))
colnames(gammas_Y1_H)<-c("H","C1","C2","X1","X2","Y1","Y2", "cond", "gamma")

gammas<-matrix(0,dim(gammas_Y1_H)[1], nB)

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
			
						            
for(sw in 1:nB){
	

y2i<-1
 for (x2i in 0:1) {
 for (x1i in 0:1) {
    for (y1i in 0:1) {
    for (h1i in 0:1) {
      for (c2i in 0:1) {
            for (c1i in 0:1) {
gammas_Y1_H[gammas_Y1_H$cond=="H" & gammas_Y1_H$H== h1i,]$gamma<-HprobB(h1i)	          
gammas_Y1_H[gammas_Y1_H$cond=="C1" & gammas_Y1_H$C1== c1i & gammas_Y1_H$H== h1i,]$gamma<-C1probB(h1i ,c1i)	
 gammas_Y1_H[gammas_Y1_H$cond=="Y1" & gammas_Y1_H$H== h1i & gammas_Y1_H$Y1== y1i  & gammas_Y1_H$C1== c1i  & gammas_Y1_H$X1== x1i,]$gamma<-Y1probB(h1i, y1i, c1i, x1i)
gammas_Y1_H[gammas_Y1_H$cond=="C2" & gammas_Y1_H$H== h1i & gammas_Y1_H$C2== c2i  & gammas_Y1_H$C1== c1i  & gammas_Y1_H$X1== x1i,]$gamma<-C2probB(h1i, c2i, c1i, x1i)          
 gammas_Y1_H[gammas_Y1_H$cond=="Y2" & gammas_Y1_H$H== h1i  & gammas_Y1_H$Y2== y2i & gammas_Y1_H$C2== c2i   & gammas_Y1_H$X2== x2i  & gammas_Y1_H$Y1== y1i,]$gamma<-Y2probB(h1i, y2i, c2i, x2i, y1i)       
 }}}}}}

sumtheta11B<-0
    x1i<-1
        x2i<-1
        y2i<-1
for(c2i in 0:1){
for(y1i in 0:1){
for(c1i in 0:1){
	for (h1i in 0:1) {
               
 sumtheta11B<-sumtheta11B+
unique(gammas_Y1_H[gammas_Y1_H$cond=="H" & gammas_Y1_H$H== h1i,]$gamma*
gammas_Y1_H[gammas_Y1_H$cond=="C1" & gammas_Y1_H$C1== c1i & gammas_Y1_H$H== h1i,]$gamma*
gammas_Y1_H[gammas_Y1_H$cond=="C2" & gammas_Y1_H$H== h1i & gammas_Y1_H$C2== c2i  & gammas_Y1_H$C1== c1i  & gammas_Y1_H$X1== x1i,]$gamma*
gammas_Y1_H[gammas_Y1_H$cond=="Y1" & gammas_Y1_H$H== h1i & gammas_Y1_H$Y1== y1i  & gammas_Y1_H$C1== c1i  & gammas_Y1_H$X1== x1i,]$gamma*
 gammas_Y1_H[gammas_Y1_H$cond=="Y2" & gammas_Y1_H$H== h1i  & gammas_Y1_H$Y2== y2i & gammas_Y1_H$C2== c2i   & gammas_Y1_H$X2== x2i  & gammas_Y1_H$Y1== y1i,]$gamma)
}}}}
sumtheta00B<-0
    x1i<-0
        x2i<0
        y2i<-1
for(c2i in 0:1){
for(y1i in 0:1){
for(c1i in 0:1){
	for (h1i in 0:1) {
        
 sumtheta00B<-sumtheta00B+
unique(gammas_Y1_H[gammas_Y1_H$cond=="H" & gammas_Y1_H$H== h1i,]$gamma*
gammas_Y1_H[gammas_Y1_H$cond=="C1" & gammas_Y1_H$H== h1i  & gammas_Y1_H$C1== c1i ,]$gamma*
gammas_Y1_H[gammas_Y1_H$cond=="C2"& gammas_Y1_H$H== h1i  & gammas_Y1_H$C2== c2i  & gammas_Y1_H$C1== c1i  & gammas_Y1_H$X1== x1i,]$gamma*
gammas_Y1_H[gammas_Y1_H$cond=="Y1" & gammas_Y1_H$H== h1i & gammas_Y1_H$Y1== y1i  & gammas_Y1_H$C1== c1i  & gammas_Y1_H$X1== x1i,]$gamma*
 gammas_Y1_H[gammas_Y1_H$cond=="Y2" & gammas_Y1_H$H== h1i & gammas_Y1_H$Y2== y2i    & gammas_Y1_H$C2== c2i  & gammas_Y1_H$X2== x2i  & gammas_Y1_H$Y1== y1i,]$gamma)
}}}}
             
         
sumtheta11Bvec[sw]<-sumtheta11B
sumtheta00Bvec[sw]<-sumtheta00B
gammas[,sw]<-gammas_Y1_H$gamma
}


#	   my_gammas_Y1_H[[i]][,j]<-rowMeans(gammas[,1:nB])   
        sumtheta00Bvec_Y1_H <- sumtheta00Bvec
        sumtheta11Bvec_Y1_H <- sumtheta11Bvec




# 
print('FREQUENTIST APPROACH WITH Y1 WITH H')
Fgammas_Y1_H<-data.frame(expand.grid(c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(1), c("H","C1","C2","Y1","Y2"), c(NA)))
colnames(Fgammas_Y1_H)<-c("H","C1","C2","X1","X2","Y1","Y2", "cond", "gamma")


Hprob<-function(h1){
pp<-predict(glm(H~1, family="binomial",data=simdata), newdata=data.frame(X="ZZ"),type="response")
if(h1==0){return(1-as.numeric(pp))}
if(h1==1){return(as.numeric(pp))}}

 C1prob<-function(h1, c1){
pp<-predict(glm(C1~1, family="binomial",data=simdata[simdata$H==h1,]), newdata=data.frame(X="ZZ"),type="response")
if(c1==0){return(1-as.numeric(pp))}
if(c1==1){return(as.numeric(pp))}}

Y1prob<-function(h, y1,c1,x1,nm=1){
pp<-predict(glm(Y1~1, family="binomial",data=simdata[simdata$H == h & simdata$X1==x1 & simdata$C1==c1,]), newdata=data.frame(X="ZZ"),type="response")
if(y1==0){return(1-as.numeric(pp))}
if(y1==1){return(as.numeric(pp))}}

C2prob<-function(h1, c2, c1,x1){
pp<-predict(glm(C2~1,data=simdata[simdata$C1==c1 & simdata$X1==x1 & simdata$H==h1, ], family="binomial"), newdata=data.frame(X="ZZ"),type="response")
if(c2==0){return(1-as.numeric(pp))}
if(c2==1){return(as.numeric(pp))}}

Y2prob<-function(h1,y2,c2,x2, y1){
pp<-predict(glm(Y2~1,data=simdata[simdata$H == h1 & simdata$C2==c2 & simdata$X2==x2 & simdata$Y1==y1,], family="binomial"), newdata=data.frame(X="ZZ"),type="response")
if(y2==0){return(1-as.numeric(pp))}
if(y2==1){return(as.numeric(pp))}}

################################################################
# Since these are all binary, we can compute theta00 and theta11 explicitly by summation

y2i<-1
 for (x2i in 0:1) {
x1i<-x2i 
      for (c2i in 0:1) {
      	for(y1i in 0:1){
     	 for (h1i in 0:1) {
            for (c1i in 0:1) {
Fgammas_Y1_H[Fgammas_Y1_H$cond=="H" & Fgammas_Y1_H$H== h1i,]$gamma<-Hprob(h1i)	
Fgammas_Y1_H[Fgammas_Y1_H$cond=="C1" & Fgammas_Y1_H$C1== c1i & Fgammas_Y1_H$H== h1i,]$gamma<-C1prob(h1i ,c1i)	
 Fgammas_Y1_H[Fgammas_Y1_H$cond=="Y1" & Fgammas_Y1_H$H== h1i & Fgammas_Y1_H$Y1== y1i  & Fgammas_Y1_H$C1== c1i  & Fgammas_Y1_H$X1== x1i,]$gamma<-Y1prob(h1i, y1i, c1i, x1i)
Fgammas_Y1_H[Fgammas_Y1_H$cond=="C2" & Fgammas_Y1_H$H== h1i  & Fgammas_Y1_H$C2== c2i  & Fgammas_Y1_H$C1== c1i  & Fgammas_Y1_H$X1== x1i,]$gamma<-C2prob(h1i, c2i, c1i, x1i)
 Fgammas_Y1_H[Fgammas_Y1_H$cond=="Y2"& gammas_Y1_H$H== h1i  & Fgammas_Y1_H$Y2== y2i  & Fgammas_Y1_H$C2== c2i  & Fgammas_Y1_H$X2== x2i  & Fgammas_Y1_H$Y1== y1i,]$gamma<-Y2prob(h1i, y2i, c2i, x2i, y1i)  
}}}}}

        sumtheta11<-0
		sumtheta00<-0
        x1i<-1
        x2i<-1
        y2i<-1

for(c2i in 0:1){
for(y1i in 0:1){
for(h1i in 0:1){
for(c1i in 0:1){
sumtheta11<-sumtheta11+unique(
Fgammas_Y1_H[Fgammas_Y1_H$cond=="H" & Fgammas_Y1_H$H== h1i,]$gamma*
Fgammas_Y1_H[Fgammas_Y1_H$cond=="C1" & Fgammas_Y1_H$H== h1i  & Fgammas_Y1_H$C1== c1i ,]$gamma*
Fgammas_Y1_H[Fgammas_Y1_H$cond=="C2"& Fgammas_Y1_H$H== h1i  & Fgammas_Y1_H$C2== c2i  & Fgammas_Y1_H$C1== c1i  & Fgammas_Y1_H$X1== x1i,]$gamma*
Fgammas_Y1_H[Fgammas_Y1_H$cond=="Y1" & Fgammas_Y1_H$H== h1i & Fgammas_Y1_H$Y1== y1i  & Fgammas_Y1_H$C1== c1i  & Fgammas_Y1_H$X1== x1i,]$gamma*
 Fgammas_Y1_H[Fgammas_Y1_H$cond=="Y2" & gammas_Y1_H$H== h1i & Fgammas_Y1_H$Y2== y2i    & Fgammas_Y1_H$C2== c2i & Fgammas_Y1_H$X2== x2i  & Fgammas_Y1_H$Y1== y1i,]$gamma)}}}}
        x1i<-0
        x2i<-0
        y2i<-1
# Integrate over the values of C1 and C2: Prob(Y2=1 | X1=0 and X2=0)
sumtheta00<-0
for(c2i in 0:1){
for(y1i in 0:1){
for(h1i in 0:1){
for(c1i in 0:1){

sumtheta00<-sumtheta00 +
unique(Fgammas_Y1_H[Fgammas_Y1_H$cond=="H" & Fgammas_Y1_H$H== h1i,]$gamma*
Fgammas_Y1_H[Fgammas_Y1_H$cond=="C1" & Fgammas_Y1_H$H== h1i  & Fgammas_Y1_H$C1== c1i ,]$gamma*
Fgammas_Y1_H[Fgammas_Y1_H$cond=="C2"& Fgammas_Y1_H$H== h1i  & Fgammas_Y1_H$C2== c2i  & Fgammas_Y1_H$C1== c1i  & Fgammas_Y1_H$X1== x1i,]$gamma*
Fgammas_Y1_H[Fgammas_Y1_H$cond=="Y1" & Fgammas_Y1_H$H== h1i & Fgammas_Y1_H$Y1== y1i  & Fgammas_Y1_H$C1== c1i  & Fgammas_Y1_H$X1== x1i,]$gamma*
 Fgammas_Y1_H[Fgammas_Y1_H$cond=="Y2" & gammas_Y1_H$H== h1i & Fgammas_Y1_H$Y2== y2i    & Fgammas_Y1_H$C2== c2i  & Fgammas_Y1_H$X2== x2i  & Fgammas_Y1_H$Y1== y1i,]$gamma)}}}}
 sumtheta00
sumtheta11
print(c(mean(sumtheta00Bvec_Y1_H ), sumtheta00))
print(c(mean(sumtheta11Bvec_Y1_H ), sumtheta11))


AA<-cbind(Fgammas_Y1_H,rowMeans(gammas[,1:nB])  )
AA