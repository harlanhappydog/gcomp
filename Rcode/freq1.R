###############################################################
# FREQUENTIST APPROACH WITH Y1 WITHOUT H

 C1prob<-function(c1){
pp<-predict(glm(C1~1, family="binomial"), newdata=data.frame(X="ZZ"),type="response")
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
  for (x1i in 0:1) {
    for (y1i in 0:1) {
      for (c2i in 0:1) {
            for (c1i in 0:1) {
Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="C1" & Fgammas_Y1_wH$C1== c1i,]$gamma<-C1probB(c1i)	
Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="Y1" & Fgammas_Y1_wH$Y1== y1i  & Fgammas_Y1_wH$C1== c1i  & Fgammas_Y1_wH$X1== x1i,]$gamma<-Y1probB(y1i, c1i, x1i)
Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="C2" & Fgammas_Y1_wH$C2== c2i  & Fgammas_Y1_wH$C1== c1i  & Fgammas_Y1_wH$X1== x1i,]$gamma<-C2probB(c2i, c1i, x1i)
Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="Y2" & Fgammas_Y1_wH$Y2== y2i  & Fgammas_Y1_wH$C2== c2i  & Fgammas_Y1_wH$X2== x2i  & Fgammas_Y1_wH$Y1== y1i,]$gamma<-Y2probB(y2i, c2i, x2i, y1i)
}}}}}


        x1i<-1
        x2i<-1
        y2i<-1
        sumtheta11<-0
# Integrate over the values of C1 and C2: Prob(Y2=1 | X1=1 and X2=1)
sumtheta11<-0
for(c2i in 0:1){
for(y1i in 0:1){
for(c1i in 0:1){
sumtheta11<-sumtheta11+Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="C1" & Fgammas_Y1_wH$C1== c1i,]$gamma*
Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="Y1" & Fgammas_Y1_wH$Y1== y1i  & Fgammas_Y1_wH$C1== c1i  & Fgammas_Y1_wH$X1== x1i,]$gamma*
Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="C2" & Fgammas_Y1_wH$C2== c2i  & Fgammas_Y1_wH$C1== c1i  & Fgammas_Y1_wH$X1== x1i,]$gamma*
Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="Y2" & Fgammas_Y1_wH$Y2== y2i  & Fgammas_Y1_wH$C2== c2i  & Fgammas_Y1_wH$X2== x2i  & Fgammas_Y1_wH$Y1== y1i,]$gamma
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
sumtheta00<-sumtheta00 +Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="C1" & Fgammas_Y1_wH$C1== c1i,]$gamma*
Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="Y1" & Fgammas_Y1_wH$Y1== y1i  & Fgammas_Y1_wH$C1== c1i  & Fgammas_Y1_wH$X1== x1i,]$gamma*
Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="C2" & Fgammas_Y1_wH$C2== c2i  & Fgammas_Y1_wH$C1== c1i  & Fgammas_Y1_wH$X1== x1i,]$gamma*
Fgammas_Y1_wH[Fgammas_Y1_wH$cond=="Y2" & Fgammas_Y1_wH$Y2== y2i  & Fgammas_Y1_wH$C2== c2i  & Fgammas_Y1_wH$X2== x2i  & Fgammas_Y1_wH$Y1== y1i,]$gamma}
}}

sumtheta00
sumtheta11

Bpval<-1-mean((sumtheta00Bvec-sumtheta11Bvec)>0)
