# It is realistic to assume that clinicians are more likely to prescribe 
# treatment(A0) to patients who are more ill(L0).  

invlogit<-function(x){exp(x)/(1+exp(x))}
logit<-function(x){log(x/(1-x))}
powerlev<-function(x){c(mean(x<0.05), mean(x<0.10))}

testing<-function(n=100, 
a_		=	0,
a0_a1	=	3, 
l1_a1	=	-3, 
a0_l1	=	0, 
u_		=	0, 
h_		=	4, 
a0_y		= 	a_, 
a1_y		=	a_, 
l1_y		=	0,
interconstant=FALSE){


justeffect<-FALSE
if(n==1){justeffect<-TRUE}

h_a0<-h_
h_a1<-h_



u0_l1<-u_
u0_y<-u_



g_h <- invlogit(0)
H <- rbinom(n,1,g_h)		# 50/50

g_u0<- invlogit(0)
U0 <- rbinom(n,1, g_u0)		# 50/50

inter_a0<-(-(h_a0*g_h))-2
if(interconstant){inter_a0<-(-0.5)}
g_a0 <- invlogit(inter_a0+(h_a0*H))
A0 <- rbinom(n,1,g_a0)  # 50/50

inter_l1<-(-mean(a0_l1*g_a0 +u0_l1*g_u0))
if(interconstant){inter_l1<-(-0.75)}
g_l1<-invlogit(inter_l1 + (a0_l1*A0 + u0_l1*U0) )
L1<-rbinom(n,1,g_l1)		

inter_a1<-(-mean(h_a1*g_h+a0_a1*g_a0+l1_a1*g_l1))
if(interconstant){inter_a1<-(-0.95)}
g_a1<-invlogit(inter_a1 + (h_a1*H+a0_a1*A0+l1_a1*L1))
A1<-rbinom(n,1,g_a1)

inter_y<-(-mean(a0_y*g_a0 + a1_y*g_a1+ u0_y*g_u0 +l1_y*g_l1))
if(interconstant){inter_y<-(-1)}
g_y<-invlogit(inter_y+(a0_y*A0 + a1_y*A1+ u0_y*U0+l1_y*L1))
Y<-rbinom(n,1,g_y)
summary(lm(g_y~A0+ A1+ U0+ L1))

#print("A1|(L1=0 & A0=0)   A1|(L1=1 & A0=0) for H=1")
#print(round(c(mean(A1[L1==0 & A0==0 & H==1]),mean(A1[L1==1 & A0==0 & H==1])),3))

#print(" A1|A0=1  for H=1")
#print(round(c(mean(A1[L1==0 & A0==1 & H==1]),mean(A1[L1==1 & A0==1 & H==1])),3))


#print("A0|H=0  A0|H=1")
#print(round(c(mean(A0[H==0]),mean(A0[H==1])),3))

marginals<-(rbind(round(c(mean(A1[L1==0 & A0==0 & H==1]),mean(A1[L1==1 & A0==0 & H==1])),3),
round(c(mean(A1[L1==0 & A0==1 & H==1]),mean(A1[L1==1 & A0==1 & H==1])),3),
round(c(mean(A0[H==0]),mean(A0[H==1])),3)))

#print(marginals)

simdata<-data.frame(H,U0,A0,L1,A1,Y)
simdata_obs<-data.frame(A0,L1,A1,Y)
if(sum(c(unlist(table(simdata_obs)))<3)>0){print(paste("Sparse?", sum(c(unlist(table(simdata_obs)))<3)))}

# Calculate the true treatment effect:
# theta(a0,a1) = Pr(Y|A0=a0,A1=a1,U0,L1)*Pr(L1|A0=a0,U0)*Pr(U0)

true_theta_function<-function(a0,a1){
sum2<-0
for(u0 in c(0,1)){
	for(l1 in c(0,1)){
		sum1<-
((u0*g_u0) +(1-u0)*(1-g_u0))*

(
(l1*invlogit(inter_l1 + (a0_l1*a0 + u0_l1*u0) )) +
((1-l1)*(1-invlogit(inter_l1 + (a0_l1*a0 + u0_l1*u0)))) 
)*
invlogit(inter_y+(a0_y*a0 + a1_y*a1+ u0_y*u0 + l1_y*l1))

		sum2<-sum2+sum1
	}
}
return(sum2)}

# The true joint causal effect of A on Y:
true_effect<-round(true_theta_function(1,1)-true_theta_function(0,0),5)
true_effect

if(justeffect){return(true_effect)}
if(!justeffect){
#######################################################################
# Functions
####################################################################### 
makebeta<-function(veci, i, nm){
            ss <- sum(veci)
            nn <- length(veci)
            pp <- (rbeta(nm, (ss + 1), (nn - ss + 1)))
            if (i %in% 0) {return(1 - as.numeric(pp))}
            if (i %in% 1) {return(as.numeric(pp))}}

  

L1probB <- function(l1=c(1), a0=c(0,1), nm = 1) {
            veci<-simdata$L1[simdata$A0 %in% a0]
            makebeta(veci,l1, nm)}
            
YprobB <- function(h=c(0,1), y=c(1), l1=c(0,1), a0=c(0,1), a1=c(0,1), nm = 1) {            
veci<-simdata$Y[simdata$A0%in%a0 & simdata$A1%in%a1 & simdata$L1%in%l1]
                  makebeta(veci,y, nm)}    
	
################################################################## 
theta_function<-function(a0,a1){
 sum2<-0
	for(l1 in c(0,1)){
		sum1<-L1probB(l1=l1, a0=a0)*YprobB(a0=a0, a1=a1,l1=l1)
		sum2<-sum2+sum1}
	return(sum2)}

# The estimated joint causal effect of A on Y, by non-parametric g-computation:
gcomp<-function(nB=500){
	effect<-apply(cbind(1:nB),1,function(x){theta_function(1,1)-theta_function(0,0)})
	pval<-1 - mean((effect) > 0)
	twopval<-2*min(c((1 - mean((effect) > 0)), (1 - mean((effect) < 0))))
	return(list(effect=mean(effect), onesidepval=pval, twosidepval=twopval))}
g_effect<-gcomp()	
g_effect
##################################################################
library(lmtest)
# The standard estimated effect is dilluted:
glmmod1<-glm(Y~A0+A1+L1, family="binomial")
#print(summary(glmmod1))
glmmod0<-glm(Y~L1, family="binomial")
standard_pval<-lrtest(glmmod1, glmmod0)$"Pr(>Chisq)"[2]

# The total causal effect of A0 on Y and of A1 on Y could be estimated using standard adjustment:
glmmod<-glm(Y~A0, family="binomial")

standard_A0_onesidepval<-(1-pnorm(coef(summary(glmmod))[2,3]))
standard_A1_onesidepval<-(1-pnorm(coef(summary(glmmod1))[3,3]))

standard_A0_pval<-coef(summary(glmmod))[2,c(4)]
standard_A1_pval<-coef(summary(glmmod1))[3,c(4)]



####################################################################### 
# The IV estimated effect is dilluted:
glmmod<-glm(Y~H, family="binomial")
iv_onesidepval<-(1-pnorm(coef(summary(glmmod))[2,3]))
iv_pval<-coef(summary(glmmod))[2,4]
####################################################################### 


return(list(
true_effect				= true_effect, 
g_effect					= g_effect$effect,

standard_pval 			= standard_pval, 
standard_A0_pval			= standard_A0_pval,  
standard_A1_pval			= standard_A1_pval,
g_pval					= g_effect$twosidepval,
iv_pval					= iv_pval, 

standard_A0_onesidepval = standard_A0_onesidepval,
standard_A1_onesidepval = standard_A1_onesidepval,
g_effect_onesidepval		= g_effect$onesidepval,
iv_onesidepval			= iv_onesidepval,
marginals				= marginals
))}}
##################################################################
##################################################################
##################################################################

