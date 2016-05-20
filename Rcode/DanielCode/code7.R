# It is realistic to assume that clinicians are more likely to prescribe 
# treatment(A0) to patients who are more ill(L0).  

invlogit<-function(x){exp(x)/(1+exp(x))}

testing<-function(n=100, a_=0, a0_a1=0.5, l1_a1=0.5, a0_l1=0, u_=0, h_=0, a0_y= a_, a1_y=a_){


h_a0<-h_
h_a1<-h_



u0_l1<-u_
u0_y<-u_



g_h <- invlogit(0)
H <- rbinom(n,1,g_h)		# 50/50
round(mean(H),2)== g_h

g_u0<- invlogit(0)
U0 <- rbinom(n,1, g_u0)		# 50/50
round(mean(U0),2)== g_u0

inter_a0<-(-(h_a0*g_h))
g_a0 <- invlogit(inter_a0+(h_a0*H))
A0 <- rbinom(n,1,g_a0)  # 50/50
summary(lm(g_a0~H))$coef[2,4]
round(mean(A0),2)== round(mean(g_a0),2)

inter_l1<-(-mean(a0_l1*g_a0 +u0_l1*g_u0))
g_l1<-invlogit(inter_l1 + (a0_l1*A0 + u0_l1*U0) )
L1<-rbinom(n,1,g_l1)		
summary(lm(g_l1~A0+U0))$coef[2:3,4]
round(mean(L1),2)== round(mean(g_l1),2)

inter_a1<-(-mean(h_a1*g_h+a0_a1*g_a0+l1_a1*g_l1))
g_a1<-invlogit(inter_a1 + (h_a1*H+a0_a1*A0+l1_a1*L1))
A1<-rbinom(n,1,g_a1)
summary(lm(g_a1~H+A0+L1))$coef[2:4,4]
round(mean(A1),2)== round(mean(g_a1),2)

inter_y<-(-mean(a0_y*g_a0 + a1_y*g_a1+ u0_y*g_u0))
g_y<-invlogit(inter_y+(a0_y*A0 + a1_y*A1+ u0_y*U0))
Y<-rbinom(n,1,g_y)
summary(lm(g_y~A0+A1+U0))$coef[2:4,4]
round(mean(Y),2)== round(mean(g_y),2)

#print(c(mean(A1),mean(L1),mean(Y)))
#print(cor(cbind(H,A0))[1,2])
#print(cor(cbind(H,A1))[1,2])
simdata<-data.frame(H,U0,A0,L1,A1,Y)
simdata_obs<-data.frame(A0,L1,A1,Y)
print(paste("Sparse?", sum(c(unlist(table(simdata_obs)))<3)))

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
invlogit(inter_y+(a0_y*a0 + a1_y*a1+ u0_y*u0))

		sum2<-sum2+sum1
	}
}
return(sum2)}

# The true joint causal effect of A on Y:
true_effect<-true_theta_function(1,1)-true_theta_function(0,0)
true_effect

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
	
####################################################################### 
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
####################################################################### 
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
g_effect_onesidepval	= g_effect$onesidepval,
iv_onesidepval			= iv_onesidepval
))}
##################################################################
##################################################################
##################################################################


# Standard method fails under NULL, g-comp provides unbiased result:
testing(1500, a_=0, a0_a1=0, l1_a1=0, a0_l1=(-2), u_=(2), h_=0)


# All direct effect  (a0_a1, a0_y, a1_y):
testing(1, a_=0, a0_a1=0.7, l1_a1=0, a0_l1=0, u_=(2), h_=0.5, a0_y = 0.31, a1_y = 0.2)

# All indirect effect ( a0_l1, l1_a1, a1_y):
testing(1, a_=0, a0_a1=0, l1_a1=0.5, a0_l1=0.5, u_=(2), h_=0.5, a0_y = 0, a1_y = 0.2)


#indirect to direct ratio:
(a0_l1 + l1_a1)/(a0_a1+ a0_y)



# Type 1 error in situations when standard method has 0 power:
rel_power<-function(nSim=50, nn=200){
	g_pval_vec<-iv_pval_vec<-g_onepval_vec<-iv_onepval_vec <-rep(0,nSim)
	for(ii in 1:nSim){
		t1<-testing(nn, a_=0, a0_a1=0, l1_a1=0, a0_l1=(-2), u_=(2), h_=0)
		g_pval_vec[ii]<-t1$g_pval
		iv_pval_vec[ii]<-t1$iv_pval

		g_onepval_vec[ii]<-t1$g_effect_onesidepval
		iv_onepval_vec[ii]<-t1$iv_onesidepval


		print(ii)
		print(quantile(g_pval_vec[1:ii], c(0.05,0.10)))
		print(quantile(iv_pval_vec[1:ii], c(0.05,0.10)))
		print("one-sided")
		print(quantile(g_onepval_vec[1:ii], c(0.05,0.10)))
		print(quantile(iv_onepval_vec[1:ii], c(0.05,0.10)))}


return(
list(
g_pval=quantile(g_pval_vec[1:ii], c(0.05,0.10)),
iv_pval=quantile(iv_pval_vec[1:ii], c(0.05,0.10)),
g_onepval=quantile(g_onepval_vec[1:ii], c(0.05,0.10)),
iv_onepval=quantile(iv_onepval_vec[1:ii], c(0.05,0.10))))
}


r1<-rel_power(1000,50)
r2<-rel_power(1000,200)
r3<-rel_power(1000,1000)
r4<-rel_power(1000,10000)
r5<-rel_power(1000,50000)


t1<-testing(nn, a_=0, a0_a1=0, l1_a1=0, a0_l1=(-2), u_=(2), h_=0)






plot(c(r1$g_pval[1],r2$g_pval[1],r3$g_pval[1],r4$g_pval[1]), type='l', ylim=c(0,0.5))
lines(c(r1$g_pval[2],r2$g_pval[2],r3$g_pval[2],r4$g_pval[2]), type='l', ylim=c(0,0.5))

# compare the relative power of IV and g-comp methods:

rel_power<-function(h_, nSim=50){
g_pval_vec<-iv_pval_vec<-rep(0,nSim)
for(ii in 1:nSim){
t1<-testing(300, 5, 0, -5, 5,-1, h_)
print(mean(g_effect_vec[1:(ii-1)]))
g_pval_vec[ii]<-t1$g_effect$pval
iv_pval_vec[ii]<-t1$iv_pval
}
return(c(mean(g_pval_vec<0.1), mean(iv_pval_vec<0.1)))}

r1<-rel_power(-1)
r2<-rel_power(-3)
r3<-rel_power(-5)
r4<-rel_power(-10)

testing(5000, 5, 0, -5, 5)


# Standard method fails, g-comp provides unbiased result:
t1<-testing(1000, 5, 0, -5, 5, -1)


bias_n<-function(n, nSim=200){
g_effect_vec<-rep(0,nSim)
for(ii in 1:nSim){
t1<-testing(n, 5, 0, -5, 5, 0)
print(mean(g_effect_vec[1:(ii-1)]))
g_effect_vec[ii]<-t1$g_effect$effect}
return(mean(g_effect_vec[1:(ii-1)]))}

b1<-bias_n(100)
b2<-bias_n(500)
b3<-bias_n(1000)
b4<-bias_n(5000)

data.frame(n=c(100,500,1000,5000),est=c(b1,b2,b3,b4),bias=c(c(b1,b2,b3,b4)-0.156))
