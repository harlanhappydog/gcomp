# It is realistic to assume that clinicians are more likely to prescribe 
# treatment(A0) to patients who are more ill(L0).  


testing<-function(n, alpha0, a_, l_0, l_1, h_){

a_1<-a_
a_0<-a_

invlogit<-function(x){exp(x)/(1+exp(x))}

H<-rbinom(n,1,invlogit(1))
L0<-rbinom(n,1,invlogit(1))
A0<-rbinom(n,1,invlogit(0.5+h_*H+0.5*L0))
L1<-rbinom(n,1,invlogit(0.5+0.5*L0+alpha0*A0))
A1<-rbinom(n,1,invlogit(0.5+h_*H+0.5*A0+0.5*L1))
Y<-rbinom(n,1,invlogit(0.5+l_0*L0+l_1*L1+a_0*A0+a_1*A1))
print(c(mean(A1),mean(L1),mean(Y)))
print(cor(cbind(H,A0))[1,2])
print(cor(cbind(H,A1))[1,2])
simdata<-data.frame(H,L0,A0,L1,A1,Y)
simdata_H<-data.frame(L0,A0,L1,A1,Y)
print("Sparse?")
print(sum(c(unlist(table(simdata_H)))<3))

# Calculate the true treatment effect:
# theta(a0,a1) = Pr(Y|A0=a0,A1=a1,L0,L1)*Pr(L1|A0=a0,L0)*Pr(L0)

true_theta_function<-function(a0,a1){
sum2<-0
for(l0 in c(0,1)){
	for(l1 in c(0,1)){
		sum1<-
(l0*(invlogit(1)) +(1-l0)*(1-invlogit(1)))*
(l1*(invlogit(0.5+0.5*l0+alpha0*a0)) +(1-l1)*(1-invlogit(0.5+0.5*l0+alpha0*a0)))*
invlogit(0.5+l_0*l0+l_1*l1+a_0*a0+a_1*a1)
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

L0probB <- function(h=c(0,1), l0=1, nm = 1) {
			veci<-simdata$L0[simdata$H %in% h]
			makebeta(veci,l0, nm)}    

L1probB <- function(h=c(0,1), l1=c(1), l0=c(0,1), a0=c(0,1), nm = 1) {
            veci<-simdata$L1[simdata$H %in% h & simdata$L0 %in% l0 & simdata$A0 %in% a0]
            makebeta(veci,l1, nm)}
            
YprobB <- function(h=c(0,1), y=c(1), l1=c(0,1), l0=c(0,1), a0=c(0,1), a1=c(0,1), nm = 1) {            
veci<-simdata$Y[simdata$A0%in%a0 & simdata$A1%in%a1 & simdata$L0%in%l0 & simdata$L1%in%l1]
                  makebeta(veci,y, nm)}    
	
####################################################################### 
theta_function<-function(a0,a1){
sum2<-0
for(l0 in c(0,1)){
	for(l1 in c(0,1)){
		sum1<-L0probB(l0=l0) * 
				L1probB(l1=l1, l0=l0, a0=a0) * 
					YprobB(a0=a0, a1=a1, l0=l0, l1=l1)
		sum2<-sum2+sum1}}
return(sum2)}

# The estimated joint causal effect of A on Y, by non-parametric g-computation:
gcomp<-function(nB=100){
	effect<-apply(cbind(1:nB),1,function(x){theta_function(1,1)-theta_function(0,0)})
	pval<-2*min(c((1 - mean((effect) > 0)), (1 - mean((effect) < 0))))
	return(list(effect=mean(effect), pval=pval))}
g_effect<-gcomp()	
g_effect
####################################################################### 
# The standard estimated effect is dilluted:
glmmod1<-glm(Y~A0+A1+L0+L1, family="binomial")
glmmod0<-glm(Y~L0+L1, family="binomial")
standard_pval<-lrtest(glmmod1, glmmod0)$"Pr(>Chisq)"[2]


# The total causal effect of A0 on Y and of A1 on Y could be estimated using standard adjustment:
glmmod<-glm(Y~A0+L0, family="binomial")
standard_A0_pval<-coef(summary(glmmod))[2,c(4)]
standard_A1_pval<-coef(summary(glmmod1))[3,c(4)]
####################################################################### 
# The IV estimated effect is dilluted:
glmmod<-glm(Y~H, family="binomial")
iv_pval<-coef(summary(glmmod))[2,4]

return(list(true_effect= true_effect, g_effect= g_effect, iv_pval= iv_pval, standard_pval = standard_pval, standard_A0_pval,  standard_A1_pval))}
####################################################################### 
####################################################################### 
####################################################################### 


# Power in situations when standard method has 0 power (ie all effect is driven through L):

# Standard method fails, g-comp provides unbiased result, IV method?  (true effect is 0.156):
testing(1500, 5, 0, -5, 5, -2)



# Type 1 error in situations when standard method has 0 power:

# Standard method fails, g-comp provides unbiased result  (true effect is 0.156):
testing(1500, 5, 0, 0, 0, -2)


rel_power<-function(nSim=50){
g_pval_vec<-iv_pval_vec<-rep(0,nSim)
for(ii in 1:nSim){
t1<-testing(1500, 5, 0, 0, 0, -2)
print(mean(g_effect_vec[1:(ii-1)]))
g_pval_vec[ii]<-t1$g_effect$pval
iv_pval_vec[ii]<-t1$iv_pval
}
print(quantile(g_pval_vec))
print(quantile(iv_pval_vec))

return(c(mean(g_pval_vec<0.1), mean(iv_pval_vec<0.1)))}

r1<-rel_power(500)




# compare the relative power of IV and g-comp methods:

rel_power<-function(h_, nSim=50){
g_pval_vec<-iv_pval_vec<-rep(0,nSim)
for(ii in 1:nSim){
t1<-testing(300, 5, 0, -5, 5, h_)
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
