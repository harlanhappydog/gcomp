# It is realistic to assume that clinicians are more likely to prescribe 
# treatment(A0) to patients who are more ill(L0).  

alpha0<-(5)
a_0<-0.0

l_0<-(-0.75)

testing<-function(threeparms){

alpha0<-(-1)
a_0<-threeparms[1]
l_0<-threeparms[2]

a_1<-a_0
l_1<-threeparms[3]

n<-500000
invlogit<-function(x){exp(x)/(1+exp(x))}

L0<-rbinom(n,1,invlogit(1))
A0<-rbinom(n,1,invlogit(0.5+0.5*L0))
L1<-rbinom(n,1,invlogit(0.5+0.5*L0+alpha0*A0))
A1<-rbinom(n,1,invlogit(0.5+0.5*A0+0.5*L1))
Y<-rbinom(n,1,invlogit(0.5+l_0*L0+l_1*L1+a_0*A0+a_1*A1))


# Calculate the true treatment effect:
# theta(a0,a1) = P(Y|A0=a0,A1=a1,L0,L1)*P(L1|A0=a0,L0)*P(L0)


theta_function<-function(a0,a1){
sum2<-0
for(l0 in c(0,1)){
	for(l1 in c(0,1)){
		sum1<-mean(L0==l0)*mean(L1[L0==l0 & A0==a0]==l1)*
		mean(Y[A0==a0 & A1==a1 & L0==l0 & L1==l1])
		sum2<-sum2+sum1
	}
}
return(sum2)}

# The joint causal effect of A on Y:
effect<-theta_function(1,1)-theta_function(0,0)


# The estimated effect is dilluted: 
glmmod<-glm(Y~A0+A1+L0+L1, family="binomial")
effect2<-10000*abs(effect) +12
e1<-0
if(sum(coef(summary(glmmod))[2:3,4]<0.01)>0){
	e1<-1
	effect2<-10000*abs(effect)-abs(sum(coef(summary(glmmod))[2:3,1]))
}

if(abs(round(effect,4))<0.01 & e1==1){
print(paste("zero",round(effect,4)))
print(coef(summary(glmmod))[2:3,c(1,4)])
print(threeparms)
}
return(round(effect2,4))}

testing(c(0,1,2))
nlminb(c(0,0.1,0),testing)


