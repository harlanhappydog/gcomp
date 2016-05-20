alpha0<-0
n<-100000
invlogit<-function(x){exp(x)/(1+exp(x))}

L0<-rbinom(n,1,invlogit(1))
A0<-rbinom(n,1,invlogit(0.5+0.5*L0))
L1<-rbinom(n,1,invlogit(0.5+0.5*L0+alpha0*A0))
A1<-rbinom(n,1,invlogit(0.5+0.5*A0+0.5*L1))


Y<-rbinom(n,1,invlogit(0.5+0.5*L0+0.5*L1+0.5*A0+0.5*A1))

glmmod<-glm(Y~A0+A1+L0+L1, family="binomial")
summary(glmmod)
round(glmmod$coef,2)