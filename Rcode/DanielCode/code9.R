


######################################################################

# Standard method fails under NULL, g-comp provides unbiased result:
testing(1000, a_=0, a0_a1=0, l1_a1=0, a0_l1=logit(0.5-0.305), u_=(2), h_=2.5)


standard_pval<-g_pval<-iv_pval<-rep(0,100)
for(ii in 1:100){
tt1<-testing(1000, a_=0, a0_a1=0, l1_a1=0, a0_l1=logit(0.5-0.305), u_=(2), h_=2.5)
standard_pval[ii]<-tt1$standard_pval
g_pval[ii]<-tt1$g_pval
iv_pval[ii]<-tt1$iv_pval
}
powerlev(standard_pval)  	# 0.334 0.450  	# biased AWAY from the NULL
powerlev(g_pval) 			# 0.027 0.071	# substantially conservative
powerlev(iv_pval)			# 0.063 0.107	# somewhat liberal



######################################################################

# Does power make a differnece if effect is direct versus indirect?
# both cases have total effect of 0.078.  One is entirely direct: A->Y
# while the other is entirely indirect: A->L1->Y

# All direct effect  ( a0_y, a1_y):
testing(100, a_=logit(0.5+0.048), a0_a1=0, l1_a1=0, a0_l1=logit(0.5-0.305), u_=2, h_=2.5, l1_y=0)$true_effect

# All indirect effect  even transition
testing(100, a_=NA, a0_a1=0, l1_a1=0, a0_l1=logit(0.5-0.305), u_=2, h_=2.5, a0_y = 0, a1_y =  0, l1_y=logit(0.5-0.305))$true_effect

# All indirect effect uneven transition
testing(100, a_=NA, a0_a1=0, l1_a1=0, a0_l1=2*logit(0.5-0.305), u_=2, h_=2.5, a0_y = 0, a1_y =  0, l1_y=0.51*logit(0.5-0.31))$true_effect

# All indirect effect uneven transition
testing(100, a_=NA, a0_a1=0, l1_a1=0, a0_l1=0.50*logit(0.5-0.305), u_=2, h_=2.5, a0_y = 0, a1_y =  0, l1_y=2.1*logit(0.5-0.305))$true_effect



my_rel_power<-function(nSim=50, nn=200, type="NULL"){
g_pval_vec<-iv_pval_vec<-g_onepval_vec<-iv_onepval_vec <-standard_pval_vec<-rep(0,nSim)
for(ii in 1:nSim){

if(type=="NULL"){
	t1<-testing(nn, a_=0, a0_a1=0, l1_a1=0, a0_l1=logit(0.5-0.305), u_=(2), h_=2.5)}

if(type=="direct"){
t1<-testing(nn, a_=logit(0.5+0.048), a0_a1=0, l1_a1=0, a0_l1=logit(0.5-0.305), u_=2, h_=2.5, l1_y=0)
}

if(type=="indirect"){
t1<-testing(nn, a_=NA, a0_a1=0, l1_a1=0, a0_l1=logit(0.5-0.305), u_=2, h_=2.5, a0_y = 0, a1_y =  0, l1_y=logit(0.5-0.305))}


if(type=="indirect2"){
t1<-testing(nn, a_=NA, a0_a1=0, l1_a1=0, a0_l1=2*logit(0.5-0.305), u_=2, h_=2.5, a0_y = 0, a1_y =  0, l1_y=0.51*logit(0.5-0.31))
}

if(type=="indirect3"){
t1<-testing(nn, a_=NA, a0_a1=0, l1_a1=0, a0_l1=0.50*logit(0.5-0.305), u_=2, h_=2.5, a0_y = 0, a1_y =  0, l1_y=2.1*logit(0.5-0.305))
}



print(round(ii/nSim,3))
	standard_pval_vec[ii]<-t1$standard_pval
	g_pval_vec[ii]<-t1$g_pval
	iv_pval_vec[ii]<-t1$iv_pval
	g_onepval_vec[ii]<-t1$g_effect_onesidepval
	iv_onepval_vec[ii]<-t1$iv_onesidepval
}

return(
list(
standard_pval=powerlev(standard_pval_vec),
g_pval=powerlev(g_pval_vec),
iv_pval=powerlev(iv_pval_vec),
g_pval1=powerlev(g_onepval_vec),
iv_pval1=powerlev(iv_onepval_vec)
))
}


p1<-my_rel_power(1000,300,"NULL")  # g-comp appears conservative (two-sided case in particular)
p2<-my_rel_power(1000,300,"direct") # g-comp has 0.370 power (iv: 0.074)
p3<-my_rel_power(1000,300,"indirect") # g-comp has 0.351 power (iv: 0.064)
p4<-my_rel_power(1000,300,"indirect2") # 
p5<-my_rel_power(1000,300,"indirect3") # 


gcomp300<-c(p1$g_pval[2],p2$g_pval[2],p3$g_pval[2],p4$g_pval[2],p5$g_pval[2])
iv300<-c(p1$iv_pval[2],p2$iv_pval[2],p3$iv_pval[2],p4$iv_pval[2],p5$iv_pval[2])

plot(0,xlim=c(1,5),ylim=c(0,1))
lines(gcomp300)
lines(iv300, col="blue")



p1<-my_rel_power(1000,1000,"NULL")  # g-comp appears conservative (two-sided case in particular)
p2<-my_rel_power(1000,1000,"direct") # g-comp has 0.370 power (iv: 0.074)
p3<-my_rel_power(1000,1000,"indirect") # g-comp has 0.351 power (iv: 0.064)
p4<-my_rel_power(1000,1000,"indirect2") # 
p5<-my_rel_power(1000,1000,"indirect3") # 


gcomp1000<-c(p1$g_pval[2],p2$g_pval[2],p3$g_pval[2],p4$g_pval[2],p5$g_pval[2])
iv1000<-c(p1$iv_pval[2],p2$iv_pval[2],p3$iv_pval[2],p4$iv_pval[2],p5$iv_pval[2])

plot(0,xlim=c(1,5),ylim=c(0,1))
lines(gcomp1000)
lines(iv1000, col="blue")


# Appears to be comparable! 


rel_power_ind<-function(nSim=50, nn=200){
for(ii in 1: nSim){

# All indirect effect
t1<-testing(nn, a_=NA, a0_a1=0, l1_a1=0, a0_l1=logit(0.5+0.28822), u_=0, h_=0, a0_y = 0, a1_y =  0, l1_y=logit(0.5+0.28822), interconstant=FALSE)

	g_pval_vec[ii]<-t1$g_pval
	iv_pval_vec[ii]<-t1$iv_pval
	g_onepval_vec[ii]<-t1$g_effect_onesidepval
	iv_onepval_vec[ii]<-t1$iv_onesidepval

}
return(
list(
g_pval=quantile(g_pval_vec[1:ii], c(0.05,0.10)),
iv_pval=quantile(iv_pval_vec[1:ii], c(0.05,0.10)),
g_onepval=quantile(g_onepval_vec[1:ii], c(0.05,0.10)),
iv_onepval=quantile(iv_onepval_vec[1:ii], c(0.05,0.10))))
}



# all indirect effect
testing(500, a_=NA, a0_a1=0, l1_a1=0, a0_l1=logit(0.5+0.288), u_=0, h_=0, a0_y = 0, a1_y =  0, l1_y=logit(0.5+0.288), interconstant=FALSE)


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
