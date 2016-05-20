



my_rel_power<-function(nSim=50, nn=200, type="NULL"){
g_pval_vec<-iv_pval_vec<-g_onepval_vec<-iv_onepval_vec <-standard_pval_vec<-rep(0,nSim)
for(ii in 1:nSim){

if(type=="NULL"){
	t1<-testing(nn,0)}

if(type=="direct1"){
t1<-testing(nn, 0.1, h_=1, l1_a1=-2)
print(t1$marginals)
}

if(type=="direct2"){
t1<-testing(nn, 0.1, h_=2, l1_a1=-1.5)
print(t1$marginals)
}

if(type=="direct3"){
t1<-testing(nn, 0.1, h_=3)
print(t1$marginals)
}

if(type=="direct4"){
t1<-testing(nn, 0.1, h_=4)
print(t1$marginals)
}

if(type=="direct5"){
t1<-testing(nn, 0.1, h_=5)
print(t1$marginals)
}

if(type=="direct6"){
t1<-testing(nn, 0.1, h_=6)
print(t1$marginals)
}

if(type=="direct7"){
t1<-testing(nn, 0.1, h_=7)
print(t1$marginals)
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

my_rel_power(200, 500, "NULL")

d1<-my_rel_power(1000, 500, "direct1")
d2<-my_rel_power(1000, 500, "direct2")
d3<-my_rel_power(1000, 500, "direct3")
d4<-my_rel_power(1000, 500, "direct4")
d5<-my_rel_power(1000, 500, "direct5")
d6<-my_rel_power(1000, 500, "direct6")
d7<-my_rel_power(1000, 500, "direct7")


