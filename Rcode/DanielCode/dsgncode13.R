source("~/functions1.R")
# Design of experiment
nSim<-1000
lvls <- list(NA)
seqlev<-c(0, 0.5, -0.25) # levels for  interesting parameters

lvls[[1]] <- seqlev
lvls[[2]] <- seqlev
lvls[[3]] <- seqlev 
lvls[[4]] <- seqlev
lvls[[5]] <- seqlev
lvls[[6]] <- seqlev
lvls[[7]] <- seqlev
lvls[[8]] <- seqlev

### factorial design
dsgn <- as.matrix(expand.grid(lvls[[1]], lvls[[2]], lvls[[3]], lvls[[4]], lvls[[5]], lvls[[6]], lvls[[7]], lvls[[8]]))
dim(dsgn)
dim(dsgn)[1]



g_pval_vec<-iv_pval_vec<-g_onepval_vec<-iv_onepval_vec <-true_effect <-list()
results<-list()


for(i in 500:dim(dsgn)[1]){
g_pval_vec[[i]]<-iv_pval_vec[[i]]<-g_onepval_vec[[i]]<-iv_onepval_vec[[i]]<-true_effect[[i]]<-rep(0,nSim)

for(ii in 1:nSim){

t1<-testing(300, a_=NA, a0_a1=dsgn[i, 1], l1_a1=dsgn[i, 2], a0_l1=dsgn[i, 3], u_=dsgn[i, 4], h_=dsgn[i, 5], a0_y = dsgn[i, 6], a1_y = dsgn[i, 7], l1_y=dsgn[i, 8])

print(round(ii/nSim,3))
	true_effect[[i]][ii]<-t1$true_effect
	g_pval_vec[[i]][ii]<-t1$g_pval
	iv_pval_vec[[i]][ii]<-t1$iv_pval
	g_onepval_vec[[i]][ii]<-t1$g_effect_onesidepval
	iv_onepval_vec[[i]][ii]<-t1$iv_onesidepval
}

results[[i]]<-list(
true_eff=unique(round(true_effect[[i]],3)),
g_pval=powerlev(g_pval_vec[[i]]),
iv_pval=powerlev(iv_pval_vec[[i]]),
g_pval1=powerlev(g_onepval_vec[[i]]),
iv_pval1=powerlev(iv_onepval_vec[[i]])
)
print(results[[i]])
print("done1")
print(i)
}

