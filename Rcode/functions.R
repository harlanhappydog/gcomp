C2probB <- function(h=c(0,1), c2=c(1), c1=c(0,1), x1=c(0,1), nm = 1) {
            ss <- sum(simdata$C2[simdata$H %in% h & simdata$C1 %in% c1 & simdata$X1 %in% x1])
            nn <- length(simdata$C2[simdata$H %in% h & simdata$C1 %in% c1 & simdata$X1 %in% x1])
            pp <- (rbeta(nm, (ss + 1), (nn - ss + 1)))
            if (c2 %in% 0) {return(1 - as.numeric(pp))}
            if (c2 %in% 1) {return(as.numeric(pp))}}	
            
Y2probB<-function(h=c(0,1), y2=c(1),c2=c(0,1),x2=c(0,1),y1=c(0,1), c1=c(0,1), x1=c(0,1), nm=1){
			ss<-sum(simdata$Y2[simdata$H %in%  h & simdata$C2%in% c2 & simdata$X2%in% x2 & simdata$Y1%in% y1 & simdata$C1%in% c1 & simdata$X1%in% x1])
			nn<-length(simdata$Y2[simdata$H %in% h & simdata$C2%in% c2 & simdata$X2%in% x2 & simdata$Y1%in% y1  & simdata$C1%in% c1 & simdata$X1%in% x1])
			pp<-(rbeta(nm,(ss+1),(nn-ss+1)))
			if(y2%in%0){return(1-as.numeric(pp))}
			if(y2%in%1){return(as.numeric(pp))}}
			
			
Y1probB<-function(h=c(0,1), y1=1,c1=c(0,1),x1=c(0,1),nm=1){
			ss<-sum(simdata$Y1[simdata$H %in% h & simdata$X1%in%x1 & simdata$C1%in%c1])
			nn<-length(simdata$Y1[simdata$H %in% h & simdata$X1%in%x1 & simdata$C1%in%c1])
			pp<-(rbeta(nm,(ss+1),(nn-ss+1)))
			if(y1%in%0){return(1-as.numeric(pp))}
			if(y1%in%1){return(as.numeric(pp))}}
			
			
C1probB <- function(h=c(0,1), c1=1, nm = 1) {
            ss <- sum(simdata$C1[simdata$H %in% h])
            nn <- length(simdata$C1[simdata$H %in% h])
            pp <- (rbeta(nm, (ss + 1), (nn - ss + 1)))
            if (c1 %in% 0) {return(1 - as.numeric(pp))}
            if (c1 %in% 1) {return(as.numeric(pp))}}            
            
            
HprobB <- function(h=1, nm = 1) {
            ss <- sum(simdata$H)
            nn <- length(simdata$H)
            pp <- (rbeta(nm, (ss + 1), (nn - ss + 1)))
            if (h == 0) {return(1 - as.numeric(pp))}
            if (h == 1) {return(as.numeric(pp))}}