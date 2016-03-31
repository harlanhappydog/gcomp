follow<-read.csv("~/Desktop/EMMES-Projects/HeadStart/FollowUp/Headstartfollowup _A1c_DDS.csv")


head(follow)

## Formatting PATID
follow$PATID<-follow$X


## Formatting a treatment variable
for(i in c(1:dim(follow)[1])){follow$treat[i]<-strsplit(as.character(follow$PATID),"-")[i][[1]][3]}
# numeric variable:
follow$treat_n<-as.numeric(as.factor(follow$treat))-1
data.frame(follow$PATID,follow$treat)




## Formating A1C
a1c_adm<-as.numeric(as.character(follow[,2]))
a1c_dis<-as.numeric(as.character(follow[,3]))
a1c_fol<-as.numeric(as.character(follow[,5]))
a1c_strike<-as.numeric(as.character(follow[,4]))

## Formatting DDS
qvec<-seq(6,56,3)
DDS_adm_colnames<-c(colnames(follow)[qvec])
DDS_adm<-data.frame(rowMeans(as.matrix(follow[, qvec]),na.rm=TRUE))

qvec<-seq(7,56,3)
DDS_dis_colnames<-c(colnames(follow)[qvec])
DDS_dis<-data.frame(rowMeans(as.matrix(follow[, qvec]),na.rm=TRUE))

qvec<-seq(8,56,3)
DDS_fol_colnames<-c(colnames(follow)[qvec])
DDS_fol<-data.frame(rowMeans(as.matrix(follow[, qvec]),na.rm=TRUE))


PRIME_DAT<-data.frame(follow$PATID, follow$treat, follow$treat_n, a1c_adm, a1c_dis,  a1c_fol,  a1c_strike, DDS_adm, DDS_dis, DDS_fol)
colnames(PRIME_DAT)<-c("PATID","treat","treat_n","a1c_adm","a1c_dis", "a1c_fol", "a1c_strike", "DDS17_admission", "DDS17_discharge", "DDS17_follow")


is.na(PRIME_DAT$a1c_strike)

data.frame(PRIME_DAT$a1c_fol,is.na(PRIME_DAT$a1c_strike))

PRIME_DAT$a1c_fol2<-PRIME_DAT$a1c_fol*as.numeric(is.na(PRIME_DAT$a1c_strike))
PRIME_DAT$a1c_fol2[PRIME_DAT$a1c_fol2==0]<-NA
data.frame(PRIME_DAT$a1c_fol,PRIME_DAT$a1c_fol2,is.na(PRIME_DAT$a1c_strike))



########################################
#### Remove PATIDS THAT DID NOT PROVIDE ALL BASELINE DATA

# There are six such individuals:
PRIME_DAT[is.na(PRIME_DAT$a1c_adm) | is.na(PRIME_DAT$DDS17_admission),]
patids_miss<-PRIME_DAT[is.na(PRIME_DAT$a1c_adm) | is.na(PRIME_DAT$DDS17_admission),]$PATID

PRIME_DAT2<-PRIME_DAT[!PRIME_DAT$PATID%in%patids_miss,]

PRIME_DAT2_FOL<-PRIME_DAT2
########################################
#### A1C Change:
library(psych)
a1c_change_ALL<-PRIME_DAT2$a1c_dis-PRIME_DAT2$a1c_adm
PRIME_DAT2$a1c_change_ALL<-a1c_change_ALL
A1CTAB<-SUMMARYTABLE(PRIME_DAT2,"a1c_change_ALL", "a1c_adm")
A1CTAB

a1c_change_FOL<-PRIME_DAT2$a1c_fol-PRIME_DAT2$a1c_adm
PRIME_DAT2$a1c_change_FOL<-a1c_change_FOL
A1CTAB_FOL<-SUMMARYTABLE(PRIME_DAT2,"a1c_change_FOL", "a1c_adm")
A1CTAB_FOL

a1c_change_FOL2<-PRIME_DAT2$a1c_fol2-PRIME_DAT2$a1c_adm
PRIME_DAT2$a1c_change_FOL2<-a1c_change_FOL2
A1CTAB_FOL2<-SUMMARYTABLE(PRIME_DAT2,"a1c_change_FOL2", "a1c_adm")
A1CTAB_FOL2




########################################
## DDS Change:

dds_change_ALL<-PRIME_DAT2$DDS17_discharge-PRIME_DAT2$DDS17_admission
PRIME_DAT2$dds_change_ALL <-dds_change_ALL
ddsTAB<-SUMMARYTABLE(PRIME_DAT2,"dds_change_ALL", "DDS17_admission")
ddsTAB

dds_change_FOL<-PRIME_DAT2$DDS17_follow-PRIME_DAT2$DDS17_admission
PRIME_DAT2$dds_change_FOL<-dds_change_FOL
ddsTAB_FOL<-SUMMARYTABLE(PRIME_DAT2,"dds_change_FOL", "DDS17_admission")
ddsTAB_FOL

A1<-rbind(
A1CTAB[[1]],
A1CTAB_FOL[[1]],
A1CTAB_FOL2[[1]],
ddsTAB[[1]],
ddsTAB_FOL[[1]])


write.csv(A1, file="~/Desktop/EMMES-Projects/HeadStart/FollowUp/Headstartfollowup _initial_summary.csv")