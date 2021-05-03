library(GET)

nd.kernel=function(a,b){
  return(sqrt(sum((a-b)^2)))
}
N.dist=function(L,m1,m2){
  N1=sum(as.vector(L[(m1+1):(m1+m2),1:m1]))
  N2=sum(as.vector(L[1:m1,1:m1]))
  N3=sum(as.vector(L[(m1+1):(m1+m2),(m1+1):(m1+m2)]))
  return(abs(2*N1/(m1*m2)-N2/m1^2-N3/m2^2))
}
lo=function(a,b){
  d=length(a)
  i=1
  while((a[i]==b[i])&&(i<=d)){
      i=i+1
  }
  if(i==d+1){
    return(1)
  }
  else{
    if(a[i]<b[i]){
    return(1)
  }
  else{
    return(0)
  }}
}

Ntest=function(M1,M2,s){
M=rbind(M1,M2)
m1=dim(M1)[1]
m2=dim(M2)[1]
n1=dim(M1)[2]
m=m1+m2
L1=matrix(rep(0,m^2),nrow=m)
L2=matrix(rep(0,m^2),nrow=m)
for(i in 1:m){
  for(j in 1:m){
    L1[i,j]=nd.kernel(M[i,1],M[j,1])
    L2[i,j]=nd.kernel(M[i,2:n1],M[j,2:n1])
  }
}
T1=N.dist(L1,m1,m2)
T2=N.dist(L2,m1,m2)
t1=rep(0,s)
t2=rep(0,s)

for(i in 1:s){
  S=sample(1:m)
  t1[i]=N.dist(L1[S,S],m1,m2)
  t2[i]=N.dist(L2[S,S],m1,m2)
}
#t2
#cset=create_curve_set(list(r = 1:2, obs = as.matrix(rbind(C1,C2))))
cset <- create_curve_set(list(r=1:2, obs=c(T1,T2), 
                              sim_m=rbind(t1,t2)))
#plot(cset)
res=rank_envelope(cset, type = "erl", alternative="greater")
p=attr(res,"p")
#plot(res)                    
p1=(sum(t1>=T1)+1)/(s+1)
p2=(sum(t2>=T2)+1)/(s+1)
return(list(p=p,p1=p1,p2=p2))
}

##  R=3 Different processes
pBR=rep(0,100)
pBR1=rep(0,100)
pBR2=rep(0,100)
procesi=c("outputs_new/Boolean_elipse_3/out_3_",
          "outputs_new/Boolean_matrix_200_3/out_3_",
          "outputs_new/Boolean_rectangles_narrow_3/out_3_",
          "outputs_new/Cluster_novo3_3/out_3_",
          "outputs_new/Reduced_Boolean_matrix_100_3/out_3_",
          "outputs_new/Repulsive_matrix_200_3/out_3_",
          "outputs_new/Boolean_squares_new_3/out_3_")
kr=c("Be","B","Nr","Cn","Br","R","S")
imena=c("Boolean elipse",
       "Boolean",
       "Boolean rectangles narrow",
       "Custer",
       "Reduced Boolean",
       "Repulsive",
       "Squares")
So=matrix(rep(0,7*7),nrow=7)
S1=matrix(rep(0,7*7),nrow=7)
S2=matrix(rep(0,7*7),nrow=7)
s=2499
S=10
for(I in 1:6){
  for(J in 7){
for(i in 1:100){
  ime1=procesi[I]
  if(I==5){ime1=paste(ime1,i+99,sep="")}
  else{ime1=paste(ime1,i-1,sep="")}
  ime1=paste(ime1,".txt",sep="")
  ime2=procesi[J]
  if(J==5){ime2=paste(ime2,i+99,sep="")}
  else{ime2=paste(ime2,i-1,sep="")}
  ime2=paste(ime2,".txt",sep="")
  M1=read.table(ime1)
  M2=read.table(ime2)
  m1=dim(M1)[1]
  m2=dim(M2)[1]
  n=dim(M1)[2]
  for(j in 1:m1){
    M1[j,2:n]=M1[j,2:n]/sum(M1[,2:n])
  }
  for(j in 1:m2){
    M2[j,2:n]=M2[j,2:n]/sum(M2[,2:n])
  }
  m=min(c(S,m1,m2))
  rez=Ntest(as.matrix(M1[sample(1:dim(M1)[1],m),]),as.matrix(M2[sample(1:dim(M2)[1],m),]),999)
  pBR[i]=rez$p
  pBR1[i]=rez$p1
  pBR2[i]=rez$p2
  #print(c(i, pBR[i], pBR1[i], pBR2[i]))
}
dat=paste(kr[I],kr[J],sep="")
dat1=paste(dat,"_R=3_n=10_pval",sep="")
dat2=paste(dat,"_R=3_n=10_omjeri_pval",sep="")
dat3=paste(dat,"_R=3_n=10_zakrivljenost_pval",sep="")
write(pBR,dat1)
write(pBR1,dat2)
write(pBR2,dat3)
hm=paste(imena[I],"vs")
hm=paste(hm, imena[J])
hm=paste(hm, "R=3,n=10")
hm1=paste(hm,"(omjeri)")
hm2=paste(hm,"(zakrivljenost)")
di=paste(kr[I],kr[J],sep="")
di=paste(di,"_R=3_n=10_hist",sep="_")
di1=paste(di,"omjeri",sep="")
di2=paste(di,"zakrivljenost",sep="_")

  jpeg(paste(di, ".jpg", sep=""))
  hist(pBR,breaks = seq(0,1,0.05),
       main=hm,xlab="p-value")
  dev.off()
  jpeg(paste(di1, ".jpg", sep=""))
  hist(pBR1,breaks = seq(0,1,0.05),
       main=hm1,xlab="p-value")
  dev.off()
  jpeg(paste(di2, ".jpg", sep=""))
  hist(pBR2,breaks = seq(0,1,0.05),
       main=hm2,xlab="p-value")
  dev.off()
So[I,J]=sum(pBR<=0.05)
S1[I,J]=sum(pBR1<=0.05)
S2[I,J]=sum(pBR2<=0.05)
print(c(kr[I],kr[J],So[I,J],S1[I,J],S2[I,J]))
}}
## R=3 Same processes



S=c(10,20,30,50)
for(K in 1:4){
  for(I in c(1:4,6)){
    for(i in 1:100){
      ime1=procesi[I]
      ime1=paste(ime1,2*i-2,sep="")
      ime1=paste(ime1,".txt",sep="")
      ime2=procesi[I]
      ime2=paste(ime2,2*i-1,sep="")
      ime2=paste(ime2,".txt",sep="")
      M1=read.table(ime1)
      M2=read.table(ime2)
      m1=dim(M1)[1]
      m2=dim(M2)[1]
      n=dim(M1)[2]
      for(j in 1:m1){
        M1[j,2:n]=M1[j,2:n]/sum(M1[,2:n])
      }
      for(j in 1:m2){
        M2[j,2:n]=M2[j,2:n]/sum(M2[,2:n])
      }
      m=min(c(S[K],m1,m2))
      rez=Ntest(as.matrix(M1[sample(1:dim(M1)[1],m),]),as.matrix(M2[sample(1:dim(M2)[1],m),]),999)
      pBR[i]=rez$p
      pBR1[i]=rez$p1
      pBR2[i]=rez$p2
      #print(c(i, pBR[i], pBR1[i], pBR2[i]))
    }
    dat=paste(kr[I],kr[I],sep="")
    dat=paste(dat,"_R=3__n=",S[K],sep="")
    dat1=paste(dat,"_pval",sep="")
    dat2=paste(dat,"_omjeri_pval",sep="")
    dat3=paste(dat,"_zakrivljenost_pval",sep="")
    write(pBR,dat1)
    write(pBR1,dat2)
    write(pBR2,dat3)
    hm=paste(imena[I],"vs")
    hm=paste(hm, imena[I])
    hm=paste(hm, "R=3_n=",sep="")
    hm=paste(hm, S[K],sep="")
    hm1=paste(hm,"(omjeri)")
    hm2=paste(hm,"(zakrivljenost)")
    di=paste(kr[I],kr[I],sep="")
    di=paste(di,"_R=3_n=",sep="")
    di=paste(di,S[K],sep="")
    di=paste(di,"_hist",sep="_")
    di1=paste(di,"omjeri",sep="")
    di2=paste(di,"zakrivljenost",sep="_")
    
    jpeg(paste(di, ".jpg", sep=""))
    hist(pBR,breaks = seq(0,1,0.05),
         main=hm,xlab="p-value")
    dev.off()
    jpeg(paste(di1, ".jpg", sep=""))
    hist(pBR1,breaks = seq(0,1,0.05),
         main=hm1,xlab="p-value")
    dev.off()
    jpeg(paste(di2, ".jpg", sep=""))
    hist(pBR2,breaks = seq(0,1,0.05),
         main=hm2,xlab="p-value")
    dev.off()
    So[I,I]=sum(pBR<=0.05)
    S1[I,I]=sum(pBR1<=0.05)
    S2[I,I]=sum(pBR2<=0.05)
    print(c(kr[I],kr[I],So[I,I],S1[I,I],S2[I,I]))
  }
}


##  R=5 Different processes
pBR=rep(0,100)
pBR1=rep(0,100)
pBR2=rep(0,100)
procesi=c("outputs_new/Boolean_elipse_5/out_5_",
          "outputs_new/Boolean_matrix_200_5/out_5_",
          "outputs_new/Boolean_rectangles_narrow_5/out_5_",
          "outputs_new/Cluster_novo3_5/out_5_",
          "outputs_new/Reduced_Boolean_matrix_100_5/out_5_",
          "outputs_new/Repulsive_matrix_200_5/out_5_",
          "outputs_new/Boolean_squares_new_5/out_5_")
kr=c("Be","B","Nr","Cn","Br","R","S")
imena=c("Boolean elipse",
        "Boolean",
        "Boolean rectangles narrow",
        "Custer",
        "Reduced Boolean",
        "Repulsive",
        "Squares")
So5=matrix(rep(0,7*7),nrow=7)
S15=matrix(rep(0,7*7),nrow=7)
S25=matrix(rep(0,7*7),nrow=7)
s=2499
S=10
for(I in 1:6){
  for(J in 7){
    for(i in 1:100){
      ime1=procesi[I]
      if(I==5){ime1=paste(ime1,i+99,sep="")}
      else{ime1=paste(ime1,i-1,sep="")}
      ime1=paste(ime1,".txt",sep="")
      ime2=procesi[J]
      if(J==5){ime2=paste(ime2,i+99,sep="")}
      else{ime2=paste(ime2,i-1,sep="")}
      ime2=paste(ime2,".txt",sep="")
      M1=read.table(ime1)
      M2=read.table(ime2)
      m1=dim(M1)[1]
      m2=dim(M2)[1]
      n=dim(M1)[2]
      for(j in 1:m1){
        M1[j,2:n]=M1[j,2:n]/sum(M1[,2:n])
      }
      for(j in 1:m2){
        M2[j,2:n]=M2[j,2:n]/sum(M2[,2:n])
      }
      m=min(c(S,m1,m2))
      rez=Ntest(as.matrix(M1[sample(1:dim(M1)[1],m),]),as.matrix(M2[sample(1:dim(M2)[1],m),]),999)
      pBR[i]=rez$p
      pBR1[i]=rez$p1
      pBR2[i]=rez$p2
      #print(c(i, pBR[i], pBR1[i], pBR2[i]))
    }
    dat=paste(kr[I],kr[J],sep="")
    dat1=paste(dat,"_R=5_n=10_pval",sep="")
    dat2=paste(dat,"_R=5_n=10_omjeri_pval",sep="")
    dat3=paste(dat,"_R=5_n=10_zakrivljenost_pval",sep="")
    write(pBR,dat1)
    write(pBR1,dat2)
    write(pBR2,dat3)
    hm=paste(imena[I],"vs")
    hm=paste(hm, imena[J])
    hm=paste(hm, "R=5,n=10")
    hm1=paste(hm,"(omjeri)")
    hm2=paste(hm,"(zakrivljenost)")
    di=paste(kr[I],kr[J],sep="")
    di=paste(di,"_R=5_n=10_hist",sep="_")
    di1=paste(di,"omjeri",sep="")
    di2=paste(di,"zakrivljenost",sep="_")
    
    jpeg(paste(di, ".jpg", sep=""))
    hist(pBR,breaks = seq(0,1,0.05),
         main=hm,xlab="p-value")
    dev.off()
    jpeg(paste(di1, ".jpg", sep=""))
    hist(pBR1,breaks = seq(0,1,0.05),
         main=hm1,xlab="p-value")
    dev.off()
    jpeg(paste(di2, ".jpg", sep=""))
    hist(pBR2,breaks = seq(0,1,0.05),
         main=hm2,xlab="p-value")
    dev.off()
    So5[I,J]=sum(pBR<=0.05)
    S15[I,J]=sum(pBR1<=0.05)
    S25[I,J]=sum(pBR2<=0.05)
    print(c(kr[I],kr[J],So5[I,J],S15[I,J],S25[I,J]))
  }}

## R=5 Same processes

S=c(10,20,30,50)
for(K in 1:4){
for(I in c(1:4,6)){
    for(i in 1:100){
      ime1=procesi[I]
      ime1=paste(ime1,2*i-2,sep="")
      ime1=paste(ime1,".txt",sep="")
      ime2=procesi[I]
      ime2=paste(ime2,2*i-1,sep="")
      ime2=paste(ime2,".txt",sep="")
      M1=read.table(ime1)
      M2=read.table(ime2)
      m1=dim(M1)[1]
      m2=dim(M2)[1]
      n=dim(M1)[2]
      for(j in 1:m1){
        M1[j,2:n]=M1[j,2:n]/sum(M1[,2:n])
      }
      for(j in 1:m2){
        M2[j,2:n]=M2[j,2:n]/sum(M2[,2:n])
      }
      m=min(c(S[K],m1,m2))
      rez=Ntest(as.matrix(M1[sample(1:dim(M1)[1],m),]),as.matrix(M2[sample(1:dim(M2)[1],m),]),999)
      pBR[i]=rez$p
      pBR1[i]=rez$p1
      pBR2[i]=rez$p2
      #print(c(i, pBR[i], pBR1[i], pBR2[i]))
    }
    dat=paste(kr[I],kr[I],sep="")
    dat=paste(dat,"_R=5__n=",S[K],sep="")
    dat1=paste(dat,"_pval",sep="")
    dat2=paste(dat,"_omjeri_pval",sep="")
    dat3=paste(dat,"_zakrivljenost_pval",sep="")
    write(pBR,dat1)
    write(pBR1,dat2)
    write(pBR2,dat3)
    hm=paste(imena[I],"vs")
    hm=paste(hm, imena[I])
    hm=paste(hm, "R=5_n=",sep="")
    hm=paste(hm, S[K],sep="")
    hm1=paste(hm,"(omjeri)")
    hm2=paste(hm,"(zakrivljenost)")
    di=paste(kr[I],kr[I],sep="")
    di=paste(di,"_R=5_n=",sep="")
    di=paste(di,S[K],sep="")
    di=paste(di,"_hist",sep="_")
    di1=paste(di,"omjeri",sep="")
    di2=paste(di,"zakrivljenost",sep="_")
    
    jpeg(paste(di, ".jpg", sep=""))
    hist(pBR,breaks = seq(0,1,0.05),
         main=hm,xlab="p-value")
    dev.off()
    jpeg(paste(di1, ".jpg", sep=""))
    hist(pBR1,breaks = seq(0,1,0.05),
         main=hm1,xlab="p-value")
    dev.off()
    jpeg(paste(di2, ".jpg", sep=""))
    hist(pBR2,breaks = seq(0,1,0.05),
         main=hm2,xlab="p-value")
    dev.off()
    So5[I,I]=sum(pBR<=0.05)
    S15[I,I]=sum(pBR1<=0.05)
    S25[I,I]=sum(pBR2<=0.05)
    print(c(kr[I],kr[I],So5[I,I],S15[I,I],S25[I,I]))
  }
}

rownames(So)=imena
colnames(So)=imena
rownames(S1)=imena
colnames(S1)=imena
rownames(S2)=imena
colnames(S2)=imena

rownames(So5)=imena
colnames(So5)=imena
rownames(S15)=imena
colnames(S15)=imena
rownames(S25)=imena
colnames(S25)=imena

install.packages("xlsx")
library(xlsx)
write.xlsx(So, "Broj_p-vrijednosti_manjih_od_005_R=3_n=10.xlsx")
write.xlsx(So1, "Broj_p-vrijednosti_manjih_od_005_R=3_n=10_omjeri.xlsx")
write.xlsx(So2, "Broj_p-vrijednosti_manjih_od_005_R=3_n=10_zakrivljenost.xlsx")
write.xlsx(So5, "Broj_p-vrijednosti_manjih_od_005_R=5_n=10.xlsx")
write.xlsx(So15, "Broj_p-vrijednosti_manjih_od_005_R=5_n=10_omjeri.xlsx")
write.xlsx(So25, "Broj_p-vrijednosti_manjih_od_005_R=5_n=10_zakrivljenost.xlsx")


## Mamca Vs Mamca (isti proces)
imena=c("masto1","masto2","masto3","masto4",
        "masto5","masto6","masto7","masto8",
        "mamca1","mamca2","mamca3","mamca4",
        "mamca5","mamca6","mamca7","mamca8",
        "mastoCC1","mastoCC2","mastoCC3","mastoCC4",
        "mastoCC5","mastoCC6","mastoCC7","mastoCC8",
        "mamcaCC1","mamcaCC2","mamcaCC3","mamcaCC4",
        "mamcaCC5","mamcaCC6","mamcaCC7","mamcaCC8")

S=c(10,20,30)
R=c(3,5)
pBR=rep(0,100)
pBR1=rep(0,100)
pBR2=rep(0,100)
proc=c(rep("masto",8),rep("mamca",8),
       rep("mastoCC",8),rep("mamcaCC",8))
proc1=c(1:8,1:8,1:8,1:8)
for(K in 1:3){
for(J in 1:2){
for(i in 1:16){
  for(l in i:16){
  ime="mamcaCC_5_out/"
  ime=paste(ime,proc[i],sep="")
  ime=paste(ime,"_",sep="")
  ime=paste(ime,R[J],sep="")
  ime=paste(ime,"_",sep="")  
  ime=paste(ime,proc1[i],sep="")
  ime=paste(ime,".txt",sep="")
  M1=read.table(ime)
  ime="mamcaCC_5_out/"
  ime=paste(ime,proc[l],sep="")
  ime=paste(ime,"_",sep="")
  ime=paste(ime,R[J],sep="")
  ime=paste(ime,"_",sep="")  
  ime=paste(ime,proc1[l],sep="")
  ime=paste(ime,".txt",sep="")
  M2=read.table(ime)
  m1=dim(M1)[1]
  m2=dim(M2)[1]
  n=dim(M1)[2]
  for(j in 1:m1){
    M1[j,2:n]=M1[j,2:n]/sum(M1[,2:n])
  }
  for(j in 1:m2){
    M2[j,2:n]=M2[j,2:n]/sum(M2[,2:n])
  }
  m=min(c(S[K],m1,m2))
  for(L in 1:100){
  rez=Ntest(as.matrix(M1[sample(1:dim(M1)[1],m),]),as.matrix(M2[sample(1:dim(M2)[1],m),]),999)
  pBR[L]=rez$p
  pBR1[L]=rez$p1
  pBR2[L]=rez$p2
  }

  dat=paste(imena[i],imena[l],sep="")
  dat=paste(dat,"_R=",sep="")
  dat=paste(dat,R[J],sep="")
  dat=paste(dat,"_n=",S[K],sep="")
  dat1=paste(dat,"_pval",sep="")
  dat2=paste(dat,"_omjeri_pval",sep="")
  dat3=paste(dat,"_zakrivljenost_pval",sep="")
  write(pBR,dat1)
  write(pBR1,dat2)
  write(pBR2,dat3)
  hm=paste(imena[i],"vs")
  hm=paste(hm, imena[l])
  hm=paste(hm, "R=",sep=", ")
  hm=paste(hm,R[J],sep="")
  hm=paste(hm,"n=",sep=", ")
  hm=paste(hm, S[K],sep="")
  hm1=paste(hm,"(omjeri)")
  hm2=paste(hm,"(zakrivljenost)")
  di=paste(imena[i],imena[l],sep="")
  di=paste(di,"_R=",sep="")
  di=paste(di,R[J],sep="")
  di=paste(di,"_n=",sep="")
  di=paste(di,S[K],sep="")
  di=paste(di,"_hist",sep="_")
  di1=paste(di,"omjeri",sep="")
  di2=paste(di,"zakrivljenost",sep="_")
  
  jpeg(paste(di, ".jpg", sep=""))
  hist(pBR,breaks = seq(0,1,0.05),
       main=hm,xlab="p-value")
  dev.off()
  jpeg(paste(di1, ".jpg", sep=""))
  hist(pBR1,breaks = seq(0,1,0.05),
       main=hm1,xlab="p-value")
  dev.off()
  jpeg(paste(di2, ".jpg", sep=""))
  hist(pBR2,breaks = seq(0,1,0.05),
       main=hm2,xlab="p-value")
  dev.off()
  
}}
for(i in 17:32){
  for(l in i:32){
    ime="mamcaCC_5_out/"
    ime=paste(ime,proc[i],sep="")
    ime=paste(ime,"_",sep="")
    ime=paste(ime,R[J],sep="")
    ime=paste(ime,"_",sep="")  
    ime=paste(ime,proc1[i],sep="")
    ime=paste(ime,".txt",sep="")
    M1=read.table(ime)
    ime="mamcaCC_5_out/"
    ime=paste(ime,proc[l],sep="")
    ime=paste(ime,"_",sep="")
    ime=paste(ime,R[J],sep="")
    ime=paste(ime,"_",sep="")  
    ime=paste(ime,proc1[l],sep="")
    ime=paste(ime,".txt",sep="")
    M2=read.table(ime)
    m1=dim(M1)[1]
    m2=dim(M2)[1]
    n=dim(M1)[2]
    for(j in 1:m1){
      M1[j,2:n]=M1[j,2:n]/sum(M1[,2:n])
    }
    for(j in 1:m2){
      M2[j,2:n]=M2[j,2:n]/sum(M2[,2:n])
    }
    m=min(c(S[K],m1,m2))
    for(L in 1:100){
      rez=Ntest(as.matrix(M1[sample(1:dim(M1)[1],m),]),as.matrix(M2[sample(1:dim(M2)[1],m),]),999)
      pBR[L]=rez$p
      pBR1[L]=rez$p1
      pBR2[L]=rez$p2
    }
    
    dat=paste(imena[i],imena[l],sep="")
    dat=paste(dat,"_R=",sep="")
    dat=paste(dat,R[J],sep="")
    dat=paste(dat,"_n=",S[K],sep="")
    dat1=paste(dat,"_pval",sep="")
    dat2=paste(dat,"_omjeri_pval",sep="")
    dat3=paste(dat,"_zakrivljenost_pval",sep="")
    write(pBR,dat1)
    write(pBR1,dat2)
    write(pBR2,dat3)
    hm=paste(imena[i],"vs")
    hm=paste(hm, imena[l])
    hm=paste(hm, "R=",sep=", ")
    hm=paste(hm,R[J],sep="")
    hm=paste(hm,"n=",sep=", ")
    hm=paste(hm, S[K],sep="")
    hm1=paste(hm,"(omjeri)")
    hm2=paste(hm,"(zakrivljenost)")
    di=paste(imena[i],imena[l],sep="")
    di=paste(di,"_R=",sep="")
    di=paste(di,R[J],sep="")
    di=paste(di,"_n=",sep="")
    di=paste(di,S[K],sep="")
    di=paste(di,"_hist",sep="_")
    di1=paste(di,"omjeri",sep="")
    di2=paste(di,"zakrivljenost",sep="_")
    
    jpeg(paste(di, ".jpg", sep=""))
    hist(pBR,breaks = seq(0,1,0.05),
         main=hm,xlab="p-value")
    dev.off()
    jpeg(paste(di1, ".jpg", sep=""))
    hist(pBR1,breaks = seq(0,1,0.05),
         main=hm1,xlab="p-value")
    dev.off()
    jpeg(paste(di2, ".jpg", sep=""))
    hist(pBR2,breaks = seq(0,1,0.05),
         main=hm2,xlab="p-value")
    dev.off()
    
  }}
  }}  