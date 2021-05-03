##  R=3 Different processes
pBR=rep(0,40)
pBR1=rep(0,40)
pBR2=rep(0,40)
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
S=100
P=c(1,2,4,6)
I=1
J=2
for(I in 1:4 ){
  for(J in (I+1):4){
    for(i in 1:40){
      ime1=procesi[P[I]]
      #if(I==5){ime1=paste(ime1,i+99,sep="")}
      #else{
        ime11=paste(ime1,5*i-5,sep="")
        ime12=paste(ime1,5*i-4,sep="")
        ime13=paste(ime1,5*i-3,sep="")
        ime14=paste(ime1,5*i-2,sep="")
        ime15=paste(ime1,5*i-1,sep="")
      #  }
      ime11=paste(ime11,".txt",sep="")
      ime12=paste(ime12,".txt",sep="")
      ime13=paste(ime13,".txt",sep="")
      ime14=paste(ime14,".txt",sep="")
      ime15=paste(ime15,".txt",sep="")
      ime2=procesi[P[J]]
      #if(J==5){ime2=paste(ime2,i+99,sep="")}
      #else{
      ime21=paste(ime2,5*i-5,sep="")
      ime22=paste(ime2,5*i-4,sep="")
      ime23=paste(ime2,5*i-3,sep="")
      ime24=paste(ime2,5*i-2,sep="")
      ime25=paste(ime2,5*i-1,sep="")
      #}
      ime21=paste(ime21,".txt",sep="")
      ime22=paste(ime22,".txt",sep="")
      ime23=paste(ime23,".txt",sep="")
      ime24=paste(ime24,".txt",sep="")
      ime25=paste(ime25,".txt",sep="")
      M11=read.table(ime11)
      M12=read.table(ime12)
      M13=read.table(ime13)
      M14=read.table(ime14)
      M15=read.table(ime15)
      M21=read.table(ime21)
      M22=read.table(ime22)
      M23=read.table(ime23)
      M24=read.table(ime24)
      M25=read.table(ime25)
      M1=rbind(M11,M12,M13,M14,M15)
      M2=rbind(M21,M22,M23,M24,M25)
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
    dat=paste("Slijepljeni/",kr[P[I]],sep="")
    dat=paste(dat,kr[P[J]],sep="")
    dat1=paste(dat,"_R=3_n=50_pval",sep="")
    dat2=paste(dat,"_R=3_n=50_omjeri_pval",sep="")
    dat3=paste(dat,"_R=3_n=50_zakrivljenost_pval",sep="")
    write(pBR,dat1)
    write(pBR1,dat2)
    write(pBR2,dat3)
    hm=paste(imena[P[I]],"vs")
    hm=paste(hm, imena[P[J]])
    hm=paste(hm, "R=3,n=50")
    hm1=paste(hm,"(omjeri)")
    hm2=paste(hm,"(zakrivljenost)")
    di=paste("Slijepljeni/",kr[P[I]],sep="")
    di=paste(di,kr[P[J]],sep="")
    di=paste(di,"_R=3_n=50_hist",sep="_")
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
    So[P[I],P[J]]=sum(pBR<=0.05)/40
    S1[P[I],P[J]]=sum(pBR1<=0.05)/40
    S2[P[I],P[J]]=sum(pBR2<=0.05)/40
    print(c(kr[P[I]],kr[P[J]],So[P[I],P[J]],S1[P[I],P[J]],S2[P[I],P[J]]))
}}


##  R=3 Same processes
pBR=rep(0,20)
pBR1=rep(0,20)
pBR2=rep(0,20)

for(I in 1:4 ){
  J=I
    for(i in 1:20){
      ime1=procesi[P[I]]
      #if(I==5){ime1=paste(ime1,i+99,sep="")}
      #else{
      ime11=paste(ime1,10*i-5,sep="")
      ime12=paste(ime1,10*i-4,sep="")
      ime13=paste(ime1,10*i-3,sep="")
      ime14=paste(ime1,10*i-2,sep="")
      ime15=paste(ime1,10*i-1,sep="")
      #  }
      ime11=paste(ime11,".txt",sep="")
      ime12=paste(ime12,".txt",sep="")
      ime13=paste(ime13,".txt",sep="")
      ime14=paste(ime14,".txt",sep="")
      ime15=paste(ime15,".txt",sep="")
      ime2=procesi[P[J]]
      #if(J==5){ime2=paste(ime2,i+99,sep="")}
      #else{
      ime21=paste(ime2,10*i-6,sep="")
      ime22=paste(ime2,10*i-7,sep="")
      ime23=paste(ime2,10*i-8,sep="")
      ime24=paste(ime2,10*i-9,sep="")
      ime25=paste(ime2,10*i-10,sep="")
      #}
      ime21=paste(ime21,".txt",sep="")
      ime22=paste(ime22,".txt",sep="")
      ime23=paste(ime23,".txt",sep="")
      ime24=paste(ime24,".txt",sep="")
      ime25=paste(ime25,".txt",sep="")
      M11=read.table(ime11)
      M12=read.table(ime12)
      M13=read.table(ime13)
      M14=read.table(ime14)
      M15=read.table(ime15)
      M21=read.table(ime21)
      M22=read.table(ime22)
      M23=read.table(ime23)
      M24=read.table(ime24)
      M25=read.table(ime25)
      M1=rbind(M11,M12,M13,M14,M15)
      M2=rbind(M21,M22,M23,M24,M25)
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
    dat=paste("Slijepljeni/",kr[P[I]],sep="")
    dat=paste(dat,kr[P[J]],sep="")
    dat1=paste(dat,"_R=3_n=50_pval",sep="")
    dat2=paste(dat,"_R=3_n=50_omjeri_pval",sep="")
    dat3=paste(dat,"_R=3_n=50_zakrivljenost_pval",sep="")
    write(pBR,dat1)
    write(pBR1,dat2)
    write(pBR2,dat3)
    hm=paste(imena[P[I]],"vs")
    hm=paste(hm, imena[P[J]])
    hm=paste(hm, "R=3,n=50")
    hm1=paste(hm,"(omjeri)")
    hm2=paste(hm,"(zakrivljenost)")
    di=paste("Slijepljeni/",kr[P[I]],sep="")
    di=paste(di,kr[P[J]],sep="")
    di=paste(di,"_R=3_n=50_hist",sep="_")
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
    So[P[I],P[J]]=sum(pBR<=0.05)/20
    S1[P[I],P[J]]=sum(pBR1<=0.05)/20
    S2[P[I],P[J]]=sum(pBR2<=0.05)/20
    print(c(kr[P[I]],kr[P[J]],So[P[I],P[J]],S1[P[I],P[J]],S2[P[I],P[J]]))
  }


##  R=3 Different processes Rand
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
d=c(0,0,0,0,100,0,0)
g=c(199,199,199,199,199,199,99)
imena=c("Boolean elipse",
        "Boolean",
        "Boolean rectangles narrow",
        "Custer",
        "Reduced Boolean",
        "Repulsive",
        "Squares")
SoR=matrix(rep(0,7*7),nrow=7)
S1R=matrix(rep(0,7*7),nrow=7)
S2R=matrix(rep(0,7*7),nrow=7)
s=2499
S=50
ss=10
#P=c(1,2,4,6)
P=1:7

for(I in 1:7 ){
  for(J in (I+1):7){
    for(i in 1:100){
      ime1=procesi[P[I]]
      #if(I==5){ime1=paste(ime1,i+99,sep="")}
      #else{
      sa=sample(d[I]:g[I],5)
      ime11=paste(ime1,sa[1],sep="")
      ime12=paste(ime1,sa[2],sep="")
      ime13=paste(ime1,sa[3],sep="")
      ime14=paste(ime1,sa[4],sep="")
      ime15=paste(ime1,sa[5],sep="")
      #  }
      ime11=paste(ime11,".txt",sep="")
      ime12=paste(ime12,".txt",sep="")
      ime13=paste(ime13,".txt",sep="")
      ime14=paste(ime14,".txt",sep="")
      ime15=paste(ime15,".txt",sep="")
      ime2=procesi[P[J]]
      #if(J==5){ime2=paste(ime2,i+99,sep="")}
      #else{
      sa=sample(d[J]:g[J],5)
      ime21=paste(ime2,sa[1],sep="")
      ime22=paste(ime2,sa[2],sep="")
      ime23=paste(ime2,sa[3],sep="")
      ime24=paste(ime2,sa[4],sep="")
      ime25=paste(ime2,sa[5],sep="")
      #}
      ime21=paste(ime21,".txt",sep="")
      ime22=paste(ime22,".txt",sep="")
      ime23=paste(ime23,".txt",sep="")
      ime24=paste(ime24,".txt",sep="")
      ime25=paste(ime25,".txt",sep="")
      M11=read.table(ime11)
      M12=read.table(ime12)
      M13=read.table(ime13)
      M14=read.table(ime14)
      M15=read.table(ime15)
      M21=read.table(ime21)
      M22=read.table(ime22)
      M23=read.table(ime23)
      M24=read.table(ime24)
      M25=read.table(ime25)
      M1=rbind(M11[sample(1:dim(M11)[1],min(c(ss,dim(M11)[1]))),],
               M12[sample(1:dim(M12)[1],min(c(ss,dim(M12)[1]))),]
               ,M13[sample(1:dim(M13)[1],min(c(ss,dim(M13)[1]))),],
               M14[sample(1:dim(M14)[1],min(c(ss,dim(M14)[1]))),]
               ,M15[sample(1:dim(M15)[1],min(c(ss,dim(M15)[1]))),])
      M2=rbind(M21[sample(1:dim(M21)[1],min(c(ss,dim(M21)[1]))),],
               M22[sample(1:dim(M22)[1],min(c(ss,dim(M22)[1]))),]
               ,M23[sample(1:dim(M23)[1],min(c(ss,dim(M23)[1]))),],
               M24[sample(1:dim(M24)[1],min(c(ss,dim(M24)[1]))),]
               ,M25[sample(1:dim(M25)[1],min(c(ss,dim(M25)[1]))),])
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
    dat=paste("Slijepljeni/",kr[P[I]],sep="")
    dat=paste(dat,kr[P[J]],sep="")
    dat1=paste(dat,"_R=3_n=50_pval_R",sep="")
    dat2=paste(dat,"_R=3_n=50_omjeri_pval_R",sep="")
    dat3=paste(dat,"_R=3_n=50_zakrivljenost_pval_R",sep="")
    write(pBR,dat1)
    write(pBR1,dat2)
    write(pBR2,dat3)
    hm=paste(imena[P[I]],"vs")
    hm=paste(hm, imena[P[J]])
    hm=paste(hm, "R=3,n=50")
    hm1=paste(hm,"(omjeri)")
    hm2=paste(hm,"(zakrivljenost)")
    di=paste("Slijepljeni/",kr[P[I]],sep="")
    di=paste(di,kr[P[J]],sep="")
    di=paste(di,"_R=3_n=50_hist_R_",sep="_")
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
    SoR[P[I],P[J]]=sum(pBR<=0.05)
    S1R[P[I],P[J]]=sum(pBR1<=0.05)
    S2R[P[I],P[J]]=sum(pBR2<=0.05)
    print(c(kr[P[I]],kr[P[J]],SoR[P[I],P[J]],S1R[P[I],P[J]],S2R[P[I],P[J]]))
  }}



##  R=3 Same processes
pBR=rep(0,100)
pBR1=rep(0,100)
pBR2=rep(0,100)

for(I in 1:7 ){
  J=I
  for(i in 1:100){
    ime1=procesi[P[I]]
    #if(I==5){ime1=paste(ime1,i+99,sep="")}
    #else{
    sa=sample(d[I]:g[I],10)
    ime11=paste(ime1,sa[1],sep="")
    ime12=paste(ime1,sa[2],sep="")
    ime13=paste(ime1,sa[3],sep="")
    ime14=paste(ime1,sa[4],sep="")
    ime15=paste(ime1,sa[5],sep="")
    #  }
    ime11=paste(ime11,".txt",sep="")
    ime12=paste(ime12,".txt",sep="")
    ime13=paste(ime13,".txt",sep="")
    ime14=paste(ime14,".txt",sep="")
    ime15=paste(ime15,".txt",sep="")
    ime2=procesi[P[J]]
    #if(J==5){ime2=paste(ime2,i+99,sep="")}
    #else{
    ime21=paste(ime2,sa[6],sep="")
    ime22=paste(ime2,sa[7],sep="")
    ime23=paste(ime2,sa[8],sep="")
    ime24=paste(ime2,sa[9],sep="")
    ime25=paste(ime2,sa[10],sep="")
    #}
    ime21=paste(ime21,".txt",sep="")
    ime22=paste(ime22,".txt",sep="")
    ime23=paste(ime23,".txt",sep="")
    ime24=paste(ime24,".txt",sep="")
    ime25=paste(ime25,".txt",sep="")
    M11=read.table(ime11)
    M12=read.table(ime12)
    M13=read.table(ime13)
    M14=read.table(ime14)
    M15=read.table(ime15)
    M21=read.table(ime21)
    M22=read.table(ime22)
    M23=read.table(ime23)
    M24=read.table(ime24)
    M25=read.table(ime25)
    M1=rbind(M11[sample(1:dim(M11)[1],min(c(ss,dim(M11)[1]))),],
             M12[sample(1:dim(M12)[1],min(c(ss,dim(M12)[1]))),]
             ,M13[sample(1:dim(M13)[1],min(c(ss,dim(M13)[1]))),],
             M14[sample(1:dim(M14)[1],min(c(ss,dim(M14)[1]))),]
             ,M15[sample(1:dim(M15)[1],min(c(ss,dim(M15)[1]))),])
    M2=rbind(M21[sample(1:dim(M21)[1],min(c(ss,dim(M21)[1]))),],
             M22[sample(1:dim(M22)[1],min(c(ss,dim(M22)[1]))),]
             ,M23[sample(1:dim(M23)[1],min(c(ss,dim(M23)[1]))),],
             M24[sample(1:dim(M24)[1],min(c(ss,dim(M24)[1]))),]
             ,M25[sample(1:dim(M25)[1],min(c(ss,dim(M25)[1]))),])
    
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
  dat=paste("Slijepljeni/",kr[P[I]],sep="")
  dat=paste(dat,kr[P[J]],sep="")
  dat1=paste(dat,"_R=3_n=50_pval_R",sep="")
  dat2=paste(dat,"_R=3_n=50_omjeri_pval_R",sep="")
  dat3=paste(dat,"_R=3_n=50_zakrivljenost_pval_R",sep="")
  write(pBR,dat1)
  write(pBR1,dat2)
  write(pBR2,dat3)
  hm=paste(imena[P[I]],"vs")
  hm=paste(hm, imena[P[J]])
  hm=paste(hm, "R=3,n=50")
  hm1=paste(hm,"(omjeri)")
  hm2=paste(hm,"(zakrivljenost)")
  di=paste("Slijepljeni/",kr[P[I]],sep="")
  di=paste(di,kr[P[J]],sep="")
  di=paste(di,"_R=3_n=50_hist_R_",sep="_")
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
  SoR[P[I],P[J]]=sum(pBR<=0.05)
  S1R[P[I],P[J]]=sum(pBR1<=0.05)
  S2R[P[I],P[J]]=sum(pBR2<=0.05)
  print(c(kr[P[I]],kr[P[J]],SoR[P[I],P[J]],S1R[P[I],P[J]],S2R[P[I],P[J]]))
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
d=c(0,0,0,0,100,0,0)
g=c(199,199,199,199,199,199,99)
imena=c("Boolean elipse",
        "Boolean",
        "Boolean rectangles narrow",
        "Custer",
        "Reduced Boolean",
        "Repulsive",
        "Squares")
SoR5=matrix(rep(0,7*7),nrow=7)
S1R5=matrix(rep(0,7*7),nrow=7)
S2R5=matrix(rep(0,7*7),nrow=7)
s=2499
S=50
ss=10
#P=c(1,2,4,6)
P=1:7

for(I in 1:7 ){
  for(J in (I+1):7){
    for(i in 1:100){
      ime1=procesi[P[I]]
      #if(I==5){ime1=paste(ime1,i+99,sep="")}
      #else{
      sa=sample(d[I]:g[I],20)
      ime11=paste(ime1,sa[1],sep="")
      ime12=paste(ime1,sa[2],sep="")
      ime13=paste(ime1,sa[3],sep="")
      ime14=paste(ime1,sa[4],sep="")
      ime15=paste(ime1,sa[5],sep="")
      ime16=paste(ime1,sa[6],sep="")
      ime17=paste(ime1,sa[7],sep="")
      ime18=paste(ime1,sa[8],sep="")
      ime19=paste(ime1,sa[9],sep="")
      ime110=paste(ime1,sa[10],sep="")
      #  }
      ime11=paste(ime11,".txt",sep="")
      ime12=paste(ime12,".txt",sep="")
      ime13=paste(ime13,".txt",sep="")
      ime14=paste(ime14,".txt",sep="")
      ime15=paste(ime15,".txt",sep="")
      ime16=paste(ime16,".txt",sep="")
      ime17=paste(ime17,".txt",sep="")
      ime18=paste(ime18,".txt",sep="")
      ime19=paste(ime19,".txt",sep="")
      ime110=paste(ime110,".txt",sep="")
      ime2=procesi[P[J]]
      #if(J==5){ime2=paste(ime2,i+99,sep="")}
      #else{
      ime21=paste(ime2,sa[11],sep="")
      ime22=paste(ime2,sa[12],sep="")
      ime23=paste(ime2,sa[13],sep="")
      ime24=paste(ime2,sa[14],sep="")
      ime25=paste(ime2,sa[15],sep="")
      ime26=paste(ime2,sa[16],sep="")
      ime27=paste(ime2,sa[17],sep="")
      ime28=paste(ime2,sa[18],sep="")
      ime29=paste(ime2,sa[19],sep="")
      ime210=paste(ime2,sa[20],sep="")
      #}
      ime21=paste(ime21,".txt",sep="")
      ime22=paste(ime22,".txt",sep="")
      ime23=paste(ime23,".txt",sep="")
      ime24=paste(ime24,".txt",sep="")
      ime25=paste(ime25,".txt",sep="")
      ime26=paste(ime26,".txt",sep="")
      ime27=paste(ime27,".txt",sep="")
      ime28=paste(ime28,".txt",sep="")
      ime29=paste(ime29,".txt",sep="")
      ime210=paste(ime210,".txt",sep="")
      M11=read.table(ime11)
      M12=read.table(ime12)
      M13=read.table(ime13)
      M14=read.table(ime14)
      M15=read.table(ime15)
      M21=read.table(ime21)
      M22=read.table(ime22)
      M23=read.table(ime23)
      M24=read.table(ime24)
      M25=read.table(ime25)
      M16=read.table(ime16)
      M17=read.table(ime17)
      M18=read.table(ime18)
      M19=read.table(ime19)
      M110=read.table(ime110)
      M26=read.table(ime26)
      M27=read.table(ime27)
      M28=read.table(ime28)
      M29=read.table(ime29)
      M210=read.table(ime210)
      M1=rbind(M11[sample(1:dim(M11)[1],min(c(ss,dim(M11)[1]))),],
               M12[sample(1:dim(M12)[1],min(c(ss,dim(M12)[1]))),]
               ,M13[sample(1:dim(M13)[1],min(c(ss,dim(M13)[1]))),],
               M14[sample(1:dim(M14)[1],min(c(ss,dim(M14)[1]))),]
               ,M15[sample(1:dim(M15)[1],min(c(ss,dim(M15)[1]))),],
               M16[sample(1:dim(M16)[1],min(c(ss,dim(M16)[1]))),],
               M17[sample(1:dim(M17)[1],min(c(ss,dim(M17)[1]))),]
               ,M18[sample(1:dim(M18)[1],min(c(ss,dim(M18)[1]))),],
               M19[sample(1:dim(M19)[1],min(c(ss,dim(M19)[1]))),]
               ,M110[sample(1:dim(M110)[1],min(c(ss,dim(M110)[1]))),])
      M2=rbind(M21[sample(1:dim(M21)[1],min(c(ss,dim(M21)[1]))),],
               M22[sample(1:dim(M22)[1],min(c(ss,dim(M22)[1]))),]
               ,M23[sample(1:dim(M23)[1],min(c(ss,dim(M23)[1]))),],
               M24[sample(1:dim(M24)[1],min(c(ss,dim(M24)[1]))),]
               ,M25[sample(1:dim(M25)[1],min(c(ss,dim(M25)[1]))),],
               M26[sample(1:dim(M26)[1],min(c(ss,dim(M26)[1]))),],
               M27[sample(1:dim(M27)[1],min(c(ss,dim(M27)[1]))),]
               ,M28[sample(1:dim(M28)[1],min(c(ss,dim(M28)[1]))),],
               M29[sample(1:dim(M29)[1],min(c(ss,dim(M29)[1]))),]
               ,M210[sample(1:dim(M210)[1],min(c(ss,dim(M210)[1]))),])
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
    dat=paste("Slijepljeni/",kr[P[I]],sep="")
    dat=paste(dat,kr[P[J]],sep="")
    dat1=paste(dat,"_R=5_n=50_pval_R",sep="")
    dat2=paste(dat,"_R=5_n=50_omjeri_pval_R",sep="")
    dat3=paste(dat,"_R=5_n=50_zakrivljenost_pval_R",sep="")
    write(pBR,dat1)
    write(pBR1,dat2)
    write(pBR2,dat3)
    hm=paste(imena[P[I]],"vs")
    hm=paste(hm, imena[P[J]])
    hm=paste(hm, "R=5,n=50")
    hm1=paste(hm,"(omjeri)")
    hm2=paste(hm,"(zakrivljenost)")
    di=paste("Slijepljeni/",kr[P[I]],sep="")
    di=paste(di,kr[P[J]],sep="")
    di=paste(di,"_R=5_n=50_hist_R_",sep="_")
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
    SoR5[P[I],P[J]]=sum(pBR<=0.05)
    S1R5[P[I],P[J]]=sum(pBR1<=0.05)
    S2R5[P[I],P[J]]=sum(pBR2<=0.05)
    print(c(kr[P[I]],kr[P[J]],SoR5[P[I],P[J]],S1R5[P[I],P[J]],S2R5[P[I],P[J]]))
  }}


##  R=5 Same processes
pBR=rep(0,100)
pBR1=rep(0,100)
pBR2=rep(0,100)
S=50
ss=10
for(I in 1:7 ){
  J=I
  for(i in 1:100){
    ime1=procesi[P[I]]
    #if(I==5){ime1=paste(ime1,i+99,sep="")}
    #else{
    sa=sample(d[I]:g[I],20)
    ime11=paste(ime1,sa[1],sep="")
    ime12=paste(ime1,sa[2],sep="")
    ime13=paste(ime1,sa[3],sep="")
    ime14=paste(ime1,sa[4],sep="")
    ime15=paste(ime1,sa[5],sep="")
    ime16=paste(ime1,sa[6],sep="")
    ime17=paste(ime1,sa[7],sep="")
    ime18=paste(ime1,sa[8],sep="")
    ime19=paste(ime1,sa[9],sep="")
    ime110=paste(ime1,sa[10],sep="")
    #  }
    ime11=paste(ime11,".txt",sep="")
    ime12=paste(ime12,".txt",sep="")
    ime13=paste(ime13,".txt",sep="")
    ime14=paste(ime14,".txt",sep="")
    ime15=paste(ime15,".txt",sep="")
    ime16=paste(ime16,".txt",sep="")
    ime17=paste(ime17,".txt",sep="")
    ime18=paste(ime18,".txt",sep="")
    ime19=paste(ime19,".txt",sep="")
    ime110=paste(ime110,".txt",sep="")
    ime2=procesi[P[J]]
    #if(J==5){ime2=paste(ime2,i+99,sep="")}
    #else{
    ime21=paste(ime2,sa[11],sep="")
    ime22=paste(ime2,sa[12],sep="")
    ime23=paste(ime2,sa[13],sep="")
    ime24=paste(ime2,sa[14],sep="")
    ime25=paste(ime2,sa[15],sep="")
    ime26=paste(ime2,sa[16],sep="")
    ime27=paste(ime2,sa[17],sep="")
    ime28=paste(ime2,sa[18],sep="")
    ime29=paste(ime2,sa[19],sep="")
    ime210=paste(ime2,sa[20],sep="")
    #}
    ime21=paste(ime21,".txt",sep="")
    ime22=paste(ime22,".txt",sep="")
    ime23=paste(ime23,".txt",sep="")
    ime24=paste(ime24,".txt",sep="")
    ime25=paste(ime25,".txt",sep="")
    ime26=paste(ime26,".txt",sep="")
    ime27=paste(ime27,".txt",sep="")
    ime28=paste(ime28,".txt",sep="")
    ime29=paste(ime29,".txt",sep="")
    ime210=paste(ime210,".txt",sep="")
    M11=read.table(ime11)
    M12=read.table(ime12)
    M13=read.table(ime13)
    M14=read.table(ime14)
    M15=read.table(ime15)
    M21=read.table(ime21)
    M22=read.table(ime22)
    M23=read.table(ime23)
    M24=read.table(ime24)
    M25=read.table(ime25)
    M16=read.table(ime16)
    M17=read.table(ime17)
    M18=read.table(ime18)
    M19=read.table(ime19)
    M110=read.table(ime110)
    M26=read.table(ime26)
    M27=read.table(ime27)
    M28=read.table(ime28)
    M29=read.table(ime29)
    M210=read.table(ime210)
    M1=rbind(M11[sample(1:dim(M11)[1],min(c(ss,dim(M11)[1]))),],
             M12[sample(1:dim(M12)[1],min(c(ss,dim(M12)[1]))),]
             ,M13[sample(1:dim(M13)[1],min(c(ss,dim(M13)[1]))),],
             M14[sample(1:dim(M14)[1],min(c(ss,dim(M14)[1]))),]
             ,M15[sample(1:dim(M15)[1],min(c(ss,dim(M15)[1]))),],
             M16[sample(1:dim(M16)[1],min(c(ss,dim(M16)[1]))),],
             M17[sample(1:dim(M17)[1],min(c(ss,dim(M17)[1]))),]
             ,M18[sample(1:dim(M18)[1],min(c(ss,dim(M18)[1]))),],
             M19[sample(1:dim(M19)[1],min(c(ss,dim(M19)[1]))),]
             ,M110[sample(1:dim(M110)[1],min(c(ss,dim(M110)[1]))),])
    M2=rbind(M21[sample(1:dim(M21)[1],min(c(ss,dim(M21)[1]))),],
             M22[sample(1:dim(M22)[1],min(c(ss,dim(M22)[1]))),]
             ,M23[sample(1:dim(M23)[1],min(c(ss,dim(M23)[1]))),],
             M24[sample(1:dim(M24)[1],min(c(ss,dim(M24)[1]))),]
             ,M25[sample(1:dim(M25)[1],min(c(ss,dim(M25)[1]))),],
             M26[sample(1:dim(M26)[1],min(c(ss,dim(M26)[1]))),],
             M27[sample(1:dim(M27)[1],min(c(ss,dim(M27)[1]))),]
             ,M28[sample(1:dim(M28)[1],min(c(ss,dim(M28)[1]))),],
             M29[sample(1:dim(M29)[1],min(c(ss,dim(M29)[1]))),]
             ,M210[sample(1:dim(M210)[1],min(c(ss,dim(M210)[1]))),])
    
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
  dat=paste("Slijepljeni/",kr[P[I]],sep="")
  dat=paste(dat,kr[P[J]],sep="")
  dat1=paste(dat,"_R=5_n=50_pval_R",sep="")
  dat2=paste(dat,"_R=5_n=50_omjeri_pval_R",sep="")
  dat3=paste(dat,"_R=5_n=50_zakrivljenost_pval_R",sep="")
  write(pBR,dat1)
  write(pBR1,dat2)
  write(pBR2,dat3)
  hm=paste(imena[P[I]],"vs")
  hm=paste(hm, imena[P[J]])
  hm=paste(hm, "R=5,n=50")
  hm1=paste(hm,"(omjeri)")
  hm2=paste(hm,"(zakrivljenost)")
  di=paste("Slijepljeni/",kr[P[I]],sep="")
  di=paste(di,kr[P[J]],sep="")
  di=paste(di,"_R=5_n=50_hist_R_",sep="_")
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
  SoR5[P[I],P[J]]=sum(pBR<=0.05)
  S1R5[P[I],P[J]]=sum(pBR1<=0.05)
  S2R5[P[I],P[J]]=sum(pBR2<=0.05)
  print(c(kr[P[I]],kr[P[J]],SoR5[P[I],P[J]],S1R5[P[I],P[J]],S2R5[P[I],P[J]]))
}

