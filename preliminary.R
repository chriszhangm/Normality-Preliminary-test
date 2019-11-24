#############normal#####################################################################################
library(parallel)
M=100000
funrun=function(n,alphag,M){
  fun=function(x,n,alphag){
    flag=0;cnt=0
    while(flag==0){
      data=rnorm(n,0,1);cnt=cnt+1
      if((shapiro.test(data)$p.value)>alphag)
      {flag=1
      return(list(t.test(data,mu=0)$p.value,cnt))}
    }
  }
  
  cl<- makeCluster(detectCores()-1)
  clusterExport(cl,c("fun","n","alphag"),envir=environment())
  res<-parLapply(cl,1:M,function(x) fun(x,n,alphag))
  stopCluster(cl)
  
  result1=numeric(length = M)
  result2=numeric(length = M)
  for(i in 1:M){result1[i]=res[[i]][1];result2[i]=res[[i]][2]}
  
  return(
    list(
      alphat_10=sum(result1<0.1)/M*100,
      alphat_5 =sum(result1<0.05)/M*100,
      alphat_1 =sum(result1<0.01)/M*100,
      alphat_0.5=sum(result1<0.005)/M*100,
      power=sum(unlist(result2))
    )
  )
}



summay=data.frame(alphat_10=NA,alphat_5=NA,alphat_1=NA,alphat_0.5=NA,power=NA,n=NA,alphag=NA)
it=0
#n=30;alphag=0.005;
for (n in c(10,20,30,50)){
  for(alphag in c(0.1,0.05,0.01,0.005,0.001,0)){
    sy=Sys.time()
    it=it+1
    output=c(unlist(funrun(n,alphag,M)),n,alphag)
    summay[it,]=output
    print(Sys.time()-sy)
    print(summay)
  }
}
summay

write.csv(summay, file = "DA4unif.csv")


#x<-read.csv("d.csv",header = T,row.names = 1)
x<-matrix(summay[,2],byrow = F,ncol=4)
pdf("normal.pdf")
plot(x,type="n",xlim = c(10,50),ylim=c(3.5,5.5),main="estimated type 1 error rates under Normal Distribution",xlab="sample size (n)",ylab="estimated type 1 error rates (in %) of t-test")
for(i in 1:6){
  lines(c(10,20,30,50),x[i,],type="b",pch=i+10,cex=1)}
legend("bottomleft",c(expression(alpha[g]==10~"%"),expression(alpha[g]==5~"%"),expression(alpha[g]==1~"%"),expression(alpha[g]==0.5~"%"),expression(alpha[g]==0.1~"%"),"w/o pretest"),pch=11:16,cex=1.15)
dev.off()
#######unif#############################################

funrun=function(n,alphag,M){
  fun=function(x,n,alphag){
    flag=0;cnt=0
    while(flag==0){
      data=runif(n,0,1);cnt=cnt+1
      if((shapiro.test(data)$p.value)>alphag)
      {flag=1
      return(list(t.test(data,mu=0.5)$p.value,cnt))}
    }
  }
  
  cl<- makeCluster(detectCores()-1)
  clusterExport(cl,c("fun","n","alphag"),envir=environment())
  res<-parLapply(cl,1:M,function(x) fun(x,n,alphag))
  stopCluster(cl)
  
  result1=numeric(length = M)
  result2=numeric(length = M)
  for(i in 1:M){result1[i]=res[[i]][1];result2[i]=res[[i]][2]}
  
  return(
    list(
      alphat_10=sum(result1<0.1)/M*100,
      alphat_5 =sum(result1<0.05)/M*100,
      alphat_1 =sum(result1<0.01)/M*100,
      alphat_0.5=sum(result1<0.005)/M*100,
      power=sum(unlist(result2))
    )
  )
}



summay=data.frame(alphat_10=NA,alphat_5=NA,alphat_1=NA,alphat_0.5=NA,power=NA,n=NA,alphag=NA)
it=0
#n=30;alphag=0.005;
for (n in c(10,20,30,50)){
  for(alphag in c(0.1,0.05,0.01,0.005,0.001,0)){
    sy=Sys.time()
    it=it+1
    output=c(unlist(funrun(n,alphag,M)),n,alphag)
    summay[it,]=output
    print(Sys.time()-sy)
    print(summay)
  }
}
summay

write.csv(summay, file = "DA4unif.csv")


x<-matrix(summay[,2],byrow = F,ncol=4)
pdf("unif.pdf")
plot(x,type="n",xlim = c(10,50),ylim=c(3.5,5.5),main="estimated type 1 error rates under Uniform Distribution",xlab="sample size (n)",ylab="estimated type 1 error rates (in %) of t-test")
for(i in 1:6){
  lines(c(10,20,30,50),x[i,],type="b",pch=i+10,cex=2)}
legend("bottomleft",c(expression(alpha[g]==10~"%"),expression(alpha[g]==5~"%"),expression(alpha[g]==1~"%"),expression(alpha[g]==0.5~"%"),expression(alpha[g]==0.1~"%"),"w/o pretest"),pch=11:16,cex=1.15)
dev.off()