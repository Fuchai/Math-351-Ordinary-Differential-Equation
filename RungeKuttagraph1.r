require(data.table)
require(dplyr)
require(deSolve)
require(ggplot2)

K<-80000
M<-8000
r<-0.02

de <- function(t, N, parms) {
    with(as.list(parms), {
        dN <- r * N[1] * (1 - N[1]/K)*(N[1]/M - 1)
        list(dN)
    })
} 

outout<-data.table()
n0<-seq(160000,4000,-1000)

for (nnn in n0){
    N<-c(N=nnn)
    time<-seq(0,100,0.3)
    parms<-c(r=r,K=K,M=M)
    out<-as.data.table(rk4(N,time,de,parms))
    out<-mutate(out,N0=nnn)
    outout<-bind_rows(outout,out)
}

ggplot(outout,aes(x=time,y=N,group=N0))+
    geom_point(aes(color=N0),alpha=1,size = 0.5)+
    xlab("Time in years")+
    ylab("Total population of wolves")+
    labs(color=expression(Initial~population~N[0]))+
    theme(legend.position="bottom",legend.key.width=unit(3,"line"))