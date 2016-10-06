require(data.table)
require(dplyr)
require(deSolve)
require(ggplot2)

de <- function(t, N, parms) {
    with(as.list(parms), {
        dN <- r * N[1] * (1 + (15*N[1]*cos(pi*t/11)-25*N)/(4*A0*d^t))*(25*N[1]/(A0*d^t) - 1)-800*(1+(3*cos(pi*t/11)-5)/(4*0.99^t))*(5/(0.99^t)-1)
        list(dN)
    })
} 
# parameters and preparations
outout<-data.table()
n0<-120000
timestep<-0.2
A0<-200000
d=0.99
r<-0.02

N<-c(N=n0)
time<-seq(0,100,timestep)
parms<-c(r=r,A0=A0,d=d)
really<-as.data.table(rk4(N,time,de,parms))
really<-mutate(really,h=1)

de <- function(t, N, parms) {
    with(as.list(parms), {
        dN <- r * N[1] * (1 + (15*N[1]*cos(pi*t/11)-25*N)/(4*A0*d^t))*(25*N[1]/(A0*d^t) - 1)
        list(dN)
    })
} 

parms<-c(r=r,A0=A0,d=d)
really2<-as.data.table(rk4(N,time,de,parms))
really2<-mutate(really2,h=0)
really<-bind_rows(really,really2)
ggplot(data=really,aes(x=time,y=N,color=factor(h,labels = c("No harvesting (h=0)","Harvesting scheme as in (3.7)"))))+geom_line()+
    labs(title="Population change under two harvesting schemes",
         x="Time, years",y="Total population of wolves",color="Harvest Schemes")+
    theme(legend.position="bottom")
