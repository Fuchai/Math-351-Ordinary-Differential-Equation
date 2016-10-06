# This code is not ready to run, as the parameters are not specified locally.
# Use K=80000, M=8000, r=0.02

de <- function(t, N, parms) {
    with(as.list(parms), {
        dN <- r * N[1] * (1 - N[1]/K)*(N[1]/M - 1)-h
        list(dN)
    })
} 
# parameters and preparations
outout<-data.table()
n0<-120000
h<-5000
top<-10000
bottom<-0
timestep<-0.2
K<-80000
M<-8000
r<-0.02

# iterate until criterion met
while (top-bottom>1){
    N<-c(N=n0)
    time<-seq(0,100,timestep)
    parms<-c(r=r,K=K,M=M,h=h)
    out<-as.data.table(rk4(N,time,de,parms))
    out<-mutate(out,h=h)
    outout<-bind_rows(outout,out)
    if (out[100/timestep+1,N]>0) {
        bottom<-h
    }
    else{
        top<-h
    }
    h<-(top+bottom)/2
}

result<-data.table(h=unique(outout$h))
value<-c()
for (h.iter in result$h){
    value<-c(value,filter(outout,h==h.iter)%>%filter(row_number()==501)%>%select(N))
}
result<-mutate(result,N100=value)
xtable(result)