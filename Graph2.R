#Scaled specially

h<-function(N){
    M<-30000
    r*N*(1-N/K)*(N/M-1)
}
x=seq(-0.3e+5,1e+5,100)
gx=seq(0,1e+5,100)
ggplot(data.frame(x), aes(x))+
    stat_function(fun=h,geom="line",aes())+
    labs(x="N",y="h")+
    theme_bw()+
    theme(axis.title.y=element_text(angle=0),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks=element_blank(),
          axis.text=element_blank()) +
    geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+
    geom_hline(yintercept = 300)+
    geom_hline(yintercept = -123.4568)+
    annotate("text",x=30000,y=100,label="M")+
    annotate("text",x=81000,y=100,label="K")+
    annotate("text",x=-4000,y=400,label="h[2]", parse=TRUE)+
    annotate("text",x=-4000,y=-200,label="h[1]", parse=TRUE)+
    coord_flip()
