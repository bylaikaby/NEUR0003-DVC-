# analysis of the cFOS-Glp1 staining 
library(tidyverse)
library(ggplot2)
library(ggsignif)

dg <- tibble(
  cFos =  c(41,45,32,35,50,27,40,42),
  glp1 = c(40,35,34,29,35,26,31,35),
  percent= c(0.125,0.142,0.029,0.069,0.114,0.115,0.125,0.029)
)

contrl <- tibble(
  cFos=c(0,0,0,5,6,3,2),
  glp1= c(33,35,36,40,26,38,16),
  percent = c(0,0,0,0.025,0,0.026,0)

)



t.test(dg$cFos,contrl$cFos,var.equal=T)

t.test(dg$percent,contrl$percent,var.equal=T)


fos=c(dg$cFos,contrl$cFos)
sd=c(sd(dg$cFos),sd(contrl$cFos))
mean=c(mean(dg$cFos),mean(contrl$cFos))
treatment_groups <- c("2-DG","Saline")

co=c(dg$percent,contrl$percent)
sdco=c(sd(dg$percent),sd(contrl$percent))
meanco=c(mean(dg$percent),mean(contrl$percent))


plot1 <- ggplot(mapping=aes(x=treatment_groups,y=mean,fill=treatment_groups))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.2) +
  scale_fill_grey()+theme_classic()+
  ylab("number of cFos+ cells")+ylim(-1,50)+ 
  geom_signif(comparisons = list(c("2-DG","Saline")),annotations="***")+NoLegend()+
  labs(x=" ")


plot2 <- ggplot(mapping=aes(x=treatment_groups,y=meanco,fill=treatment_groups))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=meanco-sdco,ymax=meanco+sdco),width=0.2) +
  scale_fill_grey()+theme_classic()+
  ylab("cFos+/ GLP1+ cells")+ 
  geom_signif(comparisons = list(c("2-DG","Saline")),annotations="***")+NoLegend()+
  labs(x="")

plot1+plot2

