library(stats)
library(ggplot2)

# X values
corn_A<-0
corn_B<-50
corn_C<-100
start_x<-25
start_y<-25
its <- 10000000
new_point<-((start_x+corn_C)/2)

y <- as.numeric(round(runif(its, min = 1, max = 3),0))
z<-as.data.frame(capture.output(for (val in y) {
  if(val == 3)
  new_point = ((new_point+corn_C)/2)
  if(val == 2)
  new_point = ((new_point+corn_B)/2)
  if(val == 1)
  new_point = ((new_point+corn_A)/2)
  print(new_point)
}))
colnames(z) <- c("xvals")
cleanz = as.data.frame(substring(z$xvals,5))
colnames(cleanz)<- c("xvals")

# Y values
corn_A<-0
corn_B<-50
corn_C<-0
new_point<-((start_x+corn_C)/2)

p<-as.data.frame(capture.output(for (val in y) {
  if(val == 3)
    new_point = ((new_point+corn_C)/2)
  if(val == 2)
    new_point = ((new_point+corn_B)/2)
  if(val == 1)
    new_point = ((new_point+corn_A)/2)
  print(new_point)
}))
colnames(p) <- c("yvals")
cleanp<-as.data.frame(substring(p$yvals,5))
colnames(cleanp) <- c("yvals")

graph<-as.data.frame(cbind(cleanz,cleanp))


ggplot(data = graph, aes(x = xvals, y=yvals)) + geom_point(size=.1)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
