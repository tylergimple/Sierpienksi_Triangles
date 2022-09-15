library(stats)
library(ggplot2)
library(plotly)
library(png) 
library(gifski)


# X values
corn_A<-0
corn_B<-50
corn_C<-100
start_x<-50
start_y<-20
index <- its <- 10000
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
cleanz = as.data.frame(as.numeric(substring(z$xvals,5)))
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
cleanp<-as.data.frame(as.numeric(substring(p$yvals,5)))
colnames(cleanp) <- c("yvals")

index<-as.data.frame(seq(1:index))
graph<-as.data.frame(cbind(cleanz,cleanp,index))
colnames(graph) <- c("xvals", "yvals","index")


ggplot(data = graph, aes(x = xvals, y=yvals)) + geom_point(size=.1) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

bubbleplot2 <- plot_ly(graph, x = ~xvals, y = ~yvals,
                       sizes = c(10, 50),
                       marker =
                         list(opacity = 0.7,
                              sizemode = "diameter"), color = ~yvals, colors = 'Purples')
bubbleplot2


p <- ggplot(graph, aes(xvals, yvals)) +
  geom_point(size=0.2) +
  transition_time(index) +
  shadow_mark()

animate(p)



plot_ly(data = graph, x = ~xvals, y = ~yvals, type = "scatter", mode = "markers", marker = list(size = 1, color = "red"))

f<-as.data.frame(rep(1, its))
onex<-as.data.frame(graph$xvals[1:(its/100)])
oney<-as.data.frame(graph$yvals[1:(its/100)])
onez<-as.data.frame(rep(1, its/100))
twox<-as.data.frame(graph$xvals[1:(its/50)])
twoy<-as.data.frame(graph$yvals[1:(its/50)])
twoz<-as.data.frame(rep(2, its/50))
thrx<-as.data.frame(graph$xvals[1:(its/30)])
thry<-as.data.frame(graph$yvals[1:(its/30)])
thrz<-as.data.frame(rep(3, its/30))
fourx<-as.data.frame(graph$xvals[1:(its/25)])
foury<-as.data.frame(graph$yvals[1:(its/25)])
fourz<-as.data.frame(rep(4, its/25))
fivex<-as.data.frame(graph$xvals[1:(its/20)])
fivey<-as.data.frame(graph$yvals[1:(its/20)])
fivez<-as.data.frame(rep(5, its/20))
sixx<-as.data.frame(graph$xvals[1:(its/15)])
sixy<-as.data.frame(graph$yvals[1:(its/15)])
sixz<-as.data.frame(rep(6, its/15))
sevx<-as.data.frame(graph$xvals[1:(its/10)])
sevy<-as.data.frame(graph$yvals[1:(its/10)])
sevz<-as.data.frame(rep(7, its/10))
eigx<-as.data.frame(graph$xvals[1:(its/5)])
eigy<-as.data.frame(graph$yvals[1:(its/5)])
eigz<-as.data.frame(rep(8, its/5))
ninx<-as.data.frame(graph$xvals[1:(its/2)])
niny<-as.data.frame(graph$yvals[1:(its/2)])
ninz<-as.data.frame(rep(9, its/2))
tenx<-as.data.frame(graph$xvals[1:(its)])
teny<-as.data.frame(graph$yvals[1:(its)])
tenz<-as.data.frame(rep(10, its/1))
one<-as.data.frame(cbind(onex,oney,onez))
colnames(one) <- c("xvals", "yvals","its")
two<-as.data.frame(cbind(twox,twoy,twoz))
colnames(two) <- c("xvals", "yvals","its")
thr<-as.data.frame(cbind(thrx,thry,thrz))
colnames(thr) <- c("xvals", "yvals","its")
four<-as.data.frame(cbind(fourx,foury,fourz))
colnames(four) <- c("xvals", "yvals","its")
five<-as.data.frame(cbind(fivex,fivey,fivez))
colnames(five) <- c("xvals", "yvals","its")
six<-as.data.frame(cbind(sixx,sixy,sixz))
colnames(six) <- c("xvals", "yvals","its")
sev<-as.data.frame(cbind(sevx,sevy,sevz))
colnames(sev) <- c("xvals", "yvals","its")
eig<-as.data.frame(cbind(eigx,eigy,eigz))
colnames(eig) <- c("xvals", "yvals","its")
nin<-as.data.frame(cbind(ninx,niny,ninz))
colnames(nin) <- c("xvals", "yvals","its")
ten<-as.data.frame(cbind(tenx,teny,tenz))
colnames(ten) <- c("xvals", "yvals","its")
final<-as.data.frame(rbind(one,two,thr,four,five,six,sev,eig,nin,ten))




fig <- final %>%
  plot_ly(
    x = ~xvals, 
    y = ~yvals,
    frame = ~its, 
    text = ~its,
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers',
    marker = list(size = 2.5, color = "black"))
fig
