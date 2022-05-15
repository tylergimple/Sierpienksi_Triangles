library(rgl)

# X values
corn_A<-0
corn_B<-50
corn_C<-100
corn_Z<-50
start_x<-20
start_y<-20
start_z<-20
its <- 50000
new_point<-((start_x+corn_C)/2)
y <- as.numeric(round(runif(its, min = 1, max = 4),0))
z<-as.data.frame(capture.output(for (val in y) {
  if(val == 3)
    new_point = ((new_point+corn_C)/2)
  if(val == 2)
    new_point = ((new_point+corn_B)/2)
  if(val == 1)
    new_point = ((new_point+corn_A)/2)
  if(val == 4)
    new_point = ((new_point+corn_Z)/2)
  print(new_point)
}))
  
  colnames(z) <- c("xvals")
  cleanz = as.data.frame(as.numeric(substring(z$xvals,5)))
  colnames(cleanz)<- c("xvals")

# Y values
corn_A<-0
corn_B<-50
corn_C<-0
corn_Z<-25
new_point<-((start_x+corn_C)/2)

p<-as.data.frame(capture.output(for (val in y) {
  if(val == 3)
    new_point = ((new_point+corn_C)/2)
  if(val == 2)
    new_point = ((new_point+corn_B)/2)
  if(val == 1)
    new_point = ((new_point+corn_A)/2)
  if(val == 4)
    new_point = ((new_point+corn_Z)/2)
  print(new_point)
}))
colnames(p) <- c("yvals")
cleanp<-as.data.frame(as.numeric(substring(p$yvals,5)))
colnames(cleanp) <- c("yvals")

#z values
corn_A<-0
corn_B<-0
corn_C<-0
corn_Z<-50
new_point<-((start_x+corn_C)/2)

q<-as.data.frame(capture.output(for (val in y) {
  if(val == 3)
    new_point = ((new_point+corn_C)/2)
  if(val == 2)
    new_point = ((new_point+corn_B)/2)
  if(val == 1)
    new_point = ((new_point+corn_A)/2)
  if(val == 4)
    new_point = ((new_point+corn_Z)/2)
  print(new_point)
}))
colnames(q) <- c("zvals")
cleanq<-as.data.frame(as.numeric(substring(q$zvals,5)))
colnames(cleanq) <- c("zvals")

graph<-as.data.frame(cbind(cleanz,cleanp,cleanq))
colnames(graph) <- c("xvals", "yvals","zvals")

plot3d( 
  x=graph$xvals, y=graph$yvals, z=graph$zvals, 
  type = 's', 
  radius = .1)
