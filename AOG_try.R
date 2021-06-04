library(readxl)
library(dplyr)
phasing  <- read_excel("D:/BILAL/smart traffic signal system/신호패턴_TOD_최종.xlsx")
phasing<- phasing[phasing$교차로명 %in% c("서대전나들목삼거리","서일고교삼거리","진잠네거리","구봉중삼거리")
                  & phasing$요일 %in% "주중" & phasing$시간 %in% "17:00~21:00",]

phasing$phase1<- phasing$`1현시-녹색`+phasing$`1현시-황색`
phasing$phase2<- phasing$`2현시-녹색`+phasing$`2현시-황색`
phasing$phase3<- phasing$`3현시-녹색`+phasing$`3현시-황색`
phasing$phase4<- phasing$`4현시-녹색`+phasing$`4현시-황색`
phasing$phase5<- phasing$`5현시-녹색`+phasing$`5현시-황색`
phasing<- phasing[, (colnames(phasing) %in% c("교차로명","요일","시간","phase1", "phase2", "phase3","phase4","phase5"))]

# #intersection random arrival
# limit <- 180
# times1<- 30
# times2<- 20
# times3<- 10
# times4<- 5
# 
# times1w<- 30
# times2w<- 30
# times3w<- 30
# times4w<- 30

#distance between intersections
d1<- 0
d2<- 191
d3<- 628
d4<- 1200
d<- c(0,190,628,1200)

#start times green of eAST BOUND approach
b1<- 0#phasing$phase1[1]+off1
b2<- 0#b1+off2
b3<- 0#b2+off3
b4<- 0#b3+off4
b11<- 0
b22<- 0
b33<- 0
b44<- 0

#generating the arrival vehs for east bound
# AOG1<- sample.int(limit,times1, replace = FALSE)
# AOG2<- sample.int(limit,times2, replace = FALSE)
# AOG3<- sample.int(limit,times3, replace = FALSE)
# AOG4<- sample.int(limit,times4, replace = FALSE)
AOG1<- read.csv("D:/AOG1.csv")
AOG1<- AOG1$x
AOG2<- read.csv("D:/AOG2.csv")
AOG2<- AOG2$x
AOG3<- read.csv("D:/AOG3.csv")
AOG3<- AOG3$x
AOG4<- read.csv("D:/AOG4.csv")
AOG4<- AOG4$x

#generating the arrival vehs for west bound
# AOG1w<- sample.int(limit,times1w, replace = TRUE)
# AOG2w<- sample.int(limit,times2w, replace = TRUE)
# AOG3w<- sample.int(limit,times3w, replace = TRUE)
# AOG4w<- sample.int(limit,times4w, replace = TRUE)
AOG1w<- read.csv("D:/AOG1w.csv")
AOG2w<- read.csv("D:/AOG2w.csv")
AOG3w<- read.csv("D:/AOG3w.csv")
AOG4w<- read.csv("D:/AOG4w.csv")
AOG1w<- AOG1w$x
AOG2w<- AOG2w$x
AOG3w<- AOG3w$x
AOG4w<- AOG4w$x

#total cycle length
C<-  rowSums(phasing[1,4:8], na.rm = TRUE)
#setting speed
speed_E<- 0.05
speed_W<- 0.05
#setting offsets#change values based on the slack time calculated
ref_val<- 174
off1_val<- 174
off2_val<- 174
off3_val<- 159
off4_val<- 156

off1<- ref_val-off1_val
off2<- off2_val-off1_val
off3<- off3_val-off2_val #offset value can not be moved up or down more than the slack time calculated
off4<- off4_val-off3_val

#function to find the difference
distance <- function(intersect_pt,bottom_pt) {
  return(intersect_pt[2]-bottom_pt[2])
}

#function to find the intersection pts
intersect_pts <- function(speed,start_pt,width_E,dist) {
  A<- matrix(c(speed,1,1,0), nrow = 2, ncol = 2, byrow = TRUE)
  B<- matrix(c(start_pt+width,dist), nrow = 2, ncol = 1, byrow = TRUE)
  invA<- solve(A)
  int_pt<- invA %*% B
  int_pt<- as.vector(int_pt)
  return(int_pt)
}

#function to find the location of max AOG position and number of veh passing
green_adj_off<- function(a,b,AOG){
  vector10<-NULL
  c=1
  while(!a>C-1){
    b<- b+1
    a<- a+1
    count_before<- sum(AOG > b & AOG < a)
    vector10[c]<-count_before
    c<- c+1
  }
  b_count<- list("place" = which.max(vector10), "Arrival_num" = max(vector10))
  return(b_count)  
}

#end time of green time of east bound # for overlapping phases we will add the phase for that movement
a1<- b1+phasing$phase2[1]
a11<- b11+phasing$phase2[1]
a2<- b2+phasing$phase2[2]
a22<- b22+phasing$phase2[2]
a3<- b3+phasing$phase1[3]+phasing$phase2[3]
a33<-b33+phasing$phase2[3]
a4<- b4+phasing$phase2[4]
a44<-b44+phasing$phase1[4]+phasing$phase2[4]


#finding the best starting for max AOG for east bound
int1e<- green_adj_off(a1,b1,AOG1)
b1<- as.vector(int1e$place[1])
int2e<- green_adj_off(a2,b2,AOG2)
b2<- as.vector(int2e$place[1])
int3e<- green_adj_off(a3,b3,AOG3)
b3<- as.vector(int3e$place[1])
int4e<- green_adj_off(a4,b4,AOG4)
b4<- as.vector(int4e$place[1])

#finding the best starting for max AOG for west bound
int4w<- green_adj_off(a11,b11,AOG1w)
b11<- as.vector(int1e$place[1])
int3w<- green_adj_off(a22,b22,AOG2w)
b22<- as.vector(int2e$place[1])
int2w<- green_adj_off(a33,b33,AOG3w)
b33<- as.vector(int3e$place[1])
int1w<- green_adj_off(a44,b44,AOG4w)
b44<- as.vector(int4e$place[1])


#end time of green time of east bound # for overlapping phases we will add the phase for that movement
a1<- b1+phasing$phase2[1]
a11<- b11+phasing$phase2[1]
a2<- b2+phasing$phase2[2]
a22<- b22+phasing$phase2[2]
a3<- b3+phasing$phase1[3]+phasing$phase2[3]
a33<-b33+phasing$phase2[3]
a4<- b4+phasing$phase2[4]
a44<-b44+phasing$phase1[4]+phasing$phase2[4]

#plotting changed plans..................................................................

#y axis pts. on each intersection to make plot
h1 <- c(0,b1,a1,C)
h2 <- c(0,b2,a2,C)
h3 <- c(0,b3,a3,C)
h31<- c(0,b3,a33,C)
h4 <- c(0,b4,a4,C)
h41<- c(0,b4,a44,C)

#x axis pts on each intersection to make plots
x <- c(d1,d2,d3,d4)
x4<-rep(d4,4) 
x3<-rep(d3,4)   
x2<-rep(d2,4) 
x1<-rep(d1,4)   

#making empty plot
plot(1, type = "n",xlab = "space_m", ylab = "time_sec", xlim = c(0, d4), ylim = c(0, C))
#making intersection vertical lines for plotting
abline(a=NULL, b=NULL, h=NULL, v=x, col="black", lwd = 3)

#placing pts of phases of intersection
points(x4, h4, col="blue", pch=18, cex=1.4)
points(x4, h41, col="red", pch=21, cex=1.4)
points(x3, h3, col="blue", pch=18, cex=1.4)
points(x3, h31, col="red", pch=21, cex=1.4)
points(x2, h2, col="blue", pch=18, cex=1.4)
points(x1, h1, col="blue", pch=18, cex=1.4)

#plotting the AOGs for E bound and W bound travelers
points(rep(d1,times1), AOG1, col="yellow", pch=18, cex=0.8)
points(rep(d2,times2), AOG2, col="yellow", pch=18, cex=0.8)
points(rep(d3,times3), AOG3, col="yellow", pch=18, cex=0.8)
points(rep(d4,times4), AOG4, col="yellow", pch=18, cex=0.8)
# 
points(rep(d1,times1w), AOG1w, col="red", pch=4, cex=0.8)
points(rep(d2,times2w), AOG2w, col="red", pch=4, cex=0.8)
points(rep(d3,times3w), AOG3w, col="red", pch=4, cex=0.8)
points(rep(d4,times4w), AOG4w, col="red", pch=4, cex=0.8)

#making a slope for speed_E
abline(a=b1, b=speed_E, h=NULL, v=NULL, col="red", lwd = 2)

#making slope for speed_W
b1_new <- b4+(speed_W*d4)
abline(a=b1_new, b=-speed_W, h=NULL, v=NULL, col="red", lwd = 2)


res1_v<-NULL
res2_v<-NULL
res3_v<-NULL
res4_v<-NULL
constraint1_v<- NULL
constraint2_v<- NULL
constraint3_v<- NULL
constraint4_v<- NULL
width_E_v<- NULL
r=0
b1=0
a1<- b1+phasing$phase2[1]
a11<- b11+phasing$phase2[1]
a2<- b2+phasing$phase2[2]
a22<- b22+phasing$phase2[2]
a3<- b3+phasing$phase1[3]+phasing$phase2[3]
a33<-b33+phasing$phase2[3]
a4<- b4+phasing$phase2[4]
a44<-b44+phasing$phase1[4]+phasing$phase2[4]

while(!a1>C-1){
  b1<- b1+1
  a1<- a1+1
  r<-r+1
  res1<- intersect_pts(-speed_E,b1,0,d1)[2]
  res2<- intersect_pts(-speed_E,b1,0,d2)[2]
  res3<- intersect_pts(-speed_E,b1,0,d3)[2]
  res4<- intersect_pts(-speed_E,b1,0,d4)[2]
  width_E = 0
  while((res_e1<a1)&(res_e2<a2)&(res_e3<a3)&(res_e4<a4)){
    
    res_e1<- res1
    res_e2<- res2
    res_e3<- res3
    res_e4<- res4
    res_e1<- intersect_pts(-speed_E,b1,width_E,d1)
    res_e2<- intersect_pts(-speed_E,b1,width_E,d2)
    res_e3<- intersect_pts(-speed_E,b1,width_E,d3)
    res_e4<- intersect_pts(-speed_E,b1,width_E,d4)
    width_E = width_E+1
    
  }
  width_E_v[r]<- width_E
  constraint1<- abs(res1-b1)
  constraint2<- abs(res2-b2)
  constraint3<- abs(res3-b3)
  constraint4<- abs(res4-b4)
  res1_v[r]<- res1
  res2_v[r]<- res2
  res3_v[r]<- res3
  res4_v[r]<- res4
  constraint1_v[r]<- constraint1
  constraint2_v[r]<- constraint2
  constraint3_v[r]<- constraint3
  constraint4_v[r]<- constraint4

}

bilal<- cbind(res1_v,res2_v,res3_v,res4_v,constraint1_v, constraint2_v, constraint3_v, constraint4_v, width_E_v)
# billz<- list("place" = which.max(vector1), "Arrival_num" = max(vector1))

#finding the intersection pts for E bound
#int_1_E
res1<- intersect_pts(-speed_E,b1,0,d1)
res2<- intersect_pts(-speed_E,b1,0,d2)
res3<- intersect_pts(-speed_E,b1,0,d3)
res4<- intersect_pts(-speed_E,b1,0,d4)

#constraints towards E bound
constraint1<- abs(res1[2]-b1)
constraint2<- abs(res2[2]-b2)
constraint3<- abs(res3[2]-b3)
constraint4<- abs(res4[2]-b4)

#finding the width of the maximized line
# width_E<- abs(res4[2]-a4)
width_E = 0
res_e1<- res1
res_e2<- res2
res_e3<- res3
res_e4<- res4

while((res_e1[2]<a1)&(res_e2[2]<a2)&(res_e3[2]<a3)&(res_e4[2]<a4)){
  width_E = width_E+1
  res_e1<- intersect_pts(-speed_E,b1,width_E,d1)
  res_e2<- intersect_pts(-speed_E,b1,width_E,d2)
  res_e3<- intersect_pts(-speed_E,b1,width_E,d3)
  res_e4<- intersect_pts(-speed_E,b1,width_E,d4)
}

print(width_E)

abline(a=b1+width_E, b=speed_E, h=NULL, v=NULL, col="red", lwd = 2)


#finding the intersection pts for W bound................
res11<- intersect_pts(speed_W,b1_new,0,d1)
res22<- intersect_pts(speed_W,b1_new,0,d2)
res33<- intersect_pts(speed_W,b1_new,0,d3)
res44<- intersect_pts(speed_W,b1_new,0,d4)

#constraints towards W bound traffic
constraint11<- abs(res11[2]-b1)
constraint22<- abs(res22[2]-b2)
constraint33<- abs(res33[2]-b3)
constraint44<- abs(res44[2]-b4)

#finding the width of the maximized line
width_W = 0
res_w1<- res11
res_w2<- res22
res_w3<- res33
res_w4<- res44

while((res_w1[2]<a44)&(res_w2[2]<a33)&(res_w3[2]<a2)&(res_w4[2]<a1)){
  width_W = width_W+1
  res_w1<- intersect_pts(speed_W,b1_new,width_W,d4)
  res_w2<- intersect_pts(speed_W,b1_new,width_W,d3)
  res_w3<- intersect_pts(speed_W,b1_new,width_W,d2)
  res_w4<- intersect_pts(speed_W,b1_new,width_W,d1)
}

print(width_W)
abline(a=b1_new+width_W, b=-speed_W, h=NULL, v=NULL, col="red", lwd = 2)

#finding the TOtal constraint of each intersection........................................
#first_constraint is distance from intersection of opposite line to end of phase 2
bottom_constraint_1<- min(constraint1,constraint11)
bottom_constraint_2<- min(constraint2,constraint22)
bottom_constraint_3<- min(constraint3,constraint33)
bottom_constraint_4<- min(constraint4,constraint44)

#for same second phase timings of W and E bound
top_constraint_1<- min(constraint1,constraint11)

constraint_1<- abs(res_w4[2]-a1)+constraint1

#Last_constraint is distance from intersection of opposite line to end of phase 2
if(a44>a4){
  constraint_e4<- abs(res_e4[2]-a44)
  constraint_4<- constraint44+constraint_e4
}else{
  constraint_e4<- abs(res_e4[2]-a4)
  constraint_4<- constraint44+constraint_e4
}
#second constraint
if(res_w3[2]>res_e2[2]){
  constraint_w3<- abs(res_w3[2]-a2)
  constraint_2<- constraint2+constraint_w3
}else{
  constraint_w3<- abs(res_e2[2]-a2)
  constraint_2<- constraint2+constraint_w3
}
#third constraint
if(res_w2[2]>res_e3[2]){
  constraint_w2<- abs(res_w2[2]-a3)
  constraint_3<- constraint3+constraint_w2
}else{
  constraint_w2<- abs(res_e3[2]-a3)
  constraint_3<- constraint3+constraint_w2
}





