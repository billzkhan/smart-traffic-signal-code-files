Sys.setenv(LANGUAGE='en')
# install.packages("schoolmath")
library(schoolmath)
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


#total cycle length
C<-  rowSums(phasing[1,4:8], na.rm = TRUE)

#setting speed
speed_E<- 0.05
speed_W<- 0.05

#distance between intersections
d1<- 0
d2<- 191
d3<- 628
d4<- 1200
d<- c(0,190,628,1200)

#setting offsets#change values based on the slack time calculated
# ref_val<- 174
# off1_val<- 174
# off2_val<- 160
# off3_val<- 190
# off4_val<- 156
# 
# off1<- ref_val-off1_val
# off2<- off2_val-off1_val
# off3<- off3_val-off2_val #offset value can not be moved up or down more than the slack time calculated
# off4<- off4_val-off3_val
# 
# #start times green of eAST BOUND approach
# b1<- phasing$phase1[1]+off1
# b2<- b1+off2
# b3<- b2+off3
# b4<- b3+off4
# b<- c(phasing$phase1[1]+off1,b1+off2,b2+off3,b3+off4)
b<- c(0,0,0,0)#random choosing to match the combinations using int1 to 4

#end time of green time of east bound # for overlapping phases we will add the phase for that movement
a1<- b[1]+phasing$phase2[1]
a2<- b[2]+phasing$phase2[2]
a3<- b[3]+phasing$phase1[3]+phasing$phase2[3]
a33<-b[3]+phasing$phase2[3]
a4<- b[4]+phasing$phase2[4]
a44<-b[4]+phasing$phase1[4]+phasing$phase2[4]
a_e<- c(a1,a2,a3,a4)
a_w<- c(a1,a2,a33,a44)

#combinations of all green split movements over the cycle length
int_e<- C-(a_e-b)
int1_e<- as.vector(1:int_e[1])
int2_e<- as.vector(1:int_e[2])
int3_e<- as.vector(1:int_e[3])
int4_e<- as.vector(1:int_e[4])
com_e<- expand.grid(int1_e,int2_e,int3_e,int4_e)
com_e<- com_e[1:50,]

int_w<- C-(a_w-b)
int1_w<- as.vector(1:int_w[1])
int2_w<- as.vector(1:int_w[2])
int3_w<- as.vector(1:int_w[3])
int4_w<- as.vector(1:int_w[4])
com_w<- expand.grid(int1_w,int2_w,int3_w,int4_w)
com_w<- com_w[1:3,]


# com$Var12<- com$Var1-com$Var2
# com$Var13<- com$Var1-com$Var3
# com$Var14<- com$Var1-com$Var4
# h_e<- NULL
# for (i in 1:4){
#   h_e[i]<- speed_E*d[i]
# }
# com_new <- subset(com, (Var12 >= h_e[2])&(Var13 >= abs(h_e[3]))&(Var14 >= abs(h_e[4])))



# my_vector_e <- vector(mode="numeric")
# my_vector_w <- vector(mode="numeric")

# for(i in 1:3) {
#   b<- com[i,]
# 
#   my_vector_e <- append(my_vector_e, width_e)
#   my_vector_w <- append(my_vector_w, width_w)
#   
# }



#function to find the intersection pts
intersect_pts <- function(speed,start_pt,width,dist) {
  A<- matrix(c(speed,1,1,0), nrow = 2, ncol = 2, byrow = TRUE)
  B<- matrix(c(start_pt+width,dist), nrow = 2, ncol = 1, byrow = TRUE)
  invA<- solve(A)
  int_pt<- invA %*% B
  int_pt<- as.vector(int_pt)
  return(int_pt)
}

#make empty plot
#y axis pts. on each intersection to make plot
h1 <- c(0,b[1],a_e[1],C)
h2 <- c(0,b[2],a_e[2],C)
h3 <- c(0,b[3],a_e[3],C)
h31<- c(0,b[3],a_w[3],C)
h4 <- c(0,b[4],a_e[4],C)
h41<- c(0,b[4],a_w[4],C)

#x axis pts on each intersection to make plots
x <- c(d[1],d[2],d[3],d[4])
x4<-rep(d[4],4) 
x3<-rep(d[3],4)   
x2<-rep(d[2],4) 
x1<-rep(d[1],4)   

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

#making bottom line of bandwidth of both bounds...............UP AND DOWN....................................
#finding the max for east bound
m<- max(b)
p<- b==m
p<- which(p)
m_e<- min(p)
#finding the intersection pts for E bound
#int_1_E
res_e<- NULL
for (i in 1:4){
  res_e[i]<- intersect_pts(-speed_E,as.numeric(b[1]),0,d[i])[2]
  if(res_e[i]>a_e[i]){
    stop1<- TRUE
    res_e<- NULL
    print("skip this combination, line_e_down going out of limit")
  }else{
    res_en<- res_e
  }
}

try(
  if(stop1){
    next
  }
  , silent = TRUE)



intercept_e<- b[1]
abline(a=intercept_e, b=speed_E, h=NULL, v=NULL, col="red", lwd = 2)

step<- b[m_e]-res_en[m_e]

if(step!=0){
  intercept_e<- b[1]+step
  for (i in 1:4){
    res_en[i]<- intersect_pts(-speed_E,as.numeric(intercept_e),0,d[i])[2]
    if(res_en[i]<b[i]){
      res_en<- res_e
      intercept_e<- b[1]
      print("line going out of below limit, thus using first line as actual line")
    }
    if(res_en[i]>a_e[i]){
      res_en<- NULL
      intercept_e<- b[1]
      print("line going out of above limit, thus dont consider the abline drawn")
    }
    
  }
  constraint_e_down<- res_en-b
  abline(a=intercept_e, b=speed_E, h=NULL, v=NULL, col="red", lwd = 2)
} 


#making bottom line of bandwidth of both bounds..................................................
#finding the max for east bound
m1<- min(a_e)
p<- a_e==m1
p<- which(p)
m1_e<- min(p)
stepup_e<- abs(res_en[m1_e]-a_e[m1_e])
abline(a=intercept_e+stepup_e, b=speed_E, h=NULL, v=NULL, col="red", lwd = 2)
res_e_up<- NULL
for(i in 1:4){
  res_e_up[i]<- intersect_pts(-speed_E,as.numeric(intercept_e),stepup_e,d[i])[2]
  #find number of points below end of green split
}
constraint_e_up<- a_e-res_e_up

#finding the neg location and then subtracting from the blimit
if(any(is.negative(constraint_e_up), na.rm=FALSE)){
  Negative <- max(which(constraint_e_up < 0))
  Negative<- constraint_e_up[Negative]
  stepup_e<- stepup_e+Negative
  abline(a=intercept_e+stepup_e, b=speed_E, h=NULL, v=NULL, col="red", lwd = 2)
  for(i in 1:4){
    res_e_up[i]<- intersect_pts(-speed_E,as.numeric(intercept_e),stepup_e,d[i])[2]
    #find number of points below end of green split
  }
}
constraint_e_up<- a_e-res_e_up

#finding the neg location and then subtracting from the blimit
if(any(is.negative(constraint_e_up), na.rm=FALSE)){
  Negative <- min(which(constraint_e_up < 0))
  Negative<- constraint_e_up[Negative]
  stepup_e<- stepup_e+Negative
  abline(a=intercept_e+stepup_e, b=speed_E, h=NULL, v=NULL, col="red", lwd = 2)
  for(i in 1:4){
    res_e_up[i]<- intersect_pts(-speed_E,as.numeric(intercept_e),stepup_e,d[i])[2]
    #find number of points below end of green split
  }
}

width_e<- stepup_e

#finding the max for west bound.................................................................UP and DOWN
m<- max(b)
p<- b==m
p<- which(p)
m_w<- max(p)

#finding the intersection pts for W bound................
b1_new <- b[4]+(speed_W*d[4])
res_w<- NULL
for (i in 1:4){
  res_w[i]<- intersect_pts(speed_W,as.numeric(b1_new),0,d[i])[2]
  if(res_w[i]>a_w[i]){
    res_w<- NULL
    print("line going out of limit")
  }
}
res_wn<-res_w
constraint_w_down<- res_wn-b
abline(a=b1_new, b=-speed_W, h=NULL, v=NULL, col="red", lwd = 2)
intercept_w<- b1_new
step<- b[m_w]-res_w[m_w]

if(step!=0){
  intercept_w<- b1_new+step
  res_wn<- NULL
  for (i in 1:4){
    res_wn[i]<- intersect_pts(speed_W,as.numeric(intercept_w),0,d[i])[2]
    if(res_wn[i]<b[i]){
      res_wn<- res_w
      intercept_w<- b1_new
      print("line going out of below limit, thus using first line as actual line")
    }
    if(res_wn[i]>a_e[i]){
      res_wn<- NULL
      intercept_w<- b1_new
      print("line going out of above limit, thus dont consider the abline drawn")
    }
  }
  abline(a=intercept_w, b=-speed_W, h=NULL, v=NULL, col="red", lwd = 2)
  constraint_w_down<- res_wn-b
}

#making top line of bandwidth of east bound...................................................
#making bottom line of bandwidth of both bounds...................................................
#finding the max for west bound
m1<- min(a_w)
p<- a_w==m1
p<- which(p)
m1_w<- max(p)
stepup_w<- abs(res_wn[m1_w]-a_w[m1_w])
res_w_up<- NULL
for(i in 1:4){
  res_w_up[i]<- intersect_pts(speed_W,as.numeric(intercept_w),stepup_w,d[i])[2]
}
constraint_w_up<- a_w-res_w_up
abline(a=intercept_w+stepup_w, b=-speed_W, h=NULL, v=NULL, col="red", lwd = 2)

if(any(is.negative(constraint_w_up), na.rm=FALSE)){
  Negative <- max(which(constraint_w_up < 0))
  Negative<- constraint_w_up[Negative]
  stepup_w<- stepup_w+Negative
  abline(a=intercept_w+stepup_w, b=-speed_W, h=NULL, v=NULL, col="red", lwd = 2)
  for(i in 1:4){
    res_w_up[i]<- intersect_pts(speed_W,as.numeric(intercept_w),stepup_w,d[i])[2]
    #find number of points below end of green split
  }
}
constraint_w_up<- a_w-res_w_up

if(any(is.negative(constraint_w_up), na.rm=FALSE)){
  Negative <- min(which(constraint_w_up < 0))
  Negative<- constraint_w_up[Negative]
  stepup_w<- stepup_w+Negative
  abline(a=intercept_w+stepup_w, b=-speed_W, h=NULL, v=NULL, col="red", lwd = 2)
  for(i in 1:4){
    res_w_up[i]<- intersect_pts(speed_W,as.numeric(intercept_w),stepup_w,d[i])[2]
    #find number of points below end of green split
  }
}
constraint_w_up<- a_w-res_w_up
width_w<- stepup_w


#step1....phase wise it is different
through<- 2000
rt_lane<- 1800
lt_lane<- 1800

#step2....V/S is critical flow ratio is sum of all V/s for critical flows in case of non-overlapping...bigger of the movements is used for sum
#intersecton_1
#EB_L
#WB_L
#EB_TR



#step3....g/c green split ratio of that phase


#step4....


