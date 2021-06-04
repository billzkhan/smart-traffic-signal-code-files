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

AOG1e<- read.csv("D:/AOG1.csv")
AOG2e<- read.csv("D:/AOG2.csv")
AOG3e<- read.csv("D:/AOG3.csv")
AOG4e<- read.csv("D:/AOG4.csv")
AOG1e<- AOG1e$x
AOG2e<- AOG2e$x
AOG3e<- AOG3e$x
AOG4e<- AOG4e$x
AOG_e<- list(AOG1e,AOG2e,AOG3e,AOG4e)

AOG1w<- read.csv("D:/AOG1w.csv")
AOG2w<- read.csv("D:/AOG2w.csv")
AOG3w<- read.csv("D:/AOG3w.csv")
AOG4w<- read.csv("D:/AOG4w.csv")
AOG1w<- AOG1w$x
AOG2w<- AOG2w$x
AOG3w<- AOG3w$x
AOG4w<- AOG4w$x
AOG_w<- list(AOG1w,AOG2w,AOG3w,AOG4w)
T_AOG_e<- data.frame(unlist(lapply(AOG_e, function(x) length(x))))
T_AOG_w<- data.frame(unlist(lapply(AOG_w, function(x) length(x))))


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
# com_e<- com_e[40680000:40683104,]

int_w<- C-(a_w-b)
int1_w<- as.vector(1:int_w[1])
int2_w<- as.vector(1:int_w[2])
int3_w<- as.vector(1:int_w[3])
int4_w<- as.vector(1:int_w[4])
com_w<- expand.grid(int1_w,int2_w,int3_w,int4_w)
# com_w<- com_w[33110000:33110616,]

#to minimize the total number of iterations
com<- com_e[!(com_e$Var4>max(com_w$Var4)),]
com<- com[23680000:23688896,]


#function to find the intersection pts
intersect_pts <- function(speed,start_pt,width,dist) {
  A<- matrix(c(speed,1,1,0), nrow = 2, ncol = 2, byrow = TRUE)
  B<- matrix(c(start_pt+width,dist), nrow = 2, ncol = 1, byrow = TRUE)
  invA<- solve(A)
  int_pt<- invA %*% B
  int_pt<- as.vector(int_pt)
  return(int_pt)
}

#function to find the location of max AOG position and number of veh passing
green_adj_off<- function(a,b,AOG){
  vector10<-vector(mode = "numeric")
  # c=1
  while(!a>C-1){
    b<- b+1
    a<- a+1
    count_before<- sum(AOG > b & AOG < a)
    # vector10[c]<-count_before
    # c<- c+1
    vector10<- append(vector10,count_before)
    
  }
  # b_count<- list("place" = which.max(vector10), "Arrival_num" = max(vector10))
  return(vector10)  
}

aog_e<- NULL
aog_w<- NULL

for(i in 1:4){
  bb<- unlist(AOG_e[i])
  cc<- unlist(AOG_w[i])
  aog_e[i]<- list(green_adj_off(as.numeric(a_e[i]),b[i],bb))
  aog_w[i]<- list(green_adj_off(as.numeric(a_w[i]),b[i],cc))
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



my_vector_e1 <- vector(mode="numeric")
my_vector_e2 <- vector(mode="numeric")
my_vector_e3 <- vector(mode="numeric")
my_vector_e4 <- vector(mode="numeric")

# my_vector_w <- vector(mode="numeric")
count_step<-0

for (i in 1:nrow(com)){
  b <- com[i,]
  a1<- b[1]+phasing$phase2[1]
  a2<- b[2]+phasing$phase2[2]
  a3<- b[3]+phasing$phase1[3]+phasing$phase2[3]
  a4<- b[4]+phasing$phase2[4]
  a_e<- c(a1,a2,a3,a4)
  
  #drawing bottom line.............................................
  #finding the intersection pts for E bound
  #int_1_E
  res_e<- NULL
  stop1<- FALSE
  stop4<- FALSE
  
  
  for (i in 1:4){
    res_e[i]<- intersect_pts(-speed_E,as.numeric(b[1]),0,d[i])[2]
    if(res_e[i]>a_e[i]){
      res_e<- NULL
      stop1<- TRUE
      break
    }
  }
  print(com)
  try(
    if(stop1){
      next
    }
    , silent = TRUE)
  
  #finding max value for east bound
  m<- max(b)
  p<- b==m
  p<- which(p)
  m_e<- min(p)
  
  #finding the step needed or not
  res_en<- res_e
  constraint_e_down<- res_en-b
  intercept_e<- b[1]
  step<- b[m_e]-res_en[m_e]
  
  if(step<0){
    
    res_en<- res_e
    constraint_e_down<- res_en-b
  }
  if(step>0){
    
    intercept_e<- b[1]+step
    
    for (i in 1:4){
      res_en[i]<- intersect_pts(-speed_E,as.numeric(intercept_e),0,d[i])[2]
      
      if(res_en[i]>a_e[i]){
        res_en<- NULL
        intercept_e<- b[1]
        stop4<- TRUE
        break
      }
    }
  }
  
  try(
    if(stop4){
      next
    }
    , silent = TRUE)
  
  #bottom line drawn
  constraint_e_down<- res_en-b
  
  #drawing top line....................................
  
  m1<- min(as.numeric(a_e))
  p<- as.numeric(a_e)==m1
  p<- which(p)
  m1_e<- min(p)
  stepup_e<- abs(res_en[m1_e]-as.numeric(a_e[m1_e]))
  res_e_up<- NULL
  
  # abline(a=intercept_e+stepup_e, b=speed_E, h=NULL, v=NULL, col="red", lwd = 2)
  for(i in 1:4){
    res_e_up[i]<- intersect_pts(-speed_E,as.numeric(intercept_e),stepup_e,d[i])[2]
    #find number of points below end of green split
  }
  constraint_e_up<- as.numeric(a_e)-as.numeric(res_e_up)
  
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
  constraint_e_up<- as.numeric(a_e)-as.numeric(res_e_up)
  
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
  
  constraint_e_up<- as.numeric(a_e)-as.numeric(res_e_up)
  width_e<- stepup_e
  
  my_vector_e1 <- append(my_vector_e1, width_e)
  my_vector_e2 <- append(my_vector_e2, constraint_e_up)
  my_vector_e3 <- append(my_vector_e3, constraint_e_down)
  my_vector_e4 <- append(my_vector_e4, b)
  count_step<-count_step+1
  
}

constraint_e_up<-as.data.frame(matrix(my_vector_e2, ncol = 4,  byrow = TRUE), stringsAsFactors = FALSE)
constraint_e_down<-as.data.frame(matrix(my_vector_e3, ncol = 4,  byrow = TRUE), stringsAsFactors = FALSE)
combinations_e<-as.data.frame(matrix(my_vector_e4, ncol = 4,  byrow = TRUE), stringsAsFactors = FALSE)
my_vector_e1

output_e<- cbind.data.frame(combinations_e,my_vector_e1,constraint_e_down,constraint_e_up)
output_e<- output_e[,c(1:5)]
names(output_e)<- c("com1","com2","com3","com4", "BW_e")


#WEST BOUND NOW.........................................
my_vector_w1 <- vector(mode="numeric")
my_vector_w2 <- vector(mode="numeric")
my_vector_w3 <- vector(mode="numeric")
my_vector_w4 <- vector(mode="numeric")
# my_vector_w <- vector(mode="numeric")
count_step1<-0

for (i in 1:nrow(com)){
  b<-com[i,]
  a1<- b[1]+phasing$phase2[1]
  a2<- b[2]+phasing$phase2[2]
  a33<-b[3]+phasing$phase2[3]
  a44<-b[4]+phasing$phase1[4]+phasing$phase2[4]
  a_w<- c(a1,a2,a33,a44)
  
  #finding the max for west bound.................................................................UP and DOWN
  #finding the intersection pts for W bound................
  b1_new <- b[4]+(speed_W*d[4])
  res_w<- NULL
  stop2<- FALSE
  stop5<- FALSE
  
  for (i in 1:4){
    res_w[i]<- intersect_pts(speed_W,as.numeric(b1_new),0,d[i])[2]
    if(res_w[i]>a_w[i]){
      res_w<- NULL
      stop2<- TRUE
      break
    }
  }
  print(com)
  
  try(
    if(stop2){
      next
    }
    , silent = TRUE)
  
  #finding max start point of all intersections
  m<- max(b)
  p<- b==m
  p<- which(p)
  m_w<- max(p)
  
  res_wn<-res_w
  constraint_w_down<- res_wn-b
  intercept_w<- b1_new
  step<- b[m_w]-res_wn[m_w]
  
  if(step<0){
    res_wn<-res_w
    constraint_w_down<- res_wn-b 
  }
  
  if(step>0){
    intercept_w<- b1_new+step
    
    for (i in 1:4){
      res_wn[i]<- intersect_pts(speed_W,as.numeric(intercept_w),0,d[i])[2]
      
      if(res_wn[i]>a_w[i]){
        res_wn<- NULL
        intercept_w<- b1_new
        stop5<- TRUE
        break
      }
    }}
  
  try(
    if(stop5){
      next
    }, silent = TRUE)
  
  #bottom line drawn
  constraint_w_down<- res_wn-b
  
  #drawing top line
  m1<- min(as.numeric(a_w))
  p<- as.numeric(a_w)==m1
  p<- which(p)
  m1_w<- max(p)
  stepup_w<- abs(res_wn[m1_w]-as.numeric(a_w[m1_w]))
  res_w_up<- NULL
  
  for(i in 1:4){
    res_w_up[i]<- intersect_pts(speed_W,as.numeric(intercept_w),stepup_w,d[i])[2]
  }
  
  constraint_w_up<- as.numeric(a_w)-as.numeric(res_w_up)
  
  #finding negative location and subtracting it 
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
  constraint_w_up<- as.numeric(a_w)-as.numeric(res_w_up)
  
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
  
  constraint_w_up<- as.numeric(a_w)-as.numeric(res_w_up)
  width_w<- stepup_w
  
  print(constraint_w_down)
  my_vector_w1 <- append(my_vector_w1, width_w)
  my_vector_w2 <- append(my_vector_w2, constraint_w_up)
  my_vector_w3 <- append(my_vector_w3, constraint_w_down)
  my_vector_w4 <- append(my_vector_w4, b)
  count_step1<-count_step1+1
}

constraint_w_up<-as.data.frame(matrix(my_vector_w2, ncol = 4,  byrow = TRUE), stringsAsFactors = FALSE)
constraint_w_down<-as.data.frame(matrix(my_vector_w3, ncol = 4,  byrow = TRUE), stringsAsFactors = FALSE)
combinations_w<-as.data.frame(matrix(my_vector_w4, ncol = 4,  byrow = TRUE), stringsAsFactors = FALSE)
my_vector_w1

output_w<- cbind.data.frame(combinations_w,my_vector_w1,constraint_w_down,constraint_w_up)
output_w<- output_w[,c(1:5)]
names(output_w)<- c("com1","com2","com3","com4", "BW_w")

#length of output might be different for both outputs, therefore checking the length and finding the common combinations again
# output_w<- output_w[!(output_w$V1>max(unlist(output_e$com1))),]
output<- merge.data.frame(output_e,output_w, by = c("com1","com2","com3","com4"))
output <- apply(output,2,as.character)


aog_e1<- as.data.frame(unlist(aog_e[1]))
aog_e2<- as.data.frame(unlist(aog_e[[2]]))
aog_e3<- as.data.frame(unlist(aog_e[[3]]))
aog_e4<- as.data.frame(unlist(aog_e[[4]]))

aog_e1$com1<- 1:length(aog_e[[1]])
aog_e2$com2<- 1:length(aog_e[[2]])
aog_e3$com3<- 1:length(aog_e[[3]])
aog_e4$com4<- 1:length(aog_e[[4]])

output2<- merge(x = output, y = aog_e1, by = "com1", all.x = TRUE)
output2<- merge(x = output2, y = aog_e2, by = "com2", all.x = TRUE)
output2<- merge(x = output2, y = aog_e3, by = "com3", all.x = TRUE)
output2<- merge(x = output2, y = aog_e4, by = "com4", all.x = TRUE)

aog_w1<- as.data.frame(unlist(aog_w[[1]]))
aog_w2<- as.data.frame(unlist(aog_w[[2]]))
aog_w3<- as.data.frame(unlist(aog_w[[3]]))
aog_w4<- as.data.frame(unlist(aog_w[[4]]))

aog_w1$com1<- 1:length(aog_w[[1]])
aog_w2$com2<- 1:length(aog_w[[2]])
aog_w3$com3<- 1:length(aog_w[[3]])
aog_w4$com4<- 1:length(aog_w[[4]])


output2<- merge(x = output2, y = aog_w1, by = "com1", all.x = TRUE)
output2<- merge(x = output2, y = aog_w2, by = "com2", all.x = TRUE)
output2<- merge(x = output2, y = aog_w3, by = "com3", all.x = TRUE)
output2<- merge(x = output2, y = aog_w4, by = "com4", all.x = TRUE)

output2[, 5:14] <- sapply(output2[, 5:14], as.numeric)
output2$BW_max<- rowSums(output2[,c(5:6)])

# output2$aog_rate_e1<- (output2$`unlist(aog_e[1])`/30)*100
# output2$aog_rate_e2<- (output2$`unlist(aog_e[[2]])`/20)*100
# output2$aog_rate_e3<- (output2$`unlist(aog_e[[3]])`/10)*100
# output2$aog_rate_e4<- (output2$`unlist(aog_e[[4]])`/5)*100
# 
# output2$aog_rate_w1<- (output2$`unlist(aog_w[[1]])`/30)*100
# output2$aog_rate_w2<- (output2$`unlist(aog_w[[2]])`/30)*100
# output2$aog_rate_w3<- (output2$`unlist(aog_w[[3]])`/30)*100
# output2$aog_rate_w4<- (output2$`unlist(aog_w[[4]])`/30)*100

output2$AOG_max<- (rowSums(output2[,c(7:14)])/(30+20+10+5+30+30+30+30))*100
output2$AOG_emax<- (rowSums(output2[,c(7:10)])/(30+20+10+5))*100
output2$AOG_wmax<- (rowSums(output2[,c(11:14)])/(30+30+30+30))*100

plot(output2$BW_max,output2$AOG_max)
plot(output2$BW_e,output2$AOG_emax)
plot(output2$BW_w,output2$AOG_wmax)

#for optimization 
#objective function
#select a and b based on engineer knowledge based on importance we want to give to each, we can also do it separate for all intersections

# sol<- a*AOG_max + b*BW_max

#constraints to be defined as well


# #for normalizing the output values
# mean_BW<-mean(output2$BW_max)
# sd_BW<-sd(output2$BW_max)
# mean_AOG<-mean(output2$AOG_max)
# sd_AOG<-sd(output2$AOG_max)
# max_max<- max(output2$max)
# T_F<- max_max==output2$max
# konsi_value<- which(T_F)
# dat2 <- output2 %>% mutate_at(c("BW_max", "AOG_max"), ~(scale(.) %>% as.vector))
# plot(dat2$BW_max,dat2$AOG_max)




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



