
Sys.setenv(LANGUAGE='en')
# install.packages("schoolmath")
start_time <- Sys.time()

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

b<- c(1,62,71,38)#random choosing to match the combinations using int1 to 4

#end time of green time of east bound # for overlapping phases we will add the phase for that movement
a1<- b[1]+phasing$phase2[1]
a2<- b[2]+phasing$phase2[2]
a3<- b[3]+phasing$phase1[3]+phasing$phase2[3]
a33<-b[3]+phasing$phase2[3]
a4<- b[4]+phasing$phase2[4]
a44<-b[4]+phasing$phase1[4]+phasing$phase2[4]
a_e<- c(a1,a2,a3,a4)
a_w<- c(a1,a2,a33,a44)

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

#drawing bottom line.............................................
#finding the intersection pts for E bound
#int_1_E
res_e<- NULL
res_w<- NULL
b1_new <- b[4]+(speed_W*d[4])
intercept_e<- b[1]
intercept_w<- b1_new


for (i in 1:4){
      
  res_e[i]<- intersect_pts(-speed_E,as.numeric(b[1]),0,d[i])[2]

      if(res_e[i]>a_e[i]){
        res_e<- NULL
        break
      }
}
abline(a=intercept_e, b=speed_E, h=NULL, v=NULL, col="red", lwd = 2)

for (i in 1:4){
  
  res_w[i]<- intersect_pts(speed_W,as.numeric(b1_new),0,d[i])[2]
  
  if(res_w[i]>a_w[i]){
    res_w<- NULL
    break
  }
}
abline(a=intercept_w, b=-speed_W, h=NULL, v=NULL, col="red", lwd = 2)


#finding max value for east bound
    m<- max(b)
    p<- b==m
    p<- which(p)
    m_e<- min(p)
    m_w<- max(p)
    
    #finding the step needed or not
    res_en<- res_e
    res_wn<-res_w
    constraint_w_down<- res_wn-b
    constraint_e_down<- res_en-b
    step<- b[m_e]-res_en[m_e]
    step1<- b[m_w]-res_wn[m_w]
    
    
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
abline(a=intercept_e, b=speed_E, h=NULL, v=NULL, col="red", lwd = 2)
constraint_e_down<- res_en-b

#finding the neg location and then subtracting from the blimit
if(any(is.negative(constraint_e_down), na.rm=FALSE)){
  Negative <- max(which(constraint_e_down < 0))
  Negative<- constraint_e_down[Negative]
  step<- abs(Negative)
  # abline(a=intercept_e+step, b=speed_E, h=NULL, v=NULL, col="red", lwd = 2)
  for(i in 1:4){
    res_e[i]<- intersect_pts(-speed_E,as.numeric(intercept_e),step,d[i])[2]
    #find number of points below end of green split
  }
}
constraint_e_down<- as.numeric(a_e)-as.numeric(res_e)

    

#opposite direction
    if(step1<0){
      res_wn<-res_w
      constraint_w_down<- res_wn-b 
    }
    
    if(step1>0){
      intercept_w<- b1_new+step1
      
      for (i in 1:4){
        res_wn[i]<- intersect_pts(speed_W,as.numeric(intercept_w),0,d[i])[2]
        
        if(res_wn[i]>a_w[i]){
          res_wn<- NULL
          intercept_w<- b1_new
          stop5<- TRUE
          break
        }
      }}
    abline(a=intercept_w, b=-speed_W, h=NULL, v=NULL, col="red", lwd = 2)
    
    #finding the neg location and then subtracting from the blimit
    if(any(is.negative(constraint_w_down), na.rm=FALSE)){
      Negative <- max(which(constraint_w_down < 0))
      Negative<- constraint_w_down[Negative]
      step1<- abs(Negative)
      abline(a=intercept_w+step1, b=-speed_W, h=NULL, v=NULL, col="red", lwd = 2)
      for(i in 1:4){
        res_w[i]<- intersect_pts(speed_W,as.numeric(intercept_w),step1,d[i])[2]
        #find number of points below end of green split
      }
    }
    constraint_e_down<- as.numeric(a_e)-as.numeric(res_e)
    
   #drawing top line....................................
    m1<- min(as.numeric(a_e))
    p<- as.numeric(a_e)==m1
    p<- which(p)
    m1_e<- min(p)
    stepup_e<- abs(res_en[m1_e]-as.numeric(a_e[m1_e]))
    res_e_up<- NULL
    
    #drawing top line
    m2<- min(as.numeric(a_w))
    p2<- as.numeric(a_w)==m2
    p2<- which(p2)
    m2_w<- max(p2)
    stepup_w<- abs(res_wn[m2_w]-as.numeric(a_w[m2_w]))
    res_w_up<- NULL
    
    
    # abline(a=intercept_e+stepup_e, b=speed_E, h=NULL, v=NULL, col="red", lwd = 2)
    for(i in 1:4){
      res_e_up[i]<- intersect_pts(-speed_E,as.numeric(intercept_e),stepup_e,d[i])[2]
      res_w_up[i]<- intersect_pts(speed_W,as.numeric(intercept_w),stepup_w,d[i])[2]
      
      #find number of points below end of green split
    }
    constraint_e_up<- as.numeric(a_e)-as.numeric(res_e_up)
    constraint_w_up<- as.numeric(a_w)-as.numeric(res_w_up)
    
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
    abline(a=intercept_e+stepup_e, b=speed_E, h=NULL, v=NULL, col="red", lwd = 2)
    
    constraint_e_up<- as.numeric(a_e)-as.numeric(res_e_up)
    width_e<- stepup_e

     
    #finding negative location and subtracting it 
    if(any(is.negative(constraint_w_up), na.rm=FALSE)){
      Negative2 <- min(which(constraint_w_up < 0)) #changed from max to min
      Negative2<- constraint_w_up[Negative2]
      stepup_w<- stepup_w+Negative2
      abline(a=intercept_w+stepup_w, b=-speed_W, h=NULL, v=NULL, col="red", lwd = 2)
      for(i in 1:4){
        res_w_up[i]<- intersect_pts(speed_W,as.numeric(intercept_w),stepup_w,d[i])[2]
        #find number of points below end of green split
      }
    }
    constraint_w_up<- as.numeric(a_w)-as.numeric(res_w_up)
    
    
    if(any(is.negative(constraint_w_up), na.rm=FALSE)){
      Negative2 <- min(which(constraint_w_up < 0))
      Negative2<- constraint_w_up[Negative2]
      stepup_w<- stepup_w+Negative2
      abline(a=intercept_w+stepup_w, b=-speed_W, h=NULL, v=NULL, col="red", lwd = 2)
      for(i in 1:4){
        res_w_up[i]<- intersect_pts(speed_W,as.numeric(intercept_w),stepup_w,d[i])[2]
        #find number of points below end of green split
      }
    }
    constraint_w_up<- as.numeric(a_w)-as.numeric(res_w_up)
    width_w<- stepup_w
    
    if(any(is.negative(constraint_w_up), na.rm=FALSE)){
      Negative2 <- min(which(constraint_w_up < 0))
      Negative2<- constraint_w_up[Negative2]
      stepup_w<- stepup_w+Negative2
      abline(a=intercept_w+stepup_w, b=-speed_W, h=NULL, v=NULL, col="red", lwd = 2)
      for(i in 1:4){
        res_w_up[i]<- intersect_pts(speed_W,as.numeric(intercept_w),stepup_w,d[i])[2]
        #find number of points below end of green split
      }
    }
    constraint_w_up<- as.numeric(a_w)-as.numeric(res_w_up)
    width_w<- stepup_w
   abline(a=intercept_w+stepup_w, b=-speed_W, h=NULL, v=NULL, col="red", lwd = 2)



print(abs(res_e[1]-res_e_up[1]))
print(abs(res_w[1]-res_w_up[1]))

   
