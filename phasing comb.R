memory.limit()
memory.limit(size=500000000)

Sys.setenv(LANGUAGE='en')
# install.packages("schoolmath")
start_time <- Sys.time()

library(schoolmath)
library(readxl)
library(dplyr)
phasing  <- read_excel("D:/BILAL/4. smart traffic signal system/phasing.xlsx")
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

#existing phasing
phasing$phase1m<- c("3","7","8,3","4,7")
phasing$phase2m<- c("8,4","8,4","8,4","8,4")
phasing$phase3m<- c("5","1","7","1")
phasing$phase4m<- c("2,5")
phasing$phase5m<- c("6,1")

#volume
phasing$ET<- c(2820,2616,1730,2495)
phasing$EL<- c(399,0,560,0)
phasing$WT<- c(1383,1372,1006,1878)
phasing$WL<- c(0,0,64,156)
phasing$NT<- c(0,0,333,0)
phasing$NL<- c(0,193,130,126)
phasing$ST<- c(0,0,660,0)
phasing$SL<- c(137,0,979,0)

#saturation per lane and total lanes
saturation<- data.frame("ET"=rep(2000,4),"WT"=rep(2000,4),"NT"=rep(2000,4),"ST"=rep(2000,4),
                        "EL"=rep(2000,4),"WL"=rep(2000,4),"NL"=rep(2000,4),"SL"=rep(2000,4),
                        "lane_ET" = rep(4,4),"lane_WT" = rep(4,4),"lane_NT" = c(1,2,1,1),"lane_ST" = c(1,0,4,0),
                        "lane_EL" = rep(1,4),"lane_WL" = rep(1,4),"lane_NL" = rep(1,4),"lane_SL" = rep(1,4))

saturation$ET_a<- (saturation$ET*(saturation$lane_ET))
saturation$WT_a<- (saturation$WT*(saturation$lane_WT))
saturation$NT_a<- (saturation$NT*(saturation$lane_NT))
saturation$ST_a<- (saturation$ST*(saturation$lane_ST))
saturation$EL_a<- (saturation$EL*(saturation$lane_EL))
saturation$WL_a<- (saturation$WL*(saturation$lane_WL))
saturation$NL_a<- (saturation$NL*(saturation$lane_NL))
saturation$SL_a<- (saturation$SL*(saturation$lane_SL))

#v/S
v_s<- data.frame("ET_v/s"=(phasing$ET/saturation$ET_a),"WT_v/s"=(phasing$WT/saturation$WT_a),
                  "NT_v/s"=(phasing$NT/saturation$NT_a),"ST_v/s"=(phasing$ST/saturation$ST_a),
                 "EL_v/s"=(phasing$EL/saturation$EL_a),"WL_v/s"=(phasing$WL/saturation$WL_a),
                 "NL_v/s"=(phasing$NL/saturation$NL_a),"SL_v/s"=(phasing$SL/saturation$SL_a))
v_s[is.na(v_s)] = 0


EW2<- data.frame("v1"=c(3),"v2"=c(7),"v3"=c(4),"v4"=c(8))
# EW2<- data.frame("v1"=c(3,3),"v2"=c(7,8),"v3"=c(4,4),"v4"=c(8,7))
NS2<- data.frame("v1"=c(1,1),"v2"=c(5,6),"v3"=c(2,2),"v4"=c(6,5))
NS2_O<- data.frame("v1"=c(2,2),"v2"=c(6,5),"v3"=c(1,1),"v4"=c(5,6))

EW3<- data.frame("v1"=c(3,3),"v2"=c(7,7),"v3"=c(4,3),"v4"=c(7,8),
                 "v5"=c(4,4),"v6"=c(8,8))
# EW2<- data.frame("v1"=c(3,3),"v2"=c(7,8),"v3"=c(4,4),"v4"=c(8,7))
NS3<- data.frame("v1"=c(1,1,1,1),"v2"=c(5,5,6,6),"v3"=c(2,1,2,1),"v4"=c(5,6,6,5),
                 "v5"=c(2,2,2,2),"v6"=c(6,6,5,5))
NS3_O<- data.frame("v1"=c(2,2,2,2),"v2"=c(6,6,5,5),"v3"=c(1,2,1,2),"v4"=c(6,5,5,6),
                 "v5"=c(1,1,1,1),"v6"=c(5,5,6,6))

EW2_NS2<-cbind(EW2,NS2)
EW2_NS2_O<- cbind(EW2,NS2_O)
EWNS_22<- rbind(EW2_NS2,EW2_NS2_O)

EW2_NS3<- cbind(EW2,NS3)
EW2_NS3_O<- cbind(EW2,NS3_O)
EWNS_23<- rbind(EW2_NS3,EW2_NS3_O)

EW3_NS2<- rbind(cbind(EW3,NS2),cbind(EW3,NS2[order(nrow(NS2):1),]))
EW3_NS2_O<- rbind(cbind(EW3,NS2_O),cbind(EW3,NS2_O[order(nrow(NS2_O):1),]))
EWNS_32<- rbind(EW3_NS2,EW3_NS2_O)

EW3_NS3<- rbind(cbind(EW3,NS3),cbind(EW3,NS3[order(nrow(NS3):1),]))
EW3_NS3_O<- rbind(cbind(EW3,NS3_O),cbind(EW3,NS3_O[order(nrow(NS3_O):1),]))
EWNS_33<- rbind(EW3_NS3,EW3_NS3_O)


#take values of vs and put them accordingly
my_vector22 <- vector(mode="numeric")
v_s_st<- as.matrix(v_s[3,])#change to whichever intersection you want
v_s_st<- as.vector(v_s_st)
v_s_t_num<- c(8,4,6,2,7,3,5,1)
v_s_t_new<- cbind.data.frame(v_s_st,v_s_t_num)

for (i in 1:4){

  xxxx<- EWNS_22[i,]
  xxxx<- as.data.frame(as.vector(as.matrix(xxxx)))
  xxxx$id<- 1:nrow(xxxx)
  yyy<- merge(xxxx,v_s_t_new, all = TRUE, by.x = "as.vector(as.matrix(xxxx))", by.y = "v_s_t_num")
  yyy<- yyy[order(yyy$id),]
  
  ph1<- max(yyy[c(T,F,F,F,F,F,F,F),3],yyy[c(F,T,F,F,F,F,F,F),3])
  ph2<- max(yyy[c(F,F,T,F,F,F,F,F),3],yyy[c(F,F,F,T,F,F,F,F),3])
  ph3<- max(yyy[c(F,F,F,F,T,F,F,F),3],yyy[c(F,F,F,F,F,T,F,F),3])
  ph4<- max(yyy[c(F,F,F,F,F,F,T,F),3],yyy[c(F,F,F,F,F,F,F,T),3])
  vs_sum<- sum(ph1+ph2+ph3+ph4)
  # new_out<- cbind(EWNS_22[1,],vs_sum)
  print(vs_sum)
  my_vector22 <- append(my_vector22, vs_sum)
}
com_phasing_22<- cbind.data.frame(EWNS_22,my_vector22)


my_vector23 <- vector(mode="numeric")
v_s23<- rbind(v_s,v_s)
for (i in 1:8){
  # v_s_st<- as.matrix(v_s23[i,])
  # v_s_st<- as.vector(v_s_st)
  # v_s_t_num<- c(8,4,6,2,7,3,5,1)
  # v_s_t_new<- cbind.data.frame(v_s_st,v_s_t_num)
  # 
  
  xxxx<- EWNS_23[i,]
  xxxx<- as.data.frame(as.vector(as.matrix(xxxx)))
  xxxx$id<- 1:nrow(xxxx)
  yyy<- merge(xxxx,v_s_t_new, all = TRUE, by.x = "as.vector(as.matrix(xxxx))", by.y = "v_s_t_num")
  yyy<- yyy[order(yyy$id),]
  
  ph1<- max(yyy[c(T,F,F,F,F,F,F,F,F,F),3],yyy[c(F,T,F,F,F,F,F,F,F,F),3])
  ph2<- max(yyy[c(F,F,T,F,F,F,F,F,F,F),3],yyy[c(F,F,F,T,F,F,F,F,F,F),3])
  ph3<- max(yyy[c(F,F,F,F,T,F,F,F,F,F),3],yyy[c(F,F,F,F,F,T,F,F,F,F),3])
  ph4<- max(yyy[c(F,F,F,F,F,F,T,F,F,F),3],yyy[c(F,F,F,F,F,F,F,T,F,F),3])         
  ph5<- max(yyy[c(F,F,F,F,F,F,F,F,T,F),3],yyy[c(F,F,F,F,F,F,F,F,F,T),3])
  vs_sum<- sum(ph1+ph2+ph3+ph4+ph5)
  # new_out<- cbind(EWNS_22[1,],vs_sum)
  print(vs_sum)
  my_vector23 <- append(my_vector23, vs_sum)
}
com_phasing_23<- cbind.data.frame(EWNS_23,my_vector23)

my_vector32 <- vector(mode="numeric")
# v_s32<- rbind(v_s,v_s)
for (i in 1:8){
  # v_s_st<- as.matrix(v_s32[i,])
  # v_s_st<- as.vector(v_s_st)
  # v_s_t_num<- c(8,4,6,2,7,3,5,1)
  # v_s_t_new<- cbind.data.frame(v_s_st,v_s_t_num)
  # 
  # 
  xxxx<- EWNS_32[i,]
  xxxx<- as.data.frame(as.vector(as.matrix(xxxx)))
  xxxx$id<- 1:nrow(xxxx)
  yyy<- merge(xxxx,v_s_t_new, all = TRUE, by.x = "as.vector(as.matrix(xxxx))", by.y = "v_s_t_num")
  yyy<- yyy[order(yyy$id),]
  
  ph1<- max(yyy[c(T,F,F,F,F,F,F,F,F,F),3],yyy[c(F,T,F,F,F,F,F,F,F,F),3])
  ph2<- max(yyy[c(F,F,T,F,F,F,F,F,F,F),3],yyy[c(F,F,F,T,F,F,F,F,F,F),3])
  ph3<- max(yyy[c(F,F,F,F,T,F,F,F,F,F),3],yyy[c(F,F,F,F,F,T,F,F,F,F),3])
  ph4<- max(yyy[c(F,F,F,F,F,F,T,F,F,F),3],yyy[c(F,F,F,F,F,F,F,T,F,F),3])
  ph5<- max(yyy[c(F,F,F,F,F,F,F,F,T,F),3],yyy[c(F,F,F,F,F,F,F,F,F,T),3])
  vs_sum<- sum(ph1+ph2+ph3+ph4+ph5)
  # new_out<- cbind(EWNS_22[1,],vs_sum)
  print(vs_sum)
  my_vector32 <- append(my_vector32, vs_sum)
}
com_phasing_32<- cbind.data.frame(EWNS_32,my_vector32)


my_vector33 <- vector(mode="numeric")
v_s33<- rbind(v_s,v_s,v_s,v_s)

for (i in 1:16){
  # v_s_st<- as.matrix(v_s33[1,])
  # v_s_st<- as.vector(v_s_st)
  # v_s_t_num<- c(8,4,6,2,7,3,5,1)
  # v_s_t_new<- cbind.data.frame(v_s_st,v_s_t_num)
  # 
  # 
  xxxx<- EWNS_33[i,]
  xxxx<- as.data.frame(as.vector(as.matrix(xxxx)))
  xxxx$id<- 1:nrow(xxxx)
  yyy<- merge(xxxx,v_s_t_new, all = TRUE, by.x = "as.vector(as.matrix(xxxx))", by.y = "v_s_t_num")
  yyy<- yyy[order(yyy$id),]
  
  ph1<- max(yyy[c(T,F,F,F,F,F,F,F,F,F,F,F),3],yyy[c(F,T,F,F,F,F,F,F,F,F,F,F),3])
  ph2<- max(yyy[c(F,F,T,F,F,F,F,F,F,F,F,F),3],yyy[c(F,F,F,T,F,F,F,F,F,F,F,F),3])
  ph3<- max(yyy[c(F,F,F,F,T,F,F,F,F,F,F,F),3],yyy[c(F,F,F,F,F,T,F,F,F,F,F,F),3])
  ph4<- max(yyy[c(F,F,F,F,F,F,T,F,F,F,F,F),3],yyy[c(F,F,F,F,F,F,F,T,F,F,F,F),3])
  ph5<- max(yyy[c(F,F,F,F,F,F,F,F,T,F,F,F),3],yyy[c(F,F,F,F,F,F,F,F,F,T,F,F),3])
  ph6<- max(yyy[c(F,F,F,F,F,F,F,F,F,F,T,F),3],yyy[c(F,F,F,F,F,F,F,F,F,F,F,T),3])
  vs_sum<- sum(ph1+ph2+ph3+ph4+ph5+ph6)
  # new_out<- cbind(EWNS_22[1,],vs_sum)
  print(vs_sum)
  my_vector33 <- append(my_vector33, vs_sum)
}
com_phasing_33<- cbind.data.frame(EWNS_33,my_vector33)

#which value and corresponding combination
a22<- com_phasing_22[which.max(com_phasing_22$my_vector22),]
a23<- com_phasing_23[which.max(com_phasing_23$my_vector23),]
a32<- com_phasing_32[which.max(com_phasing_32$my_vector32),]
a33<- com_phasing_33[which.max(com_phasing_33$my_vector33),]

min_vec<- c(a22$my_vector22,a23$my_vector23,a32$my_vector32,a33$my_vector33)
min_vec_loc<- which.min(min_vec)
min_vec_val<- min(min_vec)

if(min_vec_loc==1){
  combination<- a22[a22$my_vector22 == min_vec_val,]
  print(combination)
  print("a22")
} else if(min_vec_loc==2){
  combination<- a23[a23$my_vector23 == min_vec_val,]
  print(combination)
  print("a23")
} else if(min_vec_loc==3){
  combination<- a32[a32$my_vector32 == min_vec_val,]
  print(combination)
  print("a32")
} else{
  combination<- a33[a33$my_vector33 == min_vec_val,]
  print(combination)
  print("a33")
}


















