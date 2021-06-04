#increasing memory size
memory.limit()
memory.limit(size=500000000)
#loading libraries
library(schoolmath)
library(readxl)
library(dplyr)
library(gtools)
#error in english
Sys.setenv(LANGUAGE='en')
#loading in the phasing data
phasing  <- read_excel("D:/BILAL/smart traffic signal system/신호패턴_TOD_최종.xlsx")
phasing<- phasing[phasing$교차로명 %in% c("서대전나들목삼거리","서일고교삼거리","진잠네거리","구봉중삼거리")
                  & phasing$요일 %in% "주중" & phasing$시간 %in% "17:00~21:00",]

phasing$phase1<- phasing$`1현시-녹색`+phasing$`1현시-황색`
phasing$phase2<- phasing$`2현시-녹색`+phasing$`2현시-황색`
phasing$phase3<- phasing$`3현시-녹색`+phasing$`3현시-황색`
phasing$phase4<- phasing$`4현시-녹색`+phasing$`4현시-황색`
phasing$phase5<- phasing$`5현시-녹색`+phasing$`5현시-황색`
phasing<- phasing[, (colnames(phasing) %in% c("교차로명","요일","시간","phase1", "phase2", "phase3","phase4","phase5"))]
#total_cycle_time
C<-  rowSums(phasing[1,4:8], na.rm = TRUE)
#select the intersection number and standard phasing to be used (main: through movements)
#ask from user
intersection_num <- 0
proposed_g<- vector(mode = "numeric")
slacktime<- c(10,10,10,10) #add slack up and down
# slacktime_e<- c(10,10,10,10)
# slacktime_w<- c(10,10,10,10)
ped_time<- c(10,10,10,10)

while (intersection_num<5) {
  total_intersections<- 4
  intersection_num<- intersection_num+1
  DS_phasing_standard<- 2

  actual_phase_count_nas<- rowSums(is.na(phasing)) 
  actual_phase_count<-ncol(phasing[,-c(1,2,3)])-actual_phase_count_nas  
  
  #green As_IS
  green_old<- phasing[,c(4:8)]
  
  #v/s for As_Is situation....hard coded; find a way to not do it this way
  vs_old1<- c(0.222,0.3525,0.076)
  vs_old2<- c(0.0355,0.3270,0.1072)
  vs_old3<- c(0.311,0.2162,0.050,0.544,0.1665)
  vs_old4<- c(0.2347,0.312,0.070)
  #As_Is green time
  green_old1<- phasing[1,4:6]
  green_old2<- phasing[2,4:6]
  green_old3<- phasing[3,4:8]
  green_old4<- phasing[4,4:6]
  #DS of As_Is......................................v/s*C/g
  DS_old1<- (vs_old1*C)/green_old1
  DS_old2<- (vs_old2*C)/green_old2
  DS_old3<- (vs_old3*C)/green_old3
  DS_old4<- (vs_old4*C)/green_old4
  DS_old<- smartbind(DS_old1,DS_old2,DS_old3,DS_old4)
  green_old<- smartbind(green_old1,green_old2,green_old3,green_old4)
  
  #finding the average DS needed 
  DS_needed<- rowSums(DS_old, na.rm = TRUE)
  na_perrow<- rowSums(is.na(DS_old))
  divider_perphase<-ncol(DS_old)-na_perrow  
  DS_needed<- DS_needed/divider_perphase
  
  #GENERATE Sequence from original DS to needed DS....and calculate green time for all
  DS_seq<- seq(min(DS_old[intersection_num,DS_phasing_standard],DS_needed[intersection_num]),max(DS_old[intersection_num,DS_phasing_standard],DS_needed[intersection_num]), by = 0.01)
  #vectors to save data generated from for loop
  my_vector_e1 <- vector(mode="numeric")
  my_vector_e2 <- vector(mode="numeric")
  
  for (i in 1:length(DS_seq)){
    green2<- ((DS_old[intersection_num,DS_phasing_standard]/DS_seq[i]))*(as.numeric(green_old[intersection_num,DS_phasing_standard]))
    diff_green<- as.numeric(green_old[intersection_num,DS_phasing_standard])- green2
    # print(diff_green)
    my_vector_e1<- append(my_vector_e1,diff_green)
    my_vector_e2<- append(my_vector_e2,green2)
    
  }  
  DS_green_new<- data.frame("DS"=DS_seq,"new_green"=my_vector_e2, "diff_green" =my_vector_e1,"green_old" =green_old[intersection_num,DS_phasing_standard])
  DS_green_new$diff_green<- (DS_green_new$diff_green)*(-1) 
  #checking if the difference calcualted is lesser than the slack time calculated
  #assumed slack time
  
  if(nrow(DS_green_new[(abs(DS_green_new$diff_green)<slacktime[intersection_num])&(abs(DS_green_new$diff_green)<ped_time[intersection_num]),])==0){
    print("calculated green time lies outside constraint")
    update<- FALSE
  }else{
    update<- TRUE
    possible_times<- DS_green_new[(abs(DS_green_new$diff_green)<slacktime[intersection_num])&(abs(DS_green_new$diff_green)<ped_time[intersection_num]),]
    print(possible_times)
  }
  recommended_green<- possible_times[which.max(abs(possible_times$diff_green)),]
  print(recommended_green)
  print(intersection_num)
  other_phasing_addition<- (recommended_green$diff_green)/(divider_perphase[intersection_num]-1)
  proposed_g<- append(proposed_g,ceiling(recommended_green$new_green))

  
}

# #update the table of this 
# if(update==TRUE){
#   phasing$phase2[intersection_num]<- ceiling(recommended_green$new_green)
#   if(other_phasing_addition<0){
#     phasing$phase2[intersection_num]<- ceiling(recommended_green$new_green)
#     }
# }



