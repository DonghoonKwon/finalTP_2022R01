
# 환승 / 무환승 / 시각표
# return : 각각 대기 시간 /  합

#calculatingWaitingTime(estArr_Transfer,estArr_nonTransfer,tableWT_scheduled,where)

# WT_Transfer = estArr_Transfer
# WT_nonTransfer = estArr_nonTransfer
# tableWT = tableWT_scheduled
# where = where

calculatingWaitingTime <- function(WT_Transfer,WT_nonTransfer,tableWT,where){
  
  r <- 1
  for(r in 1:nrow(WT_Transfer)){
    
    #
    temp <- data.frame(tableWT[,where+1])
    colnames(temp) <- "busDepTime"
    temp$busDepTime <- as.POSIXct(temp$busDepTime)
    tempDep <- (filter(temp, WT_Transfer$estArrTime[r] <= temp$busDepTime) )[1,]
    #
    WT_Transfer$waitingTime[r] <- difftime(tempDep,WT_Transfer$estArrTime[r],units="sec")
    
  }# NA 빼면됨
  
  WT_Transfer <- na.omit(WT_Transfer)
  
  
  #nonTransfer
  r <- 1
  for(r in 1:nrow(WT_nonTransfer)){
    
    #
    temp <- data.frame(tableWT[,where+1])
    colnames(temp) <- "busDepTime"
    temp$busDepTime <- as.POSIXct(temp$busDepTime)
    tempDep <- (filter(temp, WT_nonTransfer$estArrivalTime[r] <= temp$busDepTime) )[1,]
    #
    WT_nonTransfer$waitingTime[r] <- difftime(tempDep,WT_nonTransfer$estArrivalTime[r],units="sec")
    
  }
  
  #sum( WT_nonTransfer$waitingTime)/60
  
  res <- data.frame()
  res <- c(sum(WT_Transfer$waitingTime)/60, sum( WT_nonTransfer$waitingTime)/60)
  return(res )
  
  
}





# 1. 승객 도착 table 만들기
# nonTransferPax랑 transferPax를  tableWT랑 매칭해서 

analysisDate <- "0517"
paxPath <- "C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/중간save/paxArrivalTable/"

head(boardingStopList)

###
###
###


it <- 4

non <- read.csv(paste0(paxPath,"nonTransferPax",boardingStopList[it],"-",analysisDate,".csv"))

yes <- read.csv(paste0(paxPath,"transferPax",boardingStopList[it],"-",analysisDate,".csv"))

tableWT <- read.csv(paste0(paxPath,"talbeBusArrandDep-basedOnData.csv"),header=T)
tableWT <- tableWT[,-c(1)]

#view(tableWT)

###
###
###

# 1) nonTransfer 는 .. (non)

if(unique(non$승차역) == "108000004"){
  # 수유역
  # 어떤 칼럼을 tableWT에서 선택할지
  where <- which(colnames(tableWT)=="ArrT_4")
  Hwhere <- which(colnames(tableWT)=="h4")
  
}else if(unique(non$승차역) == "108000012"){
  # 미아사거리역
  where <- which(colnames(tableWT)=="ArrT_3")
  Hwhere <- which(colnames(tableWT)=="h3")
  
}else if(unique(non$승차역) == "107000043"){
  # 월곡역
  where <- which(colnames(tableWT)=="ArrT_2")
  Hwhere <- which(colnames(tableWT)=="h2")
  
}else if(unique(non$승차역)== "105000447"){
  # 청량리역
  where <- which(colnames(tableWT)=="ArrT_1")
  Hwhere <- which(colnames(tableWT)=="h1")
  
}
print(where)
print(Hwhere)

##
nonArrival <- data.frame()

r<-1

for(r in 1:nrow(tableWT)-1){
  
  #available boarding time
  temp <- non[non$BT > tableWT[r,where+1], ]
  temp <- temp[temp$BT <=tableWT[r+1,where+1], ]
  
  if(nrow(temp)==0){
    
  }else{
    
    temp$estArrivalTime <- as.POSIXct("2017-05-17 12:00:00")
    
    print(paste0(unique(non$승차역)," ",r))
    
    temp <- temp %>% arrange(BT)
    
    tempHead <- tableWT[r+1,Hwhere]
    
    rr <- 1
    for(rr in 1:nrow(temp)){
      
      temp$estArrivalTime[rr] <-
        as.POSIXct(tableWT[r+1,where], orders = "%Y-%m-%d %H:%M:%S") - seconds(tempHead*(nrow(temp)-rr+1)/(nrow(temp)))
      
    }
    
    temp$estArrivalTime <- as.POSIXct(temp$estArrivalTime, origin = "1970-01-01")    
    
    nonArrival <- rbind(nonArrival,temp) 
    
  }

  rm(temp)
   
}

nrow(nonArrival)==nrow(filter(non, as.POSIXct(paste0("2017-05-17 17:00")) < non$BT & non$BT < as.POSIXct(paste0("2017-05-17 19:00"))))

estPath <- "C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/중간save/paxArrivalTableEST/"

write.csv(nonArrival, file=paste0(estPath,"nonTransferPax-estArrivalTime-",boardingStopList[it],"-",analysisDate,".csv"))


#ggplot

it<-4

nonArrivalPlot <- read.csv(file=paste0(estPath,"nonTransferPax-estArrivalTime-",boardingStopList[it],"-",analysisDate,".csv"))

nonArrivalPlot$BT <- as.POSIXct(nonArrivalPlot$BT)
nonArrivalPlot$AT <- as.POSIXct(nonArrivalPlot$AT)
nonArrivalPlot$estArrivalTime <- as.POSIXct(nonArrivalPlot$estArrivalTime)

#도착 추정 시각 분포
ggplot(nonArrivalPlot, aes(BT, AT)) + geom_point()
ggplot(nonArrivalPlot, aes(estArrivalTime, AT)) + geom_point()

# 그래서 어떤 버스 스케줄에 대하여 WT 이 얼마나??
## new column 말하는거




###
###
###

# 2) transfer 는... (yes)...이거 ...AT기준으로 등간격으로 다시
# 이렇게 안 하면 방법이 없다 AT2가 같은데 왜 버스 태그 분포가 그렇게 까지 달라지냐구


it <- 1

#non <- read.csv(paste0(paxPath,"nonTransferPax",boardingStopList[it],"-",analysisDate,".csv"))

yes <- read.csv(paste0(paxPath,"transferPax",boardingStopList[it],"-",analysisDate,".csv"))

#str(yes)
yes$BT1 <- as.POSIXct(yes$BT1)
yes$AT1 <- as.POSIXct(yes$AT1)
yes$BT2 <- as.POSIXct(yes$BT2)
yes$AT2 <- as.POSIXct(yes$AT2)



{#     # 4호선.... 상행으로 할까 :  수유 미아사거리
# line4 <- read.csv(file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/data/subwayTimeTable-Line4.csv"))

# line4$수유도착 <- as.POSIXct(paste0("2017-",substr(analysisDate,1,2),"-",substr(analysisDate,3,4)," ",line4$수유도착))
# line4$수유출발 <- as.POSIXct(paste0("2017-",substr(analysisDate,1,2),"-",substr(analysisDate,3,4)," ",line4$수유출발))
# line4$미아사거리도착 <- as.POSIXct(paste0("2017-",substr(analysisDate,1,2),"-",substr(analysisDate,3,4)," ",line4$미아사거리도착))
# line4$미아사거리출발 <- as.POSIXct(paste0("2017-",substr(analysisDate,1,2),"-",substr(analysisDate,3,4)," ",line4$미아사거리출발))
# 
# line4Upper <- filter(line4, line4$상하행 == "상행")
# line4Upper <- line4Upper %>% arrange(수유도착)
}

#

yesObs <- filter(yes, as.POSIXct("2017-05-17 16:00:00") < yes$AT1 & yes$AT1 < as.POSIXct("2017-05-17 19:00:00"))

yesObs$BT1 <- as.POSIXct(yesObs$BT1)
yesObs$AT1 <- as.POSIXct(yesObs$AT1)
yesObs$BT2 <- as.POSIXct(yesObs$BT2)
yesObs$AT2 <- as.POSIXct(yesObs$AT2)


yesArrival <- yesObs

yesArrival$estArrTime <- yesArrival$BT2 - minutes(1)

estPath <- "C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/중간save/paxArrivalTableEST/"

write.csv(yesArrival, file=paste0(estPath,"transferPax-estArrivalTime-",boardingStopList[it],"-",analysisDate,".csv"))


# 2022-06-06
# 2. 승객 도착 table은 고정(1)시키고 버스 도착 분포를 다르게 하면서 DP
# 2.1. WT구하기

estPath <- "C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/중간save/paxArrivalTableEST/"

it <- 4

if(boardingStopList[it] == "108000004"){
  # 수유역
  # 어떤 칼럼을 tableWT에서 선택할지
  where <- which(colnames(tableWT)=="ArrT_4")
  Hwhere <- which(colnames(tableWT)=="h4")
  
}else if(boardingStopList[it] == "108000012"){
  # 미아사거리역
  where <- which(colnames(tableWT)=="ArrT_3")
  Hwhere <- which(colnames(tableWT)=="h3")
  
}else if(boardingStopList[it] == "107000043"){
  # 월곡역
  where <- which(colnames(tableWT)=="ArrT_2")
  Hwhere <- which(colnames(tableWT)=="h2")
  
}else if(boardingStopList[it] == "105000447"){
  # 청량리역
  where <- which(colnames(tableWT)=="ArrT_1")
  Hwhere <- which(colnames(tableWT)=="h1")
  
}
print(where)
print(Hwhere)

it <- 4
paxY <- read.csv(paste0(estPath,"transferPax-estArrivalTime-",boardingStopList[it],"-",analysisDate,".csv"))
paxN <- read.csv(paste0(estPath,"nonTransferPax-estArrivalTime-",boardingStopList[it],"-",analysisDate,".csv"))

paxY$BT1 <- as.POSIXct(paxY$BT1)
paxY$AT1 <- as.POSIXct(paxY$AT1)
paxY$BT2 <- as.POSIXct(paxY$BT2)
paxY$AT2 <- as.POSIXct(paxY$AT2)
paxY$estArrTime <- as.POSIXct(paxY$estArrTime)


paxN$BT <- as.POSIXct(paxN$BT)
paxN$AT <- as.POSIXct(paxN$AT)
colnames(paxN)[which(colnames(paxN)=="estArrivalTime")] <- "estArrTime"
paxN$estArrTime <- as.POSIXct(paxN$estArrTime)

##

WT_Transfer <- paxY
WT_nonTransfer <- paxN

WT_Transfer$waitingTime <- -999
WT_nonTransfer$waitingTime <- -999

# Transfer # 출발까지해야


r <- 1
for(r in 1:nrow(WT_Transfer)){
  
  #
  temp <- data.frame(tableWT[,where+1])
  colnames(temp) <- "busDepTime"
  temp$busDepTime <- as.POSIXct(temp$busDepTime)
  tempDep <- (filter(temp, WT_Transfer$estArrTime[r] <= temp$busDepTime) )[1,]
  #
  WT_Transfer$waitingTime[r] <- difftime(tempDep,WT_Transfer$estArrTime[r],units="sec")
  
}# NA 빼면됨

WT_Transfer <- na.omit(WT_Transfer)

sum(WT_Transfer$waitingTime)/60
#nonTransfer
r <- 1
for(r in 1:nrow(WT_nonTransfer)){
  
  #
  temp <- data.frame(tableWT[,where+1])
  colnames(temp) <- "busDepTime"
  temp$busDepTime <- as.POSIXct(temp$busDepTime)
  tempDep <- (filter(temp, WT_nonTransfer$estArrTime[r] <= temp$busDepTime) )[1,]
  #
  WT_nonTransfer$waitingTime[r] <- difftime(tempDep,WT_nonTransfer$estArrTime[r],units="sec")
  
}

sum( WT_nonTransfer$waitingTime)/60

# 2.2. 기존시간표 만들기

#평균 통행시간
#3~4
tableWT[,3] <- as.POSIXct(tableWT[,3])
tableWT[,4] <- as.POSIXct(tableWT[,4])

tableWT[,6] <- as.POSIXct(tableWT[,6])
tableWT[,7] <- as.POSIXct(tableWT[,7])

tableWT[,9] <- as.POSIXct(tableWT[,9])
tableWT[,10] <- as.POSIXct(tableWT[,10])

tableWT[,12] <- as.POSIXct(tableWT[,12])
tableWT[,13] <- as.POSIXct(tableWT[,13])

# 미아사거리 ~ 수유역 평균11분
mean(tableWT[,12] - tableWT[,10])

# 월곡역 ~ 미아사거리역 12분
mean(tableWT[,9] - tableWT[,7])

# 청량리역 ~ 월곡역 24분
mean(tableWT[,6] - tableWT[,4])


#
tableWT_scheduled <- tableWT

tableWT_scheduled[,3] <- as.POSIXct(tableWT_scheduled[,3])
tableWT_scheduled[,4] <- as.POSIXct(tableWT_scheduled[,4])

tableWT_scheduled[,6] <- as.POSIXct(tableWT_scheduled[,6])
tableWT_scheduled[,7] <- as.POSIXct(tableWT_scheduled[,7])

tableWT_scheduled[,9] <- as.POSIXct(tableWT_scheduled[,9])
tableWT_scheduled[,10] <- as.POSIXct(tableWT_scheduled[,10])

tableWT_scheduled[,12] <- as.POSIXct(tableWT_scheduled[,12])
tableWT_scheduled[,13] <- as.POSIXct(tableWT_scheduled[,13])

tableWT_scheduled$ArrT_4[1] <- tableWT_scheduled$DepT_4[1] - minutes(1)

r<-1
for(r in 1:(nrow(tableWT_scheduled)-1)){
  
  tableWT_scheduled[r+1,12] <- tableWT_scheduled[r,13] + minutes(5)
  tableWT_scheduled[r+1,13] <- tableWT_scheduled[r+1,12] + minutes(1)
  
}
#미아사거리역
tableWT_scheduled[,10] <- tableWT_scheduled[,12] - minutes(11)
tableWT_scheduled[,9] <- tableWT_scheduled[,10] - minutes(1)

#월곡역
tableWT_scheduled[,7] <- tableWT_scheduled[,9] - minutes(12)
tableWT_scheduled[,6] <- tableWT_scheduled[,7] - minutes(1)

#청량리역
tableWT_scheduled[,4] <- tableWT_scheduled[,6] - minutes(24)
tableWT_scheduled[,3] <- tableWT_scheduled[,4] - minutes(1)


#>
tableWT_scheduled$DwellT_1 <- difftime(tableWT_scheduled$DepT_1,tableWT_scheduled$ArrT_1,unit="sec")
tableWT_scheduled$DwellT_2 <- difftime(tableWT_scheduled$DepT_2,tableWT_scheduled$ArrT_2,unit="sec")
tableWT_scheduled$DwellT_3 <- difftime(tableWT_scheduled$DepT_3,tableWT_scheduled$ArrT_3,unit="sec")
tableWT_scheduled$DwellT_4 <- difftime(tableWT_scheduled$DepT_4,tableWT_scheduled$ArrT_4,unit="sec")

r<-1
for(r in 1:20){
  
  tableWT_scheduled$h1[r+1] <- difftime(tableWT_scheduled$ArrT_1[r+1],tableWT_scheduled$DepT_1[r],unit = "sec")  
  tableWT_scheduled$h2[r+1] <- difftime(tableWT_scheduled$ArrT_2[r+1],tableWT_scheduled$DepT_2[r],unit = "sec")  
  tableWT_scheduled$h3[r+1] <- difftime(tableWT_scheduled$ArrT_3[r+1],tableWT_scheduled$DepT_3[r],unit = "sec")  
  tableWT_scheduled$h4[r+1] <- difftime(tableWT_scheduled$ArrT_4[r+1],tableWT_scheduled$DepT_4[r],unit = "sec")  
  
}


{#Draw Plot
  
  tempTrj <- tableWT_scheduled
  plotTrj <- data.frame()
  distTable <- c(18.615,18.615, 24.186,24.186, 26.526,26.526, 29.412,29.412 )
  
  #view(tempTrj)
  
  for(busNum in tempTrj$차량등록번호){
    
    #busNum <- tempTrj$차량등록번호[1]
    
    where <- which(tempTrj$차량등록번호 == busNum)
    
    temp <- rbind(tempTrj$ArrT_1[where],tempTrj$DepT_1[where],
                  tempTrj$ArrT_2[where],tempTrj$DepT_2[where],
                  tempTrj$ArrT_3[where],tempTrj$DepT_3[where],
                  tempTrj$ArrT_4[where],tempTrj$DepT_4[where])
    temp <- as.POSIXct(temp,origin = "1970-01-01")
    temp2 <- data.frame("Time"=temp)
    temp2$Bus <- busNum
    temp2$Distance <- distTable
    
    plotTrj <- rbind(plotTrj, temp2)
  }
  #view(plotTrj)
  
  plotTrj$Time <- as.POSIXct(plotTrj$Time,origin = "1970-01-01")
  
  ggplot(plotTrj, aes(Time, Distance, color=Bus)) + geom_line(size=1.3) +
    geom_hline(yintercept = c(18.615,24.186,26.526,29.412), lty=3)
  
}



###########################################################################
###############
###############
###############       G                 A
###############
###########################################################################

###########################################################################
# 1) scheduled Time table에 대하여
###########################################################################

resulstTable <- data.frame("transfer_passengers"=0, "non_Transfer_passengers"=0)

it <- 1

for(it in 1:4){
    estArr_Transfer <- read.csv(paste0(estPath,"transferPax-estArrivalTime-",boardingStopList[it],"-",analysisDate,".csv"))
    {
      estArr_Transfer$BT1 <- as.POSIXct(estArr_Transfer$BT1)
      estArr_Transfer$AT1 <- as.POSIXct(estArr_Transfer$AT1)
      estArr_Transfer$BT2 <- as.POSIXct(estArr_Transfer$BT2)
      estArr_Transfer$AT2 <- as.POSIXct(estArr_Transfer$AT2)
      estArr_Transfer$estArrTime <- as.POSIXct(estArr_Transfer$estArrTime)
    }
    estArr_Transfer <- estArr_Transfer[estArr_Transfer$차량등록번호 %in% busList, ]
    
    estArr_nonTransfer <- read.csv(paste0(estPath,"nonTransferPax-estArrivalTime-",boardingStopList[it],"-",analysisDate,".csv"))
    {
      estArr_nonTransfer$BT <- as.POSIXct(estArr_nonTransfer$BT)
      estArr_nonTransfer$AT <- as.POSIXct(estArr_nonTransfer$AT)
      estArr_nonTransfer$estArrivalTime <- as.POSIXct(estArr_nonTransfer$estArrivalTime)
    }
    estArr_nonTransfer <- estArr_nonTransfer[estArr_nonTransfer$차량등록번호 %in% busList, ]
    
    {
      
      if(unique(estArr_nonTransfer$승차역) == "108000004"){
        # 수유역
        # 어떤 칼럼을 tableWT에서 선택할지
        where <- which(colnames(tableWT_scheduled)=="ArrT_4")
        Hwhere <- which(colnames(tableWT_scheduled)=="h4")
        
      }else if(unique(estArr_nonTransfer$승차역) == "108000012"){
        # 미아사거리역
        where <- which(colnames(tableWT_scheduled)=="ArrT_3")
        Hwhere <- which(colnames(tableWT_scheduled)=="h3")
        
      }else if(unique(estArr_nonTransfer$승차역) == "107000043"){
        # 월곡역
        where <- which(colnames(tableWT_scheduled)=="ArrT_2")
        Hwhere <- which(colnames(tableWT_scheduled)=="h2")
        
      }else if(unique(estArr_nonTransfer$승차역)== "105000447"){
        # 청량리역
        where <- which(colnames(tableWT_scheduled)=="ArrT_1")
        Hwhere <- which(colnames(tableWT_scheduled)=="h1")
        
      }
      
    }
    
    #view(tableWT_scheduled)
    
    ttemp <- calculatingWaitingTime(estArr_Transfer,estArr_nonTransfer,tableWT_scheduled,where)
    resulstTable <- rbind(resulstTable, ttemp)

}

###########################################################################
# 2) 원래대로 움직이면
###########################################################################

tableWT <- read.csv(file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/중간save/paxArrivalTable/talbeBusArrandDep-basedOnData.csv"))


resulstTable <- data.frame("transfer_passengers"=0, "non_Transfer_passengers"=0)

it <- 1

for(it in 1:4){
  estArr_Transfer <- read.csv(paste0(estPath,"transferPax-estArrivalTime-",boardingStopList[it],"-",analysisDate,".csv"))
  {
    estArr_Transfer$BT1 <- as.POSIXct(estArr_Transfer$BT1)
    estArr_Transfer$AT1 <- as.POSIXct(estArr_Transfer$AT1)
    estArr_Transfer$BT2 <- as.POSIXct(estArr_Transfer$BT2)
    estArr_Transfer$AT2 <- as.POSIXct(estArr_Transfer$AT2)
    estArr_Transfer$estArrTime <- as.POSIXct(estArr_Transfer$estArrTime)
  }
  estArr_Transfer <- estArr_Transfer[estArr_Transfer$차량등록번호 %in% busList, ]
  
  estArr_nonTransfer <- read.csv(paste0(estPath,"nonTransferPax-estArrivalTime-",boardingStopList[it],"-",analysisDate,".csv"))
  {
    estArr_nonTransfer$BT <- as.POSIXct(estArr_nonTransfer$BT)
    estArr_nonTransfer$AT <- as.POSIXct(estArr_nonTransfer$AT)
    estArr_nonTransfer$estArrivalTime <- as.POSIXct(estArr_nonTransfer$estArrivalTime)
  }
  estArr_nonTransfer <- estArr_nonTransfer[estArr_nonTransfer$차량등록번호 %in% busList, ]
  
  {
    
    if(unique(estArr_nonTransfer$승차역) == "108000004"){
      # 수유역
      # 어떤 칼럼을 tableWT에서 선택할지
      where <- which(colnames(tableWT)=="ArrT_4")
      Hwhere <- which(colnames(tableWT)=="h4")
      
    }else if(unique(estArr_nonTransfer$승차역) == "108000012"){
      # 미아사거리역
      where <- which(colnames(tableWT)=="ArrT_3")
      Hwhere <- which(colnames(tableWT)=="h3")
      
    }else if(unique(estArr_nonTransfer$승차역) == "107000043"){
      # 월곡역
      where <- which(colnames(tableWT)=="ArrT_2")
      Hwhere <- which(colnames(tableWT)=="h2")
      
    }else if(unique(estArr_nonTransfer$승차역)== "105000447"){
      # 청량리역
      where <- which(colnames(tableWT)=="ArrT_1")
      Hwhere <- which(colnames(tableWT)=="h1")
      
    }
    
  }
  
  #view(tableWT)
  
  ttemp <- calculatingWaitingTime(estArr_Transfer,estArr_nonTransfer,tableWT,where)
  resulstTable <- rbind(resulstTable, ttemp)
  
}

# shifting 넣고 Dwell Time / 통행시간은 동일

tableForDP <- tableWT_scheduled

view(tableForDP)

