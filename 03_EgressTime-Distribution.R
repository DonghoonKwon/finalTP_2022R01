## Egress time 간단하게


#역별 오후 피크에 열차 도착에서 하차까지
# 수유 미아사거리 월곡 청량리

analysisDate

it<-4

#여기서는 필요없음
yes <- read.csv(paste0(paxPath,"transferPax",boardingStopList[it],"-",analysisDate,".csv"))

###
###
###

# 4호선.... 상행으로 할까 :  수유 미아사거리
line4 <- read.csv(file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/data/subwayTimeTable-Line4.csv"))

line4$수유도착 <- as.POSIXct(paste0("2017-",substr(analysisDate,1,2),"-",substr(analysisDate,3,4)," ",line4$수유도착))
line4$수유출발 <- as.POSIXct(paste0("2017-",substr(analysisDate,1,2),"-",substr(analysisDate,3,4)," ",line4$수유출발))
line4$미아사거리도착 <- as.POSIXct(paste0("2017-",substr(analysisDate,1,2),"-",substr(analysisDate,3,4)," ",line4$미아사거리도착))
line4$미아사거리출발 <- as.POSIXct(paste0("2017-",substr(analysisDate,1,2),"-",substr(analysisDate,3,4)," ",line4$미아사거리출발))

line4Upper <- filter(line4, line4$상하행 == "상행")
line4Upper <- line4Upper %>% arrange(수유도착)

#
alight <- filter(rawDataReal, rawDataReal$하차정류장ID_정산사업자 == "0414")
#str(alight)
  
alight$BT <- posixTrans(alight$승차일시)
alight$AT <- posixTrans(alight$하차일시)

alightT <- alight

alightT <- filter(alightT, 414 < alightT$승차정류장ID_교통사업자 & alightT$승차정류장ID_교통사업자 <499)

#str(alightT)

alightT <- filter(alightT, 
                  as.POSIXct("2017-05-17 16:00")<alightT$AT & alightT$AT < as.POSIXct("2017-05-17 20:00"))

#view(alightT)

i<-1
alightT$estArrTime <- as.POSIXct("2017-05-17 09:00")
for(i in 1:nrow(alightT)){
  
  alightT$estArrTime[i] <- max(line4Upper[line4Upper$수유도착 <alightT$AT[i], ]$수유도착)
  
  if(i %% 100 == 0){
    print(i)
  }
}

#
egressPlot <- data.frame("trainArr" = alightT$estArrTime, "paxTagOut" = alightT$AT )

egressPlot <- filter(egressPlot, 
                     as.POSIXct("2017-05-17 17:08:00") < egressPlot$trainArr & egressPlot$trainArr < as.POSIXct("2017-05-17 17:10:00"))

ggplot(egressPlot, aes(trainArr, paxTagOut)) + geom_point()


egressPlot$distribution <- paste0(as.integer(substr(egressPlot$paxTagOut,12,13))*3600 + as.integer(substr(egressPlot$paxTagOut,15,16))*60 +
                            as.integer(substr(egressPlot$paxTagOut,18,19)))

egressPlot$distribution <- as.integer(egressPlot$distribution)

egressPlot$distribution <- egressPlot$distribution-min(egressPlot$distribution)

ggplot(egressPlot, aes(x=distribution, y=..density..)) + 
  geom_histogram(binwidth = 10) + 
  theme_minimal() + geom_line(stat="density", adjust=2, color="red",lwd = 1.1)

integ



###
###
###

# 6호선.... 월곡

line6 <- read.csv(file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/data/subwayTimeTable-Line6.csv"))






###
###
###

# 1호선.... 청량리
line1 <- read.csv(file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/data/subwayTimeTable-Line1.csv"))


