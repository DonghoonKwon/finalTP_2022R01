#>===============================================
#>
#> 2022R01 교통및물류최적화 기말 텀프로젝트
#> 지도교수 : 강승모 교수님
#> 건축사회환경공학과 권동훈 2022010509
#> 
#> 
#> 

rm(list=ls())
gc()




# posixTrans 날짜양식변경 함수
posixTrans <- function(data){
  
  temp <-(as.POSIXct(paste0(as.character(substr(data,1,4)),"-",as.character(substr(data,5,6)),"-",as.character(substr(data,7,8)),
                            " ",as.character(substr(data,9,10)),":",as.character(substr(data,11,12)),":",as.character(substr(data,13,14)))))
  return(temp)
}


#>===============================================
#>  0. source (library & function)

source("99_Library.R")

options(scipen=1000)

#>===============================================
#>  1. processing

analysisDate <- "0517" #16: 화


#>===============================================
#> 2. 2017년 5월 20일 월요일
#> 2.1. rawData 다듬기

Rpath <- "T:/05_Smart_Card_Data/2016-2017_SNU/2017_Card_only-201701-201702-201705/02)RawData/"
dataPath <- "C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/data"

rawDataReal <- fread(paste0(Rpath,analysisDate,".txt"))

rawDataAnl <- rawDataReal



#>===============================================
#>



# for clustering ================================
# dateList <- c("0516","0517","0518","0519")
# 
# rawDataReal <- data.frame()
# #4일 평균
# for(dt in dateList){
#   
#   temp <- fread(paste0(Rpath,dt,".txt"))
#   rawDataReal <- rbind(rawDataReal,temp)
#   rm(temp)
#   
# }
#rawData <- rawDataReal[,c(2,3,5,6,10,11,12,13,14,16,17,18,19,20,22)]
#colnames(rawData)[21]

str(rawData)

#> 2.2. 수유역 탑승  (지하철 하차 -> 수유역.강북구청 버스(108000004) 탑승) === boarding_Suyu
head(rawDataReal)

rawDataAnl <- rawDataReal

boarding_Suyu <- filter(rawDataAnl,rawDataAnl$승차정류장ID_국토부표준 == stopList[it] )

#> 2.3. 그 중에서 4호선으로 올라온애 === totalTable

length(unique(boarding_Suyu$가상카드번호))

totalTable <- data.frame()
where <- 1
for(ID in unique(boarding_Suyu$가상카드번호)){
  
  temp <- filter(rawDataAnl,rawDataAnl$가상카드번호 == ID)
  
  temp <- temp %>% arrange(승차일시)
  
  totalTable <- rbind(totalTable,temp)
  
  rm(temp)
  
  print(paste0(where,"/",length(unique(boarding_Suyu$가상카드번호))))
  where <- where + 1
}
totalTable

#> 2.4. 중간 save=====================================================================================
write.csv(totalTable,file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/",
                                  "2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/120번_버스/",
                                 "totalTable-",stopList[it],"-",analysisDate,".csv"))

#> ============
#it <- 3
totalTable <- read.csv(file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/",
                                 "2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/120번_버스/",
                                 "totalTable-",stopList[it],"-",analysisDate,".csv"))


#> 2.5. 수유 승차 중... (분석) === tripChainTable

#view(totalTable)

subAlight_busBoarding <- filter(totalTable, totalTable$하차정류장ID_정산사업자 == stationList[it])

tripChainTable <- data.frame()


for(ID in unique(subAlight_busBoarding$가상카드번호)){
  
  
  temp <- totalTable[totalTable$가상카드번호 %in% ID, ]
  
  tempA1  <- temp[temp$하차정류장ID_정산사업자 %in% stationList[it],]
  tempB2 <- temp[temp$승차정류장ID_국토부표준 %in% stopList[it], ]
  tempAB <- rbind(tempA1,tempB2)
  
  tempAB <- tempAB %>% arrange(승차일시)
  
  if(tempAB$환승횟수[2]-tempAB$환승횟수[1] == 1){
    
    tripChainTable <- rbind(tripChainTable,tempAB)
  
  }
  
  rm(tempAB)
}


#> 2.6. 아이디 별로 붙이기 === totalTrpChainTable 


#> 2.6.1. 헐..., 120번만 골라내기 111006400


nrow(tripChainTable)

totalTrpChainTable <- data.frame()
reCheckneedList <- data.frame()

for(ID in unique(tripChainTable$가상카드번호)){
  
  temp <- filter(tripChainTable, tripChainTable$가상카드번호 == ID)
  
  temp <- temp %>% arrange(승차일시)
  

  if(nrow(temp) == 2 | nrow(temp) == 3 ){
    
    #tripChainTable
    
    tempSum <- data.frame("가상카드번호" = temp$가상카드번호[1],
                          
                          "승차1" = temp[1,]$승차일시, "승차역1" = temp[1,]$승차정류장ID_교통사업자,
                          "하차1" = temp[1,]$하차일시, "하차역1" = temp[1,]$하차정류장ID_정산사업자,
                          
                          "차량등록번호" = temp[2,]$차량등록번호, "교통사업자ID"=temp[2,]$교통사업자ID,
                          "승차2" = temp[2,]$승차일시, "승차역2" = temp[2,]$승차정류장ID_국토부표준,
                          "하차2" = temp[2,]$하차일시, "하차역2" = temp[2,]$하차정류장ID_국토부표준
                               
                          )
    
    totalTrpChainTable <- rbind(totalTrpChainTable, tempSum)
    
    rm(tempSum)
    
    
  }else if(nrow(temp) == 4 | nrow(temp) == 5 ){
    
    print(paste0("!: ",nrow(temp)))
    
    tempSum <- data.frame("가상카드번호" = temp$가상카드번호[1],
                          
                          "승차1" = temp[1,]$승차일시, "승차역1" = temp[1,]$승차정류장ID_교통사업자,
                          "하차1" = temp[1,]$하차일시, "하차역1" = temp[1,]$하차정류장ID_정산사업자,
                          
                          "차량등록번호" = temp[2,]$차량등록번호, "교통사업자ID"=temp[2,]$교통사업자ID,
                          "승차2" = temp[2,]$승차일시, "승차역2" = temp[2,]$승차정류장ID_국토부표준,
                          "하차2" = temp[2,]$하차일시, "하차역2" = temp[2,]$하차정류장ID_국토부표준
                          
    )
    
    totalTrpChainTable <- rbind(totalTrpChainTable, tempSum)
    
    rm(tempSum)
    
    tempSum <- data.frame("가상카드번호" = temp$가상카드번호[1],
                          
                          "승차1" = temp[3,]$승차일시, "승차역1" = temp[3,]$승차정류장ID_교통사업자,
                          "하차1" = temp[3,]$하차일시, "하차역1" = temp[3,]$하차정류장ID_정산사업자,
                          
                          "차량등록번호" = temp[4,]$차량등록번호, "교통사업자ID"=temp[4,]$교통사업자ID,
                          "승차2" = temp[4,]$승차일시, "승차역2" = temp[4,]$승차정류장ID_국토부표준,
                          "하차2" = temp[4,]$하차일시, "하차역2" = temp[4,]$하차정류장ID_국토부표준
                          
    )
    
    totalTrpChainTable <- rbind(totalTrpChainTable, tempSum)
      
    rm(tempSum)
    
  }else if(nrow(temp) == 6 | nrow(temp) == 7){
    
    print(paste0("!: ",nrow(temp)))
    rm(tempSum)
    
    
  }else{
    
    print(paste0("! ", ID))
    
    reCheckneedList <- rbind(reCheckneedList,ID)
    
  }
  
  
  
}

totalTrpChainTable <- filter(totalTrpChainTable,totalTrpChainTable$교통사업자ID == "111006400") #120번
totalTrpChainTable <- filter(totalTrpChainTable,totalTrpChainTable$하차2 != "~" )

nrow(totalTrpChainTable)

totalTrpChainTable$BT1 <- posixTrans(totalTrpChainTable$승차1)
totalTrpChainTable$AT1 <- posixTrans(totalTrpChainTable$하차1)
totalTrpChainTable$BT2 <- posixTrans(totalTrpChainTable$승차2)
totalTrpChainTable$AT2 <- posixTrans(totalTrpChainTable$하차2)


nrow(totalTrpChainTable)

write.csv(totalTrpChainTable,file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/",
                                         "2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/120번_버스/",
                                         "totalTrpChainTable-",stopList[it],"-",analysisDate,".csv"))






#> 2.7. 본격 분석 (totalTrpChainTable를 이용하여)

# stopList <- c("108000004", "108000008", "108000339", "107000043", "107000046", "107000052", "105000447")
# stationList <- c("0414","0415","0416","2642","2643","2644","0158")
# 
# stopList <- c("105000447", "107000043", "108000339", "108000004")
# stationList <- c("0158", "")
# 

it <- 1


totalTrpChainTable <- read.csv(file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/",
                                           "2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/120번_버스/",
                                           "totalTrpChainTable-",stopList[it],"-",analysisDate,".csv"))


view(totalTrpChainTable)

critTime1 <- as.POSIXct(paste0("2017-",substr(analysisDate,1,2),"-",substr(analysisDate,3,4)," 17:00:00"))
critTime2 <- as.POSIXct(paste0("2017-",substr(analysisDate,1,2),"-",substr(analysisDate,3,4)," 19:00:00"))

yx <- data.frame("AT1" = c(critTime1,critTime2),
                 "BT1" = c(critTime1,critTime2))

trpChainByTime <- filter(totalTrpChainTable, totalTrpChainTable$AT1>=critTime1 & totalTrpChainTable$AT2<=critTime2)

ggplot(trpChainByTime, aes(AT1,BT2, color=차량등록번호)) +
  geom_point(size=2,shape=3) +
  geom_text(aes(label=(차량등록번호)), check_overlap = T, size=3,
            vjust = -1, nudge_y = 0.5) +
  geom_line(data=yx, aes(AT1,BT1),color="red")


#> 2.8. 여기에 그냥 일반 탑승자





#> 2.9. 이거하고 돌려이제 그만해!!!!!!!! 한 역에 대해서 대기시간(BM) 돌려봐



#> 2.10. 헐헐.... 타는 사람이 없다... 시간대별 120번 승객 분포============================================= 2022-05-30 여기부터

#> 2.10.1. 120번 버스 17:00~19:00 승차량 순위

bus120 <- filter(rawDataAnl,rawDataAnl$교통사업자ID == "111006400")

bus120 <- filter(bus120, bus120$하차일시 != "~")

bus120$BT <- posixTrans(bus120$승차일시)
bus120$AT <- posixTrans(bus120$하차일시)

critTime1 <- as.POSIXct(paste0("2017-",substr(analysisDate,1,2),"-",substr(analysisDate,3,4)," 16:00:00"))
critTime2 <- as.POSIXct(paste0("2017-",substr(analysisDate,1,2),"-",substr(analysisDate,3,4)," 20:00:00"))

bus120PMpeak <- filter(bus120, bus120$BT>=critTime1 & bus120$BT<=critTime2)

data.frame(table(bus120PMpeak$승차정류장ID_국토부표준)) %>% arrange(Freq)

#> 2.10.2. 승차량별 하차지점과 환승률  [[[[[[[[[[시작은 여기부터]]]]]]]]]]

# boardingStopList <- c("108000004", "105000447", "107000043", "108000012")
# alightingStationList <- c("0414", "0158", "2642", "0416")

boardingStopList <- c("105000447", "107000043","108000012", "108000004" )
alightingStationList <- c("0158", "2642", "0416", "0414")

it <- 1

obsList <- filter(bus120PMpeak, bus120PMpeak$승차정류장ID_국토부표준 == boardingStopList[it])

#head(obsList)
#length(unique(obsList$가상카드번호))



# 트립체인
 {
#     
#     #> 2.10.3. 트립체인 만들기
#     {# tripchain 만들기
#     
#       tripChainTable <- data.frame()
#     
#       for(ID in unique(obsList$가상카드번호)){
#     
#         temp <- rawDataAnl[rawDataAnl$가상카드번호 %in% ID, ]
#     
#         tempA1  <- temp[temp$하차정류장ID_정산사업자 %in% alightingStationList[it],]
#         tempB2 <- temp[temp$승차정류장ID_국토부표준 %in% boardingStopList[it], ]
#         tempAB <- rbind(tempA1,tempB2)
#     
#         tempAB <- tempAB %>% arrange(승차일시)
#     
#         if(nrow(tempAB) >= 2){
#       
#           if(tempAB$환승횟수[2]-tempAB$환승횟수[1] == 1){
#     
#             tripChainTable <- rbind(tripChainTable,tempAB)
#     
#           }
#         }
#     
#         rm(tempAB)
#       }
#     
#     
#       nrow(tripChainTable)
#     
#       totalTrpChainTable <- data.frame()
#       reCheckneedList <- data.frame()
#     
#       for(ID in unique(tripChainTable$가상카드번호)){
#     
#         temp <- filter(tripChainTable, tripChainTable$가상카드번호 == ID)
#     
#         temp <- temp %>% arrange(승차일시)
#     
#         if(nrow(temp) == 0 | nrow(temp) == 1 ){
#     
#           print(paste0("!: ", " has no transfer! ",ID))
#     
#         }else if(nrow(temp) == 2 | nrow(temp) == 3 ){
#     
#           #tripChainTable
#     
#           tempSum <- data.frame("가상카드번호" = temp$가상카드번호[1],
#     
#                                 "승차1" = temp[1,]$승차일시, "승차역1" = temp[1,]$승차정류장ID_교통사업자,
#                                 "하차1" = temp[1,]$하차일시, "하차역1" = temp[1,]$하차정류장ID_정산사업자,
#     
#                                 "차량등록번호" = temp[2,]$차량등록번호, "교통사업자ID"=temp[2,]$교통사업자ID,
#                                 "승차2" = temp[2,]$승차일시, "승차역2" = temp[2,]$승차정류장ID_국토부표준,
#                                 "하차2" = temp[2,]$하차일시, "하차역2" = temp[2,]$하차정류장ID_국토부표준
#     
#           )
#     
#           totalTrpChainTable <- rbind(totalTrpChainTable, tempSum)
#     
#           rm(tempSum)
#     
#     
#         }else if(nrow(temp) == 4 | nrow(temp) == 5 ){
#     
#           print(paste0("!: ",nrow(temp)))
#     
#           tempSum <- data.frame("가상카드번호" = temp$가상카드번호[1],
#     
#                                 "승차1" = temp[1,]$승차일시, "승차역1" = temp[1,]$승차정류장ID_교통사업자,
#                                 "하차1" = temp[1,]$하차일시, "하차역1" = temp[1,]$하차정류장ID_정산사업자,
#     
#                                 "차량등록번호" = temp[2,]$차량등록번호, "교통사업자ID"=temp[2,]$교통사업자ID,
#                                 "승차2" = temp[2,]$승차일시, "승차역2" = temp[2,]$승차정류장ID_국토부표준,
#                                 "하차2" = temp[2,]$하차일시, "하차역2" = temp[2,]$하차정류장ID_국토부표준
#     
#           )
#     
#           totalTrpChainTable <- rbind(totalTrpChainTable, tempSum)
#     
#           rm(tempSum)
#     
#           tempSum <- data.frame("가상카드번호" = temp$가상카드번호[1],
#     
#                                 "승차1" = temp[3,]$승차일시, "승차역1" = temp[3,]$승차정류장ID_교통사업자,
#                                 "하차1" = temp[3,]$하차일시, "하차역1" = temp[3,]$하차정류장ID_정산사업자,
#     
#                                 "차량등록번호" = temp[4,]$차량등록번호, "교통사업자ID"=temp[4,]$교통사업자ID,
#                                 "승차2" = temp[4,]$승차일시, "승차역2" = temp[4,]$승차정류장ID_국토부표준,
#                                 "하차2" = temp[4,]$하차일시, "하차역2" = temp[4,]$하차정류장ID_국토부표준
#     
#           )
#     
#           totalTrpChainTable <- rbind(totalTrpChainTable, tempSum)
#     
#           rm(tempSum)
#     
#         }else if(nrow(temp) == 6 | nrow(temp) == 7){
#     
#           print(paste0("!: ",nrow(temp)))
#           rm(tempSum)
#     
#     
#         }else{
#     
#           print(paste0("! ", ID))
#     
#           reCheckneedList <- rbind(reCheckneedList,ID)
#     
#         }
#     
#     
#     
#       }
#     
#       totalTrpChainTable <- filter(totalTrpChainTable,totalTrpChainTable$교통사업자ID == "111006400") #120번
#       totalTrpChainTable <- filter(totalTrpChainTable,totalTrpChainTable$하차2 != "~" )
#     
#       nrow(totalTrpChainTable)
#     
#       totalTrpChainTable$BT1 <- posixTrans(totalTrpChainTable$승차1)
#       totalTrpChainTable$AT1 <- posixTrans(totalTrpChainTable$하차1)
#       totalTrpChainTable$BT2 <- posixTrans(totalTrpChainTable$승차2)
#       totalTrpChainTable$AT2 <- posixTrans(totalTrpChainTable$하차2)
#     
#     
#       nrow(totalTrpChainTable)
#     
#       write.csv(totalTrpChainTable,file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/",
#                                                "2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/120번_버스/",
#                                                "totalTrpChainTable-PMpeak-",boardingStopList[it],"-",analysisDate,".csv"))
#     
#     
#     }
#       
#       
 }


#> 2.10.4. 동시에! 

# 1) 동시에
boardingStopList <- c("105000447", "107000043","108000012", "108000004" )
alightingStationList <- c("0158", "2642", "0416", "0414")

#it <- 2

obsList <- filter(bus120PMpeak, bus120PMpeak$승차정류장ID_국토부표준 == boardingStopList[it])

#str(obsList)
#length(unique(obsList$가상카드번호))

obsList$BT <- posixTrans(obsList$승차일시)
obsList$AT <- posixTrans(obsList$하차일시)

#전부다

obsList <- filter(obsList,obsList$BT > as.POSIXct(paste0("2017-05-17 16:00:00")))
obsList <- filter(obsList,obsList$BT < as.POSIXct(paste0("2017-05-17 20:00:00")))


ggplot(obsList, aes(BT, AT, color=차량등록번호)) + geom_point(shape=3) +
  geom_text(aes(label=(차량등록번호)), check_overlap = T, size=3,
            vjust = -1, nudge_y = 0.5) 




# 2) 이 중에 환승객은 누구?

#환승객
totalTrpChainTablePMpeak <- read.csv(file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/",
                                         "2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/120번_버스/",
                                         "totalTrpChainTable-PMpeak-",boardingStopList[it],"-",analysisDate,".csv"))

totalTrpChainTablePMpeak$BT1 <- as.POSIXct(totalTrpChainTablePMpeak$BT1)
totalTrpChainTablePMpeak$AT1 <- as.POSIXct(totalTrpChainTablePMpeak$AT1)
totalTrpChainTablePMpeak$BT2 <- as.POSIXct(totalTrpChainTablePMpeak$BT2)
totalTrpChainTablePMpeak$AT2 <- as.POSIXct(totalTrpChainTablePMpeak$AT2)

ggplot(totalTrpChainTablePMpeak, aes(BT2,AT2, color=차량등록번호)) + geom_point()+
  geom_text(aes(label=(차량등록번호)), check_overlap = T, size=3,
            vjust = -1, nudge_y = 0.5) 


#length(totalTrpChainTablePMpeak$가상카드번호)
#length(unique((totalTrpChainTablePMpeak$가상카드번호)))

#환승객 / 비환승객 구분
#head(obsList)

transferPax <- totalTrpChainTablePMpeak

nonTransferPax <-data.frame()
i<-1
for(i in 1:nrow(obsList)){
  
  ID <- obsList$가상카드번호[i]
  
  if(length(which(totalTrpChainTablePMpeak$가상카드번호==ID))==0){# obs에는 있고 환승데이터에는 없는
    
    nonTransferPax <- rbind(nonTransferPax,obsList[i,])
    
    
  }
  
}

if(nrow(obsList) == nrow(nonTransferPax) + nrow(transferPax)){
  
  print(paste0("Seperation has been completed!: ", Sys.time()))
  
}

# 비환승객 테이블 환승객 처럼 + 환승객 그룹화(Y or N)

nonTransferPax <- data.frame("가상카드번호" = nonTransferPax$가상카드번호,
                             "차량등록번호" = nonTransferPax$차량등록번호, "교통사업자ID" = nonTransferPax$교통사업자ID,
                             "승차" = nonTransferPax$승차일시, "승차역" = nonTransferPax$승차정류장ID_국토부표준,
                             "하차" = nonTransferPax$하차일시, "하차역" = nonTransferPax$하차정류장ID_국토부표준
                             )

nonTransferPax$BT <- posixTrans(nonTransferPax$승차)
nonTransferPax$AT <- posixTrans(nonTransferPax$하차)


transferPax$transfer <- "Y"
nonTransferPax$transfer <- "N"



# 3) 진짜 겹쳐서 그리기

tempT <- data.frame("BT"=transferPax$BT2, "AT"=transferPax$AT2,"transfer"=transferPax$transfer)
tempN <- data.frame("BT"=nonTransferPax$BT, "AT"=nonTransferPax$AT,"transfer"=nonTransferPax$transfer)

duplicatPlot <- rbind(tempT, tempN)

ggplot(duplicatPlot, aes(BT, AT, color = transfer)) + geom_point() 


#4개역

#view(nonTransferPax)
#view(transferPax)

write.csv(nonTransferPax,
          file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/중간save/paxArrivalTable/nonTransferPax",boardingStopList[it],"-",analysisDate,".csv"))

write.csv(transferPax,
          file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/중간save/paxArrivalTable/transferPax",boardingStopList[it],"-",analysisDate,".csv"))


####
#### obs 수유역 역별 만들어 놓고 
#### 

# str(obsList)
# 
# ggplot(obsList, aes(BT, AT , color=차량등록번호)) +
#   geom_point() + ggtitle(labe=paste0(boardingStopList[it]))
# 
# 
# ##
# obsListAnl <- filter(obsList,obsList$BT > as.POSIXct("2017-05-17 17:00:00") & obsList$BT < as.POSIXct("2017-05-17 19:00:00"))
# 
# timeTableByStation <- data.frame()
# #alightForDwellTime <- data.frame()
# 
# for(busNum in unique(obsListAnl$차량등록번호)){
#   
#   #busNum <- unique(obsListAnl$차량등록번호)[1]
#   temp <- filter(obsListAnl, obsListAnl$차량등록번호 == busNum)
#   
#   temp <- filter(temp, 
#                  temp$BT > as.POSIXct("2017-05-17 17:00:00") & temp$BT < as.POSIXct("2017-05-17 19:00:00"))
#   
#   
#   #departure : max(마지막 태그인, 마지막 태그아웃)
#   {
#     
#     alightForDT <- filter(bus120PMpeak, bus120PMpeak$하차정류장ID_국토부표준 == boardingStopList[it])
#     alightForDT <- filter(alightForDT, alightForDT$차량등록번호 == busNum )
#     alightForDT <- filter(alightForDT, 
#                           alightForDT$AT > as.POSIXct("2017-05-17 17:00:00") & alightForDT$AT < as.POSIXct("2017-05-17 19:00:00"))
#     
#   }
#   
#   if( length(max(temp$BT))!=0 & length(alightForDT$AT)!=0 ){
#     
#     maxDep <- max(max(temp$BT), max(alightForDT$AT))
#   }else{
#     
#     maxDep <- max(max(temp$BT))
#   }
#   
#   
#   tempT <- data.frame("차량등록번호" = busNum,
#                       "ArrivalTime" =  min(temp$BT), "DepartureTime" = maxDep,
#                       "DwellTime" = difftime(maxDep,min(temp$BT),units="secs"))
#   
#   timeTableByStation <- rbind(timeTableByStation, tempT)
#   
# }
# 
# 
# timeTableByStation <- timeTableByStation %>% arrange(ArrivalTime)
# 
# str(timeTableByStation)
# 
# 





####  23132132132132
#### timeTableByStation이걸 가지고 차량등록번호 기준으로 it 3,2,1
####

busNumList <- timeTableByStation$차량등록번호

timeTableByAllStop <- timeTableByStation

colnames(timeTableByAllStop) <- c("차량등록번호","ArrT_4","DepT_4","DwellT_4")





# 다시 3,2,1 21321321312312312

#it <- 2

obsList <- filter(bus120PMpeak, bus120PMpeak$승차정류장ID_국토부표준 == boardingStopList[it])


obsList$BT <- posixTrans(obsList$승차일시)
obsList$AT <- posixTrans(obsList$하차일시)

#전부다

obsListAnl <- filter(obsList,obsList$BT > as.POSIXct(paste0("2017-05-17 16:00:00")))
obsListAnl <- filter(obsList,obsList$BT < as.POSIXct(paste0("2017-05-17 20:00:00")))

obsListAnl <- obsList[obsList$차량등록번호 %in% busNumList, ]

obsListAnl <- filter(obsListAnl, obsListAnl$BT < as.POSIXct(paste0("2017-05-17 19:00:00")))

ggplot(obsListAnl, aes(BT, AT, color=차량등록번호)) + geom_point(shape=3) +
  geom_text(aes(label=(차량등록번호)), check_overlap = T, size=3,
            vjust = -1, nudge_y = 0.5)



# 다 끝나고 tempTimeTable를 timeTableByAllStop 이랑 merge

tempTimeTable <- data.frame("차량등록번호"=timeTableByStation$차량등록번호,
                            "ArrT_1"=as.POSIXct(paste0("2017-05-17 19:00:00"))[1:20],
                            "DepT_1"=as.POSIXct(paste0("2017-05-17 19:00:00"))[1:20],
                            "DwellT_1"=difftime(critTime1, critTime2, units="secs")[1:20])

#colnames(tempTimeTable)<-c("차량등록번호","ArrT#3", "DepT#3","DwellTime")

for(bus in tempTimeTable$차량등록번호){
  
  #bus <- busNumList[1]
  temp <- filter(obsListAnl, obsListAnl$차량등록번호 == bus)
  
  if(nrow(temp)>0){
    
    # 1. arrT
    arrT <- min(temp$BT)
    
    # 2. depT
    {
      
      alightForDT <- filter(bus120PMpeak, bus120PMpeak$하차정류장ID_국토부표준 == boardingStopList[it])
      alightForDT <- filter(alightForDT, alightForDT$차량등록번호 == bus )
      alightForDT <- filter(alightForDT, 
                            alightForDT$AT > as.POSIXct("2017-05-17 16:00:00") & alightForDT$AT < as.POSIXct("2017-05-17 19:00:00"))
      
    }
    
    if(nrow(alightForDT)!=0){
      depT <- max(max(temp$BT), max(alightForDT$AT))
    }else{
      depT <- max(temp$BT)
    }
    
    
   
    # 3. 넣기
    which(tempTimeTable$차량등록번호==bus)
    
    tempTimeTable$ArrT_1[which(tempTimeTable$차량등록번호==bus)] <- arrT
    tempTimeTable$DepT_1[which(tempTimeTable$차량등록번호==bus)] <- depT
    tempTimeTable$DwellT_1[which(tempTimeTable$차량등록번호==bus)] <- difftime(depT,arrT, units="secs")
    
    
    
    #tempTimeTable <- merge(tempTimeTable, temp2, by="차량등록번호", all=TRUE)
    
    
    
  }else{
    #없으면 도착으로 대체 이것도 없으면 중간 interpolation
    {
      
      alightForDT <- filter(bus120PMpeak, bus120PMpeak$하차정류장ID_국토부표준 == boardingStopList[it])
      alightForDT <- filter(alightForDT, alightForDT$차량등록번호 == bus )
      alightForDT <- filter(alightForDT, 
                            alightForDT$AT > as.POSIXct("2017-05-17 16:00:00") & alightForDT$AT < as.POSIXct("2017-05-17 19:00:00"))
      
    }
    
    if(nrow(alightForDT)>0){
      
      arrT <- min(alightForDT$AT)
      depT <- max(alightForDT$AT)
      
      
    }else{
      
      print(paste0(bus))
    }
    
    tempTimeTable$ArrT_1[which(tempTimeTable$차량등록번호==bus)] <- arrT
    tempTimeTable$DepT_1[which(tempTimeTable$차량등록번호==bus)] <- depT
    tempTimeTable$DwellT_1[which(tempTimeTable$차량등록번호==bus)] <- difftime(depT,arrT, units="secs")
    
    
  }#else
  
    
}

finalTableByAllStop <- merge( tempTimeTable, finalTableByAllStop, by="차량등록번호", all=F)

finalTableByAllStop <- finalTableByAllStop %>% arrange(ArrT_4)

#write.csv(finalTableByAllStop,file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/",
#                                          "2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/중간save/busTimeTable_4Stops.csv"))



### 시공도 그려보기
it

plotTrajectory <- read.csv(file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/",
                                       "2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/중간save/busTimeTable_4Stops.csv"))

tempTrj <- plotTrajectory
plotTrj <- data.frame()
distTable <- c(18.615,18.615, 24.186,24.186, 26.526,26.526, 29.412,29.412 )

for(busNum in tempTrj$차량등록번호){
  
  #busNum <- tempTrj$차량등록번호[1]
  
  where <- which(tempTrj$차량등록번호 == busNum)
  temp <- rbind(tempTrj$ArrT_1[where],tempTrj$DepT_1[where],
                tempTrj$ArrT_2[where],tempTrj$DepT_2[where],
                tempTrj$ArrT_3[where],tempTrj$DepT_3[where],
                tempTrj$ArrT_4[where],tempTrj$DepT_4[where])
  temp2 <- data.frame("Time"=temp)
  temp2$Bus <- busNum
  temp2$Distance <- distTable
  
  plotTrj <- rbind(plotTrj, temp2)
}
str(plotTrj)

plotTrj$Time <- as.POSIXct(plotTrj$Time)

ggplot(plotTrj, aes(Time, Distance, color=Bus)) + geom_line(size=1.3) +
  geom_hline(yintercept = c(18.615,24.186,26.526,29.412), lty=3)




###
### 시공도 기반으로 승객 도착 분포 매칭해서  승객별 Waiting time 구할 수 있게 준비
###

timeTableForCalWT <- plotTrajectory 
head(nonTransferPax) # => trj
head(transferPax)    # => trj 바로 넣어서 WT 나오는 각각의 함수

##
## transferPax
##

# 그 전에 Egress Time => transferPax의 상하행을 구분할 수 있을까?...

line1 <- read.csv(file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/data/subwayTimeTable-Line1.csv"))

line4 <- read.csv(file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/data/subwayTimeTable-Line4.csv"))

line6 <- read.csv(file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/data/subwayTimeTable-Line6.csv"))

##
## nonTransferPax
##

#WTresult <- data.frame()

criticRow <- data.frame("X"=0, "차량등록번호" = "직전출발버스", 
                        "ArrT_1"=as.POSIXct(paste0("2017-05-17 16:03:00")),"DepT_1"=as.POSIXct(paste0("2017-05-17 16:03:00")),"DwellT_1"=0,
                        "ArrT_2"=as.POSIXct(paste0("2017-05-17 16:29:00")),"DepT_2"=as.POSIXct(paste0("2017-05-17 16:29:00")),"DwellT_2"=0,
                        "ArrT_3"=as.POSIXct(paste0("2017-05-17 16:43:00")),"DepT_3"=as.POSIXct(paste0("2017-05-17 16:43:00")),"DwellT_3"=0,
                        "ArrT_4"=as.POSIXct(paste0("2017-05-17 17:00:00")),"DepT_4"=as.POSIXct(paste0("2017-05-17 17:00:00")),"DwellT_4"=0)

tableWT <- rbind(criticRow, timeTableForCalWT)

#head(tableWT)

# 1) headway (tableWT)
view(tableWT)

tableWT$h1 <- 0
tableWT$h2 <- 0
tableWT$h3 <- 0
tableWT$h4 <- 0

r <- 2
for(r in 2:nrow(tableWT)){
  
  tableWT$h1[r] <- difftime(tableWT$ArrT_1[r], tableWT$DepT_1[r-1],units="secs")
  tableWT$h2[r] <- difftime(tableWT$ArrT_2[r], tableWT$DepT_2[r-1],units="secs")
  tableWT$h3[r] <- difftime(tableWT$ArrT_3[r], tableWT$DepT_3[r-1],units="secs")
  tableWT$h4[r] <- difftime(tableWT$ArrT_4[r], tableWT$DepT_4[r-1],units="secs")

  #tableWT$h1[r] <- as.POSIXct(tableWT$ArrT_1[r] - tableWT$DepT_1[r-1])
  
}



# nonTransfer 옆에 어떤 버스 타는지 column 추가

nonT <- nonTransferPax

if(unique(nonT$승차역) == "108000004"){
  # 수유역
  # 어떤 칼럼을 tableWT에서 선택할지
  where <- which(colnames(tableWT)=="ArrT_4")
  Hwhere <- which(colnames(tableWT)=="h4")
  
}else if(unique(nonT$승차역) == "108000012"){
  # 미아사거리역
  where <- which(colnames(tableWT)=="ArrT_3")
  Hwhere <- which(colnames(tableWT)=="h3")
  
}else if(unique(nonT$승차역) == "107000043"){
  # 월곡역
  where <- which(colnames(tableWT)=="ArrT_2")
  Hwhere <- which(colnames(tableWT)=="h2")
  
}else if(unique(nonT$승차역)== "105000447"){
  # 청량리역
  where <- which(colnames(tableWT)=="ArrT_1")
  Hwhere <- which(colnames(tableWT)=="h1")
  
}
print(where)

# 1. 버스 매칭시키고
nonT2 <- data.frame()
r<-1
for(r in 1:(nrow(tableWT)-1)){
  
  #tableWT[r,where]
  t1 <- tableWT[r,where+1]
  t2 <- tableWT[r+1,where+1]
  
  temp <- filter(nonT, t1 < nonT$BT & nonT$BT <= t2 )
  
  if(nrow(temp)!=0){
    
    tempList <- temp$가상카드번호
    
    temp$whichBus <- tableWT$차량등록번호[r+1]
  }
  
  nonT2 <- rbind(nonT2, temp)
  
}

which(nonT2$차량등록번호 != nonT2$whichBus)

# 2. WT 계산

#headway table이랑 인원수 테이블...new table필요

totalWTtable <- data.frame()

col1 <- unique(nonT2$whichBus)
totalWTtable <- data.frame("차량등록번호"=col1)
totalWTtable$stop <- unique(nonT$승차역)
totalWTtable$totalWT <- -999

{
  
  temp <- data.frame(table(nonT2$whichBus))
  colnames(temp) <- c("차량등록번호","Freq")
  
  temp2 <- tableWT[,c(2,Hwhere)]
  
  mergeRes <- merge(temp2,temp,by="차량등록번호")
  
  totalWTtable$totalWT <- mergeRes[,2]/2*(mergeRes$Freq)
  
}

WTresult <- rbind(WTresult, totalWTtable)


sum(WTresult$totalWT)/60



# 
write.csv(tableWT,
          file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/중간save/paxArrivalTable/talbeBusArrandDep-basedOnData.csv"))





#zzz <- nonTransferPax[which(nonTransferPax$승차역 == nonTransferPax$하차역),]



print("!!")



#####====#####====#####====#####====#####====#####====#####====#####====#####====#####====#####====
#####====#####====#####====#####====#####====#####====#####====#####====#####====#####====#####====
#####==============살펴보고 삭제======

head(obsList)

obsListAnl <- obsListAnl[obsListAnl$차량등록번호 %in% busNumList,]

obsListAnl <- filter(obsListAnl, obsListAnl$BT < as.POSIXct(paste0("2017-05-17 19:30")))

ggplot(obsListAnl,aes(BT,AT, color=차량등록번호)) + geom_point()+
  geom_text(aes(label=(차량등록번호)), check_overlap = T, size=3,
            vjust = -1, nudge_y = 0.5)


# 2) 이 중에 환승객은 누구?

#환승객
totalTrpChainTablePMpeak <- read.csv(file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/",
                                                 "2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/120번_버스/",
                                                 "totalTrpChainTable-PMpeak-",boardingStopList[it],"-",analysisDate,".csv"))

head(totalTrpChainTablePMpeak)

totalTrpChainTablePMpeak$BT1 <- as.POSIXct(totalTrpChainTablePMpeak$BT1)
totalTrpChainTablePMpeak$AT1 <- as.POSIXct(totalTrpChainTablePMpeak$AT1)
totalTrpChainTablePMpeak$BT2 <- as.POSIXct(totalTrpChainTablePMpeak$BT2)
totalTrpChainTablePMpeak$AT2 <- as.POSIXct(totalTrpChainTablePMpeak$AT2)

ggplot(totalTrpChainTablePMpeak, aes(BT2,AT2, color=차량등록번호)) + geom_point()+
  geom_text(aes(label=(차량등록번호)), check_overlap = T, size=3,
            vjust = -1, nudge_y = 0.5)


#length(totalTrpChainTablePMpeak$가상카드번호)
#length(unique((totalTrpChainTablePMpeak$가상카드번호)))

#환승객 / 비환승객 구분
#head(obsList)

totalTrpChainTablePMpeak <- filter(totalTrpChainTablePMpeak,totalTrpChainTablePMpeak$BT2 < as.POSIXct(paste0("2017-05-17 19:30:00")))

transferPax <- totalTrpChainTablePMpeak

nonTransferPax <-data.frame()
i<-1
for(i in 1:nrow(obsListAnl)){

  ID <- obsListAnl$가상카드번호[i]

  if(length(which(totalTrpChainTablePMpeak$가상카드번호==ID))==0){# obs에는 있고 환승데이터에는 없는

    nonTransferPax <- rbind(nonTransferPax,obsListAnl[i,])


  }

}

if(nrow(obsListAnl) == nrow(nonTransferPax) + nrow(transferPax)){

  print(paste0("Seperation has been completed!: ", Sys.time()))
  #구간으로 잡아내는거라 크게 상관은 없을 듯....

}

# 비환승객 테이블 환승객 처럼 + 환승객 그룹화(Y or N)

nonTransferPax <- data.frame("가상카드번호" = nonTransferPax$가상카드번호,
                             "차량등록번호" = nonTransferPax$차량등록번호, "교통사업자ID" = nonTransferPax$교통사업자ID,
                             "승차" = nonTransferPax$승차일시, "승차역" = nonTransferPax$승차정류장ID_국토부표준,
                             "하차" = nonTransferPax$하차일시, "하차역" = nonTransferPax$하차정류장ID_국토부표준
)

nonTransferPax$BT <- posixTrans(nonTransferPax$승차)
nonTransferPax$AT <- posixTrans(nonTransferPax$하차)


transferPax$transfer <- "Y"
nonTransferPax$transfer <- "N"



# 3) 진짜 겹쳐서 그리기

tempT <- data.frame("BT"=transferPax$BT2, "AT"=transferPax$AT2,"transfer"=transferPax$transfer)
tempN <- data.frame("BT"=nonTransferPax$BT, "AT"=nonTransferPax$AT,"transfer"=nonTransferPax$transfer)

duplicatPlot <- rbind(tempT, tempN)

ggplot(duplicatPlot, aes(BT, AT, color = transfer)) + geom_point()






timeTableByStation <- data.frame()
#alightForDwellTime <- data.frame()

for(busNum in unique(obsListAnl$차량등록번호)){

  #busNum <- unique(obsListAnl$차량등록번호)[1]
  temp <- filter(obsListAnl, obsListAnl$차량등록번호 == busNum)

  temp <- filter(temp,
                 temp$BT > as.POSIXct("2017-05-17 17:00:00") & temp$BT < as.POSIXct("2017-05-17 19:00:00"))


  #departure : max(마지막 태그인, 마지막 태그아웃)
  {

    alightForDT <- filter(bus120PMpeak, bus120PMpeak$하차정류장ID_국토부표준 == boardingStopList[it])
    alightForDT <- filter(alightForDT, alightForDT$차량등록번호 == busNum )
    alightForDT <- filter(alightForDT,
                          alightForDT$AT > as.POSIXct("2017-05-17 17:00:00") & alightForDT$AT < as.POSIXct("2017-05-17 19:00:00"))

  }

  if( length(max(temp$BT))!=0 & length(alightForDT$AT)!=0 ){

    maxDep <- max(max(temp$BT), max(alightForDT$AT))
  }else{

    maxDep <- max(max(temp$BT))
  }


  tempT <- data.frame("차량등록번호" = busNum,
                      "ArrivalTime" =  min(temp$BT), "DepartureTime" = maxDep,
                      "DwellTime" = difftime(maxDep,min(temp$BT),units="secs"))

  timeTableByStation <- rbind(timeTableByStation, tempT)

}


timeTableByStation <- timeTableByStation %>% arrange(ArrivalTime)




nrow(timeTableByStation)


#===============================================================================================================
#===============================================================================================================
#===============================================================================================================
#===============================================================================================================
#===============================================================================================================
#===============================================================================================================
#===============================================================================================================
#===============================================================================================================
#===============================================================================================================
#===============================================================================================================
#===============================================================================================================
#===============================================================================================================
#===============================================================================================================
#===============================================================================================================
#===============================================================================================================
#===============================================================================================================
#===============================================================================================================
#===============================================================================================================

#> 2.10.5 waiting time 계산 후 save================================================
#> transfer 는 1분 전에 도착했다고 가정
#> nonTransfer 는 직전 버스 출발부터 등간격으로 도착
#> 그 전에  bus table 만들기


ggplot(transferPax, aes(AT1, BT2, color = 차량등록번호)) + geom_point()+
  geom_text(aes(label=(차량등록번호)), check_overlap = T, size=3,
            vjust = -1, nudge_y = 0.5) 


# 1) 버스 테이블(환승여부 상관없이)
ggplot(duplicatPlot, aes(BT, AT, color = transfer)) + geom_point() 

#ggplot(obsList, aes())

# tttt <- filter(obsList, 
#                obsList$BT > as.POSIXct(paste0("2017-05-17 18:15:00"))  & obsList$BT < as.POSIXct(paste0("2017-05-17 18:30:00"))  )
# 
# ggplot(tttt, aes(BT,AT, color=차량등록번호)) + geom_point()+
#   geom_text(aes(label=(차량등록번호)), check_overlap = T, size=3,
#             vjust = -1, nudge_y = 0.5) 


busTableByBusNum <- data.frame() # 차량등록번호 / 도착 / 출발 / dwell time
for(busNum in unique(obsList$차량등록번호)){
  
  #busNum <- unique(obsList$차량등록번호)[1]
  
  temp <- obsList[obsList$차량등록번호 %in% busNum, ]
  
  tempT <- data.frame("차량등록번호" = busNum,
                                 "ArrivalTime" =  min(temp$BT), "DepartureTime" = max(temp$BT),
                                 "DwellTime" = difftime(max(temp$BT),min(temp$BT),units="secs"))
  #as.difftime(max(temp$BT)-min(temp$BT))

  busTableByBusNum <- rbind(busTableByBusNum,tempT)
  
}

busTableByBusNum <- busTableByBusNum %>% arrange(ArrivalTime)
  
criticRow <- data.frame("차량등록번호" = boardingStopList[it],
                        "ArrivalTime" = as.POSIXct(paste0("2017-05-17 16:59:00")) ,
                        "DepartureTime" = as.POSIXct(paste0("2017-05-17 17:00:00")),
                        "DwellTime" = difftime(as.POSIXct(paste0("2017-05-17 17:00:00")), as.POSIXct(paste0("2017-05-17 16:59:00")),units="secs")) 

busTableByBusNum <- rbind(criticRow,busTableByBusNum)
  
headwayTable <- data.frame()
for(i in 2:nrow(busTableByBusNum)){
  
  #i <- 12
  headwayTable <- rbind(headwayTable,
                        difftime(busTableByBusNum$ArrivalTime[i], busTableByBusNum$DepartureTime[i-1], units="mins"))

}
colnames(headwayTable) <- "headway"

headwayTable <- rbind(0,headwayTable)  
  
busTableByBusNum <- cbind(busTableByBusNum,headwayTable)

view(busTableByBusNum)




#> 2.11. 정확한 dwell Time 확보를 위해 마지막 태그인 분석(앞에거까지 하고 난 다음에 가능!)

#it <- 2 # 1: 청량리 2: 월곡 3: 미아사거리 4: 수유

alightForDT <- filter(bus120PMpeak, bus120PMpeak$하차정류장ID_국토부표준 == boardingStopList[it])

nrow(alightForDT)

for( i in 2:nrow(busTableByBusNum)){#1은 기준버스
  
  busNum <- (busTableByBusNum$차량등록번호)[i]
  
  temp <- filter(alightForDT,alightForDT$차량등록번호 == busNum)
  
  print(paste0(i," ",nrow(temp)))
  if(nrow(temp)!=0){
    
    
    if(max(temp$AT) > busTableByBusNum$DepartureTime[i]){
      
      busTableByBusNum$DepartureTime[i] <- max(temp$AT)
      print(paste0(busTableByBusNum$DepartureTime[i]," => ", max(temp$AT)))
      
    }
    
  }
  
}
#DT, headway 다시 계산

# 2.12. 근데 뭐다? 수유역 기준 차량기준으로 다시 짜야한다... 

busNumList <- busTableByBusNum[2:nrow(busTableByBusNum),1]

str(bus120)

ctime1 <- as.POSIXct(paste0("2017-05-17 16:00:00"))
ctime2 <- as.POSIXct(paste0("2017-05-17 19:00:00"))

bus120forSuyu <- bus120[bus120$차량등록번호 %in% busNumList, ]

bus120forSuyu <- filter(bus120forSuyu,bus120forSuyu$BT > ctime1 & bus120forSuyu$BT < ctime2)


ggplot(bus120forSuyu, aes(BT,AT, color=차량등록번호)) + geom_point()

#CRR / WLG / MSG
bus120forSuyuOnCRR <- filter(bus120forSuyu,bus120forSuyu$승차정류장ID_국토부표준=="105000447")

ggplot(bus120forSuyuOnCRR, aes(BT,AT, color=차량등록번호)) + geom_point() #...tip chain을 다시 만들어야한당



#>===============================================
#>===============================================
#>===============================================
#>===============================================
#>===============================================
#>===============================================
#>===============================================
#>===============================================
#>===============================================
#>===============================================
#>===============================================
#>===============================================




#> 2.2 제대로
#> 2.2.0. as.POSIXct
rawData$승차일시[1]
yr <- substr(rawData$승차일시[1],1,4)
MM <- substr(rawData$승차일시[1],5,6)
dd <- substr(rawData$승차일시[1],7,8)
hh <- substr(rawData$승차일시[1],9,10)
mm <- substr(rawData$승차일시[1],11,12)
ss <- substr(rawData$승차일시[1],13,14)

as.POSIXct(paste0(yr,"-",MM,"-",dd," ",hh,":",mm,":",ss))


#> 2.2.1. 성신여대에서 성북04 탄 사람 





#> 2.2.2. 성신여대 하차사람!! 중에 성북04 승차list(근데 직전에 탔어야함...)
#> <<<<<<<<<<<<< 여기부터

ssMtr <- rawData[rawData$하차정류장ID_정산사업자 %in% "0418",]



#> 2.2.3. ssMtr 애들을 rawData에서 탐사: 418-> 성북04 9111835 | 9011835
#> 2.2.3.1. 418이 포함된
totalTable <- data.frame()

unqCardIdList <- unique(ssMtr$가상카드번호)

where <- 1
for(ID in unqCardIdList){
  
  whatisR <- rawData[rawData$가상카드번호 %in% ID, ]
  #filter , subset
  
  whatisR <- whatisR %>% arrange(승차일시)
  
  TorF <- na.omit(whatisR$승차일시[]!="~" & whatisR$하차일시[]!="~")
  TorF[1]
  if(is.na(TorF[1])){
    
    whatisR$BT <- posixTrans(whatisR$승차일시)
    whatisR$AT <- posixTrans(whatisR$하차일시)
    
    
    
    #**한줄짜리는 날리자
    if(nrow(whatisR)>1){
      
      totalTable <- rbind(totalTable,whatisR)
      
    }
  
    print(paste0("! proceeding to analysis: ",where,"/",length(unqCardIdList)))
    where <- where + 1
  }
}



#? filter로 하면 더 빠르지 않을까???,,,

# "qfGu2OxK5+CI/DJJRgrdnl/0bNZua78trm+Ov6LoKEo="

#> 2.2.3.2. 우선 이 중에서 하자 415->0415 이후 0415-> 9111835

head(totalTable)

sbMbus <- c("9111835","9011835")

mtrNbusTable <- totalTable[totalTable$승차정류장ID_교통사업자 %in% sbMbus, ]

ll <- unique(mtrNbusTable$가상카드번호)

totalTable2 <- totalTable[totalTable$가상카드번호 %in% ll,]

#0418 환승횟수0 && 9111835 환승회수1
totalTable3 <- data.frame()

for(ID in unique(totalTable2$가상카드번호)){
  
  #ID <- unique(totalTable2$가상카드번호)[8]
  
  TT <- totalTable2[totalTable2$가상카드번호 %in% ID, ]
  
  temp  <- TT[TT$하차정류장ID_정산사업자 %in% "0418",]
  temp2 <- TT[TT$승차정류장ID_교통사업자 %in% sbMbus, ]
  tempF <- rbind(temp,temp2)
  
  if(tempF$환승횟수[2]-tempF$환승횟수[1] == 1){
    
    totalTable3 <- rbind(totalTable3,tempF)
  }
  
    
}
#중간 save
write.csv(totalTable3,file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/",
                                  "2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/중간save/",
                                  "01_tableTransferSubBus",analysisDate,".csv"))

totalTable3 <- read.csv(file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/",
                                    "2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/중간save/",
                                    "01_tableTransferSubBus",analysisDate,".csv"))

#clusterig용 여러번 부르기
dateList <- c("0516","0517","0518","0519")

totalTable3 <- data.frame()

for(dt in dateList){
  
  temp <- read.csv(file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/",
                                      "2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/중간save/",
                                      "01_tableTransferSubBus",dt,".csv"))
  totalTable3 <- rbind(totalTable3,temp)
  
  rm(temp)
}



nrow(totalTable3)

#하루에 성신여대에 1000명 가량 승차하는데 그중에 300정도가 성신여대서 오는 거면 충분히??! feeder bus?
#기점 정보를 쓸 수 있을까?(rawData 다시 만져야함)

for(ID in unique(totalTable3$가상카드번호)){
  
  #ID <- unique(totalTable2$가상카드번호)[1]
  
  yyy <- totalTable3[totalTable3$가상카드번호 %in% ID,]
  nrow(yyy)
  if(nrow(yyy)>=3){
    print(paste0("WARNING!: ",ID))
    unique(yyy$가상카드번호)
    which(totalTable3$가상카드번호 == unique(yyy$가상카드번호))
    
    tempTransfer <- totalTable3[-which(totalTable3$가상카드번호 == unique(yyy$가상카드번호)),]
    
  }
  
}
nrow(tempTransfer)

#tttt <- totalTable3[totalTable3$가상카드번호 %in% "MW+fcj3mbAaQzrQjN2OaZmTga6OELmlltOgz0NNTdjU=", ]

tttt <- tttt %>% arrange(BT)

tempTransfer <- rbind(tempTransfer,tttt[1:2,],tttt[3:4,])


#1. tempTransfer 만
bTtTaT_table <- data.frame()

tableTransferSubBus <- tempTransfer
for(ID in unique(tableTransferSubBus$가상카드번호)){
  
  #ID <- tableTransferSubBus$가상카드번호[1]
  temp <- tableTransferSubBus[tableTransferSubBus$가상카드번호 %in% ID,]
  if(nrow(temp)==2){
    
    temp <- temp %>% arrange(BT)
    temp2 <- data.frame("가상카드번호"=unique(temp$가상카드번호),
                        "승차1"=temp$승차정류장ID_교통사업자[1],"BT1"=temp$BT[1],
                        "하차1"=temp$하차정류장ID_정산사업자[1],"AT1"=temp$AT[1],
                        "승차2"=temp$승차정류장ID_교통사업자[2],"BT2"=temp$BT[2],
                        "하차2"=temp$하차정류장ID_정산사업자[1],"AT2"=temp$BT[2])
    
  }
  
  bTtTaT_table <- rbind(bTtTaT_table,temp2)
  
  
  
}

head(bTtTaT_table)

#2. warning 뜬 ID로만

#일단 하나라서 스킵


#>===============================================
#> 3. 5번으로는 안 된다...카드  OD로 만들기

# 3.1. 전체 시간
str(bTtTaT_table)
nrow(bTtTaT_table)



ggplot(data=bTtTaT_table, aes(as.POSIXct(AT1), as.POSIXct(BT2)))+geom_point()


# 3.2. 오전 타임
analysisDate

critTime1 <- as.POSIXct(paste0("2017-",substr(analysisDate,1,2),"-",substr(analysisDate,3,4)," 06:00:00"))
critTime2 <- as.POSIXct(paste0("2017-",substr(analysisDate,1,2),"-",substr(analysisDate,3,4)," 12:00:00"))

peakAM <- filter(bTtTaT_table, critTime1 <= bTtTaT_table$BT2 & bTtTaT_table$BT2 <= critTime2) 

available_transferTime <- 3600
peakAM$gap <- as.integer(as.POSIXct(peakAM$BT2) - as.POSIXct(peakAM$AT1))
peakAM <- filter(peakAM,peakAM$gap<=available_transferTime )
nrow(peakAM)

#y=x
yx <- data.frame("AT1" = c(critTime1,critTime2),
                 "BT1" = c(critTime1,critTime2))

ggplot(data=peakAM, aes(as.POSIXct(AT1), as.POSIXct(BT2)))+geom_point()+
  geom_line(data=yx, aes(AT1,BT1),color="red")


# 3.3. 환승시간(ET Escape TIme)

peakAM$gap <- as.integer(as.POSIXct(peakAM$BT2) - as.POSIXct(peakAM$AT1))

gapTable <- table(peakAM$gap)

gapTable <- data.frame(gapTable)

hist((peakAM$gap/60), breaks=60, main=paste0("Distribution of Escape Time: ",critTime1,"~",critTime2),
      xlim=c(0,max(peakAM$gap/60))) 

#fiveA <- fivenum((peakAM$gap/60))
sumA <- summary((peakAM$gap/60))
print(sumA)

#uuu <- rawDataReal[rawDataReal$가상카드번호 %in% "PxxLed3wzsWnUJWEFSvZhEa3TEKsmjyKcouFAVhMacI=", ]




#>===============================================
#> 4. 다 필요하겠다 

head(rawDataReal)

boardingListSungshin <- c("9011835","9111835")

onlyBusB <- rawDataReal[rawDataReal$승차정류장ID_교통사업자 %in% boardingListSungshin, ]

#head(onlyBusB)

bus1 <- onlyBusB[onlyBusB$차량등록번호 %in% unique(onlyBusB$차량등록번호)[1], ]

str(bus1)

unique(bus1$운행출발일시)

#일단 전체
onlyBusB <- filter(onlyBusB,onlyBusB$하차일시!="~")

onlyBusB$BT <- posixTrans(onlyBusB$승차일시)
onlyBusB$AT <- posixTrans(onlyBusB$하차일시)

critTime <- as.POSIXct("2017-05-20 12:00:00")
onlyBusB1 <- filter(onlyBusB, onlyBusB$BT <= critTime)

ggplot(data=onlyBusB1, aes(BT,AT)) + geom_point() #  별 의미가없다...


#>===============================================
#> 5. 성신여대 하차 / 성신여대->성북04 / 성북04 승차

# 5.1. 성신여대 하차만
head(rawDataReal)

sungshin_alighting <- rawDataReal[rawDataReal$하차정류장ID_정산사업자 %in% "0418", ]

sungshin_alighting$BT <- posixTrans(sungshin_alighting$승차일시)
sungshin_alighting$AT <- posixTrans(sungshin_alighting$하차일시)

#05:00:00 -> 300min
sungshin_alighting$HM.B <- as.integer(substr(sungshin_alighting$BT,12,13))*60 + as.integer(substr(sungshin_alighting$BT,15,16))
sungshin_alighting$HM.A <- as.integer(substr(sungshin_alighting$AT,12,13))*60 + as.integer(substr(sungshin_alighting$AT,15,16))
#sungshin_alighting$HM.A <- as.integer(paste0(substr(sungshin_alighting$AT,12,13),substr(sungshin_alighting$AT,15,16)))

#하차 분포
pA <- ggplot(sungshin_alighting, aes(x = HM.A, y=..density..)) +
  theme_minimal()

pA + geom_histogram(aes(HM.A), binwidth=15, color="black", fill="gray") +
  geom_line(stat="density", adjust=2, color="red",lwd = 1.1) +
  geom_vline(xintercept = c(360,720,1080,1440), lty = 2, color="blue" ) +
  labs(x = "시간(분)", y = "count(density)")

ggplot(sungshin_alighting, aes(x = HM.A)) +
  theme_minimal() +
  geom_histogram(aes(HM.A), binwidth=15, color="black", fill="gray")



#승차 분포
pB <- ggplot(sungshin_alighting, aes(x = HM.B, y=..density..)) +
  theme_minimal()

pB + geom_histogram(aes(HM.B), binwidth=15, color="black", fill="gray") +
  geom_line(stat="density", adjust=2, color="blue",lwd = 1.1)


#합쳐서 그리기



#plotNormalHistogram(sungshin_alighting$HM.A, prob=FALSE,
#                    main="distribution", length=100)





# 5.2. 10분 간격
# 쓸어담을 table 만들기
#해결


for(i in 1:nrow(sungshin_alighting)){
  
  
}

# 5.3. 시간대별 성신여대 하차 중 성북04 환승률
# 성신여대 하차 승객 중 성북04를 얼마나 타러가나?
nrow(totalTable3)


{   # 5.4. 시간대별 성북 04 승차 중 환승한 성북04
    # 성북 04 승차 승객 중 환승해서 온 애들
    sbMbus <- c("9111835","9011835")
    sungshin_boarding <- rawDataReal[rawDataReal$승차정류장ID_교통사업자 %in% sbMbus, ]
    
    # 하차태그 안 한 사람 제외 (성신여대(성북04)-> 어디론가..)
    sungshin_boarding <- filter(sungshin_boarding, sungshin_boarding$하차일시 !="~")
    
    
    
    sungshin_boarding$BT <- posixTrans(sungshin_boarding$승차일시)
    sungshin_boarding$AT <- posixTrans(sungshin_boarding$하차일시)
    
    sungshin_boarding$HM.B <- as.integer(substr(sungshin_boarding$BT,12,13))*60 + as.integer(substr(sungshin_boarding$BT,15,16))
    sungshin_boarding$HM.A <- as.integer(substr(sungshin_boarding$AT,12,13))*60 + as.integer(substr(sungshin_boarding$AT,15,16))
}

#view(sungshin_boarding)

p <- ggplot(sungshin_boarding, aes(x = HM.B, y=..density..)) +
  theme_minimal()

p + geom_histogram(aes(HM.B), binwidth=15, color="black", fill="gray") +
  geom_line(stat="density", adjust=2, color="blue",lwd = 1.1)+
  geom_vline(xintercept = c(360,720,1080,1440), lty = 2, color="blue" ) +
  labs(x = "시간(분)", y = "count(density)")


#>===============================================
#> 6. 진짜 분석해보기
#

# 6.1.1. 성북04 기존 시간표불러오기 17:00~19:00 저녁 첨두

sb04timeTable <- read.csv(paste0(dataPath,"/","sb04_timeTable-PMpeak-220527.csv"))

#view(sb04timeTable)

busDepTable <- data.frame()

donamList <- c(2,4,6,8,10,12,14)
r<-1
for(r in 1:nrow(sb04timeTable)){
  
  c<-2
  for(c in donamList){
    
    busDepTable <- rbind(busDepTable,sb04timeTable[r,c])

  }
  
}
colnames(busDepTable) <- "depTime"

# 6.1.2 sungshin_boarding

head(sungshin_boarding)

unique(sungshin_boarding$하차정류장ID_정산사업자)

candidateMostAlight <- data.frame(table(sungshin_boarding$하차정류장ID_정산사업자)) %>% arrange(desc(Freq)) 

nrow(sungshin_boarding)
sum(candidateMostAlight$Freq)

# 6.2. 출발시각을 고정시키고 카드 데이터를 매칭 시켜보기 **

busDepTable <- filter(busDepTable,busDepTable$depTime != "" & busDepTable$depTime != "종료")

busDepTable_obs <- as.data.frame(busDepTable[busDepTable$depTime >= "15:00" & busDepTable$depTime <= "23:00",])

colnames(busDepTable_obs) <- "depTime"

busDepTable_obs$depTime2 <- as.POSIXct(paste0("2017-",substr(analysisDate,1,2),"-",substr(analysisDate,3,4)," ",
                                          substr(busDepTable_obs$depTime,1,2),":", substr(busDepTable_obs$depTime,4,5),":","00" )) 


#승차  clustering 확인

#sungshin_boarding_obs <- sungshin_boarding[sungshin_boarding$하차정류장ID_정산사업자 %in% candidateMostAlight[2,1], ]
sungshin_boarding_obs <- sungshin_boarding


#for Clustering
sungshin_boarding_obs$btAll <- substr(sungshin_boarding_obs$BT,12,19)
sungshin_boarding_obs$atAll <- substr(sungshin_boarding_obs$AT,12,19)

bPlot <- sungshin_boarding_obs[sungshin_boarding_obs$BT >= paste0("2017-",substr(analysisDate,1,2),"-",substr(analysisDate,3,4)," 17:00:00")&
                             sungshin_boarding_obs$BT <= paste0("2017-",substr(analysisDate,1,2),"-",substr(analysisDate,3,4)," 19:00:00"),]

# bPlot <- sungshin_boarding_obs[sungshin_boarding_obs$btAll >= paste0("17:00:00")&
#                                  sungshin_boarding_obs$btAll <= paste0("19:00:00"),]


str(bPlot)

critTime1 <- as.POSIXct(paste0("2017-",substr(analysisDate,1,2),"-",substr(analysisDate,3,4)," 17:00:00"))
critTime2 <- as.POSIXct(paste0("2017-",substr(analysisDate,1,2),"-",substr(analysisDate,3,4)," 19:00:00"))

#y=x
yx <- data.frame("AT1" = c(critTime1,critTime2),
                 "BT1" = c(critTime1,critTime2))
str(yx)

p <- ggplot(bPlot, aes(x = BT, y = AT, color=차량등록번호)) +
  theme_minimal()

p + geom_point(size=3)+  geom_line(data=yx, aes(AT1,BT1),color="red")
  geom_vline(xintercept = paste0("2017-",substr(analysisDate,1,2),"-",substr(analysisDate,3,4)," ",busDepTable$depTime,":00"),
             color="red", lty = 2)+



view(sungshin_boarding_obs)

#>===============================================

#>====================하지마===========================
#> 3. 매칭시킬 지하철 시공도 그리기 = 굳이 그릴 필요가 있을까?








#>===============================================
#> 4. 19년도 데이터로 몇 퍼센트가 왔는지
#> 그 때 가정은 태그 뒤쪽부터? 아니면 랜덤? 가장 보수적으로?
#> BSTS ||  ARIMA



#>===============================================
#> 5. 성북04 시공도 그려보기
#> 5.1. 기점 출발시각
dataPath <- "C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/data/"

sb04 <- read.csv(file=paste0(dataPath,"sb04_timetable.csv"))

head(sb04)

str(sb04) 

#> 5.2. 거리표
distTable <- read.csv(file=paste0(dataPath,"성북04_정류장정보.csv"))
head(distTable)
str(distTable)

#> 5.3. 시공도 그리기

timeZone <- distTable$소요시간.올림
cumTimeZone <- data.frame()
#누적소요시간으로!!
i<-1
for(i in 1:length(timeZone)){
  
  j<-1
  sum<-0
  for(j in 1:i){
    
    temp <- timeZone[j]
    sum <- sum+temp
  }
  cumTimeZone <- rbind(cumTimeZone,sum)
  
}
#cumTimeZone[-nrow(cumTimeZone),]


distZone <- distTable$정류장누적거리

plotTable <- data.frame("거리"=distZone,"시간"=cumTimeZone$X0)
#plotTable[-nrow(plotTable),]

DWtime <- 1/6 # 60초/3 1분/3

plotTable2 <- plotTable
plotTable2$시간 <- plotTable2$시간 + (DWtime)

plotTable <- rbind(plotTable,plotTable2)

head(plotTable)

plotTable <- plotTable[complete.cases(plotTable[,c("거리")]),]

ggplot(plotTable,aes(시간,거리)) + geom_line()



#>===============================================
#> 7. tripchain 

dddd <- fread(file=paste0("T:/05_Smart_Card_Data/2016-2017_SNU/트립체인 2016/1-2.트립체인_20161017/1-2.트립체인_20161017.csv"))

head(dddd)

rm(dddd)
gc()

#>===============================================

stationNO <- fread(paste0(rawPath,"/00_recieved/COMMON_CD/","CD_TFCMN.dat"))

head(stationNO)


