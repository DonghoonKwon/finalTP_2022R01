
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

# for clustering
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

##iteration

#stopList <- c("108000004", "108000008", "108000012", "107000043", "107000046", "107000052")
stopList <- c( "108000012", "107000043", "107000046", "107000052")
# stationList <- c("0414","0415","0416","2642","2643","2644")
stationList <- c("0416","2642","2643","2644")

Sys.time()
for(iteration in stopList){
  
  iterationForStation <- 1
  
      boarding_Suyu <- filter(rawDataAnl,rawDataAnl$승차정류장ID_국토부표준 == iteration )
      
      #> 2.3. 그 중에서 4호선으로 올라온애 === totalTable
      
      #length(unique(boarding_Suyu$가상카드번호))
      
      totalTable <- data.frame()
      where <- 1
      for(ID in unique(boarding_Suyu$가상카드번호)){
        
        temp <- filter(rawDataAnl,rawDataAnl$가상카드번호 == ID)
        
        temp <- temp %>% arrange(승차일시)
        
        totalTable <- rbind(totalTable,temp)
        
        rm(temp)
        
        #print(paste0(where,"/",length(unique(boarding_Suyu$가상카드번호))))
        where <- where + 1
      }
      #totalTable
      
      #> 2.4. 중간 save
      
      write.csv(totalTable,file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/",
                                       "2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/120번_버스/",
                                       "totalTable-",iteration,"-",analysisDate,".csv"))
      
      
      
      iterationForStation <- iterationForStation + 1    
      
      print(paste0(Sys.time(),"  ", iteration))
}
      
      #> 2.5. 수유 승차 중... (분석) === tripChainTable
      
      #view(totalTable)
      
      subAlight_busBoarding <- filter(totalTable, totalTable$하차정류장ID_정산사업자 == stationList[iterationForStation])
      
      tripChainTable <- data.frame()
      
      
      for(ID in unique(subAlight_busBoarding$가상카드번호)){
        
        
        temp <- totalTable[totalTable$가상카드번호 %in% ID, ]
        
        tempA1  <- temp[temp$하차정류장ID_정산사업자 %in% stationList[iterationForStation],]
        tempB2 <- temp[temp$승차정류장ID_국토부표준 %in% iteration, ]
        tempAB <- rbind(tempA1,tempB2)
        
        tempAB <- tempAB %>% arrange(승차일시)
        
        if(tempAB$환승횟수[2]-tempAB$환승횟수[1] == 1){
          
          tripChainTable <- rbind(tripChainTable,tempAB)
          
        }
        
        rm(tempAB)
      }
      
      
      #> 2.6. 아이디 별로 붙이기 === totalTrpChainTable 
      
      
      #> 2.6.1. 헐..., 105번만 골라내기 111006400
      
      
      #nrow(tripChainTable)
      
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
          
          #print(paste0("!: ",nrow(temp)))
          
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
          
          #print(paste0("!: ",nrow(temp)))
          rm(tempSum)
          
          
        }else{
          
          #print(paste0("! ", ID))
          
          reCheckneedList <- rbind(reCheckneedList,ID)
          
        }
        
        
        
      }
      
      totalTrpChainTable <- filter(totalTrpChainTable,totalTrpChainTable$교통사업자ID == "111006400")#150번만
      totalTrpChainTable <- filter(totalTrpChainTable,totalTrpChainTable$하차2 != "~" )
      
      
      
      totalTrpChainTable$BT1 <- posixTrans(totalTrpChainTable$승차1)
      totalTrpChainTable$AT1 <- posixTrans(totalTrpChainTable$하차1)
      totalTrpChainTable$BT2 <- posixTrans(totalTrpChainTable$승차2)
      totalTrpChainTable$AT2 <- posixTrans(totalTrpChainTable$하차2)
      
      write.csv(totalTrpChainTable,file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/",
                                       "2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/120번_버스/",
                                       "totalTrpChainTable-",iteration,"-",analysisDate,".csv"))
      

      rm(totalTrpChainTable)

      
      


#>====================================================
#>====================================================
#>====================================================












#> 2.7. 본격 분석 (totalTrpChainTable를 이용하여)
head(totalTrpChainTable)

critTime1 <- as.POSIXct(paste0("2017-",substr(analysisDate,1,2),"-",substr(analysisDate,3,4)," 17:00:00"))
critTime2 <- as.POSIXct(paste0("2017-",substr(analysisDate,1,2),"-",substr(analysisDate,3,4)," 19:00:00"))

yx <- data.frame("AT1" = c(critTime1,critTime2),
                 "BT1" = c(critTime1,critTime2))

trpChanByTime <- filter(totalTrpChainTable, totalTrpChainTable$AT1>=critTime1 & totalTrpChainTable$AT2<=critTime2)

ggplot(trpChanByTime, aes(AT1,BT2, color=차량등록번호)) +
  geom_point(size=2,shape=3) +
  geom_text(aes(label=(차량등록번호)), check_overlap = T, size=3,
            vjust = -1, nudge_y = 0.5) +
  geom_line(data=yx, aes(AT1,BT1),color="red")


#> 2.8. 여기에 그냥 일반 탑승자





#> 2.9. 이거하고 돌려이제 그만해!!!!!!!! 한 역에 대해서 대기시간(BM) 돌려봐
