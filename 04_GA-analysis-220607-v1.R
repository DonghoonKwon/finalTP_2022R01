#install.packages("GA")

library(GA)


#================================================================================#

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

#================================================================================#


drawPlot <- function(tableWTforPlot,gen){#Draw Plot
  
  plotSavePath <- "C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/중간save/fromGA/"
  
  tempTrj <- tableWTforPlot
  plotTrj <- data.frame()
  distTable <- c(18.615,18.615, 24.186,24.186, 26.526,26.526, 29.412,29.412 )
  
  #str(tempTrj)
  
  for(busNum in tempTrj[["tableForGA"]][["차량등록번호"]]){
    
    #busNum <- tempTrj[["tableForGA"]][["차량등록번호"]][1]
    
    where <- which(tempTrj[["tableForGA"]][["차량등록번호"]] == busNum)
    
    temp <- rbind(tempTrj[["tableForGA"]][["ArrT_1"]][where], tempTrj[["tableForGA"]][["DepT_1"]][where],
                  tempTrj[["tableForGA"]][["ArrT_2"]][where], tempTrj[["tableForGA"]][["DepT_2"]][where],
                  tempTrj[["tableForGA"]][["ArrT_3"]][where], tempTrj[["tableForGA"]][["DepT_3"]][where],
                  tempTrj[["tableForGA"]][["ArrT_4"]][where], tempTrj[["tableForGA"]][["DepT_4"]][where])
    temp <- as.POSIXct(temp, origin = "1970-01-01")
    temp2 <- data.frame("Time"=temp)
    temp2$Bus <- busNum
    temp2$Distance <- distTable
    
    plotTrj <- rbind(plotTrj, temp2)
    #plotTrj$Time <- as.POSIXct(plotTrj$Time, origin = "1970-01-01")
  }
  #view(plotTrj)
  
  plotTrj$Time <- as.POSIXct(plotTrj$Time,origin = "1970-01-01")
  
  pA <- ggplot(plotTrj, aes(Time, Distance, color=Bus)) + geom_line(size=1.3) +
    geom_hline(yintercept = c(18.615,24.186,26.526,29.412), lty=3)
  
  ggsave(paste0(plotSavePath,"/",gen,"/fitnessPlot-",gen,".png"),width=24, height=18, units="cm")
  
}

#================================================================================#


###########################################################################
# 4) GA [02_analysis에서 오면...]
###########################################################################

# write.csv(tableWT_scheduled,
#           file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/중간save/forGA/tableWT_scheduled.csv"))

tableWT_scheduled <-read.csv(file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/중간save/forGA/tableWT_scheduled.csv"))

tableWT_scheduled$ArrT_1 <- as.POSIXct(tableWT_scheduled$ArrT_1)
tableWT_scheduled$DepT_1 <- as.POSIXct(tableWT_scheduled$DepT_1)

tableWT_scheduled$ArrT_2 <- as.POSIXct(tableWT_scheduled$ArrT_2)
tableWT_scheduled$DepT_2 <- as.POSIXct(tableWT_scheduled$DepT_2)

tableWT_scheduled$ArrT_3 <- as.POSIXct(tableWT_scheduled$ArrT_3)
tableWT_scheduled$DepT_3 <- as.POSIXct(tableWT_scheduled$DepT_3)

tableWT_scheduled$ArrT_4 <- as.POSIXct(tableWT_scheduled$ArrT_4)
tableWT_scheduled$DepT_4 <- as.POSIXct(tableWT_scheduled$DepT_4)

busList <- tableWT_scheduled$차량등록번호
busList <- busList[-c(1)]


estPath <- "C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/중간save/paxArrivalTableEST/"

boardingStopList <- c("105000447", "107000043","108000012", "108000004" )
analysisDate <- "0517"

savePath <- "C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/중간save/fromGA/"

###########################################################################
# I . initial condition 50 개
###########################################################################
totalChromosome <- vector(mode="list", length=50)

#tableForDP <- tableWT_scheduled

gen <- 0

chromo <- 1

for(chromo in 1:50){
      # chromosome setting
      tableForGA <- tableWT_scheduled
      
      #str(tableForGA)
      
      #shifting <- c(-60:60)
      #dwellTime <- c(10:60)
      
      #shiftGene <- sample(x=-60:60, size = 80, replace=T)
      #dwellGene <- sample(x= 10:60, size = 80, replace=T)
      
      # 3142634211356211345621136521135621135235252513612136521135612135112312631235
      shiftGene <- sample(x=c(-60,-50,-40,-30,-20,-10,0,10,20,30,40,50,60), size = 80, replace=T)
      dwellGene <- sample(x=c(10,20,30,40,50,60), size = 80, replace=T)
      
      r<-1
      for(r in 1:20){
        
        
        tableForGA$ArrT_1[r+1] <- tableWT_scheduled$ArrT_1[r+1] + seconds(shiftGene[ (4*r)-3 ])
        tableForGA$DepT_1[r+1] <- tableForGA$ArrT_1[r+1] + seconds(dwellGene[ (4*r)-3 ])
        
        tableForGA$ArrT_2[r+1] <- tableWT_scheduled$ArrT_2[r+1] + seconds(shiftGene[ (4*r)-2 ])
        tableForGA$DepT_2[r+1] <- tableForGA$ArrT_2[r+1] + seconds(dwellGene[ (4*r)-2 ])
        
        tableForGA$ArrT_3[r+1] <- tableWT_scheduled$ArrT_3[r+1] + seconds(shiftGene[ (4*r)-1 ])
        tableForGA$DepT_3[r+1] <- tableForGA$ArrT_3[r+1] + seconds(dwellGene[ (4*r)-1 ])
        
        tableForGA$ArrT_4[r+1] <- tableWT_scheduled$ArrT_4[r+1] + seconds(shiftGene[ (4*r) ])
        tableForGA$DepT_4[r+1] <- tableForGA$ArrT_4[r+1] + seconds(dwellGene[ (4*r) ])
        
        
        
      }
      
      #>
      tableForGA$DwellT_1 <- difftime(tableForGA$DepT_1,tableForGA$ArrT_1,unit="sec")
      tableForGA$DwellT_2 <- difftime(tableForGA$DepT_2,tableForGA$ArrT_2,unit="sec")
      tableForGA$DwellT_3 <- difftime(tableForGA$DepT_3,tableForGA$ArrT_3,unit="sec")
      tableForGA$DwellT_4 <- difftime(tableForGA$DepT_4,tableForGA$ArrT_4,unit="sec")
      
      r<-1
      for(r in 1:20){
        
        tableForGA$h1[r+1] <- difftime(tableForGA$ArrT_1[r+1],tableForGA$DepT_1[r],unit = "sec")  
        tableForGA$h2[r+1] <- difftime(tableForGA$ArrT_2[r+1],tableForGA$DepT_2[r],unit = "sec")  
        tableForGA$h3[r+1] <- difftime(tableForGA$ArrT_3[r+1],tableForGA$DepT_3[r],unit = "sec")  
        tableForGA$h4[r+1] <- difftime(tableForGA$ArrT_4[r+1],tableForGA$DepT_4[r],unit = "sec")  
        
      }
      
      # waiting time 계산
      
      
      
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
            where <- which(colnames(tableForGA)=="ArrT_4")
            #Hwhere <- which(colnames(tableForGA)=="h4")
            
          }else if(unique(estArr_nonTransfer$승차역) == "108000012"){
            # 미아사거리역
            where <- which(colnames(tableForGA)=="ArrT_3")
            #Hwhere <- which(colnames(tableForGA)=="h3")
            
          }else if(unique(estArr_nonTransfer$승차역) == "107000043"){
            # 월곡역
            where <- which(colnames(tableForGA)=="ArrT_2")
            #Hwhere <- which(colnames(tableForGA)=="h2")
            
          }else if(unique(estArr_nonTransfer$승차역)== "105000447"){
            # 청량리역
            where <- which(colnames(tableForGA)=="ArrT_1")
            #Hwhere <- which(colnames(tableForGA)=="h1")
            
          }
          
        }
        
        #view(tableForGA)
        
        ttemp <- calculatingWaitingTime(estArr_Transfer,estArr_nonTransfer,tableForGA,where)
        resulstTable <- rbind(resulstTable, ttemp)
        
      }
      totalChromosome[[chromo]] <- list("shiftGene"=shiftGene,
                                        "dwellGene"=dwellGene,
                                        "tableForGA"=tableForGA,
                                        "resulstTable"=resulstTable,
                                        "Fit"=sum(resulstTable))
      
      print(chromo)
}

#View(totalChromosome)

min <- 99999999999
minPoint <- 0
for(where in 1:50){
  
  if(min > totalChromosome[[where]][["Fit"]]){
    
    min <- totalChromosome[[where]][["Fit"]]
    minPoint <- where
  }
  
}
print(paste0(minPoint," ", round(min,2)))
{
  saveforResPath <- paste0(savePath,gen)
  
  if(!dir.exists(saveforResPath)){
    
    dir.create(saveforResPath)
    
    write.csv(totalChromosome[[minPoint]][1],
              file=paste0(saveforResPath,"/shift-gen-",gen,".csv"))
    write.csv(totalChromosome[[minPoint]][2],
              file=paste0(saveforResPath,"/dwellTime-gen-",gen,".csv"))
    write.csv(totalChromosome[[minPoint]][3],
              file=paste0(saveforResPath,"/wholeTimeTable-gen-",gen,".csv"))
    write.csv(totalChromosome[[minPoint]][4],
              file=paste0(saveforResPath,"/waitingTime-gen-",gen,".csv"))
    write.csv(totalChromosome[[minPoint]][5],
              file=paste0(saveforResPath,"/Fit-gen-",gen,".csv"))
    
  }else{
    
    write.csv(totalChromosome[[minPoint]][1],
              file=paste0(saveforResPath,"/shift-gen-",gen,".csv"))
    write.csv(totalChromosome[[minPoint]][2],
              file=paste0(saveforResPath,"/dwellTime-gen-",gen,".csv"))
    write.csv(totalChromosome[[minPoint]][3],
              file=paste0(saveforResPath,"/wholeTimeTable-gen-",gen,".csv"))
    write.csv(totalChromosome[[minPoint]][4],
              file=paste0(saveforResPath,"/waitingTime-gen-",gen,".csv"))
    write.csv(totalChromosome[[minPoint]][5],
              file=paste0(saveforResPath,"/Fit-gen-",gen,".csv"))
    
    
  }
}# initial set 완료 gen==0


tableWTforPlot <- totalChromosome[[minPoint]][3]

drawPlot(tableWTforPlot,gen)






###########################################################################
#  ITERATION START!!
###########################################################################
maxIteration <- 100

crossOverProb <- 0.8

mutationProb <- 0.05
###



parents <- totalChromosome

gen <-1

print(paste0("Start!: ",Sys.time()))

for(gen in 1:maxIteration){
  
  ###########################################################################
  # II . Selection ; fitness 확률로 50개 될때까지 뽑기 근데...infeasible 한 경우는 없어서 검증은 따로 필요 없을듯
  # 그리고 일단은 single objective
  ###########################################################################
  
  nextOffspring <- vector(mode="list", length=50)
  # 1~50 서로 다른 두 숫자 50번 뽑기
  for(crane in 1:50){
    
    ranNum <- sample(1:50, 2, replace = F) 
    
    offCandid1 <- parents[[ranNum[1]]]
    offCandid2 <- parents[[ranNum[2]]]
    
    if(offCandid1[["Fit"]] < offCandid2[["Fit"]]){
      
      nextOffspring[[crane]] <- offCandid1
      
    }else{
      
      nextOffspring[[crane]] <- offCandid2
      
    }
    
  }#for(crane in 1:50)
  
  print(paste0("generation: ",gen,"/",maxIteration," || II. Selection Completed! ||",Sys.time()))
  ###########################################################################
  # III . Cross-Over 0.8
  ###########################################################################
  crossOverList <- sample(1:50, 50, replace = F) 
  
  
  couple <- 1
  for(couple in 1:50){
    if(couple %% 2 !=0){# 
        crsIndex1 <-crossOverList[couple]
        crsIndex2 <-crossOverList[couple+1]
        
        
      #1.할래말래
      if(runif(1)<crossOverProb){
        
        #
        tempShift1 <- nextOffspring[[crsIndex1]][["shiftGene"]]
        tempShift2 <- nextOffspring[[crsIndex2]][["shiftGene"]]
        
        tempDwell1 <- nextOffspring[[crsIndex1]][["dwellGene"]]
        tempDwell2 <- nextOffspring[[crsIndex2]][["dwellGene"]]
        
        # 뒤를 옮길거임
        shiftCut <- sample(1:19, 1, replace = F) 
        
        #
        moveTo2<- tempShift1[((4*(shiftCut)+1):(length(tempShift1)))]
        moveTo1<- tempShift2[((4*(shiftCut)+1):(length(tempShift2)))]
        
        tempShift1[(4*(shiftCut)+1):(length(tempShift1))] <- moveTo1
        tempShift2[(4*(shiftCut)+1):(length(tempShift2))] <- moveTo2
        
        #length(tempShift1)
        
        #
        shiftCut <- sample(1:19, 1, replace = F) 
        
        #
        moveTo2<- tempDwell1[((4*(shiftCut)+1):(length(tempDwell1)))]
        moveTo1<- tempDwell2[((4*(shiftCut)+1):(length(tempDwell2)))]
        
        tempDwell1[(4*(shiftCut)+1):(length(tempDwell1))] <- moveTo1
        tempDwell2[(4*(shiftCut)+1):(length(tempDwell2))] <- moveTo2
        
        #paste0(tempShift1, tempShift2, tempDwell1, tempDwell2)
        
        
        nextOffspring[[crsIndex1]][["shiftGene"]] <- tempShift1
        nextOffspring[[crsIndex2]][["shiftGene"]] <- tempShift2
        
        nextOffspring[[crsIndex1]][["dwellGene"]] <- tempDwell1
        nextOffspring[[crsIndex2]][["dwellGene"]] <- tempDwell2
        
      }
      
    }
    
  }# crossover
  
  print(paste0("generation: ",gen,"/",maxIteration," || III. CrossOVer Completed! ||",Sys.time()))
  ###########################################################################
  # IV . Mutation 0.05
  ###########################################################################
  
  #shiftGene <- sample(x=c(-60,-50,-40,-30,-20,-10,0,10,20,30,40,50,60), size = 80, replace=T)
  #dwellGene <- sample(x=c(10,20,30,40,50,60), size = 80, replace=T)
  
  
  mu<-1
  for(mu in 1:50){
    if(runif(1)<mutationProb){
      
      tempShift <- nextOffspring[[mu]][["shiftGene"]]
      tempDwell <- nextOffspring[[mu]][["dwellGene"]]
      
      #
      muObj <- sample(1:20, 1, replace = F)
      tempShift[(4*muObj-3):(4*muObj)] <- sample(x=c(-60,-50,-40,-30,-20,-10,0,10,20,30,40,50,60), size = 4, replace=T)
      #tempShift[(4*muObj-3):(4*muObj)] <- sample(x=-60:60, size = 4, replace=T)
      
      #
      muObj <- sample(1:20, 1, replace = F)
      tempDwell[(4*muObj-3):(4*muObj)] <- sample(x=c(10,20,30,40,50,60), size = 4, replace=T)
      #tempDwell[(4*muObj-3):(4*muObj)] <- sample(x=10:60, size = 4, replace=T)
      
      
      ##
      nextOffspring[[mu]][["shiftGene"]] <- tempShift
      nextOffspring[[mu]][["dwellGene"]] <- tempDwell
      
      
      
    }
    
    
  }# mutation
  
  
  print(paste0("generation: ",gen,"/",maxIteration," || IV. Mutation Completed! ||",Sys.time()))
  ###########################################################################
  # V . 위에서 나온 offsprings으로 tableForGA만들고 resultTable까지
  ###########################################################################
  
  #nextOffspring
  ready<-1
  for(ready in 1:50){
    
    shiftGene <- nextOffspring[[ready]][["shiftGene"]]
    dwellGene <- nextOffspring[[ready]][["dwellGene"]]
    
    #shift[1]
    
    tableForGA <- tableWT_scheduled
    
    r<-1
    for(r in 1:20){
      
      
      tableForGA$ArrT_1[r+1] <- tableWT_scheduled$ArrT_1[r+1] + seconds(shiftGene[ (4*r)-3 ])
      tableForGA$DepT_1[r+1] <- tableForGA$ArrT_1[r+1] + seconds(dwellGene[ (4*r)-3 ])
      
      tableForGA$ArrT_2[r+1] <- tableWT_scheduled$ArrT_2[r+1] + seconds(shiftGene[ (4*r)-2 ])
      tableForGA$DepT_2[r+1] <- tableForGA$ArrT_2[r+1] + seconds(dwellGene[ (4*r)-2 ])
      
      tableForGA$ArrT_3[r+1] <- tableWT_scheduled$ArrT_3[r+1] + seconds(shiftGene[ (4*r)-1 ])
      tableForGA$DepT_3[r+1] <- tableForGA$ArrT_3[r+1] + seconds(dwellGene[ (4*r)-1 ])
      
      tableForGA$ArrT_4[r+1] <- tableWT_scheduled$ArrT_4[r+1] + seconds(shiftGene[ (4*r) ])
      tableForGA$DepT_4[r+1] <- tableForGA$ArrT_4[r+1] + seconds(dwellGene[ (4*r) ])
      
      
    }
    #
    tableForGA$DwellT_1 <- difftime(tableForGA$DepT_1,tableForGA$ArrT_1,unit="sec")
    tableForGA$DwellT_2 <- difftime(tableForGA$DepT_2,tableForGA$ArrT_2,unit="sec")
    tableForGA$DwellT_3 <- difftime(tableForGA$DepT_3,tableForGA$ArrT_3,unit="sec")
    tableForGA$DwellT_4 <- difftime(tableForGA$DepT_4,tableForGA$ArrT_4,unit="sec")
    
    #
    r<-1
    for(r in 1:20){
      
      tableForGA$h1[r+1] <- difftime(tableForGA$ArrT_1[r+1],tableForGA$DepT_1[r],unit = "sec")  
      tableForGA$h2[r+1] <- difftime(tableForGA$ArrT_2[r+1],tableForGA$DepT_2[r],unit = "sec")  
      tableForGA$h3[r+1] <- difftime(tableForGA$ArrT_3[r+1],tableForGA$DepT_3[r],unit = "sec")  
      tableForGA$h4[r+1] <- difftime(tableForGA$ArrT_4[r+1],tableForGA$DepT_4[r],unit = "sec")  
      
    }
    
    #
    nextOffspring[[ready]][["tableForGA"]] <- tableForGA
    
    
    ## WT
    
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
          where <- which(colnames(tableForGA)=="ArrT_4")
          #Hwhere <- which(colnames(tableForGA)=="h4")
          
        }else if(unique(estArr_nonTransfer$승차역) == "108000012"){
          # 미아사거리역
          where <- which(colnames(tableForGA)=="ArrT_3")
          #Hwhere <- which(colnames(tableForGA)=="h3")
          
        }else if(unique(estArr_nonTransfer$승차역) == "107000043"){
          # 월곡역
          where <- which(colnames(tableForGA)=="ArrT_2")
          #Hwhere <- which(colnames(tableForGA)=="h2")
          
        }else if(unique(estArr_nonTransfer$승차역)== "105000447"){
          # 청량리역
          where <- which(colnames(tableForGA)=="ArrT_1")
          #Hwhere <- which(colnames(tableForGA)=="h1")
          
        }
        
      }
      
      #view(tableForGA)
      
      ttemp <- calculatingWaitingTime(estArr_Transfer,estArr_nonTransfer,tableForGA,where)
      resulstTable <- rbind(resulstTable, ttemp)
      
    }
    nextOffspring[[ready]][["resulstTable"]] <- resulstTable
    nextOffspring[[ready]][["Fit"]] <- sum(resulstTable)
    

    rm(tableForGA)
    rm(resulstTable)
    
  }# V. 끝
  
  print(paste0("generation: ",gen,"/",maxIteration," || V. Calculation Completed! ||",Sys.time()))
  ###########################################################################
  # VI . 제일 작은 값 저장 + 그림 저장
  ###########################################################################
  
  
  min <- 99999999999
  minPoint <- 0
  for(where in 1:50){
    
    if(min > nextOffspring[[where]][["Fit"]]){
      
      min <- nextOffspring[[where]][["Fit"]]
      minPoint <- where
    }
    
  }
  #print(paste0(minPoint," ", round(min,2)))
  {
    saveforResPath <- paste0(savePath,gen)
    
    if(!dir.exists(saveforResPath)){
      
      dir.create(saveforResPath)
      
      write.csv(nextOffspring[[minPoint]][1],
                file=paste0(saveforResPath,"/shift-gen-",gen,".csv"))
      write.csv(nextOffspring[[minPoint]][2],
                file=paste0(saveforResPath,"/dwellTime-gen-",gen,".csv"))
      write.csv(nextOffspring[[minPoint]][3],
                file=paste0(saveforResPath,"/wholeTimeTable-gen-",gen,".csv"))
      write.csv(nextOffspring[[minPoint]][4],
                file=paste0(saveforResPath,"/waitingTime-gen-",gen,".csv"))
      write.csv(nextOffspring[[minPoint]][5],
                file=paste0(saveforResPath,"/Fit-gen-",gen,".csv"))
      
    }else{
      
      write.csv(nextOffspring[[minPoint]][1],
                file=paste0(saveforResPath,"/shift-gen-",gen,".csv"))
      write.csv(nextOffspring[[minPoint]][2],
                file=paste0(saveforResPath,"/dwellTime-gen-",gen,".csv"))
      write.csv(nextOffspring[[minPoint]][3],
                file=paste0(saveforResPath,"/wholeTimeTable-gen-",gen,".csv"))
      write.csv(nextOffspring[[minPoint]][4],
                file=paste0(saveforResPath,"/waitingTime-gen-",gen,".csv"))
      write.csv(nextOffspring[[minPoint]][5],
                file=paste0(saveforResPath,"/Fit-gen-",gen,".csv"))
      
      
    }
  }
  #draw
  drawPlot(tableWTforPlot = nextOffspring[[minPoint]][3],gen)
  
  ###########################################################################
  # VII . 넘기기 전에 1개 쳐내기 & elitism
  ###########################################################################
  
  #Foolitism
  max <- -99999999999
  maxPoint <- 0
  for(where in 1:50){

    if(max < nextOffspring[[where]][["Fit"]]){

      max <- nextOffspring[[where]][["Fit"]]
      maxPoint <- where
    }

  }
  nextOffspring[[maxPoint]][["Fit"]] <- 9999999999
  # 
  
  #Elitism: parents 우세 25 /
  #elite <- nextOffspring[[minPoint]]
  ###
  # parentsArrange <- data.frame()
  # for(ppp in 1:50){
  #   
  #   temp <- parents[[ppp]][["Fit"]]
  #   parentsArrange <- rbind(parentsArrange,temp)
  #   
  # }
  # colnames(parentsArrange) <- "FitParents"
  # orderingNum <- c(1:50)
  # parentsArrange <- cbind(parentsArrange, orderingNum)
  # 
  # parentsArrange <- parentsArrange %>% arrange(FitParents)
  # 
  # #
  # remember1 <- parentsArrange[1:25,2]
  # 
  # ###
  # offSpringArrange <- data.frame()
  # for(ppp in 1:50){
  #   
  #   temp <- nextOffspring[[ppp]][["Fit"]]
  #   offSpringArrange <- rbind(offSpringArrange,temp)
  #   
  # }
  # colnames(offSpringArrange) <- "FitParents"
  # orderingNum <- c(1:50)
  # offSpringArrange <- cbind(offSpringArrange, orderingNum)
  # 
  # offSpringArrange <- offSpringArrange %>% arrange(FitParents)
  # 
  # #
  # remember2 <- offSpringArrange[1:25,2]
  
  
  ### itr
  
  parents <- nextOffspring
  # poolSide <- vector(mode="list", length=50)
  # 
  # p<-1
  # for(rmb in remember1){
  #   
  #   
  #   rmb <- 1
  #   poolSide[[p]] <- parents[[rmb]]
  #     
  #   p <- p + 1  
  # }
  # 
  # for(rmb in remember2){
  #   
  #   
  #   rmb <- 1
  #   poolSide[[p]] <- nextOffspring[[rmb]]
  #   
  #   p <- p + 1  
  # }
  # 
  # 
  # 
  # parents <- poolSide
  
  rm(nextOffspring)
  #gc()
  #rm(poolSide)
  
  print(paste0("generation: ",gen,"/",maxIteration," || ",Sys.time()))
}#end




#=======================================================================#
#=======================================================================#
#=======================================================================#
#========================                   ============================#
#=================================  ====================================#
#========================                   ============================#
#=======================================================================#
#========================                   ============================#
#=======================================================================#
#========================                   ============================#
#=======================================================================#
#========================                   ============================#
#=================================  ====================================#
#========================                   ============================#
#=======================================================================#
#=======================================================================#
#=======================================================================#
#=======================================================================#

###########################################################################
# VII . fitness value by iteration
###########################################################################

#fitnessValue

fitTranfer <- data.frame()
fitNonTransfer <- data.frame()
fitnessValue <- data.frame()

genOrder <-0
for(genOrder in 0:100){
  
  reading <- read.csv(paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/중간save/fromGA/",genOrder,"/waitingTime-gen-",genOrder,".csv"))
  
  fitTranfer <- rbind(fitTranfer, sum(reading[,2]))
  fitNonTransfer <- rbind(fitNonTransfer, sum(reading[,3]))
  fitnessValue <- rbind(fitnessValue, sum(reading[,2], reading[,3]))
}
genOrderCol <- c(0:100)
category1 <- c(0:100)
category1[] <- "fit"

category2 <- c(0:100)
category2[] <- "transfer"

category3 <- c(0:100)
category3[] <- "bus Only"

fitnessValue <- data.frame("fit"=fitnessValue, "iteration"=genOrderCol,category1 )
colnames(fitnessValue) <- c("fit","iteration","category")

fitTranfer <- data.frame("fit"=fitTranfer, "iteration"=genOrderCol,category2 )
colnames(fitTranfer) <- c("fit","iteration","category")

fitNonTransfer <- data.frame("fit"=fitNonTransfer, "iteration"=genOrderCol,category3 )
colnames(fitNonTransfer) <- c("fit","iteration","category")



totalFitPlot <- rbind(fitnessValue, fitTranfer, fitNonTransfer)

ggplot(totalFitPlot, aes(iteration, fit, color=category))+ geom_line(size=1.5)





ggplot(fitnessValue, aes(iteration,fit),color=)+geom_line()
ggplot(fitTranfer, aes(iteration,fit))+geom_line()
ggplot(fitNonTransfer, aes(iteration,fit))+geom_line()


#=====#


if(runif(1)<0.05){
  
  
}



#====#
saveforResPath <- paste0(savePath,gen)

if(!dir.exists(saveforResPath)){
  
  dir.create(saveforResPath)
  
  write.csv(survivedList[[1]][1],
            file=paste0(saveforResPath,"/shift-gen-",gen,".csv"))
  write.csv(survivedList[[1]][2],
            file=paste0(saveforResPath,"/dwellTime-gen-",gen,".csv"))
  write.csv(survivedList[[1]][3],
            file=paste0(saveforResPath,"/wholeTimeTable-gen-",gen,".csv"))
  write.csv(survivedList[[1]][4],
            file=paste0(saveforResPath,"/waitingTime-gen-",gen,".csv"))
  write.csv(survivedList[[1]][5],
            file=paste0(saveforResPath,"/Fit-gen-",gen,".csv"))
  
}else{
  
  write.csv(survivedList[[1]][1],
            file=paste0(saveforResPath,"/shift-gen-",gen,".csv"))
  write.csv(survivedList[[1]][2],
            file=paste0(saveforResPath,"/dwellTime-gen-",gen,".csv"))
  write.csv(survivedList[[1]][3],
            file=paste0(saveforResPath,"/wholeTimeTable-gen-",gen,".csv"))
  write.csv(survivedList[[1]][4],
            file=paste0(saveforResPath,"/waitingTime-gen-",gen,".csv"))
  write.csv(survivedList[[1]][5],
            file=paste0(saveforResPath,"/Fit-gen-",gen,".csv"))
  
  
}

###########################################################################
# II . Selection ; fitness 확률로 50개 될때까지 뽑기 근데...infeasible 한 경우는 없어서 검증은 따로 필요 없을듯
# 그리고 일단은 single objective
###########################################################################



#gen <- 1

wtRanking <- data.frame()
for(ll in 1:50){
  
  temp <- sum(totalChromosome[[ll]][[4]])
  wtRanking <- rbind(wtRanking, temp)
  
}
colnames(wtRanking)<- "waiting_time"
numbering <- c(1:nrow(wtRanking))
wtRanking <- cbind(wtRanking,numbering)

totalChromosome[which(wtRanking$waiting_time == min(wtRanking$waiting_time))]

wtRanking <- wtRanking %>% arrange(waiting_time)

# 상위 25개를 crossover해서

upper25List <- wtRanking$numbering[1:25]

survivedList <- vector(mode="list", length=50)
od<-1
for(sss in upper25List){
  
  survivedList[[od]] <- totalChromosome[[sss]]
  od <- od + 1
}

# #각각 저장해야겠다 허거걱...그것도 베스트만
# write.csv(totalChromosome[[1]][1],
#           file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/중간save/fromGA/testGA-res.csv"))
# iteration 횟수별


saveforResPath <- paste0(savePath,gen)

if(!dir.exists(saveforResPath)){
  
  dir.create(saveforResPath)
  
  write.csv(survivedList[[1]][1],
            file=paste0(saveforResPath,"/shift-gen-",gen,".csv"))
  write.csv(survivedList[[1]][2],
            file=paste0(saveforResPath,"/dwellTime-gen-",gen,".csv"))
  write.csv(survivedList[[1]][3],
            file=paste0(saveforResPath,"/wholeTimeTable-gen-",gen,".csv"))
  write.csv(survivedList[[1]][4],
            file=paste0(saveforResPath,"/waitingTime-gen-",gen,".csv"))
  write.csv(survivedList[[1]][5],
            file=paste0(saveforResPath,"/Fit-gen-",gen,".csv"))
  
}else{
  
  write.csv(survivedList[[1]][1],
            file=paste0(saveforResPath,"/shift-gen-",gen,".csv"))
  write.csv(survivedList[[1]][2],
            file=paste0(saveforResPath,"/dwellTime-gen-",gen,".csv"))
  write.csv(survivedList[[1]][3],
            file=paste0(saveforResPath,"/wholeTimeTable-gen-",gen,".csv"))
  write.csv(survivedList[[1]][4],
            file=paste0(saveforResPath,"/waitingTime-gen-",gen,".csv"))
  write.csv(survivedList[[1]][5],
            file=paste0(saveforResPath,"/Fit-gen-",gen,".csv"))
  
  
}




###########################################################################
# III . Crossover ; 25 개 25개 elitism = 2
###########################################################################

nrow(survivedList)




###########################################################################
# IV . Crossover ; 25 개 25개 & mutation 가급적이면 함수로 
###########################################################################






###########################################################################
# 번외 . 변경된 시각표 그리기
###########################################################################

plotGA <- totalChromosome[[5]][[3]]

ggplot(plotGA, aes())


{#Draw Plot
  
  tempTrj <- plotGA
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
# BM . Data 기반 시간표 / scheduled 기반 시간표
###########################################################################

{# Data 기반 시간표
  resulstTable <- data.frame()
  resulstTable <- data.frame("transfer_passengers"=0, "non_Transfer_passengers"=0)
    
    tableWT <- read.csv(file=paste0("C:/Users/TLSYSLAB_3_ADMIN/Desktop/donghoon_temp/2022-1/2022W11-수업-교통및물류최적화(Prof.강)/final/중간save/paxArrivalTable/talbeBusArrandDep-basedOnData.csv"))
    
    tableWT$ArrT_1 <- as.POSIXct(tableWT$ArrT_1)
    tableWT$DepT_1 <- as.POSIXct(tableWT$DepT_1)
    
    tableWT$ArrT_2 <- as.POSIXct(tableWT$ArrT_2)
    tableWT$DepT_2 <- as.POSIXct(tableWT$DepT_2)
    
    tableWT$ArrT_3 <- as.POSIXct(tableWT$ArrT_3)
    tableWT$DepT_3 <- as.POSIXct(tableWT$DepT_3)
    
    tableWT$ArrT_4 <- as.POSIXct(tableWT$ArrT_4)
    tableWT$DepT_4 <- as.POSIXct(tableWT$DepT_4)
    
    
    
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
    
}
(resulstTable)




{ # scheduled 시간표 기반
  resulstTable <- data.frame()
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
  
  
}



#===#

chromosome <- data.frame("arrtime"=c())