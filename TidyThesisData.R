rm(list=ls())

library(magrittr)
library(dplyr)
library(reshape2)
library(matrixStats)
library(readr)
library(ggplot2)

### READ IN DATA ###
numericcols <- c(4,20:26,39:40)
classes <- rep("factor",41)
classes[numericcols] <- "numeric"

setwd("C:/Users/Jeff.Bernard/Data/IPUMS")
data <- read.csv("usa_00002.csv", colClasses=classes)
rm(list=c("classes","numericcols"))


### READ IN SHAPEFILE ###
library(rgdal)
library(maptools)
library(RColorBrewer)

NYPUMAs <- readShapeSpatial("tl_2014_36_puma10")
NYCPUMAs <- NYPUMAs[substr(as.character(NYPUMAs@data$NAMELSAD10),1,4) == "NYC-",]
NYCPUMAIDs <- substr(as.character(NYCPUMAs@data$PUMACE10),2,5)
plot(NYCPUMAs,col=brewer.pal(8,"Pastel1"))

rm(NYPUMAs)





aggdata <- data
aggdata %<>% filter(!(YEAR %in% c("1990","2000")),PUMA %in% NYCPUMAIDs)
aggdata$RENTGRS[aggdata$RENTGRS == 0] <- NA
aggdata$RENT[aggdata$RENT == 0] <- NA
aggdata$VALUEH[aggdata$VALUEH == 0] <- NA
aggdata$VALUEH[aggdata$RENTGRS != 0] <- NA
aggdata$OCCSOC[aggdata$OCCSOC == ""] <- NA
aggdata$OCCSOC[aggdata$OCCSOC == "000000"] <- NA
aggdata$OCCSOC %<>% 
  as.character %>% 
  substr(1,3) %>%
  as.factor
levels(aggdata$OCCSOC)[levels(aggdata$OCCSOC) == "47X"] <- "478"

aggdata %<>% droplevels

weightedStats <-
  group_by(aggdata,PUMA,YEAR) %>%
  dplyr::summarise(MHI = weightedMedian(HHINCOME,HHWT),
            MGR = weightedMedian(na.omit(RENTGRS),HHWT[!is.na(RENTGRS)]),
            MCR = weightedMedian(na.omit(RENT),HHWT[!is.na(RENT)]),
            MHV = weightedMedian(na.omit(VALUEH),HHWT[!is.na(VALUEH)]),
            RMB = sum(PERWT[MOVEDIN %in% 1:3])/sum(HHWT),
            OCC = sum(PERWT[OCC %in% 1:3600])/sum(PERWT),
            OSC = sum(PERWT[OCCSOC %in% 1:300])/sum(PERWT),
            EDU = sum(PERWT[EDUC %in% 10:11])/sum(PERWT),
            ED2 = sum(PERWT[EDUC %in% 7:11])/sum(PERWT),
            MNA = weighted.mean(AGE,PERWT),
            MDA = weightedMedian(AGE,PERWT),
            UTF = sum(PERWT[AGE < 35])/sum(PERWT),
            WHT = sum(RACE == 1 & HISPAN == 0)/sum(PERWT))


normedStats <-
  group_by(weightedStats,YEAR) %>%
  arrange(YEAR) %>%
  transmute(PUMA = PUMA,
            MHI = (MHI-mean(MHI))/sd(MHI),
            MGR = (MGR-mean(MGR))/sd(MGR),
            #MCR = (MCR-mean(MCR))/sd(MCR),
            MHV = (MHV-mean(MHV))/sd(MHV),
            RMB = (RMB-mean(RMB))/sd(RMB),
            OCC = (OCC-mean(OCC))/sd(OCC),
            #OSC = (OSC-mean(OSC))/sd(OSC),
            #EDU = (EDU-mean(EDU))/sd(EDU),
            ED2 = (ED2-mean(ED2))/sd(ED2),
            #MNA = -(MNA-mean(MNA))/sd(MNA),
            #MDA = -(MDA-mean(MDA))/sd(MDA),
            UTF = (UTF-mean(UTF))/sd(UTF),
            WHT = (WHT-mean(WHT))/sd(WHT))
normedStats$NDX <- rowSums(normedStats[,-(1:2)])/(ncol(normedStats)-2)


##############################################################
######            PLOTTING VARIABLES ON MAP             ######
##############################################################

melted <- melt(normedStats,id.vars=1:2)

ggplot(filter(melted,PUMA=="4001")) + 
  geom_line(aes(x=YEAR,y=value,group=variable,color=variable)) +
  geom_hline(y=0)

ggplot(filter(melted,variable=="NDX")) + 
  geom_line(aes(x=YEAR,y=value,group=PUMA)) +
  geom_hline(y=0)

longform <- 
  melt(normedStats,id.vars=1:2) %>%
  dcast(PUMA ~ variable + YEAR)
longform$diff <- longform$NDX_2011 - longform$NDX_2005

qplot(longform$diff)
longform$PUMA[which(longform$diff > 0.25)]

threedim <- melt(normedStats,id.vars=1:2) %>% acast(PUMA ~ variable ~ YEAR)

combi <- left_join(longform,PUMAtots)
combi$PUMA %<>% as.factor

library(plyr)
nyc <- readOGR(dsn=".", layer="tl_2014_36_puma10")
nyc <- nyc[substr(as.character(nyc@data$NAMELSAD10),1,4) == "NYC-",]
nyc@data$PUMA <- substr(nyc@data$PUMACE10,2,5) %>% as.factor
nyc@data$id <- rownames(nyc@data)
nyc.points <- fortify(nyc, region="id")
nyc.df <- join(nyc.points, nyc@data, by="id") %>% join(longform, by="PUMA")

ggplot(nyc.df) + geom_polygon(aes(long,lat,group=group,fill=diff)) + 
  coord_equal() + scale_fill_gradientn(colours=rev(brewer.pal(7,"PRGn")))


##############################################################
######    ZCTA BUSINESS PATTERNS ECONOMIC INDICATORS    ######
##############################################################

years<-c("05","06","07","08","09","10","11")

# add totals data
readtotal2 <- function(filename){
  total <- read_csv(paste0("zbp",filename,"totals.txt"))
  if(ncol(total)==7){total <- total[,c(1,4:7)]}
  else              {total <- total[,c(1,5,7,9,10)]}
  names(total)<- totalsnames
  total$YEAR <- rep.int(paste0("20",filename),nrow(total))
  return(total)
}
totalslist2 <- list()
for (i in 1:length(years)) totalslist2[[i]] <- readtotal2(years[i])
aggtotals2 <- do.call(rbind,totalslist2)
aggtotals2$zip %<>% clean.zipcodes %>% as.factor
aggtotals2 %<>% filter(zip %in% NYC) %>% droplevels
aggtotals2$YEAR %<>% as.factor
aggtotals2 <- aggtotals2[,c(1,6,2:5)]

PUMAtots2 <- data.frame(matrix(0,55*7,6))
names(PUMAtots2) <- c("PUMA",names(aggtotals2)[-1])
PUMAtots2$PUMA <- normedStats$PUMA
PUMAtots2$YEAR <- normedStats$YEAR

for(i in 1:nrow(crosswalk)){
  row <- crosswalk[i,]
  sourceZIP <- as.character(row$zcta5)
  targetPUMA <- as.character(row$puma12)
  if(sourceZIP %in% aggtotals2$zip){
    for(yr in paste0("20",years)){
      sourceindex <- which((sourceZIP == aggtotals2$zip) & (yr == aggtotals2$YEAR))
      scaled <- row$afact * aggtotals2[sourceindex,-c(1:2)]
      targetindex <- which((targetPUMA == PUMAtots2$PUMA) & (yr == PUMAtots2$YEAR))
      PUMAtots2[targetindex,-c(1:2)] <- PUMAtots2[targetindex,-c(1:2)] + scaled
    }
  }
}
combi2 <- left_join(PUMAtots2,normedStats)

naivefit.ap <- lm(ap ~ . - YEAR -emp -est -qp1 -NDX, data=combi2)
naivefit.emp <- lm(emp ~ . - YEAR -ap -est -qp1 -NDX, data=combi2)
naivefit.est <- lm(est ~ . - YEAR -emp -ap -qp1 -NDX, data=combi2)
naivefit.qp1 <- lm(qp1 ~ . - YEAR -emp -est -ap -NDX, data=combi2)



library(zipcode)
nyczips <- read.csv("nyczips.csv")
NYC <- as.character(nyczips$x)
rm(nyczips)

totalslist <- list()
totalsnames<- c("zip","emp","qp1","ap","est") #"name","empflag",

readtotal <- function(filename){
  total <- read_csv(paste0("zbp",filename,"totals.txt"))
  if(ncol(total)==7){total <- total[,c(1,4:7)]}
  else              {total <- total[,c(1,5,7,9,10)]}
  names(total)<-paste0(totalsnames,".",filename)
  names(total)[1] <- "zip"
  return(total)
}

library(plyr)
setwd("C:/Users/Jeff.Bernard/Data/zipcode business patterns/")
for (i in 1:length(years)) totalslist[[i]] <- readtotal(years[i])
widetotals<-join_all(totalslist,by="zip")

widetotals$zip %<>% clean.zipcodes %>% as.factor
widetotals %<>% filter(zip %in% NYC) %>% droplevels

crosswalk <- read.csv("../IPUMS/zcta_to_puma_crosswalk.csv")
widetotals$zip[which(!(widetotals$zip %in% crosswalk$zcta5))] %>% as.character %>% write.csv("missingzips.txt",row.names=F)
## hand-code missing zips and re-read file
crosswalk <- read.csv("../IPUMS/zcta_to_puma_crosswalk.csv")

crosswalk$puma12 %<>% as.factor
crosswalk$zcta5 %<>% as.factor
PUMAtots <- data.frame(matrix(0,55,28),row.names = unique(crosswalk$puma12))
names(PUMAtots) <- names(widetotals)[-1]

for(i in 1:nrow(crosswalk)){
  row <- crosswalk[i,]
  if(row$zcta5 %in% widetotals$zip){
    scaled <- row$afact * widetotals[which(widetotals$zip %in% row$zcta5),-1]
    PUMAtots[as.character(row$puma12),] <- PUMAtots[as.character(row$puma12),] + scaled
  }
}

PUMAtots$PUMA <- row.names(PUMAtots)
PUMAtots %<>% 
  mutate(emp.diff = emp.11 - emp.05,
         qp1.diff = qp1.11 - qp1.05,
         ap.diff = ap.11 - ap.05,
         est.diff = est.11 - est.05)

##############################################################
######     OLD/UNUSED CODE FOR OUTPUTTING CLUSTERS      ######
##############################################################

write.csv(weightedStats,"nycgentdata.csv")
ohfive <- weightedStats[weightedStats$YEAR==2005,]
clus05<- kmeans(ohfive,5)
ohsix <- weightedStats[weightedStats$YEAR==2006,]
clus06<- kmeans(ohsix,5)
ohsev <- weightedStats[weightedStats$YEAR==2007,]
clus07<- kmeans(ohsev,5)
oheight <- weightedStats[weightedStats$YEAR==2008,]
clus08<- kmeans(oheight,5)
ohnine <- weightedStats[weightedStats$YEAR==2009,]
clus09<- kmeans(ohnine,5)
ten <- weightedStats[weightedStats$YEAR==2010,]
clus10<- kmeans(ten,5)
eleven <- weightedStats[weightedStats$YEAR==2011,]
clus11<- kmeans(eleven,5)



##############################################################
######     OLD/UNUSED CODE FOR OUTPUTTING JSON FILE     ######
##############################################################

library(jsonlite)

outa <- split(weightedStats[,-1],weightedStats[,1],drop=TRUE)
outb <- lapply(names(outa), makeYRS)
names(outb) <- names(outa)

makeYRS <- function(y){
  sdf <- split(outa[[y]][,-1],outa[[y]][,1],drop=TRUE)
  olst <- list()
  for(yr in sdf){
    ilst <- list()
    for(val in yr){
      ilst <- append(lst,val)
    }
    names(ilst) <- names(yr)  
    olst <- append(olst,list(ilst))
  }
  names(olst) <- names(sdf)
  return(olst)
}

first <- makeYRS("3701")

sink("nycgentdata.json")
cat(toJSON(outb,"rows",pretty=TRUE),file="nycgentdata.json")
cat(toJSON(outb,"rows",pretty=TRUE),file="nycgentdata2.json")

makeList<-function(x){
  if(ncol(x)>2){
    listSplit<-split(x[-1],x[1],drop=T)
    lapply(names(listSplit),function(y){list(name=y,children=makeList(listSplit[[y]]))})
  }else{
    lapply(seq(nrow(x[1])),function(y){list(name=x[,1][y],Percentage=x[,2][y])})
  }
}


jsonOut<-toJSON(list(name="MyData",children=makeList(MyData[-1])))

