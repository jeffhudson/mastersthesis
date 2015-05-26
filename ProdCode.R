rm(list=ls())

library(magrittr)
library(dplyr)
library(reshape2)
library(matrixStats)
library(readr)
library(ggplot2)
theme_set(theme_bw())

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
#plot(NYCPUMAs,col=brewer.pal(8,"Pastel1"))

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
######    ZCTA BUSINESS PATTERNS ECONOMIC INDICATORS    ######
##############################################################

library(zipcode)
setwd("../zipcode business patterns/")
nyczips <- read.csv("nyczips.csv")
NYC <- as.character(nyczips$x)
rm(nyczips)

years<-c("05","06","07","08","09","10","11")

# add totals data
totalslist <- list()
totalsnames<- c("zip","emp","qp1","ap","est")

#############################################################
###### USE FOR LONG DATA FRAME (YEAR = COL, VAR = COL) ######
#############################################################

## read totals files
readtotal2 <- function(filename){
  total <- read_csv(paste0("zbp",filename,"totals.txt"))
  if(ncol(total)==7){total <- total[,c(1,4:7)]}
  else              {total <- total[,c(1,5,7,9,10)]}
  names(total)<- totalsnames
  total$YEAR <- rep.int(paste0("20",filename),nrow(total))
  return(total)
}
# read all totals files and combine into long list
totalslist2 <- list()
for (i in 1:length(years)) totalslist2[[i]] <- readtotal2(years[i])
aggtotals2 <- do.call(rbind,totalslist2)

## clean up factors, drop non-NYC zips and reorder
aggtotals2$zip %<>% clean.zipcodes %>% as.factor
aggtotals2 %<>% filter(zip %in% NYC) %>% droplevels
aggtotals2$YEAR %<>% as.factor
aggtotals2 <- aggtotals2[,c(1,6,2:5)]

# generate placeholder PUMA dataframe
PUMAtots2 <- data.frame(matrix(0,55*7,6))
names(PUMAtots2) <- c("PUMA",names(aggtotals2)[-1])
PUMAtots2$PUMA <- normedStats$PUMA
PUMAtots2$YEAR <- normedStats$YEAR


## read in crosswalk file 

## ~40 zips handcoded; remainder generated from 
##     http://mcdc.missouri.edu/websas/geocorr12.html
##  on April 25, 2015

crosswalk <- read.csv("../IPUMS/zcta_to_puma_crosswalk.csv")
crosswalk$puma12 %<>% as.factor
crosswalk$zcta5 %<>% as.factor

## populate rows of PUMA dataframe using crosswalk to walk data from zip to PUMA
## zips in multiple PUMAs are split by % of population in each PUMA by 2010 census
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
# combine gentrification indicators with business patterns data
combi2 <- left_join(PUMAtots2,normedStats)



################################################################
######         THE ACTUAL DATA ANALYSIS!! (YAYY)          ######
################################################################

## including intercept for each PUMA
naivefit.ap  <- lm(ap ~ . - YEAR -emp -est -qp1 -NDX, data=combi2)  # -PUMA, data=combi2)
naivefit.emp <- lm(emp ~ . - YEAR -ap -est -qp1 -NDX, data=combi2) # -PUMA, data=combi2)
naivefit.est <- lm(est ~ . - YEAR -emp -ap -qp1 -NDX, data=combi2) # -PUMA, data=combi2)
naivefit.qp1 <- lm(qp1 ~ . - YEAR -emp -est -ap -NDX, data=combi2) # -PUMA, data=combi2)

naiverfit.ap  <- lm(ap ~ NDX + PUMA, data=combi2)
naiverfit.emp <- lm(emp ~ NDX + PUMA, data=combi2)
naiverfit.est <- lm(est ~ NDX + PUMA, data=combi2)
naiverfit.qp1 <- lm(qp1 ~ NDX + PUMA, data=combi2)

## All Dummy Variables (All the Time!)
dummy.ap <- lm(ap ~ NDX + PUMA + YEAR, data=combi2)
summary(dummy.ap)$coef[1:2,]
dummy.b.ap <- lm(ap ~ MHI + MGR + MHV + RMB + OCC + ED2 + UTF + WHT + PUMA + YEAR, data=combi2)
summary(dummy.b.ap)$coef[1:9,] # MHI, MHV, WHT, (OCC)
dummy.emp <- lm(emp ~ NDX + PUMA + YEAR, data=combi2)
summary(dummy.emp)$coef[1:2,]
dummy.b.emp <- lm(emp ~ MHI + MGR + MHV + RMB + OCC + ED2 + UTF + WHT + PUMA + YEAR, data=combi2)
summary(dummy.b.emp)$coef[1:9,] # MHI, OCC
dummy.est <- lm(est ~ NDX + PUMA + YEAR, data=combi2)
summary(dummy.est)$coef[1:2,] # NDX
dummy.b.est <- lm(est ~ MHI + MGR + MHV + RMB + OCC + ED2 + UTF + WHT + PUMA + YEAR, data=combi2)
summary(dummy.b.est)$coef[1:9,] # WHT, UTF, (OCC)
dummy.qp1 <- lm(qp1 ~ NDX + PUMA + YEAR, data=combi2)
summary(dummy.qp1)$coef[1:2,]
dummy.b.qp1 <- lm(qp1 ~ MHI + MGR + MHV + RMB + OCC + ED2 + UTF + WHT + PUMA + YEAR, data=combi2)
summary(dummy.b.qp1)$coef[1:9,] #MHI, MHV, WHT

## Clustered Standard Error Approach
CSE.ap <- plm(ap ~ NDX, data=combi2, index=c("PUMA","YEAR"), model="pooling")
clusterSE(CSE.ap, cluster.var = "PUMA")
CSE.ap <- plm(ap ~ MHI + MGR + MHV + RMB + OCC + ED2 + UTF + WHT, data=combi2, index=c("PUMA","YEAR"), model="pooling")
clusterSE(CSE.ap, cluster.var = "PUMA")
CSE.emp <- plm(emp ~ NDX, data=combi2, index=c("PUMA","YEAR"), model="pooling")
clusterSE(CSE.emp, cluster.var = "PUMA")
CSE.emp <- plm(emp ~ MHI + MGR + MHV + RMB + OCC + ED2 + UTF + WHT, data=combi2, index=c("PUMA","YEAR"), model="pooling")
clusterSE(CSE.emp, cluster.var = "PUMA")
CSE.qp1 <- plm(qp1 ~ NDX, data=combi2, index=c("PUMA","YEAR"), model="pooling")
clusterSE(CSE.qp1, cluster.var = "PUMA")
CSE.qp1 <- plm(qp1 ~ MHI + MGR + MHV + RMB + OCC + ED2 + UTF + WHT, data=combi2, index=c("PUMA","YEAR"), model="pooling")
clusterSE(CSE.qp1, cluster.var = "PUMA")
CSE.est <- plm(est ~ NDX, data=combi2, index=c("PUMA","YEAR"), model="pooling")
clusterSE(CSE.est, cluster.var = "PUMA")
CSE.est <- plm(est ~ MHI + MGR + MHV + RMB + OCC + ED2 + UTF + WHT, data=combi2, index=c("PUMA","YEAR"), model="pooling")
clusterSE(CSE.est, cluster.var = "PUMA")

## NDX is highly SS in all; when broken down, only MGR and RMB are consistently SS

## First Differences Approach
FD.ap <- plm(ap ~ NDX, data=combi2, index=c("PUMA","YEAR"), model="fd")
summary(FD.ap)
FD.ap <- plm(ap ~ MHI + MGR + MHV + RMB + OCC + ED2 + UTF + WHT, data=combi2, index=c("PUMA","YEAR"), model="fd")
summary(FD.ap) ## MHI, (RMB, WHT)
FD.emp <- plm(emp ~ NDX, data=combi2, index=c("PUMA","YEAR"), model="fd")
summary(FD.emp)
FD.emp <- plm(emp ~ MHI + MGR + MHV + RMB + OCC + ED2 + UTF + WHT, data=combi2, index=c("PUMA","YEAR"), model="fd")
summary(FD.emp) ## MHI, OCC, WHT (RMB)
FD.qp1 <- plm(qp1 ~ NDX, data=combi2, index=c("PUMA","YEAR"), model="fd")
summary(FD.qp1)
FD.qp1 <- plm(qp1 ~ MHI + MGR + MHV + RMB + OCC + ED2 + UTF + WHT, data=combi2, index=c("PUMA","YEAR"), model="fd")
summary(FD.qp1) ## MHI, WHT
FD.est <- plm(est ~ NDX, data=combi2, index=c("PUMA","YEAR"), model="fd")
summary(FD.est) ## NDX
FD.est <- plm(est ~ MHI + MGR + MHV + RMB + OCC + ED2 + UTF + WHT, data=combi2, index=c("PUMA","YEAR"), model="fd")
summary(FD.est) ## WHT, (UTF)

## Fixed Effects Approach
FE.ap <- plm(ap ~ NDX + YEAR, data=combi2, index=c("PUMA","YEAR"), model="within")
summary(FE.ap)
FE.ap <- plm(ap ~ YEAR + MHI + MGR + MHV + RMB + OCC + ED2 + UTF + WHT, data=combi2, index=c("PUMA","YEAR"), model="within")
summary(FE.ap) ## MHI, -MHV, -WHT
FE.emp <- plm(emp ~ NDX + YEAR, data=combi2, index=c("PUMA","YEAR"), model="within")
summary(FE.emp)
FE.emp <- plm(emp ~ YEAR + MHI + MGR + MHV + RMB + OCC + ED2 + UTF + WHT, data=combi2, index=c("PUMA","YEAR"), model="within")
summary(FE.emp) ## MHI, -OCC
FE.qp1 <- plm(qp1 ~ NDX + YEAR, data=combi2, index=c("PUMA","YEAR"), model="within")
summary(FE.qp1)
FE.qp1 <- plm(qp1 ~ YEAR + MHI + MGR + MHV + RMB + OCC + ED2 + UTF + WHT, data=combi2, index=c("PUMA","YEAR"), model="within")
summary(FE.qp1) ## MHI, -MHV, -WHT
FE.est <- plm(est ~ NDX + YEAR, data=combi2, index=c("PUMA","YEAR"), model="within")
summary(FE.est) ## NDX
FE.est <- plm(est ~ YEAR + MHI + MGR + MHV + RMB + OCC + ED2 + UTF + WHT, data=combi2, index=c("PUMA","YEAR"), model="within")
summary(FE.est) ## WHT, -UTF, (OCC)

## Random Effects Approach
RE.ap <- plm(ap ~ YEAR + NDX, data=combi2, index=c("PUMA","YEAR"), model="random")
summary(RE.ap)
RE.ap <- plm(ap ~ YEAR + MHI + MGR + MHV + RMB + OCC + ED2 + UTF + WHT, data=combi2, index=c("PUMA","YEAR"), model="random")
summary(RE.ap) ## MHI -WHT (RMB) -(MHV)
RE.emp <- plm(emp ~ YEAR + NDX, data=combi2, index=c("PUMA","YEAR"), model="random")
summary(RE.emp)
RE.emp <- plm(emp ~ YEAR + MHI + MGR + MHV + RMB + OCC + ED2 + UTF + WHT, data=combi2, index=c("PUMA","YEAR"), model="random")
summary(RE.emp) ## MHI (RMB) (ED2)
RE.qp1 <- plm(qp1 ~ YEAR + NDX, data=combi2, index=c("PUMA","YEAR"), model="random")
summary(RE.qp1) 
RE.qp1 <- plm(qp1 ~ YEAR + MHI + MGR + MHV + RMB + OCC + ED2 + UTF + WHT, data=combi2, index=c("PUMA","YEAR"), model="random")
summary(RE.qp1) ## MHI -WHT -(MHV) (ED2)
RE.est <- plm(est ~ YEAR + NDX, data=combi2, index=c("PUMA","YEAR"), model="random")
summary(RE.est) ## NDX
RE.est <- plm(est ~ YEAR + MHI + MGR + MHV + RMB + OCC + ED2 + UTF + WHT, data=combi2, index=c("PUMA","YEAR"), model="random")
summary(RE.est) ## WHT OCC -UTF


summary(lm(ap.diff ~ MHI.diff + MGR.diff + MHV.diff + RMB.diff + OCC.diff + ED2.diff + UTF.diff + WHT.diff,data=wideform))
summary(lm(emp.diff ~ MHI.diff + MGR.diff + MHV.diff + RMB.diff + OCC.diff + ED2.diff + UTF.diff + WHT.diff,data=wideform))
summary(lm(est.diff ~ MHI.diff + MGR.diff + MHV.diff + RMB.diff + OCC.diff + ED2.diff + UTF.diff + WHT.diff,data=wideform))
summary(lm(qp1.diff ~ MHI.diff + MGR.diff + MHV.diff + RMB.diff + OCC.diff + ED2.diff + UTF.diff + WHT.diff,data=wideform))


## Hausman tests on full models; only est passes the test
phtest(FE.est,RE.est)
phtest(FE.emp,RE.emp)
phtest(FE.qp1,RE.qp1)
phtest(FE.ap,RE.ap)

rownames(combi2) <- paste0(combi2$PUMA,".",combi2$YEAR)
dummy <- lm(emp ~ MHI + MGR + MHV + RMB + OCC + ED2 + UTF + WHT + PUMA, data=combi2)

residuals <- dummy$residuals
names(residuals) <- sapply(names(residuals), substr, 1,4)
residuals2 <- as.data.frame(residuals)
residuals2$PUMA <- names(residuals) %>% as.factor
residuals2 %<>% group_by(PUMA) %>% dplyr::summarise(res = sum(residuals))

## GWR / MAP RESIDUALS OF OLS
library(plyr)
setwd("../IPUMS/")
nyc <- readOGR(dsn=".", layer="tl_2014_36_puma10")
nyc <- nyc[substr(as.character(nyc@data$NAMELSAD10),1,4) == "NYC-",]
nyc@data$PUMA <- substr(nyc@data$PUMACE10,2,5) %>% as.factor
nyc@data$id <- rownames(nyc@data)
nyc.points <- fortify(nyc, region="id")
nyc.df <- plyr::join(nyc.points, nyc@data, by="id") %>% plyr::join(residuals2, by="PUMA")
nyc.df$resSum %<>% as.character %>% as.numeric
nyc.df$resMean %<>% as.character %>% as.numeric

ggplot(nyc.df) + geom_polygon(aes(long,lat,group=group,fill=res), color="gray35") + 
  coord_equal() + scale_fill_gradientn(colours=brewer.pal(3,"Blues"))


########################################
######        WIDE TOTALS         ######
########################################

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

############################################
######           GWR TESTS            ######
############################################

wideform <- 
  melt(normedStats,id.vars=1:2) %>%
  dcast(PUMA ~ variable + YEAR)
wideform$diff <- wideform$NDX_2011 - wideform$NDX_2005
wideform %<>% join(PUMAtots)
wideform %<>% mutate(MHI.diff = MHI_2011 - MHI_2005,
                     MGR.diff = MGR_2011 - MGR_2005,
                     MHV.diff = MHV_2011 - MHV_2005,
                     RMB.diff = RMB_2011 - RMB_2005,
                     OCC.diff = OCC_2011 - OCC_2005,
                     ED2.diff = ED2_2011 - ED2_2005,
                     UTF.diff = UTF_2011 - UTF_2005,
                     WHT.diff = WHT_2011 - WHT_2005)

library(spdep)
library(spgwr)

addeddata <- join(nyc.points, nyc@data, by="id") %>% join(wideform, by="PUMA")

nyc@data %<>% join(wideform)

bwG <- gwr.sel(ap.diff ~ MHI.diff + MGR.diff + MHV.diff + RMB.diff + OCC.diff + ED2.diff + UTF.diff + WHT.diff, data=nyc, gweight=gwr.Gauss, verbose=TRUE)

gwrG <- gwr(ap.diff ~ MHI.diff + MGR.diff + MHV.diff + RMB.diff + OCC.diff + ED2.diff + UTF.diff + WHT.diff, data=nyc, bandwidth=bwG, gweight=gwr.Gauss)

names(gwrG$SDF)
spplot(gwrG$SDF,"localR2")

############################################################
######   USE FOR WIDE DATA FRAME (VAR x YEAR = COL)   ######
############################################################

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
aggtotals<-join_all(totalslist,by="zip")


aggtotals$zip %<>% clean.zipcodes %>% as.factor
aggtotals %<>% filter(zip %in% NYC) %>% droplevels

crosswalk <- read.csv("../IPUMS/zcta_to_puma_crosswalk.csv")
aggtotals$zip[which(!(aggtotals$zip %in% crosswalk$zcta5))] %>% as.character %>% write.csv("missingzips.txt",row.names=F)
## hand-code missing zips and re-read file
crosswalk <- read.csv("../IPUMS/zcta_to_puma_crosswalk.csv")

crosswalk$puma12 %<>% as.factor
crosswalk$zcta5 %<>% as.factor
PUMAtots <- data.frame(matrix(0,55,28),row.names = unique(crosswalk$puma12))
names(PUMAtots) <- names(aggtotals)[-1]

for(i in 1:nrow(crosswalk)){
  row <- crosswalk[i,]
  if(row$zcta5 %in% aggtotals$zip){
    scaled <- row$afact * aggtotals[which(aggtotals$zip %in% row$zcta5),-1]
    PUMAtots[as.character(row$puma12),] <- PUMAtots[as.character(row$puma12),] + scaled
  }
}

PUMAtots$PUMA <- row.names(PUMAtots)
PUMAtots %<>% 
  mutate(emp.diff = emp.11 - emp.05,
         qp1.diff = qp1.11 - qp1.05,
         ap.diff = ap.11 - ap.05,
         est.diff = est.11 - est.05)

