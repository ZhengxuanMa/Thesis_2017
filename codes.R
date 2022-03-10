library(tidyverse)
du <- read_csv("regressioncsv1.csv")
library(lubridate)

du <- du[-1]

du$`Report Date` <- ymd(du$`Report Date`)

names(du) <- c('stockid','Report Date','Beg.Amt','End.Amt','DerInvestment Return','Currency Derivative(end)','Commodity/Index Futures(end)','Interest Derivative (end)','Cashflow Hedge','Equity Options','Derivative Usage Dummy','oversea')


####assigning 1 to all companies with derivative usages.



##################这里先不动 ！！！等combine 到MAIN 里面一起弄， 一起assign derivative dummy

for (i in 1:length(du$`Derivative Usage Dummy`)) {
  
 if(is.na(du$Beg.Amt[i] | du$End.Amt[i])){
   
   du$`Derivative Usage Dummy`[i]<- 0
   
   
   
   
   du$`Derivative Usage Dummy`[i] <- 1} else if(is.na(du$Beg.Amt[i] & du$End.Amt[i])) {du$`Derivative Usage Dummy`[i]<- 0}
                                                else{du$`Derivative Usage Dummy`[i] <- 0}
  
  print(i)
}

#####




du <- mutate(du,Currency_Dummy = 0)

for (i in 1:length(MAIN$Currency_Dummy)) {
  
  if(MAIN$`Currency Derivative(end)`[i] != 0 & !is.na(MAIN$`Currency Derivative(end)`[i])){ 
    
    MAIN$Currency_Dummy[i] <- 1} else{MAIN$Currency_Dummy[i] <- 0}
  
  print(i)
}




###creating derivative dummies by type

du <- mutate(du,Currency_Dummy = 0)

du <- mutate(du,CFutures_Dummy = 0)

du <- mutate(du,Interest_Dummy = 0)

du <- du %>% mutate(Equity_Dummy = 0)

du <- du %>% mutate(CFhedge_Dummy = 0)




######CF HEDGING DUMMY######################

for (i in 1:length(MAIN1$CFhedge_Dummy)) {
  
  if(MAIN1$`Cashflow Hedge`[i] != 0 & !is.na(MAIN1$`Cashflow Hedge`[i])){ 
    
    MAIN1$CFhedge_Dummy[i] <- 1} else{MAIN1$CFhedge_Dummy[i] <- 0 }
  
  print(i)
}








######equity dummy 


for (i in 1:length(MAIN$Equity_Dummy)) {
  
  if(MAIN$`Equity Options`[i] != 0 & !is.na(MAIN$`Equity Options`[i])){ 
    
    MAIN$Equity_Dummy[i] <- 1} else{MAIN$Equity_Dummy[i] <- 0 }
  
  print(i)
}





###assigning futuers dummy


for (i in 1:length(MAIN$CFutures_Dummy)) {
  
  if(MAIN$`Commodity/Index Futures(end)`[i] != 0 & !is.na(MAIN$`Commodity/Index Futures(end)`[i])){ 
    
    MAIN$CFutures_Dummy[i] <- 1} else{MAIN$CFutures_Dummy[i] <- 0 }
  
  print(i)
}


###assigning interest rate dummy


  
for (i in 1:length(MAIN$`Interest Derivative (end)`)) {
  
  if(MAIN$`Interest Derivative (end)`[i] != 0 & !is.na(MAIN$`Interest Derivative (end)`[i])){ 
    
    MAIN$Interest_Dummy[i] <- 1} else{MAIN$Interest_Dummy[i] <- 0 }
  
  print(i)
}













##changing dummies into factor 

MAIN$Interest_Dummy <- parse_factor(du$Interest_Dummy,levels = c(1,0))
MAIN$CFutures_Dummy <-  parse_factor(du$CFutures_Dummy,levels = c(1,0))
MAIN$Currency_Dummy <-  parse_factor(du$Currency_Dummy,levels = c(1,0))


save.image("Research R Codes.rdata")





tb <- read_csv('TOBINSQ.csv')
#####adding stock id to the du file
###Renaming tobin's q file
names(tb) <- c('stockid','Report Date','Type','D/A','ROA','Div/Share','OPCFstability','OPrevenuegrowth','OPprofitgrowth','MainOPprofitgrowth','ProfitGrowth','EV','TQ','Dividend Yield')

tb <- tb[,-3]

####Merging two dataframes with respect to stock ID and Report Date


###mainData <- merge(x=tb,y=du,by=c("stockid",'Report Date'),all=TRUE)


#####Reading in 关于公司是否有海外业务的dummy组成数据 data




###把tb的stock id 弄成 跟du 一样。


tb1 <- tb

i = 1


while(nchar(tb$stockid[i]) == 1){
  
  
 tb$stockid[i] <-  paste0('00000',tb$stockid[i])
  i <- i+1
  
  
  
  
}

while(nchar(tb$stockid[i]) == 2) {
  
  
  tb$stockid[i] <-  paste0('0000',tb$stockid[i])
  i <- i+1
  
  print(i)
}


while(nchar(tb$stockid[i]) == 3) {
  
  
  tb$stockid[i] <-  paste0('000',tb$stockid[i])
  i <- i+1
  
  print(i)
}


while(nchar(tb$stockid[i]) == 4) {
  
  
  tb$stockid[i] <-  paste0('00',tb$stockid[i])
  i <- i+1
  
  print(i)
}



#########Creating a data set that consists of only company ID 000001 TO 300397 

sampleTB <- filter(tb,stockid < 300398)


####Trying to merge current data

#### MAIN <- merge(x=sampleTB,y=du,by=c("stockid",'Report Date'),all=TRUE)


MAIN <- full_join(x=sampleTB,y=du,by=c("stockid",'Report Date'),all=TRUE)

MAINDU <- full_join(x=du,y=sampleTB,by=c("stockid",'Report Date'),all=TRUE)
#Data mainipulation with Overseas operation data and converting them into dummy variable


##Balance sheet currency information:

Foregncurrencyfrombalancesheet <- read_csv('关于公司是否有海外业务的dummy组成数据/balancesheet.csv')

Foregncurrencyfrombalancesheet <- Foregncurrencyfrombalancesheet[,-1]

names(Foregncurrencyfrombalancesheet) <- c('stockid','Report Date','BalancesheetCurrencyDiff')

#####adding zero before stock id from Foregncurrencyformbalancesheet:
i = 1


while(nchar(Foregncurrencyfrombalancesheet$stockid[i]) == 1){
  
  
  Foregncurrencyfrombalancesheet$stockid[i] <-  paste0('00000',Foregncurrencyfrombalancesheet$stockid[i])
  i <- i+1
  
  
  
  
}

while(nchar(Foregncurrencyfrombalancesheet$stockid[i]) == 2) {
  
  
  Foregncurrencyfrombalancesheet$stockid[i] <-  paste0('0000',Foregncurrencyfrombalancesheet$stockid[i])
  i <- i+1
  
  print(i)
}


while(nchar(Foregncurrencyfrombalancesheet$stockid[i]) == 3) {
  
  
  Foregncurrencyfrombalancesheet$stockid[i] <-  paste0('000',Foregncurrencyfrombalancesheet$stockid[i])
  i <- i+1
  
  print(i)
}


while(nchar(Foregncurrencyfrombalancesheet$stockid[i]) == 4) {
  
  
  Foregncurrencyfrombalancesheet$stockid[i] <-  paste0('00',Foregncurrencyfrombalancesheet$stockid[i])
  i <- i+1
  
  print(i)
}


#Income statement currency info:

incomeCurrency1 <- read_csv('关于公司是否有海外业务的dummy组成数据/利润汇兑收益1.csv')

incomeCurrency1 <- incomeCurrency1[,-1]
names(incomeCurrency1) <- c('stockid','Report Date','ISCurrencyDiff')

incomeCurrency1 <- filter(incomeCurrency1,`Report Date` >= "2005-12-31" )


####Removing duplicated Report Dates item, with similar ISCurrencyDiff, we only gonna take one value per period.

n <- c()

for(i in 1:length(incomeCurrency1$stockid)){
  
  if(incomeCurrency1$`Report Date`[i] == incomeCurrency1$`Report Date`[i+1] ){
    
    n<- c(n,i)
    
  }
  print(i)
  
}

incomeCurrency1 <- incomeCurrency1[-n,]


##############Repitition for 关于公司是否有海外业务的dummy组成数据/利润汇兑收益2.csv


incomeCurrency2 <- read_csv('关于公司是否有海外业务的dummy组成数据/利润汇兑收益2.csv')

incomeCurrency2 <- incomeCurrency2[,-1]
names(incomeCurrency2) <- c('stockid','Report Date','ISCurrencyDiff')

incomeCurrency2 <- filter(incomeCurrency2,`Report Date` >= "2005-12-31" )

incomeCurrency2$stockid <- parse_character(incomeCurrency2$stockid)
####Removing duplicated Dates item for IncomeCurrency2
n <- c()

for(i in 1:length(incomeCurrency2$stockid)){
  
  if(incomeCurrency2$`Report Date`[i] == incomeCurrency2$`Report Date`[i+1] ){
    
    n<- c(n,i)
    
  }
  print(i)
  
}


incomeCurrency2 <- incomeCurrency2[-n,]



###Row binding the two income statement currency gains and loss data:

IS_Currency_GL <- bind_rows(incomeCurrency1,incomeCurrency2)


################Foregn currency related data under Capital Reserves Accounting subject


CapitalAccountForeignCurrency <- read_csv('关于公司是否有海外业务的dummy组成数据/资本公积下面的外币折算1.csv')


CapitalAccountForeignCurrency <- CapitalAccountForeignCurrency[,-1]

names(CapitalAccountForeignCurrency) <- c('stockid','Report Date','ForeignCapital','ForeignCapitalInvDiff')



############Foreign currency data from EVA汇兑.csv

EVAcurrency <-bind_rows(read_csv('关于公司是否有海外业务的dummy组成数据/EVA汇兑1.csv'),read_csv('关于公司是否有海外业务的dummy组成数据/EVA汇兑2.csv'),read_csv('关于公司是否有海外业务的dummy组成数据/EVA汇兑3.csv')

)

names(EVAcurrency) <-c('stockid','Report Date','Exchange Loss')
EVAcurrency <- EVAcurrency[-(1:2),]


EVAcurrency$`Report Date` <- ymd(EVAcurrency$`Report Date`)




######Joining ALL the Foreign currency related data into One big data

Foreign <- full_join(x=IS_Currency_GL,y=CapitalAccountForeignCurrency,by=c("stockid",'Report Date'),all=TRUE)



Foreign <- full_join(x=Foreign,y=Foregncurrencyfrombalancesheet,by=c("stockid",'Report Date'),all=TRUE)



Foreign <- full_join(x=Foreign,y=EVAcurrency,by=c("stockid",'Report Date'),all=TRUE)



####filtering Foreign Curreny Company number 

sampleForeign <- filter(Foreign,stockid < 300398)



########## in the MAIN variable, the MAIN$overseas will take 1 if any samleForeign is not NA, zero otherwise.

##Filtering out companies that do not have foreign currency, extract company that have
withForeign <- filter(sampleForeign,!is.na(ISCurrencyDiff)|!is.na(ForeignCapital)|!is.na(ForeignCapitalInvDiff)|!is.na(BalancesheetCurrencyDiff)|!is.na(`Exchange Loss`))

###Confirmation check:


num <- c() 

for(i in 1:length(sampleForeign$stockid)) {
  
  if(any(!is.na(sampleForeign[i,3:7]))){
    
    num <- c(num,sampleForeign$stockid[i])
    
  }
  
 
  print(i)
  
  
}

###if length of num equals length of withForeign$stockid  , then we are good

for(i in MAIN1$stockid){
  
  if(i %in% withForeign$stockid) {
    
    MAIN1[which(MAIN1$stockid == i),12] <- 1
    
  }
  else {MAIN1[which(MAIN1$stockid == i),12] <- 0}
  
  
print(i)
}




###########NOW ASSIGNING DERIVATIVE USAGE DUMMY..

#Write a function to denote "not in" something..:

`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))




################## IF no is utilized , assign dummy with value 0, or else equals 1.
#######The latest MAIN1 IS CF PER SHARE MAIN ---UPDATED 9/5/2017
for (i in 1:length(MAIN1$stockid)) {
  
  if(i %not in% which(is.na(MAIN1$Beg.Amt) & is.na(MAIN1$`Currency Derivative(end)`) & is.na(MAIN1$End.Amt) & is.na(MAIN1$`DerInvestment Return`) & is.na(MAIN1$`Commodity/Index Futures(end)`) & is.na(MAIN1$`Interest Derivative (end)`) & is.na(MAIN1$`Cashflow Hedge`) & is.na(MAIN1$`Equity Options`)))
  {
    
    MAIN1[i,11] <- 1
    
    
  }
  
  else {MAIN1[i,11] <- 0}
print(i)
}

########Arranging MAIN in turns of the right order of Report date.
MAIN <- arrange(MAINDU, stockid,`Report Date`)

MAIN <- mutate(MAIN, averageusage = (Beg.Amt + End.Amt)/2)


#############Monotonic TEST





summary(felm(MAIN$TQ ~ MAIN$`Derivative Usage Dummy` + MAIN$oversea+
    MAIN$`D/A` + MAIN$EV + MAIN$`Dividend Yield`+ MAIN$OPrevenuegrowth
   + MAIN$ROA, data = MAIN))







###Currency dummy regression FIXED EFFECT
summary(felm(MAIN$TQ ~  MAIN$oversea+
     MAIN$`D/A` + MAIN$EV + MAIN$`Dividend Yield`+ MAIN$OPrevenuegrowth
   + MAIN$ROA + MAIN$Currency_Dummy, data = MAIN))




###

summary(felm(MAIN$TQ ~  MAIN$oversea+
               MAIN$`D/A` + MAIN$EV + MAIN$`Div/Share`+ MAIN$OPrevenuegrowth
             + MAIN$ROA + MAIN$Currency_Dummy + MAIN$CFutures_Dummy + MAIN$Interest_Dummy + MAIN$Equity_Dummy, data = MAIN))


####Interest usage dummy


summary(felm(MAIN$TQ ~  MAIN$oversea+
               MAIN$`D/A` + MAIN$EV + MAIN$`Div/Share`+ MAIN$OPrevenuegrowth
             + MAIN$ROA + MAIN$Interest_Dummy, data = MAIN))

#######POOLED OLS:





summary(lm(MAIN$TQ ~ MAIN$`Report Date`+  MAIN$`Derivative Usage Dummy` + MAIN$oversea+
               MAIN$`D/A` + log(MAIN$EV) + MAIN$`Dividend Yield`+ MAIN$OPrevenuegrowth
             + MAIN$ROA, data = MAIN))

####FIXED EFFECT TESTING 1 WITH 'felm' package


summary(felm(MAIN$TQ ~ MAIN$`Report Date`+  MAIN$`Derivative Usage Dummy` + MAIN$oversea+
               MAIN$`D/A` + log(MAIN$EV) + MAIN$`Dividend Yield`+ MAIN$OPrevenuegrowth
             + MAIN$ROA, data = MAIN))

######FIXED EFFECT TESTING 2 WITH PLM package and Rearranged MAIN data  >> testMAIN
testMAIN <- arrange(MAIN, `Report Date`,stockid)

summary(plm(testMAIN$TQ ~ testMAIN$`Report Date`+ testMAIN$`Derivative Usage Dummy` + testMAIN$oversea+
              testMAIN$`D/A` + log(testMAIN$EV) + testMAIN$`Dividend Yield`+ testMAIN$OPrevenuegrowth
            + testMAIN$ROA, data = MAIN,index =c("stockid","Report Date") ,model="within",effect = 'time'))

##REMOVING THE REPORT DATE:

summary(plm(testMAIN$TQ ~ testMAIN$`Derivative Usage Dummy` + testMAIN$oversea+
              testMAIN$`D/A` + log(testMAIN$EV) + testMAIN$`Dividend Yield`+ testMAIN$OPrevenuegrowth
            + testMAIN$ROA, data = MAIN,index =c("stockid","Report Date") ,model="within",effect = 'time'))




#####Making the difference of CF/SHER volatility  included MAIN1 data into once that you could run with Fixed model
######Final version

testMAIN1 <- arrange(MAIN1, `Report Date`,stockid)

summary(plm(testMAIN1$TQ ~ testMAIN1$`Derivative Usage Dummy` + testMAIN1$oversea+
            testMAIN1$`D/A` + log(testMAIN1$EV) + testMAIN1$`Dividend Yield`+ testMAIN1$OPrevenuegrowth
            + testMAIN1$ROA, data = testMAIN1,index =c("stockid","Report Date") ,model="within",effect = 'time'))


summary(plm(MAIN1$TQ ~ MAIN1$`Report Date`+ MAIN1$`Derivative Usage Dummy` + MAIN1$oversea+
              MAIN1$`D/A` + log(MAIN1$EV) + MAIN1$`Dividend Yield`+ MAIN1$OPrevenuegrowth
            + MAIN1$ROA, data = MAIN1,index =c("stockid","Report Date") ,model="within",effect = 'time'))



summary(plm(testMAIN1$ChangeInTQ ~ testMAIN1$`Derivative Usage Dummy` + testMAIN1$oversea+
              testMAIN1$`D/A` + log(testMAIN1$EV) + testMAIN1$`Dividend Yield`+ testMAIN1$OPrevenuegrowth
            + testMAIN1$ROA, data = testMAIN1,index =c("stockid","Report Date") ,model="within",effect = 'time'))





summary(plm(MAIN1$ChangeInTQ ~ MAIN1$`Report Date`+ MAIN1$`Derivative Usage Dummy` + MAIN1$oversea+
              MAIN1$`D/A` + log(MAIN1$EV) + MAIN1$`Dividend Yield`+ MAIN1$OPrevenuegrowth
            + MAIN1$ROA, data = MAIN1,index =c("stockid","Report Date") ,model="within",effect = 'time'))





#####Assing the CF/SHARE test main MAIN1 to THE TIME ARRANGED MAIN2



#######Wrong , it shrinks the data size  CFMAIN <- filter(MAIN1, `Report Date` == MAIN$`Report Date`)


MAIN2 <- arrange(CFMAIN, `Report Date`,stockid)
summary(plm(MAIN2$StandardDeviation ~ MAIN2$`Derivative Usage Dummy` + MAIN2$oversea+
              MAIN2$`D/A` + log(MAIN2$EV) + MAIN2$`Dividend Yield`+ MAIN2$OPrevenuegrowth
            + MAIN2$ROA, data = MAIN2,index =c("stockid","Report Date") ,model="within",effect = 'time'))


###MAAC Beth




summary(pmg(MAIN1$StandardDeviation ~ MAIN1$`Derivative Usage Dummy` + MAIN1$oversea+
              MAIN1$`D/A` + log(MAIN1$EV) + MAIN1$`Dividend Yield`+ MAIN1$OPrevenuegrowth
            + MAIN1$ROA, data = MAIN1,index =c('Report Date',"stockid")))



###coeftest(pmg(CFMAIN$StandardDeviation ~ CFMAIN$`Derivative Usage Dummy` + CFMAIN$oversea+
    #   CFMAIN$`D/A` + log(CFMAIN$EV) + CFMAIN$`Dividend Yield`+ CFMAIN$OPrevenuegrowth
    # + CFMAIN$ROA, data = CFMAIN,index=c('Report Date','stockid')))

      
#summary(felm(CFMAIN$StandardDeviation ~ CFMAIN$`Derivative Usage Dummy` + CFMAIN$oversea+
             ## CFMAIN$`D/A` + log(CFMAIN$EV) + CFMAIN$`Dividend Yield`+ CFMAIN$OPrevenuegrowth
            ##+ CFMAIN$ROA, data = CFMAIN))








###TESTING FD method on testMAIN1 07/06/2017  - - - NOT WORKING, IT INCREASES STANDARD DEVIATION..


summary(plm(testMAIN1$StandardDeviation ~ testMAIN1$`Derivative Usage Dummy` + testMAIN1$oversea+
              testMAIN1$`D/A` + log(testMAIN1$EV) + testMAIN1$`Dividend Yield`+ testMAIN1$OPrevenuegrowth
            + testMAIN1$ROA, data = testMAIN1
            ,index =c("Report Date","stockid"),model='fd',effect='individual'))




summary(plm(main_with_totalasset$TQ ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset` + main_with_totalasset$`Dividend Yield`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA, data = main_with_totalasset
            ,index =c("stockid","Report Date"),model='random'))




summary(plm(L$ChangeInTQ ~ L$`Derivative Usage Dummy` + L$oversea+
              L$`D/A` + L$`Total Asset` + L$`Dividend Yield`+ L$OPrevenuegrowth
            + L$ROA, data = L
            ,index =c("stockid","Report Date"),model='random'))






######################Differenced regression#### 9/5/2017



#summary(lm(ChangeInCFvolatility~ MAIN1$`Derivative Usage Dummy` + MAIN1$oversea+
             # MAIN1$`D/A` + log(MAIN1$EV) + MAIN1$`Dividend Yield`+ MAIN1$OPrevenuegrowth
            #+ MAIN1$ROA, data = MAIN1))













####Fixed Effect#################FINAL RESULT!

####Consistent with J.M Nelson et al's finding , they found that ,if we were to test TQ with small and large firms all at once
###they get a negative DERIVATIVE coefficient, they realize that this is because allayannis used only large firms, 
###if we use only large firms, that are larger than the median , we get a consistent result.

median(testMAIN1$EV,na.rm=T)
LARGEFIRMS <- filter(testMAIN1,EV>3059957951)
summary(plm(LARGEFIRMS$TQ~ LARGEFIRMS$`Derivative Usage Dummy` + LARGEFIRMS$oversea+
              LARGEFIRMS$`D/A` + log(LARGEFIRMS$EV) + LARGEFIRMS$`Dividend Yield`+ LARGEFIRMS$OPrevenuegrowth
            + LARGEFIRMS$ROA, data = LARGEFIRMS, index = c("Report Date","stockid"),model = 'within',effect='time'))









summary(plm(testMAIN1$TQ ~ testMAIN1$`Derivative Usage Dummy` + testMAIN1$oversea+
              testMAIN1$`D/A` + log(testMAIN1$EV) + testMAIN1$`Dividend Yield`+ testMAIN1$OPrevenuegrowth
            + testMAIN1$ROA, data =testMAIN1,index =c("stockid","Report Date"),model='within',effect='time'))

######random
summary(plm(testMAIN1$TQ ~ testMAIN1$`Derivative Usage Dummy` + testMAIN1$oversea+
              testMAIN1$`D/A` + log(testMAIN1$EV) + testMAIN1$`Dividend Yield`+ testMAIN1$OPrevenuegrowth
            + testMAIN1$ROA, data =testMAIN1,index =c("stockid","Report Date"),model='random',effect='time'))




summary(plm(MAIN1$TQ ~ MAIN1$`Derivative Usage Dummy` + MAIN1$oversea+
              MAIN1$`D/A` + log(MAIN1$EV) + MAIN1$`Dividend Yield`+ MAIN1$OPrevenuegrowth
            + MAIN1$ROA, data =MAIN1,index =c("stockid","Report Date"),model='within',effect='time'))








#########Random effect







summary(plm(MAIN1$TQ ~ MAIN1$`Derivative Usage Dummy` + MAIN1$oversea+
              MAIN1$`D/A` + log(MAIN1$EV) + MAIN1$`Dividend Yield`+ MAIN1$OPrevenuegrowth
            + MAIN1$ROA, data =MAIN1,index =c("stockid","Report Date"),model='random'))





summary(plm(MAIN1$StandardDeviation ~ MAIN1$`Derivative Usage Dummy` + MAIN1$oversea+
              MAIN1$`D/A` + log(MAIN1$EV) + MAIN1$`Dividend Yield`+ MAIN1$OPrevenuegrowth
            + MAIN1$ROA, data =MAIN1,index =c("stockid","Report Date"),model='random'))





##########################TESTING THE CHANGE OF THE NETOPCF/SHARE #####PRESENTATION RESULT!!!!


##worked with between model!!!!

summary(plm(testMAIN1$ChangeInCFVolatility~ testMAIN1$`Derivative Usage Dummy` + testMAIN1$oversea+
              testMAIN1$`D/A` + log(testMAIN1$EV) + testMAIN1$`Dividend Yield`+ testMAIN1$OPrevenuegrowth
            + testMAIN1$ROA, data = testMAIN1, index = c("stockid","Report Date"),model = 'between'))


#####rearranged is testMAIN1 arranged with respect to stockid and Report date: this is the data needed to look like if we were to regress with a goal to remove time fixed effects etc.
###Below two measures are ALL CONSISTENT!
rearranged <- arrange(testMAIN1,stockid,`Report Date`)

summary(plm(rearranged$ChangeInCFVolatility~ rearranged$`Derivative Usage Dummy` + rearranged$oversea+
              rearranged$`D/A` + log(rearranged$EV) + rearranged$`Dividend Yield`+ rearranged$OPrevenuegrowth
            + rearranged$ROA, data = rearranged, index = c("stockid","Report Date"),model = 'between'))



summary(plm(rearranged$ChangeInTQ~ rearranged$`Derivative Usage Dummy` + rearranged$oversea+
              rearranged$`D/A` + log(rearranged$EV) + rearranged$`Dividend Yield`+ rearranged$OPrevenuegrowth
            + rearranged$ROA, data = rearranged, index = c("stockid","Report Date"),model = 'within'))


#######################################






summary(plm(MAIN1$ChangeInCFVolatility~ MAIN1$`Derivative Usage Dummy` + MAIN1$oversea+
              MAIN1$`D/A` + log(MAIN1$EV) + MAIN1$`Dividend Yield`+ MAIN1$OPrevenuegrowth
            + MAIN1$ROA, data = MAIN1, index = c("stockid","Report Date"),model ='between'))





summary(lm(testMAIN1$ChangeInCFVolatility~ testMAIN1$`Derivative Usage Dummy` + testMAIN1$oversea+
              testMAIN1$`D/A` + log(testMAIN1$EV) + testMAIN1$`Dividend Yield`+ testMAIN1$OPrevenuegrowth
            + testMAIN1$ROA, data = testMAIN1))





summary(lm(MAIN1$ChangeInCFVolatility~ MAIN1$`Derivative Usage Dummy` + MAIN1$oversea+
              MAIN1$`D/A` + log(MAIN1$EV) + MAIN1$`Dividend Yield`+ MAIN1$OPrevenuegrowth
            + MAIN1$ROA, data = MAIN1))






summary(plm(TT$SD~ TT$`Derivative Usage Dummy` + TT$oversea+
             TT$`D/A` + log(TT$EV) + TT$`Dividend Yield`+ TT$OPrevenuegrowth
           + TT$ROA, data = TT,index=c('stockid','Report Date'),model='within',effect = 'time'))

TTarg <- arrange(TT, `Report Date`,stockid)


summary(plm(TTarg$SD~ TTarg$`Derivative Usage Dummy` + TTarg$oversea+
              TTarg$`D/A` + log(TTarg$EV) + TTarg$`Dividend Yield`+ TTarg$OPrevenuegrowth
            + TTarg$ROA, data = TTarg,index=c('stockid','Report Date'),model='within',effect = 'time'))



summary(lm(TTarg$ChangeInCFVolatility~ TTarg$`Derivative Usage Dummy` + TTarg$oversea+
              TTarg$`D/A` + log(TTarg$EV) + TTarg$`Dividend Yield`+ TTarg$OPrevenuegrowth
            + TTarg$ROA, data = TTarg))




##########testMAIN1 <<< IS THE MAIN DATA WITH CF VARIABLES!!!!USE IT!!




summary(lm(DIFFERENCE$DIFFERENCE ~ CFMAIN[-1,]$`Derivative Usage Dummy` + CFMAIN[-1,]$oversea+
              CFMAIN[-1,]$`D/A` + log(CFMAIN[-1,]$EV) + CFMAIN[-1,]$`Dividend Yield`+ CFMAIN[-1,]$OPrevenuegrowth
            + CFMAIN[-1,]$ROA, data = CFMAIN[-1,]))





#######no dates included



summary(lm(testMAIN$TQ ~ testMAIN$`Derivative Usage Dummy` + testMAIN$oversea+
             testMAIN$`D/A` + log(testMAIN$EV) + testMAIN$`Dividend Yield`+ testMAIN$OPrevenuegrowth
           + testMAIN$ROA, data = MAIN))




#####This is a function that adds zero to stockid







Addzero <- function(x){

i = 1
j <- x

while(nchar(x[i]) == 1){
  
  
  j[i] <- paste0('00000',x[i])
  i <- i+1
  
  

}

while(nchar(x[i]) == 2) {
  
  
 j[i] <-  paste0('0000',x[i])
  i <- i+1
  
  print(i)
}

while(nchar(x[i]) == 3) {
  
  
 j[i] <-  paste0('000',x[i])
  i <- i+1
  
  print(i)
}

while(nchar(x[i]) == 4) {
  
  
 j[i] <-  paste0('00',x[i])
  
 
 if(i < length(x)){i <- i+1} else {break}
  
  print(i)
}



 return(j)
}



#####Import Total Asset

totalassets1 <- bind_rows(read_csv('资产总额/资产总额1.csv'),read_csv('资产总额/资产总额2.csv'))

names(totalassets1) <- c('stockid','Report Date','Total Asset')

for(i in 1:length(totalassets1$`Report Date`)){
  
  if(!is.na(totalassets1$`Total Asset`[i])){
    
    if(totalassets1$stockid[i] == totalassets1$stockid[i+1] & totalassets1$`Report Date`[i] == totalassets1$`Report Date`[i+1] ){
      
      
  
        
        totalassets1 <- totalassets1[-(i),]
        
        
      
    
    }
    
  }
  
  
  print(i)
  
}

beep()

######君安的tobinsq



JUNANTQ <- read_csv('君安tobinsq/君安tobinsq.csv')
names(JUNANTQ) <- c('stockid','Report Date','INDUSTRY CODE','TQA','TQB','TQC','TQD')


JUNANTQ <- (filter(JUNANTQ,stockid <300398))

######08/06/2017 导入beta and alpha


Beta_Alpha <- read_csv('beta and alpha/beta and alpha.csv')

Beta_Alpha <- Beta_Alpha[,-2]

names(Beta_Alpha) <- c('stockid','Report Date','Alpha Float Weighted','Beta Float Weighted','Alpha MarketCap Weighted','Beta MarketCap Weighted')


Beta_Alpha <- filter(Beta_Alpha,stockid<300430)


##checking duplicated years

for(i in 1:length(Beta_Alpha$`Report Date`)){
  if(Beta_Alpha$stockid[i] == Beta_Alpha$stockid[i+1] & year(Beta_Alpha$`Report Date`[i]) == year(Beta_Alpha$`Report Date`[i+1])){
    
    print(Beta_Alpha$stockid[i])
    
  }
    print(i)
  }

##change dates not equal to xx/12/31 to xx/12/31:

month(Beta_Alpha$`Report Date`[which(month(Beta_Alpha$`Report Date`) != 12)]) <- 12
  

day(Beta_Alpha$`Report Date`[which(day(Beta_Alpha$`Report Date`) != 31)]) <- 31


main_with_totalasset <- left_join(main_with_totalasset,Beta_Alpha,by=c('stockid','Report Date'),all=TRUE)




#####Importing and dealing with stock cumulated annual return data


CAR <- read_csv('年收益与波动/年累计收益率.csv')

CAR <- CAR[,-2]

names(CAR) <- c('stockid','Report Date','Cumulative return')

month(CAR$`Report Date`[which(month(CAR$`Report Date`) != 12)]) <- 12


day(CAR$`Report Date`[which(day(CAR$`Report Date`) != 31)]) <- 31



main_with_totalasset <- left_join(main_with_totalasset,CAR,by=c('stockid','Report Date'),all=TRUE)




Pooled_main_with_totalasset <- left_join(Pooled_main_with_totalasset,CAR, by=c('stockid','Report Date'),all=TRUE)



#####Junan's stock return

JCAR <- read_csv('年收益与波动/JUNAN年累计收益率.csv')


names(JCAR) <- c('stockid','Report Date','CR NO DIV','Cumulative No div')

JCAR <- JCAR[-1,]

for(i in 1:length(JCAR$`Report Date`)){

if(JCAR$`Report Date`[i] == '2015'){ JCAR$`Report Date`[i] <-'2015-12-31'}
  if(JCAR$`Report Date`[i] == '2014') {JCAR$`Report Date`[i] <- '2014-12-31'}
  if(JCAR$`Report Date`[i] == '2013') {JCAR$`Report Date`[i] <- '2013-12-31'}
  if(JCAR$`Report Date`[i] == '2012') {JCAR$`Report Date`[i] <- '2012-12-31'}
  if(JCAR$`Report Date`[i] == '2011') {JCAR$`Report Date`[i] <- '2011-12-31'}
  if(JCAR$`Report Date`[i] == '2010') {JCAR$`Report Date`[i] <- '2010-12-31'}
  if(JCAR$`Report Date`[i] == '2009') {JCAR$`Report Date`[i] <- '2009-12-31'}
  if(JCAR$`Report Date`[i] == '2008') {JCAR$`Report Date`[i] <- '2008-12-31'}
  
  if(JCAR$`Report Date`[i] == '2007') {JCAR$`Report Date`[i] <- '2007-12-31'}
  if(JCAR$`Report Date`[i] == '2006') {JCAR$`Report Date`[i] <- '2006-12-31'}
  if(JCAR$`Report Date`[i] == '2005') {JCAR$`Report Date`[i] <- '2005-12-31'}
  
  
  print(i)
  
}

JCAR <- filter(JCAR,stockid < 300430)



main_with_totalasset <-  left_join(main_with_totalasset,JCAR,by=c('stockid','Report Date'),all=TRUE)




#######股价年波动数据导入与cleaning


VOL <- read_csv('个股年波动率.csv')



names(VOL) <- c('stockid','Report Date','stockreturnvolatility')

month(VOL$`Report Date`[which(month(VOL$`Report Date`) != 12)]) <- 12


day(VOL$`Report Date`[which(day(VOL$`Report Date`) != 31)]) <- 31



main_with_totalasset <- left_join(main_with_totalasset,VOL,by=c('stockid','Report Date'),all=TRUE)

Pooled_main_with_totalasset <- left_join(Pooled_main_with_totalasset,CAR, by=c('stockid','Report Date'),all=TRUE)










######CF per share import

CF1 <- read_csv('CF Per share Measures/CF per share1.csv')
CF1 <- CF1[-1,]



names(CF1) <- c('stockid','Report Date','SALE/SHARE TTM','OPPROFT/SHARE','NETOPCF/SHARE','NETOPCF/SHARETTM','FCFF/SHARE','FCFF/SHARETTM','FCFFORIG/SHARE','FCFTTM/SHARE1')
CF1$`Report Date` <- ymd(CF1$`Report Date`)
###Transforming CF1 according to stockid , and report date accending order

CF1 <- arrange(CF1,stockid,Year,Month)

####Splitting the current Report Date into Year column, month column, day column

datetxt <- CF1$`Report Date`
datetxt <- as.Date(datetxt)
df <- tibble(date = datetxt,
                 year = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 day = as.numeric(format(datetxt, format = "%d")))


CF0 <- CF1
CF1 <- mutate(CF1, Year = df$year)

CF1 <- mutate(CF1, Month = df$month)

CF1$`NETOPCF/SHARETTM` <- parse_double(CF1$`NETOPCF/SHARETTM`)
  CFsd1 <- data.frame()
  temp<-c()
  for(i in 1:length(CF1$Year)){
    
    
    
  
  if(CF1$Month[i] %in% c(3,6,9)){ temp <- c(temp,CF1$`NETOPCF/SHARETTM`[i])
  
  
  }
  else if(CF1$Month[i] %in% 12)
        
      { 
        temp <- c(temp,CF1$`NETOPCF/SHARETTM`[i])
        
        sdd <- sd(c(temp,na.rm=T),na.rm=T)
        CFsd1 <- rbind(CFsd1,c(sdd,i,as.numeric(CF1$stockid)[i],CF1$`Report Date`[i]))
        
        temp <-c()
        
      
      }
  
      
    print(i)
    
  }
  
  ##############Turning the CF Standard deviation Data into Tibble data.
  library('zoo')
  CFsd1$X13148 <- as.Date(CFsd1$X13148)
  
  
  names(CFsd1) <- c('StandDeviation','i Counts','stockid','Report Date')
  CFsd1 <- tibble(CFsd1$stockid,CFsd1$`Report Date`,CFsd1$`i Counts`,CFsd1$StandDeviation)
  
  
  names(CFsd1)  <- c('stockid','Report Date','i Counts','StandardDeviation')
  
  
  
  ###adding zero
  i = 1
  
  
  while(nchar(CF1$stockid[i]) == 1){
    
    
    CF1$stockid[i] <-  paste0('00000',CF1$stockid[i])
    i <- i+1
    
    
    
    
  }
  
  while(nchar(CF1$stockid[i]) == 2) {
    
    
    CF1$stockid[i] <-  paste0('0000',CF1$stockid[i])
    i <- i+1
    
    print(i)
  }
  
  
  while(nchar(CF1$stockid[i]) == 3) {
    
    
    CF1$stockid[i] <-  paste0('000',CF1$stockid[i])
    i <- i+1
    
    print(i)
  }
  
  
  while(nchar(CF1$stockid[i]) == 4) {
    
    
    CF1$stockid[i] <-  paste0('00',CF1$stockid[i])
    i <- i+1
    
    print(i)
  }
  

##########Remove Duplicated dates on same stockid
  
  
  for(i in 1:length(CF1$Month)){
    
    if(!is.na(CF1$Month[i])){
    
    if(CF1$stockid[i] == CF1$stockid[i+1] & CF1$Year[i] == CF1$Year[i+1] ){
      
      
      if(CF1$Month[i] == CF1$Month[i+1]){
        
        
        CF1 <- CF1[-(i),]
        
        
      }
      
    }
      
    }
    
    
    print(i)
  
  }
  
  beep()
  
  
  
 ######import CF2
  
  
  CF2 <- read_csv('CF Per share Measures/CF per share2.csv')
  CF2 <- CF2[-1,]
  
  
  
  names(CF2) <- c('stockid','Report Date','SALE/SHARE TTM','OPPROFT/SHARE','NETOPCF/SHARE','NETOPCF/SHARETTM','FCFF/SHARE','FCFF/SHARETTM','FCFFORIG/SHARE','FCFTTM/SHARE1')
  CF2$`Report Date` <- ymd(CF2$`Report Date`)
  ###Transforming CF1 according to stockid , and report date accending order
  
 
  
  ####Splitting the current Report Date into Year column, month column, day column
  
  datetxt <- CF2$`Report Date`
  datetxt <- as.Date(datetxt)
  df <- tibble(date = datetxt,
               year = as.numeric(format(datetxt, format = "%Y")),
               month = as.numeric(format(datetxt, format = "%m")),
               day = as.numeric(format(datetxt, format = "%d")))
  
  
  CF0 <- CF2
  CF2 <- mutate(CF2, Year = df$year)
  
  CF2 <- mutate(CF2, Month = df$month)
 
  
  CF2$`NETOPCF/SHARETTM` <- parse_double(CF2$`NETOPCF/SHARETTM`)
  CFsd2 <- data.frame()
  temp<-c()
  for(i in 1:length(CF2$Year)){
    
    
    
    
    if(CF2$Month[i] %in% c(3,6,9)){ temp <- c(temp,CF2$`NETOPCF/SHARETTM`[i])
    
    
    }
    else if(CF2$Month[i] %in% 12)
      
    { 
      temp <- c(temp,CF2$`NETOPCF/SHARETTM`[i])
      
      sdd <- sd(c(temp,na.rm=T),na.rm=T)
      CFsd2 <- rbind(CFsd2,c(sdd,i,as.numeric(CF2$stockid)[i],CF2$`Report Date`[i]))
      
      temp <-c()
      
      
    }
    
    
    print(i)
    
  }
  
  
  library('zoo')
  CFsd2$X14974 <- as.Date(CFsd2$X14974)
  
  
  names(CFsd2) <- c('StandDeviation','i Counts','stockid','Report Date')
  CFsd2$stockid <- Addzero(CFsd2$stockid)
  
  CFsd2 <- tibble(CFsd2$stockid,CFsd2$`Report Date`,CFsd2$`i Counts`,CFsd2$StandDeviation)
  
  
  names(CFsd2) <- names(CFsd2) <- c('stockid','Report Date','i Counts','StandardDeviation')
  
  ####Double check:
  
##take CF2[901:905,] for example:
  
  # A tibble: 5 × 12
 # stockid `Report Date` `SALE/SHARE TTM`
 # <chr>        <date>            <chr>
  #  1  002466    2011-03-31             <NA>
  #  2  002466    2011-06-30             <NA>
  #  3  002466    2011-09-30         2.593742#
 # 4  002466    2011-12-31          2.37499#
 # 5  002466    2012-03-31           2.4498#
  
  
 ## CFsd2$X3[239:242]
  # 900 904 908 912
 #### > CFsd2$X0.748879114285143[239:242]
 ## [1] 0.8967585 0.6495543 0.5006302 0.8151800
  
  ##The CFsd2  239 to 242 row , is corresponding to 
  #row in CF2 900 to 912 row.
  
  
  #sd(c(CF2$`NETOPCF/SHARETTM`[901:904],na.rm=T),na.rm=T)
  #[1] 0.6495543
  
  
  #901:904 is a complete year, for stock 002466 , from 2011-03-31 to
  # 2011-12-31
  
  
  #thereby,standarde deviaition calculated explicitly sd(c(CF2$`NETOPCF/SHARETTM`[901:904],na.rm=T),na.rm=T)
  #should equate to CFsd2$X0.748879114285143[239:242][2]
 #which is 0.6495543
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###adding zero
  i = 1
  
  
  while(nchar(CF2$stockid[i]) == 1){
    
    
    CF2$stockid[i] <-  paste0('00000',CF2$stockid[i])
    i <- i+1
    
    
    
    
  }
  
  while(nchar(CF2$stockid[i]) == 2) {
    
    
    CF2$stockid[i] <-  paste0('0000',CF2$stockid[i])
    i <- i+1
    
    print(i)
  }
  
  
  while(nchar(CF2$stockid[i]) == 3) {
    
    
    CF2$stockid[i] <-  paste0('000',CF2$stockid[i])
    i <- i+1
    
    print(i)
    
  }
  
  
  while(nchar(CF2$stockid[i]) == 4) {
    
    
    CF2$stockid[i] <-  paste0('00',CF2$stockid[i])
    i <- i+1
    
    print(i)
    
  }
  beep()
  
  
  
  ########################
  CF2 <- arrange(CF2,stockid,Year,Month)
  ##########Remove Duplicated dates on same stockid
  
  
  for(i in 1:length(CF2$Month)){
    
    if(!is.na(CF2$Month[i])){
      
      if(CF2$stockid[i] == CF2$stockid[i+1] & CF2$Year[i] == CF2$Year[i+1] ){
        
        if(CF2$Month[i] == CF2$Month[i+1]){
          
          CF2 <- CF2[-(i),]
          
        }
        
      }
      
    }
    
    print(i)
    
  
  }
  
  beep()
  
  
  
  ############CF per share 3
  
  
  
  
  
  ######Trying to delete duplicated dates in MAIN1 ##############
  #############################################################
  
  
  for(i in 1:length(MAIN1$stockid)){
    
    
      
      if(MAIN1$stockid[i] == MAIN1$stockid[i+1] & MAIN1$`Report Date`[i] == MAIN1$`Report Date`[i+1] ){
        
        
          
          MAIN1 <- MAIN1[-(i),]
          
        
        
      }
      
    
    
    print(i)
    
    
  }
  
  beep()
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  CF3 <- read_csv('CF Per share Measures/CF per share3.csv')
  CF3 <- CF3[-1,]
  
  
  
  names(CF3) <- c('stockid','Report Date','SALE/SHARE TTM','OPPROFT/SHARE','NETOPCF/SHARE','NETOPCF/SHARETTM','FCFF/SHARE','FCFF/SHARETTM','FCFFORIG/SHARE','FCFTTM/SHARE1')
  CF3$`Report Date` <- ymd(CF3$`Report Date`)
  ###Transforming CF1 according to stockid , and report date accending order
  
  
  
  ####Splitting the current Report Date into Year column, month column, day column
  
  datetxt <- CF3$`Report Date`
  datetxt <- as.Date(datetxt)
  df <- tibble(date = datetxt,
               year = as.numeric(format(datetxt, format = "%Y")),
               month = as.numeric(format(datetxt, format = "%m")),
               day = as.numeric(format(datetxt, format = "%d")))
  
  
  CF0 <- CF3
  CF3 <- mutate(CF3, Year = df$year)
  
  CF3 <- mutate(CF3, Month = df$month)
  
  
  CF3$`NETOPCF/SHARETTM` <- parse_double(CF3$`NETOPCF/SHARETTM`)
  CFsd3 <- data.frame()
  temp<-c()
  for(i in 1:length(CF3$Year)){
    
    
    
    
    if(CF3$Month[i] %in% c(3,6,9)){ temp <- c(temp,CF3$`NETOPCF/SHARETTM`[i])
    
    
    }
    else if(CF3$Month[i] %in% 12)
      
    { 
      temp <- c(temp,CF3$`NETOPCF/SHARETTM`[i])
      
      sdd <- sd(c(temp,na.rm=T),na.rm=T)
      CFsd3 <- rbind(CFsd3,c(sdd,i,as.numeric(CF3$stockid)[i],CF3$`Report Date`[i]))
      
      temp <-c()
      
      
    }
    
    
    print(i)
    
  }
  
  
  library('zoo')
  CFsd3$X13148 <- as.Date(CFsd3$X13148)
  
  
  names(CFsd3) <- c('StandDeviation','i Counts','stockid','Report Date')
  
  CFsd3$stockid <- Addzero(CFsd3$stockid)
  
  CFsd3 <- tibble(CFsd3$stockid,CFsd3$`Report Date`,CFsd3$`i Counts`,CFsd3$StandDeviation)
  
  
  names(CFsd3) <- c('stockid','Report Date','i Counts','StandardDeviation')
  
  
  
  
  
  

  
  
  
  ######Double checking:
  
  
  sd(c(CF3$`NETOPCF/SHARETTM`[801:804],na.rm=T),na.rm=T)
  
  
  #Should be equal to :
  
  CFsd3[CFsd3$X1 == 804,]
  
  
  
  
  
  
  
  ###adding zero
  i = 1
  
  
  while(nchar(CF3$stockid[i]) == 1){
    
    
    CF3$stockid[i] <-  paste0('00000',CF3$stockid[i])
    i <- i+1
    
    
    
    
  }
  
  while(nchar(CF3$stockid[i]) == 2) {
    
    
    CF3$stockid[i] <-  paste0('0000',CF3$stockid[i])
    i <- i+1
    
    print(i)
  }
  
  
  while(nchar(CF3$stockid[i]) == 3) {
    
    
    CF3$stockid[i] <-  paste0('000',CF3$stockid[i])
    i <- i+1
    
    print(i)
    
  }
  
  
  while(nchar(CF3$stockid[i]) == 4) {
    
    
    CF3$stockid[i] <-  paste0('00',CF3$stockid[i])
    i <- i+1
    
    print(i)
    
  }
  beep()
  
  
  
  ########################
  CF3 <- arrange(CF3,stockid,Year,Month)
  ##########Remove Duplicated dates on same stockid
  
  
  for(i in 1:length(CF3$Month)){
    
    if(!is.na(CF3$Month[i])){
      
      if(CF3$stockid[i] == CF3$stockid[i+1] & CF3$Year[i] == CF3$Year[i+1] ){
        
        if(CF3$Month[i] == CF3$Month[i+1]){
          
          CF3 <- CF3[-(i),]
          
        }
        
      }
      
    }
    
    print(i)
    
    
  }
  
  beep()
  
  
  
  
  ######bind_rows Binding the rows of the three CFs.
  
  
  
  Cashflow_Volatility <- bind_rows(CFsd1,CFsd2,CFsd3) 
  
  
  
  
  ##################Trying to calculate the differenced CF volatility.
  
  gb <- group_by(MAIN1,stockid,`Report Date`)
  
  gb <- summarize(gb,difference = diff(StandardDeviation))
  
  ChangeInCFvolatility <- matrix(nrow = length(gb$difference)+1)
  
  ChangeInCFvolatility[1:length(gb$difference)] <- gb$difference
  
  
  
  #####Doing the samething for Tobins Q ###这是用来算TQ 的变动的。
  
  
  gb <- group_by(MAIN1,stockid,`Report Date`)
  
  gb <- summarize(gb,differenceTQ = diff(TQ))
  
  ChangeInTQ <- matrix(nrow = length(gb$differenceTQ)+1)
  
  ChangeInTQ[1:length(gb$differenceTQ)] <- gb$differenceTQ
  
  
  
gb <- group_by(main_with_totalasset,stockid,`Report Date`)
  
  gb<- summarize(gb,differenceTQA = diff(TQA))
  
  
  
  #######################main_with_totalasset 是 rearranged 然后left_joint totalassets1.
  ####然后又left joint 了 JUNANTQ ---君安数据库的TQ。
  
  CT <- diff(main_with_totalasset$TQA)
  CTTQ <- matrix(nrow = length(CT)+1)
  
  CTTQ[1:length(CT)] <- CT
  
  main_with_totalasset <- mutate(main_with_totalasset,ChangeInTQA = CTTQ)
  
  
  for(i in 1:length(main_with_totalasset$ChangeInCFVolatility)){
    
    if(main_with_totalasset$stockid[i+1] != main_with_totalasset$stockid[i]){
      
      main_with_totalasset$ChangeInTQA[i] <- NA
      
      
    }
    
    print(i)
    
    
  }
  beep()
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  ####07/06/2017 Running main_with_totalasset regression:
  
  ###if dependent is ChangeIn TQ , or Change in TQA , both result is consistent , Derivative usage have POSITIVE impact on change in TQ.
  summary(plm(main_with_totalasset$ChangeInTQA ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                main_with_totalasset$`D/A` + log(main_with_totalasset$EV)+ main_with_totalasset$`Dividend Yield`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA , data = main_with_totalasset
              ,index =c("stockid","Report Date"),model='within'))
  
  ####with change in CF volatility , also is consistent:
  summary(plm(main_with_totalasset$ChangeInCFVolatility ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                main_with_totalasset$`D/A` + log(main_with_totalasset$EV) + main_with_totalasset$`Dividend Yield`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA , data = main_with_totalasset
              ,index =c("stockid","Report Date"),model='between'))
  
  
  
  
  
  
  ##trying beta   ###doesnt seem to work..
  
  summary(plm(log(main_with_totalasset$stockreturnvolatility) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                main_with_totalasset$`D/A` + log(main_with_totalasset$EV)+ main_with_totalasset$`Dividend Yield`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA , data = main_with_totalasset
              ,index =c("stockid","Report Date"),model='within'))
  
  
  
summary(plm(log(Pooled_main_with_totalasset$StandardDeviation) ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
                  Pooled_main_with_totalasset$`D/A` + log(Pooled_main_with_totalasset$EV)+ Pooled_main_with_totalasset$`Dividend Yield`+ Pooled_main_with_totalasset$OPrevenuegrowth
                + Pooled_main_with_totalasset$ROA  , data = Pooled_main_with_totalasset
                ,model='pool'))
    



summary(plm(main_with_totalasset$`CR NO DIV` ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + log(main_with_totalasset$EV)+ main_with_totalasset$`Dividend Yield`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
            ,model='within'))


summary(plm(Pooled_main_with_totalasset$`Cumulative No div` ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
              Pooled_main_with_totalasset$`D/A` + log(Pooled_main_with_totalasset$EV)+ Pooled_main_with_totalasset$`Dividend Yield`+ Pooled_main_with_totalasset$OPrevenuegrowth
            + Pooled_main_with_totalasset$ROA , data = Pooled_main_with_totalasset
            ,index =c("stockid","Report Date"),model='within',effect='time'))






#### Trying Year by Year###########


summary(plm(Pool_main_2005$`Cumulative No div` ~ Pool_main_2005$`Derivative Usage Dummy` + Pool_main_2005$oversea+
              Pool_main_2005$`D/A` + log(Pool_main_2005$EV)+ Pool_main_2005$`Div/Share`+ Pool_main_2005$OPrevenuegrowth
            + Pool_main_2005$ROA , data = Pool_main_2005
            ,model='pool'))

summary(plm(Pool_main_2006$TQA~ Pool_main_2006$`Derivative Usage Dummy` + Pool_main_2006$oversea+
              Pool_main_2006$`D/A` + Pool_main_2006$`Total Asset`+ Pool_main_2006$`Div/Share`+ Pool_main_2006$OPrevenuegrowth
            + Pool_main_2006$ROA , data = Pool_main_2006
            ,index =c("stockid"),model='pool'))


summary(plm(Pool_main_2007$TQ ~ Pool_main_2007$`Derivative Usage Dummy` + Pool_main_2007$oversea+
              Pool_main_2007$`D/A` + Pool_main_2007$`Total Asset` + Pool_main_2007$`Div/Share`+ Pool_main_2007$OPrevenuegrowth
            + Pool_main_2007$ROA + Pool_main_2007$`INDUSTRY CODE`, data = Pool_main_2007
            ,index =c("stockid"),model='pool'))

  
  
summary(plm(Pool_main_2008$TQA ~ Pool_main_2008$`Derivative Usage Dummy` + Pool_main_2008$oversea+
              Pool_main_2008$`D/A` + Pool_main_2008$`Total Asset`+ Pool_main_2008$`Div/Share`+ Pool_main_2008$OPrevenuegrowth
            + Pool_main_2008$ROA + Pool_main_2008$`INDUSTRY CODE`, data = Pool_main_2008
            ,index =c("stockid"),model='pool'))


  
summary(plm(Pool_main_2009$TQA ~ Pool_main_2009$`Derivative Usage Dummy` + Pool_main_2009$oversea+
              Pool_main_2009$`D/A` + Pool_main_2009$`Total Asset`+ Pool_main_2009$`Div/Share`+ Pool_main_2009$OPrevenuegrowth
            + Pool_main_2009$ROA + Pool_main_2009$`INDUSTRY CODE`, data = Pool_main_2009
            ,index =c("stockid"),model='pool'))




summary(lm(Pool_main_2009$`CR NO DIV` ~ Pool_main_2009$`Derivative Usage Dummy` + Pool_main_2009$oversea+
              Pool_main_2009$`D/A` + log(Pool_main_2009$EV)+ Pool_main_2009$`Div/Share`+ Pool_main_2009$OPrevenuegrowth
            + Pool_main_2009$ROA , data = Pool_main_2009
            ))


summary(lm(Pool_main_2010$TQA ~ Pool_main_2010$`Derivative Usage Dummy` + Pool_main_2010$oversea+
             Pool_main_2010$`D/A` + Pool_main_2010$`Total Asset`+ Pool_main_2010$`Div/Share`+ Pool_main_2010$OPrevenuegrowth
           + Pool_main_2010$ROA , data = Pool_main_2010
))

summary(lm(log(Pool_main_2011$EV) ~ Pool_main_2011$`Derivative Usage Dummy` + Pool_main_2011$oversea+
             Pool_main_2011$`D/A` + Pool_main_2011$`Total Asset`+ Pool_main_2011$`Div/Share`+ Pool_main_2011$OPrevenuegrowth
           + Pool_main_2011$ROA , data = Pool_main_2011
))

summary(lm(log(Pool_main_2012$EV) ~ Pool_main_2012$`Derivative Usage Dummy` + Pool_main_2012$oversea+
             Pool_main_2012$`D/A` + Pool_main_2012$`Total Asset`+ Pool_main_2012$`Div/Share`+ Pool_main_2012$OPrevenuegrowth
           + Pool_main_2012$ROA  + Pool_main_2012$`INDUSTRY CODE`, data = Pool_main_2012
))



summary(lm(log(Pool_main_2013$EV) ~ Pool_main_2013$`Derivative Usage Dummy` + Pool_main_2013$oversea+
             Pool_main_2013$`D/A` + Pool_main_2013$`Total Asset`+ Pool_main_2013$`Div/Share`+ Pool_main_2013$OPrevenuegrowth
           + Pool_main_2013$ROA  + Pool_main_2013$`INDUSTRY CODE`, data = Pool_main_2013
))



summary(lm(log(Pool_main_2014$EV) ~ Pool_main_2014$`Derivative Usage Dummy` + Pool_main_2014$oversea+
             Pool_main_2014$`D/A` + Pool_main_2014$`Total Asset`+ Pool_main_2014$`Div/Share`+ Pool_main_2014$OPrevenuegrowth
           + Pool_main_2014$ROA  + Pool_main_2014$`INDUSTRY CODE`, data = Pool_main_2014
))




summary(lm(log(Pool_main_2015$EV) ~ Pool_main_2015$`Derivative Usage Dummy` + Pool_main_2015$oversea+
             Pool_main_2015$`D/A` + Pool_main_2015$`Total Asset`+ Pool_main_2015$`Div/Share`+ Pool_main_2015$OPrevenuegrowth
           + Pool_main_2015$ROA  , data = Pool_main_2015
))
###Above all doesnt seem to work..

#########FIRM SIZES:



Large <- filter(main_with_totalasset,EV >= 3058234578)

Small <- filter(main_with_totalasset,EV < 3058234578)

Pooled_Large <- filter(Pooled_main_with_totalasset,EV >= 3058234578)

Pooled_Small <- filter(Pooled_main_with_totalasset,EV < 3058234578)
##################Trying it with size



summary(plm(Pooled_Large$TQA~ Pooled_Large$`Derivative Usage Dummy` + Pooled_Large$oversea+
              Pooled_Large$`D/A` + log(Pooled_Large$EV)+ Pooled_Large$`Dividend Yield`+ Pooled_Large$OPrevenuegrowth
            + Pooled_Large$ROA , data = Pooled_Large
            ,index =c("stockid","Report Date"),model='pool'))





summary(plm(Pooled_Small$TQA~ Pooled_Small$`Derivative Usage Dummy` + Pooled_Small$oversea+
              Pooled_Small$`D/A` + log(Pooled_Small$EV)+ Pooled_Small$`Dividend Yield`+ Pooled_Small$OPrevenuegrowth
            + Pooled_Small$ROA , data = Pooled_Small
            ,index =c("stockid","Report Date"),model='pool'))




#######panel data


###Size separate tests, using fixed effect are still consistent with 
##change measures, however, other measures not consistent.
summary(plm(Small$ChangeInCFVolatility~ Small$`Derivative Usage Dummy` + Small$oversea+
              Small$`D/A` + log(Small$EV)+ Small$`Dividend Yield`+ Small$OPrevenuegrowth
            + Small$ROA , data = Small
            ,index =c("stockid","Report Date"),model="between"))

#######also consisitnt still, using Change measures.


summary(plm(Large$ChangeInCFVolatility~ Large$`Derivative Usage Dummy` + Large$oversea+
              Large$`D/A` + log(Large$EV)+ Large$`Dividend Yield`+ Large$OPrevenuegrowth
            + Large$ROA + Large$`INDUSTRY CODE` , data = Large
            ,index =c("stockid","Report Date"),model="between"))


################ Regression using change in stockreturn measures

###Good!!consistent! Derivative usage is positive with the change in stock return using fixed effect measure
summary(plm(main_with_totalasset$Changeinstockreturn~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + log(main_with_totalasset$EV)+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA  , data = main_with_totalasset
            ,index =c("stockid","Report Date"),model="random"))



summary(plm(main_with_totalasset$ChangeInCFVolatility~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + log(main_with_totalasset$EV)+ main_with_totalasset$`Dividend Yield`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA  , data = main_with_totalasset
            ,index =c("stockid","Report Date"),model="between"))






##############stockreturn volatility measures are consistent!!! adding or removing industry code both decreases volatility! for panel data
summary(plm(log(main_with_totalasset$stockreturnvolatility) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + log(main_with_totalasset$EV)+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA , data = main_with_totalasset
            ,index =c("stockid","Report Date"),model='within'))



summary(plm(log(Pooled_main_with_totalasset$stockreturnvolatility) ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
              Pooled_main_with_totalasset$`D/A` + log(Pooled_main_with_totalasset$EV)+ Pooled_main_with_totalasset$`Dividend Yield`+ Pooled_main_with_totalasset$OPrevenuegrowth
            + Pooled_main_with_totalasset$ROA , data = Pooled_main_with_totalasset
            ,index =c("stockid","Report Date"),model='pool'))






######Testing change measures of Stock returns


changeinstock <- diff(main_with_totalasset$`Cumulative No div`)
Changestock <- matrix(nrow = length(changeinstock)+1)

Changestock[1:length(changeinstock)] <- changeinstock

main_with_totalasset <- mutate(main_with_totalasset,Changeinstockreturn = Changestock)


for(i in 1:length(main_with_totalasset$ChangeInCFVolatility)){
  
  if(main_with_totalasset$stockid[i+1] != main_with_totalasset$stockid[i]){
    
    main_with_totalasset$Changeinstockreturn[i] <- NA
    
    
  }
  
  print(i)
  
  
}
beep()







  
 #######07.06.2017 mannually adding derivative info on new companies, using main_with_totalasset 
  ####to secure current working, before doing anything :
  
  
  main_with_totalasset_B4 <- main_with_totalasset
  
  
  
  
  
  #############Deleting "change" data when stock id switches.
  ##########This did not produce any significance results.
  
  
  
  for(i in 1:length(TETSTS$ChangeInCFVolatility)){
    
    if(TETSTS$stockid[i+1] != TETSTS$stockid[i]){
      
      TETSTS$ChangeInCFVolatility[i] <- TETSTS$ChangeInTQ[i] <- NA
      
      
    }
    
    print(i)
    
    
  }
  beep()
  
  
  
  
  
  
  
  ########
  
  
  #############Deleting "change" data when stock id switches.
  
  for(i in 1:length(TT$ChangeInCFVolatility)){
    
    if(TT$stockid[i] != TT$stockid[i+1]){
      
      TT <- TT[-i,] 
      
      
    }
    
    print(i)
    
    
  }
  beep()
  
  
  
  
  
###making a notifying sound when finish running the sript!!
  beep <- function(n = 3){
    for(i in seq(n)){
      system("rundll32 user32.dll,MessageBeep -1")
      Sys.sleep(.5)
    }
  }
  


  
  
  
  
  ######09/06/2017 Regression  with all three research interest
  
  ###First Regression Interest:
  ##Derivvative usage and firm value:
  
  ##Using TQ ,TQA , changeInTQ,changeInTQA: 
  
  ###After correcting dividend, it is consistent, that derivative usage will increase TQ

  summary(plm(log(main_with_totalasset$TQ) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA , data = main_with_totalasset
              ,index =c("stockid","Report Date"),model='within'))
  
  
  
  summary(plm(log(main_with_totalasset$TQA) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea +
                main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE` , data = main_with_totalasset
              ,index =c("stockid","Report Date"),model='within'))
  

  
#####This is after deleting extreme tq values..
  
  summary(plm(log(try1$StandardDeviation) ~ try1$`Derivative Usage Dummy` + try1$oversea +
                try1$`D/A` + try1$`Total Asset`+ try1$`Div/Share`+ try1$OPrevenuegrowth
              + try1$ROA + try1$`INDUSTRY CODE` , data = try1
              ,index =c("stockid","Report Date"),model='pool',effect='time'))
  
  
  
  
  
  
  
  
  #####Using Pooled measures:
  ####either Significant but Negatively to Tobins Q or insignificant..
  
  summary(plm(log(Pooled_main_with_totalasset$TQ) ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
                Pooled_main_with_totalasset$`D/A` + Pooled_main_with_totalasset$`Total Asset`+ Pooled_main_with_totalasset$`Div/Share`+ Pooled_main_with_totalasset$OPrevenuegrowth
              + Pooled_main_with_totalasset$ROA , data = Pooled_main_with_totalasset
              ,index =c("stockid","Report Date"),model='pool'))
  
  
  summary(lm(log(Pooled_main_with_totalasset$TQA) ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
                Pooled_main_with_totalasset$`D/A` + Pooled_main_with_totalasset$`Total Asset`+ Pooled_main_with_totalasset$`Div/Share`+ Pooled_main_with_totalasset$OPrevenuegrowth
              + Pooled_main_with_totalasset$ROA , data = Pooled_main_with_totalasset
              ))
  
  
  
  
  
  
  summary(plm(log(Pooled_main_with_totalasset$TQ) ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
                Pooled_main_with_totalasset$`D/A` + Pooled_main_with_totalasset$`Total Asset`+ Pooled_main_with_totalasset$`Div/Share`+ Pooled_main_with_totalasset$OPrevenuegrowth
              + Pooled_main_with_totalasset$ROA + Pooled_main_with_totalasset$`INDUSTRY CODE` , data = Pooled_main_with_totalasset
              ,index =c("stockid","Report Date"),model='pool'))
  
  
  summary(plm(Pooled_main_with_totalasset$ChangeInTQA ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
                Pooled_main_with_totalasset$`D/A` + Pooled_main_with_totalasset$`Total Asset`+ Pooled_main_with_totalasset$`Div/Share`+ Pooled_main_with_totalasset$OPrevenuegrowth
              + Pooled_main_with_totalasset$ROA + Pooled_main_with_totalasset$`INDUSTRY CODE` , data = Pooled_main_with_totalasset
              ,index =c("stockid","Report Date"),model='pool',effect='time'))
  
  
  
  
  summary(lm(log(Pooled_main_with_totalasset$EV) ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
                Pooled_main_with_totalasset$`D/A` + Pooled_main_with_totalasset$`Total Asset`+ Pooled_main_with_totalasset$`Div/Share`+ Pooled_main_with_totalasset$OPrevenuegrowth
              + Pooled_main_with_totalasset$ROA , data = Pooled_main_with_totalasset
              ))
  
  
  ####Separating into small and large sizes with both Panel and Pooled data structure:
  
  
  ###Panel
  
  ###Consistent
  summary(plm(log(Large$EV) ~ Large$`Derivative Usage Dummy` + Large$oversea+
                Large$`D/A` + Large$`Total Asset`+ Large$`Div/Share`+ Large$OPrevenuegrowth
              + Large$ROA  + Large$`INDUSTRY CODE`, data = Large
              ,index =c("stockid","Report Date"),model='within'))
  
  
  summary(plm(Large$TQA ~ Large$`Derivative Usage Dummy` + Large$oversea+
                Large$`D/A` + Large$`Total Asset`+ Large$`Dividend Yield`+ Large$OPrevenuegrowth
              + Large$ROA  + Large$`INDUSTRY CODE`, data = Large
              ,index =c("stockid","Report Date"),model='within'))
  
  
  summary(plm(log(Small$EV) ~ Small$`Derivative Usage Dummy` + Small$oversea+
                Small$`D/A` + Small$`Total Asset`+ Small$`Div/Share`+ Small$OPrevenuegrowth
              + Small$ROA  + Small$`INDUSTRY CODE`, data = Small
              ,index =c("stockid","Report Date"),model='within'))
  
  
  
  ##Pooled:
  
  ###Increases EV
  
  summary(plm(log(Pooled_Large$EV) ~ Pooled_Large$`Derivative Usage Dummy` + Pooled_Large$oversea+
                Pooled_Large$`D/A` + Pooled_Large$`Total Asset`+ Pooled_Large$`Div/Share`+ Pooled_Large$OPrevenuegrowth
              + Pooled_Large$ROA  + Pooled_Large$`INDUSTRY CODE`, data = Pooled_Large
              ,index =c("stockid","Report Date"),model='pool'))
  ###Increases EV
  
  
  summary(plm(log(Pooled_Small$EV) ~ Pooled_Small$`Derivative Usage Dummy` + Pooled_Small$oversea+
                Pooled_Small$`D/A` + Pooled_Small$`Total Asset`+ Pooled_Small$`Div/Share`+ Pooled_Small$OPrevenuegrowth
              + Pooled_Small$ROA  + Pooled_Small$`INDUSTRY CODE`, data = Pooled_Small
              ,index =c("stockid","Report Date"),model='pool'))
  
  
  
  ###Change TQ measures:
  ##Not significant with corrected dividend
  summary(plm(main_with_totalasset$ChangeInTQA ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA , data = main_with_totalasset
              ,index =c("stockid","Report Date"),model='random'))
  
  

  
  
  
  
  
  ###not good with corrected dividend
  summary(plm(log(main_with_totalasset$ChangeInTQA) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
              ,index =c("stockid","Report Date"),model='within',effect='time'))
  
  ####Good , DERIVATIVE USAGE INCREASES Enterprise Value.Even with corrected dividend measure
  
  summary(plm(log(main_with_totalasset$EV) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA , data = main_with_totalasset
              ,index =c("stockid","Report Date"),model='within'))
  
  ####Pooled Data measure
  
  ###Not significant AND not consistent
  summary(plm(Pooled_main_with_totalasset$ChangeInTQA ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
                Pooled_main_with_totalasset$`D/A` + Pooled_main_with_totalasset$`Total Asset`+ Pooled_main_with_totalasset$`Div/Share`+ Pooled_main_with_totalasset$OPrevenuegrowth
              + Pooled_main_with_totalasset$ROA , data = Pooled_main_with_totalasset
              ,index =c("stockid","Report Date"),model='pool'))
  
  
  
  
  summary(lm(Pooled_main_with_totalasset$ChangeInTQA ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
                Pooled_main_with_totalasset$`D/A` + Pooled_main_with_totalasset$`Total Asset`+ Pooled_main_with_totalasset$`Div/Share` + Pooled_main_with_totalasset$OPrevenuegrowth
              + Pooled_main_with_totalasset$ROA , data = Pooled_main_with_totalasset
             ))
  
  
  ######SECOND RESEARCHH INTEREST DERIVATIVE usage and firm risk mitigation
  
  ####Two testing dependent variables , CF volatility and  Stock return volatility
  
  ####Works, Deerivative usage decreases CF volatility.
  ####doesnt work with corrected dividend..
  summary(plm(log(main_with_totalasset$StandardDeviation) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset` + main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA  , data = main_with_totalasset
              ,index =c("stockid","Report Date"),model='within'))
  
  #####it worked under the not corrected(with lot of NA in dividend) data.
  summary(plm(FinalMain$StandardDeviation ~ FinalMain$`Derivative Usage Dummy` + FinalMain$oversea+
                FinalMain$`D/A` + log(FinalMain$EV) + FinalMain$`Dividend Yield`+ FinalMain$OPrevenuegrowth
              + FinalMain$ROA + FinalMain$`INDUSTRY CODE`, data = FinalMain
              ,index =c("stockid","Report Date"),model='within'))
  
  
  
  ###work but weak
  
  summary(plm(log(main_with_totalasset$ChangeInCFVolatility) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
              ,index =c("stockid","Report Date"),model='within'))
  
  
  
  summary(plm(log(main_with_totalasset$ChangeInCFVolatility) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA , data = main_with_totalasset
              ,index =c("stockid","Report Date"),model='within'))
  
  
  
  
  
  
  summary(plm(log(main_with_totalasset$ChangeInCFVolatility) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
              ,index =c("stockid","Report Date"),model='pool',effect='time'))
  
  
  
  ###Nothing...reverse significan
  
  summary(plm(log(Pooled_main_with_totalasset$StandardDeviation) ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
                Pooled_main_with_totalasset$`D/A` + Pooled_main_with_totalasset$`Total Asset`+ Pooled_main_with_totalasset$`Div/Share`+ Pooled_main_with_totalasset$OPrevenuegrowth
              + Pooled_main_with_totalasset$ROA + Pooled_main_with_totalasset$`INDUSTRY CODE`, data = Pooled_main_with_totalasset
              ,index =c("stockid","Report Date"),model='pool'))
  
  
  
  ###Now use stock return volatility measures
  
  ####Good###Market measure, use EV
  summary(plm(log(main_with_totalasset$stockreturnvolatility) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                main_with_totalasset$`D/A` + log(main_with_totalasset$EV)+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
              ,index =c("stockid","Report Date"),model='random',effect='time'))
  
  
  summary(plm(log(main_with_totalasset$stockreturnvolatility) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
              ,index =c("stockid","Report Date"),model='pool',effect='time'))
  
  
  
  summary(plm(log(main_with_totalasset$stockreturnvolatility) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
              ,index =c("stockid","Report Date"),model='within',effect='time'))
  
  
  
  summary(plm(log(main_with_totalasset$stockreturnvolatility) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
              ,index =c("stockid","Report Date"),model='random',effect='time'))
  
  
  
  
  
  
  ###Good only with log.. and only with EV , but not Total Asset
  
  summary(lm(log(Pooled_main_with_totalasset$stockreturnvolatility) ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
                Pooled_main_with_totalasset$`D/A` + log(Pooled_main_with_totalasset$EV)+ Pooled_main_with_totalasset$`Div/Share`+ Pooled_main_with_totalasset$OPrevenuegrowth
              + Pooled_main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = Pooled_main_with_totalasset
             ))
  
  
  summary(plm(log(Pooled_main_with_totalasset$stockreturnvolatility) ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
                Pooled_main_with_totalasset$`D/A` + Pooled_main_with_totalasset$`Total Asset`+ Pooled_main_with_totalasset$`Dividend Yield`+ Pooled_main_with_totalasset$OPrevenuegrowth
              + Pooled_main_with_totalasset$ROA , data = Pooled_main_with_totalasset
              ,index =c("stockid","Report Date"),model='pool'))
  
  #########Third research interest Derivative Usage and Stock return
  ###Consistent:
  
  summary(plm(main_with_totalasset$Changeinstockreturn~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                main_with_totalasset$`D/A` + log(main_with_totalasset$EV)+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
              ,index =c("stockid","Report Date"),model="within", effect='time'))
  
  
  
  summary(plm(main_with_totalasset$Changeinstockreturn~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                main_with_totalasset$`D/A` + log(main_with_totalasset$EV)+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
              ,index =c("stockid","Report Date"),model="pool", effect='time'))
  
  
  summary(plm(main_with_totalasset$Changeinstockreturn~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                main_with_totalasset$`D/A` + log(main_with_totalasset$EV)+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
              ,index =c("stockid","Report Date"),model="random", effect='time'))
  
  
  
  
  
  summary(plm(main_with_totalasset$Changeinstockreturn~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
              ,index =c("stockid","Report Date"),model="random", effect='time'))
  
  
  
  
  #####Total asset not significant...
  
  summary(plm(main_with_totalasset$Changeinstockreturn~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
              ,index =c("stockid","Report Date"),model="pool"))
  
  
  
  
  
  
  ###Significant but in the contrary direction..:
  
  summary(plm(main_with_totalasset$`Cumulative return`~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                main_with_totalasset$`D/A` + log(main_with_totalasset$EV)+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
              ,index =c("stockid","Report Date"),model="within",effect='time'))
  
  
  
  
  
  
  
  
  
  #####Alpha 
  

  
  
  summary(plm(main_with_totalasset$`Alpha MarketCap Weighted`~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
              ,index =c("stockid","Report Date"),model="random",effect='time'))
  
  
  
  
  summary(plm(main_with_totalasset$`Alpha MarketCap Weighted`~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
              + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
              ,index =c("stockid","Report Date"),model="random",effect='time'))
  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  ###Removing NA in the Oversea variable.
  ### FinalMain as back up 
  
  
  ###FinalMain is now backup for data before correction of Dividend/share ,updated with dividend share is stored in main_with_totalasset
  ####FinalMain_Pooled is Pooled_main_with_totalasset before dividend correction. 
  
  
  
  
  FinalMain <- main_with_totalasset
  
  for(i in 1:length(main_with_totalasset$stockid)){
    
    if(is.na(main_with_totalasset$oversea[i])){
      
      
      if((main_with_totalasset$stockid[i-1] == main_with_totalasset$stockid[i] )& (!is.na(main_with_totalasset$oversea[i-1]))){
        
        
        main_with_totalasset$oversea[i] <- main_with_totalasset$oversea[i-1]
      
        print(paste0('It is an oversea company',i))
        
      }
      
      else{main_with_totalasset$oversea[i] <- 0}
        
      
      
      }
      
      
    
    
  }
  
  
  
  
  
  ##############################################Just a testing for TQ with mismatched time###### Dony worry about it , it is negative still.... Not consistent with research interest that derivative may inprove TQ

  ##############################Doesn't seem to have any relationship...
  TQtplus1 <- matrix(nrow= length(main_with_totalasset$TQ)+1)
  
  
 
  
  
  for(i in 1:length(main_with_totalasset$TQA)){
    
    if(main_with_totalasset$stockid[i] == main_with_totalasset$stockid[i+1]){
      
      TQtplus1[i] <- main_with_totalasset$TQ[i+1]
      
      
    }
    
    else {
      
      TQtplus1[i] <- NA
    
    }
    
    print(i)
    
    
  }
  #####
  for(i in 1:length(TQtplus1)){
    
    if(is.na(TQtplus1[i])){
      
      
      try[i,3:ncol(try)] <- NA
      
      
    }
    print(i)
    
  }
  

  
  
  
  summary(plm(try$TQ ~ try$`Derivative Usage Dummy` + try$oversea+
                try$`D/A` + try$`Total Asset`+ try$`Dividend Yield`+ try$OPrevenuegrowth
              + try$ROA + try$`INDUSTRY CODE` , data = try
              ,index =c("stockid","Report Date"),model='pool'))
  
  
  
  
 ###similar but this time apply to stock return , i am trying to see, if derivative usage has anything to do with subsequent year's stock return..
  
  
  ###########They dont seem to have any relationships..
  
  
  stockreturntplus1 <- matrix(nrow= length(main_with_totalasset$`Cumulative No div`)+1)
  
  
  for(i in 1:length(main_with_totalasset$`Cumulative No div`)){
    
    if(main_with_totalasset$stockid[i] == main_with_totalasset$stockid[i+1]){
      
      stockreturntplus1[i] <- main_with_totalasset$`Cumulative No div`[i+1]
      
      
    }
    
    else {
      
      stockreturntplus1[i] <- NA
      
    }
    
    print(i)
    
    
  }
  
  ####assigning NAs to cope(make in line) with the new measure
  
  for(i in 1:length(stockreturntplus1)){
    
    if(is.na(stockreturntplus1[i])){
      
      
      try[i,3:ncol(try)] <- NA
      
      
    }
    print(i)
    
  }
  
  summary(plm(stockreturntplus1 ~ try$`Derivative Usage Dummy` + try$oversea+
                try$`D/A` + try$`Total Asset`+ try$`Dividend Yield`+ try$OPrevenuegrowth
              + try$ROA , data = try
              ,index =c("stockid","Report Date"),model='pool'))
  
  
  
  
  
  
  
  ####################Using above method to test for cash flow volatility
  
  
  CFsdtplus1 <- matrix(nrow= length(main_with_totalasset$StandardDeviation)+1)
  
  
  for(i in 1:length(main_with_totalasset$StandardDeviation)){
    
    if(main_with_totalasset$stockid[i] == main_with_totalasset$stockid[i+1]){
      
      CFsdtplus1[i] <- main_with_totalasset$StandardDeviation[i+1]
      
      
    }
    
    else {
      
      CFsdtplus1[i] <- NA
      
    }
    
    print(i)
    
    
  }
  
  ####assigning NAs to cope(make in line) with the new measure
  
  for(i in 1:length(CFsdtplus1)){
    
    if(is.na(CFsdtplus1[i])){
      
      
      try[i,3:ncol(try)] <- NA
      
      
    }
    print(i)
    
  }
  
  summary(plm(log(CFsdtplus1) ~ try$`Derivative Usage Dummy` + try$oversea+
                try$`D/A` + try$`Total Asset` + try$`Div/Share`+ try$OPrevenuegrowth
              + try$ROA , data = try
              ,index =c("stockid","Report Date"),model='within'))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ########################################################
  
  ###correcting dividend data 
  
  
  Divcorrect <- read_csv('分红.csv')
  
  
  names(Divcorrect) <- c('stockid','Report Date','DivPs','PayoutRatio')
  
  for(i in 1:length(main_with_totalasset$stockid)){
    
    if(main_with_totalasset$stockid[i] %in% main_with_totalasset$stockid[which(is.na(main_with_totalasset$`Div/Share`) & main_with_totalasset$`Derivative Usage Dummy` ==1)])
    {
      
      if(is.na(main_with_totalasset$`Div/Share`[i]) & is.na(Divcorrect[which(Divcorrect$stockid == main_with_totalasset$stockid[i] & Divcorrect$`Report Date` == main_with_totalasset$ `Report Date`[i]),][3]))
      
     { main_with_totalasset$`Div/Share`[i] <- 0
      
      
    }
    
    else if(is.na(main_with_totalasset$`Div/Share`[i]) & !is.na(Divcorrect[which(Divcorrect$stockid == main_with_totalasset$stockid[i] & Divcorrect$`Report Date` == main_with_totalasset$ `Report Date`[i]),][3]))
    
{
      
      
      main_with_totalasset$`Div/Share`[i] <- as.numeric(Divcorrect[which(Divcorrect$stockid == main_with_totalasset$stockid[i] & Divcorrect$`Report Date` == main_with_totalasset$ `Report Date`[i]),][3])
      
      
    }
    
    
        
        
        
    }
    
    print(i)
    
  }
  
  
  
  for(i in 15903:length(main_with_totalasset$stockid)){
    
  if(any(Divcorrect$stockid %in% main_with_totalasset$stockid[i])){
      
      if(is.na(main_with_totalasset$`Div/Share`[i]) & is.na(Divcorrect[which(Divcorrect$stockid == main_with_totalasset$stockid[i] & Divcorrect$`Report Date` == main_with_totalasset$ `Report Date`[i]),][3]))
        
      { main_with_totalasset$`Div/Share`[i] <- 0
      
      
      }
      
      else if(is.na(main_with_totalasset$`Div/Share`[i]) & !is.na(Divcorrect[which(Divcorrect$stockid == main_with_totalasset$stockid[i] & Divcorrect$`Report Date` == main_with_totalasset$ `Report Date`[i]),][3]))
        
      {
        
        
        main_with_totalasset$`Div/Share`[i] <- as.numeric(Divcorrect[which(Divcorrect$stockid == main_with_totalasset$stockid[i] & Divcorrect$`Report Date` == main_with_totalasset$ `Report Date`[i]),][3])
        print(paste0('something new'))
        
      }
      
      
      
      
  }
    
    
    print(i)
    
  }
  
  
  
  
  
  
  ##############Trying the robustness test of allynois
  
  ###########THIS LOGIC  IS WRONG....
  
  
  for(i in 1:length(main_with_totalasset$stockid)){
    
    
    if(main_with_totalasset$stockid[i] == main_with_totalasset$stockid[i+1]){
      
      
     if( main_with_totalasset$`Derivative Usage Dummy`[i] ==0 & main_with_totalasset$`Derivative Usage Dummy`[i+1] == 1){
       
       NH <- rbind(NH, data.frame(stockid =main_with_totalasset$stockid[i], `Report Date` =main_with_totalasset$`Report Date`[i] ))
       
       if(main_with_totalasset$stockid[i] %in% HN){
         
         HN[-(which(HN == main_with_totalasset$stockid[i])),]
         print('deleted from HN')
       }
         
       print(main_with_totalasset$stockid[i])
       
       
     
    }
      
      if( main_with_totalasset$`Derivative Usage Dummy`[i] ==1 & main_with_totalasset$`Derivative Usage Dummy`[i+1] == 0){
        
  HN <- rbind(HN, data.frame(stockid =  main_with_totalasset$stockid[i], `Report Date` = main_with_totalasset$`Report Date`[i]))
  if(main_with_totalasset$stockid[i] %in% NH){
    
   NH[-which(NH == main_with_totalasset$stockid[i]),]
    print('deleted from NH')
  }
    
    
  
  
      }
      
      
      if( main_with_totalasset$`Derivative Usage Dummy`[i] ==0 & main_with_totalasset$`Derivative Usage Dummy`[i+1] == 0){
        
        NN <- rbind(NN, data.frame(stockid =  main_with_totalasset$stockid[i], `Report Date` = main_with_totalasset$`Report Date`[i]))
        
      }
      
  
    }
  
  
  }
  
  
  
  
  
  
  for(i in 1:length(try$Robust)){
    
    if( try$stockid[i] %in% HN$stockid){
      
      if(try$`Report Date`[i] == HN$Report.Date[which(try$stockid[i] == HN[,1])]){
      
      try$Robust[i] <- 2
    }
    }
    
    
    if(try$stockid[i] %in% NH$stockid ) {
        
       if(try$`Report Date`[i] == NH$Report.Date[which(try$stockid[i] == NH[,1])]){
      
      try$Robust[i] <- 3
    }
    }
    
    if(try$stockid[i] %in% NN$stockid){
        
        if(try$`Report Date`[i] == NN$Report.Date[which(try$stockid[i] == NN[,1])]){
      
      try$Robust[i] <- 1
    }
    }
    
    print(i)
    
    
  }
  
  
  
  ############TRYING SOMETHING NEW: Robustness Tests of Allayannis and Weston
  
  
  for(i in unique(main_with_totalasset$stockid)){
    
    for(d in 1:length(main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)])){
    
    if(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d] == 1
       & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d] == "2008-12-31"){
      
      
      if(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d+4] == 1
      & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][length(main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)])] == "2012-12-31"){
        
        
        
        HH <- rbind(HH, data.frame(stockid = main_with_totalasset$stockid[which(main_with_totalasset$stockid == i)][1], ReportDate = main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d]))
        
        
        
      }
      
      else if(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d+4] == 0
              & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][length(main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)])] == "2012-12-31"){
        
        
        HN <- rbind(HN, data.frame(stockid =main_with_totalasset$stockid[which(main_with_totalasset$stockid == i)][1], ReportDate = main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d]))
        
        
        
      }
      
    
      
      
      
    }
      if(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d] == 0
         & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d] == "2008-12-31"){
      
        if(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d+4] == 1
           & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][length(main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)])] == "2012-12-31"){
          
          
          NH <- rbind(NH, data.frame(stockid =main_with_totalasset$stockid[which(main_with_totalasset$stockid == i)][1], ReportDate = main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d]))
          
        }
        
        else if(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d+4] == 0
                & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][length(main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)])] == "2012-12-31"){
          
          
          NN <- rbind(NN, data.frame(stockid =main_with_totalasset$stockid[which(main_with_totalasset$stockid == i)][1], ReportDate = main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d]))
          
          
        
        }
    
      
      
      }
     
    }
    print(i)
  }
  
  ####same as above code but with end dates not equal to 2015:
  for(i in unique(main_with_totalasset$stockid)){
    
    for(d in 1:length(main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)])){
      
      if(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d] == 1
         & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d] == "2012-12-31"){
        
        
        if((!is.na(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid==i)][d+2])) & main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d+2] == 1
           & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d+2] == "2014-12-31"){
          
          
          
          HH <- rbind(HH, data.frame(stockid = main_with_totalasset$stockid[which(main_with_totalasset$stockid == i)][1], ReportDate = main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d]))
          
          
          
        }
        
        else if((!is.na(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid==i)][d+2])) & main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d+2] == 0
                & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d+2] == "2014-12-31"){
          
          
          HN <- rbind(HN, data.frame(stockid =main_with_totalasset$stockid[which(main_with_totalasset$stockid == i)][1], ReportDate = main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d]))
          
          
          
        }
        
        
        
        
        
      }
      if(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d] == 0
         & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d] == "2012-12-31"){
        
        if((!is.na(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid==i)][d+2]))& main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d+2] == 1
           & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d+2] == "2014-12-31"){
          
          
          NH <- rbind(NH, data.frame(stockid =main_with_totalasset$stockid[which(main_with_totalasset$stockid == i)][1], ReportDate = main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d]))
          
        }
        
        else if((!is.na(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid==i)][d+2])) & main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d+2] == 0
                & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d+2] == "2014-12-31"){
          
          
          NN <- rbind(NN, data.frame(stockid =main_with_totalasset$stockid[which(main_with_totalasset$stockid == i)][1], ReportDate = main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d]))
          
          
          
        }
        
        
        
      }
      
    }
    print(i)
  }
  
  #############Above is modified version.. 
  
  
  
  
  which(unique(main_with_totalasset$stockid) == i)
  
  
  
  HH <- HN <- NH <- NN <- data.frame()
  try201214 <- filter(try,`Report Date` == "2012-12-31")
  HH$ReportDate <- ymd(HH$ReportDate)
  NH$ReportDate <- ymd(NH$ReportDate)
  
  HN$ReportDate <- ymd(HN$ReportDate)
  
  NN$ReportDate <- ymd(NN$ReportDate)
  
  NN$stockid <- as.character(NN$stockid)
  HN$stockid <- as.character(HN$stockid)
  HH$stockid <- as.character(HH$stockid)
  
  NH$stockid <- as.character(NH$stockid)
  try201214$Robust <- parse_factor(try201214$Robust,levels = c(0,1,2,3))
  ######assigning the above to relevant dates
  
  for(i in 1:length(try201214$Robust)){
    
    if( try201214$stockid[i] %in% HN$stockid){
      
     
        
        try201214$Robust[i] <- 2
      }
    
    
    
    if(try201214$stockid[i] %in% NH$stockid ) {
      
      
        try201214$Robust[i] <- 3
      }
    
    
    if(try201214$stockid[i] %in% NN$stockid){
      
      
        try201214$Robust[i] <- 1
      }
    
    
    
    print(i)
    
    
  }
  
  

  
  ###################Robustness tests
  
  summary(lm(log(try201012$TQA)~ try201012$Robust + try201012$oversea+
               try201012$`D/A` + try201012$`Total Asset`+ try201012$`Div/Share`+ try201012$OPrevenuegrowth
             + try201012$ROA + try201012$`INDUSTRY CODE`, data = try201012
  ))
  
  
  
  
  summary(lm(log(try201012$EV)~ try201012$Robust + try201012$oversea+
               try201012$`D/A` + try201012$`Total Asset`+ try201012$`Div/Share`+ try201012$OPrevenuegrowth
             + try201012$ROA + try201012$`INDUSTRY CODE`, data = try201012
  ))
  
  
  summary(lm(log(try201214$EV)~ try201214$Robust + try201214$oversea+
               try201214$`D/A` + try201214$`Total Asset`+ try201214$`Div/Share`+ try201214$OPrevenuegrowth
             + try201214$ROA + try201214$`INDUSTRY CODE`, data = try201214
  ))
  
  
  summary(lm(log(try201214$TQA)~ try201214$Robust + try201214$oversea+
               try201214$`D/A` + try201214$`Total Asset`+ try201214$`Div/Share`+ try201214$OPrevenuegrowth
             + try201214$ROA + try201214$`INDUSTRY CODE`, data = try201214
  ))
  
  
  
  
  
  ###USE:
  
    summary(lm(log(try200911$TQA)~ try200911$Robust + try200911$oversea+
                 try200911$`D/A` + try200911$`Total Asset`+ try200911$`Div/Share`+ try200911$OPrevenuegrowth
               + try200911$ROA + try200911$`INDUSTRY CODE`, data = try200911
    ))
    
  
  summary(lm(log(try200911$EV)~ try200911$Robust + try200911$oversea+
               try200911$`D/A` + try200911$`Total Asset`+ try200911$`Div/Share`+ try200911$OPrevenuegrowth
             + try200911$ROA + try200911$`INDUSTRY CODE`, data = try200911
  ))
  
  
  
  
  
  
  ###Consistent with Allyanis and weston
  summary(lm(log(try2009$TQA)~ try2009$Robust + try2009$oversea+
                try2009$`D/A` + log(try2009$EV)+ try2009$`Div/Share`+ try2009$OPrevenuegrowth
              + try2009$ROA + try2009$`INDUSTRY CODE`, data = try2009
             ))
  
  
  summary(lm(log(try2009$EV)~ try2009$Robust + try2009$oversea+
               try2009$`D/A` + try2009$`Total Asset`+ try2009$`Div/Share`+ try2009$OPrevenuegrowth
             + try2009$ROA + try2009$`INDUSTRY CODE`, data = try2009
  ))
  
  
  summary(lm(log(try2009$TQA)~ try2009$Robust + try2009$oversea+
               try2009$`D/A` + try2009$`Total Asset`+ try2009$`Div/Share`+ try2009$OPrevenuegrowth
             + try2009$ROA + try2009$`INDUSTRY CODE`, data = try2009
  ))
  
  
  
  ###Consistent
  summary(lm(log(try2013$TQA)~ try2013$Robust + try2013$oversea+
               try2013$`D/A` + try2013$`Total Asset`+ try2013$`Div/Share`+ try2013$OPrevenuegrowth
             + try2013$ROA + try2013$`INDUSTRY CODE`, data = try2013
  ))

  summary(lm(log(try2013$EV)~ try2013$Robust + try2013$oversea+
               try2013$`D/A` + try2013$`Total Asset`+ try2013$`Div/Share`+ try2013$OPrevenuegrowth
             + try2013$ROA + try2013$`INDUSTRY CODE`, data = try2013
  ))
  
  
  ###Consistent
  
  summary(lm(log(try2013$TQA)~ try2013$Robust + try2013$oversea+
               try2013$`D/A` + log(try2013$EV)+ try2013$`Div/Share`+ try2013$OPrevenuegrowth
             + try2013$ROA + try2013$`INDUSTRY CODE`, data = try2013
  ))
  
  
  ###USE
  
  
  summary(lm(log(try2012$TQA)~ try2012$Robust + try2012$oversea+
               try2012$`D/A` + try2012$`Total Asset`+ try2012$`Div/Share`+ try2012$OPrevenuegrowth
             + try2012$ROA + try2012$`INDUSTRY CODE`, data = try2012
  ))
  
  
  
  summary(lm(log(try2012$EV)~ try2012$Robust + try2012$oversea+
               try2012$`D/A` + try2012$`Total Asset`+ try2012$`Div/Share`+ try2012$OPrevenuegrowth
             + try2012$ROA + try2012$`INDUSTRY CODE`, data = try2012
  ))
  
  
  
  
  
  
  
  summary(lm(log(try2011$TQB)~ try2011$Robust + try2011$oversea+
               try2011$`D/A` + try2011$`Total Asset`+ try2011$`Div/Share`+ try2011$OPrevenuegrowth
             + try2011$ROA + try2011$`INDUSTRY CODE`, data = try2011
  ))
  
  
  
  ###NOT
  
  summary(lm(log(try2010$TQA)~ try2010$Robust + try2010$oversea+
               try2010$`D/A` + try2010$`Total Asset`+ try2010$`Div/Share`+ try2010$OPrevenuegrowth
             + try2010$ROA + try2010$`INDUSTRY CODE`, data = try2010
  ))
  
  
  

  ###not SIGNIFICANT BUT right direction.
  
  
  summary(lm(log(try200812$TQA)~ try200812$Robust + try200812$oversea+
               try200812$`D/A` + try200812$`Total Asset`+ try200812$`Div/Share`+ try200812$OPrevenuegrowth
             + try200812$ROA + try200812$`INDUSTRY CODE`, data = try200812
  ))
  
  
  summary(lm(log(try200812$EV)~ try200812$Robust + try200812$oversea+
               try200812$`D/A` + try200812$`Total Asset`+ try200812$`Div/Share`+ try200812$OPrevenuegrowth
             + try200812$ROA + try200812$`INDUSTRY CODE`, data = try200812
  ))
  
  
  
  
  
  ###201014
  
  
  
  summary(lm(log(try201014$TQA)~ try201014$Robust + try201014$oversea+
               try201014$`D/A` + try201014$`Total Asset`+ try201014$`Div/Share`+ try201014$OPrevenuegrowth
             + try201014$ROA + try201014$`INDUSTRY CODE`, data = try201014
  ))
  
  ###USING EV is significant..
  
  summary(lm(log(try201014$EV)~ try201014$Robust + try201014$oversea+
               try201014$`D/A` + try201014$`Total Asset`+ try201014$`Div/Share`+ try201014$OPrevenuegrowth
             + try201014$ROA + try201014$`INDUSTRY CODE`, data = try201014
  ))
  
  
  
###201114 ##tqa IS OK
  
  summary(lm(log(try201114$TQA)~ try201114$Robust + try201114$oversea+
               try201114$`D/A` + try201114$`Total Asset`+ try201114$`Div/Share`+ try201114$OPrevenuegrowth
             + try201114$ROA + try201114$`INDUSTRY CODE`, data = try201114
  ))
  
  
  ###USING EV as control also ok...
  
  
  summary(lm(log(try201114$EV)~ try201114$Robust + try201114$oversea+
               try201114$`D/A` + try201114$`Total Asset`+ try201114$`Div/Share`+ try201114$OPrevenuegrowth
             + try201114$ROA + try201114$`INDUSTRY CODE`, data = try201114
  ))
  
  
  
  
  
write_csv(main_with_totalasset,file ='1main.csv')
  
  
  
  
  
  

###ROBUSTNESS TEST AND CORRECTIONS

tqarobust <- plm(log(main_with_totalasset$TQA) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                   main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
                 + main_with_totalasset$ROA  , data = main_with_totalasset
                 ,index =c("stockid","Report Date"),model='within')



tqaInrobust <- plm(log(main_with_totalasset$TQA) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                     main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
                   + main_with_totalasset$ROA  , data = main_with_totalasset
                   ,index =c("stockid","Report Date"),model='within')




EVrobust <- plm(log(main_with_totalasset$EV) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                  main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
                + main_with_totalasset$ROA  , data = main_with_totalasset
                ,index =c("stockid","Report Date"),model='within')



EVInrobust<- plm(log(main_with_totalasset$EV) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                   main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
                 + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE` , data = main_with_totalasset
                 ,index =c("stockid","Report Date"),model='within')





RiskInrobust<- plm(log(main_with_totalasset$ChangeInCFVolatility) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                   main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
                 + main_with_totalasset$ROA  , data = main_with_totalasset
                 ,index =c("stockid","Report Date"),model='within')


stockvolrobust<- plm(log(main_with_totalasset$stockreturnvolatility) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                     main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
                   + main_with_totalasset$ROA +main_with_totalasset$`INDUSTRY CODE` , data = main_with_totalasset
                   ,index =c("stockid","Report Date"),model='within',effect='time')






coeftest(RiskInrobust, vcovSCC(RiskInrobust,type="HC3"))

coeftest(EVInrobust, vcovSCC(EVInrobust,type="HC3"))

coeftest(tqarobust, vcovHC(EVrobust, method = "arellano"))

bptest(RiskInrobust)

pbgtest(correct1,order=)

  ##change in CFvolatility
bptest(RiskInrobust)
pbgtest(RiskInrobust,order=1)
  
  ####stock volatility 

bptest(stockvolrobust)
pbgtest(stockvolrobust,order=3)
library(tidyverse)
du <- read_csv("regressioncsv1.csv")
library(lubridate)

du <- du[-1]

du$`Report Date` <- ymd(du$`Report Date`)

names(du) <- c('stockid','Report Date','Beg.Amt','End.Amt','DerInvestment Return','Currency Derivative(end)','Commodity/Index Futures(end)','Interest Derivative (end)','Cashflow Hedge','Equity Options','Derivative Usage Dummy','oversea')


####assigning 1 to all companies with derivative usages.



##################这里先不动 ！！！等combine 到MAIN 里面一起弄， 一起assign derivative dummy

for (i in 1:length(du$`Derivative Usage Dummy`)) {
  
  if(is.na(du$Beg.Amt[i] | du$End.Amt[i])){
    
    du$`Derivative Usage Dummy`[i]<- 0
    
    
    
    
    du$`Derivative Usage Dummy`[i] <- 1} else if(is.na(du$Beg.Amt[i] & du$End.Amt[i])) {du$`Derivative Usage Dummy`[i]<- 0}
  else{du$`Derivative Usage Dummy`[i] <- 0}
  
  print(i)
}

#####




du <- mutate(du,Currency_Dummy = 0)

for (i in 1:length(MAIN$Currency_Dummy)) {
  
  if(MAIN$`Currency Derivative(end)`[i] != 0 & !is.na(MAIN$`Currency Derivative(end)`[i])){ 
    
    MAIN$Currency_Dummy[i] <- 1} else{MAIN$Currency_Dummy[i] <- 0}
  
  print(i)
}




###creating derivative dummies by type

du <- mutate(du,Currency_Dummy = 0)

du <- mutate(du,CFutures_Dummy = 0)

du <- mutate(du,Interest_Dummy = 0)

du <- du %>% mutate(Equity_Dummy = 0)

du <- du %>% mutate(CFhedge_Dummy = 0)




######CF HEDGING DUMMY######################

for (i in 1:length(MAIN1$CFhedge_Dummy)) {
  
  if(MAIN1$`Cashflow Hedge`[i] != 0 & !is.na(MAIN1$`Cashflow Hedge`[i])){ 
    
    MAIN1$CFhedge_Dummy[i] <- 1} else{MAIN1$CFhedge_Dummy[i] <- 0 }
  
  print(i)
}








######equity dummy 


for (i in 1:length(MAIN$Equity_Dummy)) {
  
  if(MAIN$`Equity Options`[i] != 0 & !is.na(MAIN$`Equity Options`[i])){ 
    
    MAIN$Equity_Dummy[i] <- 1} else{MAIN$Equity_Dummy[i] <- 0 }
  
  print(i)
}





###assigning futuers dummy


for (i in 1:length(MAIN$CFutures_Dummy)) {
  
  if(MAIN$`Commodity/Index Futures(end)`[i] != 0 & !is.na(MAIN$`Commodity/Index Futures(end)`[i])){ 
    
    MAIN$CFutures_Dummy[i] <- 1} else{MAIN$CFutures_Dummy[i] <- 0 }
  
  print(i)
}


###assigning interest rate dummy



for (i in 1:length(MAIN$`Interest Derivative (end)`)) {
  
  if(MAIN$`Interest Derivative (end)`[i] != 0 & !is.na(MAIN$`Interest Derivative (end)`[i])){ 
    
    MAIN$Interest_Dummy[i] <- 1} else{MAIN$Interest_Dummy[i] <- 0 }
  
  print(i)
}













##changing dummies into factor 

MAIN$Interest_Dummy <- parse_factor(du$Interest_Dummy,levels = c(1,0))
MAIN$CFutures_Dummy <-  parse_factor(du$CFutures_Dummy,levels = c(1,0))
MAIN$Currency_Dummy <-  parse_factor(du$Currency_Dummy,levels = c(1,0))


save.image("Research R Codes.rdata")





tb <- read_csv('TOBINSQ.csv')
#####adding stock id to the du file
###Renaming tobin's q file
names(tb) <- c('stockid','Report Date','Type','D/A','ROA','Div/Share','OPCFstability','OPrevenuegrowth','OPprofitgrowth','MainOPprofitgrowth','ProfitGrowth','EV','TQ','Dividend Yield')

tb <- tb[,-3]

####Merging two dataframes with respect to stock ID and Report Date


###mainData <- merge(x=tb,y=du,by=c("stockid",'Report Date'),all=TRUE)


#####Reading in 关于公司是否有海外业务的dummy组成数据 data




###把tb的stock id 弄成 跟du 一样。


tb1 <- tb

i = 1


while(nchar(tb$stockid[i]) == 1){
  
  
  tb$stockid[i] <-  paste0('00000',tb$stockid[i])
  i <- i+1
  
  
  
  
}

while(nchar(tb$stockid[i]) == 2) {
  
  
  tb$stockid[i] <-  paste0('0000',tb$stockid[i])
  i <- i+1
  
  print(i)
}


while(nchar(tb$stockid[i]) == 3) {
  
  
  tb$stockid[i] <-  paste0('000',tb$stockid[i])
  i <- i+1
  
  print(i)
}


while(nchar(tb$stockid[i]) == 4) {
  
  
  tb$stockid[i] <-  paste0('00',tb$stockid[i])
  i <- i+1
  
  print(i)
}



#########Creating a data set that consists of only company ID 000001 TO 300397 

sampleTB <- filter(tb,stockid < 300398)


####Trying to merge current data

#### MAIN <- merge(x=sampleTB,y=du,by=c("stockid",'Report Date'),all=TRUE)


MAIN <- full_join(x=sampleTB,y=du,by=c("stockid",'Report Date'),all=TRUE)

MAINDU <- full_join(x=du,y=sampleTB,by=c("stockid",'Report Date'),all=TRUE)
#Data mainipulation with Overseas operation data and converting them into dummy variable


##Balance sheet currency information:

Foregncurrencyfrombalancesheet <- read_csv('关于公司是否有海外业务的dummy组成数据/balancesheet.csv')

Foregncurrencyfrombalancesheet <- Foregncurrencyfrombalancesheet[,-1]

names(Foregncurrencyfrombalancesheet) <- c('stockid','Report Date','BalancesheetCurrencyDiff')

#####adding zero before stock id from Foregncurrencyformbalancesheet:
i = 1


while(nchar(Foregncurrencyfrombalancesheet$stockid[i]) == 1){
  
  
  Foregncurrencyfrombalancesheet$stockid[i] <-  paste0('00000',Foregncurrencyfrombalancesheet$stockid[i])
  i <- i+1
  
  
  
  
}

while(nchar(Foregncurrencyfrombalancesheet$stockid[i]) == 2) {
  
  
  Foregncurrencyfrombalancesheet$stockid[i] <-  paste0('0000',Foregncurrencyfrombalancesheet$stockid[i])
  i <- i+1
  
  print(i)
}


while(nchar(Foregncurrencyfrombalancesheet$stockid[i]) == 3) {
  
  
  Foregncurrencyfrombalancesheet$stockid[i] <-  paste0('000',Foregncurrencyfrombalancesheet$stockid[i])
  i <- i+1
  
  print(i)
}


while(nchar(Foregncurrencyfrombalancesheet$stockid[i]) == 4) {
  
  
  Foregncurrencyfrombalancesheet$stockid[i] <-  paste0('00',Foregncurrencyfrombalancesheet$stockid[i])
  i <- i+1
  
  print(i)
}


#Income statement currency info:

incomeCurrency1 <- read_csv('关于公司是否有海外业务的dummy组成数据/利润汇兑收益1.csv')

incomeCurrency1 <- incomeCurrency1[,-1]
names(incomeCurrency1) <- c('stockid','Report Date','ISCurrencyDiff')

incomeCurrency1 <- filter(incomeCurrency1,`Report Date` >= "2005-12-31" )


####Removing duplicated Report Dates item, with similar ISCurrencyDiff, we only gonna take one value per period.

n <- c()

for(i in 1:length(incomeCurrency1$stockid)){
  
  if(incomeCurrency1$`Report Date`[i] == incomeCurrency1$`Report Date`[i+1] ){
    
    n<- c(n,i)
    
  }
  print(i)
  
}

incomeCurrency1 <- incomeCurrency1[-n,]


##############Repitition for 关于公司是否有海外业务的dummy组成数据/利润汇兑收益2.csv


incomeCurrency2 <- read_csv('关于公司是否有海外业务的dummy组成数据/利润汇兑收益2.csv')

incomeCurrency2 <- incomeCurrency2[,-1]
names(incomeCurrency2) <- c('stockid','Report Date','ISCurrencyDiff')

incomeCurrency2 <- filter(incomeCurrency2,`Report Date` >= "2005-12-31" )

incomeCurrency2$stockid <- parse_character(incomeCurrency2$stockid)
####Removing duplicated Dates item for IncomeCurrency2
n <- c()

for(i in 1:length(incomeCurrency2$stockid)){
  
  if(incomeCurrency2$`Report Date`[i] == incomeCurrency2$`Report Date`[i+1] ){
    
    n<- c(n,i)
    
  }
  print(i)
  
}


incomeCurrency2 <- incomeCurrency2[-n,]



###Row binding the two income statement currency gains and loss data:

IS_Currency_GL <- bind_rows(incomeCurrency1,incomeCurrency2)


################Foregn currency related data under Capital Reserves Accounting subject


CapitalAccountForeignCurrency <- read_csv('关于公司是否有海外业务的dummy组成数据/资本公积下面的外币折算1.csv')


CapitalAccountForeignCurrency <- CapitalAccountForeignCurrency[,-1]

names(CapitalAccountForeignCurrency) <- c('stockid','Report Date','ForeignCapital','ForeignCapitalInvDiff')



############Foreign currency data from EVA汇兑.csv

EVAcurrency <-bind_rows(read_csv('关于公司是否有海外业务的dummy组成数据/EVA汇兑1.csv'),read_csv('关于公司是否有海外业务的dummy组成数据/EVA汇兑2.csv'),read_csv('关于公司是否有海外业务的dummy组成数据/EVA汇兑3.csv')
                        
)

names(EVAcurrency) <-c('stockid','Report Date','Exchange Loss')
EVAcurrency <- EVAcurrency[-(1:2),]


EVAcurrency$`Report Date` <- ymd(EVAcurrency$`Report Date`)




######Joining ALL the Foreign currency related data into One big data

Foreign <- full_join(x=IS_Currency_GL,y=CapitalAccountForeignCurrency,by=c("stockid",'Report Date'),all=TRUE)



Foreign <- full_join(x=Foreign,y=Foregncurrencyfrombalancesheet,by=c("stockid",'Report Date'),all=TRUE)



Foreign <- full_join(x=Foreign,y=EVAcurrency,by=c("stockid",'Report Date'),all=TRUE)



####filtering Foreign Curreny Company number 

sampleForeign <- filter(Foreign,stockid < 300398)



########## in the MAIN variable, the MAIN$overseas will take 1 if any samleForeign is not NA, zero otherwise.

##Filtering out companies that do not have foreign currency, extract company that have
withForeign <- filter(sampleForeign,!is.na(ISCurrencyDiff)|!is.na(ForeignCapital)|!is.na(ForeignCapitalInvDiff)|!is.na(BalancesheetCurrencyDiff)|!is.na(`Exchange Loss`))

###Confirmation check:


num <- c() 

for(i in 1:length(sampleForeign$stockid)) {
  
  if(any(!is.na(sampleForeign[i,3:7]))){
    
    num <- c(num,sampleForeign$stockid[i])
    
  }
  
  
  print(i)
  
  
}

###if length of num equals length of withForeign$stockid  , then we are good

for(i in MAIN1$stockid){
  
  if(i %in% withForeign$stockid) {
    
    MAIN1[which(MAIN1$stockid == i),12] <- 1
    
  }
  else {MAIN1[which(MAIN1$stockid == i),12] <- 0}
  
  
  print(i)
}




###########NOW ASSIGNING DERIVATIVE USAGE DUMMY..

#Write a function to denote "not in" something..:

`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))




################## IF no is utilized , assign dummy with value 0, or else equals 1.
#######The latest MAIN1 IS CF PER SHARE MAIN ---UPDATED 9/5/2017
for (i in 1:length(MAIN1$stockid)) {
  
  if(i %not in% which(is.na(MAIN1$Beg.Amt) & is.na(MAIN1$`Currency Derivative(end)`) & is.na(MAIN1$End.Amt) & is.na(MAIN1$`DerInvestment Return`) & is.na(MAIN1$`Commodity/Index Futures(end)`) & is.na(MAIN1$`Interest Derivative (end)`) & is.na(MAIN1$`Cashflow Hedge`) & is.na(MAIN1$`Equity Options`)))
  {
    
    MAIN1[i,11] <- 1
    
    
  }
  
  else {MAIN1[i,11] <- 0}
  print(i)
}

########Arranging MAIN in turns of the right order of Report date.
MAIN <- arrange(MAINDU, stockid,`Report Date`)

MAIN <- mutate(MAIN, averageusage = (Beg.Amt + End.Amt)/2)


#############Monotonic TEST





summary(felm(MAIN$TQ ~ MAIN$`Derivative Usage Dummy` + MAIN$oversea+
               MAIN$`D/A` + MAIN$EV + MAIN$`Dividend Yield`+ MAIN$OPrevenuegrowth
             + MAIN$ROA, data = MAIN))







###Currency dummy regression FIXED EFFECT
summary(felm(MAIN$TQ ~  MAIN$oversea+
               MAIN$`D/A` + MAIN$EV + MAIN$`Dividend Yield`+ MAIN$OPrevenuegrowth
             + MAIN$ROA + MAIN$Currency_Dummy, data = MAIN))




###

summary(felm(MAIN$TQ ~  MAIN$oversea+
               MAIN$`D/A` + MAIN$EV + MAIN$`Div/Share`+ MAIN$OPrevenuegrowth
             + MAIN$ROA + MAIN$Currency_Dummy + MAIN$CFutures_Dummy + MAIN$Interest_Dummy + MAIN$Equity_Dummy, data = MAIN))


####Interest usage dummy


summary(felm(MAIN$TQ ~  MAIN$oversea+
               MAIN$`D/A` + MAIN$EV + MAIN$`Div/Share`+ MAIN$OPrevenuegrowth
             + MAIN$ROA + MAIN$Interest_Dummy, data = MAIN))

#######POOLED OLS:





summary(lm(MAIN$TQ ~ MAIN$`Report Date`+  MAIN$`Derivative Usage Dummy` + MAIN$oversea+
             MAIN$`D/A` + log(MAIN$EV) + MAIN$`Dividend Yield`+ MAIN$OPrevenuegrowth
           + MAIN$ROA, data = MAIN))

####FIXED EFFECT TESTING 1 WITH 'felm' package


summary(felm(MAIN$TQ ~ MAIN$`Report Date`+  MAIN$`Derivative Usage Dummy` + MAIN$oversea+
               MAIN$`D/A` + log(MAIN$EV) + MAIN$`Dividend Yield`+ MAIN$OPrevenuegrowth
             + MAIN$ROA, data = MAIN))

######FIXED EFFECT TESTING 2 WITH PLM package and Rearranged MAIN data  >> testMAIN
testMAIN <- arrange(MAIN, `Report Date`,stockid)

summary(plm(testMAIN$TQ ~ testMAIN$`Report Date`+ testMAIN$`Derivative Usage Dummy` + testMAIN$oversea+
              testMAIN$`D/A` + log(testMAIN$EV) + testMAIN$`Dividend Yield`+ testMAIN$OPrevenuegrowth
            + testMAIN$ROA, data = MAIN,index =c("stockid","Report Date") ,model="within",effect = 'time'))

##REMOVING THE REPORT DATE:

summary(plm(testMAIN$TQ ~ testMAIN$`Derivative Usage Dummy` + testMAIN$oversea+
              testMAIN$`D/A` + log(testMAIN$EV) + testMAIN$`Dividend Yield`+ testMAIN$OPrevenuegrowth
            + testMAIN$ROA, data = MAIN,index =c("stockid","Report Date") ,model="within",effect = 'time'))




#####Making the difference of CF/SHER volatility  included MAIN1 data into once that you could run with Fixed model
######Final version

testMAIN1 <- arrange(MAIN1, `Report Date`,stockid)

summary(plm(testMAIN1$TQ ~ testMAIN1$`Derivative Usage Dummy` + testMAIN1$oversea+
              testMAIN1$`D/A` + log(testMAIN1$EV) + testMAIN1$`Dividend Yield`+ testMAIN1$OPrevenuegrowth
            + testMAIN1$ROA, data = testMAIN1,index =c("stockid","Report Date") ,model="within",effect = 'time'))


summary(plm(MAIN1$TQ ~ MAIN1$`Report Date`+ MAIN1$`Derivative Usage Dummy` + MAIN1$oversea+
              MAIN1$`D/A` + log(MAIN1$EV) + MAIN1$`Dividend Yield`+ MAIN1$OPrevenuegrowth
            + MAIN1$ROA, data = MAIN1,index =c("stockid","Report Date") ,model="within",effect = 'time'))



summary(plm(testMAIN1$ChangeInTQ ~ testMAIN1$`Derivative Usage Dummy` + testMAIN1$oversea+
              testMAIN1$`D/A` + log(testMAIN1$EV) + testMAIN1$`Dividend Yield`+ testMAIN1$OPrevenuegrowth
            + testMAIN1$ROA, data = testMAIN1,index =c("stockid","Report Date") ,model="within",effect = 'time'))





summary(plm(MAIN1$ChangeInTQ ~ MAIN1$`Report Date`+ MAIN1$`Derivative Usage Dummy` + MAIN1$oversea+
              MAIN1$`D/A` + log(MAIN1$EV) + MAIN1$`Dividend Yield`+ MAIN1$OPrevenuegrowth
            + MAIN1$ROA, data = MAIN1,index =c("stockid","Report Date") ,model="within",effect = 'time'))





#####Assing the CF/SHARE test main MAIN1 to THE TIME ARRANGED MAIN2



#######Wrong , it shrinks the data size  CFMAIN <- filter(MAIN1, `Report Date` == MAIN$`Report Date`)


MAIN2 <- arrange(CFMAIN, `Report Date`,stockid)
summary(plm(MAIN2$StandardDeviation ~ MAIN2$`Derivative Usage Dummy` + MAIN2$oversea+
              MAIN2$`D/A` + log(MAIN2$EV) + MAIN2$`Dividend Yield`+ MAIN2$OPrevenuegrowth
            + MAIN2$ROA, data = MAIN2,index =c("stockid","Report Date") ,model="within",effect = 'time'))


###MAAC Beth




summary(pmg(MAIN1$StandardDeviation ~ MAIN1$`Derivative Usage Dummy` + MAIN1$oversea+
              MAIN1$`D/A` + log(MAIN1$EV) + MAIN1$`Dividend Yield`+ MAIN1$OPrevenuegrowth
            + MAIN1$ROA, data = MAIN1,index =c('Report Date',"stockid")))



###coeftest(pmg(CFMAIN$StandardDeviation ~ CFMAIN$`Derivative Usage Dummy` + CFMAIN$oversea+
#   CFMAIN$`D/A` + log(CFMAIN$EV) + CFMAIN$`Dividend Yield`+ CFMAIN$OPrevenuegrowth
# + CFMAIN$ROA, data = CFMAIN,index=c('Report Date','stockid')))


#summary(felm(CFMAIN$StandardDeviation ~ CFMAIN$`Derivative Usage Dummy` + CFMAIN$oversea+
## CFMAIN$`D/A` + log(CFMAIN$EV) + CFMAIN$`Dividend Yield`+ CFMAIN$OPrevenuegrowth
##+ CFMAIN$ROA, data = CFMAIN))








###TESTING FD method on testMAIN1 07/06/2017  - - - NOT WORKING, IT INCREASES STANDARD DEVIATION..


summary(plm(testMAIN1$StandardDeviation ~ testMAIN1$`Derivative Usage Dummy` + testMAIN1$oversea+
              testMAIN1$`D/A` + log(testMAIN1$EV) + testMAIN1$`Dividend Yield`+ testMAIN1$OPrevenuegrowth
            + testMAIN1$ROA, data = testMAIN1
            ,index =c("Report Date","stockid"),model='fd',effect='individual'))




summary(plm(main_with_totalasset$TQ ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset` + main_with_totalasset$`Dividend Yield`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA, data = main_with_totalasset
            ,index =c("stockid","Report Date"),model='random'))




summary(plm(L$ChangeInTQ ~ L$`Derivative Usage Dummy` + L$oversea+
              L$`D/A` + L$`Total Asset` + L$`Dividend Yield`+ L$OPrevenuegrowth
            + L$ROA, data = L
            ,index =c("stockid","Report Date"),model='random'))






######################Differenced regression#### 9/5/2017



#summary(lm(ChangeInCFvolatility~ MAIN1$`Derivative Usage Dummy` + MAIN1$oversea+
# MAIN1$`D/A` + log(MAIN1$EV) + MAIN1$`Dividend Yield`+ MAIN1$OPrevenuegrowth
#+ MAIN1$ROA, data = MAIN1))













####Fixed Effect#################FINAL RESULT!

####Consistent with J.M Nelson et al's finding , they found that ,if we were to test TQ with small and large firms all at once
###they get a negative DERIVATIVE coefficient, they realize that this is because allayannis used only large firms, 
###if we use only large firms, that are larger than the median , we get a consistent result.

median(testMAIN1$EV,na.rm=T)
LARGEFIRMS <- filter(testMAIN1,EV>3059957951)
summary(plm(LARGEFIRMS$TQ~ LARGEFIRMS$`Derivative Usage Dummy` + LARGEFIRMS$oversea+
              LARGEFIRMS$`D/A` + log(LARGEFIRMS$EV) + LARGEFIRMS$`Dividend Yield`+ LARGEFIRMS$OPrevenuegrowth
            + LARGEFIRMS$ROA, data = LARGEFIRMS, index = c("Report Date","stockid"),model = 'within',effect='time'))









summary(plm(testMAIN1$TQ ~ testMAIN1$`Derivative Usage Dummy` + testMAIN1$oversea+
              testMAIN1$`D/A` + log(testMAIN1$EV) + testMAIN1$`Dividend Yield`+ testMAIN1$OPrevenuegrowth
            + testMAIN1$ROA, data =testMAIN1,index =c("stockid","Report Date"),model='within',effect='time'))

######random
summary(plm(testMAIN1$TQ ~ testMAIN1$`Derivative Usage Dummy` + testMAIN1$oversea+
              testMAIN1$`D/A` + log(testMAIN1$EV) + testMAIN1$`Dividend Yield`+ testMAIN1$OPrevenuegrowth
            + testMAIN1$ROA, data =testMAIN1,index =c("stockid","Report Date"),model='random',effect='time'))




summary(plm(MAIN1$TQ ~ MAIN1$`Derivative Usage Dummy` + MAIN1$oversea+
              MAIN1$`D/A` + log(MAIN1$EV) + MAIN1$`Dividend Yield`+ MAIN1$OPrevenuegrowth
            + MAIN1$ROA, data =MAIN1,index =c("stockid","Report Date"),model='within',effect='time'))








#########Random effect







summary(plm(MAIN1$TQ ~ MAIN1$`Derivative Usage Dummy` + MAIN1$oversea+
              MAIN1$`D/A` + log(MAIN1$EV) + MAIN1$`Dividend Yield`+ MAIN1$OPrevenuegrowth
            + MAIN1$ROA, data =MAIN1,index =c("stockid","Report Date"),model='random'))





summary(plm(MAIN1$StandardDeviation ~ MAIN1$`Derivative Usage Dummy` + MAIN1$oversea+
              MAIN1$`D/A` + log(MAIN1$EV) + MAIN1$`Dividend Yield`+ MAIN1$OPrevenuegrowth
            + MAIN1$ROA, data =MAIN1,index =c("stockid","Report Date"),model='random'))





##########################TESTING THE CHANGE OF THE NETOPCF/SHARE #####PRESENTATION RESULT!!!!


##worked with between model!!!!

summary(plm(testMAIN1$ChangeInCFVolatility~ testMAIN1$`Derivative Usage Dummy` + testMAIN1$oversea+
              testMAIN1$`D/A` + log(testMAIN1$EV) + testMAIN1$`Dividend Yield`+ testMAIN1$OPrevenuegrowth
            + testMAIN1$ROA, data = testMAIN1, index = c("stockid","Report Date"),model = 'between'))


#####rearranged is testMAIN1 arranged with respect to stockid and Report date: this is the data needed to look like if we were to regress with a goal to remove time fixed effects etc.
###Below two measures are ALL CONSISTENT!
rearranged <- arrange(testMAIN1,stockid,`Report Date`)

summary(plm(rearranged$ChangeInCFVolatility~ rearranged$`Derivative Usage Dummy` + rearranged$oversea+
              rearranged$`D/A` + log(rearranged$EV) + rearranged$`Dividend Yield`+ rearranged$OPrevenuegrowth
            + rearranged$ROA, data = rearranged, index = c("stockid","Report Date"),model = 'between'))



summary(plm(rearranged$ChangeInTQ~ rearranged$`Derivative Usage Dummy` + rearranged$oversea+
              rearranged$`D/A` + log(rearranged$EV) + rearranged$`Dividend Yield`+ rearranged$OPrevenuegrowth
            + rearranged$ROA, data = rearranged, index = c("stockid","Report Date"),model = 'within'))


#######################################






summary(plm(MAIN1$ChangeInCFVolatility~ MAIN1$`Derivative Usage Dummy` + MAIN1$oversea+
              MAIN1$`D/A` + log(MAIN1$EV) + MAIN1$`Dividend Yield`+ MAIN1$OPrevenuegrowth
            + MAIN1$ROA, data = MAIN1, index = c("stockid","Report Date"),model ='between'))





summary(lm(testMAIN1$ChangeInCFVolatility~ testMAIN1$`Derivative Usage Dummy` + testMAIN1$oversea+
             testMAIN1$`D/A` + log(testMAIN1$EV) + testMAIN1$`Dividend Yield`+ testMAIN1$OPrevenuegrowth
           + testMAIN1$ROA, data = testMAIN1))





summary(lm(MAIN1$ChangeInCFVolatility~ MAIN1$`Derivative Usage Dummy` + MAIN1$oversea+
             MAIN1$`D/A` + log(MAIN1$EV) + MAIN1$`Dividend Yield`+ MAIN1$OPrevenuegrowth
           + MAIN1$ROA, data = MAIN1))






summary(plm(TT$SD~ TT$`Derivative Usage Dummy` + TT$oversea+
              TT$`D/A` + log(TT$EV) + TT$`Dividend Yield`+ TT$OPrevenuegrowth
            + TT$ROA, data = TT,index=c('stockid','Report Date'),model='within',effect = 'time'))

TTarg <- arrange(TT, `Report Date`,stockid)


summary(plm(TTarg$SD~ TTarg$`Derivative Usage Dummy` + TTarg$oversea+
              TTarg$`D/A` + log(TTarg$EV) + TTarg$`Dividend Yield`+ TTarg$OPrevenuegrowth
            + TTarg$ROA, data = TTarg,index=c('stockid','Report Date'),model='within',effect = 'time'))



summary(lm(TTarg$ChangeInCFVolatility~ TTarg$`Derivative Usage Dummy` + TTarg$oversea+
             TTarg$`D/A` + log(TTarg$EV) + TTarg$`Dividend Yield`+ TTarg$OPrevenuegrowth
           + TTarg$ROA, data = TTarg))




##########testMAIN1 <<< IS THE MAIN DATA WITH CF VARIABLES!!!!USE IT!!




summary(lm(DIFFERENCE$DIFFERENCE ~ CFMAIN[-1,]$`Derivative Usage Dummy` + CFMAIN[-1,]$oversea+
             CFMAIN[-1,]$`D/A` + log(CFMAIN[-1,]$EV) + CFMAIN[-1,]$`Dividend Yield`+ CFMAIN[-1,]$OPrevenuegrowth
           + CFMAIN[-1,]$ROA, data = CFMAIN[-1,]))





#######no dates included



summary(lm(testMAIN$TQ ~ testMAIN$`Derivative Usage Dummy` + testMAIN$oversea+
             testMAIN$`D/A` + log(testMAIN$EV) + testMAIN$`Dividend Yield`+ testMAIN$OPrevenuegrowth
           + testMAIN$ROA, data = MAIN))




#####This is a function that adds zero to stockid







Addzero <- function(x){
  
  i = 1
  j <- x
  
  while(nchar(x[i]) == 1){
    
    
    j[i] <- paste0('00000',x[i])
    i <- i+1
    
    
    
  }
  
  while(nchar(x[i]) == 2) {
    
    
    j[i] <-  paste0('0000',x[i])
    i <- i+1
    
    print(i)
  }
  
  while(nchar(x[i]) == 3) {
    
    
    j[i] <-  paste0('000',x[i])
    i <- i+1
    
    print(i)
  }
  
  while(nchar(x[i]) == 4) {
    
    
    j[i] <-  paste0('00',x[i])
    
    
    if(i < length(x)){i <- i+1} else {break}
    
    print(i)
  }
  
  
  
  return(j)
}



#####Import Total Asset

totalassets1 <- bind_rows(read_csv('资产总额/资产总额1.csv'),read_csv('资产总额/资产总额2.csv'))

names(totalassets1) <- c('stockid','Report Date','Total Asset')

for(i in 1:length(totalassets1$`Report Date`)){
  
  if(!is.na(totalassets1$`Total Asset`[i])){
    
    if(totalassets1$stockid[i] == totalassets1$stockid[i+1] & totalassets1$`Report Date`[i] == totalassets1$`Report Date`[i+1] ){
      
      
      
      
      totalassets1 <- totalassets1[-(i),]
      
      
      
      
    }
    
  }
  
  
  print(i)
  
}

beep()

######君安的tobinsq



JUNANTQ <- read_csv('君安tobinsq/君安tobinsq.csv')
names(JUNANTQ) <- c('stockid','Report Date','INDUSTRY CODE','TQA','TQB','TQC','TQD')


JUNANTQ <- (filter(JUNANTQ,stockid <300398))

######08/06/2017 导入beta and alpha


Beta_Alpha <- read_csv('beta and alpha/beta and alpha.csv')

Beta_Alpha <- Beta_Alpha[,-2]

names(Beta_Alpha) <- c('stockid','Report Date','Alpha Float Weighted','Beta Float Weighted','Alpha MarketCap Weighted','Beta MarketCap Weighted')


Beta_Alpha <- filter(Beta_Alpha,stockid<300430)


##checking duplicated years

for(i in 1:length(Beta_Alpha$`Report Date`)){
  if(Beta_Alpha$stockid[i] == Beta_Alpha$stockid[i+1] & year(Beta_Alpha$`Report Date`[i]) == year(Beta_Alpha$`Report Date`[i+1])){
    
    print(Beta_Alpha$stockid[i])
    
  }
  print(i)
}

##change dates not equal to xx/12/31 to xx/12/31:

month(Beta_Alpha$`Report Date`[which(month(Beta_Alpha$`Report Date`) != 12)]) <- 12


day(Beta_Alpha$`Report Date`[which(day(Beta_Alpha$`Report Date`) != 31)]) <- 31


main_with_totalasset <- left_join(main_with_totalasset,Beta_Alpha,by=c('stockid','Report Date'),all=TRUE)




#####Importing and dealing with stock cumulated annual return data


CAR <- read_csv('年收益与波动/年累计收益率.csv')

CAR <- CAR[,-2]

names(CAR) <- c('stockid','Report Date','Cumulative return')

month(CAR$`Report Date`[which(month(CAR$`Report Date`) != 12)]) <- 12


day(CAR$`Report Date`[which(day(CAR$`Report Date`) != 31)]) <- 31



main_with_totalasset <- left_join(main_with_totalasset,CAR,by=c('stockid','Report Date'),all=TRUE)




Pooled_main_with_totalasset <- left_join(Pooled_main_with_totalasset,CAR, by=c('stockid','Report Date'),all=TRUE)



#####Junan's stock return

JCAR <- read_csv('年收益与波动/JUNAN年累计收益率.csv')


names(JCAR) <- c('stockid','Report Date','CR NO DIV','Cumulative No div')

JCAR <- JCAR[-1,]

for(i in 1:length(JCAR$`Report Date`)){
  
  if(JCAR$`Report Date`[i] == '2015'){ JCAR$`Report Date`[i] <-'2015-12-31'}
  if(JCAR$`Report Date`[i] == '2014') {JCAR$`Report Date`[i] <- '2014-12-31'}
  if(JCAR$`Report Date`[i] == '2013') {JCAR$`Report Date`[i] <- '2013-12-31'}
  if(JCAR$`Report Date`[i] == '2012') {JCAR$`Report Date`[i] <- '2012-12-31'}
  if(JCAR$`Report Date`[i] == '2011') {JCAR$`Report Date`[i] <- '2011-12-31'}
  if(JCAR$`Report Date`[i] == '2010') {JCAR$`Report Date`[i] <- '2010-12-31'}
  if(JCAR$`Report Date`[i] == '2009') {JCAR$`Report Date`[i] <- '2009-12-31'}
  if(JCAR$`Report Date`[i] == '2008') {JCAR$`Report Date`[i] <- '2008-12-31'}
  
  if(JCAR$`Report Date`[i] == '2007') {JCAR$`Report Date`[i] <- '2007-12-31'}
  if(JCAR$`Report Date`[i] == '2006') {JCAR$`Report Date`[i] <- '2006-12-31'}
  if(JCAR$`Report Date`[i] == '2005') {JCAR$`Report Date`[i] <- '2005-12-31'}
  
  
  print(i)
  
}

JCAR <- filter(JCAR,stockid < 300430)



main_with_totalasset <-  left_join(main_with_totalasset,JCAR,by=c('stockid','Report Date'),all=TRUE)




#######股价年波动数据导入与cleaning


VOL <- read_csv('个股年波动率.csv')



names(VOL) <- c('stockid','Report Date','stockreturnvolatility')

month(VOL$`Report Date`[which(month(VOL$`Report Date`) != 12)]) <- 12


day(VOL$`Report Date`[which(day(VOL$`Report Date`) != 31)]) <- 31



main_with_totalasset <- left_join(main_with_totalasset,VOL,by=c('stockid','Report Date'),all=TRUE)

Pooled_main_with_totalasset <- left_join(Pooled_main_with_totalasset,CAR, by=c('stockid','Report Date'),all=TRUE)










######CF per share import

CF1 <- read_csv('CF Per share Measures/CF per share1.csv')
CF1 <- CF1[-1,]



names(CF1) <- c('stockid','Report Date','SALE/SHARE TTM','OPPROFT/SHARE','NETOPCF/SHARE','NETOPCF/SHARETTM','FCFF/SHARE','FCFF/SHARETTM','FCFFORIG/SHARE','FCFTTM/SHARE1')
CF1$`Report Date` <- ymd(CF1$`Report Date`)
###Transforming CF1 according to stockid , and report date accending order

CF1 <- arrange(CF1,stockid,Year,Month)

####Splitting the current Report Date into Year column, month column, day column

datetxt <- CF1$`Report Date`
datetxt <- as.Date(datetxt)
df <- tibble(date = datetxt,
             year = as.numeric(format(datetxt, format = "%Y")),
             month = as.numeric(format(datetxt, format = "%m")),
             day = as.numeric(format(datetxt, format = "%d")))


CF0 <- CF1
CF1 <- mutate(CF1, Year = df$year)

CF1 <- mutate(CF1, Month = df$month)

CF1$`NETOPCF/SHARETTM` <- parse_double(CF1$`NETOPCF/SHARETTM`)
CFsd1 <- data.frame()
temp<-c()
for(i in 1:length(CF1$Year)){
  
  
  
  
  if(CF1$Month[i] %in% c(3,6,9)){ temp <- c(temp,CF1$`NETOPCF/SHARETTM`[i])
  
  
  }
  else if(CF1$Month[i] %in% 12)
    
  { 
    temp <- c(temp,CF1$`NETOPCF/SHARETTM`[i])
    
    sdd <- sd(c(temp,na.rm=T),na.rm=T)
    CFsd1 <- rbind(CFsd1,c(sdd,i,as.numeric(CF1$stockid)[i],CF1$`Report Date`[i]))
    
    temp <-c()
    
    
  }
  
  
  print(i)
  
}

##############Turning the CF Standard deviation Data into Tibble data.
library('zoo')
CFsd1$X13148 <- as.Date(CFsd1$X13148)


names(CFsd1) <- c('StandDeviation','i Counts','stockid','Report Date')
CFsd1 <- tibble(CFsd1$stockid,CFsd1$`Report Date`,CFsd1$`i Counts`,CFsd1$StandDeviation)


names(CFsd1)  <- c('stockid','Report Date','i Counts','StandardDeviation')



###adding zero
i = 1


while(nchar(CF1$stockid[i]) == 1){
  
  
  CF1$stockid[i] <-  paste0('00000',CF1$stockid[i])
  i <- i+1
  
  
  
  
}

while(nchar(CF1$stockid[i]) == 2) {
  
  
  CF1$stockid[i] <-  paste0('0000',CF1$stockid[i])
  i <- i+1
  
  print(i)
}


while(nchar(CF1$stockid[i]) == 3) {
  
  
  CF1$stockid[i] <-  paste0('000',CF1$stockid[i])
  i <- i+1
  
  print(i)
}


while(nchar(CF1$stockid[i]) == 4) {
  
  
  CF1$stockid[i] <-  paste0('00',CF1$stockid[i])
  i <- i+1
  
  print(i)
}


##########Remove Duplicated dates on same stockid


for(i in 1:length(CF1$Month)){
  
  if(!is.na(CF1$Month[i])){
    
    if(CF1$stockid[i] == CF1$stockid[i+1] & CF1$Year[i] == CF1$Year[i+1] ){
      
      
      if(CF1$Month[i] == CF1$Month[i+1]){
        
        
        CF1 <- CF1[-(i),]
        
        
      }
      
    }
    
  }
  
  
  print(i)
  
}

beep()



######import CF2


CF2 <- read_csv('CF Per share Measures/CF per share2.csv')
CF2 <- CF2[-1,]



names(CF2) <- c('stockid','Report Date','SALE/SHARE TTM','OPPROFT/SHARE','NETOPCF/SHARE','NETOPCF/SHARETTM','FCFF/SHARE','FCFF/SHARETTM','FCFFORIG/SHARE','FCFTTM/SHARE1')
CF2$`Report Date` <- ymd(CF2$`Report Date`)
###Transforming CF1 according to stockid , and report date accending order



####Splitting the current Report Date into Year column, month column, day column

datetxt <- CF2$`Report Date`
datetxt <- as.Date(datetxt)
df <- tibble(date = datetxt,
             year = as.numeric(format(datetxt, format = "%Y")),
             month = as.numeric(format(datetxt, format = "%m")),
             day = as.numeric(format(datetxt, format = "%d")))


CF0 <- CF2
CF2 <- mutate(CF2, Year = df$year)

CF2 <- mutate(CF2, Month = df$month)


CF2$`NETOPCF/SHARETTM` <- parse_double(CF2$`NETOPCF/SHARETTM`)
CFsd2 <- data.frame()
temp<-c()
for(i in 1:length(CF2$Year)){
  
  
  
  
  if(CF2$Month[i] %in% c(3,6,9)){ temp <- c(temp,CF2$`NETOPCF/SHARETTM`[i])
  
  
  }
  else if(CF2$Month[i] %in% 12)
    
  { 
    temp <- c(temp,CF2$`NETOPCF/SHARETTM`[i])
    
    sdd <- sd(c(temp,na.rm=T),na.rm=T)
    CFsd2 <- rbind(CFsd2,c(sdd,i,as.numeric(CF2$stockid)[i],CF2$`Report Date`[i]))
    
    temp <-c()
    
    
  }
  
  
  print(i)
  
}


library('zoo')
CFsd2$X14974 <- as.Date(CFsd2$X14974)


names(CFsd2) <- c('StandDeviation','i Counts','stockid','Report Date')
CFsd2$stockid <- Addzero(CFsd2$stockid)

CFsd2 <- tibble(CFsd2$stockid,CFsd2$`Report Date`,CFsd2$`i Counts`,CFsd2$StandDeviation)


names(CFsd2) <- names(CFsd2) <- c('stockid','Report Date','i Counts','StandardDeviation')

####Double check:

##take CF2[901:905,] for example:

# A tibble: 5 × 12
# stockid `Report Date` `SALE/SHARE TTM`
# <chr>        <date>            <chr>
#  1  002466    2011-03-31             <NA>
#  2  002466    2011-06-30             <NA>
#  3  002466    2011-09-30         2.593742#
# 4  002466    2011-12-31          2.37499#
# 5  002466    2012-03-31           2.4498#


## CFsd2$X3[239:242]
# 900 904 908 912
#### > CFsd2$X0.748879114285143[239:242]
## [1] 0.8967585 0.6495543 0.5006302 0.8151800

##The CFsd2  239 to 242 row , is corresponding to 
#row in CF2 900 to 912 row.


#sd(c(CF2$`NETOPCF/SHARETTM`[901:904],na.rm=T),na.rm=T)
#[1] 0.6495543


#901:904 is a complete year, for stock 002466 , from 2011-03-31 to
# 2011-12-31


#thereby,standarde deviaition calculated explicitly sd(c(CF2$`NETOPCF/SHARETTM`[901:904],na.rm=T),na.rm=T)
#should equate to CFsd2$X0.748879114285143[239:242][2]
#which is 0.6495543















###adding zero
i = 1


while(nchar(CF2$stockid[i]) == 1){
  
  
  CF2$stockid[i] <-  paste0('00000',CF2$stockid[i])
  i <- i+1
  
  
  
  
}

while(nchar(CF2$stockid[i]) == 2) {
  
  
  CF2$stockid[i] <-  paste0('0000',CF2$stockid[i])
  i <- i+1
  
  print(i)
}


while(nchar(CF2$stockid[i]) == 3) {
  
  
  CF2$stockid[i] <-  paste0('000',CF2$stockid[i])
  i <- i+1
  
  print(i)
  
}


while(nchar(CF2$stockid[i]) == 4) {
  
  
  CF2$stockid[i] <-  paste0('00',CF2$stockid[i])
  i <- i+1
  
  print(i)
  
}
beep()



########################
CF2 <- arrange(CF2,stockid,Year,Month)
##########Remove Duplicated dates on same stockid


for(i in 1:length(CF2$Month)){
  
  if(!is.na(CF2$Month[i])){
    
    if(CF2$stockid[i] == CF2$stockid[i+1] & CF2$Year[i] == CF2$Year[i+1] ){
      
      if(CF2$Month[i] == CF2$Month[i+1]){
        
        CF2 <- CF2[-(i),]
        
      }
      
    }
    
  }
  
  print(i)
  
  
}

beep()



############CF per share 3





######Trying to delete duplicated dates in MAIN1 ##############
#############################################################


for(i in 1:length(MAIN1$stockid)){
  
  
  
  if(MAIN1$stockid[i] == MAIN1$stockid[i+1] & MAIN1$`Report Date`[i] == MAIN1$`Report Date`[i+1] ){
    
    
    
    MAIN1 <- MAIN1[-(i),]
    
    
    
  }
  
  
  
  print(i)
  
  
}

beep()



















CF3 <- read_csv('CF Per share Measures/CF per share3.csv')
CF3 <- CF3[-1,]



names(CF3) <- c('stockid','Report Date','SALE/SHARE TTM','OPPROFT/SHARE','NETOPCF/SHARE','NETOPCF/SHARETTM','FCFF/SHARE','FCFF/SHARETTM','FCFFORIG/SHARE','FCFTTM/SHARE1')
CF3$`Report Date` <- ymd(CF3$`Report Date`)
###Transforming CF1 according to stockid , and report date accending order



####Splitting the current Report Date into Year column, month column, day column

datetxt <- CF3$`Report Date`
datetxt <- as.Date(datetxt)
df <- tibble(date = datetxt,
             year = as.numeric(format(datetxt, format = "%Y")),
             month = as.numeric(format(datetxt, format = "%m")),
             day = as.numeric(format(datetxt, format = "%d")))


CF0 <- CF3
CF3 <- mutate(CF3, Year = df$year)

CF3 <- mutate(CF3, Month = df$month)


CF3$`NETOPCF/SHARETTM` <- parse_double(CF3$`NETOPCF/SHARETTM`)
CFsd3 <- data.frame()
temp<-c()
for(i in 1:length(CF3$Year)){
  
  
  
  
  if(CF3$Month[i] %in% c(3,6,9)){ temp <- c(temp,CF3$`NETOPCF/SHARETTM`[i])
  
  
  }
  else if(CF3$Month[i] %in% 12)
    
  { 
    temp <- c(temp,CF3$`NETOPCF/SHARETTM`[i])
    
    sdd <- sd(c(temp,na.rm=T),na.rm=T)
    CFsd3 <- rbind(CFsd3,c(sdd,i,as.numeric(CF3$stockid)[i],CF3$`Report Date`[i]))
    
    temp <-c()
    
    
  }
  
  
  print(i)
  
}


library('zoo')
CFsd3$X13148 <- as.Date(CFsd3$X13148)


names(CFsd3) <- c('StandDeviation','i Counts','stockid','Report Date')

CFsd3$stockid <- Addzero(CFsd3$stockid)

CFsd3 <- tibble(CFsd3$stockid,CFsd3$`Report Date`,CFsd3$`i Counts`,CFsd3$StandDeviation)


names(CFsd3) <- c('stockid','Report Date','i Counts','StandardDeviation')










######Double checking:


sd(c(CF3$`NETOPCF/SHARETTM`[801:804],na.rm=T),na.rm=T)


#Should be equal to :

CFsd3[CFsd3$X1 == 804,]







###adding zero
i = 1


while(nchar(CF3$stockid[i]) == 1){
  
  
  CF3$stockid[i] <-  paste0('00000',CF3$stockid[i])
  i <- i+1
  
  
  
  
}

while(nchar(CF3$stockid[i]) == 2) {
  
  
  CF3$stockid[i] <-  paste0('0000',CF3$stockid[i])
  i <- i+1
  
  print(i)
}


while(nchar(CF3$stockid[i]) == 3) {
  
  
  CF3$stockid[i] <-  paste0('000',CF3$stockid[i])
  i <- i+1
  
  print(i)
  
}


while(nchar(CF3$stockid[i]) == 4) {
  
  
  CF3$stockid[i] <-  paste0('00',CF3$stockid[i])
  i <- i+1
  
  print(i)
  
}
beep()



########################
CF3 <- arrange(CF3,stockid,Year,Month)
##########Remove Duplicated dates on same stockid


for(i in 1:length(CF3$Month)){
  
  if(!is.na(CF3$Month[i])){
    
    if(CF3$stockid[i] == CF3$stockid[i+1] & CF3$Year[i] == CF3$Year[i+1] ){
      
      if(CF3$Month[i] == CF3$Month[i+1]){
        
        CF3 <- CF3[-(i),]
        
      }
      
    }
    
  }
  
  print(i)
  
  
}

beep()




######bind_rows Binding the rows of the three CFs.



Cashflow_Volatility <- bind_rows(CFsd1,CFsd2,CFsd3) 




##################Trying to calculate the differenced CF volatility.

gb <- group_by(MAIN1,stockid,`Report Date`)

gb <- summarize(gb,difference = diff(StandardDeviation))

ChangeInCFvolatility <- matrix(nrow = length(gb$difference)+1)

ChangeInCFvolatility[1:length(gb$difference)] <- gb$difference



#####Doing the samething for Tobins Q ###这是用来算TQ 的变动的。


gb <- group_by(MAIN1,stockid,`Report Date`)

gb <- summarize(gb,differenceTQ = diff(TQ))

ChangeInTQ <- matrix(nrow = length(gb$differenceTQ)+1)

ChangeInTQ[1:length(gb$differenceTQ)] <- gb$differenceTQ



gb <- group_by(main_with_totalasset,stockid,`Report Date`)

gb<- summarize(gb,differenceTQA = diff(TQA))



#######################main_with_totalasset 是 rearranged 然后left_joint totalassets1.
####然后又left joint 了 JUNANTQ ---君安数据库的TQ。

CT <- diff(main_with_totalasset$TQA)
CTTQ <- matrix(nrow = length(CT)+1)

CTTQ[1:length(CT)] <- CT

main_with_totalasset <- mutate(main_with_totalasset,ChangeInTQA = CTTQ)


for(i in 1:length(main_with_totalasset$ChangeInCFVolatility)){
  
  if(main_with_totalasset$stockid[i+1] != main_with_totalasset$stockid[i]){
    
    main_with_totalasset$ChangeInTQA[i] <- NA
    
    
  }
  
  print(i)
  
  
}
beep()
















####07/06/2017 Running main_with_totalasset regression:

###if dependent is ChangeIn TQ , or Change in TQA , both result is consistent , Derivative usage have POSITIVE impact on change in TQ.
summary(plm(main_with_totalasset$ChangeInTQA ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + log(main_with_totalasset$EV)+ main_with_totalasset$`Dividend Yield`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA , data = main_with_totalasset
            ,index =c("stockid","Report Date"),model='within'))

####with change in CF volatility , also is consistent:
summary(plm(main_with_totalasset$ChangeInCFVolatility ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + log(main_with_totalasset$EV) + main_with_totalasset$`Dividend Yield`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA , data = main_with_totalasset
            ,index =c("stockid","Report Date"),model='between'))






##trying beta   ###doesnt seem to work..

summary(plm(log(main_with_totalasset$stockreturnvolatility) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + log(main_with_totalasset$EV)+ main_with_totalasset$`Dividend Yield`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA , data = main_with_totalasset
            ,index =c("stockid","Report Date"),model='within'))



summary(plm(log(Pooled_main_with_totalasset$StandardDeviation) ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
              Pooled_main_with_totalasset$`D/A` + log(Pooled_main_with_totalasset$EV)+ Pooled_main_with_totalasset$`Dividend Yield`+ Pooled_main_with_totalasset$OPrevenuegrowth
            + Pooled_main_with_totalasset$ROA  , data = Pooled_main_with_totalasset
            ,model='pool'))




summary(plm(main_with_totalasset$`CR NO DIV` ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + log(main_with_totalasset$EV)+ main_with_totalasset$`Dividend Yield`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
            ,model='within'))


summary(plm(Pooled_main_with_totalasset$`Cumulative No div` ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
              Pooled_main_with_totalasset$`D/A` + log(Pooled_main_with_totalasset$EV)+ Pooled_main_with_totalasset$`Dividend Yield`+ Pooled_main_with_totalasset$OPrevenuegrowth
            + Pooled_main_with_totalasset$ROA , data = Pooled_main_with_totalasset
            ,index =c("stockid","Report Date"),model='within',effect='time'))






#### Trying Year by Year###########


summary(plm(Pool_main_2005$`Cumulative No div` ~ Pool_main_2005$`Derivative Usage Dummy` + Pool_main_2005$oversea+
              Pool_main_2005$`D/A` + log(Pool_main_2005$EV)+ Pool_main_2005$`Div/Share`+ Pool_main_2005$OPrevenuegrowth
            + Pool_main_2005$ROA , data = Pool_main_2005
            ,model='pool'))

summary(plm(Pool_main_2006$TQA~ Pool_main_2006$`Derivative Usage Dummy` + Pool_main_2006$oversea+
              Pool_main_2006$`D/A` + Pool_main_2006$`Total Asset`+ Pool_main_2006$`Div/Share`+ Pool_main_2006$OPrevenuegrowth
            + Pool_main_2006$ROA , data = Pool_main_2006
            ,index =c("stockid"),model='pool'))


summary(plm(Pool_main_2007$TQ ~ Pool_main_2007$`Derivative Usage Dummy` + Pool_main_2007$oversea+
              Pool_main_2007$`D/A` + Pool_main_2007$`Total Asset` + Pool_main_2007$`Div/Share`+ Pool_main_2007$OPrevenuegrowth
            + Pool_main_2007$ROA + Pool_main_2007$`INDUSTRY CODE`, data = Pool_main_2007
            ,index =c("stockid"),model='pool'))



summary(plm(Pool_main_2008$TQA ~ Pool_main_2008$`Derivative Usage Dummy` + Pool_main_2008$oversea+
              Pool_main_2008$`D/A` + Pool_main_2008$`Total Asset`+ Pool_main_2008$`Div/Share`+ Pool_main_2008$OPrevenuegrowth
            + Pool_main_2008$ROA + Pool_main_2008$`INDUSTRY CODE`, data = Pool_main_2008
            ,index =c("stockid"),model='pool'))



summary(plm(Pool_main_2009$TQA ~ Pool_main_2009$`Derivative Usage Dummy` + Pool_main_2009$oversea+
              Pool_main_2009$`D/A` + Pool_main_2009$`Total Asset`+ Pool_main_2009$`Div/Share`+ Pool_main_2009$OPrevenuegrowth
            + Pool_main_2009$ROA + Pool_main_2009$`INDUSTRY CODE`, data = Pool_main_2009
            ,index =c("stockid"),model='pool'))




summary(lm(Pool_main_2009$`CR NO DIV` ~ Pool_main_2009$`Derivative Usage Dummy` + Pool_main_2009$oversea+
             Pool_main_2009$`D/A` + log(Pool_main_2009$EV)+ Pool_main_2009$`Div/Share`+ Pool_main_2009$OPrevenuegrowth
           + Pool_main_2009$ROA , data = Pool_main_2009
))


summary(lm(Pool_main_2010$TQA ~ Pool_main_2010$`Derivative Usage Dummy` + Pool_main_2010$oversea+
             Pool_main_2010$`D/A` + Pool_main_2010$`Total Asset`+ Pool_main_2010$`Div/Share`+ Pool_main_2010$OPrevenuegrowth
           + Pool_main_2010$ROA , data = Pool_main_2010
))

summary(lm(log(Pool_main_2011$EV) ~ Pool_main_2011$`Derivative Usage Dummy` + Pool_main_2011$oversea+
             Pool_main_2011$`D/A` + Pool_main_2011$`Total Asset`+ Pool_main_2011$`Div/Share`+ Pool_main_2011$OPrevenuegrowth
           + Pool_main_2011$ROA , data = Pool_main_2011
))

summary(lm(log(Pool_main_2012$EV) ~ Pool_main_2012$`Derivative Usage Dummy` + Pool_main_2012$oversea+
             Pool_main_2012$`D/A` + Pool_main_2012$`Total Asset`+ Pool_main_2012$`Div/Share`+ Pool_main_2012$OPrevenuegrowth
           + Pool_main_2012$ROA  + Pool_main_2012$`INDUSTRY CODE`, data = Pool_main_2012
))



summary(lm(log(Pool_main_2013$EV) ~ Pool_main_2013$`Derivative Usage Dummy` + Pool_main_2013$oversea+
             Pool_main_2013$`D/A` + Pool_main_2013$`Total Asset`+ Pool_main_2013$`Div/Share`+ Pool_main_2013$OPrevenuegrowth
           + Pool_main_2013$ROA  + Pool_main_2013$`INDUSTRY CODE`, data = Pool_main_2013
))



summary(lm(log(Pool_main_2014$EV) ~ Pool_main_2014$`Derivative Usage Dummy` + Pool_main_2014$oversea+
             Pool_main_2014$`D/A` + Pool_main_2014$`Total Asset`+ Pool_main_2014$`Div/Share`+ Pool_main_2014$OPrevenuegrowth
           + Pool_main_2014$ROA  + Pool_main_2014$`INDUSTRY CODE`, data = Pool_main_2014
))




summary(lm(log(Pool_main_2015$EV) ~ Pool_main_2015$`Derivative Usage Dummy` + Pool_main_2015$oversea+
             Pool_main_2015$`D/A` + Pool_main_2015$`Total Asset`+ Pool_main_2015$`Div/Share`+ Pool_main_2015$OPrevenuegrowth
           + Pool_main_2015$ROA  , data = Pool_main_2015
))
###Above all doesnt seem to work..

#########FIRM SIZES:



Large <- filter(main_with_totalasset,EV >= 3058234578)

Small <- filter(main_with_totalasset,EV < 3058234578)

Pooled_Large <- filter(Pooled_main_with_totalasset,EV >= 3058234578)

Pooled_Small <- filter(Pooled_main_with_totalasset,EV < 3058234578)
##################Trying it with size



summary(plm(Pooled_Large$TQA~ Pooled_Large$`Derivative Usage Dummy` + Pooled_Large$oversea+
              Pooled_Large$`D/A` + log(Pooled_Large$EV)+ Pooled_Large$`Dividend Yield`+ Pooled_Large$OPrevenuegrowth
            + Pooled_Large$ROA , data = Pooled_Large
            ,index =c("stockid","Report Date"),model='pool'))





summary(plm(Pooled_Small$TQA~ Pooled_Small$`Derivative Usage Dummy` + Pooled_Small$oversea+
              Pooled_Small$`D/A` + log(Pooled_Small$EV)+ Pooled_Small$`Dividend Yield`+ Pooled_Small$OPrevenuegrowth
            + Pooled_Small$ROA , data = Pooled_Small
            ,index =c("stockid","Report Date"),model='pool'))




#######panel data


###Size separate tests, using fixed effect are still consistent with 
##change measures, however, other measures not consistent.
summary(plm(Small$ChangeInCFVolatility~ Small$`Derivative Usage Dummy` + Small$oversea+
              Small$`D/A` + log(Small$EV)+ Small$`Dividend Yield`+ Small$OPrevenuegrowth
            + Small$ROA , data = Small
            ,index =c("stockid","Report Date"),model="between"))

#######also consisitnt still, using Change measures.


summary(plm(Large$ChangeInCFVolatility~ Large$`Derivative Usage Dummy` + Large$oversea+
              Large$`D/A` + log(Large$EV)+ Large$`Dividend Yield`+ Large$OPrevenuegrowth
            + Large$ROA + Large$`INDUSTRY CODE` , data = Large
            ,index =c("stockid","Report Date"),model="between"))


################ Regression using change in stockreturn measures

###Good!!consistent! Derivative usage is positive with the change in stock return using fixed effect measure
summary(plm(main_with_totalasset$Changeinstockreturn~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + log(main_with_totalasset$EV)+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA  , data = main_with_totalasset
            ,index =c("stockid","Report Date"),model="random"))



summary(plm(main_with_totalasset$ChangeInCFVolatility~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + log(main_with_totalasset$EV)+ main_with_totalasset$`Dividend Yield`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA  , data = main_with_totalasset
            ,index =c("stockid","Report Date"),model="between"))






##############stockreturn volatility measures are consistent!!! adding or removing industry code both decreases volatility! for panel data
summary(plm(log(main_with_totalasset$stockreturnvolatility) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + log(main_with_totalasset$EV)+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA , data = main_with_totalasset
            ,index =c("stockid","Report Date"),model='within'))



summary(plm(log(Pooled_main_with_totalasset$stockreturnvolatility) ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
              Pooled_main_with_totalasset$`D/A` + log(Pooled_main_with_totalasset$EV)+ Pooled_main_with_totalasset$`Dividend Yield`+ Pooled_main_with_totalasset$OPrevenuegrowth
            + Pooled_main_with_totalasset$ROA , data = Pooled_main_with_totalasset
            ,index =c("stockid","Report Date"),model='pool'))






######Testing change measures of Stock returns


changeinstock <- diff(main_with_totalasset$`Cumulative No div`)
Changestock <- matrix(nrow = length(changeinstock)+1)

Changestock[1:length(changeinstock)] <- changeinstock

main_with_totalasset <- mutate(main_with_totalasset,Changeinstockreturn = Changestock)


for(i in 1:length(main_with_totalasset$ChangeInCFVolatility)){
  
  if(main_with_totalasset$stockid[i+1] != main_with_totalasset$stockid[i]){
    
    main_with_totalasset$Changeinstockreturn[i] <- NA
    
    
  }
  
  print(i)
  
  
}
beep()








#######07.06.2017 mannually adding derivative info on new companies, using main_with_totalasset 
####to secure current working, before doing anything :


main_with_totalasset_B4 <- main_with_totalasset





#############Deleting "change" data when stock id switches.
##########This did not produce any significance results.



for(i in 1:length(TETSTS$ChangeInCFVolatility)){
  
  if(TETSTS$stockid[i+1] != TETSTS$stockid[i]){
    
    TETSTS$ChangeInCFVolatility[i] <- TETSTS$ChangeInTQ[i] <- NA
    
    
  }
  
  print(i)
  
  
}
beep()







########


#############Deleting "change" data when stock id switches.

for(i in 1:length(TT$ChangeInCFVolatility)){
  
  if(TT$stockid[i] != TT$stockid[i+1]){
    
    TT <- TT[-i,] 
    
    
  }
  
  print(i)
  
  
}
beep()





###making a notifying sound when finish running the sript!!
beep <- function(n = 3){
  for(i in seq(n)){
    system("rundll32 user32.dll,MessageBeep -1")
    Sys.sleep(.5)
  }
}







######09/06/2017 Regression  with all three research interest

###First Regression Interest:
##Derivvative usage and firm value:

##Using TQ ,TQA , changeInTQ,changeInTQA: 

###After correcting dividend, it is consistent, that derivative usage will increase TQ

summary(plm(log(main_with_totalasset$TQ) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA , data = main_with_totalasset
            ,index =c("stockid","Report Date"),model='within'))



summary(plm(log10(main_with_totalasset$TQA) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea +
              main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE` , data = main_with_totalasset
            ,index =c("stockid","Report Date"),model='within'))



#####This is after deleting extreme tq values..

summary(plm(log10(try1$StandardDeviation) ~ try1$`Derivative Usage Dummy` + try1$oversea +
              try1$`D/A` + try1$`Total Asset`+ try1$`Div/Share`+ try1$OPrevenuegrowth
            + try1$ROA + try1$`INDUSTRY CODE` , data = try1
            ,index =c("stockid","Report Date"),model='pool',effect='time'))








#####Using Pooled measures:
####either Significant but Negatively to Tobins Q or insignificant..

summary(plm(log(Pooled_main_with_totalasset$TQ) ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
              Pooled_main_with_totalasset$`D/A` + Pooled_main_with_totalasset$`Total Asset`+ Pooled_main_with_totalasset$`Div/Share`+ Pooled_main_with_totalasset$OPrevenuegrowth
            + Pooled_main_with_totalasset$ROA , data = Pooled_main_with_totalasset
            ,index =c("stockid","Report Date"),model='pool'))


summary(lm(log(Pooled_main_with_totalasset$TQA) ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
             Pooled_main_with_totalasset$`D/A` + Pooled_main_with_totalasset$`Total Asset`+ Pooled_main_with_totalasset$`Div/Share`+ Pooled_main_with_totalasset$OPrevenuegrowth
           + Pooled_main_with_totalasset$ROA , data = Pooled_main_with_totalasset
))






summary(plm(log(Pooled_main_with_totalasset$TQ) ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
              Pooled_main_with_totalasset$`D/A` + Pooled_main_with_totalasset$`Total Asset`+ Pooled_main_with_totalasset$`Div/Share`+ Pooled_main_with_totalasset$OPrevenuegrowth
            + Pooled_main_with_totalasset$ROA + Pooled_main_with_totalasset$`INDUSTRY CODE` , data = Pooled_main_with_totalasset
            ,index =c("stockid","Report Date"),model='pool'))


summary(plm(Pooled_main_with_totalasset$ChangeInTQA ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
              Pooled_main_with_totalasset$`D/A` + Pooled_main_with_totalasset$`Total Asset`+ Pooled_main_with_totalasset$`Div/Share`+ Pooled_main_with_totalasset$OPrevenuegrowth
            + Pooled_main_with_totalasset$ROA + Pooled_main_with_totalasset$`INDUSTRY CODE` , data = Pooled_main_with_totalasset
            ,index =c("stockid","Report Date"),model='pool',effect='time'))




summary(lm(log(Pooled_main_with_totalasset$EV) ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
             Pooled_main_with_totalasset$`D/A` + Pooled_main_with_totalasset$`Total Asset`+ Pooled_main_with_totalasset$`Div/Share`+ Pooled_main_with_totalasset$OPrevenuegrowth
           + Pooled_main_with_totalasset$ROA , data = Pooled_main_with_totalasset
))


####Separating into small and large sizes with both Panel and Pooled data structure:


###Panel

###Consistent
summary(plm(log(Large$EV) ~ Large$`Derivative Usage Dummy` + Large$oversea+
              Large$`D/A` + Large$`Total Asset`+ Large$`Div/Share`+ Large$OPrevenuegrowth
            + Large$ROA  + Large$`INDUSTRY CODE`, data = Large
            ,index =c("stockid","Report Date"),model='within'))


summary(plm(Large$TQA ~ Large$`Derivative Usage Dummy` + Large$oversea+
              Large$`D/A` + Large$`Total Asset`+ Large$`Dividend Yield`+ Large$OPrevenuegrowth
            + Large$ROA  + Large$`INDUSTRY CODE`, data = Large
            ,index =c("stockid","Report Date"),model='within'))


summary(plm(log(Small$EV) ~ Small$`Derivative Usage Dummy` + Small$oversea+
              Small$`D/A` + Small$`Total Asset`+ Small$`Div/Share`+ Small$OPrevenuegrowth
            + Small$ROA  + Small$`INDUSTRY CODE`, data = Small
            ,index =c("stockid","Report Date"),model='within'))



##Pooled:

###Increases EV

summary(plm(log(Pooled_Large$EV) ~ Pooled_Large$`Derivative Usage Dummy` + Pooled_Large$oversea+
              Pooled_Large$`D/A` + Pooled_Large$`Total Asset`+ Pooled_Large$`Div/Share`+ Pooled_Large$OPrevenuegrowth
            + Pooled_Large$ROA  + Pooled_Large$`INDUSTRY CODE`, data = Pooled_Large
            ,index =c("stockid","Report Date"),model='pool'))
###Increases EV


summary(plm(log(Pooled_Small$EV) ~ Pooled_Small$`Derivative Usage Dummy` + Pooled_Small$oversea+
              Pooled_Small$`D/A` + Pooled_Small$`Total Asset`+ Pooled_Small$`Div/Share`+ Pooled_Small$OPrevenuegrowth
            + Pooled_Small$ROA  + Pooled_Small$`INDUSTRY CODE`, data = Pooled_Small
            ,index =c("stockid","Report Date"),model='pool'))



###Change TQ measures:
##Not significant with corrected dividend
summary(plm(main_with_totalasset$ChangeInTQA ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA , data = main_with_totalasset
            ,index =c("stockid","Report Date"),model='random'))








###not good with corrected dividend
summary(plm(log(main_with_totalasset$ChangeInTQA) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
            ,index =c("stockid","Report Date"),model='within',effect='time'))

####Good , DERIVATIVE USAGE INCREASES Enterprise Value.Even with corrected dividend measure

summary(plm(log10(main_with_totalasset$EV) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA , data = main_with_totalasset
            ,index =c("stockid","Report Date"),model='within'))

####Pooled Data measure

###Not significant AND not consistent
summary(plm(Pooled_main_with_totalasset$ChangeInTQA ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
              Pooled_main_with_totalasset$`D/A` + Pooled_main_with_totalasset$`Total Asset`+ Pooled_main_with_totalasset$`Div/Share`+ Pooled_main_with_totalasset$OPrevenuegrowth
            + Pooled_main_with_totalasset$ROA , data = Pooled_main_with_totalasset
            ,index =c("stockid","Report Date"),model='pool'))




summary(lm(Pooled_main_with_totalasset$ChangeInTQA ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
             Pooled_main_with_totalasset$`D/A` + Pooled_main_with_totalasset$`Total Asset`+ Pooled_main_with_totalasset$`Div/Share` + Pooled_main_with_totalasset$OPrevenuegrowth
           + Pooled_main_with_totalasset$ROA , data = Pooled_main_with_totalasset
))


######SECOND RESEARCHH INTEREST DERIVATIVE usage and firm risk mitigation

####Two testing dependent variables , CF volatility and  Stock return volatility

####Works, Deerivative usage decreases CF volatility.
####doesnt work with corrected dividend..
summary(plm(log10(main_with_totalasset$StandardDeviation) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset` + main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA  , data = main_with_totalasset
            ,index =c("stockid","Report Date"),model='within'))

#####it worked under the not corrected(with lot of NA in dividend) data.
summary(plm(FinalMain$StandardDeviation ~ FinalMain$`Derivative Usage Dummy` + FinalMain$oversea+
              FinalMain$`D/A` + log(FinalMain$EV) + FinalMain$`Dividend Yield`+ FinalMain$OPrevenuegrowth
            + FinalMain$ROA + FinalMain$`INDUSTRY CODE`, data = FinalMain
            ,index =c("stockid","Report Date"),model='within'))



###work but weak

summary(plm(log10(main_with_totalasset$ChangeInCFVolatility) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
            ,index =c("stockid","Report Date"),model='within'))



summary(plm(log10(main_with_totalasset$ChangeInCFVolatility) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA , data = main_with_totalasset
            ,index =c("stockid","Report Date"),model='within'))






summary(plm(log10(main_with_totalasset$ChangeInCFVolatility) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
            ,index =c("stockid","Report Date"),model='random',effect='time'))



###Nothing...reverse significan

summary(plm(log(Pooled_main_with_totalasset$StandardDeviation) ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
              Pooled_main_with_totalasset$`D/A` + Pooled_main_with_totalasset$`Total Asset`+ Pooled_main_with_totalasset$`Div/Share`+ Pooled_main_with_totalasset$OPrevenuegrowth
            + Pooled_main_with_totalasset$ROA + Pooled_main_with_totalasset$`INDUSTRY CODE`, data = Pooled_main_with_totalasset
            ,index =c("stockid","Report Date"),model='pool'))



###Now use stock return volatility measures

####Good###Market measure, use EV
summary(plm(log10(main_with_totalasset$stockreturnvolatility) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + log(main_with_totalasset$EV)+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
            ,index =c("stockid","Report Date"),model='random',effect='time'))


summary(plm(log10(main_with_totalasset$stockreturnvolatility) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
            ,index =c("stockid","Report Date"),model='pool',effect='time'))



summary(plm(log(main_with_totalasset$stockreturnvolatility) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
            ,index =c("stockid","Report Date"),model='within',effect='time'))



summary(plm(log10(main_with_totalasset$stockreturnvolatility) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
            ,index =c("stockid","Report Date"),model='random',effect='time'))






###Good only with log.. and only with EV , but not Total Asset

summary(lm(log(Pooled_main_with_totalasset$stockreturnvolatility) ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
             Pooled_main_with_totalasset$`D/A` + log(Pooled_main_with_totalasset$EV)+ Pooled_main_with_totalasset$`Div/Share`+ Pooled_main_with_totalasset$OPrevenuegrowth
           + Pooled_main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = Pooled_main_with_totalasset
))


summary(plm(log(Pooled_main_with_totalasset$stockreturnvolatility) ~ Pooled_main_with_totalasset$`Derivative Usage Dummy` + Pooled_main_with_totalasset$oversea+
              Pooled_main_with_totalasset$`D/A` + Pooled_main_with_totalasset$`Total Asset`+ Pooled_main_with_totalasset$`Dividend Yield`+ Pooled_main_with_totalasset$OPrevenuegrowth
            + Pooled_main_with_totalasset$ROA , data = Pooled_main_with_totalasset
            ,index =c("stockid","Report Date"),model='pool'))

#########Third research interest Derivative Usage and Stock return
###Consistent:

summary(plm(main_with_totalasset$Changeinstockreturn~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + log(main_with_totalasset$EV)+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
            ,index =c("stockid","Report Date"),model="within", effect='time'))



summary(plm(main_with_totalasset$Changeinstockreturn~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + log(main_with_totalasset$EV)+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
            ,index =c("stockid","Report Date"),model="pool", effect='time'))


summary(plm(main_with_totalasset$Changeinstockreturn~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + log(main_with_totalasset$EV)+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
            ,index =c("stockid","Report Date"),model="random", effect='time'))





summary(plm(main_with_totalasset$Changeinstockreturn~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
            ,index =c("stockid","Report Date"),model="random", effect='time'))




#####Total asset not significant...

summary(plm(main_with_totalasset$Changeinstockreturn~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
            ,index =c("stockid","Report Date"),model="pool"))






###Significant but in the contrary direction..:

summary(plm(main_with_totalasset$`Cumulative return`~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + log(main_with_totalasset$EV)+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
            ,index =c("stockid","Report Date"),model="within",effect='time'))









#####Alpha 




summary(plm(main_with_totalasset$`Alpha MarketCap Weighted`~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
            ,index =c("stockid","Report Date"),model="random",effect='time'))




summary(plm(main_with_totalasset$`Alpha MarketCap Weighted`~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
            ,index =c("stockid","Report Date"),model="random",effect='time'))





















###Removing NA in the Oversea variable.
### FinalMain as back up 


###FinalMain is now backup for data before correction of Dividend/share ,updated with dividend share is stored in main_with_totalasset
####FinalMain_Pooled is Pooled_main_with_totalasset before dividend correction. 




FinalMain <- main_with_totalasset

for(i in 1:length(main_with_totalasset$stockid)){
  
  if(is.na(main_with_totalasset$oversea[i])){
    
    
    if((main_with_totalasset$stockid[i-1] == main_with_totalasset$stockid[i] )& (!is.na(main_with_totalasset$oversea[i-1]))){
      
      
      main_with_totalasset$oversea[i] <- main_with_totalasset$oversea[i-1]
      
      print(paste0('It is an oversea company',i))
      
    }
    
    else{main_with_totalasset$oversea[i] <- 0}
    
    
    
  }
  
  
  
  
}





##############################################Just a testing for TQ with mismatched time###### Dony worry about it , it is negative still.... Not consistent with research interest that derivative may inprove TQ

##############################Doesn't seem to have any relationship...
TQtplus1 <- matrix(nrow= length(main_with_totalasset$TQ)+1)





for(i in 1:length(main_with_totalasset$TQA)){
  
  if(main_with_totalasset$stockid[i] == main_with_totalasset$stockid[i+1]){
    
    TQtplus1[i] <- main_with_totalasset$TQ[i+1]
    
    
  }
  
  else {
    
    TQtplus1[i] <- NA
    
  }
  
  print(i)
  
  
}
#####
for(i in 1:length(TQtplus1)){
  
  if(is.na(TQtplus1[i])){
    
    
    try[i,3:ncol(try)] <- NA
    
    
  }
  print(i)
  
}





summary(plm(try$TQ ~ try$`Derivative Usage Dummy` + try$oversea+
              try$`D/A` + try$`Total Asset`+ try$`Dividend Yield`+ try$OPrevenuegrowth
            + try$ROA + try$`INDUSTRY CODE` , data = try
            ,index =c("stockid","Report Date"),model='pool'))




###similar but this time apply to stock return , i am trying to see, if derivative usage has anything to do with subsequent year's stock return..


###########They dont seem to have any relationships..


stockreturntplus1 <- matrix(nrow= length(main_with_totalasset$`Cumulative No div`)+1)


for(i in 1:length(main_with_totalasset$`Cumulative No div`)){
  
  if(main_with_totalasset$stockid[i] == main_with_totalasset$stockid[i+1]){
    
    stockreturntplus1[i] <- main_with_totalasset$`Cumulative No div`[i+1]
    
    
  }
  
  else {
    
    stockreturntplus1[i] <- NA
    
  }
  
  print(i)
  
  
}

####assigning NAs to cope(make in line) with the new measure

for(i in 1:length(stockreturntplus1)){
  
  if(is.na(stockreturntplus1[i])){
    
    
    try[i,3:ncol(try)] <- NA
    
    
  }
  print(i)
  
}

summary(plm(stockreturntplus1 ~ try$`Derivative Usage Dummy` + try$oversea+
              try$`D/A` + try$`Total Asset`+ try$`Dividend Yield`+ try$OPrevenuegrowth
            + try$ROA , data = try
            ,index =c("stockid","Report Date"),model='pool'))







####################Using above method to test for cash flow volatility


CFsdtplus1 <- matrix(nrow= length(main_with_totalasset$StandardDeviation)+1)


for(i in 1:length(main_with_totalasset$StandardDeviation)){
  
  if(main_with_totalasset$stockid[i] == main_with_totalasset$stockid[i+1]){
    
    CFsdtplus1[i] <- main_with_totalasset$StandardDeviation[i+1]
    
    
  }
  
  else {
    
    CFsdtplus1[i] <- NA
    
  }
  
  print(i)
  
  
}

####assigning NAs to cope(make in line) with the new measure

for(i in 1:length(CFsdtplus1)){
  
  if(is.na(CFsdtplus1[i])){
    
    
    try[i,3:ncol(try)] <- NA
    
    
  }
  print(i)
  
}

summary(plm(log(CFsdtplus1) ~ try$`Derivative Usage Dummy` + try$oversea+
              try$`D/A` + try$`Total Asset` + try$`Div/Share`+ try$OPrevenuegrowth
            + try$ROA , data = try
            ,index =c("stockid","Report Date"),model='within'))

























########################################################

###correcting dividend data 


Divcorrect <- read_csv('分红.csv')


names(Divcorrect) <- c('stockid','Report Date','DivPs','PayoutRatio')

for(i in 1:length(main_with_totalasset$stockid)){
  
  if(main_with_totalasset$stockid[i] %in% main_with_totalasset$stockid[which(is.na(main_with_totalasset$`Div/Share`) & main_with_totalasset$`Derivative Usage Dummy` ==1)])
  {
    
    if(is.na(main_with_totalasset$`Div/Share`[i]) & is.na(Divcorrect[which(Divcorrect$stockid == main_with_totalasset$stockid[i] & Divcorrect$`Report Date` == main_with_totalasset$ `Report Date`[i]),][3]))
      
    { main_with_totalasset$`Div/Share`[i] <- 0
    
    
    }
    
    else if(is.na(main_with_totalasset$`Div/Share`[i]) & !is.na(Divcorrect[which(Divcorrect$stockid == main_with_totalasset$stockid[i] & Divcorrect$`Report Date` == main_with_totalasset$ `Report Date`[i]),][3]))
      
    {
      
      
      main_with_totalasset$`Div/Share`[i] <- as.numeric(Divcorrect[which(Divcorrect$stockid == main_with_totalasset$stockid[i] & Divcorrect$`Report Date` == main_with_totalasset$ `Report Date`[i]),][3])
      
      
    }
    
    
    
    
    
  }
  
  print(i)
  
}



for(i in 15903:length(main_with_totalasset$stockid)){
  
  if(any(Divcorrect$stockid %in% main_with_totalasset$stockid[i])){
    
    if(is.na(main_with_totalasset$`Div/Share`[i]) & is.na(Divcorrect[which(Divcorrect$stockid == main_with_totalasset$stockid[i] & Divcorrect$`Report Date` == main_with_totalasset$ `Report Date`[i]),][3]))
      
    { main_with_totalasset$`Div/Share`[i] <- 0
    
    
    }
    
    else if(is.na(main_with_totalasset$`Div/Share`[i]) & !is.na(Divcorrect[which(Divcorrect$stockid == main_with_totalasset$stockid[i] & Divcorrect$`Report Date` == main_with_totalasset$ `Report Date`[i]),][3]))
      
    {
      
      
      main_with_totalasset$`Div/Share`[i] <- as.numeric(Divcorrect[which(Divcorrect$stockid == main_with_totalasset$stockid[i] & Divcorrect$`Report Date` == main_with_totalasset$ `Report Date`[i]),][3])
      print(paste0('something new'))
      
    }
    
    
    
    
  }
  
  
  print(i)
  
}






##############Trying the robustness test of allynois

###########THIS LOGIC  IS WRONG....


for(i in 1:length(main_with_totalasset$stockid)){
  
  
  if(main_with_totalasset$stockid[i] == main_with_totalasset$stockid[i+1]){
    
    
    if( main_with_totalasset$`Derivative Usage Dummy`[i] ==0 & main_with_totalasset$`Derivative Usage Dummy`[i+1] == 1){
      
      NH <- rbind(NH, data.frame(stockid =main_with_totalasset$stockid[i], `Report Date` =main_with_totalasset$`Report Date`[i] ))
      
      if(main_with_totalasset$stockid[i] %in% HN){
        
        HN[-(which(HN == main_with_totalasset$stockid[i])),]
        print('deleted from HN')
      }
      
      print(main_with_totalasset$stockid[i])
      
      
      
    }
    
    if( main_with_totalasset$`Derivative Usage Dummy`[i] ==1 & main_with_totalasset$`Derivative Usage Dummy`[i+1] == 0){
      
      HN <- rbind(HN, data.frame(stockid =  main_with_totalasset$stockid[i], `Report Date` = main_with_totalasset$`Report Date`[i]))
      if(main_with_totalasset$stockid[i] %in% NH){
        
        NH[-which(NH == main_with_totalasset$stockid[i]),]
        print('deleted from NH')
      }
      
      
      
      
    }
    
    
    if( main_with_totalasset$`Derivative Usage Dummy`[i] ==0 & main_with_totalasset$`Derivative Usage Dummy`[i+1] == 0){
      
      NN <- rbind(NN, data.frame(stockid =  main_with_totalasset$stockid[i], `Report Date` = main_with_totalasset$`Report Date`[i]))
      
    }
    
    
  }
  
  
}






for(i in 1:length(try$Robust)){
  
  if( try$stockid[i] %in% HN$stockid){
    
    if(try$`Report Date`[i] == HN$Report.Date[which(try$stockid[i] == HN[,1])]){
      
      try$Robust[i] <- 2
    }
  }
  
  
  if(try$stockid[i] %in% NH$stockid ) {
    
    if(try$`Report Date`[i] == NH$Report.Date[which(try$stockid[i] == NH[,1])]){
      
      try$Robust[i] <- 3
    }
  }
  
  if(try$stockid[i] %in% NN$stockid){
    
    if(try$`Report Date`[i] == NN$Report.Date[which(try$stockid[i] == NN[,1])]){
      
      try$Robust[i] <- 1
    }
  }
  
  print(i)
  
  
}



############TRYING SOMETHING NEW: Robustness Tests of Allayannis and Weston


for(i in unique(main_with_totalasset$stockid)){
  
  for(d in 1:length(main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)])){
    
    if(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d] == 1
       & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d] == "2008-12-31"){
      
      
      if(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d+4] == 1
         & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][length(main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)])] == "2012-12-31"){
        
        
        
        HH <- rbind(HH, data.frame(stockid = main_with_totalasset$stockid[which(main_with_totalasset$stockid == i)][1], ReportDate = main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d]))
        
        
        
      }
      
      else if(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d+4] == 0
              & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][length(main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)])] == "2012-12-31"){
        
        
        HN <- rbind(HN, data.frame(stockid =main_with_totalasset$stockid[which(main_with_totalasset$stockid == i)][1], ReportDate = main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d]))
        
        
        
      }
      
      
      
      
      
    }
    if(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d] == 0
       & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d] == "2008-12-31"){
      
      if(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d+4] == 1
         & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][length(main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)])] == "2012-12-31"){
        
        
        NH <- rbind(NH, data.frame(stockid =main_with_totalasset$stockid[which(main_with_totalasset$stockid == i)][1], ReportDate = main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d]))
        
      }
      
      else if(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d+4] == 0
              & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][length(main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)])] == "2012-12-31"){
        
        
        NN <- rbind(NN, data.frame(stockid =main_with_totalasset$stockid[which(main_with_totalasset$stockid == i)][1], ReportDate = main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d]))
        
        
        
      }
      
      
      
    }
    
  }
  print(i)
}

####same as above code but with end dates not equal to 2015:
for(i in unique(main_with_totalasset$stockid)){
  
  for(d in 1:length(main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)])){
    
    if(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d] == 1
       & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d] == "2012-12-31"){
      
      
      if((!is.na(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid==i)][d+2])) & main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d+2] == 1
         & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d+2] == "2014-12-31"){
        
        
        
        HH <- rbind(HH, data.frame(stockid = main_with_totalasset$stockid[which(main_with_totalasset$stockid == i)][1], ReportDate = main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d]))
        
        
        
      }
      
      else if((!is.na(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid==i)][d+2])) & main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d+2] == 0
              & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d+2] == "2014-12-31"){
        
        
        HN <- rbind(HN, data.frame(stockid =main_with_totalasset$stockid[which(main_with_totalasset$stockid == i)][1], ReportDate = main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d]))
        
        
        
      }
      
      
      
      
      
    }
    if(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d] == 0
       & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d] == "2012-12-31"){
      
      if((!is.na(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid==i)][d+2]))& main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d+2] == 1
         & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d+2] == "2014-12-31"){
        
        
        NH <- rbind(NH, data.frame(stockid =main_with_totalasset$stockid[which(main_with_totalasset$stockid == i)][1], ReportDate = main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d]))
        
      }
      
      else if((!is.na(main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid==i)][d+2])) & main_with_totalasset$`Derivative Usage Dummy`[which(main_with_totalasset$stockid == i)][d+2] == 0
              & main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d+2] == "2014-12-31"){
        
        
        NN <- rbind(NN, data.frame(stockid =main_with_totalasset$stockid[which(main_with_totalasset$stockid == i)][1], ReportDate = main_with_totalasset$`Report Date`[which(main_with_totalasset$stockid == i)][d]))
        
        
        
      }
      
      
      
    }
    
  }
  print(i)
}

#############Above is modified version.. 




which(unique(main_with_totalasset$stockid) == i)



HH <- HN <- NH <- NN <- data.frame()
try201214 <- filter(try,`Report Date` == "2012-12-31")
HH$ReportDate <- ymd(HH$ReportDate)
NH$ReportDate <- ymd(NH$ReportDate)

HN$ReportDate <- ymd(HN$ReportDate)

NN$ReportDate <- ymd(NN$ReportDate)

NN$stockid <- as.character(NN$stockid)
HN$stockid <- as.character(HN$stockid)
HH$stockid <- as.character(HH$stockid)

NH$stockid <- as.character(NH$stockid)
try201214$Robust <- parse_factor(try201214$Robust,levels = c(0,1,2,3))
######assigning the above to relevant dates

for(i in 1:length(try201214$Robust)){
  
  if( try201214$stockid[i] %in% HN$stockid){
    
    
    
    try201214$Robust[i] <- 2
  }
  
  
  
  if(try201214$stockid[i] %in% NH$stockid ) {
    
    
    try201214$Robust[i] <- 3
  }
  
  
  if(try201214$stockid[i] %in% NN$stockid){
    
    
    try201214$Robust[i] <- 1
  }
  
  
  
  print(i)
  
  
}




###################Robustness tests

summary(lm(log(try201012$TQA)~ try201012$Robust + try201012$oversea+
             try201012$`D/A` + try201012$`Total Asset`+ try201012$`Div/Share`+ try201012$OPrevenuegrowth
           + try201012$ROA + try201012$`INDUSTRY CODE`, data = try201012
))




summary(lm(log(try201012$EV)~ try201012$Robust + try201012$oversea+
             try201012$`D/A` + try201012$`Total Asset`+ try201012$`Div/Share`+ try201012$OPrevenuegrowth
           + try201012$ROA + try201012$`INDUSTRY CODE`, data = try201012
))


summary(lm(log(try201214$EV)~ try201214$Robust + try201214$oversea+
             try201214$`D/A` + try201214$`Total Asset`+ try201214$`Div/Share`+ try201214$OPrevenuegrowth
           + try201214$ROA + try201214$`INDUSTRY CODE`, data = try201214
))


summary(lm(log(try201214$TQA)~ try201214$Robust + try201214$oversea+
             try201214$`D/A` + try201214$`Total Asset`+ try201214$`Div/Share`+ try201214$OPrevenuegrowth
           + try201214$ROA + try201214$`INDUSTRY CODE`, data = try201214
))





###USE:

summary(lm(log(try200911$TQA)~ try200911$Robust + try200911$oversea+
             try200911$`D/A` + try200911$`Total Asset`+ try200911$`Div/Share`+ try200911$OPrevenuegrowth
           + try200911$ROA + try200911$`INDUSTRY CODE`, data = try200911
))


summary(lm(log(try200911$EV)~ try200911$Robust + try200911$oversea+
             try200911$`D/A` + try200911$`Total Asset`+ try200911$`Div/Share`+ try200911$OPrevenuegrowth
           + try200911$ROA + try200911$`INDUSTRY CODE`, data = try200911
))






###Consistent with Allyanis and weston
summary(lm(log(try2009$TQA)~ try2009$Robust + try2009$oversea+
             try2009$`D/A` + log(try2009$EV)+ try2009$`Div/Share`+ try2009$OPrevenuegrowth
           + try2009$ROA + try2009$`INDUSTRY CODE`, data = try2009
))


summary(lm(log(try2009$EV)~ try2009$Robust + try2009$oversea+
             try2009$`D/A` + try2009$`Total Asset`+ try2009$`Div/Share`+ try2009$OPrevenuegrowth
           + try2009$ROA + try2009$`INDUSTRY CODE`, data = try2009
))


summary(lm(log(try2009$TQA)~ try2009$Robust + try2009$oversea+
             try2009$`D/A` + try2009$`Total Asset`+ try2009$`Div/Share`+ try2009$OPrevenuegrowth
           + try2009$ROA + try2009$`INDUSTRY CODE`, data = try2009
))



###Consistent
summary(lm(log(try2013$TQA)~ try2013$Robust + try2013$oversea+
             try2013$`D/A` + try2013$`Total Asset`+ try2013$`Div/Share`+ try2013$OPrevenuegrowth
           + try2013$ROA + try2013$`INDUSTRY CODE`, data = try2013
))

summary(lm(log(try2013$EV)~ try2013$Robust + try2013$oversea+
             try2013$`D/A` + try2013$`Total Asset`+ try2013$`Div/Share`+ try2013$OPrevenuegrowth
           + try2013$ROA + try2013$`INDUSTRY CODE`, data = try2013
))


###Consistent

summary(lm(log(try2013$TQA)~ try2013$Robust + try2013$oversea+
             try2013$`D/A` + log(try2013$EV)+ try2013$`Div/Share`+ try2013$OPrevenuegrowth
           + try2013$ROA + try2013$`INDUSTRY CODE`, data = try2013
))


###USE


summary(lm(log(try2012$TQA)~ try2012$Robust + try2012$oversea+
             try2012$`D/A` + try2012$`Total Asset`+ try2012$`Div/Share`+ try2012$OPrevenuegrowth
           + try2012$ROA + try2012$`INDUSTRY CODE`, data = try2012
))



summary(lm(log(try2012$EV)~ try2012$Robust + try2012$oversea+
             try2012$`D/A` + try2012$`Total Asset`+ try2012$`Div/Share`+ try2012$OPrevenuegrowth
           + try2012$ROA + try2012$`INDUSTRY CODE`, data = try2012
))







summary(lm(log(try2011$TQB)~ try2011$Robust + try2011$oversea+
             try2011$`D/A` + try2011$`Total Asset`+ try2011$`Div/Share`+ try2011$OPrevenuegrowth
           + try2011$ROA + try2011$`INDUSTRY CODE`, data = try2011
))



###NOT

summary(lm(log(try2010$TQA)~ try2010$Robust + try2010$oversea+
             try2010$`D/A` + try2010$`Total Asset`+ try2010$`Div/Share`+ try2010$OPrevenuegrowth
           + try2010$ROA + try2010$`INDUSTRY CODE`, data = try2010
))




###not SIGNIFICANT BUT right direction.


summary(lm(log(try200812$TQA)~ try200812$Robust + try200812$oversea+
             try200812$`D/A` + try200812$`Total Asset`+ try200812$`Div/Share`+ try200812$OPrevenuegrowth
           + try200812$ROA + try200812$`INDUSTRY CODE`, data = try200812
))


summary(lm(log(try200812$EV)~ try200812$Robust + try200812$oversea+
             try200812$`D/A` + try200812$`Total Asset`+ try200812$`Div/Share`+ try200812$OPrevenuegrowth
           + try200812$ROA + try200812$`INDUSTRY CODE`, data = try200812
))





###201014



summary(lm(log(try201014$TQA)~ try201014$Robust + try201014$oversea+
             try201014$`D/A` + try201014$`Total Asset`+ try201014$`Div/Share`+ try201014$OPrevenuegrowth
           + try201014$ROA + try201014$`INDUSTRY CODE`, data = try201014
))

###USING EV is significant..

summary(lm(log(try201014$EV)~ try201014$Robust + try201014$oversea+
             try201014$`D/A` + try201014$`Total Asset`+ try201014$`Div/Share`+ try201014$OPrevenuegrowth
           + try201014$ROA + try201014$`INDUSTRY CODE`, data = try201014
))



###201114 ##tqa IS OK

summary(lm(log(try201114$TQA)~ try201114$Robust + try201114$oversea+
             try201114$`D/A` + try201114$`Total Asset`+ try201114$`Div/Share`+ try201114$OPrevenuegrowth
           + try201114$ROA + try201114$`INDUSTRY CODE`, data = try201114
))


###USING EV as control also ok...


summary(lm(log(try201114$EV)~ try201114$Robust + try201114$oversea+
             try201114$`D/A` + try201114$`Total Asset`+ try201114$`Div/Share`+ try201114$OPrevenuegrowth
           + try201114$ROA + try201114$`INDUSTRY CODE`, data = try201114
))





write_csv(main_with_totalasset,file ='1main.csv')







###ROBUSTNESS TEST AND CORRECTIONS

tqarobust <- plm(log(main_with_totalasset$TQA) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                   main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
                 + main_with_totalasset$ROA  , data = main_with_totalasset
                 ,index =c("stockid","Report Date"),model='within')



tqaInrobust <- plm(log(main_with_totalasset$TQA) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                     main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
                   + main_with_totalasset$ROA  , data = main_with_totalasset
                   ,index =c("stockid","Report Date"),model='within')




EVrobust <- plm(log(main_with_totalasset$EV) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                  main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
                + main_with_totalasset$ROA  , data = main_with_totalasset
                ,index =c("stockid","Report Date"),model='within')



EVInrobust<- plm(log(main_with_totalasset$EV) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                   main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
                 + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE` , data = main_with_totalasset
                 ,index =c("stockid","Report Date"),model='within')





RiskInrobust<- plm(log(main_with_totalasset$ChangeInCFVolatility) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                     main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
                   + main_with_totalasset$ROA  , data = main_with_totalasset
                   ,index =c("stockid","Report Date"),model='within')


stockvolrobust<- plm(log(main_with_totalasset$stockreturnvolatility) ~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
                       main_with_totalasset$`D/A` + main_with_totalasset$`Total Asset`+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
                     + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`  , data = main_with_totalasset
                     ,index =c("stockid","Report Date"),model='within',effect='time')

returnrobust<- plm(main_with_totalasset$Changeinstockreturn~ main_with_totalasset$`Derivative Usage Dummy` + main_with_totalasset$oversea+
              main_with_totalasset$`D/A` + log(main_with_totalasset$EV)+ main_with_totalasset$`Div/Share`+ main_with_totalasset$OPrevenuegrowth
            + main_with_totalasset$ROA + main_with_totalasset$`INDUSTRY CODE`, data = main_with_totalasset
            ,index =c("stockid","Report Date"),model="within", effect='time')









coeftest(RiskInrobust, vcovSCC(RiskInrobust,type="HC3"))

coeftest(EVInrobust, vcovSCC(EVInrobust,type="HC3"))

coeftest(tqarobust, vcovHC(EVrobust, method = "arellano"))

bptest(RiskInrobust)

pbgtest(correct1,order=)

##change in CFvolatility
bptest(RiskInrobust)
pbgtest(RiskInrobust,order=1)

####stock volatility 

bptest(stockvolrobust)
pbgtest(stockvolrobust,order=3)
pbgtest(stockvolrobust,order=2)
pbgtest(stockvolrobust,order=1)
###correct
coeftest(RiskInrobust, vcovSCC(RiskInrobust,type="HC3"))

coeftest(RiskInrobust, vcovHC(RiskInrobust, method = "arellano"))

coeftest(stockvolrobust, vcovSCC(RiskInrobust,type="HC3"))

coeftest(stockvolrobust, vcovHC(RiskInrobust, method = "arellano"))



###correct
coeftest(RiskInrobust, vcovSCC(RiskInrobust,type="HC3"))

coeftest(RiskInrobust, vcovHC(RiskInrobust, method = "arellano"))

coeftest(stockvolrobust, vcovSCC(stockvolrobust,type="HC3"))

coeftest(stockvolrobust, vcovHC(stockvolrobust, method = "arellano"))









###Correction for third research interest
bptest(returnrobust)
pbgtest(returnrobust,order=3)
pbgtest(returnrobust,order=2)
pbgtest(returnrobust,order=1)

coeftest(returnrobust, vcovSCC(returnrobust,type="HC3"))

coeftest(returnrobust, vcovHC(returnrobust, method = "arellano"))




rcorr(cbind(main_with_totalasset$TQA, main_with_totalasset$`Derivative Usage Dummy` , main_with_totalasset$oversea,
        main_with_totalasset$`D/A` , exp(main_with_totalasset$`Total Asset`), main_with_totalasset$`Div/Share`, main_with_totalasset$OPrevenuegrowth
      ,main_with_totalasset$ROA)


rcorr(cbind(main_with_totalasset$TQA,main_with_totalasset$EV))



rcorr(cbind(main_with_totalasset$EV, main_with_totalasset$`Derivative Usage Dummy` , main_with_totalasset$oversea,
            main_with_totalasset$`D/A` , exp(main_with_totalasset$`Total Asset`), main_with_totalasset$`Div/Share`, main_with_totalasset$OPrevenuegrowth
            ,main_with_totalasset$ROA))
      





