# TIME SERIES MODELS
install.packages("readxl")
library(readxl)
install.packages("writexl")
library(writexl)
install.packages("forecast")
library(forecast)
install.packages("xts")
library(xts)
install.packages("nlme")
library(nlme)
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
require(INLA)
require(ggplot2)
require(gridExtra)
require(mgcv)
require(dplyr)
install.packages("DAAG")
require(DAAG)
require(reshape2)
require(MASS)
require(plotly)
require(car)
require(zoo)
require(mgcv)
require(stringr)
library(extrafont)
loadfonts(device = "win")


# Helper Functions
#-----------------#

cleanTable <- function(table) {
  # A function to transform a csv file into a table.
  clean.table <- c()
  for (row in 1:length(table)) {
    this.row <- strsplit(table[row],"\t")
    clean.table <- rbind(clean.table,as.character(this.row[[1]]))
  }
  clean.table
}

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

index <- function(lst,item,k,all=FALSE) {
  # Return the first index of item in lst. If k = -1, return the last one. Return
  # only the one index is all is false, else return all.
  indices <- c()
  n <- length(lst)
  for (i in 1:n) {
    if (item==lst[i]) {
      indices <- append(indices,i,length(indices))
    }
  }
  if ((k!=-1)&&(!all)) {
    indices[k]
  }
  else if (k==-1) {
    n <- length(indices)
    indices[n]
  }
  else {
    indices
  }
}

unique <- function(lst) {
  # Create a list of each unique element of lst.
  unit <- c()
  for (item in lst) {
    if (!(item%in%unit)) {
      unit <- append(unit,item,length(unit))
    }
  }
  unit
}

countUnique <- function(lst) {
  #Return each element in lst exactly once
  unique <- c()
  for (item in lst) {
    if (!(item%in%unique)) {
      unique <- append(unique,item,length(unique))
    }
  }
  unique
}

myFilter <- function(lst, val,type="LEq") {
  # Return all indices of list of type LEq (defualt==less than or equal to).
  # Possible values of type== L, Eq, G, LEq, Eq, GEq
  my.vals <- c()
  n <- length(lst)
  for (i in 1:n) {
    if (lst[i]<=val) {
      my.vals <- append(my.vals,i,length(my.vals))
    }
  }
  my.vals
}

remove_if <- function(lst,test_fn) {
  ## DD . char or list
  if (class(lst)=="character"){
    unlist(lapply(lst ,function(x){if(!(test_fn(x))) x }))
  } else {
    
    remove_if_null(lapply(lst ,function(x){if(!(test_fn(x))) x }))}
}


## MASK TESTS
colnames(worldtest.num)[1] <- "Date"

# CANADA
can.maskhyp <- c()
can.maxmask <- can.tsdf[59:132,] # The 59th observation, on April 22, is generally considered
# to be the peak of COVID-19's first wave, after which many areas began to mandate masks.
can.provdayswhich <- c()
for (i in 1:nca) {
  provmask <- as.numeric(as.character(can.tsdf[,((17*(i-1))+16)]))
  masklaw <- as.numeric(as.character(can.tsdf[,((17*(i-1))+9)]))
  provwhich <- provmask[which(provmask==1)]
  fracmask <- length(provwhich)/132 # If fracmask > 50%, declared to have a mask mandate
  isMask <- (fracmask>=0.5)
  prov.maskLaw <- list(provmask,provwhich,fracmask,isMask)
  can.provdayswhich <- append(can.provdayswhich,prov.maskLaw,length(can.provdayswhich))
}

can.yesmasks <- c()
for (i in 1:nca) {
  if (can.provdayswhich[[4*i]]==TRUE) {
    can.yesmasks <- append(can.yesmasks,i,length(can.yesmasks))
  }
}
nYes.ca <- length(can.yesmasks)
can.yespops <- can.pops[can.yesmasks]

can.yesMaskChi1000 <- c()
for (i in 1:132) {
  daysum <- 0
  for (y in can.yesmasks) {
    this.pop <- can.pops[y]
    daysum <- daysum + ((as.numeric(as.character(can.tsdf[i,((17*(y-1))+4)])))*(this.pop))
  }
  can.yesMaskChi1000 <- append(can.yesMaskChi1000,daysum/(sum(can.yespops*1000)),length(can.yesMaskChi1000))
}

can.nomasks <- c(1:nca)[-can.yesmasks]
nNo.ca <- length(can.nomasks)
can.nopops <- can.pops[can.nomasks]
can.noMaskChi1000 <- c()
for (i in 1:132) {
  daysum <- 0
  for (y in can.nomasks) {
    this.pop <- can.pops[y]
    daysum <- daysum + ((as.numeric(as.character(can.tsdf[i,((17*(y-1))+4)])))*(this.pop))
  }
  can.noMaskChi1000 <- append(can.noMaskChi1000,daysum/(sum(can.nopops)*1000),length(can.noMaskChi1000))
}

can.maskplot1000 <- ggplot(data=NULL,aes(x=c(1:132),y=can.yesMaskChi1000))+
  geom_line(data=NULL,aes(x=c(1:132),y=can.yesMaskChi1000),col="blue")+
  geom_line(data=NULL,aes(x=c(1:132),y=can.noMaskChi1000),col="red")

can.maskChi1000 <- ((can.yesMaskChi1000-can.noMaskChi1000)/can.noMaskChi1000)^2
plot(can.noMaskChi1000,can.yesMaskChi1000)
can.sidemod <- worldtest.num[1:(nca*132),]
can.andmask <- c()
for (prov in 1:nca) {
  temp <- can.sidemod[c(((132-1)*prov+1):((132-1)*prov+132)),]
  if (prov%in%can.yesmasks) {
    temp <- cbind(temp,can.yesMaskChi1000)
  }
  else {
    temp <- cbind(temp,can.noMaskChi1000)
  }
  can.andmask <- rbind(can.andmask,temp)
}
can.sidemod <- can.andmask

can.mask1000 <- as.data.frame(cbind(can.df$Date,can.yesMaskChi1000,
                                    can.noMaskChi1000))
can.mask1000$Date <- as.Date(can.df$Date,"%Y-%m-%d")
can.mask1000xts <- as.xts(can.mask1000)
can.maskline1000 <- tslm(ts(can.noMaskChi1000,start=1,end=132)~
                           ts(can.yesMaskChi1000,start=1,end=132),data=NULL)
can.fit1000 <- fitted(can.maskline1000)
plot(x=can.noMaskChi1000,y=can.yesMaskChi1000)
lines(can.noMaskChi1000,can.fit1000,col="blue")

# Line:
can.b = summary(can.maskline1000)$coef
can.b0 = cbind(can.b[1,1],can.b[1,1]-can.b[1,2],can.b[1,1]+can.b[1,2])
can.b1 = cbind(can.b[2,1],can.b[2,1]-can.b[2,2],can.b[2,1]+can.b[2,2])

ggplot(data=NULL,aes(x=can.noMaskChi1000,y=can.yesMaskChi1000))+geom_point()+geom_smooth()+
  labs(x="Daily Cases in Canada, provinces with no mask mandate",
       y="Daily Cases in Canada, provinces with a mask mandate",
       title="Effect of mask mandate on Daily Cases: Canada")

# In y = b0 + b1x, if b1 < 1, then x (no masks) is less effective than y (masks).
# If b1 > 1, x (no masks) is more effective than y (masks).
# If b1 = 1, there is no significant differnce.
# Perform a hypothesis test.

# Masks versus positivity rates--how do the provinces compare?
can.posmask <- c(as.numeric(as.character(coef(can.maskline1000)[2]))*1000,
                 as.numeric(as.character(coef(can.Posline)[2]))) # normalized back to per capita
epiMask.can <- can.posmask[1]/can.posmask[2] # The positivity-case effect.
# Do a chi square test to see independence?
# If epiMask > 1, then for a given daily positivity rate, provinces without mask mandates are testing more
# efficiently, and so the rate of case growth is largely independent of the testing rate.
# If epiMask < 1, then for a given daily positivity rate, provinces without mask mandates are testing less
# efficiently, and so the rate of case growth depends on the testing rate.
# If epiMask = 1, there is no effect of testing.
epiK <- log(can.posmask) # The log rate between daily cases per 1000 and positivifty rate for provinces
# without/with masks.

# Take dCases(yesMask)/dt = b0 + b1*dCases(noMask)/dt, FIGURE THIS OUT
# Combinations of yesMask, noMask
b1 = c()
for (j in yesmask) {
  for (i in nomask) {
    thisres <- canmaskres[i][[1]]$coef[2,c(1:3)]/sqrt(vecSum(k,((xkj-xki)^2)/max(xk)))
    thisnew <- c(thisres[1],thisres[1]-thisres[2],thisres[1]+thisres[2])
    b1 <- append(b1,thisnew,length(b1))
  }
}
wij = sqrt(vecSum(k,((xkj-xki)^2)/max(xk)))
b1avg = (1/vecSum(j,vecSum(i,(1/wij))))*sum(b1[,1])

##################

# USA
usa.maskhyp <- c()
usa.maxmask <- usa.tsdf[59:132,] # The 59th observation, on April 22, is generally considered
# to be the peak of COVID-19's first wave, after which many areas began to mandate masks.
usa.provdayswhich <- c()
for (i in 1:nusa) {
  provmask <- as.numeric(as.character(usa.tsdf[,((18*(i-1))+17)]))
  masklaw <- as.numeric(as.character(usa.tsdf[,((18*(i-1))+10)]))
  provwhich <- provmask[which(masklaw==1)]
  fracmask <- length(provwhich)/132 # If fracmask > 50%, declared to have a mask mandate
  isMask <- (fracmask>=0.5)
  prov.maskLaw <- list(provmask,provwhich,fracmask,isMask)
  usa.provdayswhich <- append(usa.provdayswhich,prov.maskLaw,length(usa.provdayswhich))
}

usa.yesmasks <- c()
for (i in 1:nusa) {
  if (usa.provdayswhich[[4*i]]==TRUE) {
    usa.yesmasks <- append(usa.yesmasks,i,length(usa.yesmasks))
  }
}
nYes.usa <- length(usa.yesmasks)
usa.yespops <- usa.pops[usa.yesmasks]

usa.yesMaskChi1000 <- c()
for (i in 1:132) {
  daysum <- 0
  for (y in usa.yesmasks) {
    num <- usa.tsdf[i,((18*(y-1))+5)]
    if (!(is.na(as.numeric(as.character(num))))) {
      daysum <- daysum + ((as.numeric(as.character(num)))/this.pop*1000)
    }
    else {
      daysum <- daysum + 0
    }
  }
  usa.yesMaskChi1000 <- append(usa.yesMaskChi1000,daysum/sum(usa.yespops),length(usa.yesMaskChi1000))
}
usa.yeslog <- log(usa.yesMaskChi)

usa.nomasks <- c(1:nusa)[-usa.yesmasks]
nNo.usa <- length(usa.nomasks)
usa.nopops <- usa.pops[usa.nomasks]
usa.noMaskChi1000 <- c()
for (i in 1:132) {
  daysum <- 0
  for (y in usa.nomasks) {
    this.pop <- usa.pops[y]
    num <- usa.tsdf[i,((18*(y-1))+5)]
    if (!(is.na(as.numeric(as.character(num))))) {
      daysum <- daysum + ((as.numeric(as.character(num)))/this.pop*1000)
    }
    else {
      daysum <- daysum + 0
    }
  }
  usa.noMaskChi1000 <- append(usa.noMaskChi1000,daysum,length(usa.noMaskChi1000))
}
usa.nolog <- log(usa.noMaskChi)

usa.maskplot1000 <- ggplot(data=NULL,aes(x=c(1:132),y=usa.yesMaskChi1000))+
  geom_line(data=NULL,aes(x=c(1:132),y=usa.yesMaskChi1000),col="blue")+
  geom_line(data=NULL,aes(x=c(1:132),y=usa.noMaskChi1000),col="red")

usa.maskChi1000 <- ((usa.yesMaskChi1000-usa.noMaskChi1000)/usa.noMaskChi1000)^2
plot(usa.noMaskChi1000,usa.yesMaskChi1000)
usa.maskline1000 <- tslm(usa.yesMaskChi1000~usa.noMaskChi1000, data=NULL)
usa.fit1000 <- fitted(usa.maskline1000)
lines(usa.yesMaskChi1000,usa.fit1000,col="blue")

# Line:
usa.b = summary(usa.maskline1000)$coef
usa.b0 = cbind(usa.b[1,1],usa.b[1,1]-usa.b[1,2],usa.b[1,1]+usa.b[1,2])
usa.b1 = cbind(usa.b[2,1],usa.b[2,1]-usa.b[2,2],usa.b[2,1]+usa.b[2,2])

ggplot(data=NULL,aes(x=usa.noMaskChi1000,y=usa.yesMaskChi1000))+geom_point()+geom_smooth()+
  labs(x="Daily Cases in the United States, states with no mask mandate",
       y="Daily Cases in the United States, states with a mask mandate",
       title="Effect of mask mandate on Daily Cases: USA")


# In y = b0 + b1x, if b1 < 1, then x (no masks) is less effective than y (masks).
# If b1 > 1, x (no masks) is more effective than y (masks).
# If b1 = 1, there is no significant differnce.
# Perform a hypothesis test.

# Masks versus positivity rates--how do the provinces compare?
usa.posmask <- c(as.numeric(as.character(coef(usa.maskline1000)[2]))*1000,
                 as.numeric(as.character(coef(usa.Posline)[2]))) # normalized back to per capita
epiMask.usa <- usa.posmask[1]/usa.posmask[2] # The positivity-case effect.
# Do a chi square test to see independence?
# If epiMask > 1, then for a given daily positivity rate, provinces without mask mandates are testing more
# efficiently, and so the rate of case growth is largely independent of the testing rate.
# If epiMask < 1, then for a given daily positivity rate, provinces without mask mandates are testing less
# efficiently, and so the rate of case growth depends on the testing rate.
# If epiMask = 1, there is no effect of testing.
epiK <- log(usa.posmask) # The log rate between daily cases per 1000 and positivifty rate for provinces
# without/with masks.


# Germany
germ.maskhyp <- c()
germ.maxmask <- germ.tsdf[59:132,] # The 59th observation, on April 22, is generally considered
# to be the peak of COVID-19's first wave, after which many areas began to mandate masks.
germ.provdayswhich <- c()
for (i in 1:nde) {
  provmask <- as.numeric(as.character(germ.tsdf[,((18*(i-1))+17)]))
  masklaw <- as.numeric(as.character(germ.tsdf[,((18*(i-1))+7)]))
  provwhich <- provmask[which(masklaw==1)]
  fracmask <- length(provwhich)/132 # If fracmask > 50%, declared to have a mask mandate
  isMask <- (fracmask>=0.5)
  prov.maskLaw <- list(provmask,provwhich,fracmask,isMask)
  germ.provdayswhich <- append(germ.provdayswhich,prov.maskLaw,length(germ.provdayswhich))
}

germ.yesmasks <- c()
for (i in 1:nde) {
  if (germ.provdayswhich[[4*i]]==TRUE) {
    germ.yesmasks <- append(germ.yesmasks,i,length(germ.yesmasks))
  }
}
nYes.germ <- length(germ.yesmasks)
germ.yespops <- germ.pops[germ.yesmasks]

germ.yesMaskChi1000 <- c()
for (i in 1:132) {
  daysum <- 0
  for (y in germ.yesmasks) {
    num <- germ.tsdf[i,((18*(y-1))+5)]
    if (!(is.na(as.numeric(as.character(num))))) {
      daysum <- daysum + ((as.numeric(as.character(num)))/this.pop*1000)
    }
    else {
      daysum <- daysum + 0
    }
  }
  germ.yesMaskChi1000 <- append(germ.yesMaskChi1000,daysum/sum(germ.yespops),length(germ.yesMaskChi1000))
}
germ.yeslog <- log(germ.yesMaskChi)

germ.nomasks <- c(1:nde)[-germ.yesmasks]
nNo.germ <- length(germ.nomasks)
germ.nopops <- germ.pops[germ.nomasks]
germ.noMaskChi1000 <- c()
for (i in 1:132) {
  daysum <- 0
  for (y in germ.nomasks) {
    this.pop <- germ.pops[y]
    num <- germ.tsdf[i,((18*(y-1))+5)]
    if (!(is.na(as.numeric(as.character(num))))) {
      daysum <- daysum + ((as.numeric(as.character(num)))/this.pop*1000)
    }
    else {
      daysum <- daysum + 0
    }
  }
  germ.noMaskChi1000 <- append(germ.noMaskChi1000,daysum,length(germ.noMaskChi1000))
}
germ.nolog <- log(germ.noMaskChi)

germ.maskplot1000 <- ggplot(data=NULL,aes(x=c(1:132),y=germ.yesMaskChi1000))+
  geom_line(data=NULL,aes(x=c(1:132),y=germ.yesMaskChi1000),col="blue")+
  geom_line(data=NULL,aes(x=c(1:132),y=germ.noMaskChi1000),col="red")

germ.maskChi1000 <- ((germ.yesMaskChi1000-germ.noMaskChi1000)/germ.noMaskChi1000)^2
plot(germ.noMaskChi1000,germ.yesMaskChi1000)
germ.maskline1000 <- tslm(germ.yesMaskChi1000~germ.noMaskChi1000, data=NULL)
germ.fit1000 <- fitted(germ.maskline1000)
lines(germ.yesMaskChi1000,germ.fit1000,col="blue")

# Line:
germ.b = summary(germ.maskline1000)$coef
germ.b0 = cbind(germ.b[1,1],germ.b[1,1]-germ.b[1,2],germ.b[1,1]+germ.b[1,2])
germ.b1 = cbind(germ.b[2,1],germ.b[2,1]-germ.b[2,2],germ.b[2,1]+germ.b[2,2])

ggplot(data=NULL,aes(x=germ.noMaskChi1000,y=germ.yesMaskChi1000))+geom_point()+geom_smooth()+
  labs(x="Daily Cases in the United States, states with no mask mandate",
       y="Daily Cases in the United States, states with a mask mandate",
       title="Effect of mask mandate on Daily Cases: germ")


# In y = b0 + b1x, if b1 < 1, then x (no masks) is less effective than y (masks).
# If b1 > 1, x (no masks) is more effective than y (masks).
# If b1 = 1, there is no significant differnce.
# Perform a hypothesis test.

# Masks versus positivity rates--how do the provinces compare?
germ.posmask <- c(as.numeric(as.character(coef(germ.maskline1000)[2]))*1000,
                  as.numeric(as.character(coef(germ.Posline)[2]))) # normalized back to per capita
epiMask.germ <- germ.posmask[1]/germ.posmask[2] # The positivity-case effect.
# Do a chi square test to see independence?
# If epiMask > 1, then for a given daily positivity rate, provinces without mask mandates are testing more
# efficiently, and so the rate of case growth is largely independent of the testing rate.
# If epiMask < 1, then for a given daily positivity rate, provinces without mask mandates are testing less
# efficiently, and so the rate of case growth depends on the testing rate.
# If epiMask = 1, there is no effect of testing.
epiK <- log(germ.posmask) # The log rate between daily cases per 1000 and positivifty rate for provinces
# without/with masks.

######################

# some sample plots

ggplot(data=NULL,aes(x=worldtest.df$Date[1:132],y=as.numeric(as.character(worldtest.df$Positivity[1:132]))))+
  geom_point()+geom_line(data=NULL,aes(x=as.numeric(worldtest.df$Date[1:132]),
                                       y=as.numeric(as.character(worldtest.df$Positivity[1:132]))),col="black")+
  geom_point(data=NULL,aes(x=as.numeric(worldtest.df$Date[1:132]),
                           y=as.numeric(as.character(worldtest.df$PercentMasks[1:132]))),col="red")+
  geom_line(data=NULL,aes(x=as.numeric(worldtest.df$Date[1:132]),
                          y=as.numeric(as.character(worldtest.df$PercentMasks[1:132]))),col="red")+
  geom_point(data=NULL,aes(x=as.numeric(worldtest.df$Date[1:132]),
                           y=as.numeric(as.character(worldtest.df$Mobility[1:132]))),col="blue")+
  geom_line(data=NULL,aes(x=as.numeric(worldtest.df$Date[1:132]),
                          y=as.numeric(as.character(worldtest.df$Mobility[1:132]))),col="blue")

# positivity
ggplot(data=NULL,aes(x=worldtest.df$Date[1:132],y=as.numeric(as.character(worldtest.df$Positivity[1:132]))))+
  geom_point()+geom_line(data=NULL,aes(x=as.numeric(worldtest.df$Date[1:132]),
                                       y=as.numeric(as.character(worldtest.df$Positivity[1:132]))))+
  geom_smooth(data=NULL,aes(x=as.numeric(worldtest.df$Date[1:132]),
                            y=as.numeric(as.character(worldtest.df$Positivity[1:132]))),col="blue")


all.names <- readClipboard()
all.names <- cleanTable(all.names)
all.names <- c(all.names)
all.demnames <- all.names[1:98]

fedvote <- readClipboard()
fedvote <- cleanTable(fedvote)
colnames(fedvote) <- c("Lfedvote","Cfedvote","Rfedvote")
fedvote <- fedvote[1:98,]
rownames(fedvote) <- all.demnames
fedvote.df <- as.data.frame(fedvote)

regvote <- readClipboard()
regvote <- cleanTable(regvote)
regvote <- regvote[1:98,]
rownames(regvote) <- all.demnames
regvote.df <- as.data.frame(regvote)

allfedL <- c()
allfedC <- c()
allfedR <- c()
allregL <- c()
allregC <- c()
allregR <- c()
for (row in 1:98) {
  allfedL <- append(allfedL,rep(fedvote.df[row,1],132),length(allfedL))
  allfedC <- append(allfedC,rep(fedvote.df[row,2],132),length(allfedC))
  allfedR <- append(allfedR,rep(fedvote.df[row,3],132),length(allfedR))
  allregL <- append(allregL,rep(regvote.df[row,1],132),length(allregL))
  allregC <- append(allregC,rep(regvote.df[row,2],132),length(allregC))
  allregR <- append(allregR,rep(regvote.df[row,3],132),length(allregR))
}
allfed.bigger <- cbind(allfedL,allfedC,allfedR)
allreg.bigger <- cbind(allregL,allregC,allregR)
#worldtest.df <- cbind(worldtest.df,allfed.bigger)
#worldtest.df <- as.data.frame(worldtest.df)

world.bigger <- rbind(can.bigger,usa.bigger,germ.bigger,italy.bigger,eu.bigger)
worldextra.df <- as.data.frame(world.bigger[,c(4:15)][,-6])

library(lubridate)

#worldtest.df <- worldtest.df[-1,]
for (col in 1:ncol(worldtest.df)) {
  for (row in 1:nrow(worldtest.df)) {
    if ((worldtest.df[row,col]=="")|(worldtest.df[row,col]=="-")|
        (worldtest.df[row,col]=="#DIV/0!")) {
      worldtest.df[row,col] <- 0
    }
  }
}

library(stringr)
install.packages("data.table")
library(data.table)
install.packages("mltools")
library(mltools)
install.packages("ade4")
library(ade4)
install.packages("dynlm")
library(dynlm)
library(lmer)
install.packages("remotes")
remotes::install_github("stevencarlislewalker/lme4ord")
# General models

allfed.bigger <- as.data.frame(allfed.bigger)
allreg.bigger <- as.data.frame(allreg.bigger)
colnames(allfed.bigger) <- c("Lfedvote","Cfedvote","Rfedvote")
colnames(allreg.bigger) <- c("Lregvote","Cregvote","Rregvote")

worldtest.copy <- cbind(worldtest.df[,c(1:4)],world.bigger[,c(11:15)],worldtest.df[,c(5:36)])
worldtest.copy <- as.matrix(worldtest.copy,nrow=12936,ncol=36,by.row=TRUE)
write.table(worldtest.copy, file="clipboard-16384", sep="\t", col.names=NA)

world.covars = readClipboard()
world.covars = cleanTable(world.covars)
colnames(world.covars) = world.covars[1,]
world.covars = world.covars[2:133,]
world.covars.df = as.data.frame(world.covars)
world.covars.df$nCountry = c(rep(1,7),rep(2,51),rep(3,16),rep(4,21),5,6,7,rep(8,34))
world.covars.df$nReg = c(c(1:7),c(1:51),c(1:16),c(1:21),1,1,1,c(1:34))

govt.covars = readClipboard()
govt.covars = cleanTable(govt.covars)
govt.covars = govt.covars[,c(1:3,8:10)]
colnames(govt.covars) = c("Lfed","Cfed","Rfed","Lreg","Creg","Rreg")
govt.covars = govt.covars[3:12938,]
govt.covars.single = c()
for (row in 1:nrow(govt.covars)){ 
  if (row%%132==1) {
    govt.covars.single = rbind(govt.covars.single,govt.covars[row,])
  }
}
for (cprov in 1:34) {
  govt.covars.single = rbind(govt.covars.single,rep(0,3))
}
govt.covars.df = as.data.frame(govt.covars)
govt.covars.single.df = as.data.frame(govt.covars.single)
world.covars.df = cbind(world.covars.df[,c(1:17)],govt.covars.single[,c(1:3)],world.covars.df[,c(18:21)],
                        govt.covars.single[,c(4:6)],world.covars.df[,c(22:30)])
colnames(world.covars.df)[c(15:17)] = c("Lfedvote","Cfedvote","Rfedvote")
colnames(world.covars.df)[c(25:27)] = c("Lreggovt","Creggovt","Rreggovt")
options(digits=4)

# Social Distancing:
colnames(worldtest.df) <- worldtest.df[1,]
worldtest.df <- worldtest.df[-1,]
worldtest.df <- worldtest.df[,-c(37:40)]
worldtest.df$Lfedvote <- allfed.bigger$Lfedvote
worldtest.df$Cfedvote <- allfed.bigger$Cfedvote
worldtest.df$Rfedvote <- allfed.bigger$Rfedvote
worldtest.df$Lregvote <- allreg.bigger$Lregvote
worldtest.df$Cregvote <- allreg.bigger$Cregvote
worldtest.df$Rregvote <- allreg.bigger$Rregvote

worldtest.df$Countries = longcountries
worldtest.df$nregion = c(1:98)

options(digits=4)

library(readxl)
worldtest.df <- read_excel("UNIVERSITY/U OF T/COVID RESEARCH/Final Deliverable/social distancing.xlsx", 
                                sheet = "WorldTestDF")
View(social_distancing)

#worldtest.df <- readClipboard()
#worldtest.df <- cleanTable(worldtest.df)
#colnames(worldtest.df) <- worldtest.df[1,]
#worldtest.df <- worldtest.df[2:12937,]
#worldtest.df <- as.data.frame(worldtest.df)
#worldtest.df <- worldtest.df[,-1]

#dates <- readClipboard()
#dates <- as.Date(as.character(dates))
dates <- as.Date(worldtest.df$Date)
worldtest.df$Date <- dates

ncountries = c(7, 51, 16, 21, 1, 1, 1, 34)
countries = c("Canada","United States","Germany","Italy","United Kingdom","France","Sweden","China")
reps.can = c()
reg.can = c()
long.can = rep("Canada",(7*132))
len.can = rep(1,(7*132))
reps.usa = c()
reg.usa = c()
long.usa = rep("United States",(51*132))
len.usa = rep(2,(51*132))
reps.germ = c()
reg.germ = c()
long.germ = rep("Germany",(16*132))
len.germ = rep(3,(16*132))
reps.italy = c()
reg.italy = c()
long.italy = rep("Italy",(21*132))
len.italy = rep(4,(21*132))
for (r in 1:7) {
  reps.can = append(reps.can,rep(all.demnames[r],132),length(reps.can))
  reg.can = append(reg.can,rep(r,132),length(reg.can))
}
for (r in 1:51) {
  reps.usa = append(reps.usa,rep(all.demnames[7+r],132),length(reps.usa))
  reg.usa = append(reg.usa,rep(r,132),length(reg.usa))
}
for (r in 1:16) {
  reps.germ = append(reps.germ,rep(all.demnames[7+51+r],132),length(reps.germ))
  reg.germ = append(reg.germ,rep(r,132),length(reg.germ))
}
for (r in 1:21) {
  reps.italy = append(reps.italy,rep(all.demnames[7+51+16+r],132),length(reps.italy))
  reg.italy = append(reg.italy,rep(r,132),length(reg.italy))
}
#reps.italy = append(reps.italy,rep("Veneto",132),length(reps.italy))
longcountries = c(long.can,long.usa,long.germ,long.italy,
                  rep("United Kingdom",132),rep("France",132),rep("Sweden",132))
repcountries = c(len.can,len.usa,len.germ,len.italy,rep(1,132),rep(1,132),
                 rep(1,132))
longregions = c(reps.can,reps.usa,reps.germ,reps.italy,rep("",132),rep("",132),
                rep("",132))
repregions = c(reg.can,reg.usa,reg.germ,reg.italy,rep("",132),rep("",132),
               rep("",132))
# Length = 12936
worldtest.df$Countries = longcountries
worldtest.df$ncountry = repcountries
worldtest.df$Region = longregions
worldtest.df$nregion = repregions

alberta.LDgam = gam(as.numeric(as.character(prov.df$Mobility))~
                      s(as.numeric(Date))+
                      s(as.numeric(Date),by=interaction(
                        as.numeric(as.character(prov.df$Distancing)),
                        as.numeric(as.character(prov.df$Closures)),
                        as.numeric(as.character(prov.df$Lockdown)))),
                    data=prov.df)

for (c in 1:7) {
  country.df = worldtest.df[which(worldtest.df$ncountry==c),]
  nc = ncountries[c]
  country.fixed = c()
  country.box = c()
  country.names = c()
  if (c==1) {
    country.names = ca.names
  } else if (c==2) {
    country.names = usa.names
  } else if (c==3) {
    country.names = germ.names
  } else if (c==4) {
    country.names = italy.names
  }
  for (r in 1:nc) {
    name = country.names[r]
    prov.df = country.df[which(country.df$nregion==r),]
    f.reg = as.numeric(as.character(prov.df$Mobility))~
      f(as.numeric(as.Date(date)),model='ar1')+
      as.numeric(as.character(prov.df$Distancing))+
      as.numeric(as.character(prov.df$Closures))+
      as.numeric(as.character(prov.df$Lockdown))+
      as.numeric(as.character(prov.df$NumberInterventions))
    prov.inla = inla(f.reg,control.inla =
                       list(diagonal = 100, strategy = "gaussian",
                            int.strategy = "eb"),
                     data=prov.df)
    prov.fixed = prov.inla$summary.fixed[c(2:5),c(1:5)]
    prov.laws = c("Distancing","Closures","Lockdown","#Interventions")
    for (l in 1:4) {
      prov.laws[l] = paste(name,prov.laws[l])
    }
    rownames(prov.fixed) = prov.laws
    country.fixed = rbind(country.fixed,prov.fixed)
  }
  for (r in 1:nc) {
    country.box = rbind(country.box,t(country.fixed[c((4*(r-1)+1):(4*(r-1)+4)),1]))
  }
  if (c==1) {
    rownames(country.box) = ca.names
  } else if (c==2) {
    rownames(country.box) = usa.names
  } else if (c==3) {
    rownames(country.box) = germ.names
  } else if (c==4) {
    rownames(country.box) = italy.names
  }
}

uk.df = worldtest.df[which(worldtest.df$Countries=="United Kingdom"),]
fr.df = worldtest.df[which(worldtest.df$Countries=="France"),]
sw.df = worldtest.df[which(worldtest.df$Countries=="Sweden"),]
eu.df = rbind(uk.df,fr.df,sw.df)
eu.fixed = c()
eu.box = c()
eu.names = c("United Kingdom","France","Sweden")
for (eu in 1:3) {
  name = eu.names[eu]
  euCountry.df = NULL
  if (eu==1) {
    euCountry.df = uk.df
  } else if (eu==2) {
    euCountry.df = fr.df
  } else if (eu==3) {
    euCountry.df = sw.df
  }
  f.eu = as.numeric(as.character(euCountry.df$Mobility))~
    f(as.numeric(as.Date(date)),model='ar1')+
    as.numeric(as.character(euCountry.df$Distancing))+
    as.numeric(as.character(euCountry.df$Closures))+
    as.numeric(as.character(euCountry.df$Lockdown))+
    as.numeric(as.character(euCountry.df$NumberInterventions))
  eu.inla = inla(f.eu,control.inla =
                     list(diagonal = 100, strategy = "gaussian",
                          int.strategy = "eb"),
                   data=euCountry.df)
  euCountry.fixed = eu.inla$summary.fixed[c(2:5),c(1:5)]
  euCountry.laws = c("Distancing","Closures","Lockdown","#Interventions")
  for (l in 1:4) {
    euCountry.laws[l] = paste(name,euCountry.laws[l])
  }
  rownames(euCountry.fixed) = euCountry.laws
  eu.fixed = rbind(eu.fixed,euCountry.fixed)
}
for (eu in 1:3) {
  eu.box = rbind(eu.box,t(eu.fixed[c((4*(eu-1)+1):(4*(eu-1)+4)),1]))
}
rownames(eu.box) = eu.names

eu.casePlot = ggplot(data=NULL,aes(x=as.Date(date)))+
  geom_point(data=NULL,aes(y=uk.conf[34:165]))+
  geom_line(data=NULL,aes(y=uk.conf[34:165]),col="red")+
  geom_point(data=NULL,aes(y=fr.conf[34:165]))+
  geom_line(data=NULL,aes(y=fr.conf[34:165]),col="blue")+
  ggtitle("Cumulative cases in the UK vs. France, February 22 to July 4, 2020")+
  xlab("Days since Feb 22")+ylab("Cases")
eu.mobPlot = ggplot(data=NULL,aes(x=as.Date(date)))+
  geom_point(data=uk.df,aes(x=as.Date(date)),y=
               as.numeric(as.character(uk.df$Mobility)))+
  geom_line(data=uk.df,aes(x=as.Date(date)),y=
              as.numeric(as.character(uk.df$Mobility)),col="green")+
  geom_point(data=fr.df,aes(x=as.Date(date)),y=
               as.numeric(as.character(fr.df$Mobility)))+
  geom_line(data=fr.df,aes(x=as.Date(date)),y=
              as.numeric(as.character(fr.df$Mobility)),col="yellow")

SD.bycountry = vector('list',length=7)
SD.countrymods = vector('list',length=7)
titles = c("Country","Region","Distancing","Closures","Lockdown",
               "Start","End","#Days/132","DayMean","Lower","Upper","Std.Err.")
allReports = titles
allREPORTS = c()
# (COUNTRY) (REGION) (X) (Y) (Z) (START) (END) (START, MEAN, END) (LOWER) (UPPER) (SERR)
c <- 1
while (c <= 7) {
  countryREPORT = c()
  nc = ncountries[c]
  country = countries[c]
  dc = sum(ncountries[0:(c-1)])
  country.df <- worldtest.df[c(((132*(dc))+1):((132*(dc+nc-1))+132)),]
  #region = c()
  #for (reg in 1:nc) {
  #  region = append(region,rep(reg,132),length(region))
  #}
  #country.df$Region = region
  #countryextra.df <- worldextra.df[c(((132*(dc+1))+1):((132*(dc+nc))+132)),]
  #countryvote.df <- cbind(allfed.bigger[c(((132*(dc+1))+1):((132*(dc+nc))+132)),],
  #                        allreg.bigger[c(((132*(dc+1))+1):((132*(dc+nc))+132)),])
  #country.df <- cbind(country.df[,c(1:9)],countryextra.df,country.df[,-c(1:9)],countryvote.df)
  #country.df <- as.data.frame(country.df)
  #country.df <- as.data.frame(cbind(country.df[,c(1:9)],country.df[,c(10:20)],
  #                                  country.df[,c(21:53)]))
  
  #countryadj.df = country.df
  #hot_combined = c('Distancing','Closures','Lockdown','Cfed','Rfed')
  #for (f in hot_combined) {
  #  df_all_dummy = acm.disjonctif(countryadj.df[f])
  #  countryadj.df[f] = NULL
  #  countryadj.df = cbind(df_all_dummy,countryadj.df)
  #}
  
  SD.newcountry = vector('list',length=(nc+1))
  SD.newcountry[1][[1]] = paste("Models for",countries[c])
  subregions = vector('list',length=(nc+1))
  
  #if (!(country.df[1,])==worldtest.df[1,])
  
  # SOCIAL DISTANCING MODELS
  for (r in 1:nc) {
    regionREPORT = c()
    prov.name = paste(all.demnames[dc+1])
    #prov.name = str_sub(prov.name,end=nchar(prov.name))
    prov.df = country.df[c(((132*(r-1))+1):((132*(r-1))+132)),]
    cases000 = which((prov.df$Distancing==0)&(prov.df$Closures==0)&(prov.df$Lockdown==0))
    cases001 = which((prov.df$Distancing==0)&(prov.df$Closures==0)&(prov.df$Lockdown==1))
    cases010 = which((prov.df$Distancing==0)&(prov.df$Closures==1)&(prov.df$Lockdown==0))
    cases011 = which((prov.df$Distancing==0)&(prov.df$Closures==1)&(prov.df$Lockdown==1))
    cases100 = which((prov.df$Distancing==1)&(prov.df$Closures==0)&(prov.df$Lockdown==0))
    cases101 = which((prov.df$Distancing==1)&(prov.df$Closures==0)&(prov.df$Lockdown==1))
    cases110 = which((prov.df$Distancing==1)&(prov.df$Closures==1)&(prov.df$Lockdown==0))
    cases111 = which((prov.df$Distancing==1)&(prov.df$Closures==1)&(prov.df$Lockdown==1))
    save.mods <- c(1,2,3,4,6,7,11,14,15,16,17,20,26,27,29,33,34,35,36,43,46)
    SD.gam10 = NULL
    SD.gam10sum = list()
    if (length(cases000)>0) {
      SD.gam10 = NULL
      if (length(cases000)>10) {
        SD.gam10 = gam(as.numeric(as.character(Mobility)) ~ s(as.numeric(Date)),data=prov.df[cases000,])
      } else {
        SD.gam10 = gam(as.numeric(as.character(Mobility)) ~ s(as.numeric(Date),k=2),data=prov.df[cases000,])
      }
      SD.gam10sum = list(summary(SD.gam10))
      for (num in 1:length(save.mods)) {
        SD.gam10sum = list(SD.gam10sum,SD.gam10[save.mods[num]])
      }
    } else {
      SD.gam10sum = list(SD.gam10sum,
                         paste(prov.name,": No such model for distancing = 0 | closures = 0 | lockdown = 0"))
    }
    SD.gam11 = NULL
    SD.gam11sum = list()
    if (length(cases001)>0) {
      SD.gam11 = NULL
      if (length(cases001)>10) {
        SD.gam11 = gam(as.numeric(as.character(Mobility)) ~ s(as.numeric(Date)),data=prov.df[cases001,])
      } else {
        SD.gam11 = gam(as.numeric(as.character(Mobility)) ~ s(as.numeric(Date),k=2),data=prov.df[cases001,])
      }
      SD.gam11sum = list(summary(SD.gam11))
      for (num in 1:length(save.mods)) {
        SD.gam11sum = list(SD.gam11sum,SD.gam11[save.mods[num]])
      }
    } else {
      SD.gam11sum = list(SD.gam11sum,
                         paste(prov.name,": No such model for distancing = 0 | closures = 0 | lockdown = 1"))
    }
    SD.gam20 = NULL
    SD.gam20sum = list()
    if (length(cases010)>0) {
      SD.gam20 = NULL
      if (length(cases010)>10) {
        SD.gam20 = gam(as.numeric(as.character(Mobility)) ~ s(as.numeric(Date)),data=prov.df[cases010,])
      } else {
        SD.gam20 = gam(as.numeric(as.character(Mobility)) ~ s(as.numeric(Date),k=2),data=prov.df[cases010,])
      }
      SD.gam20sum = list(summary(SD.gam20))
      for (num in 1:length(save.mods)) {
        SD.gam20sum = list(SD.gam20sum,SD.gam20[save.mods[num]])
      }
    } else {
      SD.gam20sum = list(SD.gam20sum,
                         paste(prov.name,": No such model for distancing = 0 | closures = 1 | lockdown = 0"))
    }
    SD.gam21 = NULL
    SD.gam21sum = list()
    if (length(cases011)>0) {
      SD.gam21 = NULL
      if (length(cases011)>10) {
        SD.gam21 = gam(as.numeric(as.character(Mobility)) ~ s(as.numeric(Date)),data=prov.df[cases011,])
      } else {
        SD.gam21 = gam(as.numeric(as.character(Mobility)) ~ s(as.numeric(Date),k=2),data=prov.df[cases011,])
      }
      SD.gam21sum = list(summary(SD.gam21))
      for (num in 1:length(save.mods)) {
        SD.gam21sum = list(SD.gam21sum,SD.gam21[save.mods[num]])
      }
    } else {
      SD.gam21sum = list(SD.gam21sum,
                         paste(prov.name,": No such model for distancing = 0 | closures = 1 | lockdown = 1"))
    }
    SD.gam30 = NULL
    SD.gam30sum = list()
    if (length(cases100)>0) {
      SD.gam30 = NULL
      if (length(cases100)>10) {
        SD.gam30 = gam(as.numeric(as.character(Mobility)) ~ s(as.numeric(Date)),data=prov.df[cases100,])
      } else {
        SD.gam30 = gam(as.numeric(as.character(Mobility)) ~ s(as.numeric(Date),k=2),data=prov.df[cases100,])
      }
      SD.gam30sum = list(summary(SD.gam30))
      for (num in 1:length(save.mods)) {
        SD.gam30sum = list(SD.gam30sum,SD.gam30[save.mods[num]])
      }
    } else {
      SD.gam30sum = list(SD.gam30sum,
                         paste(prov.name,": No such model for distancing = 1 | closures = 0 | lockdown = 0"))
    }
    SD.gam31 = NULL
    SD.gam31sum = list()
    if (length(cases101)>0) {
      SD.gam31 = NULL
      if (length(cases101)>10) {
        SD.gam31 = gam(as.numeric(as.character(Mobility)) ~ s(as.numeric(Date)),data=prov.df[cases101,])
      } else {
        SD.gam31 = gam(as.numeric(as.character(Mobility)) ~ s(as.numeric(Date),k=2),data=prov.df[cases101,])
      }
      SD.gam31sum = list(summary(SD.gam31))
      for (num in 1:length(save.mods)) {
        SD.gam31sum = list(SD.gam31sum,SD.gam31[save.mods[num]])
      }
    } else {
      SD.gam31sum = list(SD.gam31sum,
                         paste(prov.name,": No such model for distancing = 1 | closures = 0 | lockdown = 1"))
    }
    SD.gam40 = NULL
    SD.gam40sum = list()
    if (length(cases110)>0) {
      SD.gam40 = NULL
      if (length(cases110)>10) {
        SD.gam40 = gam(as.numeric(as.character(Mobility)) ~ s(as.numeric(Date)),data=prov.df[cases110,])
      } else {
        SD.gam40 = gam(as.numeric(as.character(Mobility)) ~ s(as.numeric(Date),k=2),data=prov.df[cases110,])
      }
      SD.gam40sum = list(summary(SD.gam40))
      for (num in 1:length(save.mods)) {
        SD.gam40sum = list(SD.gam40sum,SD.gam40[save.mods[num]])
      }
    } else {
      SD.gam40sum = list(SD.gam40sum,
                         paste(prov.name,": No such model for distancing = 1 | closures = 1 | lockdown = 0"))
    }
    SD.gam41 = NULL
    SD.gam41sum = list()
    if (length(cases111)>0) {
      SD.gam41 = NULL
      if (length(cases111)>10) {
        SD.gam41 = gam(as.numeric(as.character(Mobility)) ~ s(as.numeric(Date)),data=prov.df[cases111,])
      } else {
        SD.gam41 = gam(as.numeric(as.character(Mobility)) ~ s(as.numeric(Date),k=2),data=prov.df[cases111,])
      }
      SD.gam41sum = list(summary(SD.gam41))
      for (num in 1:length(save.mods)) {
        SD.gam41sum = list(SD.gam41sum,SD.gam41[save.mods[num]])
      }
    } else {
      SD.gam41sum = list(SD.gam41sum,
                         paste(prov.name,": No such model for distancing = 1 | closures = 1 | lockdown = 1"))
    }
    
    ## RESULTS ANALYSIS ##
    combs = c("000","001","010","011","100","101","110","111")
    all.coefs.r = list(SD.gam10$coefficients,SD.gam11$coefficients,SD.gam20$coefficients,SD.gam21$coefficients,
                       SD.gam30$coefficients,SD.gam31$coefficients,SD.gam40$coefficients,SD.gam41$coefficients)
    all.res.r = list(summary(SD.gam10)[2][[1]],summary(SD.gam11)[2][[1]],summary(SD.gam20)[2][[1]],summary(SD.gam21)[2][[1]],
                     summary(SD.gam30)[2][[1]],summary(SD.gam31)[2][[1]],summary(SD.gam40)[2][[1]],summary(SD.gam41)[2][[1]])
    all.nmods.r = list(summary(SD.gam10)[13][[1]],summary(SD.gam11)[13][[1]],summary(SD.gam20)[13][[1]],summary(SD.gam21)[13][[1]],
                       summary(SD.gam30)[13][[1]],summary(SD.gam31)[13][[1]],summary(SD.gam40)[13][[1]],summary(SD.gam41)[13][[1]])
    SD.restest.r = vector("list",9)
    if (c < 5) {
      SD.resname = paste(all.demnames[c+r-1],"model summaries")
    } else {
      SD.resname = paste(all.demnames[dc+1],"model summaries")
    }
    SD.restest.r[1][[1]] = SD.resname
    for (m in 1:8) {
      if (!(is.null(all.coefs.r[m][[1]]))) {
        ncoefs = length(all.coefs.r[m][[1]])
        nmod = all.nmods.r[m][[1]]
        coefs = all.coefs.r[m][[1]][c(1:min(ncoefs,3))]
        ses = all.res.r[m][[1]][c(1:min(ncoefs,3))]
        maxpow = length(coefs)-1
        vals = cbind(coefs,coefs-ses,coefs+ses,ses)
        vals = cbind(0:9,vals)
        derivs = c(vals[1,2],0,ses[1])
        pow = 2
        while (pow <= maxpow+1) {
          this.deriv = vals[pow,2]*(pow-1)
          derivs = rbind(derivs,c(vals[pow,2],this.deriv,ses[pow]))
          pow = pow + 1
        }
        varssum = sum(ses^2)
        sessum = sqrt(varssum)
        calc = c()
        for (day in 1:nmod) {
          day.calc = 0 + derivs[1,2]
          for (n in 1:nrow(derivs)) {
            this.calc = derivs[n,2]*(day^n)
            day.calc = day.calc + this.calc
          }
          calc = append(calc,day.calc,length(calc))
        }
        res = cbind(calc,calc-rep(sessum,nmod),calc+rep(sessum,nmod),rep(sessum,nmod))
        res = cbind(c(1:nmod),res)
        colnames(res) <- c("Date","Local derivative by day","Lower","Upper","StdErr")
        summary = vector("list",4)
        resmean = mean(res[,1])
        resse = sqrt(sum(res[,4]^2)/nmod)
        resmeanCI = c(resmean,resmean-resse,resmean+resse,resse)
        resmeanCI = t(as.data.frame(resmeanCI))
        colnames(resmeanCI) <- c("DayMean","Lower","Upper","StdErr")
        summary[1][[1]] = resmeanCI
        quants = quantile(res[,1])
        meanmodequantileCI = quants[2:4]
        summary[2][[1]] = meanmodequantileCI
        startdate = res[1,]
        enddate = res[nmod,]
        dayn = paste("Day",nmod,":")
        dayn = gsub(" :",":",dayn)
        SD.restest.r[m+1][[1]] = list(paste("Model for case",combs[m]),
                                      list("Overall results:",res),list(list("Quantiles:",summary),list("Day 1:",startdate),list(dayn,enddate)))
      } else {
        nodat = paste("No data in this factor combination:",combs[m])
        SD.restest.r[m+1][[1]] = nodat
      }
      SD.newcountry[r+1][[1]] = SD.restest.r
      
      combs = c("000","001","010","011","100","101","110","111")
      subregions.r = SD.restest.r
      reg = c(c,r)
      for (n in 2:9) {
        nums = as.numeric(str_split(combs[n-1][1],"")[[1]])
        if (length(subregions.r[[n]])==3) {
          # There is such a model
          start = subregions.r[[n]][[3]][[2]][[1]][1] # start date
          start = gsub("Day ","",start)
          start = gsub(":","",start)
          start = as.numeric(start)
          
          subquantile = subregions.r[[n]][[3]][[1]][[2]][[1]] # 1 row, 4 col
          mndate = subquantile[1] # mean date
          meanval = mean(subquantile[2:3])
          subquantile = c(mndate,meanval,subquantile[2:4])
          
          end = subregions.r[[n]][[3]][[3]][[1]][1] # end date
          end = gsub("Day ","",end)
          end = gsub(":","",mndate)
          end = as.numeric(end)
          
          nreport = c(reg,nums,start,end,(end-start)/132,subquantile[2:5])
        } else {
          nreport = c(reg,nums,rep(NA,7))
        }
        regionREPORT = rbind(regionREPORT,nreport)
      }
    }
    countryREPORT = rbind(countryREPORT,regionREPORT)
    allshort = c()
    for (row in 1:nrow(countryREPORT)) {
      if (row%%64<9) {
        allshort = rbind(allshort,countryREPORT[row,])
      }
    }
    countryREPORT = as.data.frame(allshort)
    colnames(countryREPORT) = titles
  }
  SD.bycountry[c][[1]] = SD.newcountry
  
  allREPORTS = rbind(allREPORTS,countryREPORT)
  
  country.df$Distancing <- factor(country.df$Distancing)
  country.df$Closures <- factor(country.df$Closures)
  country.df$Lockdown <- factor(country.df$Lockdown)
  country.df$DateFactor <- factor(as.Date(country.df$Date))
  
  if (c==2) {
    country = "United States"
  } else if (c==5) {
    country = "United Kingdom"
  }
  
  if (c < 5) {
    
    country.covars.df = world.covars.df[which(world.covars.df$COUNTRY==country),]
    country.corrPlot = plot(country.covars.df[,c(3:5,7:13,15:17,19:21)])
    #corrTest.mat = country.covars.df[,c(3:5,7:13,15:17,19:21)]
    #ncorvar = ncol(corrTest.mat)
    #corr.mat = c()
    #for (var in c(1:ncorvar)) {
    #  var1 = as.numeric(as.character(corrTest.mat[,var]))
    #  corr.var = colnames(corrTest.mat)[var]
    #  for (corvar in c(1:ncorvar)) {
    #    var2 = as.numeric(as.character(corrTest.mat[,corvar]))
    #    this.corr = cov(var1,var2)/sqrt(var(var1)*var(var2))
    #    corr.var = append(corr.var,this.corr,length(corr.var))
    #  }
    #  corr.mat = rbind(corr.mat,corr.var)
    #}
    #for (row in 1:nrow(corr.mat)) {
    #  for (col in 2:ncol(corr.mat)) {
    #    corr.mat[row,col] = as.numeric(corr.mat[row,col])
    #    if (!(corr.mat[row,col]==1)) {
    #      corr.mat[row,col] = trunc(as.numeric(corr.mat[row,col])*1000)/1000
    #    }
    #  }
    #}
    #corr.mat = as.data.frame(corr.mat)
    #colnames(corr.mat) = c("",as.character(corr.mat[,1]))
    #rownames(corr.mat) = corr.mat[,1]
    #corr.mat = corr.mat[,-1]
    
    #isCorr.mat = c()
    # for (row in 1:nrow(corr.mat)) {
    #   row.corr = c()
    #   for (col in 1:ncol(corr.mat)) {
    #     if (row==col) {
    #       row.corr = append(row.corr,"Self",length(row.corr))
    #     } else if (as.numeric(as.character(corr.mat[row,col]))>0.75) {
    #       row.corr = append(row.corr,1,length(row.corr))
    #     } else if (as.numeric(as.character(corr.mat[row,col]))<(-0.75)) {
    #       row.corr = append(row.corr,-1,length(row.corr))
    #     } else {
    #       row.corr = append(row.corr,0,length(row.corr))
    #     }
    #   }
    #   isCorr.mat = rbind(isCorr.mat,row.corr)
    # }
    # colnames(isCorr.mat) = colnames(country.covars.df[c(3:5,7:13,15:17,19:20)])
    # rownames(isCorr.mat) = rownames(corr.mat)
    # count.1 = c()
    # count.0 = c()
    # count.neg1 = c()
    # for (row in 1:nrow(isCorr.mat)) {
    #   count.1 = append(count.1,length(which(isCorr.mat[row,]==1)),length(count.1))
    #   count.0 = append(count.1,length(which(isCorr.mat[row,]==0)),length(count.0))
    #   count.neg1 = append(count.neg1,length(which(isCorr.mat[row,]==-1)),length(count.neg1))
    # }
    # posorneg.corr = count.1+count.neg1
    # present = which(rownames(isCorr.mat)%in%c("Over65","PCSmk","PCObese","Lfedvote","Cfedvote","Rfedvote"))
    # isCorr.mat = cbind(isCorr.mat,count.1,count.0,count.neg1,posorneg.corr)
    # isCorr.mat = as.data.frame(isCorr.mat)
    # nat.keep = rownames(isCorr.mat)[sort(unique(c(which(posorneg.corr<3),which(c(1:16)%in%present))))]
    # nat.keep = nat.keep[-c(index(nat.keep,"Lfed",k=1,all=FALSE),index(nat.keep,"Lreg",k=1,all=FALSE))]
     
    country.df <- transform(country.df, ndate = as.numeric(Date),
                            nyear  = as.numeric(format(year(as.Date(country.df$Date)))), #as.numeric(format(country.df$Date, '%Y')),
                            nmonth = as.numeric(format(month(as.Date(country.df$Date)))), #as.numeric(format(country.df$Date, '%m')),
                            doy    = as.numeric(format(day(as.Date(country.df$Date))))) #as.numeric(format(country.df$Date, '%j')))
    country.df <- as.data.frame(country.df)
    #nameskeep = colnames(country.df)[sort(unique(c(which(colnames(country.df)%in%nat.keep),
    #                                               which(colnames(country.df)%in%
    #                                                       c("Cfed","Rfed","Cfeg","Rfeg",
    #                                                         "Lfedvote","Cfedvote","Rfedvote",
    #                                                         "Lregvote","Cregvote","Rregvote")))))]
    country.covars.df$Lfedvote = country.covars.df$Lfed
    country.covars.df$Cfedvote = country.covars.df$Cfed
    country.covars.df$Rfedvote = country.covars.df$Rfed
    country.covars.df$Lregvote = country.covars.df$Lreg
    country.covars.df$Cregvote = country.covars.df$Creg
    country.covars.df$Rregvote = country.covars.df$Rreg
    Lfed = c()
    Cfed = c()
    Rfed = c()
    Lreg = c()
    Creg = c()
    Rreg = c()
    for (row in 1:nrow(country.df)) {
      if (row%%132==1) {
        Lfed = append(Lfed,country.df$Lfed[row])
        Cfed = append(Lfed,country.df$Cfed[row])
        Rfed = append(Lfed,country.df$Rfed[row])
        Lreg = append(Lfed,country.df$Lreg[row])
        Creg = append(Lfed,country.df$Creg[row])
        Rreg = append(Lfed,country.df$Rreg[row])
      }
    }
    #levels = c()
    #for (n in 1:length(nameskeep)) {
    #  levels = append(levels,length(unique(country.df[,n])),length(levels))
    #}
    namesdrop = c("Popmill", "HospBeds1000", "Over65", "PerUrb", "HospBeds1000", "Over65",
                  "Cfed", "Rfed", "Lfedvote", "Cfedvote", "Rfedvote")
    #nameskeep = nameskeep[-namesdrop]
    #nameskeep = nameskeep[-c(index(nameskeep,"Phys1000",k=1,all=FALSE))]
    #nameskeep = nameskeep[-c(index(nameskeep,"Cfed",k=1,all=FALSE),index(nameskeep,"Cfedvote",k=1,all=FALSE))]
    nameskeep = c(nameskeep,"Lregvote","Cregvote","Rregvote")
    #smooth.name = paste("s(ndate,k=",min(levels[-namesdrop]),")")
    #smooth.name = gsub(" ","",smooth.name)
    
    keep.covars.df = country.df[,sort(unique(c(which(colnames(country.df)%in%nameskeep),
                                               which(colnames(country.df)%in%c("Mobility",
                                                                               "Date","Region","ndate")))))]
    keep.covars.df$Lfedvote = country.df$Lfedvote
    keep.covars.df$Cfedvote = country.df$Cfedvote
    keep.covars.df$Rfedvote = country.df$Rfedvote
    keep.covars.df$Lregvote = country.df$Lregvote
    keep.covars.df$Cregvote = country.df$Cregvote
    keep.covars.df$Rregvote = country.df$Rregvote
    
    # test.gls = gls(as.numeric(as.character(Mobility))~Date+
    #                  as.numeric(as.character(HospBeds1000))+
    #                  as.numeric(as.character(Over65))+
    #                  as.numeric(as.character(PerUrb))+
    #                  as.numeric(as.character(Lfedvote))+
    #                  as.numeric(as.character(Cfedvote))+
    #                  as.numeric(as.character(Rfedvote))+
    #                  as.numeric(as.character(Lregvote))+
    #                  as.numeric(as.character(Cregvote))+
    #                  as.numeric(as.character(Rregvote)),correlation=corARMA(form = ~1,p=0,q=1),
    #            data=country.df,control = list(singular.ok = TRUE))

    test.lme.ideol = lme4::lmer(
      as.numeric(as.character(Mobility)) ~ (1 | Region) + (1 | Date) +
        as.numeric(as.character(Lfedvote)) +
        as.numeric(as.character(Cfedvote)) +
        as.numeric(as.character(Rfedvote)) +
        as.numeric(as.character(Lregvote)) +
        as.numeric(as.character(Cregvote)) +
        as.numeric(as.character(Rregvote)) +
        as.numeric(as.character(Pdens)) +
        as.numeric(as.character(PerUrb)) +
        as.numeric(as.character(HospBeds1000)) +
        as.numeric(as.character(Popmill)) +
        Cfed + Rfed + Creg + Rreg,
      data = country.df
    )
    
    fixed.ideol = summary(test.lme.ideol)[10][[1]]
    random.ideol = as.data.frame(VarCorr(test.lme.ideol))[, c(1, 5)]
    estsI = rbind(fixed.ideol[c(2:7), c(1:2)], cbind(rep(NA, 3), random.ideol[, 2]))
    estsI = cbind(c("Fixed", "", "", "", "", "", "Random", "", ""), estsI)
    rownames(estsI) = c(
      "Lfedvote",
      "Cfedvote",
      "Rfedvote",
      "Lregvote",
      "Cregvote",
      "Rregvote",
      "Date",
      "Region",
      "Residual"
    )
    estsI[, c(2:3)] = round(as.numeric(estsI[, c(2:3)]), 4)
    estsI = as.data.frame(estsI)
    colnames(estsI) = c(paste(countries[c], ": Ideology Effects"), "Estimate", "Std. Err.")
    
    ncoef = nrow(coef(summary(test.lme.ideol)))
    res = sqrt(sum(residuals(test.lme.ideol)))/(ncoef-2)
    
    test.lme.health = lme4::lmer(
      as.numeric(as.character(Mobility)) ~ (1 | Region) + (1 | Date) +
        as.numeric(as.character(PCSmk)) +
        as.numeric(as.character(PCObese)) +
        as.numeric(as.character(Over65)) +
        as.numeric(as.character(MedIncomeUSD)) +
        as.numeric(as.character(Pdens)) +
        as.numeric(as.character(PerUrb)) +
        as.numeric(as.character(HospBeds1000)) +
        as.numeric(as.character(Popmill)) +
        Cfed + Rfed + Creg + Rreg,
      data = country.df
    )
    
    fixed.health = summary(test.lme.health)[10][[1]]
    random.health = as.data.frame(VarCorr(test.lme.health))[, c(1, 5)]
    estsH = rbind(fixed.health[c(4:6), c(1:2)], cbind(rep(NA, 3), random.health[, 2]))
    rownames(estsH) = c("%Smoking",
                        "%Obese",
                        "Age/Older Population",
                        "Date",
                        "Region",
                        "Residual")
    estsH = cbind(c("Fixed", "", "", "Random", "", ""), estsH)
    estsH[, c(2:3)] = round(as.numeric(estsH[, c(2:3)]), 4)
    estsH = as.data.frame(estsH)
    colnames(estsH) = c(paste(countries[c], ": Health & Age Effects"),
                        "Estimate",
                        "Std. Err.")
    
    ### TLSM
    # 
    # a = tslm(as.numeric(as.character(country.xts$Mobility))~as.numeric(country.xts$Date)+
    #            as.numeric(as.character(country.xts$Popmill))+
    #            as.numeric(as.character(country.xts$HospBeds1000))+
    #            as.numeric(as.character(country.xts$Over65))+
    #            as.numeric(as.character(country.xts$PerUrb))+factor(country.xts$Cfed)+factor(country.xts$Rfed)+
    #            as.numeric(as.character(country.xts$Lfedvote))+
    #            as.numeric(as.character(country.xts$Cfedvote))+
    #            as.numeric(as.character(country.xts$Rfedvote))+
    #            as.numeric(as.character(country.xts$Lregvote))+
    #            as.numeric(as.character(country.xts$Cregvote))+
    #            as.numeric(as.character(country.xts$Rregvote)),random=list(Region=~1),data=country.xts)
    
    #keep.covars.df$Positivity = worldtest.df$Positivity[c(((132*dc)+1):((132*(dc+nc-1))+132))]
    # SD.inla = inla(as.numeric(as.character(Mobility))~f(as.numeric(Date),model='ar1')+
    #                  f(factor(Region),model='iid')+#f(as.numeric(as.character(Positivity)),
    #                                                  #model='rw2')+
    #                  as.numeric(as.character(Popmill))+
    #                  as.numeric(as.character(HospBeds1000))+
    #                  as.numeric(as.character(Over65))+
    #                  as.numeric(as.character(PerUrb))+factor(Cfed)+factor(Rfed)+
    #                  as.numeric(as.character(Lfedvote))+
    #                  as.numeric(as.character(Cfedvote))+
    #                  as.numeric(as.character(Rfedvote))+
    #                  as.numeric(as.character(Lregvote))+
    #                  as.numeric(as.character(Cregvote))+
    #                  as.numeric(as.character(Rregvote)),
    #                control.inla = list(diagonal = 100, strategy = "gaussian",
    #                                    int.strategy = "eb"),
    #                     data=keep.covars.df)
    # 
    # nat.summ = list(SD.inla, summary(SD.inla), SD.inla$summary.fixed, SD.inla$summary.random[2])
    # nat.plot = plot(SD.inla)
    nat.summ = list(countries[c],test.lme.ideol,test.lme.health)
    SD.countrymods[c][[1]] = list(nat.summ, nat.plot)
  }
}

worldtest.df = transform(country.df, ndate = as.numeric(Date),
                         nyear  = as.numeric(format(year(as.Date(country.df$Date)))), #as.numeric(format(country.df$Date, '%Y')),
                         nmonth = as.numeric(format(month(as.Date(country.df$Date)))), #as.numeric(format(country.df$Date, '%m')),
                         nweek = as.numeric(format(week(as.Date(country.df$Date)))), #as.numeric(format(country.df$Date, '%w')),
                         doy    = as.numeric(format(day(as.Date(country.df$Date))))) #as.numeric(format(country.df$Date, '%j')))
worldtest.df <- as.data.frame(worldtest.df)

worldtest.xts = as.xts(worldtest.df,order.by=as.Date(worldtest.df$Date))
country.df = worldtest.df[which(worldtest.df$Countries=="Canada"),]
country.xts = worldtest.xts[which(worldtest.xts$Countries=="Canada"),]
can.SDlm = tslm(Mobility~factor(country.xts$Region)+
                  as.numeric(as.character(country.xts$Positivity))+
                  as.numeric(as.character(country.xts$Popmill))+
                  as.numeric(as.character(country.xts$HospBeds1000))+
                  as.numeric(as.character(country.xts$Over65))+
                  as.numeric(as.character(country.xts$PerUrb))+
                  factor(country.xts$Cfed)+factor(country.xts$Rfed)+
                  as.numeric(as.character(country.xts$Lfedvote))+
                  as.numeric(as.character(country.xts$Cfedvote))+
                  as.numeric(as.character(country.xts$Rfedvote))+
                  as.numeric(as.character(country.xts$Lregvote))+
                  as.numeric(as.character(country.xts$Cregvote))+
                  as.numeric(as.character(country.xts$Rregvote)),
                data=country.xts)

SD.global = gamm(as.numeric(as.character(country.df$Mobility))~
                   s(as.numeric(Date))+
                   as.numeric(as.character(country.df$Popmill))+
                   as.numeric(as.character(country.df$Phys1000))+
                   as.numeric(as.character(country.df$HospBeds1000))+
                   as.numeric(as.character(country.df$HealthSpendGDP))+
                   as.numeric(as.character(country.df$Over65))+
                   as.numeric(as.character(country.df$PerUrb))+
                   as.numeric(as.character(country.df$Pdens))+
                   as.numeric(as.character(country.df$MedIncomeUSD))+
                   as.numeric(as.character(country.df$PerHousehold))+
                   as.numeric(as.character(country.df$PCSmk))+
                   as.numeric(as.character(country.df$PCObese))+
                   as.numeric(as.character(country.df$PLCanc))+
                   as.numeric(as.character(country.df$Cfed))+
                   as.numeric(as.character(country.df$Rfed))+
                   as.numeric(as.character(country.df$Lfedvote))+
                   as.numeric(as.character(country.df$Cfedvote))+
                   as.numeric(as.character(country.df$Rfedvote)),
                 correlation=corAR1(form=~1|country.df$Date),method="REML",random=list(Region=~1),
                 data=country.df)

# Mobility across single-tier countries
UK.socdis = worldtest.df$Mobility[which(worldtest.df$Countries=="United Kingdom")]
diff.UK = diff(UK.socdis)
FR.socdis = worldtest.df$Mobility[which(worldtest.df$Countries=="France")]
diff.France = diff(FR.socdis)
SW.socdis = worldtest.df$Mobility[which(worldtest.df$Countries=="Sweden")]
diff.UK = diff(SW.socdis)

ca.names = c("Alberta","British Columbia","Manitoba","Nova Scotia","Ontario",
             "Quebec","Saskatchewan")
usa.names = all.demnames[8:58]
germ.names = all.demnames[59:74]
italy.names = all.demnames[75:95]

ca.socdis = c()
can.pops = world.covars.df$Popmill[which(world.covars.df$COUNTRY=="Canada")]
can.pops = as.numeric(as.character(can.pops))
us.socdis = c()
usa.pops = world.covars.df$Popmill[which(world.covars.df$COUNTRY=="United States")]
usa.pops = as.numeric(as.character(usa.pops))
de.socdis = c()
germ.pops = world.covars.df$Popmill[which(world.covars.df$COUNTRY=="Germany")]
germ.pops = as.numeric(as.character(germ.pops))
it.socdis = c()
italy.pops = world.covars.df$Popmill[which(world.covars.df$COUNTRY=="Italy")]
italy.pops = as.numeric(as.character(italy.pops))
for (day in 1:132) {
  this.CA = worldtest.df[which(worldtest.df$Countries=="Canada"),]
  CA.day = 0
  r = 1
  for (r in 1:7) {
    name = ca.names[r]
    this.reg.CA = worldtest.df[which(worldtest.df$Region==name),14][day,]
    this.reg.CA = as.numeric(as.character(this.reg.CA))
    pop = can.pops[r]
    this.reg.CA = this.reg.CA*pop
    CA.day = CA.day + this.reg.CA
  }
  ca.socdis = append(ca.socdis,CA.day,length(ca.socdis))
}
for (day in 1:132) {
  this.US = worldtest.df[which(worldtest.df$Countries=="United States"),]
  US.day = 0
  r = 1
  for (r in 1:51) {
    name = usa.names[r]
    pop = usa.pops[r]
    this.reg.US = worldtest.df[which(worldtest.df$Region==name),14][day,]
    this.reg.US = as.numeric(as.character(this.reg.US))
    this.reg.US = this.reg.US*pop
    US.day = US.day + this.reg.US
  }
  us.socdis = append(us.socdis,US.day,length(us.socdis))
}
for (day in 1:132) {
  this.DE = worldtest.df[which(worldtest.df$Countries=="Germany"),]
  DE.day = 0
  r = 1
  for (r in 1:16) {
    name = germ.names[r]
    pop = germ.pops[r]
    this.reg.DE = worldtest.df[which(worldtest.df$Region==name),14][day,]
    this.reg.DE = as.numeric(as.character(this.reg.DE))
    this.reg.DE = this.reg.DE*pop
    DE.day = DE.day + this.reg.DE
  }
  de.socdis = append(de.socdis,DE.day,length(de.socdis))
}
for (day in 1:132) {
  this.IT = worldtest.df[which(worldtest.df$Countries=="Italy"),]
  IT.day = 0
  r = 1
  for (r in 1:21) {
    name = italy.names[r]
    pop = italy.pops[r]
    this.reg.IT = worldtest.df[which(worldtest.df$Region==name),14][day,]
    this.reg.IT = as.numeric(as.character(this.reg.IT))
    this.reg.IT = this.reg.IT*pop
    IT.day = IT.day + this.reg.IT
  }
  it.socdis = append(it.socdis,IT.day,length(it.socdis))
}
ca.socdis = ca.socdis/sum(can.pops)
us.socdis = us.socdis/sum(usa.pops)
de.socdis = de.socdis/sum(germ.pops)
it.socdis = it.socdis/sum(italy.pops)

world.socdis = ggplot(data=NULL,aes(x=date))+
  geom_line(data=NULL,aes(x=date,y=UK.socdis),col="red",linetype=7,size=2)+
  geom_line(data=NULL,aes(x=date,y=FR.socdis),col="blue",linetype=7,size=2)+
  geom_line(data=NULL,aes(x=date,y=SW.socdis),col="green",linetype=7,size=2)+
  geom_line(data=NULL,aes(x=date,y=ca.socdis),col="purple",size=2)+
  geom_line(data=NULL,aes(x=date,y=us.socdis),col="grey",size=2)+
  geom_line(data=NULL,aes(x=date,y=de.socdis),col="yellow",size=2)+
  geom_line(data=NULL,aes(x=date,y=it.socdis),col="turquoise",size=2)+
  ylab("Reduction in Social Mobility, February 22 to July 4 2020")+
  ggtitle("Social distancing for select countries, national levels")

unitary.poli = rbind(
  worldtest.df[which(worldtest.df$Countries=="United Kingdom"),c(27:32,34:39)][1,],
  worldtest.df[which(worldtest.df$Countries=="France"),c(27:32,34:39)][1,],
  worldtest.df[which(worldtest.df$Countries=="Sweden"),c(27:32,34:39)][1,]
)

unitary.fed = unitary.poli[,1:6]
colnames(unitary.fed) = rep("",6)
unitary.reg = unitary.poli[,7:12]
colnames(unitary.reg) = rep("",6)
unitary = rbind(unitary.fed,unitary.reg)
colnames(unitary) = c("Lgovt","Cgovt","Rgovt","Lvote","Cvote","Rvote")

View(unitary.poli)

# Social distancing health effects
can.covars = world.covars.df[which(world.covars.df$COUNTRY=="Canada"),]
usa.covars = world.covars.df[which(world.covars.df$COUNTRY=="United States"),]
germ.covars = world.covars.df[which(world.covars.df$COUNTRY=="Germany"),]
italy.covars = world.covars.df[which(world.covars.df$COUNTRY=="Italy"),]

world.smk = ggplot(data=world.covars.df[1:98,],
                   aes(x=as.numeric(as.character(PCSmk))))+geom_density()
can.smk = ggplot(data=can.covars,
                 aes(x=as.numeric(as.character(PCSmk))))+geom_density()
usa.smk = ggplot(data=usa.covars,
                 aes(x=as.numeric(as.character(PCSmk))))+geom_density()
germ.smk = ggplot(data=germ.covars,
                  aes(x=as.numeric(as.character(PCSmk))))+geom_density()
italy.smk = ggplot(data=italy.covars,
                   aes(x=as.numeric(as.character(PCSmk))))+geom_density()
world.smk = ggplot(data=world.covars.df[1:98,],
       aes(x=as.numeric(as.character(PCSmk))))+geom_density()
all.smk = ggplot(data=can.covars,
                   aes(x=as.numeric(as.character(PCSmk))))+
  geom_density(col="purple",size=2)+
  geom_density(data=usa.covars,aes(x=as.numeric(as.character(PCSmk))),
               col="grey",size=2)+
  geom_density(data=germ.covars,aes(x=as.numeric(as.character(PCSmk))),
               col="yellow",size=2)+
  geom_density(data=italy.covars,aes(x=as.numeric(as.character(PCSmk))),
               col="turquoise",size=2)+xlab("% Smokers")+
  ggtitle("Density of smoking rates across federal countries")


can.obese = ggplot(data=can.covars,
                 aes(x=as.numeric(as.character(PCObese))))+geom_density()
usa.obese = ggplot(data=usa.covars,
                 aes(x=as.numeric(as.character(PCObese))))+geom_density()
germ.obese = ggplot(data=germ.covars,
                  aes(x=as.numeric(as.character(PCObese))))+geom_density()
italy.obese = ggplot(data=italy.covars,
                   aes(x=as.numeric(as.character(PCObese))))+geom_density()
all.obese = ggplot(data=can.covars,
                 aes(x=as.numeric(as.character(PCObese))))+
  geom_density(col="purple",size=2)+
  geom_density(data=usa.covars,aes(x=as.numeric(as.character(PCObese))),
               col="grey",size=2)+
  geom_density(data=germ.covars,aes(x=as.numeric(as.character(PCObese))),
               col="yellow",size=2)+
  geom_density(data=italy.covars,aes(x=as.numeric(as.character(PCObese))),
               col="turquoise",size=2)+xlab("% Obese")+
  ggtitle("Density of obesity rates across federal countries")

world.old = ggplot(data=world.covars.df[1:98,],
                   aes(x=as.numeric(as.character(Over65))))+geom_density()
can.old = ggplot(data=can.covars,
                 aes(x=as.numeric(as.character(Over65))))+geom_density()
usa.old = ggplot(data=usa.covars,
                 aes(x=as.numeric(as.character(Over65))))+geom_density()
germ.old = ggplot(data=germ.covars,
                  aes(x=as.numeric(as.character(Over65))))+geom_density()
italy.old = ggplot(data=italy.covars,
                   aes(x=as.numeric(as.character(Over65))))+geom_density()
all.old = ggplot(data=can.covars,
                 aes(x=as.numeric(as.character(Over65))))+
  geom_density(col="purple",size=2)+
  geom_density(data=usa.covars,aes(x=as.numeric(as.character(Over65))),
               col="grey",size=2)+
  geom_density(data=germ.covars,aes(x=as.numeric(as.character(Over65))),
               col="yellow",size=2)+
  geom_density(data=italy.covars,aes(x=as.numeric(as.character(Over65))),
               col="turquoise",size=2)+xlab("% Over 65")+
  ggtitle("Density of senior citizens across federal countries")



# SD Plots for effects
ideol.USorted = c()
for (row in 1:6) {
  ideol.USorted = rbind(ideol.USorted,sort(ideol.U[row,]))
}
colnames(ideol.USorted) = NULL

bindSort = function(n) {
  rbind(ideol.LSorted[n,],ideol.estSorted[n,],ideol.USorted[n,])
}

row = rownames(ideol.est)
for (n in 1:6) {
  matplot(t(bindSort(n)),type='l',ylab = paste(row[n],"trend"))
}

matplot(rbind(t(bindSort(1)),t(bindSort(2)),t(bindSort(3))),type='l')


SD.subDate = c()
SD.countryFixed = c()
fixed.types = c("Age","Health","Govt","Ideol","Positivity") # note Govt is a factor
SD.countryRandom = c()
random.types = c("Date","Region")
for (c in 1:4) {
  for (r in 1:ncountries[c]) {##### Variance maybe?
    SD.subdate = rbind(SD.subdate,c(c,r,SD.bycountry[[c]][[r+1]]$summary.fixed))
  }
  country.fixed = SD.countrymods[c][[1]][[1]][[1]]$summary.fixed
  country.random = SD.countrymods[c][[1]][[1]][[1]]$summary.random
  fixed = rep(c,13) # add by row?
  for (eff in 1:nrow(country.fixed)) {
    this.eff = rownames(country.fixed)[eff]
    if (this.eff=="Over65") {
      effect.name = fixed.types[1]
      rows = c(1)
      ...
    } else if (this.eff%in%c("PCSmk","PCObese")) {
      effect.name = fixed.types[2]
      rows = c(1:2)
      ...
    } else if (this.eff%in%c("Cfed","Rfed","Creg","Rreg")) {
      effect.name = fixed.types[3]
      rows = c(1:4)
      ...
    } else if (this.eff%in%c("Lfedvote","Cfedvote","Rfedvote","Lregvote","Cregvote","Rregvote")) {
      effect.name = fixed.types[4]
      rows = c(1:6)
      ...
    }
    fixed = rbind(fixed,f(eff),length(fixed))
  }
  random = c()
  for (eff in 1:nrow(country.random)) {
    random = rbind(random,f(eff),length(random))
  }
  
}
ideol.d = SD.countryEffects[which(SD.countryEffects$type==Ideol)]
ideol.plot = ggplot(data = ideol.d, aes(x = Region, y = Effect)) +
  geom_boxplot(aes(fill = Ideol), width = 0.8) + theme_bw()

#Masks: cycle through i in nomasks and  j in yesmasks
Mask.bycountry = vector('list',length=7)
mask.covars.country = colnames(worldtest.df)[c(3,9,10,11,13,14,15,16:19,22:27,29:34)]
mask.weights.country = c()
c <- 1
dc <- 0

while (c <= 7) {
  nc = ncountries[c]
  mask.weights.country = c()
  
  # For some reason, 
  country.df <- worldtest.df[c(((132*(dc+1))+1):((132*(dc+nc-1))+132)),]
  country.covars = world.covars.df[which(world.covars.df$nCountry==c),]
  country.df <- transform(country.df, ndate = as.numeric(Date),
                          nyear  = as.numeric(format(year(as.Date(country.df$Date)))), #as.numeric(format(country.df$Date, '%Y')),
                          nmonth = as.numeric(format(month(as.Date(country.df$Date)))), #as.numeric(format(country.df$Date, '%m')),
                          doy    = as.numeric(format(day(as.Date(country.df$Date))))) #as.numeric(format(country.df$Date, '%j')))
  country.df <- as.data.frame(country.df)
  
  yesMasks = c()
  noMasks = c()
  ndays = as.numeric(country.df$Date)
  for (r in 1:nc) {
    prov.df = country.df[index(country.df$Region,r,k=1,all=TRUE),]
    region.change = 72 # after April 22nd
    region.ifMasks = sum(as.numeric(as.character(prov.df$Masks)))
    region.yes = ((region.ifMasks/region.change)>0.2) # over 14 days of masks or two weeks
    if (region.yes==TRUE) {
      yesMasks = append(yesMasks,r,length(yesMasks))
    } else {
      noMasks = append(noMasks,r,length(noMasks))
    }
    # Region mask models here
  }
  
  if (c < 5) {
    Mask.newcountry = vector('list',length=nc)
    Mask.newcountry[1][[1]] = paste("Models for ",countries[c])
    mask.weights.country = c()
    regnames = c("Date","Country","Region","Positivity","ifMasks","ndate","nmonth","doy")
    for (i in noMasks) {
      for (j in yesMasks) {
        iname = paste(all.demnames[c+i-1])
        inames = colnames(i.tsdf)
        for (n in 1:length(inames)) {
          this.name = paste(iname,inames[n])
          this.name = gsub(" ","",this.name)
          inames[n] = this.name
        }
        jname = paste(all.demnames[c+j-1])
        jnames = colnames(j.tsdf)
        for (n in 1:length(jnames)) {
          this.name = paste(jname,jnames[n])
          this.name = gsub(" ","",this.name)
          jnames[n] = this.name
        }
        ij.tsdf = c(country.df$Positivity[index(country.df$Region,i,k=1,all=TRUE)],
                    country.df$Positivity[index(country.df$Region,j,k=1,all=TRUE)])
        ij.country = rep(1,264)
        ij.region = c(rep(i,132),rep(j,132))
        ij.date = c(country.df$Date[index(country.df$Region,i,k=1,all=TRUE)],
                    country.df$Date[index(country.df$Region,j,k=1,all=TRUE)])
        ij.ndate = c(country.df$ndate[index(country.df$Region,i,k=1,all=TRUE)],
                     country.df$ndate[index(country.df$Region,j,k=1,all=TRUE)])
        ij.nmonth = c(country.df$nmonth[index(country.df$Region,i,k=1,all=TRUE)],
                      country.df$nmonth[index(country.df$Region,j,k=1,all=TRUE)])
        ij.doy = c(country.df$doy[index(country.df$Region,i,k=1,all=TRUE)],
                   country.df$doy[index(country.df$Region,j,k=1,all=TRUE)])
        ij.tsdf = cbind(ij.date,ij.country,ij.region,ij.tsdf,ij.ndate,ij.nmonth,ij.doy)
        ij.mask = 1*(ij.region%in%yesMasks)
        ij.tsdf = cbind(ij.tsdf,ij.mask)
        
        colnames(ij.tsdf) = c(inames,jnames)
        posi = paste(all.demnames[c+i-1],"Positivity")
        posi = gsub(" ","",posi)
        posj = psate(all.demnames[c+j-1],"Positivity")
        posj = gsub(" ","",posj)
        
        Mask.c.ij ~ gam(posj ~ posi+s(as.numeric(Date),by=ij.mask),
                        correlation=corAR1(form=~1|Date),
                        data=ij.tsdf)
        
        for (covar in colnames(country.covars)) {
          this.covar.i = world.covars.df$covar[c+i-1] # How to get this pasted?
          this.covar.j = world.covars.df$covar[c+j-1]
          max.covar = max(country.covars.df$covar)
          wgt.ij = sqrt((this.covar.i-this.covar.j/max.covar)^2)
          this.ij = c(c,i,j,covar,this.covar.i,this.covar.j,wgt.ij,(1/wgt.ij))
          mask.weights.country = rbind(mask.weights.country,this.ij)
        }
        
        # these are normalized to per 1000 people
        Mask.c.ijmod = vector('list',length=2)
        mask.c.B.ijmod = Mask.c.ij$coef[2]
        mask.c.B.ij = cbind(mask.c.B.ijmod[,1], mask.c.B.ijmod[,1] - mask.c.B.ijmod[,2],
                            mask.c.B.ijmod[,1] + mask.c.B.ijmod[,2])
        Mask.c.ijmod[1][[1]] = Mask.c.ij$fixed.coefficients
        Mask.c.ijmod[2][[1]] = Mask.c.ij$mixed.effects
        Mask.country = list(Mask.country,Mask.c.ij)
        Mask.bycountry = list(Mask.bycountry, list(Mask.country,
                                                   list(Mask.country$fixed.coefficients,
                                                        Mask.country$mixed.effects)))
      }
    }
  }
  c <- c + 1
  dc <- dc + nc
}
# Normalized B over Bij ??? typical value
Mask.aggregate = gamm(yesmaskij ~ s(nomaskij) + s(Pos) + s(CFR) + covariates, random = ~1|Country)
Mask.typical = gam(yesmask.all ~ s(nomask.all) + s(Pos) + s(CFR) + covariates)
Mask.global = vector("list",length=98) # combine all of the sub-national models?
# For USA: Do propensity score analysis
# Make sure to include population in covariates!!!!!!!!!!!!!!!!!!!!!!
# China estimates:

can.maskplot <- ggplot(data=NULL,aes(x=as.Date(can.df$Date),y=can.noMaskChi1000/sum(can.nopops)))+
  geom_line(col="red")+
  geom_line(data=NULL,aes(x=as.Date(can.df$Date),y=can.yesMaskChi1000/sum(can.yespops)),col="blue")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[59])),col="purple")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[105])),col="green")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[105+14])),col="yellow green")

can.nomaskplot <- ggplot(data=NULL,aes(x=as.Date(can.df$Date),
                                       y=can.noMaskChi1000/sum(can.nopops)))+geom_line(col="red")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[59])),col="purple")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[105])),col="green")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[105+14])),col="yellow green")

can.yesmaskplot <- ggplot(data=NULL,aes(x=as.Date(can.df$Date),
                                        y=can.yesMaskChi1000/sum(can.nopops)))+geom_line(col="blue")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[59])),col="purple")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[105])),col="green")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[105+14])),col="yellow green")

can.nomaskmeanplot <- ggplot(data=NULL,aes(x=as.Date(can.df$Date[-c(1:6)]),
                                           y=rollmean(can.noMaskChi1000,k=7)/sum(can.nopops)))+geom_line(col="red")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[59])),col="purple")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[105])),col="green")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[105+14])),col="yellow green")

can.yesmaskmeanplot <- ggplot(data=NULL,aes(x=as.Date(can.df$Date[-c(1:6)]),
                                            y=rollmean(can.yesMaskChi1000,k=7)/sum(can.nopops)))+geom_line(col="blue")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[59])),col="purple")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[105])),col="green")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[105+14])),col="yellow green")


can.diffmaskplot <- ggplot(data=NULL,aes(x=as.Date(can.df$Date[-1]),
                                         y=diff(can.noMaskChi1000)/sum(can.nopops)))+geom_line(col="red")+
  geom_line(data=NULL,aes(x=as.Date(can.df$Date[-1]),y=diff(can.yesMaskChi1000)/sum(can.yespops)),
            col="blue")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[59])),col="purple")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[105])),col="green")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[105+14])),col="yellow green")

can.nodiffplot <- ggplot(data=NULL,aes(x=as.Date(can.df$Date[-1]),
                                       y=diff(can.noMaskChi1000)/sum(can.nopops)))+geom_line(col="red")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[59])),col="purple")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[105])),col="green")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[105+14])),col="yellow green")

can.yesdiffplot <- ggplot(data=NULL,aes(x=as.Date(can.df$Date[-1]),
                                        y=diff(can.yesMaskChi1000)/sum(can.nopops)))+geom_line(col="blue")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[59])),col="purple")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[105])),col="green")+
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[105+14])),col="yellow green")

ontario.maskplot <- ggplot(data=NULL,aes(x=as.Date(can.df$Date[-c(1:6)]),
                                         y=rollmean(as.numeric(
                                           can.df$OntarioDailyCases),k=7)/can.pops[5]))+
  geom_line(col="blue")
ontario.maskplot + geom_line(data=NULL,aes(x=as.Date(can.df$Date[-c(1:6)]),
                                           y=rollmean(as.numeric(
                                             can.df$AlbertaDailyCases),k=7)/can.pops[1]),
                             col="red") +
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[99])),col="yellow") +
  geom_vline(xintercept=as.numeric(as.Date(can.df$Date[99+14])),col="green")

# Country comparative plots

install.packages("devtools")
library(devtools)
devtools::install_github("kassambara/easyGgplot2")
library(easyGgplot2)
install.packages("vctrs","colorspace","testthat")
library(vctrs)
library(colorspace)
library(testthat)
install.packages("tseries")
library(tseries)

install.packages("glmnet")
library(glmmnet)
install.packages("broom")
library(broom)
install.packages("glmmTMB")
library(glmmTMB)

#g1can <- ggplot(my_dataframe, aes(x=country.covars.df$, y=c(0))) + geom_point()
can.mobility = c()
nc = ncountries[1]
cols.can = c("orange","yellow","green","blue","purple","brown")
for (i in 1:nc) {
  prov.mobility = as.numeric(as.character(worldtest.df$Mobility[c(((132*(i-1))+1):((132*(i-1))+132))]))
  can.mobility = cbind(can.mobility,prov.mobility)
}
colnames(can.mobility) = rep("Value",nc)
date = worldtest.df$Date[c(1:132)]
colours = c(colours[2:6],"brown")
can.mobplot = ggplot(data=NULL,aes(x=worldtest.df$Date[c(1:132)],y=can.mobility[,1])) + geom_line(col="red")
#i = 2
#while (i<nc) {
#  can.mobplot = can.mobplot + geom_line(data=NULL,aes(x=date,y=can.mobility[,i]),col=cols.can[i-1])
#  i = i + 1
#}

###
can.mobplot = can.mobplot + geom_line(data=NULL,aes(x=date,y=can.mobility[,2]),col=cols.can[1]) +
  geom_line(data=NULL,aes(x=date,y=can.mobility[,3]),col=cols.can[2]) +
  geom_line(data=NULL,aes(x=date,y=can.mobility[,4]),col=cols.can[3]) +
  geom_line(data=NULL,aes(x=date,y=can.mobility[,5]),col=cols.can[4]) +
  geom_line(data=NULL,aes(x=date,y=can.mobility[,6]),col=cols.can[5]) +
  geom_line(data=NULL,aes(x=date,y=can.mobility[,7]),col=cols.can[6]) +
  ggtitle("Canada: Reduction in mobility due to social distancing")

can.mobplot <- can.mobplot + scale_color_manual(name="Legend",values=c(ca.names=colours))

rep.demnames = c()
all.names = c(all.demnames,c(rep("China",34)))
for (name in all.names) {
  rep.demnames = append(rep.demnames,rep(name,132),length(rep.demnames))
}
worldtest.df$Region = rep.demnames
worldtest.df$Countries = c(rep("Canada",(7*132)),rep("USA",(51*132)),rep("Germany",(16*132)),
                           rep("Italy",(21*132)),rep("UK",132),rep("France",132),
                           rep("Sweden",132))
worldtest.df$ncountry = c(rep(1,(7*132)),rep(2,(51*132)),rep(3,(16*132)),
                          rep(4,(21*132)),rep(5,132),rep(6,132),
                          rep(7,132))
worldtest.df$nregion = c(1:98)
colours = c("red",colours)
df = as.data.frame(cbind(as.Date(prov.df$Date),can.mobility))
colnames(df) = c("Date",all.demnames[1:ncountries[1]])
can.mobplot = ggplot(data=df,aes(x=as.Date(df$Date)))+geom_line(aes(y=df$Alberta),col=colours[1])+
  geom_line(aes(y=df$`British Columbia`),col=colours[2])+
  geom_line(aes(y=df$Manitoba),col=colours[3])+
  geom_line(aes(y=df$`Nova Scotia`),col=colours[4])+
  geom_line(aes(y=df$Ontario),col=colours[5])+
  geom_line(aes(y=df$Quebec),col=colours[6])+
  geom_line(aes(y=df$Saskatchewan),col=colours[7])+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  xlab("Days since Feb 22") +
  ylab("Reduction in mobility due to social distancing") +
  ggtitle("Social distancing in Canada, Wave 1") +
  scale_colour_discrete(name  ="Province",breaks=all.demnames[1:ncountries[1]],
                        labels=all.demnames[1:ncountries[1]])

matplot(df[,-ncol(df)], col=c(1:7), type='l', lty=1, ylim=range(df[,-ncol(df)]), axes=FALSE)
axis(1, as.numeric(df$Date), format(df$Date, "%m"))
axis(2)
box() #- to make it look "as usual"
legend('topright', names(df), col=1:ncol(df), lty=1, cex=.65)

# Mobility statistics:
long.dates = dates
dates = long.dates[c(1:132)]

max.diff = c() #1
min.mob = c() #2
may.drop = c() #3
june.drop = c() #4
max.jun = c() #5
end.jul = c() #6
up.or.down.big = c() #7
up.or.down.small = c() #8
no.drops = c() #9
length.masks = c() #10
length.Distancing = c() #11
length.Closures = c() #12
length.Lockdown = c() #13

for (i in 1:nc) {
  this.diff = c(can.mobility[1,i],diff(can.mobility[,i]))
  this.1 = max(this.diff)
  this.1 = c(this.1,index(this.diff,this.1,k=1,all=FALSE),dates[index(this.diff,this.1,k=1,all=FALSE)])
  max.diff = rbind(max.diff,this.1)
  this.2 = min(can.mobility[,i])
  this.2 = c(this.2,index(can.mobility[,i],this.2,k=1,all=FALSE),dates[index(can.mobility[,i],this.2,k=1,all=FALSE)])
  min.mob = rbind(min.mob,this.2)
  this.3 = min(can.mobility[,i][which(month(dates)==5)])
  is.this.3 = identical((this.3==0),TRUE)
  if (is.this.3==TRUE) {
    this.3 = c(this.3,index(can.mobility[,i],this.3,k=1,all=FALSE),dates[index(can.mobility[,i],this.3,k=1,all=FALSE)])
  } else {
    this.3 = c("No drop in May", "N/A", "N/A")
  }
  may.drop = rbind(may.drop,this.3)
  this.4 = min(can.mobility[,i][which(month(dates)==6)])
  is.this.4 = identical((this.4==0),TRUE)
  if (is.this.3==TRUE) {
    this.4 = c(this.4,index(can.mobility[,i],this.4,k=1,all=FALSE),dates[index(can.mobility[,i],this.4,k=1,all=FALSE)])
  } else {
    this.4 = c("No drop in June", "N/A", "N/A")
  }
  june.drop = rbind(june.drop,this.4)
  this.5 = max(can.mobility[,i][which(month(dates)==6)])
  this.5 = c(this.5,index(can.mobility[,i],this.5,k=1,all=FALSE),dates[index(can.mobility[,i],this.5,k=1,all=FALSE)])
  this.6 = this.diff[which(dates=="2020-06-30")]
  this.6 = c(this.6,index(this.diff,this.6,k=1,all=FALSE),"2020-06-30")
  end.jul = rbind(end.jul,this.6)
  low = index(dates,"2020-06-15",k=1,all=FALSE)
  high = index(dates,"2020-07-04",k=1,all=FALSE)
  this.7 = mean(diff(can.mobility[,i][which(dates%in%dates[c(low:high)])]))
  half.big = round((low+high)/2)
  this.7 = c(this.7,half.big,"Start:2020-06-15; End:2020-07-04")
  up.or.down.big = rbind(up.or.down.big,this.7)
  med = index(dates,"2020-06-28",k=1,all=FALSE)
  this.8 = mean(diff(can.mobility[,i][which(dates%in%dates[c(med:high)])]))
  half.small = round((med+high)/2)
  this.8 = c(this.8,half.small,"2020-06-28:-:2020-07-04")
  up.or.down.small = rbind(up.or.down.big,this.8)
  this.9 = length(index(this.diff,0,k=1,all=TRUE))
  this.9 = c(this.9,index(this.diff,0,k=1,all=TRUE),paste(dates[index(this.diff,0,k=1,all=TRUE)],collapse=":-:"))
  no.drops = rbind(no.drops,this.9)
  prov.masks = worldtest.df[which(worldtest.df$Region==i),]$Masks
  mask.dates = dates[index(prov.masks,1,k=1,all=TRUE)]
  this.masks = c(sum((as.numeric(as.character(prov.masks)))),mask.dates[1],mask.dates[length(mask.dates)])
  length.masks = rbind(length.masks,this.masks)
  prov.Distancing = worldtest.df[which(worldtest.df$Region==i),]$Distancing
  distancing.dates = dates[index(prov.Distancing,1,k=1,all=TRUE)]
  this.Distancing = c(sum((as.numeric(as.character(prov.Distancing)))),distancing.dates[1],distancing.dates[length(distancing.dates)])
  length.Distancing = rbind(length.Distancing,this.Distancing)
  prov.Closures = worldtest.df[which(worldtest.df$Region==i),]$Closures
  closures.dates = dates[index(prov.Closures,1,k=1,all=TRUE)]
  this.Closures = c(sum((as.numeric(as.character(prov.Closures)))),closures.dates[1],closures.dates[length(closures.dates)])
  length.Closures = rbind(length.Closures,this.Closures)
  prov.Lockdown = worldtest.df[which(worldtest.df$Region==i),]$Lockdown
  lockdown.dates = dates[index(prov.Lockdown,1,k=1,all=TRUE)]
  this.Lockdown = c(sum((as.numeric(as.character(prov.Lockdown)))),lockdown.dates[1],lockdown.dates[length(lockdown.dates)])
  length.Lockdown = rbind(length.Lockdown,this.Lockdown)
}

# Party tests
corr = function(x,y) {
  # Return the correlation between x and y.
  corr = var(x,y)/sqrt(var(x)*var(y))
  corr
}

aF = as.numeric(as.character(country.df$Lfedvote))
aR = as.numeric(as.character(country.df$Lregvote))
bF = as.numeric(as.character(country.df$Cfedvote))
bR = as.numeric(as.character(country.df$Cregvote))
cF = as.numeric(as.character(country.df$Rfedvote))
cR = as.numeric(as.character(country.df$Rregvote))
corr(aF,aR)
corr(bF,bR)
corr(cF,cR)

ca.df = worldtest.df[which(worldtest.df$Countries=="Canada"),]
ab.df = ca.df[which(ca.df$Region=="Alberta"),]
bc.df = ca.df[which(ca.df$Region=="British Columbia"),]
mb.df = ca.df[which(ca.df$Region=="Manitoba"),]
ns.df = ca.df[which(ca.df$Region=="Nova Scotia"),]
on.df = ca.df[which(ca.df$Region=="Ontario"),]
qc.df = ca.df[which(ca.df$Region=="Quebec"),]
sk.df = ca.df[which(ca.df$Region=="Saskatchewan"),]
canplot1 = ggplot(data=worldtest.df,aes(x=as.Date(date)))+
  geom_point(data=ab.df,aes(y=log(as.numeric(as.character(can.bigger[which(can.bigger$Region==1),]$DailyCases)))))+
  geom_line(data=ab.df,aes(y=log(as.numeric(as.character(can.bigger[which(can.bigger$Region==1),]$DailyCases)))),col="red")+
  #geom_vline(data=ab.df,xintercept=as.Date(index(ab.df$Distancing,1,k=1,all=FALSE),origin="2020-02-22"),col="red")+
  #geom_vline(data=ab.df,xintercept=as.Date(index(ab.df$Distancing,1,k=-1,all=FALSE),origin="2020-02-22"),col="red")+
  geom_point(data=bc.df,aes(y=log(as.numeric(as.character(can.bigger[which(can.bigger$Region==2),]$DailyCases)))))+
  geom_line(data=bc.df,aes(y=log(as.numeric(as.character(can.bigger[which(can.bigger$Region==2),]$DailyCases)))),col="orange")+
  #geom_vline(data=bc.df,xintercept=as.Date(index(bc.df$Distancing,1,k=1,all=FALSE),origin="2020-02-22"),col="orange")+
  #geom_vline(data=bc.df,xintercept=as.Date(index(bc.df$Distancing,1,k=-1,all=FALSE),origin="2020-02-22"),col="orange")+
  geom_point(data=mb.df,aes(y=log(as.numeric(as.character(can.bigger[which(can.bigger$Region==3),]$DailyCases)))))+
  geom_line(data=mb.df,aes(y=log(as.numeric(as.character(can.bigger[which(can.bigger$Region==3),]$DailyCases)))),col="yellow green")+
  geom_vline(data=mb.df,xintercept=as.Date(index(mb.df$Distancing,1,k=1,all=FALSE),origin="2020-02-22"),col="yellow green")+
  geom_text(aes(x=as.Date(index(mb.df$Distancing,1,k=1,all=FALSE),origin="2020-02-22"),
                label="\nManitoba Social Distancing Start", y=200), angle=90, text=element_text(size=11))+
  #geom_vline(data=mb.df,xintercept=as.Date(index(mb.df$Distancing,1,k=-1,all=FALSE),origin="2020-02-22"),col="yellow green")+
  geom_point(data=ns.df,aes(y=log(as.numeric(as.character(can.bigger[which(can.bigger$Region==4),]$DailyCases)))))+
  geom_line(data=ns.df,aes(y=log(as.numeric(as.character(can.bigger[which(can.bigger$Region==4),]$DailyCases)))),col="green")+
  #geom_vline(data=ns.df,xintercept=as.Date(index(ns.df$Distancing,1,k=1,all=FALSE),origin="2020-02-22"),col="green")+
  #geom_vline(data=ns.df,xintercept=as.Date(index(ns.df$Distancing,1,k=-1,all=FALSE),origin="2020-02-22"),col="green")+
  geom_point(data=on.df,aes(y=log(as.numeric(as.character(can.bigger[which(can.bigger$Region==5),]$DailyCases)))))+
  geom_line(data=on.df,aes(y=log(as.numeric(as.character(can.bigger[which(can.bigger$Region==5),]$DailyCases)))),col="turquoise")+
  geom_vline(data=on.df,xintercept=as.Date(index(on.df$Distancing,1,k=1,all=FALSE),origin="2020-02-22"),col="turquoise")+
  geom_text(aes(x=as.Date(index(on.df$Distancing,1,k=1,all=FALSE),origin="2020-02-22"),
                label="\nOntarioSocial Distancing Start", y=200), angle=90, text=element_text(size=11))+
  #geom_vline(data=on.df,xintercept=as.Date(index(on.df$Distancing,1,k=-1,all=FALSE),origin="2020-02-22"),col="turquoise")+
  geom_point(data=qc.df,aes(y=log(as.numeric(as.character(can.bigger[which(can.bigger$Region==6),]$DailyCases)))))+
  geom_line(data=qc.df,aes(y=log(as.numeric(as.character(can.bigger[which(can.bigger$Region==6),]$DailyCases)))),col="blue")+
  geom_vline(data=qc.df,xintercept=as.Date(index(qc.df$Distancing,1,k=1,all=FALSE),origin="2020-02-22"),col="blue")+
  geom_text(aes(x=as.Date(index(qc.df$Distancing,1,k=1,all=FALSE),origin="2020-02-22"),
                label="\nQuebec Social Distancing Start", y=200), angle=90, text=element_text(size=11))
#geom_vline(data=qc.df,xintercept=as.Date(index(qc.df$Distancing,1,k=-1,all=FALSE),origin="2020-02-22"),col="blue")+
geom_point(data=sk.df,aes(y=log(as.numeric(as.character(can.bigger[which(can.bigger$Region==7),]$DailyCases)))))+
  geom_line(data=sk.df,aes(y=log(as.numeric(as.character(can.bigger[which(can.bigger$Region==7),]$DailyCases)))),col="purple")#+
#geom_vline(data=sk.df,xintercept=as.Date(index(sk.df$Distancing,1,k=1,all=FALSE),origin="2020-02-22"),col="purple")+
#geom_vline(data=sk.df,xintercept=as.Date(index(sk.df$Distancing,1,k=-1,all=FALSE),origin="2020-02-22"),col="purple")

#Getting dates for French laws
fr.table = readClipboard()
fr.table = cleanTable(fr.table)
colnames(fr.table) = fr.table[1,]
fr.table = fr.table[-c(1,2),]
fr.table = fr.table[c(17:148),]
colnames(fr.table)[1] = "Dates"
for (col in 3:ncol(fr.table)) {
  for (row in 1:132) {
    if (as.character(fr.table[row,col])%in%c(""," -  ")) {
      fr.table[row,col] = 0
    }
  }
}
fr.df = as.data.frame(fr.table)
fr.df$Dates
fr.df$Masks # No masks during this time
fr.df$Distancing # Began March 17th, continued into the summer
fr.df$Closures # March 12th - May 10th
fr.df$Lockdown # March 12th - May 10th

# Comparing stringency

all.stringent = c()
for (i in 1:98) {
  all.stringent = append(all.stringent,worldtest.df$AvgStringency[(132*i)+1],length(all.stringent))
}
all.stringent[98] = 0 # Sweden had no lockdown laws
all.stringent = as.matrix(all.stringent,nrow=1)
all.stringent = t(all.stringent)
colnames(all.stringent) = all.demnames

ests = readClipboard()
ests.table = cleanTable(ests)
ests.table = ests.table[-1,]
ests.table[98,c(2:4)] = c(0,0,0)
ests.table = t(ests.table)
colnames(ests.table) = all.demnames
ests.table = ests.table[-1,]
rbind(all.stringent,ests.table)
new.table = rbind(all.stringent,ests.table)
new.table = t(new.table)
colnames(new.table) = c("#Interv.","Est","Low","Upp")
new.df = as.data.frame(new.table)
plot(all.stringent,as.numeric(as.character(ests.table[1,])))
ggplot(data=NULL,aes(all.stringent,log(as.numeric(as.character(ests.table[1,])))))+geom_point()+geom_smooth()

library(lme4)
string.fit = glm(as.numeric(as.character(AvgStr))~as.numeric(as.character(Est)),family="poisson",data=new.df)
x = all.stringent
y = predict(string.fit,list(Est=x),type="response")
x = c(seq(x))
y = c(as.numeric(y))
ggplot(data=NULL,aes(x,y))+geom_point()+geom_smooth()

time.ests = c()
for (e in 1:98) {
  time.ests = append(time.ests,rep(ests.table[1,e],132),length(time.ests))
}
plot(as.numeric(as.character(worldtest.df$NumberInterventions)),time.ests)

worldnew = cbind(worldtest.df,time.ests)
worldnew = as.data.frame(worldnew)
worldfit = lm(as.numeric(as.character(time.ests))~as.numeric(as.character(NumberInterventions)),data=worldnew)

usa.table = readClipboard()
usa.table = cleanTable(usa.table)
colnames(usa.table) = colnames(rep(can.bigger,16))
usa.table = usa.table[-c(1:18),]
usa.bigger = c()
for (i in 1:51) {
  state.table = usa.table[,c(((18*(i-1))+1):((18*(i-1))+18))]
  usa.bigger = rbind(usa.bigger,state.table)
}

germ.table = readClipboard()
germ.table = cleanTable(germ.table)
colnames(germ.table) = colnames(rep(can.bigger,16))
germ.table = germ.table[-c(1:18),]
germ.bigger = c()
for (i in 1:16) {
  state.table = germ.table[,c(((18*(i-1))+1):((18*(i-1))+18))]
  germ.bigger = rbind(germ.bigger,state.table)
}

italy.table = readClipboard()
italy.table = cleanTable(italy.table)
colnames(italy.table) = colnames(rep(can.bigger,21))
italy.table = italy.table[-c(1:18),]
italy.bigger = c()
for (i in 1:21) {
  reg.table = italy.table[,c(((18*(i-1))+1):((18*(i-1))+18))]
  italy.bigger = rbind(italy.bigger,reg.table)
}

eu.table = readClipboard()
eu.table = cleanTable(eu.table)
colnames(eu.table) = colnames(rep(can.bigger,3))
eu.table = eu.table[-c(1:18),]
eu.bigger = c()
for (i in 1:3) {
  country.table = eu.table[,c(((18*(i-1))+1):((18*(i-1))+18))]
  eu.bigger = rbind(eu.bigger,country.table)
}

world.bigger = rbind(can.bigger,usa.bigger,germ.bigger,italy.bigger,eu.bigger)
world.all.df = as.data.frame(world.bigger)
worldnew$DailyCases = world.bigger$DailyCases
worldnew$CumulCases = world.bigger$CumulCases
worldnew$DailyDeaths = world.bigger$DailyDeaths
worldnew$CumulDeaths = world.bigger$CumulDeaths
  
worldfit2 = glm(as.numeric(as.character(time.ests))~as.numeric(as.character(NumberInterventions))+
                  as.numeric(as.character(CumulCases))+(as.numeric(as.character(NumberInterventions)):
                  as.numeric(as.character(CumulCases))),data=worldnew)
x = as.numeric(as.character(worldnew$NumberInterventions))
coefsfit = summary(worldfit2)[12][[1]]
int.est = as.numeric(as.character(coefsfit[1,1]))
int.se = as.numeric(as.character(coefsfit[1,2]))
int = c(int.est,int.est-int.se,int.est+int.se)
slope.est = as.numeric(as.character(coefsfit[2,1]))
slope.se = as.numeric(as.character(coefsfit[2,2]))
slope = c(slope.est,slope.est-slope.se,slope.est+slope.se)
y = cbind(int[1] + (slope[1]*x),int[2] + (slope[2]*x),int[3] + (slope[3]*x))
matplot(x,y,type='l')

# Plots the effect of social distancing on the positivity rate vs. #Interventions

# GLOBAL MODEL

demInd = readClipboard()
demInd = cleanTable(demInd)
demInd = demInd[-c(1:2),]
world.covars = cbind(world.covars[,c(1:17)],world.covars[,c(19:21)],demInd,
                     world.covars[,c(23:28)])

demInd.repeat = c()
for (ind in demInd) {
  demInd.repeat = append(demInd.repeat,rep(ind,132),length(demInd.repeat))
}

demInd0 = demInd.repeat[c(1:12936)]
demIndCh = demInd.repeat[c(12937:17424)]

worldtest.df$demInd = demInd0

qworld = as.numeric(as.character(Mobility)) ~ f(as.numeric(Date), model = "ar1") +
  f(nregion, model = "iid") + f(ncountry, model = "iid") +
  f(Distancing, model = "rw2") + f(Closures, model = "rw2") +
  f(Lockdown, model = "rw2") + f(NumberInterventions, model = "rw2") +
  as.numeric(as.character(Popmill)) + as.numeric(as.character(HospBeds1000)) +
  as.numeric(as.character(HealthSpendGDP)) + as.numeric(as.character(Over65)) +
  as.numeric(as.character(PerUrb)) + as.numeric(as.character(Pdens)) +
  as.numeric(as.character(MedIncomeUSD)) + as.numeric(as.character(PCSmk)) +
  as.numeric(as.character(PCObese)) + as.numeric(as.character(PCLCanc)) +
  Cfed + Rfed + Creg + Rreg + as.numeric(as.character(Lfedvote)) +
  as.numeric(as.character(Cfedvote)) + as.numeric(as.character(Rfedvote)) +
  as.numeric(as.character(Lregvote)) + as.numeric(as.character(Cregvote)) +
  as.numeric(as.character(Rregvote)) + as.numeric(as.character(WV1)) +
  as.numeric(as.character(WV2)) + as.numeric(as.character(worldtest.df$demInd))

world = inla(qworld, control.inla = list(diagonal = 10000, strategy = "gaussian", int.strategy = "eb"),
             family = "gaussian",data=worldtest.df)

worldtest.df$nmonth = month(worldtest.df$Date)
worldtest.df$doy = day(worldtest.df$Date)

qworld2 = as.numeric(as.character(Mobility)) ~ s(as.numeric(Date)) +
  s(as.numeric(Date),by=ncountry) + s(as.numeric(Date),by=nregion) +
  s(Distancing) + s(Closures) + s(Lockdown) + s(NumberInterventions) +
  s(as.numeric(as.character(Popmill))) + s(as.numeric(as.character(HospBeds1000))) +
  s(as.numeric(as.character(HealthSpendGDP))) + s(as.numeric(as.character(Over65))) +
  s(as.numeric(as.character(PerUrb))) + s(as.numeric(as.character(Pdens))) +
  s(as.numeric(as.character(MedIncomeUSD))) + s(as.numeric(as.character(PCSmk))) +
  s(as.numeric(as.character(PCObese))) + s(as.numeric(as.character(PCLCanc))) +
  s(factor(Cfed)) + s(factor(Rfed)) + s(factor(Creg)) + s(factor(Rreg)) +
  s(as.numeric(as.character(Lfedvote))) +
  s(as.numeric(as.character(Cfedvote))) + s(as.numeric(as.character(Rfedvote))) +
  s(as.numeric(as.character(Lregvote))) + s(as.numeric(as.character(Cregvote))) +
  s(as.numeric(as.character(Rregvote))) + s(as.numeric(as.character(WV1))) +
  s(as.numeric(as.character(WV2))) + s(as.numeric(as.character(demInd)))

world2 = gamm(qworld2, data=worldtest.df)

## Democracy + WVS

culture = cbind(world.covars.df$WV1,world.covars,df$WV2)
all.culture = cbind(demInd,culture)
all.culture0 = all.culture[c(1:98),]
all.cultureCh = all.culture[c(99:132),]

nat = all.culture[c(1,8,59,75,96,97,98),]
dem = demInd[c(1,8,59,75,96,97,98)]
nat.df = as.data.frame(nat)
wv1 = as.numeric(as.character(nat.df$WV1))
wv2 = as.numeric(as.character(nat.df$WV2))
effects = readClipboard()
effects = cleanTable(effects)
colnames(effects) = effects[1,]
effects = effects[-1,]
nat = cbind(countries[-8],effects,dem,wv1,wv2)
nat = nat[,-1]
est = effects[,2]

summ.df = as.data.frame(nat)
ggplot(data=NULL,aes(x=as.numeric(as.character(dem)),y=as.numeric(as.character(est))))+geom_point(size=2)+
  xlab("Democracy Index")+ylab("Social Distancing Effectiveness")+
  ggtitle("Democracy and Social Distancing Effectiveness")+theme_grey(base_size=16)

cor(as.numeric(as.character(dem)),as.numeric(as.character(est)))

#install.packages("plot3D")
#library(plot3D)
scatter3D(x=as.numeric(as.character(wv1)),y=as.numeric(as.character(wv2)),z=as.numeric(as.character(est)),
          theta=15,phi=20,pch=20,xlab="Survival-Self Expression",ylab="Traditional-Secular",
          zlab="Social Distancing Effectiveness",main="World Values and Social Distancing Effectiveness")

cor(as.numeric(as.character(dem)),as.numeric(as.character(wv1)))
cor(as.numeric(as.character(dem)),as.numeric(as.character(wv2)))
