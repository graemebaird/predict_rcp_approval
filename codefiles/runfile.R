
# Predictit Sample --------------------------------------------------------

#Set current market
curr.market <- "OBAMAAPPR.090216"

# Quick look at prices
ql.PRD(curr.market)

# Quick look at poll tables
ql.RCP()

# Quick look at current average
ql.rcpAvg()

#Latest numbers for all RCP polls
rcp.frame <- updateRCP()
tail(rcp.frame,1)

#Initialize market
newmarket.PRD(curr.market)

#Update prices for market
update.PRD(curr.market)

#Look for arbitrage potential
rnorm.bracketeval(48.8,.3,curr.market)



# Fit some models (saves to disk) -----------------------------------------


add.model.saver <- function(omin,omax){
  o.frame <- ksink.builder(c(omin:omax))
  fmla <- as.formula(paste("app1044 ~ ", paste(makefunplus(omin,omax), collapse= "+")))
  
  #fitlm <- lm(fmla, data=o.frame)
  #save(fitlm, file=paste("lm_",omin,"-",omax,".rda",sep=""))
  #remove(fitlm)
  
  fitcf <- cforest(fmla, data=o.frame, controls=cforest_unbiased(ntree=2000, mtry=3))
  save(fitcf, file=paste("cf_",omin,"-",omax,".rda",sep=""))
  remove(fitcf)
  
  #fitrf <- randomForest(fmla, data=o.frame, importance=TRUE, ntree=2000)
  #save(fitrf, file=paste("rf_",omin,"-",omax,".rda",sep=""))
  #remove(fitrf)
}

for(i in 1:7){
  add.model.saver(1,14)
}


fitarim <- auto.arima(ksink$app1044)
save(fitarim, file="arima.rda")
remove(fitarim)

ksink <- ksink.builder(c(1:14))
nndf <- data.frame(ksink$dis1044, ksink$app902, ksink$dis902, ksink$app903, ksink$dis903)
fitnn <- nnetar(ksink$app1044, xreg=nndf)
save(fitnn, file="nnxreg.rda")
remove(fitnn)

fitar <- auto.arima(ksink$app1044, xreg=nndf)

fitnnbas <- nnetar(ksink$app1044)
save(fitnnbas, file="nnbasic.rda")
remove(fitnnbas)


# Load models (requires model fit) ----------------------------------------


add.model.loader <- function(omin,omax){
  load(file==paste("lm_",omin,"-",omax,".rda",sep="")
       load(file=paste("cf_",omin,"-",omax,".rda",sep="")     
            load(file=paste("rf_",omin,"-",omax,".rda",sep="")
}

load(file="modelfits/cf_1-14.rda")
load(file="modelfits/arima.rda")
load(file="modelfits/nnxreg.rda")
load(file="modelfits/nnbasic.rda")

