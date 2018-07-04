





      
##

ksink <- ksink.builder(c(1:14))
dataset.length <- NROW(ksink)
recent.data <- ksink[dataset.length,]

plot(ksink$app1044[-1:-2600], type="l")

### Most up-to-date data for preds 
recent.data


predict(fitlm, recent.data,2)
predict(fitcf, recent.data, OOB=TRUE)
predict(fitrf, recent.data, OOB=TRUE)
pnndf <- data.frame(recent.data$dis1044, recent.data$app902, recent.data$dis902, recent.data$app903, recent.data$dis903)
forecast(fitnn, xreg=pnndf, 1)                
forecast(fitnnbas,7)
forecast(fitar,xreg=pnndf,1)
forecast(fitarim,5)


###########################################################################################
###########################################################################################
###########################################################################################


###########################################################################################
###########################################################################################
###########################################################################################


plot(as.POSIXct(ksink$Date, format="%d %b %Y"), ksink$app1044, type="l")
tsdisplay(diff(ksink$app1044),main="")



