
###########################################################################################
#######################          PREP UTD RCP DATAFRAME     ###############################
###########################################################################################

updateRCP <- function(){
  
  RCPscraper <- function(pid){
    
    current_date = Sys.Date()
    unix_time = as.numeric(as.POSIXct(current_date))
    file_loc = "./"
    file = paste0(file_loc,pid,'_historical.js')
    file_time <- file.info(file)$mtime
    
    #Only update file if not present or more than a day old
    if(is.na(file_time) | Sys.time() - file_time > 9e4) {
    url = paste('http://www.realclearpolitics.com/epolls/json/',pid,'_historical.js', sep='')
    download.file(url=url,destfile = file)
    }
    
    text_string = readChar(file, file.info(file)$size)
    text_string = substring(text_string,13)
    text_string = substr(text_string, 1, nchar(text_string)-2)
    polls = fromJSON(text_string)
    
    num_points = length(
      polls$poll$rcp_avg
    )
    
    app_vals = NULL
    dis_vals = NULL
    date = NULL
    for(i in 1:num_points){
      app_vals[i] = as.numeric(polls$poll$rcp_avg[[i]]$candidate[[1]]$value)
      dis_vals[i] = as.numeric(polls$poll$rcp_avg[[i]]$candidate[[2]]$value)
      date[i] = substring(polls$poll$rcp_avg[[i]]$date, 6,16)
    }
    
    date = rev(date)
    app_vals = rev(app_vals)
    dis_vals = rev(dis_vals)
    
    output <- data.frame(as.Date(date, format = "%d %b %Y"),app_vals,dis_vals,stringsAsFactors = FALSE)
    return(output)
  }
  # 
  # obama <- RCPscraper(1044)
  # colnames(obama) <- c("Date", "app1044", "dis1044")
  trump <- RCPscraper(6179)
  colnames(trump) <- c("Date", "app6179", "dis6179")
  # congress <- RCPscraper(903)
  # colnames(congress) <- c("Date", "app903", "dis903")
  # dirc <- RCPscraper(902)
  # colnames(dirc) <- c("Date", "app902", "dis902")
  
  rcp.frame <- trump
  # 
  # rcp.frame$Date903 <- rep(NA,length(rcp.frame$Date))
  # rcp.frame$app903 <- rep(NA,length(rcp.frame$Date))
  # rcp.frame$dis903 <- rep(NA,length(rcp.frame$Date))
  # rcp.frame$Date902 <- rep(NA,length(rcp.frame$Date))
  # rcp.frame$app902 <- rep(NA,length(rcp.frame$Date))
  # rcp.frame$dis902 <- rep(NA,length(rcp.frame$Date))
  # 
  # rcp.frame$Date903[rcp.frame$Date %in% congress$Date] <- as.character(congress$Date[congress$Date %in% rcp.frame$Date])
  # rcp.frame$app903[rcp.frame$Date %in% congress$Date] <- congress$app903[congress$Date %in% rcp.frame$Date]
  # rcp.frame$dis903[rcp.frame$Date %in% congress$Date] <- congress$dis903[congress$Date %in% rcp.frame$Date]
  # 
  # rcp.frame$Date902[rcp.frame$Date %in% dirc$Date] <- as.character(dirc$Date[dirc$Date %in% rcp.frame$Date])
  # rcp.frame$app902[rcp.frame$Date %in% dirc$Date] <- dirc$app902[dirc$Date %in% rcp.frame$Date]
  # rcp.frame$dis902[rcp.frame$Date %in% dirc$Date] <- dirc$dis902[dirc$Date %in% rcp.frame$Date]
  # 
  # 
  # rcp.frame$app903[is.na(rcp.frame$app903)] <- rcp.frame$app903[max(which(rcp.frame$app903 != "NA"))]
  # rcp.frame$dis903[is.na(rcp.frame$dis903)] <- rcp.frame$dis903[max(which(rcp.frame$dis903 != "NA"))]
  # 
  # rcp.frame$app902[is.na(rcp.frame$app902)] <- rcp.frame$app902[max(which(rcp.frame$app902 != "NA"))]
  # rcp.frame$dis902[is.na(rcp.frame$dis902)] <- rcp.frame$dis902[max(which(rcp.frame$dis902 != "NA"))]
  # # 
  # ### Sanity Check
  # which(is.na(rcp.frame), TRUE)
  # 
  # tail(rcp.frame,7)
  # 
  return(rcp.frame)
}

###########################################################################################
###########################################################################################
###########################################################################################




###########################################################################################
#############################   FIRST TIME GENERATE PRD DATAFRAME #########################
###########################################################################################
# Pulls first data from a market and writes to disk - but must be bracketed yes-no market #

newmarket.PRD <- function(market){
  
  predictRecord <- data.frame(t(rep(0,21)))
  
  raw<- GET(paste("https://www.predictit.org/api/marketdata/ticker/",market,sep=""), add_headers("Accept: application/xml"))
  
  Prices <- data.frame(t(rep(0,21)))
  
  Prices[1,1] <- content(raw)$TimeStamp
  
  for (i in 1:5){
    Prices[1,i+1] <- ifelse(is.null(content(raw)$Contracts[[i]]$BestBuyYesCost) == TRUE, NA, content(raw)$Contracts[[i]]$BestBuyYesCost)
    Prices[1,i+6] <- ifelse(is.null(content(raw)$Contracts[[i]]$BestSellYesCost) == TRUE, NA, content(raw)$Contracts[[i]]$BestSellYesCost)
    Prices[1,i+11] <- ifelse(is.null(content(raw)$Contracts[[i]]$BestBuyNoCost) == TRUE, NA, content(raw)$Contracts[[i]]$BestBuyNoCost)
    Prices[1,i+16] <- ifelse(is.null(content(raw)$Contracts[[i]]$BestSellNoCost) == TRUE, NA, content(raw)$Contracts[[i]]$BestSellNoCost)
  }
  
  predictRecord <- Prices[1,]
  
  colnames(predictRecord) <- c("Date", "YesBuy.B1", "YesBuy.B2", "YesBuy.B3", "YesBuy.B4", "YesBuy.B5", 
                               "YesSell.B1", "YesSell.B2", "YesSell.B3", "YesSell.B4", "YesSell.B5", 
                               "NoBuy.B1", "NoBuy.B2", "NoBuy.B3", "NoBuy.B4", "NoBuy.B5", 
                               "NoSell.B1", "NoSell.B2", "NoSell.B3", "NoSell.B4", "NoSell.B5")
  
  predictRecord
  
  written <- FALSE
  exists <- file.exists(paste("pRecord", market, ".csv", sep=""))
  if (!file.exists(paste("pRecord", market, ".csv", sep=""))){
    write.csv(predictRecord, paste("pRecord", market, ".csv", sep=""), row.names=FALSE)
    written <- TRUE
  }
  
  return(paste("File already existed?:", exists,"Written?:",written))
}

###########################################################################################
###########################################################################################
###########################################################################################




###########################################################################################
############################     UPDATE UTD PREDICTIT DATAFRAME  ##########################
###########################################################################################

update.PRD <- function(market) {
  
  predictRecord <- read.csv(paste("pRecord", market, ".csv", sep=""))
  
  raw<- GET(paste("https://www.predictit.org/api/marketdata/ticker/",market,sep=""), add_headers("Accept: application/xml"))
  
  Prices <- data.frame(t(rep(0,21)))
  
  Prices[1,1] <- content(raw)$TimeStamp
  
  for (i in 1:5){
    Prices[1,i+1] <- ifelse(is.null(content(raw)$Contracts[[i]]$BestBuyYesCost) == TRUE, NA, content(raw)$Contracts[[i]]$BestBuyYesCost)
    Prices[1,i+6] <- ifelse(is.null(content(raw)$Contracts[[i]]$BestSellYesCost) == TRUE, NA, content(raw)$Contracts[[i]]$BestSellYesCost)
    Prices[1,i+11] <- ifelse(is.null(content(raw)$Contracts[[i]]$BestBuyNoCost) == TRUE, NA, content(raw)$Contracts[[i]]$BestBuyNoCost)
    Prices[1,i+16] <- ifelse(is.null(content(raw)$Contracts[[i]]$BestSellNoCost) == TRUE, NA, content(raw)$Contracts[[i]]$BestSellNoCost)
  }
  
  colnames(Prices) <- c("Date", "YesBuy.B1", "YesBuy.B2", "YesBuy.B3", "YesBuy.B4", "YesBuy.B5", 
                        "YesSell.B1", "YesSell.B2", "YesSell.B3", "YesSell.B4", "YesSell.B5", 
                        "NoBuy.B1", "NoBuy.B2", "NoBuy.B3", "NoBuy.B4", "NoBuy.B5", 
                        "NoSell.B1", "NoSell.B2", "NoSell.B3", "NoSell.B4", "NoSell.B5")
  
  predictRecord <- rbind(predictRecord,Prices[1,])
  
  write.csv(predictRecord, paste("pRecord", market, ".csv"), row.names=FALSE)
  
  return(tail(predictRecord,7))
  
}

###########################################################################################
###########################################################################################
###########################################################################################



###########################################################################################
############################# Quick look at prices           ##############################
###########################################################################################

ql.PRD <- function(market){
  raw<- GET(paste("https://www.predictit.org/api/marketdata/ticker/",market,sep=""), add_headers("Accept: application/xml"))
  
  Prices <- data.frame(Price = c("YesBuy","YesSell","NoBuy","NoSell"),
                       B1= numeric(4),
                       B2= numeric(4),
                       B3= numeric(4),
                       B4= numeric(4),
                       B5= numeric(4))
  
  for (i in 2:6){
    Prices[1,i] <- ifelse(is.null(content(raw)$Contracts[[i]]$BestBuyYesCost) == TRUE, NA, content(raw)$Contracts[[i]]$BestBuyYesCost)
    Prices[2,i] <- ifelse(is.null(content(raw)$Contracts[[i]]$BestSellYesCost) == TRUE, NA, content(raw)$Contracts[[i]]$BestSellYesCost)
    Prices[3,i] <- ifelse(is.null(content(raw)$Contracts[[i]]$BestBuyNoCost) == TRUE, NA, content(raw)$Contracts[[i]]$BestBuyNoCost)
    Prices[4,i] <- ifelse(is.null(content(raw)$Contracts[[i]]$BestSellNoCost) == TRUE, NA, content(raw)$Contracts[[i]]$BestSellNoCost)
    
  }
  
  return(Prices)
}


###########################################################################################
###########################################################################################
###########################################################################################


###########################################################################################
######################### Collect poll data from average tables ###########################
###########################################################################################

ql.RCP <- function(pollid){
  u = paste0("https://www.realclearpolitics.com/epolls/other/",pollid ,".html")
  poll.block <- htmltab(doc = u)
  poll.block %>% 
    mutate(Approve = as.numeric(Approve), 
           Disapprove = as.numeric(Disapprove), 
           Spread = as.numeric(Spread))
  
  return(poll.block)
}
###########################################################################################
###########################################################################################
###########################################################################################



######################################################################################
######################################################################################
##############  Functions to build dataframe for training ############################
######################################################################################
######################################################################################

#Returns an offset vector with zeros replacing missing data



createoffset <- function(offset, datavec) {
  fillvector <- rep(0, abs(offset))
  nrows <- length(datavec)
  temp.store <- c(fillvector, datavec[1:(nrows-offset)])
  return(temp.store)
}

#Returns a dataframe with offset vectors within, data trimmed to remove filler

offsetloop <- function(datavec,offsetVector, vName) {
  
  temp.dataframe <- NULL
  temp.cname <- NULL
  for (i in 1:length(offsetVector)) { 
    temp.dataframe <- cbind(temp.dataframe, createoffset(offsetVector[i], datavec))  
    temp.cname[i] <- paste(vName, "offset", abs(offsetVector[i]),sep="")
  }
  returnframe <- data.frame(temp.dataframe)
  colnames(returnframe) <- temp.cname
  
  junk.data <- abs(max(offsetVector))
  
  returnframe <- returnframe[-1:-junk.data,]
  
  return(returnframe)
}

######################################################################################
######################################################################################
######################################################################################



###############################################################################################
########### Offset frames for all 5 predictors, and a predictor frame #########################
###############################################################################################

ksink.builder <- function(off.vec) {
  
  rcp.frame <- updateRCP()  
  
  dis1044.off <- offsetloop(rcp.frame$dis1044, off.vec , "dis1044")
  app903.off <- offsetloop(rcp.frame$app903, off.vec , "app903")
  dis903.off <- offsetloop(rcp.frame$dis903, off.vec , "dis903")
  app902.off <- offsetloop(rcp.frame$app902, off.vec , "app902")
  dis902.off <- offsetloop(rcp.frame$dis902, off.vec , "dis902")
  
  clip.back<- NROW(dis1044.off)
  clip.front <- NROW(rcp.frame)
  
  all.no.off <- rcp.frame[-1:(clip.back-clip.front),]
  
  # Specific offsets can be referenced by column
  
  ksink <- cbind(all.no.off, dis1044.off, app903.off, dis903.off, app902.off, dis902.off)
  
  return(ksink)
}

######################################################################################
######################################################################################
######################################################################################



######################################################################################
######################## Prep a custom formula for playing with models ###############
######################## use with ksink                                ###############
######################################################################################

makefunplus <- function(d.back, d.max) {
  
  rcp.frame <- updateRCP()  
  off.vec <- c(1:14)
  
  dis1044.off <- offsetloop(rcp.frame$dis1044, off.vec , "dis1044")
  app903.off <- offsetloop(rcp.frame$app903, off.vec , "app903")
  dis903.off <- offsetloop(rcp.frame$dis903, off.vec , "dis903")
  app902.off <- offsetloop(rcp.frame$app902, off.vec , "app902")
  dis902.off <- offsetloop(rcp.frame$dis902, off.vec , "dis902")
  
  
  pred.names <- c(colnames(dis1044.off)[d.back:d.max], colnames(app903.off)[d.back:d.max], 
                  colnames(dis903.off)[d.back:d.max], colnames(app902.off)[d.back:d.max], colnames(dis902.off)[d.back:d.max])
  
  return(pred.names)
}


######################################################################################
######################################################################################
######################################################################################





###########################################################################################
############## Grab bracket values and use inputs to assess in-bracket probs ##############
###########################################################################################


build.brackets <- function(market){
  raw<- GET(paste("https://www.predictit.org/api/marketdata/ticker/",market,sep=""), add_headers("Accept: application/xml"))
  
  bracket.vec <- NULL
  
  for (i in 1:5){
    bracket.vec[i] <- content(raw)$Contracts[[i]]$ShortName
  }
  
  bracket.vec <- strsplit(bracket.vec, split = c("\\+","-","-","-","-"))
  
  return(bracket.vec)
}

bracket.prob.gen <- function(mean,sd,bracket.list) {
  bprobs <- data.frame(t(rep(0,5)))
  
  colnames(bprobs) <- c("B1", "B2", "B3", "B4", "B5")
  
  bprobs[1,1] <- paste("+",bracket.list[[1]], sep="")
  bprobs[1,2] <- paste(bracket.list[[2]][2],bracket.list[[2]][1], sep="-")
  bprobs[1,3] <- paste(bracket.list[[3]][2],bracket.list[[3]][1], sep="-")
  bprobs[1,4] <- paste(bracket.list[[4]][2],bracket.list[[4]][1], sep="-")
  bprobs[1,5] <- paste(bracket.list[[5]], "-", sep="")
  
  bprobs[2,1] <- as.numeric(round(1 - pnorm(as.numeric(bracket.list[[1]]),mean,sd), 4))
  bprobs[2,2] <- as.numeric(round(pnorm(as.numeric(bracket.list[[2]][2]),mean,sd) - pnorm(as.numeric(bracket.list[[2]][1]),mean,sd), 4))
  bprobs[2,3] <- as.numeric(round(pnorm(as.numeric(bracket.list[[3]][2]),mean,sd) - pnorm(as.numeric(bracket.list[[3]][1]),mean,sd), 4))
  bprobs[2,4] <- as.numeric(round(pnorm(as.numeric(bracket.list[[4]][2]),mean,sd) - pnorm(as.numeric(bracket.list[[4]][1]),mean,sd), 4))
  bprobs[2,5] <- as.numeric(round(pnorm(as.numeric(bracket.list[[5]][1]),mean,sd), 4)   )
  
  
  return(bprobs)
}


###########################################################################################
###########################################################################################
###########################################################################################



###########################################################################################
######### Estimate whether brackets are over or undervalued according to mean sd ###########
###########################################################################################

rnorm.bracketeval <- function(mean,sd,bracket) {
  
  bracket.list <- build.brackets(bracket)
  bracket.probs <- bracket.prob.gen(mean,sd,bracket.list)
  bracket.probs.vec <- as.numeric(bracket.probs[2,])
  bracket.inv.vec <- 1 - bracket.probs.vec
  
  buy.probs <- ql.PRD(bracket)[c(1,3),]
  
  buy.value <- buy.probs
  buy.value[1,] <- bracket.probs.vec - buy.probs[1,]
  buy.value[2,] <- bracket.inv.vec - buy.probs[2,]
  
  rownames(bracket.probs) <- c("Range","p")
  rownames(buy.value) <- c("PotentialYes", "PotentialNo")
  
  return(rbind(bracket.probs,buy.probs,buy.value))
}


###########################################################################################
###########################################################################################
###########################################################################################

