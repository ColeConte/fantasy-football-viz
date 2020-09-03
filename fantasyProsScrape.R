#Fantasy Pros Projections Scrape

library("XML")
library("RCurl")

scrapeProjections <- function(){
  #Do the scraping
  qbHTML = getURL("https://www.fantasypros.com/nfl/projections/qb.php")
  QB = readHTMLTable(qbHTML,asText=T,stringsAsFactors=F)$data
  rbHTML = getURL("https://www.fantasypros.com/nfl/projections/rb.php")
  RB = readHTMLTable(rbHTML,asText=T,stringsAsFactors=F)$data
  wrHTML = getURL("https://www.fantasypros.com/nfl/projections/wr.php")
  WR = readHTMLTable(wrHTML,asText=T,stringsAsFactors=F)$data
  teHTML = getURL("https://www.fantasypros.com/nfl/projections/te.php")
  TE = readHTMLTable(teHTML,asText=T,stringsAsFactors=F)$data
  kHTML = getURL("https://www.fantasypros.com/nfl/projections/k.php")
  K = readHTMLTable(kHTML,asText=T,stringsAsFactors=F)$data
  dstHTML = getURL("https://www.fantasypros.com/nfl/projections/dst.php")
  DST = readHTMLTable(dstHTML,asText=T,stringsAsFactors=F)$data
  
  #Remove commas
  QB = data.frame(sapply(QB,function(x) gsub(",","",x)),stringsAsFactors = F)
  RB = data.frame(sapply(RB,function(x) gsub(",","",x)),stringsAsFactors = F)
  WR = data.frame(sapply(WR,function(x) gsub(",","",x)),stringsAsFactors = F)
  TE = data.frame(sapply(TE,function(x) gsub(",","",x)),stringsAsFactors = F)
  DST = data.frame(sapply(DST,function(x) gsub(",","",x)),stringsAsFactors = F)
  K = data.frame(sapply(K,function(x) gsub(",","",x)),stringsAsFactors = F)
  
  #Convert to Numeric
  QB[,2:ncol(QB)] = sapply(QB[, 2:ncol(QB)], as.numeric)
  RB[,2:ncol(RB)] = sapply(RB[, 2:ncol(RB)], as.numeric)
  WR[,2:ncol(WR)] = sapply(WR[, 2:ncol(WR)], as.numeric)
  TE[,2:ncol(TE)] = sapply(TE[, 2:ncol(TE)], as.numeric)
  DST[,2:ncol(DST)] = sapply(DST[, 2:ncol(DST)], as.numeric)
  K[,2:ncol(K)] = sapply(K[, 2:ncol(K)], as.numeric)
  
  #Set up custom projections
  QB = customProjections(QB,"QB")
  RB = customProjections(RB,"RB")
  WR = customProjections(WR,"WR")
  TE = customProjections(TE,"TE")
  K = customProjections(K,"K")
  DST = customProjections(DST,"DST")
  
  #Combine players into 
  outputDF = rbind(QB[,c("Player","Pos","FPTS")],RB[,c("Player","Pos","FPTS")],WR[,c("Player","Pos","FPTS")],TE[,c("Player","Pos","FPTS")],DST[,c("Player","Pos","FPTS")],K[,c("Player","Pos","FPTS")])
  return(outputDF)
}

customProjections<- function(df,position){
  if(position=="QB"){
    df["FPTS"] = df["CMP"] + df["YDS"]/25.0 + df["TDS"]*4.0 - df["INTS"]*2.0 + df["YDS.1"]/5.0 + df["TDS.1"]*6.0 - df["FL"]*2.0
    df["Pos"] = "QB"
    }
  else if(position=="RB"){
    df["FPTS"] = df["YDS"]/5.0 + df["TDS"]*6.0 - df["FL"]*2.0 + df["YDS.1"]/5.0 + df["TDS.1"]*6.0 + df["REC"]
    df["Pos"] = "RB"
    }
  else if(position=="WR"){
    df["FPTS"] = df["YDS"]/5.0 + df["TDS"]*6.0 - df["FL"]*2.0 + df["YDS.1"]/5.0 + df["TDS.1"]*6.0 + df["REC"]
    df["Pos"] = "WR"
  }
  else if(position=="TE"){
    df["FPTS"] = df["YDS"]/5.0 + df["TDS"]*6.0 + df["REC"] - df["FL"]*2.0
    df["Pos"] = "TE"
  }
  #Too lazy for DST right now (gotta convert to per game values and then lookup based on thresholds
  else if(position=="DST"){
    df["Pos"] = "DST" 
  }
  else if(position=="K"){
    df["Pos"] = "K"
  }
  #Data is insufficient for custom kicker projections
  return(df)
}
