#data cleaning for S&P 500 stocks in a closed market

#load and format data
stockdata = read.csv("C:/Users/richa/Downloads/testdata.csv")[,-4]
colnames(stockdata) = c("name", "ticker", "date", "price", "marketcap", "shrout")
stockdata = as.data.frame(apply(stockdata, 2, function(x) gsub(",", "", x)))
stockdata[,c("price", "marketcap", "shrout")] = apply(stockdata[,c("price", "marketcap", "shrout")], 2, as.numeric)
stockdata$date = as.Date(stockdata$date, "%Y%m%d")
#number of stocks, time periods
n = length(unique(stockdata$ticker))
Tm = length(unique(stockdata$date))

      #bullshit for testing purposes
      set.seed(1)
      stockdata$marketcap = rexp(nrow(stockdata), 1/1000)

#remove duplicates
stockdata = stockdata[!duplicated(stockdata),]

#split by date
stockdata = lapply(unique(stockdata$date), function(x) stockdata[stockdata$date == x, -(which(colnames(stockdata) %in% c("price", "shrout"))) ])

#accounting for stocks split into multiple parts
merge.caps = function(x){
  #identify duplicated stock names
  dup.names = unique(x$name[which(duplicated(x$name))])
  
  #sum market cap of each part of the company with duplicated name, delete irrelevant rows
  for (i in 1:length(dup.names)){
    z = which(x$name == dup.names[i])
    first.idx = z[1]
    x[first.idx, ]$marketcap = sum(x$marketcap[z])
    x = x[-z[2:length(z)], ]
  }
  
  return(x)
}
stockdata = lapply(stockdata, merge.caps)

      
#separate between ranked and non ranked cases
ranked = TRUE

if (ranked == TRUE){
  #order stocks by rank on each date
  stockdata = lapply(stockdata, function(x) x[order(x$marketcap, decreasing = TRUE),])
  
  #add p(t), q(t) to each date
  #helper function - d1 and d2 are dataframes representing present (d1) and future (d2) dates
  pq.add.ranked = function(d1, d2){
    out = d1
    date2 = d2
    out$p = out$marketcap/sum(out$marketcap)
    date2$q = date2$marketcap/sum(date2$marketcap)
    
    date2 = date2[, c("name", "q")]
    out = merge(out, date2, by = "name")
    out = out[order(out$marketcap, decreasing = TRUE),]
    return(out)
  }
  
  stockdata = mapply(pq.add.ranked, stockdata[1:(Tm-1)], stockdata[2:Tm])
  stockdata = apply(stockdata, 2, data.frame)
  
  #add dates as names
  names(stockdata) = lapply(stockdata, function(x) as.character(x$date[1]))
  
  #construct p and q matrices
  p = do.call(cbind, lapply(stockdata, function(x) x$p[1:n])); colnames(p) = names(stockdata)
  q = do.call(cbind, lapply(stockdata, function(x) x$q[1:n])); colnames(q) = names(stockdata)
  
} else {
  # I think this is bullshit, and it only needs to be specified in the other file:
  # #for the non ranked case, the partition must be specified here AND in the optimization file
  # #we provide a toy example here (random partition)
  # set.seed(1)
  # partition.df = data.frame(name = unique(stockdata$name), cat = sample(1:3, length(unique(stockdata$name)), replace = TRUE))
  # #add category to stockdata
  # stockdata = merge(stockdata, partition.df, by = "name")
  
  # #split by date
  # stockdata = lapply(unique(stockdata$date), function(x) stockdata[stockdata$date == x, -(which(colnames(stockdata) %in% c("price", "shrout"))) ])
  
  #add p(t), q(t) to each date
  #helper function - d1 and d2 are dataframes representing present (d1) and future (d2) dates
  pq.add.nr = function(d1, d2){
    out = d1
    date2 = d2
    out$p = out$marketcap/sum(out$marketcap)
    date2$q = date2$marketcap/sum(date2$marketcap)
    
    date2 = date2[, c("name", "q")]
    
    out = merge(out, date2, by = "name")
    out = out[order(out$name), ]
    return(out)
  }
  
  stockdata = mapply(pq.add.nr, stockdata[1:(Tm-1)], stockdata[2:Tm])
  stockdata = apply(stockdata, 2, data.frame)
  
  #add dates as names
  names(stockdata) = lapply(stockdata, function(x) as.character(x$date[1]))
  
  #construct p and q matrices
  p = do.call(cbind, lapply(stockdata, function(x) x$p[1:n])); colnames(p) = names(stockdata)
  q = do.call(cbind, lapply(stockdata, function(x) x$q[1:n])); colnames(q) = names(stockdata)
}

#write to csv
#write.csv(p, "p_mtx.csv")
#write.csv(q, "q_mtx.csv")