#data cleaning for S&P 500 stocks in an open market
#takes input of CSV file containing market capitalization data

#load and format data
stockdata = read.csv("C:/Users/richa/Downloads/sp500data.csv")
colnames(stockdata) = c("name", "ticker", "date", "price", "marketcap", "shrout")
stockdata = as.data.frame(apply(stockdata, 2, function(x) gsub(",", "", x)))
stockdata[, c("price", "marketcap", "shrout")] = apply(stockdata[,  c("price", "marketcap", "shrout")], 2, as.numeric)
stockdata$date = as.Date(stockdata$date, "%Y%m%d")
stockdata = na.omit(stockdata)

stockdata = stockdata[stockdata$date >= "1957-01-01",]

stockdata = stockdata[!duplicated(stockdata),]

      #bullshit for testing purposes
      #set.seed(1)
      #stockdata$marketcap = rexp(nrow(stockdata), 1/1000)

# stockdata = lapply(unique(stockdata$date), function(x) stockdata[stockdata$date == x, ])

sdat = list()
dates = unique(stockdata$date)
for (i in 1:length(dates)){
  sdat[[i]] = stockdata[stockdata$date == dates[i],]
  print(i)
}
stockdata = sdat
rm(sdat)

#accounting for stocks split into multiple parts
merge.caps = function(x){
  #identify duplicated stock names
  dup.names = unique(x$name[which(duplicated(x$name))])
  
  #sum market cap of each part of the company with duplicated name, delete irrelevant rows
  if (length(dup.names) > 0){
    for (i in 1:length(dup.names)){
      z = which(x$name == dup.names[i])
      first.idx = z[1]
      x[first.idx, ]$marketcap = sum(x$marketcap[z])
      x = x[-z[2:length(z)], ]
    }
  }
  
  return(x)
}
stockdata <- lapply(stockdata, merge.caps)

#order stocks by rank on each date
stockdata = lapply(stockdata, function(x) x[order(x$marketcap, decreasing = TRUE),])
names(stockdata) = lapply(stockdata, function(x) as.character(x$date[1]))
stockdata = stockdata[order(names(stockdata))]

#n = number of stocks we wish to hold positions in at any given time, Tm = number of time periods
n = 100
Tm = length(stockdata)

#add p(t), q(t) to each date
#helper function - d1 and d2 are dataframes representing present (d1) and future (d2) dates
pq.add.ranked = function(d1, d2){
  #define function output, compute p
  out = d1
  date2 = d2
  out$p = c(out$marketcap[1:n]/sum(out$marketcap[1:n]), rep(NA, nrow(out) - n))
  
  #if any stocks are delisted
  delisted.stocks = which(! (out$name %in% date2$name) )
  #add them to date2
  date2 = rbind(date2, out[delisted.stocks, !(colnames(out) %in% c("p"))])
  
  #identify rows which contribute to q
  q.inds = which(date2$name %in% out$name[1:n])
  
  #modelling total return as 1 if stock was delisted
  date2$q = NA
  date2$q[q.inds] = date2$marketcap[q.inds]/sum(date2$marketcap[q.inds])
  date2 = date2[,c("name", "q")]
  
  out = merge(out, date2, by = "name")
  out = out[order(out$marketcap, decreasing = TRUE),]
  return(out)
}
stockdata.pq = mapply(pq.add.ranked, stockdata[1:(Tm-1)], stockdata[2:Tm])
stockdata.pq = apply(stockdata.pq, 2, data.frame)

#add dates as names
names(stockdata.pq) = lapply(stockdata.pq, function(x) as.character(x$date[1]))

#construct p and q matrices
p = do.call(cbind, lapply(stockdata.pq, function(x) x$p[1:n])); colnames(p) = names(stockdata.pq)
q = do.call(cbind, lapply(stockdata.pq, function(x) x$q[1:n])); colnames(q) = names(stockdata.pq)

#write to csv
write.csv(p, "C:/Users/richa/Downloads/p_mtx.csv", row.names = FALSE)
write.csv(q, "C:/Users/richa/Downloads/q_mtx.csv", row.names = FALSE)

#PERHAPS WRITE FOR VARYING CATEGORIES? (LATER)
#PERHAPS ONLY CONSIDER TRADING IN STOCKS WHICH HAVE NOT RECENTLY HAD AN IPO