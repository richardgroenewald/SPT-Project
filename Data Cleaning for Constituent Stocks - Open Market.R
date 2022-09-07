#data cleaning for S&P 500 stocks in an open market
#takes input of CSV file containing market capitalization data

#load and format data
stockdata = read.csv("C:/Users/richa/Downloads/testdata.csv")[,-4]
colnames(stockdata) = c("name", "ticker", "date", "price", "marketcap", "sharesoutstanding")
stockdata = as.data.frame(apply(stockdata, 2, function(x) gsub(",", "", x)))
stockdata[,c("price", "marketcap", "sharesoutstanding")] = apply(stockdata[,c("price", "marketcap", "sharesoutstanding")], 2, as.numeric)
stockdata$date = as.Date(stockdata$date, "%Y%m%d")

      #bullshit for testing purposes
      set.seed(1)
      stockdata$marketcap = rexp(nrow(stockdata), 1/1000)

stockdata = lapply(unique(stockdata$date), function(x) stockdata[stockdata$date == x, -(which(colnames(stockdata) %in% c("name", "price", "sharesoutstanding"))) ])

#order stocks by rank on each date
stockdata = lapply(stockdata, function(x) x[order(x$marketcap, decreasing = TRUE),])

#n = number of stocks we wish to hold positions in at any given time, Tm = number of time periods
n = 4
Tm = length(stockdata)

#to test if method works (remove bottom two rows of every date)
stockdata = lapply(stockdata, function(x) x[1:n,])

#add p(t), q(t) to each date
#helper function - d1 and d2 are dataframes representing present (d1) and future (d2) dates
pq.add.ranked = function(d1, d2){
  
  out = d1
  date2 = d2
  out$p = c(d1$marketcap[1:n]/sum(d1$marketcap[1:n]), rep(NA, nrow(d1) - n))
  
  q.inds = which(date2$ticker %in% out$ticker[1:n])
  
  delisted.tickers = 
  
  date2$q = NA
  date2$q[q.inds] = date2$marketcap[q.inds]/sum(date2$marketcap[q.inds])
  date2 = date2[,c("ticker", "q")]
  
  out = merge(out, date2, by = "ticker")
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

#write to csv
#write.csv(p, "p_mtx.csv")
#write.csv(q, "q_mtx.csv")

#WRITE CODE TO ACCOUNT FOR DELISTINGS!!!
#PERHAPS WRITE FOR VARYING CATEGORIES? (LATER)