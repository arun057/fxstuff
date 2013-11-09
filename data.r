###############################################################################
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
###############################################################################
# Collection of routines to work with data
# Copyright (C) 2011  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################

testdata<-function()
{
  curr<-c("EUR/USD","USD/JPY","GBP/USD","USD/BRL")
  mydata <- xts(get(getFX('EUR/USD', from = Sys.Date() - 499, to = Sys.Date())));
  for(i in 2:4)
  {
    mydata<-cbind.xts(mydata, get(getFX(curr[i], from = Sys.Date() -  499, to = Sys.Date() )))
    
  }
  mydata[,c(2,4)]<-1/mydata[,c(2,4)]
  colnames(mydata)<-c("EUR","JPY","GBP","BRL")
  return(mydata)
}

find.tokens <- function
(
  txt, 		# source text
  marker,		# key-phrase(s) to find
  pos = 1,	# position to start searching at
  pos.start = T
)
{
  # find location of data
  marker = spl(marker)
  
  for(i in 1:len(marker)) {
    if( pos < 2 )
      pos1 = regexpr(marker[i], txt) 
    else 		
      pos1 = regexpr(marker[i], substr(txt, pos, nchar(txt))) 
    
    if( pos1 < 0 )	
      return(pos1)
    else {
      if( pos < 2 ) pos = pos1
      else pos = pos1 + pos - 1			
    }
    
    if( !pos.start ) pos = pos + attr(pos1, 'match.length')
  }
  
  return(pos)
}	


extract.token <- function
(
  txt, 		# source text
  smarker,	# start key-phrase(s) to find
  emarker,	# end key-phrase(s) to find
  pos = 1		# position to start searching at
)
{
  pos1 = find.tokens(txt, smarker, pos, pos.start = F)
  if( pos1 < 0 ) return("")
  pos2 = find.tokens(txt, emarker, pos1, pos.start = T) - 1
  if( pos2 < 0 ) return("")
  return(substr(txt,pos1,pos2))	
}


remove.tags <- function
(
  temp 		# source text
)
{
  # remove all formating							
  temp = gsub(pattern = '<.*?>', replacement = '', temp, perl = TRUE) 
  
  temp = gsub(pattern = '\r', replacement = '', temp, perl = TRUE) 
  temp = gsub(pattern = '\n', replacement = '', temp, perl = TRUE) 
  temp = gsub(pattern = '\t', replacement = '', temp, perl = TRUE) 
  temp = gsub(pattern = '&nbsp;', replacement = '', temp, perl = TRUE) 
  temp = gsub(pattern = '&amp;', replacement = '', temp, perl = TRUE) 
  temp = gsub(pattern = '&raquo;', replacement = '', temp, perl = TRUE) 		
  temp = gsub(pattern = '&#37;', replacement = '%', temp, perl = TRUE) 			
  
  
  return(temp)
}


###############################################################################
# extract.table.from.webpage
#' @export 
###############################################################################
extract.table.from.webpage <- function
(
  txt, 		# source text of webpage
  marker,		# key-phrase(s) located in the table to extract
  hasHeader=T	# flag if table has a header
)
{
  tryCatch({		
    # find location of data
    marker = spl(marker)
    pos1=1
    
    for(i in 1:len(marker)) {
      pos1 = regexpr(marker[i], substr(txt, pos1, nchar(txt))) + pos1
    }
    
    # find start/end of table
    pos0 = tail(gregexpr('<table', substr(txt, 1, pos1))[[1]], 1)
    pos2 = head(gregexpr('</table', substr(txt, pos1, nchar(txt)))[[1]], 1)
    temp =  substr(txt, pos0, pos1 + pos2 - 2)
    
    # remove all formating	
    temp = gsub(pattern = '<br>', replacement = '', temp, perl = TRUE) 
    
    temp = gsub(pattern = '</tr>', replacement = ';row;', temp, perl = TRUE) 
    temp = gsub(pattern = '</td>', replacement = ';col;', temp, perl = TRUE) 
    temp = gsub(pattern = '</th>', replacement = ';col;', temp, perl = TRUE) 
    
    temp = gsub(pattern = '<.*?>', replacement = '', temp, perl = TRUE) 
    
    temp = gsub(pattern = '\r', replacement = '', temp, perl = TRUE) 
    temp = gsub(pattern = '\n', replacement = '', temp, perl = TRUE) 
    temp = gsub(pattern = '\t', replacement = '', temp, perl = TRUE) 
    temp = gsub(pattern = '&nbsp;', replacement = '', temp, perl = TRUE) 
    temp = gsub(pattern = '&amp;', replacement = '', temp, perl = TRUE) 
    temp = gsub(pattern = '&raquo;', replacement = '', temp, perl = TRUE) 		
    
    # parse into matrix	
    temp = lapply( strsplit(temp, ';row;'), strsplit, ';col;')	
    n = max( sapply(temp[[1]], function(x) len(x)) )
    temp = t( sapply(temp[[1]], function(x) x[1:n]) )
    
    if(hasHeader) {
      colnames(temp) = temp[(hasHeader + 0), ]
      temp = temp[-c(1:(hasHeader + 0)), ,drop=F]
    }
    
  }, error = function(ex) {
    temp <<- txt
  }, finally = {
    return(temp)
  })
}

###############################################################################
# Test for extract.table.from.webpage function
###############################################################################
extract.table.from.webpage.test <- function()
{
  load.packages('quantmod')
  
  Symbol = 'IBM'	
  
  # download Key Statistics from yahoo	
  url = paste('http://finance.yahoo.com/q/ks?s=', Symbol, sep = '')
  txt = join(readLines(url))
  
  # extract Valuation Measures table from this page
  temp = extract.table.from.webpage(txt, 'Market Cap', hasHeader = F)
  temp = rbind(c('', Symbol), temp)	# add header row
  
  
  # download IBM price history from Yahoo
  data = getSymbols(Symbol, from = '1980-01-01', auto.assign = FALSE)
  
  # prepare IBM data for 2010:2011 and compute 50 days moving average
  y = data['2010::2011']
  sma50 = SMA(Cl(y), 50)
  
  png(filename = 'plot1.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
  
  # plote candles and volume and table	
  layout(c(1,1,2,3,3))		
  
  plota(y, type = 'candle', main = Symbol, plotX = F)
  plota.lines(sma50, col='blue')
  plota.legend(c(Symbol,'SMA 50'), 'green,blue', list(y,sma50))
  
  y = plota.scale.volume(y)
  plota(y, type = 'volume')		
  
  plot.table(temp)	
  
  dev.off()		
}


###############################################################################
# Pricing Zero Coupon Bond (i.e. yield to price)
# http://thinkanddone.com/finance/valuation-of-zero-coupon-bonds.html
#' @export 
###############################################################################
PricingZeroCouponBond <- function
( 
  yield, 
  timetomaturity, 
  parvalue = 100 
)
{
  parvalue / ( 1 + yield ) ^ timetomaturity  
}

###############################################################################
# Convert Historical TBills rates to Total Returns
# http://timelyportfolio.blogspot.com/2011/04/historical-sources-of-bond-returns_17.html
# http://timelyportfolio.blogspot.ca/2012/11/cashopportunity-lost-or-opportunity.html
#' @export 
###############################################################################
processTBill <- function 
( 
  yields, 
  timetomaturity = 1/4,
  frequency = 365
)
{
  yield = coredata(yields) / 100
  
  # price return
  pr = sapply( yield, function(x) PricingZeroCouponBond(x, timetomaturity) )
  pr = ROC(pr, type='discrete')
  pr[1] = 0
  
  # interest return
  ir = (1+mlag(yield, nlag=1))^(1 / frequency)-1
  #ir = mlag(yield, nlag=1) / frequency 
  ir[1] = 0
  
  # total return
  tr = pr + ir
  
  #out = as.xts( cbind(pr, ir, tr), index(yields) )
  #	colnames(out) = spl('PR,IR,TR')
  
  
  close.price = cumprod(1 + pr)
  adjusted.price = cumprod(1 + tr)
  
  out = as.xts( cbind(close.price, adjusted.price), index(yields) )
  colnames(out) = spl('Close,Adjusted')
  
  return(out)
}


processTBill.test <- function()	
{
  #*****************************************************************
  # Get 1 year t-bill
  #****************************************************************** 	
  quantmod::getSymbols("GS1", src = "FRED")
  ir = (1 + mlag(GS1) / 100) ^ (1/12) - 1
  ir[1] = 0
  
  out = processTBill(GS1, timetomaturity = 1,12)
  
  plota(cumprod(1 + ir), type='l', log = 'y')
  plota.lines(Ad(out), type='l', col='red')
  
  #*****************************************************************
  # Get 3 years t-bill
  #****************************************************************** 	
  SHY = getSymbols('SHY', src='yahoo', auto.assign = FALSE)	
  
  tbill.m = quantmod::getSymbols('GS3', src='FRED', auto.assign = FALSE)	
  tbill.d = quantmod::getSymbols('DGS3', src='FRED', auto.assign = FALSE)	
  timetomaturity = 3
  
  compute.raw.annual.factor(tbill.d)
  compute.raw.annual.factor(tbill.m)
  
  # compute returns
  tbill.m = processTBill(tbill.m, timetomaturity = timetomaturity, 12)
  #index(tbill.m) = as.Date(paste('1/', format(index(tbill.m), '%m/%Y'), sep=''), '%d/%m/%Y')
  
  tbill.d[] = ifna.prev(tbill.d)		
  tbill.d = processTBill(tbill.d, timetomaturity = timetomaturity,261)
  
  
  # scale to start at 1	
  dates = '2003::'
  tbill.m = tbill.m[dates,2]
  tbill.m = tbill.m / as.double(tbill.m[1])
  tbill.d = tbill.d[dates,2]
  tbill.d = tbill.d / as.double(tbill.d[1])
  SHY = Ad(SHY[dates,])
  SHY = SHY / as.double(SHY[1])
  
  # plot
  plota(tbill.d, type='l')		
  plota.lines(tbill.m, type='s', col='blue')								
  plota.lines(SHY, type='l', col='red')
  plota.legend('Daily 3YR T-Bills,Monthly 3YR T-Bills,SHY','black,blue,red')
  
}


###############################################################################
# Load CRB Commodities Index 
# http://www.jefferies.com/cositemgr.pl/html/ProductsServices/SalesTrading/Commodities/ReutersJefferiesCRB/IndexData/index.shtml
###############################################################################
# ... parameters for read.xls function
# i.e. CRB = get.CRB(perl = 'c:/perl/bin/perl.exe')
#
# This url is not working anymore, for updated example please see
#   bt.extend.DBC.update.test in bt.test.r 
###############################################################################
get.CRB <- function(...)
{
  load.packages('gtools,gdata')
  
  #http://www.jefferies.com/html/ProductsServices/SalesTrading/Commodities/scripts/genExcel.pl?Index=RJCRB_Excess&StartDate=19940103&EndDate=20111202
  url = paste('http://www.jefferies.com/html/ProductsServices/SalesTrading/Commodities/scripts/genExcel.pl?Index=RJCRB_Total&StartDate=19940101&EndDate=', format(Sys.Date(), '%Y%m%d'), sep='')	
  temp = read.xls(url, ...)
  temp = as.matrix(temp[-c(1:7),])
  
  out = repmat(as.double(temp[,2]), 1, 6)
  colnames(out) = spl('Open,High,Low,Close,Volume,Adjusted')
  out[, 'Volume'] = 0
  #out = make.xts( out, as.Date(temp[,1], '%m/%d/%y'))
  out = make.xts( out, as.POSIXct(temp[,1], tz = Sys.getenv('TZ'), format='%m/%d/%y'))	
  indexClass(out) = 'Date'	
  
  return(out)
} 	


get.CRB.test <- function()	
{
  #*****************************************************************
  # Load historical data
  #****************************************************************** 	
  CRB = get.CRB()
  
  load.packages('quantmod')	
  # http://etfdb.com/
  tickers = spl('GSG,DBC')		
  getSymbols(tickers, src = 'yahoo', from = '1970-01-01')
  
  #*****************************************************************
  # Compare different indexes
  #****************************************************************** 	
  out = na.omit(merge(Cl(CRB), Cl(GSG), Cl(DBC)))
  colnames(out) = spl('CRB,GSG,DBC')
  temp = out / t(repmat(as.vector(out[1,]),1,nrow(out)))
  
  layout(1:2)
  plota(temp, ylim=range(temp))
  plota.lines(temp[,1],col=1)
  plota.lines(temp[,2],col=2)
  plota.lines(temp[,3],col=3)
  plota.legend(colnames(temp),1:3)
  
  temp = cor(temp / mlag(temp)- 1, use='complete.obs', method='pearson')
  temp[] = plota.format(100 * temp, 0, '', '%')
  plot.table(temp)	
  
  
  layout(1:3)	
  plota.matplot(CRB[,c('Close','Adjusted')])	
  plota.matplot(DBC[,c('DBC.Close','DBC.Adjusted')])	
  plota.matplot(GSG[,c('GSG.Close','GSG.Adjusted')])	
  
  
  layout(1)				
  comm = extend.data(DBC, CRB, scale=T)
  plota(comm, type='l', col=1)
  plota.lines(CRB*0.078, type='l', lwd=5, col=col.add.alpha(2,150))
  plota.lines(DBC, type='l', lwd=5, col=col.add.alpha(3,150))
  plota.lines(comm, type='l', col=1)
  plota.legend('comm,CRB,DBC', 1:3, list(comm,CRB,DBC))
}	


###############################################################################
# Get Dow Jones Components
# http://finance.yahoo.com/q/cp?s=^DJI+Components
#' @export 
###############################################################################
dow.jones.components <- function()
{
  url = 'http://finance.yahoo.com/q/cp?s=^DJI+Components'
  txt = join(readLines(url))
  
  # extract table from this page
  temp = extract.table.from.webpage(txt, 'Volume', hasHeader = T)
  tickers = temp[, 'Symbol']
  
  return(tickers)
}

###############################################################################
# Get NASDAQ 100 Components
# http://www.nasdaq.com/markets/indices/nasdaq-100.aspx
#' @export 
###############################################################################
nasdaq.100.components <- function()
{
  url = 'http://www.nasdaq.com/markets/indices/nasdaq-100.aspx'
  txt = join(readLines(url))
  
  # extract table from this page
  temp = extract.table.from.webpage(txt, 'Symbol', hasHeader = 2)
  tickers = temp[, 'Symbol']
  
  return(tickers)
}


###############################################################################
# Get Sector SPDR Components
# http://www.sectorspdr.com/spdr/composition/?symbol=XLE
# tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU')
# tickers.desc = spl('ConsumerCyclicals,ConsumerStaples,Energy,Financials,HealthCare,Industrials,Materials,Technology,Utilities')
#' @export 
###############################################################################
sector.spdr.components <- function(sector.etf = 'XLE')
{
  url = paste('http://www.sectorspdr.com/spdr/composition/?symbol=', sector.etf, sep='')
  txt = join(readLines(url))
  
  # extract table from this page
  temp = extract.table.from.webpage(txt, 'Symbol', hasHeader = T)
  tickers = temp[, 'Symbol']
  
  return(tickers)
}


###############################################################################
# S&P 500 Components
# http://en.wikipedia.org/wiki/List_of_S%26P_500_companies
#' @export 
###############################################################################
sp500.components <- function()
{
  url = 'http://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
  txt = join(readLines(url))
  
  # extract table from this page	
  temp = extract.table.from.webpage(txt, 'Ticker', hasHeader = T)
  tickers = temp[, 'Ticker symbol']
  sector = temp[, 'GICS Sector']
  
  return(list(tickers=tickers, sector=sector))
}

# List of sites that keep SP500 Components
# http://www.s-p-500.com/stocks-a-b/
#http://www.forexpros.com/indices/us-spx-500-components
#http://marketvolume.com/indexes_exchanges/sp500_components.asp
#http://en.wikipedia.org/wiki/List_of_S%26P_500_companies
#http://en.wikipedia.org/wiki/Dow_Jones_Index


###############################################################################
# S&P 100 Components
# http://www.barchart.com/stocks/sp100.php
#' @export 
###############################################################################
sp100.components <- function()
{
  url = 'http://www.barchart.com/stocks/sp100.php'
  txt = join(readLines(url))
  
  # extract table from this page	
  temp = extract.table.from.webpage(txt, 'Components', hasHeader = T)
  i.start = grep('Name', temp[,2])
  tickers = trim(temp[-c(1:i.start), 1])
  
  return(tickers)	
}


###############################################################################
# iShares FTSE 100 (ISF)
# http://uk.ishares.com/en/rc/products/ISF/all-holdings/
# http://www.londonstockexchange.com/exchange/prices-and-markets/stocks/indices/constituents-indices.html?index=UKX
# Yahoo ticker for UK stocks ABF.L
#' @export 
###############################################################################
ftse100.components <- function()
{
  # get holdings from uk.ishares.com
  url = 'http://uk.ishares.com/en/rc/products/ISF/all-holdings/'
  txt = join(readLines(url))
  
  # extract table from this page		
  txt = gsub('&#37;','%',txt)
  temp = extract.table.from.webpage(txt, 'Security', hasHeader = T)
  
  temp = trim(temp)
  colnames(temp) = temp[1,]
  temp = temp[-1,]		
  holdings = temp
  
  
  # get ISIN to ticker map from www.londonstockexchange.com
  page.label = ''	
  ticker2ISIN = c()
  for(i in 1:100) {	
    cat(i,'\n')
    
    # download
    url = paste('http://www.londonstockexchange.com/exchange/prices-and-markets/stocks/indices/constituents-indices.html?index=UKX&page=', i, sep='')
    txt = join(readLines(url))
    
    # get page label	
    pos = regexpr('Page [0-9]+ of [0-9]+', txt, ignore.case = T)
    page.label.new = substr(txt, pos, pos + attr(pos, 'match.length')-1)
    
    if(page.label == page.label.new) break
    page.label = page.label.new
    
    # extract table
    temp.table = extract.table.from.webpage(txt, 'Price', hasHeader = T)
    colnames(temp.table)[1] = 'tickers'
    
    # extract links
    temp = gsub(pattern = '<a', replacement = '<td>', txt, perl = TRUE)
    temp = gsub(pattern = '</a>', replacement = '</td>', temp, perl = TRUE)	
    
    temp = extract.table.from.webpage(temp, 'Price', hasHeader = T)
    pos = regexpr('fourWayKey=', temp[,2])
    ISIN = as.vector(sapply(1:nrow(temp), function(j) 
      substr(temp[j,2], pos[j] + attr(pos, 'match.length')[j], pos[j] + attr(pos, 'match.length')[j] + 12 - 1)
    ))
    
    
    ticker2ISIN = rbind(ticker2ISIN, cbind(temp.table[,spl('ticker,Name,Price'), drop=F], ISIN))
  }
  
  ISIN = intersect(holdings[,'ISIN'],ticker2ISIN[,'ISIN'])
  holdings = cbind(holdings[match(ISIN, holdings[,'ISIN']), ],
                   ticker2ISIN[match(ISIN, ticker2ISIN[,'ISIN']), spl('ticker,Name,Price')])
  
  return(apply(holdings, 2, list))
}


###############################################################################
# Get the latest prices from the Google finance:
# http://digitalpbk.com/stock/google-finance-get-stock-quote-realtime
#  http://finance.google.com/finance/info?client=ig&q=MSFT,AAPL,NYSE:RY
#' @export 
###############################################################################
#getQuote.google(spl('MSFT,AAPL,IBM'))
getQuote.google <- function(tickers) {
  url = paste('http://finance.google.com/finance/info?client=ig&q=', join(tickers,','), sep='')
  txt = join(readLines(url))	
  temp = gsub(':', ',', txt) 	
  temp = scan(text = temp, what='', sep=',', quiet=T)
  temp = matrix(trim(temp), nr=len(temp)/len(tickers), byrow=F)
  
  index = match(spl('t,l,lt'), tolower(temp[,1]))+1
  names(index) = spl('ticker,last,date')
  
  last = as.double(temp[index['last'],])
  date = strptime(temp[index['date'],],format=' %b %d, %H,%M')
  
  out = data.frame(last,date)
  rownames(out) = temp[index['ticker'],]
  out
}

# an xml alternative
# http://www.jarloo.com/google-stock-api/	
#  http://www.google.com/ig/api?stock=AAPL&stock=GOOG
#getQuote.google.xml(spl('MSFT,AAPL,NYSE:RY'))
#' @export 
getQuote.google.xml <- function(tickers) {
  url = paste('http://www.google.com/ig/api?', paste('stock=',tickers, '&', sep='', collapse=''), sep='')
  txt = join(readLines(url))	
  
  temp = txt		
  temp = gsub('<finance.*?>', '', temp, perl = TRUE) 
  temp = gsub('</finance>', '', temp, perl = TRUE) 
  temp = gsub('<xml.*?>', '', temp, perl = TRUE) 
  temp = gsub('</xml.*?>', '', temp, perl = TRUE) 
  temp = gsub('<\\?xml.*?>', '', temp, perl = TRUE) 
  temp = gsub('data=', '', temp, perl = TRUE) 
  temp = gsub('/><', ' ', temp) 	
  temp = gsub('>', '', temp) 	
  temp = gsub('<', '', temp) 	
  temp = scan(text = temp, what='', sep=' ', quiet=T)
  temp = matrix(trim(temp), nr=len(temp)/len(tickers), byrow=F)
  
  cnames = spl('trade_date_utc,trade_time_utc,symbol,last,high,low,volume,open,avg_volume,market_cap,y_close')
  index = match(cnames, tolower(temp[,1]))+1
  names(index) = cnames
  
  date = strptime(paste(temp[index['trade_date_utc'],], temp[index['trade_time_utc'],]), format='%Y%m%d %H%M%S',tz='UTC')
  date = as.POSIXct(date, tz = Sys.getenv('TZ'))
  
  out = data.frame(t(temp[index[-c(1:3)],]))
  colnames(out) = cnames[-c(1:3)]	
  rownames(out) = temp[index['symbol'],]
  out
}

###############################################################################
# extend GLD and SLV historical prices with data from KITCO
# http://wikiposit.org/w?filter=Finance/Commodities/
# http://www.hardassetsinvestor.com/interviews/2091-golds-paper-price.html
#' @export 
###############################################################################
extend.GLD <- function(GLD) {
  extend.data(GLD, KITCO.data('Gold.PM') / 10)
}

#' @export 
extend.SLV <- function(SLV) {
  extend.data(SLV, KITCO.data('Silver'))
}

#' @export 
KITCO.data <- function
(
  symbol = spl('Gold.AM,Gold.PM,Silver,Platinum.AM,Platinum.PM,Palladium.AM,Palladium.PM')
)
{
  url = 'http://wikiposit.org/w?action=dl&dltypes=comma%20separated&sp=daily&uid=KITCO'
  temp = read.csv(url, skip=4, header=TRUE, stringsAsFactors=F)
  
  #hist = make.xts(as.double(temp[,symbol]), as.Date(temp[,1], '%d-%b-%Y'))
  hist = make.xts(as.double(temp[,symbol]), as.POSIXct(temp[,1], tz = Sys.getenv('TZ'), format='%d-%b-%Y'))	
  indexClass(hist) = 'Date'
  colnames(hist)='Close'
  return( hist[!is.na(hist)] )
}


# gold = extend.GLD(data$GLD)
# comm = extend.data(data$DBC, get.CRB(), scale=T)
#' @export 
extend.data <- function
(
  current,
  hist,
  scale = F
) 
{
  # find Close in hist
  close.index = find.names('Close', colnames(hist))$Close
  if(is.na(close.index)) close.index = 1
  
  if(scale) {
    # find first common observation in current and hist series
    common = merge(Cl(current), hist[,close.index], join='inner')
    
    scale = as.numeric(common[1,1]) / as.numeric(common[1,2])
    
    hist = hist * scale
  }
  
  # subset history before current
  hist = hist[format(index(current[1])-1,'::%Y:%m:%d'),,drop=F]
  
  
  if( ncol(hist) != ncol(current) )	
    hist = make.xts( rep.col(hist[,close.index], ncol(current)), index(hist))
  colnames(hist) = colnames(current)
  
  rbind( hist, current )
}


###############################################################################
# Bundes Bank - long history of gold prices
# http://www.bundesbank.de/Navigation/EN/Statistics/Time_series_databases/Macro_economic_time_series/its_list_node.html?listId=www_s331_b01015_3
# http://wikiposit.org/w?filter=Finance/Commodities/
#' @export 
###############################################################################  
bundes.bank.data <- function(symbol) {
  url = paste('http://www.bundesbank.de/cae/servlet/CsvDownload?tsId=', symbol, '&its_csvFormat=en&mode=its', sep='')
  temp = read.csv(url, skip=5, header=F, stringsAsFactors=F)
  
  #hist = make.xts(as.double(temp[,2]), as.Date(temp[,1], '%Y-%m-%d'))		
  hist = make.xts(as.double(temp[,2]), as.POSIXct(temp[,1], tz = Sys.getenv('TZ'), format='%Y-%m-%d'))
  indexClass(hist) = 'Date'			
  colnames(hist)='Close'
  return( hist[!is.na(hist)] )
}

#' @export 
bundes.bank.data.gold <- function() {
  bundes.bank.data('BBK01.WT5512')
}


###############################################################################
# Pacific Exchange Rate Service - FX rates
# Daily data is maximum for 4 years
# http://fx.sauder.ubc.ca/data.html
# http://fx.sauder.ubc.ca/cgi/fxdata?b=USD&c=AUD&c=GBP&c=CAD&c=NOK&c=EUR&c=JPY&c=NZD&c=SEK&c=CHF&rd=&fd=1&fm=1&fy=2011&ld=31&lm=12&ly=2012&y=daily&q=volume&f=csv&o= 
#
# Example
# base.cur = 'USD'
# target.curs = 'AUD,CAD,EUR'
# fx.data = rbind(fx.sauder.data(2000, 2003, base.cur, target.curs), 
#				fx.sauder.data(2004, 2007, base.cur, target.curs), 
#				fx.sauder.data(2008, 2011, base.cur, target.curs),
#				fx.sauder.data(2012, 2012, base.cur, target.curs))
#' @export 
###############################################################################  
fx.sauder.data <- function(start.year, end.year, base.cur, target.curs) {
  url = paste('http://fx.sauder.ubc.ca/cgi/fxdata?b=', base.cur, join(paste('&c=', spl(target.curs), sep='')), '&rd=&fd=1&fm=1&fy=', start.year, '&ld=31&lm=12&ly=', end.year, '&y=daily&q=volume&f=csv&o=', sep='')
  temp = read.csv(url, skip=1, header=T, stringsAsFactors=F)
  
  #hist = make.xts(as.matrix(temp[,-c(1:3)]), as.Date(temp[,2], '%Y/%m/%d'))		
  hist = make.xts(as.matrix(temp[,-c(1:3)]), as.POSIXct(temp[,2], tz = Sys.getenv('TZ'), format='%Y/%m/%d'))
  indexClass(hist) = 'Date'
  colnames(hist) = gsub(paste('.', base.cur, sep=''), '', colnames(hist))
  
  return( hist[!is.na(hist[,1]),] )
}


###############################################################################
# Download historical prices from Pi Trading - Free Market Data
# http://pitrading.com/free_market_data.htm
#' @export 
###############################################################################
getSymbols.PI <- function
(
  Symbols, 
  env = .GlobalEnv, 
  auto.assign = TRUE,
  download = TRUE	
) 
{
  # setup temp folder
  temp.folder = paste(getwd(), 'temp', sep='/')
  dir.create(temp.folder, F)
  
  # read all Symbols
  for (i in 1:len(Symbols)) {	
    if(download) {
      # http://pitrading.com/free_eod_data/SPX.zip
      url = paste('http://pitrading.com/free_eod_data/', Symbols[i], '.zip', sep='')
      filename = paste(temp.folder, '/', Symbols[i], '.zip', sep='')			
      download.file(url, filename,  mode = 'wb')
      
      # unpack
      unzip(filename, exdir=temp.folder)	
    }
    
    filename = paste(temp.folder, '/', Symbols[i], '.txt', sep='')
    
    temp = read.delim(filename, header=TRUE, sep=',')		
    #out = make.xts(temp[,-1], as.Date(temp[,1],'%m/%d/%Y'))
    out = make.xts(temp[,-1], as.POSIXct(temp[,1], tz = Sys.getenv('TZ'), format='%m/%d/%Y'))
    indexClass(out) = 'Date'
    out$Adjusted = out$Close
    
    cat(i, 'out of', len(Symbols), 'Reading', Symbols[i], '\n', sep='\t')					
    
    if (auto.assign) {		
      assign(paste(gsub('\\^', '', Symbols[i]), sep='_'), out, env)	
    }	
  }
  if (!auto.assign) {
    return(out)
  } else {		
    return(env)				
  }	
}



###############################################################################
# Download FX qoutes: end of day and hourly
# http://www.fxhistoricaldata.com/EURUSD/
#' @export 
###############################################################################
getSymbols.fxhistoricaldata <- function
(
  Symbols, 
  type = spl('hour,day'),
  env = .GlobalEnv, 
  auto.assign = TRUE,
  download = FALSE	
) 
{		
  type = type[1]
  
  # setup temp folder
  temp.folder = paste(getwd(), 'temp', sep='/')
  dir.create(temp.folder, F)
  
  # read all Symbols
  for (i in 1:len(Symbols)) {	
    if(download) {
      # http://www.fxhistoricaldata.com/download/EURUSD?t=hour
      url = paste('http://www.fxhistoricaldata.com/download/', Symbols[i], '?t=', type, sep='')
      filename = paste(temp.folder, '/', Symbols[i], '_', type, '.zip', sep='')			
      download.file(url, filename,  mode = 'wb')
      
      # unpack
      unzip(filename, exdir=temp.folder)	
    }
    
    filename = paste(temp.folder, '/', Symbols[i], '_', type, '.csv', sep='')
    
    temp = read.delim(filename, header=TRUE, sep=',')		
    colnames(temp) = gsub('[X\\.|\\.]', '', colnames(temp))			
    out = make.xts(temp[,spl('OPEN,LOW,HIGH,CLOSE')], 
                   strptime(paste(temp$DATE, temp$TIME), format='%Y%m%d %H:%M:%S'))
    
    cat(i, 'out of', len(Symbols), 'Reading', Symbols[i], '\n', sep='\t')					
    
    if (auto.assign) {		
      assign(paste(gsub('\\^', '', Symbols[i]), type, sep='_'), out, env)	
    }	
  }
  if (!auto.assign) {
    return(out)
  } else {		
    return(env)				
  }	
}





###############################################################################
# Download historical data for G10
# The PowerShares DB G10 Currency Harvest Fund
# http://www.invescopowershares.com/products/overview.aspx?ticker=DBV
#
# The G10 currency universe from which the Index selects currently includes 
# U.S. dollars, 
# euros, 
# Japanese yen, 
# Canadian dollars, 
# Swiss francs, 
# British pounds, 
# Australian dollars, 
# New Zealand dollars, 
# Norwegian krone and 
# Swedish krona
#' @export 
###############################################################################
get.G10 <- function
(
  type = spl('currency')
)
{
  if( type[1] != 'currency') {
    cat('Warning:', type[1], 'is not yet implemented in getG10 function\n')
    return()
  }
  
  # FRED acronyms for daily FX rates
  map = '
  FX          FX.NAME        
  DEXUSAL     U.S./Australia 
  DEXUSUK     U.S./U.K.      
  DEXCAUS     Canada/U.S.    
  DEXNOUS     Norway/U.S.    
  DEXUSEU     U.S./Euro      
  DEXJPUS     Japan/U.S.     
  DEXUSNZ     U.S./NewZealand
  DEXSDUS     Sweden/U.S.    
  DEXSZUS     Switzerland/U.S.
  '
  
  map = matrix(scan(text = map, what='', quiet=T), nc=2, byrow=T)
  colnames(map) = map[1,]
  map = data.frame(map[-1,], stringsAsFactors=F)
  
  # convert all quotes to be vs U.S.
  convert.index = grep('DEXUS',map$FX, value=T)	
  
  #*****************************************************************
  # Load historical data
  #****************************************************************** 
  load.packages('quantmod')
  
  # load fx from fred
  data.fx <- new.env()
  quantmod::getSymbols(map$FX, src = 'FRED', from = '1970-01-01', env = data.fx, auto.assign = T)		
  for(i in convert.index) data.fx[[i]] = 1 / data.fx[[i]]
  
  # extract fx where all currencies are available
  bt.prep(data.fx, align='remove.na')
  fx = bt.apply(data.fx, '[')
  
  return(fx)
}




###############################################################################
# getSymbols interface to tradingblox free futures and forex data
# http://www.tradingblox.com/tradingblox/free-historical-data.htm
# http://www.tradingblox.com/Data/DataOnly.zip
# Date, Open, High, Low, Close, Volume (zero for forex cash markets), 
# Open Interest (futures only), Delivery Month ( YYYYMM futures only), 
# Unadjusted Close (zero for forex cash markets)
#' @export 
###############################################################################			
getSymbols.TB <- function(
  env = .GlobalEnv, 
  auto.assign = TRUE,
  download = FALSE,
  type = c('Both', 'Futures', 'Forex'),
  rm.index =  'PB', 	# remove Pork Bellies because not traded
  clean = FALSE
) 
{
  # download zip archive
  if(download) {
    download.file('http://www.tradingblox.com/Data/DataOnly.zip', 'DataOnly.zip')
  }
  
  # setup temp folder
  temp.folder = paste(getwd(), 'temp', sep='/')
  dir.create(temp.folder, F)
  
  ##*****************************************************************
  ##	Unzip
  ##****************************************************************** 
  temp.folder = paste(getwd(), '/', 'temp', sep='')
  
  # clean temp
  if(clean) shell('del /F /S /Q temp\\*.*', wait = TRUE)
  
  # unpack
  files = unzip('DataOnly.zip', exdir=temp.folder)	
  
  # read definitions, based on Financial Instrument Model Infrastructure for R package from http://r-forge.r-project.org/R/?group_id=316
  def1 = try(read.csv('http://www.tradingblox.com/tradingblox/CSIUA/FuturesInfo.txt',skip=1,header=FALSE, stringsAsFactors=F),TRUE) 
  if(inherits(def1, 'try-error')) def1 = read.csv('FuturesInfo.txt',skip=1,header=FALSE, stringsAsFactors=F)			
  def1 = def1[-match(rm.index, def1[,1]),]	
  def1[,3] = 'Futures'
  
  def2 = try(read.csv('http://www.tradingblox.com/tradingblox/CSIUA/ForexInfo.txt',skip=1,header=FALSE, stringsAsFactors=F),TRUE)
  if(inherits(def2, 'try-error')) def2 = read.csv('ForexInfo.txt',skip=1,header=FALSE, stringsAsFactors=F)	 		
  def2[,3] = 'Forex'
  
  def = rbind(def1[,1:4], def2[,1:4]) 			
  if(type[1] == 'Futures') def = def1[,1:4]
  if(type[1] == 'Forex') def = def2[,1:4]	
  
  
  # read all files
  for( i in 1:nrow(def) ) {
    symbol = def[i,1]
    
    filename = paste(temp.folder, '/', def[i,3], '/', def[i,4], sep='')
    if(file.exists(filename)) {										
      fr <- read.csv(filename, header = FALSE) 
      fr <- make.xts(fr[,-1], as.Date(as.character(fr[,1]),'%Y%m%d'))			
      colnames(fr) <- spl('Open,High,Low,Close,Volume,OpenInterest,DeliveryMonth,Unadjusted')[1:ncol(fr)]
      fr$Adjusted = fr$Close
      if (auto.assign) assign(symbol, fr, env)
      cat(i, 'out of', nrow(def), 'Reading', symbol, format(index.xts(fr)[1],'%Y%m%d'), format(index.xts(fr)[nrow(fr)],'%Y%m%d'), '\n', sep='\t')		
    } else {
      cat('\t\t\t Missing data for ', symbol, '\n');
    }
  }
  
  #*****************************************************************
  # Add symbolnames, symbol.descriptions, and symbol.groups
  #****************************************************************** 		
  index = match(ls(env)[ na.omit(match(def[,1], ls(env))) ], def[,1])	
  
  
  temp = def[index,1]
  names(temp) = def[index,1]
  env$symbolnames = temp
  
  temp = def[index,2]
  names(temp) = def[index,1]
  env$symbol.descriptions = temp
  
  temp = def[index,3]
  names(temp) = def[index,1]
  env$symbol.groups = temp    	    	
  
  #*****************************************************************
  # Process symbol descriptions to be more readable
  #****************************************************************** 	
  names = trim(gsub(pattern = '\\(.*?\\)', replacement = '', env$symbol.descriptions, perl = TRUE))
  names = trim(gsub('-NYMEX','',names,ignore.case =T))
  names = trim(gsub('-COMEX','',names,ignore.case =T))
  names = trim(gsub('-CBT','',names,ignore.case =T))
  names = trim(gsub('-CME-','',names,ignore.case =T))
  names = trim(gsub('-CME','',names,ignore.case =T))
  names = trim(gsub('-NYCE','',names,ignore.case =T))
  names = trim(gsub('-Globex','',names,ignore.case =T))
  names = trim(gsub('-FINEX','',names,ignore.case =T))
  names = trim(gsub('-CSCE','',names,ignore.case =T))
  names = trim(gsub(' w/Prj A','',names,ignore.case =T))
  
  env$symbol.descriptions.print = names
  
  #*****************************************************************
  # Custom adjustments 
  #****************************************************************** 			
  data = env
  
  # fix DX time series - fixed by the Trading Blox
  # if(!is.null(data$DX)) data$DX['::2007:04:04', 'Unadjusted'] = coredata(data$DX['::2007:04:04']$Unadjusted * 10)
  
  #*****************************************************************
  # To compute returns and backtest, recreate each futures series:
  #
  #						(unadjusted-futures[t-1] + (back-adjusted-futures[t] - back-adjusted-futures[t-1]))	
  # futures-return[t] =	--------------------------------------------------------------------------------------------------	- 1
  #						unadjusted-futures[t-1]	
  #****************************************************************** 			
  for(i in data$symbolnames[data$symbol.groups != 'Forex']) {
    # adjust spot for roll overs
    spot = as.vector(data[[i]]$Unadjusted)
    dspot = spot - mlag(spot)
    futures = as.vector(data[[i]]$Adjusted)
    dfutures = futures - mlag(futures)
    index = which(round(dspot - dfutures,4) != 0 )
    
    spot.adjust.roll = spot
    spot.adjust.roll[(index-1)] = spot.adjust.roll[index] - dfutures[index]
    
    # compute returns
    reta = (mlag(spot.adjust.roll) + futures - mlag(futures)) / mlag(spot.adjust.roll)
    reta[1] = 1
    n = len(spot)
    
    new.series = cumprod(reta)
    data[[i]]$Close = spot[n] * new.series / new.series[n]		
    data[[i]]$Adjusted	= data[[i]]$Close
  }
  
  
  #*****************************************************************
  # Done 
  #****************************************************************** 						
  if (!auto.assign) {
    return(fr)
  } else {		
    return(env)
  }	
}





###############################################################################
# Kenneth R. French - Data Library
# http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
###############################################################################
# http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors.zip
# http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_weekly.zip
# http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily.zip
#
# data2 = get.fama.french.data('F-F_Research_Data_Factors', periodicity = 'weeks',download = F, clean = F)
# data3 = get.fama.french.data('6_Portfolios_2x3', periodicity = 'days',download = F, clean = F)	
#' @export 
###############################################################################
get.fama.french.data <- function(
  name = c('F-F_Research_Data_Factors', 'F-F_Research_Data_Factors'),
  periodicity = c('days','weeks', 'months'),
  download = FALSE,
  clean = FALSE
) 
{
  # map periodicity
  map = c('_daily', '_weekly', '')
  names(map) = c('days','weeks', 'months')
  
  # url
  filename.zip = paste(name[1], map[periodicity[1]], '.zip', sep='')
  filename.txt = paste(name[1], map[periodicity[1]], '.txt', sep='')
  url = paste('http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/', filename.zip, sep='')
  
  # download zip archive
  if(download) {
    download.file(url, filename.zip)
  }
  
  # setup temp folder
  temp.folder = paste(getwd(), 'temp', sep='/')
  dir.create(temp.folder, F)
  
  ##*****************************************************************
  ##	Unzip
  ##****************************************************************** 
  temp.folder = paste(getwd(), '/', 'temp', sep='')
  
  # clean temp
  if(clean) shell('del /F /S /Q temp\\*.*', wait = TRUE)
  
  # unpack
  files = unzip(filename.zip, exdir=temp.folder)	
  
  # read data
  filename = paste(temp.folder, '/', filename.txt, sep='')
  out = readLines(filename)
  index = which(nchar(out) == 0)
  
  data.index = grep('^[ 0-9\\.\\+-]+$', out)
  temp.index = which(diff(data.index) > 1)
  data.index = matrix(data.index[sort(c(1, temp.index, temp.index+1, len(data.index)))], nc=2, byrow=T)
  
  # extract sections	
  data = list()	
  for(i in 1:nrow(data.index)) {
    start.index = index[which( index > data.index[i,1] ) - 1][1] + 1
    end.index = data.index[i,1] - 1
    n.index = end.index - start.index + 1
    
    # column names
    name = 'data'
    colnames = scan(text = out[start.index], what='', quiet=T)
    if(n.index == 2) {
      name = trim(out[start.index])
      colnames = scan(text = out[end.index], what='', quiet=T)
    } else if(n.index > 2) {
      name = trim(out[start.index])
      colnames0 = scan(text = out[(end.index-1)], what='', quiet=T)
      colnames1 = scan(text = out[end.index], what='', quiet=T)
      colnames = paste(rep(colnames0, each = len(colnames1) / len(colnames0)), colnames1, sep='.')			
    }
    colnames = gsub('-', '.', colnames)
    #out[start.index:end.index]
    
    # re-read data	
    temp =  matrix(scan(filename, what = double(), quiet=T,
                        skip = (data.index[i,1]-1),  
                        nlines = (data.index[i,2] - data.index[i,1]+1))
                   , nc=len(colnames)+1, byrow=T)
    
    date.format = '%Y%m%d'	
    date.format.add = ''
    date.format.n = nchar(paste(temp[1,1]))
    
    if( date.format.n == 6 )
      date.format.add = '01'		
    else if( date.format.n == 4 )
      date.format.add = '0101'		
    
    data[[name]] = make.xts(temp[,-1], as.Date(paste(temp[,1], date.format.add, sep=''),date.format))
    colnames(data[[name]]) = colnames		
  }
  return( data )
}

###############################################################################
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
###############################################################################
# Collection of General Utilities
# Copyright (C) 2012  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################


###############################################################################
# Convenience Utilities
###############################################################################
#' Split string into tokens using delim
#'
#' This function will split given string into tokens using delim
#'
#' @param s input string
#' @param delim delimiter, \strong{defaults to ","}
#'
#' @return array of tokens
#'
#' @examples
#' \dontrun{ 
#' spl('a,b,c')
#' }
#' @export 
###############################################################################
spl <- function 
(
  s,			# input string
  delim = ','	# delimiter
)
{ 
  return(unlist(strsplit(s,delim))); 
}

###############################################################################
#' Join vector of strings into one string using delim
#'
#' This function will join vector of strings into one string using delim
#'
#' @param v vector of strings
#' @param delim delimiter, \strong{defaults to ","}
#'
#' @return resulting string
#'
#' @examples
#' \dontrun{ 
#' join(c('a','b','c'), ',')
#' }
#' @export 
###############################################################################
join <- function
(
  v, 			# vector of strings
  delim = ''	# delimiter
)
{ 
  return(paste(v,collapse=delim)); 
}

###############################################################################
#' Remnove any leading and trailing spaces
#'
#' This function will remnove any leading and trailing spaces
#'
#' @param s string
#'
#' @return resulting string
#'
#' @examples
#' \dontrun{ 
#' trim('  a b c  ')
#' }
#' @export 
###############################################################################
trim <- function
(
  s	# string
)
{
  s = sub(pattern = '^ +', replacement = '', x = s)
  s = sub(pattern = ' +$', replacement = '', x = s)
  return(s)
}

###############################################################################
#' Shortcut for length function
#'
#' This function is a shortcut for length function
#'
#' @param x vector / string / matrix
#'
#' @return number of elements in x
#'
#' @examples
#' \dontrun{ 
#' len(1:10)
#' }
#' @export 
###############################################################################
len <- function
(
  x	# vector
)
{
  return(length(x)) 
}

###############################################################################
#' Faster version of ifelse function
#'
#' This function is a faster version of ifelse function
#'
#' @param cond true / false condition
#' @param truepart resulting value(s) if condition is true
#' @param falsepart resulting value(s) if condition is false
#'
#' @return number of elements in x
#'
#' @examples
#' \dontrun{ 
#' iif(1:10 > 5, 1, 1:10)
#' }
#' @export 
###############################################################################
iif <- function
(
  cond,		# condition
  truepart,	# true part
  falsepart	# false part
)
{
  if(len(cond) == 1) { if(cond) truepart else falsepart }
  else {  
    if(length(falsepart) == 1) {
      temp = falsepart
      falsepart = cond
      falsepart[] = temp
    }
    
    if(length(truepart) == 1) 
      falsepart[cond] = truepart 
    else {
      cond = ifna(cond,F)
      falsepart[cond] = truepart[cond]
    }
    
    #falsepart[!is.na(cond)] = temp
    
    return(falsepart);
  }
} 

###############################################################################
#' Replace NA, NaN, Inf values
#'
#' This function will replace all NA, NaN, Inf with given values
#'
#' @param x data to check for NA, NaN, Inf
#' @param y values(s) to be used in place of NA, NaN, Inf
#'
#' @return updated data
#'
#' @examples
#' \dontrun{ 
#' ifna(c(1,NA,2,Inf,3), 4)
#' }
#' @export 
###############################################################################
ifna <- function
(
  x,	# check x for NA, NaN, Inf
  y	# if found replace with y
) 
{ 	
  return(iif(is.na(x) | is.nan(x) | is.infinite(x), y, x))
}

#' @export 
fast.na.omit <- function
(
  x
) 
{
  x[!is.na(x)]
}

###############################################################################
#' Replace NULL values
#'
#' This function will replace all NULL with given value
#'
#' @param x data to check for NULL
#' @param y values to be used in place of NULL
#'
#' @return updated data
#'
#' @examples
#' \dontrun{ 
#' temp = list()
#' temp$val1 = ifnull(temp$val1, 4)
#' }
#' @export 
############################################################################### 
ifnull <- function
(
  x,	# check x for NULL
  y	# if found replace with y
) { 	
  return(iif(is.null(x), y, x))
}

###############################################################################
#' Faster version of rep fucntion
#'
#' This function is a faster version of rep fucntion
#'
#' @param x data to be repeated
#' @param times number of times to repeat the data
#'
#' @return new data
#'
#' @examples
#' \dontrun{ 
#' fast.rep(c(1,NA,2,Inf,3), 4)
#' }
#' @export 
###############################################################################
fast.rep <- function(x, times) { 
  length(x) = times
  x[] = x[1]		
  x
}

fast.rep.test.speed <- function() {
  #install.packages('rbenchmark_0.3.tar.gz', repos = NULL, type="source")
  
  test1 <- function() {
    rep(101,10000)
  }
  test2 <- function() {
    fast.rep(101,10000)
  }
  
  require(rbenchmark)
  benchmark(
    test1(), 
    test2(),
    columns = c("test", "replications", "elapsed", "relative"),
    order = "relative",
    replications = 10000
  )
}	

###############################################################################
#' Count number of non NA elements
#'
#' This function will count number of non NA elements in the given matrix
#'
#' @param x data matrix
#' @param side margin along which to count
#'
#' @return counts
#'
#' @examples
#' \dontrun{ 
#' count(matrix(c(1,NA,2,3),2,2))
#' }
#' @export 
###############################################################################
count <- function(
  x,			# matrix with data
  side = 2	# margin along which to count
)
{
  if( is.null(dim(x)) ) { 
    sum( !is.na(x) ) 
  } else { 
    apply(!is.na(x), side, sum) 
  }
}  

###############################################################################
#' Running Count over given window
#'
#' This function will count number of non NA elements over given window
#'
#' @param x data matrix
#' @param window.len window length
#'
#' @return counts
#'
#' @examples
#' \dontrun{ 
#' run.count(matrix(1:9,3,3),2)
#' }
#' @export 
###############################################################################
run.count <- function
(
  x, 			# vector with data
  window.len	# window length
)
{ 
  n    = length(x) 
  xcount = cumsum( !is.na(x) )
  ycount = xcount[-c(1 : (k-1))] - c(0, xcount[-c((n-k+1) : n)])
  return( c( xcount[1:(k-1)], ycount))
}

###############################################################################
#' Dates Functions
#'
#' @param dates collection of dates
#'
#' @return transformed dates
#'
#' @examples
#' \dontrun{ 
#' date.dayofweek(Sys.Date())
#' }
#' @export 
#' @rdname DateFunctions
###############################################################################
date.dayofweek <- function(dates) 
{	
  return(as.double(format(dates, '%w')))
}

#' @export 
#' @rdname DateFunctions
date.day <- function(dates) 
{	
  return(as.double(format(dates, '%d')))
}

#' @export 
#' @rdname DateFunctions
date.week <- function(dates) 
{	
  return(as.double(format(dates, '%U')))
}

#' @export 
#' @rdname DateFunctions
date.month <- function(dates) 
{	
  return(as.double(format(dates, '%m')))
}

# (((1:12)-1) %/% 3)+1	
# date.quarter(Sys.Date())
#' @export 
#' @rdname DateFunctions
date.quarter <- function(dates) 
{	
  (((date.month(dates))-1) %/% 3)+1	
}

#' @export 
#' @rdname DateFunctions
date.year <- function(dates) 
{	
  return(as.double(format(dates, '%Y')))
}


###############################################################################
#' Dates Index Functions
#'
#' @param dates collection of dates
#'
#' @return location of the week/month/year ends
#'
#' @examples
#' \dontrun{ 
#' date.week.ends(seq(Sys.Date()-100, Sys.Date(), 1))
#' }
#' @export 
#' @rdname DateFunctionsIndex
###############################################################################
date.week.ends <- function(dates, last.date=T) 
{	
  ends = which(diff( 100*date.year(dates) + date.week(dates) ) != 0)
  ends.add.last.date(ends, len(dates), last.date)
}

#' @export 
#' @rdname DateFunctionsIndex
date.month.ends <- function(dates, last.date=T) 
{	
  ends = which(diff( 100*date.year(dates) + date.month(dates) ) != 0)
  ends.add.last.date(ends, len(dates), last.date)
}

#' @export 
#' @rdname DateFunctionsIndex
date.quarter.ends <- function(dates, last.date=T) 
{	
  ends = which(diff( 10*date.year(dates) + date.quarter(dates) ) != 0)
  ends.add.last.date(ends, len(dates), last.date)
}

#' @export 
#' @rdname DateFunctionsIndex
date.year.ends <- function(dates, last.date=T) 
{	
  ends = which(diff( date.year(dates) ) != 0)
  ends.add.last.date(ends, len(dates), last.date)
}

# helper function to add last date
ends.add.last.date <- function(ends, last.date, action=T) 
{
  if(action)
    unique(c(ends, last.date))
  else
    ends
}

#' @export 
#' @rdname DateFunctionsIndex
date.ends.fn <- function(periodicity) {
  switch(periodicity,
         'weeks' = date.week.ends,
         'months' = date.month.ends,
         'quarters' = date.quarter.ends,
         'years' = date.year.ends,
         date.month.ends)	
}

#' out is result of the business.days.location.end
#' @export 
#' @rdname DateFunctionsIndex
date.ends.index <- function(out, timing) {
  if(timing <= 0)
    which(out$days.till == (-timing))
  else
    which(out$days.since == (timing))
}



# to ger proper month-end and a day before month-end
# !!!note holidayTSX() is missing CALabourDay
# http://www.tmx.com/en/about_tsx/market_hours.html
# load.packages('RQuantLib')    
# from = as.Date('10Jun2013','%d%b%Y')
# to = as.Date('10Jan2014','%d%b%Y')
# holidays = getHolidayList("UnitedStates/NYSE", from, to) 	
#' @export 
business.days <- function(from, to = as.Date(from) + 31, holidays = NULL) {
  from = as.Date(from)
  to = as.Date(to)
  
  dates = seq(from, to, by='day')
  rm.index = date.dayofweek(dates) == 6 | date.dayofweek(dates) == 0
  if(!is.null(holidays)) {
    holidays = as.Date(holidays)
    rm.index = rm.index | !is.na(match(dates, holidays))        
  }
  dates[!rm.index]
}

# if date is month end, return zero
# from = as.Date('27Dec2013','%d%b%Y')
# holidays = holidayNYSE(date.year(from))
# dates = business.days(from, from + 40, holidays)
# business.days.till.end(from, holidays)
#' @export 
business.days.till.end <- function(from, holidays = NULL, fn.ends = date.month.ends) {
  from = as.Date(from)
  
  # make sure from is a business date
  dates = business.days(from - 10, from, holidays)
  from = dates[len(dates)]
  
  dates = business.days(from, from + 40, holidays)
  index = match.fun(fn.ends)(dates, F)
  index[1] - 1
}

# from = as.Date('3Dec2013','%d%b%Y')
# holidays = holidayNYSE(date.year(from))
# dates = business.days(from - 40, from+10, holidays)
# business.days.since.end(from, holidays)
#' @export  
business.days.since.end <- function(from, holidays = NULL, fn.ends = date.month.ends) {
  from = as.Date(from)
  
  # make sure from is a business date
  dates = business.days(from - 10, from, holidays)
  from = dates[len(dates)]
  
  dates = business.days(from - 40, from + 10, holidays)
  index = match.fun(fn.ends)(dates, F)
  
  last.index = index[len(index)]
  if( dates[last.index] == from) return(0)
  
  from.index = sum(dates <= from)
  if( dates[last.index] < from) return(from.index - last.index)
  
  last.index = index[(len(index) - 1)]
  return(from.index - last.index)
}

next.business.day <- function(from, holidays = NULL, offset = 0) {
  from = as.Date(from)
  
  # make sure from is a business date
  dates = business.days(from + offset, from + 10, holidays)
  dates[1]
}

last.business.day <- function(from, holidays = NULL, offset = 0) {
  from = as.Date(from)
  
  # make sure from is a business date
  dates = business.days(from - 10, from - offset, holidays)
  dates[1]
}

# load.packages('quantmod')
# data = getSymbols('AAPL', auto.assign = F)
# dates = index(data)
# out = business.days.location.end(dates, holidayNYSE)
# cbind(format(dates,'%d%b%y'), out$days.since, out$days.till)
#' @export 
business.days.location.end <- function(dates, calendar = null, fn.ends = date.month.ends) {	
  dates = as.Date(dates)
  n = len(dates)
  
  # getHolidayList
  load.packages('RQuantLib')
  
  holidays = NULL		
  if(!is.null(calendar)) holidays = getHolidayList(calendar, dates[1] - 60, dates[1] - 1) 	
  
  before = business.days(dates[1] - 60, dates[1] - 1, holidays)
  n.before = len(before) 
  
  holidays = NULL		
  if(!is.null(calendar)) holidays = getHolidayList(calendar, dates[n] + 1, dates[n] + 60) 	
  
  after = business.days(dates[n] + 1, dates[n] + 60, holidays)
  
  
  all = c(before, dates, after)
  n.all = len(all)
  all.index = (n.before + 1) : (n.before + n)
  
  index = match.fun(fn.ends)(all, F)
  
  temp.cum = cumsum(fast.rep(1,n.all))
  temp = temp.cum * NA
  temp[index] = temp.cum[index]
  days.since = temp.cum - ifna.prev(temp)
  days.till = temp[ifna.prevx.rev(temp)] - temp.cum
  
  #cbind(format(all,'%d%b%y'), days.since, days.till)[all.index, ]
  
  list(days.since = days.since[all.index], days.till = days.till[all.index])
}


###############################################################################
#' Map given time series to monthly
#'
#' If frequency of observations in the given time series is less than monthly,
#' i.e. quaterly or annually, properly align this time series to monthly
#'
#' @param equity time series
#'
#' @return xts object 
#'
#' @examples
#' \dontrun{ 
#' map2monthly(equity) 
#' }
#' @export 
###############################################################################
map2monthly <- function(equity) 
{
  #a = coredata(Cl(to.monthly(equal.weight$equity)))
  
  if(compute.annual.factor(equity) >= 12) return(equity)
  
  dates = index(equity)
  equity = coredata(equity)
  
  temp = as.Date(c('', 10000*date.year(dates) + 100*date.month(dates) + 1), '%Y%m%d')[-1]
  new.dates = seq(temp[1], last(temp), by = 'month')		
  
  map = match( 100*date.year(dates) + date.month(dates), 100*date.year(new.dates) + date.month(new.dates) ) 
  temp = rep(NA, len(new.dates))
  temp[map] = equity
  
  #range(a - temp)
  
  return( make.xts( ifna.prev(temp), new.dates) )
}

###############################################################################
#' Create monthly table
#'
#' Transform given monthly time series into matrix with Months as columns and Years as rows
#'
#' @param monthly.data monthly time series
#'
#' @return matrix with Months as columns and Years as rows
#'
#' @examples
#' \dontrun{ 
#' create.monthly.table(monthly.ret)
#' }
#' @export 
###############################################################################
create.monthly.table <- function(monthly.data) 
{
  nperiods = nrow(monthly.data)
  
  years = date.year(index(monthly.data[c(1,nperiods)]))
  years = years[1] : years[2]
  
  # create monthly matrix
  temp = matrix( double(), len(years), 12)
  rownames(temp) = years
  colnames(temp) = spl('Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec')
  
  # align months
  index = date.month(index(monthly.data[c(1,nperiods)]))
  temp[] = matrix( c( rep(NA, index[1]-1), monthly.data, rep(NA, 12-index[2]) ), ncol=12, byrow = T)
  
  return(temp)
}

###############################################################################
#' Compute the expiration date of stock options (3rd Friday of the month)
#'
#' @param year year
#' @param month month
#'
#' @return date for the third Friday of the given month and year
#'
#' @references 
#' \url{http://bytes.com/topic/python/answers/161147-find-day-week-month-year}
#'
#' \url{http://www.mysmp.com/options/options-expiration-week.html}
#' The week beginning on Monday prior to the Saturday of options expiration is referred to as options expiration week. 
#' Since the markets are closed on Saturday, the third Friday of each month represents options expiration.
#' If the third Friday of the month is a holiday, all trading dates are moved forward; meaning that Thursday will be the last trading day to exercise options.
#'
#' \url{http://www.cboe.com/TradTool/ExpirationCalendar.aspx}
#'
#' @examples
#' \dontrun{ 
#' third.friday.month(2012,1)
#' }
#' @export 
###############################################################################
third.friday.month <- function(year, month)
{
  day = date.dayofweek( as.Date(c('', 10000*year + 100*month + 1), '%Y%m%d')[-1] )
  day = c(20,19,18,17,16,15,21)[1 + day]
  return(as.Date(c('', 10000*year + 100*month + day), '%Y%m%d')[-1])
}

###############################################################################
#' Determine the index of subset of dates in the time series
#'
#' @param x xts time series
#' @param dates string represnting subset of dates i.e. '2010::2012'
#'
#' @return index of subset of dates in the time series
#'
#' @examples
#' \dontrun{ 
#' dates2index(data$prices, '2010::2012') 
#' 
#' data = textConnection('
#' date,Close
#' 2013-03-18,    154.97
#' 2013-03-19,    154.61
#' 2013-03-20,    155.69
#' 2013-03-21,    154.36
#' 2013-03-22,    155.60
#' 2013-03-25,    154.95')
#' 
#' x = read.xts(data)
#' dates2index(x, '2013-03-19')
#' 
#' }
#' @export 
###############################################################################
dates2index <- function( x, dates = 1:nrow(x) ) {
  dates.index = dates
  if(!is.numeric(dates)) {
    temp = x[,1]
    temp[] = 1:nrow(temp)
    dates.index = as.numeric(temp[dates])
  }
  return(dates.index)
} 

###############################################################################
#' Load Packages that are available and install ones that are not available
#'
#' This function a convience wrapper for install.packages() function
#'
#' @param packages names of the packages separated by comma
#' @param repos default repository
#' @param dependencies type of dependencies to install
#' @param ... additional parameters for the \code{\link{install.packages}} function
#'
#' @return nothing
#'
#' @examples
#' \dontrun{ 
#' load.packages('quantmod')
#' }
#' @export 
############################################################################### 
load.packages <- function
(
  packages, 							# names of the packages separated by comma
  repos = "http://cran.r-project.org",# default repository
  dependencies = c("Depends", "Imports"),	# install dependencies
  ...									# other parameters to install.packages
)
{
  packages = spl(packages)
  for( ipackage in packages ) {
    if(!require(ipackage, quietly=TRUE, character.only = TRUE)) {
      install.packages(ipackage, repos=repos, dependencies=dependencies, ...) 
      
      if(!require(ipackage, quietly=TRUE, character.only = TRUE)) {
        stop("package", sQuote(ipackage), 'is needed.  Stopping')
      }
    }
  }
}

###############################################################################
#' Begin Timing
#'
#' @param identifier name for this timing session
#'
#' @return nothing
#'
#' @examples
#' \dontrun{ 
#' tic(1)
#' }
#' @export 
#' @rdname TimingFunctions
############################################################################### 
tic <- function
(
  identifier	# integer value
)
{
  assign(paste('saved.time', identifier, sep=''), proc.time()[3], envir = .GlobalEnv)
}

###############################################################################
#' End Timing and report elapsed time
#'
#' @param identifier name for this timing session
#'
#' @return elapsed time
#'
#' @examples
#' \dontrun{ 
#' toc(1)
#' }
#' @export 
#' @rdname TimingFunctions
############################################################################### 
toc <- function
(
  identifier	# integer value
)
{
  if( exists(paste('saved.time', identifier, sep=''), envir = .GlobalEnv) ) {
    prevTime = get(paste('saved.time', identifier, sep=''), envir = .GlobalEnv)
    diffTimeSecs = proc.time()[3] - prevTime
    cat('Elapsed time is', round(diffTimeSecs, 2), 'seconds\n')
  } else {
    cat('Toc error\n')
  }    
  return (paste('Elapsed time is', round(diffTimeSecs,2), 'seconds', sep=' '))
}

test.tic.toc <- function()
{
  tic(10)
  for( i in 1 : 100 ) {
    temp = runif(100)
  }
  toc(10)
}

###############################################################################
#' Lag matrix or vector
#'
#' This function shifts elemnts in a vector or a mtrix by a given lag.
#' For example: mlag(x,1) - use yesterday's values and
#'  mlag(x,-1) - use tomorrow's values
#'
#' @param x vector / matrix
#' @param nlag number of lags, \strong{defaults to 1}
#'
#' @return modified object
#'
#' @examples
#' \dontrun{ 
#' mlag(1:10)
#' }
#' @export 
###############################################################################
mlag <- function
(
  m,			# matrix or vector
  nlag = 1	# number of lags
)
{ 
  # vector
  if( is.null(dim(m)) ) { 
    n = len(m)
    if(nlag > 0) {
      m[(nlag+1):n] = m[1:(n-nlag)]
      m[1:nlag] = NA
    } else if(nlag < 0) {
      m[1:(n+nlag)] = m[(1-nlag):n]
      m[(n+nlag+1):n] = NA
    } 	
    
    # matrix	
  } else {
    n = nrow(m)
    if(nlag > 0) {
      m[(nlag+1):n,] = m[1:(n-nlag),]
      m[1:nlag,] = NA
    } else if(nlag < 0) {
      m[1:(n+nlag),] = m[(1-nlag):n,]
      m[(n+nlag+1):n,] = NA
    } 
  }
  return(m);
}

###############################################################################
#' Replicate and tile a given vector
#'
#' @param v vector
#' @param n number of copies along rows
#' @param m number of copies along columns
#'
#' @return new matrix
#' 
#' @references 
#' \url{http://www.mathworks.com/help/techdoc/ref/repmat.html}
#'
#' @examples
#' \dontrun{ 
#' repmat(1:10,1,2)
#' }
#' @export 
###############################################################################
repmat <- function
(
  v,	# vector
  n,	# number of copies along rows
  m	# number of copies along columns
)
{
  kronecker( matrix(1, n, m), v )
}

###############################################################################
#' Repeat Rows
#'
#' @param m vector (row)
#' @param nr number of copies along rows
#'
#' @return new matrix
#' 
#' @examples
#' \dontrun{ 
#' matrix(1:3, nr=5, nc=3, byrow=T)
#' rep.row(1:3, 5)
#' }
#' @export 
###############################################################################
rep.row <- function
(
  m, # vector (row)
  nr # number of copies along rows
)
{
  matrix(m, nr=nr, nc=len(m), byrow=T)
}

###############################################################################
#' Repeat Columns
#'
#' @param m vector (column)
#' @param nc number of copies along columns
#'
#' @return new matrix
#' 
#' @examples
#' \dontrun{ 
#' matrix(1:5, nr=5, nc=3, byrow=F)
#' rep.col(1:5, 3)
#' }
#' @export 
###############################################################################
rep.col <- function
(
  m,	# vector (column)
  nc	# number of copies along columns
)
{
  matrix(m, nr=len(m), nc=nc, byrow=F)
}

###############################################################################
#' Find location: row, col in the matrix, given index of of observation
#'
#' @param data matrix
#' @param i index of observations
#' @param details flag to provide details, \strong{defaults to FALSE}
#'
#' @return new matrix
#' 
#' @examples
#' \dontrun{ 
#' data = matrix(1:16,4,4)
#' lookup.index(data, which(data > 4))
#' }
#' @export 
# play with following example: update 1 %% 4	
###############################################################################
lookup.index <- function
(
  data, 	# matrix
  i, 		# index of observations
  details = F	# flag to return additional details
) 
{
  n = nrow(data)
  irow = ((i - 1) %% n) + 1	
  icol = ((i - 1) %/% n) +1 
  if(details)
    list(irow=irow,icol=icol,obs=data[irow,icol],obsr=data[max(0,irow-5):min(nrow(data),irow+5),icol])
  else
    list(irow=irow,icol=icol)
}

###############################################################################
#' Convert beta (slope of reggression line) to degrees
#'
#' @param beta slope of regression line
#'
#' @return angle
#' 
#' @references 
#' \url{http://r.789695.n4.nabble.com/slope-calculation-td858652.html	}
#'
#' @examples
#' \dontrun{ 
#' beta.degree(1)
#' }
#' @export 
###############################################################################
beta.degree <- function(beta) 
{ 
  atan(beta)*360/(2*pi) 
}

###############################################################################
# XTS helper functions
###############################################################################

# must set timezone before any calls to xts
Sys.setenv(TZ = 'GMT')
#Sys.setenv(TZ = 'EST')

###############################################################################
#' The timezone is set to 'GMT' by defult
#'
#' The reason for setting the default timezone is because the following code 
#' produces different results if the timezone is NOT set and if timezone has a value.
#' 
#' @examples
#' \dontrun{ 
# 
#' # We want to set the timezone, so that following code produces expected results
#' Sys.getenv('TZ')
#' test = as.POSIXct('2012-10-31', format='%Y-%m-%d')
#'	as.numeric(test)
#'	as.numeric(as.POSIXct(as.Date(test)))
#' as.numeric(as.POSIXct(as.Date(test))) - as.numeric(test)
#' test == as.POSIXct(as.Date(test))
#'
#' # Set Time Zone
#' Sys.setenv(TZ = 'GMT')
#' Sys.getenv('TZ')
#' test = as.POSIXct('2012-10-31', format='%Y-%m-%d')
#'	as.numeric(test)
#'	as.numeric(as.POSIXct(as.Date(test)))
#' as.numeric(as.POSIXct(as.Date(test))) - as.numeric(test)
#' test == as.POSIXct(as.Date(test))
#'
#' }
#' @export 
#' @rdname XTSFunctions
###############################################################################
XTSFunctions <- function() {}

###############################################################################
#' Create \code{\link{xts}} object, faster version of \code{\link{xts}} fucntion
#'
#' @param x vector / matrix / data frame
#' @param order.by dates that correspond to rows of x
#'
#' @return \code{\link{xts}} object
#' 
#' @examples
#' \dontrun{ 
#' make.xts(1:101,seq(Sys.Date()-100, Sys.Date(), 1))
#' }
#' @export 
###############################################################################
make.xts <- function
(
  x,			# data
  order.by	# date
)
{
  #Sys.setenv(TZ = 'GMT')
  tzone = Sys.getenv('TZ')
  
  orderBy = class(order.by)
  index = as.numeric(as.POSIXct(order.by, tz = tzone))
  
  # need to handle case for one row; i.e. len(orderBy) == 1
  if( is.null(dim(x)) ) {
    if( len(orderBy) == 1 )
      x = t(as.matrix(x))
    else
      dim(x) = c(len(x), 1)
  }
  x = as.matrix(x)
  
  x = structure(.Data = x, 
                index = structure(index, tzone = tzone, tclass = orderBy), 
                class = c('xts', 'zoo'), .indexCLASS = orderBy, tclass=orderBy, .indexTZ = tzone, tzone=tzone)
  return( x )
}


###############################################################################
#' Reverse order of \code{\link{xts}} object
#'
#' @param x \code{\link{xts}} object
#'
#' @return \code{\link{xts}} object
#' 
#' @export 
###############################################################################
flip.xts <- function(x)
{
  dates = index(x)
  dates.index = nrow(x):1
  out = make.xts(coredata(x)[dates.index,], dates[dates.index])
  indexClass(out) = indexClass(x)
  return( out )
}


###############################################################################
#' Write \code{\link{xts}} object to file
#'
#' @param x \code{\link{xts}} object
#' @param filename file name
#' @param append flag to inidicate if file is overwritten or appended, \strong{defaults to FALSE}
#' @param ... additional paramaeters to the \code{\link{format}} function
#'
#' @return nothing
#' 
#' @examples
#' \dontrun{ 
#' write.xts(make.xts(1:101,seq(Sys.Date()-100, Sys.Date(), 1)), 'temp.csv')
#' }
#' @export 
###############################################################################
write.xts <- function
(
  x,			# XTS object
  filename,	# file name
  append = FALSE,	
  ...
)
{
  cat('Date', file = filename, append = append)
  
  write.table(x, sep=',',  row.names = format(index(x), ...), 
              col.names = NA, file = filename, append = T, quote = F)
  #write.csv(x, row.names = format(index(x)), filename)	
}

###############################################################################
#' Read \code{\link{xts}} object from file
#'
#' @param filename file name
#' @param date.fn function to preprocess string dates, \strong{defaults to \code{\link{paste}} - i.e. no preprocessing}
#' @param index.class class of the date object, \strong{defaults to 'Date'}
#' @param ... additional paramaeters to the \code{\link{as.POSIXct}} function
#'
#' @return \code{\link{xts}} object
#' 
#' @examples
#' \dontrun{ 
#' write.xts(make.xts(1:101,seq(Sys.Date()-100, Sys.Date(), 1)), 'temp.csv')
#' read.xts('temp.csv')
#' }
#' @export 
###############################################################################
read.xts <- function
(
  filename,	# file name
  date.fn = paste,
  index.class = 'Date',
  decreasing = FALSE,
  ...
)
{
  load.packages('data.table')
  out = fread(filename, stringsAsFactors=F)
  setnames(out,gsub(' ', '_', trim(colnames(out)))) 
  #		first.column.expr = parse(text = colnames(out)[1])
  rest.columns.expr = parse(text = paste('list(', paste(colnames(out)[-1],collapse=','),')'))
  
  #	dates = as.POSIXct(match.fun(date.fn)(out[,eval(first.column.expr)]), tz = Sys.getenv('TZ'), ...)
  dates = as.POSIXct(match.fun(date.fn)(as.matrix(out[,1,with=FALSE])), tz = Sys.getenv('TZ'), ...)		
  dates.index = order(dates, decreasing = decreasing)
  out = make.xts(out[dates.index, eval(rest.columns.expr)], dates[dates.index])
  indexClass(out) = index.class
  return( out )
}

# A few other variations to read data
read.xts.old <- function
(
  filename,	# file name
  date.fn = paste,
  index.class = 'Date',
  decreasing = FALSE,
  ...
)
{
  out = read.csv(filename, stringsAsFactors=F)
  #return( make.xts(out[,-1,drop=F], as.Date(out[,1], ...)) )
  
  dates = as.POSIXct(match.fun(date.fn)(out[,1]), tz = Sys.getenv('TZ'), ...)
  dates.index = order(dates, decreasing = decreasing)
  out = make.xts(out[dates.index,-1,drop=F], dates[dates.index])
  indexClass(out) = index.class
  return( out )
  
  # Example code from	getSymbols.yahoo (quantmod): as.POSIXct is used to avoid Dates conversion problems
  # fr = xts(1, as.POSIXct('2012-10-31', tz = Sys.getenv("TZ"), format='%Y-%m-%d'),  src = "yahoo", updated = Sys.time())
  # indexClass(fr) = "Date"	
}

read.xts.yahoo.old <- function
(
  filename,	# file name
  date.fn = paste,
  index.class = 'Date',
  decreasing = FALSE,
  ...
)
{
  temp = scan(filename, what=list('',double(0), double(0),double(0),double(0),double(0),double(0)), skip=1, sep=',', quiet =T)	
  
  dates = as.POSIXct(match.fun(date.fn)(temp[[1]]), tz = Sys.getenv('TZ'), ...)	
  dates.index = order(dates, decreasing = decreasing)
  
  out = matrix(double(1),len(dates), 6)
  colnames(out) = spl('Open,High,Low,Close,Volume,Adjusted')
  out[,1] = temp[[2]] 
  out[,2] = temp[[3]]
  out[,3] = temp[[4]]
  out[,4] = temp[[5]] 
  out[,5] = temp[[6]]
  out[,6] = temp[[7]]
  
  out = make.xts(out[dates.index,],  dates[dates.index])
  indexClass(out) = index.class
  return( out )
}

read.xts.test <- function() {
  load.packages('rbenchmark')
  
  filename = 'c:/stocks/SPY.csv'
  
  test1 <- function() {
    out = read.csv(filename, stringsAsFactors=F)
  }
  test2 <- function() {
    out1 = fread(filename, stringsAsFactors=F)
  }
  test3 <- function() {
    out2 = scan(filename, what=list('',double(0), double(0),double(0),double(0),double(0),double(0)), skip=1, sep=',', quiet =T)
  }
  
  library(rbenchmark)
  benchmark(
    test1(), 
    test2(),
    test3(),
    columns = c("test", "replications", "elapsed", "relative"),
    order = "relative",
    replications = 20
  )
  
  
  test1 <- function() {
    out = read.xts(filename, format = '%Y-%m-%d')
  }
  test2 <- function() {
    out1 = read.xts.old(filename, format = '%Y-%m-%d')
  }
  test3 <- function() {
    out2 = read.xts.yahoo.old(filename, format = '%Y-%m-%d')
  }
  
  library(rbenchmark)
  benchmark(
    test1(), 
    test2(),
    test3(),
    columns = c("test", "replications", "elapsed", "relative"),
    order = "relative",
    replications = 20
  )
  
}

###############################################################################
#' Fast alternative to index() function for \code{\link{xts}} object
#'
#' NOTE index.xts is the same name as the index function in the XTS package
#'
#' @param x \code{\link{xts}} object
#'
#' @return dates
#' 
#' @examples
#' \dontrun{ 
#' index.xts(make.xts(1:101,seq(Sys.Date()-100, Sys.Date(), 1)))
#' }
#' @export 
###############################################################################
# maybe rename to bt.index.xts
index.xts <- function
(
  x			# XTS object
)
{
  temp = attr(x, 'index')
  class(temp) = c('POSIXct', 'POSIXt')
  
  type = attr(x, '.indexCLASS')[1]
  if( type == 'Date' || type == 'yearmon' || type == 'yearqtr')
    temp = as.Date(temp)
  return(temp)
}

# other variants that are not currently used
# this function is used in plota for X axis
index4xts <- function
(
  x			# XTS object
)
{
  temp = attr(x, 'index')
  class(temp)='POSIXct' 
  
  return(temp)
}

index2date.time <- function(temp) {
  class(temp)='POSIXct' 
  
  if( attr(x, '.indexCLASS')[1] == 'Date') {	
    as.Date(temp)
  } else {
    as.POSIXct(temp, tz = Sys.getenv('TZ'))
  }
}

###############################################################################
#' File name Functions
#'
#' @param x file name
#'
#' @return part of the file name
#'
#' @examples
#' \dontrun{ 
#' get.extension('test.csv')
#' }
#' @export 
#' @rdname FilenameFunctions
###############################################################################
get.extension <- function(x) 
{ 
  trim( tail(spl(x,'\\.'),1) ) 
}	

#' @export 
#' @rdname FilenameFunctions
get.full.filename <- function(x) 
{ 
  trim( tail(spl(gsub('\\\\','/',x),'/'),1) ) 
}

#' @export 
#' @rdname FilenameFunctions
get.filename <- function(x) 
{ 
  temp = spl(get.full.filename(x),'\\.')
  join(temp[-len(temp)])
}


###############################################################################
#' Helper function to read historical stock prices saved by Seasonality tool
#'
#' @param Symbols vector of symbols
#' @param env enviroment to store prices, \strong{defaults to .GlobalEnv}
#' @param auto.assign flag to auto assign symbols, \strong{defaults to TRUE}
#' @param stock.folder stock folder, \strong{defaults to 'c:/temp/Seasonality/stocks'}
#' @param stock.date.format stock date format, \strong{defaults to '\%Y-\%m-\%d'}
#' @param ... other parameters for getSymbols function
#'
#' @return nothing is auto.assign = TRUE, prices are stored in the env enviroment
#' if auto.assign = FALSE, returns first symbol
#' 
#' @references 
#' \url{http://stackoverflow.com/questions/8970823/how-to-load-csv-data-file-into-r-for-use-with-quantmod}
#'
#' @examples
#' \dontrun{ 
#' data <- new.env()
#' getSymbols.sit(spl('SPY,IBM'), env = data, auto.assign = T)
#' }
#' @export 
######################################################################x#########
getSymbols.sit <- function
(
  Symbols, 
  env = .GlobalEnv, 
  auto.assign = TRUE, 
  stock.folder = 'c:/temp/Seasonality/stocks',
  stock.date.format = '%Y-%m-%d',
  ...
) 
{
  require(quantmod)	
  
  # http://stackoverflow.com/questions/8970823/how-to-load-csv-data-file-into-r-for-use-with-quantmod
  for(i in 1:len(Symbols)) {
    s = Symbols[i]
    
    temp = list()
    temp[[ s ]] = list(src='csv', format=stock.date.format, dir=stock.folder)
    setSymbolLookup(temp)
    
    temp = quantmod::getSymbols(s, env = env, auto.assign = auto.assign)		
    if (!auto.assign) {
      cat(s, format(range(index(temp)), '%d-%b-%Y'), '\n', sep='\t')	
      return(temp)
    }
    if(!is.null(env[[ s ]]))
      cat(i, 'out of', len(Symbols), 'Reading', s, format(range(index(env[[ s ]])), '%d-%b-%Y'), '\n', sep='\t')	
    else
      cat(i, 'out of', len(Symbols), 'Missing', s, '\n', sep='\t')	
  }
}


# Aside : all possible combinations of list elements
# expand.grid(a=1:10,b=2:3,KEEP.OUT.ATTRS=F)


###############################################################################
# Log (feedback) functions
###############################################################################

###############################################################################
#' @export 
###############################################################################
log.fn <- function(p.start=0, p.end=1) {
  p.start = p.start
  p.end = p.end
  function(..., percent=NULL) { 
    cat(..., iif(is.null(percent),'',paste(', percent = ', round(100 * (p.start + percent * (p.end - p.start)), 1), '%', sep='')), '\n') 
  }
}

###############################################################################
#' @export 
###############################################################################
log.fn.msg <- function(msg, log = log.fn()) {
  log = log
  msg = msg
  function(..., percent=NULL) { log(paste(msg, ...), percent=percent) }
}  

###############################################################################
#' Working with characters
#'
#' http://datadebrief.blogspot.ca/2011/03/ascii-code-table-in-r.html
#'
#' char is 8 bits, so to generate 512 bits random string 
#' rawToChar(as.raw(runif(512/8, 1, 255)))
#'
#' @export 
#' @rdname StringFunctions
############################################################################### 
asc <- function(x) { strtoi(charToRaw(x),16L) }

#' @export 
#' @rdname StringFunctions
chr <- function(n) { rawToChar(as.raw(n)) }

#' @export 
#' @rdname StringFunctions
make.random.string <- function(nbits = 256) { chr( runif(nbits/8, 1, 255) ) }


###############################################################################
#' List function / variables in enviroment
#'
#' http://www.mail-archive.com/r-help@stat.math.ethz.ch/msg22679.html
#'
#' @export 
#' @rdname ListEnvFunctions
############################################################################### 
ls.f <- function(env=sys.frame(-1))unlist(lapply(ls(env=env),function(x)if(is.function(get(x)))x))

#' @export 
#' @rdname ListEnvFunctions
ls.v <- function(env=sys.frame(-1))unlist(lapply(ls(env=env),function(x)if(!is.function(get(x)))x))



