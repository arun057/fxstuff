require(XML)
fxrate<-"http://rates.fxcm.com/RatesXML3"
fxdata<-xmlToDataFrame(fxrate)