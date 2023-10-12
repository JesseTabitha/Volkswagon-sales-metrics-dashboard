dividenddevlopment<-c(2020,2019,2018,2017,2016)
ordinaryshares<-c(295090,295090,295090,295090,295090)
preferredshares<-c(206205,206205,206205,206205,206205)
turnoverOfOrdShares<-c(3.1,3.3,4.3,3.5,3.3)
grossLiquidity<-c(66078,53428,39626,31556,26451)
ratioOfMarketCapitalisationToEquity<-c(0.64,0.72,0.60,0.77)
Priceperearnings<-c(10.2,6.5,5.9,7.5,13.4)

library(ggplot2)
ggplot(turnoverOfOrdShares~dividenddevlopment, aes(x=turnoverOfOrdShares, y=dividend)) +
  geom_bar(stat="identity")


barplot(turnoverOfOrdShares~dividenddevlopment,main="Turnover per share",xlab="divident development",ylab="turnover", col = c("#DAF7A6", "#C77361", "#a1e9f0", "#d9b1f0","#800000"))

barplot(grossLiquidity~dividenddevlopment,ylab="Gross Liquidity",main="gross liquidity",col = c("#eb8060", "#b9e38d", "#a1e9f0", "#d9b1f0","#800080"))

barplot(ratioOfMarketCapitalisationToEquity~dividenddevlopment,main="ratio of market capitalisation to equity",ylab="ratio",col = c("#eb8060", "#b9e38d", "#a1e9f0", "#d9b1f0","#800080"))

barplot(Priceperearnings~dividenddevlopment,ylab="p/e ratio",main="price per earning ratio",col=c("#61C7A6","#C77361","#C7A661","#B5C761","#d9b1f0"))
