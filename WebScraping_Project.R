library(corrplot)
library(ggplot2)
library(gplots)
library(ggpubr)
library(scales)
library(zoo)
library(corrplot)
library(readxl)
library(quantmod)
library(tidyr)
library(stringr)
library(rvest)
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gplots)
library(ggpubr)
library(scales)
library(zoo)
#Download/Read the html: NCREIF--------------------------------------------------------------------------------
region<- c("N","E","W","S","M","H","A","R","I","O")
region_name <- c("National","East","West","South","Middle","Hotel","Apartment","Retail","Industrial","Office")

all_return=data.frame()
for(i in 1:length(region)){
  url<-paste0("https://epitest.ncreif.org/property-index-returns.aspx?region=",region[i],"#farmland")
  html<- read_html(url)  
  get_return<- html_nodes(html,"#farmland")
  return_table <- html_table(get_return)
  return<-data.frame(return_table) 
  return$region<-rep(region_name[i],nrow(return))
  
  all_return<-rbind(all_return, return)
  print(paste0("Finished Download Region: ", region[i]))
}
pct_to_number<- function(x){
  x_replace_pct<-sub("%", "", x)
  x_as_numeric<-as.numeric(x_replace_pct)
}
all_return$Quarter.1<- pct_to_number(all_return$Quarter.1)
all_return$Quarter.2<- pct_to_number(all_return$Quarter.2)
all_return$Quarter.3<- pct_to_number(all_return$Quarter.3)
all_return$Quarter.4<- pct_to_number(all_return$Quarter.4)

diff_return <- all_return %>% gather(Quarter, Return, Quarter.1:Quarter.4)
diff_return <-diff_return[order(diff_return$Year),]%>% spread(region, Return)
diff_return$Quarter <- str_extract(diff_return$Quarter, "\\d")
col_order <- c("Year", "Quarter", "National","East","West","South","Middle","Hotel","Apartment","Retail","Industrial","Office")
diff_return <- diff_return[, col_order] %>% unite("Year", Year:Quarter, sep = ":", remove = TRUE)

ncreif_return<- diff_return[grep("1989:1", diff_return$Year):grep("2016:2", diff_return$Year),]
ncreif <-data.frame(ncreif_return[1:2])
View(ncreif_return)

stats_ncreif<- ncreif_return[2:ncol(ncreif_return)]
summary(stats_ncreif)
cor <- round(cor(stats_ncreif),2)
cor

area <- ncreif_return[,c(1,3:6)]
area_plot<- reshape2::melt(area, id.var='Year')
ncreif_area <- ggplot(area_plot, aes(x=Year, y=value, col=variable)) + geom_line(group=1,size=1)+
  labs(x = "Year", y = "%Return", title = "NCREIF Region Return")+
  theme(axis.text.x=element_text(angle=90, vjust = 0.5))
ncreif_area

type <- ncreif_return[,c(1,7:11)]
type_plot<- reshape2::melt(type, id.var='Year')
ncreif_type <- ggplot(type_plot, aes(x=Year, y=value, col=variable)) + geom_line(group=1,size=1)+
  labs(x = "Year", y = "%Return", title = "NCREIF Type Return")+
  theme(axis.text.x=element_text(angle=90, vjust = 0.5))
ncreif_type

#Using quantmod to get stock return------------------------------------------------------------------------------

#sp500
sp500<- data.frame(getSymbols("^GSPC",auto.assign = FALSE, from = "1989-01-01",to= "2016-06-30"))
SP500<-quarterlyReturn(sp500)*100
names(SP500)[1]<- c("S&P500")
chartSeries(sp500)

#Wilhire5000
wilshire<- data.frame(getSymbols("^W5000",auto.assign = FALSE, from = "1989-01-01",to= "2016-06-30"))
WILSHIRE<-quarterlyReturn(wilshire)*100
names(WILSHIRE)[1]<- c("WILSHIRE")
chartSeries(wilshire)

stock_return<-cbind.data.frame(SP500,WILSHIRE)


#Nareit-------------------------------------------------------------------------------------------------------------
Nareit <- read_excel("C:/Users/JY Development Group/Desktop/Elena/nareit_return.xlsx")[3]
NAREIT <- unlist(Nareit)*100

#ACLI----------------------------------------------------------------------------------------------------
ACLI <- read_excel("C:/Users/JY Development Group/Desktop/Elena/ACLI.xlsx",sheet="Final")
acli<-ACLI[,c(6)]
names(acli)[1]<- c("ACLI")

#All data--------------------------------------------------------------------------------------------------------
all<- cbind.data.frame(ncreif,stock_return,NAREIT,acli)
rownames(all) <- all$Year
all_return<- all[2:ncol(all)]



#----------------------------------------------------------------------------------------------------------------
summary(all_return)
cor_all<-round(cor(all_return),2)
cor_all

hist(all_return$National,col="red", breaks=20,main =  "National Return")


install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(all_return, histogram=TRUE, pch=19)

