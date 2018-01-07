

rm(list = ls())

library(httr)
library(stringr)
library(magrittr)
library(rvest)
library(tidyverse)
library(lubridate)



#ham convertDate
convertDate <- function(dt){
  paste0(str_sub(dt, 7, 10), "-", str_sub(dt, 4, 5), "-", str_sub(dt, 1, 2))
}

#ham thay the dau comma
subComma <- function(v) {
  str_replace_all(v, ",", "")
}

#chuyen percent dang text sang numeric
convertPercent <- function(v){
  str_replace_all(v, "%", "") %>% 
    as.numeric()*0.01
}





get_symbols_CP68 <- function (symbol, from, to){
  url<-"http://www.cophieu68.vn/historyprice.php"
  #lay cac trang cuoi cung
  fd <- list(id = symbol)
  
  resp <- POST(url, body = fd, endcode = "form")
  resp %>% 
    read_html() %>% 
    html_nodes(xpath = '//*[@id="navigator"]/li[7]/a') %>%
    html_attrs() -> tmp
  
  positionString <- str_locate(tmp[1], "[0-9]") %>% 
    as.data.frame() %$% 
    start
  lastPage <- as.numeric(str_sub(unlist(tmp[1]), positionString,positionString + 1))
  
  #khoi tao dataframe
  symbolData <- data.frame(
    DATE = character(),
    REF.PRICE = numeric(),
    CHANGE.PERCENT1 = numeric(),
    CHANGE.PERCENT2 = numeric(),
    CLOSE = numeric(),
    MATCH.VOLUME = numeric(),
    OPEN = numeric(),
    HIGH = numeric(),
    LOW = numeric(),
    RECONCILE.VOLUME = numeric(),
    FOREIGN.BUY = numeric(),
    FOREIGN.SALE = numeric(),
    VALUE = numeric()
  )
  
  #tao vong lap lay du lieu
  for(i in 1:lastPage){
    #tao form-data
    fd <- list(
      currentPage = i,
      id = symbol
    )
    
    resp <- POST(url, body = fd, endcode = "form")
    
    
    resp %>% 
      read_html() %>% 
      html_nodes(xpath = '//*[@id="content"]/table') %>%
      html_table() %>% 
      as.data.frame() -> tmp
    
    tmp <- tmp[-1, 2:14]
    tmp[, 1] <- as.Date(convertDate(tmp[,1]), format = "%Y-%m-%d")
    #check dieu kien de continue
    if(min(tmp[, 1]) > to) {next}
    #check dieu kien de break
    if(max(tmp[, 1]) < from) {break}
    tmp[, c(6, 10:12)] <- apply(tmp[, c(6, 10:12)], 2, subComma)
    tmp[, 4] <- convertPercent(tmp[, 4])
    tmp[, c(2:3, 5:13)] <- apply(tmp[, c(2:3, 5:13)], 2, as.numeric)
    
    colnames(tmp) <- c("DATE",
                       "REF.PRICE",
                       "CHANGE.PERCENT1",
                       "CHANGE.PERCENT2",
                       "CLOSE",
                       "MATCH.VOLUME",
                       "OPEN",
                       "HIGH",
                       "LOW",
                       "RECONCILE.VOLUME",
                       "FOREIGN.BUY",
                       "FOREIGN.SALE",
                       "VALUE")
    symbolData <- rbind(symbolData, tmp)
  }
  
  symbolData <- data.frame(symbolData, row.names = symbolData[, 1])
  colnames(symbolData) <- c("DATE",
                            "REF.PRICE",
                            "CHANGE.PERCENT1",
                            "CHANGE.PERCENT2",
                            "CLOSE",
                            "MATCH.VOLUME",
                            "OPEN",
                            "HIGH",
                            "LOW",
                            "RECONCILE.VOLUME",
                            "FOREIGN.BUY",
                            "FOREIGN.SALE",
                            "VALUE")
  symbolData <- symbolData[, c(1, 3:4, 7:9, 5, 2, 6, 10:13)]
  symbolData %<>% arrange(DATE)
  cat(paste0("#", symbol, " from ", from, " to ", to, " already collected"))
  invisible(subset(symbolData, and(DATE >= from, DATE <= to)))
}


# Test function: 

u <- get_symbols_CP68("AAM", "2016-01-01", "2016-12-31")

u %>% names()
u %>% head()

u %>% ggplot(aes(DATE, OPEN)) + geom_line()



