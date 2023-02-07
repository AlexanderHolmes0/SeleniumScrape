library(RSelenium)
library(netstat)
library(rvest)
library(tidyverse)
library(lubridate)

rD <- rsDriver(browser="chrome", 
               port=free_port(), 
               chromever ="109.0.5414.74",
               iedrver = NULL,
               verbose=F)


remote_driver <- rD$client

remote_driver$navigate("https://efdsearch.senate.gov/search/")

remote_driver$findElement(using = 'css',"#agree_statement")$clickElement()
remote_driver$findElement(using = 'css', '#filerTypeLabelSenator , #filerTypes')$clickElement()
remote_driver$findElement(using='css', '#reportTypeLabelPtr')$clickElement()
remote_driver$findElement(using='css',".btn-primary")$clickElement()


 
remote_driver$getWindowHandles()
remote_driver$switchToWindow(remote_driver$getWindowHandles()[1])
remote_driver$close()
rD$server$stop()


table <- list()
links <- list()
trans <- list()
for (i in 1:61) {
  
  table[i] <- remote_driver$getPageSource()[[1]] %>% 
    read_html() %>%
    html_table()
  
  links[[i]] <- remote_driver$getPageSource()[[1]] %>% 
      read_html() %>% 
      html_nodes("#filedReports a") %>% 
      html_attr('href') %>% 
      paste0("https://efdsearch.senate.gov",. )
  
  if(i<61){
  remote_driver$findElement(using='css', '#filedReports_next')$clickElement()
  message('Page', i, 'Scraped')
  }
  Sys.sleep(1)
}
  
  
  
trans <- list()
   for (j in 933:1053) {
   
   remote_driver$navigate(senate$trans_links[j])
     
   Sys.sleep(1)
   temp <- remote_driver$getPageSource()[[1]] %>% 
     read_html() %>%
     html_table()
   
   temp <- temp[[1]] %>% 
     mutate(senator_id = rep(j,length(temp)))
   if(length(temp)==0){temp <- "Paper Disclosure"}
   trans[[j]] <- temp
   }
   

  trans <- map_df(links, ~as_tibble((.)))
  trans <- trans[["value"]]
  senate <- map_df(table, ~as_tibble((.)))
  
senate <- senate %>% 
  mutate(trans_links = trans)

x<- data.frame(names =c ("ASD", "sd"))
x %>% 
  filter(names != toupper(x$names))

x != toupper(x)
senate <- subset(senate, !(`Last Name (Suffix)` %in% c("BLUMENTHAL",'BLUNT',"BOOZMAN","BURR","CANTWELL","CARDIN","CARPER",
                                             "COCHRAN","COLLINS","COONS","CRUZ","ENZI","FEINSTEIN","HOEVEN","KING",
                                             "PORTMAN","REED","ROBERTS","SHELBY","TOOMEY","UDALL","WARNER","WHITEHOUSE",
                                             "WYDEN")))

write.csv(senate,"CurrentSenator")


trades <- map_df(trans, ~as_tibble((.)))

senate <- senate %>% 
  mutate(senator_id = 1:1053)


senate <- senate %>% 
  inner_join(trades, by=c('senator_id'))
summary(senate)
senate$`Transaction Date`<- mdy(senate$`Transaction Date`)

write.csv(senate, "UpdateSenator.csv")

colnames(senate)<-c("First_Name_Middle","Last_Name","Office_FilerType","Report_Type","Date_Received_Filed","Trans_Links","Senator_ID","#","Transaction_Date","Owner","Ticker","Asset_Name","Asset_Type","Transaction_Type","Amount","Comment")
senate$Ticker[which(senate$Ticker=='')] <-"T"
senate$Asset_Type<- factor(senate$Asset_Type)
skimr::skim(publish)



publish <- senate[,c(1:5,9:16)]
write.csv(publish,"UpdatedSenatorTrades.csv")



