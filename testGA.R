#
# 2020 GA download page: 
# https://results.enr.clarityelections.com/GA/105369/web.264614/#/summary

# txt: https://results.enr.clarityelections.com//GA//105369/270221/reports/detailtxt.zip

library(xml2)
library(dplyr)

# to download the txt file.

if(!dir.exists('download')){
  dir.create('download')
}
down_file_unzip <- function(url){
  url <- "https://results.enr.clarityelections.com//GA//105369/270221/reports/detailtxt.zip"
  newfilename <- 'detailtxt.zip'
  
  url <- "https://results.enr.clarityelections.com//GA//105369/270221/reports/detailxml.zip"
  newfilename <- 'detailxml.zip'
  
  download.file(url=url, destfile=paste0('./download/', newfilename))
  unzip(paste0('./download/', newfilename),exdir = './download')
}

# now try to test the GA xml file:
read_ga_xml <- function(fn='download/detail.xml'){
  x <- xml2::read_xml(fn)
  contest_list <- xml_find_all(x, 'Contest')
  
  # find the President contest node
  for(i in 1:length(contest_list)){
    if(xml_attr(contest_list[[i]],'key') == '5000'){
      break
    }
  }
  # now the i is the correct contest.
  president_contest <- contest_list[[i]]
  choices <- xml_find_all(president_contest, 'Choice')
  
  for(i in 1:length(choices)){
    if(grepl('trump',xml_attr(choices[[i]],'text'), ignore.case = T)){
      trumpi <- i
    }
    if(grepl('biden',xml_attr(choices[[i]],'text'), ignore.case = T)){
      bideni <- i
    }
  }
  trump <- choices[[trumpi]]
  biden <- choices[[bideni]]
  
  trumprs <- read_subvote(trump, 'Trump')
  bidenrs <- read_subvote(biden, 'Biden')
  
  allrs <- rbind(trumprs, bidenrs) %>% tbl_df %>% mutate(Votes = as.integer(Votes))
  allrs
}

read_subvote <- function(v0, candidate='Trump'){
  # v0 = trump
  votetypes <- xml_find_all(v0, 'VoteType')
  
  .f <- function(i){
    v1 <- votetypes[[i]]
    votetypename <- xml_attr(v1, 'name')
    # as_list(v1)
    counties <- xml_find_all(v1, 'County')
    lapply(1:length(counties), function(j){
      v2 <- counties[[j]]
      countyname <- xml_attr(v2, 'name')
      countyvote <- xml_attr(v2, 'votes')
      data.frame(VoteType=votetypename, County=countyname, Votes=countyvote, stringsAsFactors = F)
    }) %>% bind_rows %>% tbl_df
  }
  d <- lapply(1:length(votetypes), .f) %>% bind_rows %>% tbl_df
  d$candidate <- candidate
  d
}


get_first_digit <- function(x){
  # x <- c(112, 239, 5)
  as.integer(substring( as.character(x), 1, 1))
}

# get GA rst.
ga_rs <- read_ga_xml()

ga_rs <- ga_rs %>% mutate(first_digit = get_first_digit(Votes))

ga_rs_stat <- ga_rs %>% group_by(VoteType,candidate, first_digit) %>% summarize(count=n())

