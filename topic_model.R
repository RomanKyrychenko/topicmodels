Sys.setlocale("LC_ALL","Ukrainian")

if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("trinker/gofastr")
pacman::p_load(tm, topicmodels, dplyr, tidyr, igraph, devtools, LDAvis, ggplot2)

## Source topicmodels2LDAvis & optimal_k functions
invisible(lapply(
  file.path(
    "https://raw.githubusercontent.com/trinker/topicmodels_learning/master/functions", 
    c("topicmodels2LDAvis.R", "optimal_k.R")
  ),
  devtools::source_url
))

library(stringr)
library(topicmodels)
library(tm)
library(tidyverse)

tem <- readxl::read_excel("digest_test_R.xlsx",sheet = 2)

object.size(tem$`Заголовок без знаков препинания`)
object.size(tem$Описание)
object.size(paste(tem$`Заголовок без знаков препинания`,tem$Описание,collapse=" "))
#tem$Описание <- paste(tem$`Заголовок без знаков препинания`,tem$Описание,collapse=" ")
tem$Описание <- tolower(gsub(">[^<^>]+<", "> <", tem$Описание)) # remove all the text in between HTML tags, leaving only HTML tags (opening and closing)
tem$Описание <- gsub("</[^<^>]+>", "", tem$Описание)
tem$`Заголовок без знаков препинания` <- tolower(gsub(">[^<^>]+<", "> <", tem$`Заголовок без знаков препинания`)) # remove all the text in between HTML tags, leaving only HTML tags (opening and closing)
tem$`Заголовок без знаков препинания` <- gsub("</[^<^>]+>", "", tem$`Заголовок без знаков препинания`)
tem$bigtext <- enc2utf8(mapply(function(x,y) paste(x,y,collapse=" "),tem$`Заголовок без знаков препинания`,tem$Описание))
#tem$bigtext <- stringi::stri_trans_general(tem$bigtext,"Ukrainian-Latin/BGN")
paperCorp <- Corpus(VectorSource(enc2native(tem$bigtext)),readerControl = list(language = "ru"))
paperCorp <- tm_map(paperCorp, enc2utf8)
#paperCorp <- Corpus(VectorSource(tem$Описание))
paperCorp <- tm_map(paperCorp, removePunctuation)
paperCorp <- tm_map(paperCorp, removeNumbers)
# added tolower
#paperCorp <- tm_map(paperCorp, tolower)
paperCorp <- tm_map(paperCorp, removeWords, stopwords("english"))
paperCorp <- tm_map(paperCorp, removeWords, stopwords("russian"))
paperCorp <- tm_map(paperCorp, removeWords, stopwords)
# moved stripWhitespace


paperCorp <- tm_map(paperCorp, stemDocument,language="russian")
paperCorp <- tm_map(paperCorp, removeWords, c("also", "article", "Article", 
                                              "download", "google", "figure",
                                              "fig", "groups","Google", "however",
                                              "high", "human", "levels","alt","feed","image","src","http","jpg",
                                              "larger", "may", "number","class",
                                              "shown", "study", "studies", "this","img",
                                              "using", "two", "the", "Scholar",
                                              "pubmedncbi", "PubMedNCBI","p","photocharles",
                                              "view", "View", "the", "biol","div",
                                              "via", "image", "doi", "one", "classbackblock",
                                              "analysis","nbspap","photocharl","dharapak","pimg", "srcbmimgcomuaberlinstoragenewsxabddbdbajpg", "alignleft", "nbspnbspnbsp", "href"))
paperCorp <- tm_map(paperCorp, stripWhitespace)
paperCorp <- tm_map(paperCorp, stringi::stri_trans_general,"Ukrainian-Latin/BGN")
scanner <- function(x) strsplit(x," ")
dtm <- DocumentTermMatrix(paperCorp,control=list(tokenize=scanner,wordLengths=c(4, 15)))
#Encoding(dtm$dimnames$Terms) <- "UTF-8"
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]  

control <- list(burnin = 500, iter = 1000, keep = 100, seed = 2500)
#(k <- optimal_k(dtm.new, 40, control = control))

k <- 500 # set number of topics
# generate model
lda <- LDA(dtm.new, k, method = "Gibbs", control = control)
ldaOut.topics <- as.matrix(topics(lda))
ldaOut.terms <- as.matrix(terms(lda,20))
gammaDF <- as.data.frame(lda@gamma) 
names(gammaDF) <- c(1:k)
toptopics <- as.data.frame(cbind(document = row.names(gammaDF), 
                                 topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))
toptext <- data_frame(text=tem$Заголовок[rowTotals> 0] ,topic=toptopics$topic)
toptext$topic <- sapply(toptext$topic, paste0, collapse=" ")
toptext$topic <- ldaOut.topics[,1]

test.topics <- posterior(lda,dtm.new)
tem2$topic[rowTotals> 0] <- as.numeric(colnames(test.topics$topics)[apply(test.topics$topics,1,which.max)])

