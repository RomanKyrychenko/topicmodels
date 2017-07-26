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
tem$Описание <- paste(tem$`Заголовок без знаков препинания`,tem$Описание,collapse=" ")
tem$Описание <- gsub(">[^<^>]+<", "> <", tem$Описание) # remove all the text in between HTML tags, leaving only HTML tags (opening and closing)
tem$Описание <- gsub("</[^<^>]+>", "", tem$Описание)
paperCorp <- Corpus(VectorSource(tem$Описание))
paperCorp <- tm_map(paperCorp, removePunctuation)
paperCorp <- tm_map(paperCorp, removeNumbers)
# added tolower
paperCorp <- tm_map(paperCorp, tolower)
paperCorp <- tm_map(paperCorp, removeWords, stopwords("english"))
paperCorp <- tm_map(paperCorp, removeWords, stopwords("russian"))
paperCorp <- tm_map(paperCorp, removeWords, stopwords("ukrainian"))
# moved stripWhitespace


paperCorp <- tm_map(paperCorp, stemDocument)
paperCorp <- tm_map(paperCorp, removeWords, c("also", "article", "Article", 
                                              "download", "google", "figure",
                                              "fig", "groups","Google", "however",
                                              "high", "human", "levels","alt","feed","image","src","http","jpg",
                                              "larger", "may", "number","class",
                                              "shown", "study", "studies", "this","img",
                                              "using", "two", "the", "Scholar",
                                              "pubmedncbi", "PubMedNCBI","p",
                                              "view", "View", "the", "biol","div",
                                              "via", "image", "doi", "one", "classbackblock",
                                              "analysis","nbspap","photocharl","dharapak","pimg", "srcbmimgcomuaberlinstoragenewsxabddbdbajpg", "alignleft", "nbspnbspnbsp", "href"))
paperCorp <- tm_map(paperCorp, stripWhitespace)
dtm <- DocumentTermMatrix(paperCorp)
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]  

control <- list(burnin = 500, iter = 1000, keep = 100, seed = 2500)
(k <- optimal_k(dtm.new, 40, control = control))

k <- 200 # set number of topics
# generate model
lda <- LDA(dtm.new, k, method = "Gibbs", control = control)

gammaDF <- as.data.frame(lda@gamma) 
names(gammaDF) <- c(1:k)
toptopics <- as.data.frame(cbind(document = row.names(gammaDF), 
                                 topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))
toptext <- data_frame(text=tem$Заголовок[rowTotals> 0] ,topic=toptopics$topic)


topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}

lda %>% topicmodels2LDAvis() %>% LDAvis::serVis()
