library(shiny)
library(shinydashboard)
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

scanner <- function(x) strsplit(x," ")

library(stringr)
library(topicmodels)
library(tm)
library(tidyverse)
library(XLConnect)

stopwords = c("div", "href", "rel", "com", "relnofollow", "про", "что",
              "для", "relnofollow", "alt", "zero","img", "alignleft", "hspace", "vspace",
              "alignleft", "hspace", "vspace", "pbrp", 
              "altновини", "hrefhttpgsfmopinionhtml", "mode","strong", "pstrong", "targetblank",
              "styletext-align", "justifi","altнапк", "classattachment-decosingl", "classreadmor", 
              "ethereum", "hrefhttpresonanceua", "hrefhttpsinforesistorg",
              "hrefhttpblogiuaus", "hrefhttpvycherpnockua", "hrefhttpwwwkmugovuacontrolukpublisharticleartidampcatid", 
              "noneimg", "solid", "start", "stylefont-s", "stylefloat", "classfield-item", 
              "classfield", "classfield-itemsdiv", 
              "field-label-hiddendiv", "hrefhttpwwwaddthiscombookmarkphpv",
              "srcbmimgcomuaberlinstoragefinancexebfcbeadfdeccjpg",
              "eff", "i","classfield-item", "classfield", "classfield-itemsdiv", "field-label-hiddendiv", "datatyp", 
              "propertyrdfslabel", "datatyp", "propertyrdfslabel", "skospreflabel", "typeofskosconcept", 
              "reldcsubjecta", "classfield-tag", "field-type-taxonomy-term-refer", 
              "classimagecach", "classlink", "classrtejustifyspan", "classtranslation-link", "classtranslationru", 
              "clearfixdiv", "data-colorbox-gallerygallery-node--ytzaljghei", 
              "propertyrdfslabel", "skospreflabel","этом","это","які","від","datatyp","-tag",
              "hrefhttpkorupciyacom", "дуже", "там", "так", "але",
              "span", "width", "classleftimgimg", "stylecolor", "stylefont-famili",
              "hspace", "vspace", "clearal","classback-block","tabletrtd", "valigntop",
              "hrefhttpwwwaddthiscombookmarkphpv", "even", "как", "titl",
              "sea", "black", "hold", "one","stylemargin", "color", "outlin", "pad", "none","nbsp",
              "centerspan", "size", "stylefont-s", "font-siz", "divfont", 
              "justifi", "center", "width", "height", "classfeed-descriptionp",
              "pimg", "wp-post-imag", "margin", "sizesmax-width","justifystrong", 
              "srchttpimageunncomuaoriginaljpg", "altновости", "centerimg",
              "styletextalign","stylefontsize","justify","fontsize","padding","helvetica",
              "laquoР","raquo","httpnovostimiracomnewshtml","hrefhttprkrinuauploadspostsmatigeroyinyajpg",
              "hrefhttpskeletinfoorg","pem","leaders","hstrong","development","religious","targetblankstrong",
              "che","glucosio","person","primarily","hrefhttpskeletinfoorg","classmetanav","clearall",
              "stylefontfamily","arial","fontfamily","outline","sansserif","textalign","border","inherit","left","pspan",         
              "justifyspan","rgb","styleboxsizing","small","googleadsectionend",
              "classfielditem","classfielditemsdiv","fieldlabelhiddendiv", "also", "article", "Article", 
              "download", "google", "figure",
              "fig", "groups","Google", "however",
              "high", "human", "levels","alt","feed","image","src","http","jpg",
              "larger", "may", "number","class",
              "shown", "study", "studies", "this","img",
              "using", "two", "the", "Scholar",
              "pubmedncbi", "PubMedNCBI","p","photocharles",
              "view", "View", "the", "biol","div",
              "via", "image", "doi", "one", "classbackblock",
              "analysis","nbspap","photocharl","dharapak","pimg", "srcbmimgcomuaberlinstoragenewsxabddbdbajpg", "alignleft", "nbspnbspnbsp", "href",                      
              "fieldnamebody","fieldtypetextwithsummary","title","datatype","fieldtypeimage","typeoffoafimage",               
              "classcolorbox","fieldnamefieldimage","fieldtypetaxonomytermreference","rdfsseealso","relogimage","evenidfirstp",
              "sizesmaxwidth","wppostimage","sizedecosingle","styledisplayblock","classattachmentdecosingle","hrefhttpspolitekanet",                                 
              "httpspolitekanetwpcontentuploadsefebimagexjpeg","httpspolitekanetwpcontentuploadsgettyimageskopiyaxjpg",
              "httpspolitekanetwpcontentuploadsunianxjpg","httpspolitekanetwpcontentuploadsgettyimagesxjpg",
              "classfielditems","odd", "classfielditem","fieldtypetext","stylewidth","classimage","classimageattachteaser","imageattachnode",              
              "medium","tahomaarialhelveticasansserif","imagethumbnail","classimagecache","pxa", "fieldfieldnewslead","fieldfieldyandexnews",
              "srchttpinmediakitemscachedddddbbecbesjpg","srchttpsgoroddpuapicnewsnewsimagesmjpg","srchttpwwwkanzasuauploadpicresizejpg",
              "ampraquo","hrefhttpelvisticomnode","hrefhttpwwwgazetamistoteualajntranslyatsiyaforumudetsentralizatsiyaosnovaformuvannyanovoyisystemyupravlinnyatamistsevogorozvytku",
              "forklog","hidden","hrefhttpgiuauseridurlhttpsaffnewstutbyfeconomicsfhtml","lineheight","overflow","colorspan", "pxbbrspan","sizeb",
              "srcbmimgcomuaacommonimgicstarsemptyvioletgif","altimg","titleСЂР","hrefhttpvideobigmirnetuser","srchttpbmimgcomuavideoimgsxjpg", "table",
              "mediumspan","sansarialsansserifspan","langruruspan","classfooterinfosocialitemfollows","classfooterinfosocialitemiconspan",
              "hrefhttpwwwpravdacomuarusnews", "classfbcommentscount","що","который","которые","также","таким","новости","несмотря")
if (interactive()) {
  options(device.ask.default = FALSE)}
ui <- dashboardPage(skin = "blue",
                    title="Corestone ML",
                    dashboardHeader(
                      title="Corestone ML",
                      tags$li(class = "dropdown",
                              tags$a(href="http://corestone.expert/", target="_blank", 
                                     tags$img(height = "20px", alt="Corestone", src="http://corestone.expert/static/icons/ic-navbar-logo.svg")
                              )
                      ),
                      dropdownMenuOutput("sys"),
                      tags$li(class = "dropdown",
                              tags$a(href = "https://github.com/RomanKyrychenko",
                                     target = "_blank",
                                     tags$img(height = "20px", 
                                              src = "https://raw.githubusercontent.com/oraza/sectarianviolencePK/master/www/github.png")
                              )
                      )
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Infoflow", tabName = "Infoflow", icon = icon("vcard-o")),
                        #menuItem("System", tabName = "sys2", icon = icon("line-chart")),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        hr(),
                        menuItem("Documentation", icon = icon("file-text-o"), 
                                 href = "https://github.com/RomanKyrychenko/digest/blob/master/README.md"),
                        menuItem("Feedback & suggestion", icon = icon("envelope-o"),
                                 href = "mailto:?Roman.Kyrychenko@corestone.expert?subject=Feedback on Corestone work tools app"),
                        menuItem("Source code", icon = icon("file-code-o"), 
                                 href = "https://github.com/RomanKyrychenko/digest"),
                        menuItem("Fork me @ github", icon = icon("code-fork"), 
                                 href = "https://github.com/RomanKyrychenko") 
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "Infoflow",
                                h2("Infoflow"),
                                fileInput('file1', 'Завантажте файл з даними',
                                          accept = c(".xlsx")),
                                tags$hr(),
                                downloadButton('down',"Завантажити результат"),
                                tableOutput("lol")
                        )#,
                        #   tabItem(tabName = "sys2",
                        #           h2("System"),
                        #           actionButton("goButton", "Go!"),
                        #           verbatimTextOutput("nText"))
                      )
                    )
)


server <- function(input,output,server,session){
  df <- reactive({
   
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    tem <- readxl::read_excel(paste(inFile$datapath, ".xlsx", sep=""),col_names = T,sheet = 3)
    #tem <- readxl::read_excel("digest_test_R.xlsx",sheet = 2)
    tem$Описание <- tolower(gsub(">[^<^>]+<", "> <", tem$Описание)) # remove all the text in between HTML tags, leaving only HTML tags (opening and closing)
    tem$Описание <- gsub("</[^<^>]+>", "", tem$Описание)
    tem$`Заголовок без знаков препинания` <- tolower(gsub(">[^<^>]+<", "> <", tem$`Заголовок без знаков препинания`)) # remove all the text in between HTML tags, leaving only HTML tags (opening and closing)
    tem$`Заголовок без знаков препинания` <- gsub("</[^<^>]+>", "", tem$`Заголовок без знаков препинания`)
    tem$bigtext <- enc2utf8(mapply(function(x,y) paste(x,y,collapse=" "),tem$`Заголовок без знаков препинания`,tem$Описание))
    
    paperCorp <- Corpus(VectorSource(enc2native(tem$bigtext)),readerControl = list(language = "ru"))
    paperCorp <- tm_map(paperCorp, enc2utf8)
    
    paperCorp <- tm_map(paperCorp, removePunctuation)
    paperCorp <- tm_map(paperCorp, removeNumbers)
    
    paperCorp <- tm_map(paperCorp, removeWords, stopwords("english"))
    paperCorp <- tm_map(paperCorp, removeWords, stopwords("russian"))
    paperCorp <- tm_map(paperCorp, removeWords, stopwords)
    
    paperCorp <- tm_map(paperCorp, stemDocument,language="russian")
    paperCorp <- tm_map(paperCorp, stripWhitespace)
    paperCorp <- tm_map(paperCorp, stringi::stri_trans_general,"Ukrainian-Latin/BGN")
    
    dtm <- DocumentTermMatrix(paperCorp,control=list(tokenize=scanner,wordLengths=c(4, 15)))
    
    rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
    dtm.new   <- dtm[rowTotals> 0, ]  
    
    control <- list(burnin = 500, iter = 1000, keep = 100, seed = 2500)
    
    k <- 5 
    
    lda <- LDA(dtm.new, k, method = "Gibbs", control = control)
   
    gammaDF <- as.data.frame(lda@gamma) 
    names(gammaDF) <- c(1:k)
    toptopics <- as.data.frame(cbind(document = row.names(gammaDF), 
                                     topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))
    toptext <- data_frame(text=tem$Заголовок[rowTotals> 0] ,topic=toptopics$topic)
    toptext$topic <- sapply(toptext$topic, paste0, collapse=" ")
    toptext$topic <- ldaOut.topics[,1]
    tem <- left_join(tem,toptext,by=c("Заголовок"="text"))
    distinct(tem)
  })
  
  
  
  output$lol <- renderTable(df())
  
  output$down <- downloadHandler(
    filename = function(){
      paste0("infoflow-",Sys.Date(),".xlsx")
    },
    content = function(file){
      fname <- paste(file,"xlsx",sep=".")
      wb <- loadWorkbook(fname, create = TRUE)
      createSheet(wb, name = "Sheet1")
      writeWorksheet(wb, df(), sheet = "Sheet1")
      saveWorkbook(wb)
      file.rename(fname,file)
    }
  )}

options(shiny.trace=TRUE)
shinyApp(ui,server)