library(shiny)
library(stringr)
library(topicmodels)
library(tm)
library(dplyr)
library(shinydashboard)

scanner <- function(x) strsplit(x," ")

stopwords = unique(c("div", "href", "rel", "com", "relnofollow", "про", "что",
                     "для", "relnofollow", "alt", "zero","img", "alignleft", "hspace", "vspace",
                     "alignleft", "hspace", "vspace", "pbrp","altновини", "hrefhttpgsfmopinionhtml", "mode","strong", "pstrong", "targetblank",
                     "styletext-align", "justifi","altнапк", "classattachment-decosingl", "classreadmor", 
                     "ethereum", "hrefhttpresonanceua", "hrefhttpsinforesistorg",
                     "hrefhttpblogiuaus", "hrefhttpvycherpnockua", "hrefhttpwwwkmugovuacontrolukpublisharticleartidampcatid", 
                     "noneimg", "solid", "start", "stylefont-s", "stylefloat", "classfield-item", 
                     "classfield", "classfield-itemsdiv", "field-label-hiddendiv", "hrefhttpwwwaddthiscombookmarkphpv",
                     "srcbmimgcomuaberlinstoragefinancexebfcbeadfdeccjpg",
                     "eff", "i","classfield-item", "classfield", "classfield-itemsdiv", "field-label-hiddendiv", "datatyp", 
                     "propertyrdfslabel", "datatyp", "propertyrdfslabel", "skospreflabel", "typeofskosconcept", 
                     "reldcsubjecta", "classfield-tag", "field-type-taxonomy-term-refer", 
                     "classimagecach", "classlink", "classrtejustifyspan", "classtranslation-link", "classtranslationru", 
                     "clearfixdiv", "data-colorbox-gallerygallery-node--ytzaljghei", 
                     "propertyrdfslabel", "skospreflabel","этом","это","які","від","datatyp","-tag",
                     "hrefhttpkorupciyacom", "дуже", "там", "так", "але","span", "width", "classleftimgimg", "stylecolor", "stylefont-famili",
                     "hspace", "vspace", "clearal","classback-block","tabletrtd", "valigntop",
                     "hrefhttpwwwaddthiscombookmarkphpv", "even", "как", "titl",
                     "sea", "black", "hold", "one","stylemargin", "color", "outlin", "pad", "none","nbsp",
                     "centerspan", "size", "stylefont-s", "font-siz", "divfont", 
                     "justifi", "center", "width", "height", "classfeed-descriptionp",
                     "pimg", "wp-post-imag", "margin", "sizesmax-width","justifystrong", 
                     "srchttpimageunncomuaoriginaljpg", "altновости", "centerimg","styletextalign","stylefontsize","justify","fontsize","padding",
                     "helvetica","laquoР","raquo","httpnovostimiracomnewshtml","hrefhttprkrinuauploadspostsmatigeroyinyajpg",
                     "hrefhttpskeletinfoorg","pem","leaders","hstrong","development","religious","targetblankstrong",
                     "che","glucosio","person","primarily","hrefhttpskeletinfoorg","classmetanav","clearall",
                     "stylefontfamily","arial","fontfamily","outline","sansserif","textalign","border","inherit","left","pspan",         
                     "justifyspan","rgb","styleboxsizing","small","googleadsectionend",
                     "classfielditem","classfielditemsdiv","fieldlabelhiddendiv", "also", "article", "Article", 
                     "download", "google", "figure","fig", "groups","Google", "however","high", "human", "levels","alt","feed","image","src","http",
                     "jpg", "larger", "may", "number","class",
                     "shown", "study", "studies", "this","img","using", "two", "the", "Scholar","pubmedncbi", "PubMedNCBI","p","photocharles",
                     "view", "View", "the", "biol","div","via", "image", "doi", "one", "classbackblock",
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
                     "hrefhttpwwwpravdacomuarusnews", "classfbcommentscount","що","который","которые","также","таким","новости","несмотря","в","на","и","не",
                     "что","у","о","по","с","про","за","з","до","і","що","экс","об","этом","из","к","от","та","для","екс","це","его","а","как","politeka","он",
                     "від","його","новости","это","твій","года","суті","при","під","после","того","через","будет","так","липня","том","уже","більше","як","все",
                     "но","щодо","які","которые","то","2017","против","также","далее","новостей","він","своей","також","еще","перед","который","є","под","сути",
                     "твой","знай","михаил","со","году","року","я","видео","проти","сил","із","сейчас","чтобы","dneprcity.net","же","й","может","над","или",
                     "стало","было","може","тому","буде","те","был","быть","между","фото","112","який","ua","только","чем","является","ли","более","21",
                     "26.07.17","без","було","вже","где","зі","ще","19","2018","laquo","мы","raquo","вона","кто","ми","своїй","ст","яких","были","ему","их",
                     "наших","него","особо","очень","себе","щоб","де","ее","зараз","котором","сам","своем","теперь","цього","але","если","которая","себя",
                     "тем","эти","https","больше","був","бути","два","навіщо","нас","таки","тогда","replyua.net","будут","йому","між","можно","ним","новость",
                     "п","ч","чи","12","50","mc","должен","им","именно","источник","которых","меньше","стал","эту","буду","має","стоит","требует","этот","11",
                     "http","была","высокий","день","имеет","її","мог","несколько","них","она","этого","вони","всего","вся","когда","которым","одним","почти",
                     "разі","стала","була","всі","вы","даже","дал","дать","кого","которое","м","нет","ни","новину","оборони","один","поки","потому","свою",
                     "стали","таким","этой","яка","якому","яку","script","the","var","ей","новини","одно","понад","проте","серед","такий","чому","вот","дати","есть"))

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
                        menuItem("Topic modeling", tabName = "Infoflow", icon = icon("vcard-o")),
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
                                h2("Topic modeling"),
                                fileInput('file1', 'Завантажте файл з даними',
                                          accept = c(".xlsx",".xlsm")),
                                numericInput("cluster","Кількість тем",100,min=2,max=1000),
                                tags$hr(),
                                downloadButton('down',"Завантажити результат")#,
                                #dataTableOutput("lol")
                        )
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
    tem <- readxl::read_excel(paste(inFile$datapath, ".xlsx", sep=""),col_names = T,sheet = 1)
    sped <- data_frame(second=c(7,10,19,33,63,123,12,22,44,81,159,301),k=c(5,10,20,40,80,160,5,10,20,40,80,160),size=c(2997,2997,2997,2997,2997,2997,5722,5722,5722,5722,5722,5722))
    fit<-lm(second~k+size,data=sped)
    time <- predict(fit, data_frame(k=input$cluster,size=nrow(tem)), interval = "prediction")[1]
    progress <- Progress$new(session, min=1, max=21)
    on.exit(progress$close())
    
    progress$set(message = 'Calculation in progress',
                 detail = 'This may take a while...')
    #tem <- readxl::read_excel("Cover_All_DB.xlsm_20170726.xlsm",sheet = 3)
    tem$Текст <- tolower(gsub(">[^<^>]+<", "> <", tem$Текст)) # remove all the text in between HTML tags, leaving only HTML tags (opening and closing)
    tem$Текст <- gsub("</[^<^>]+>", "", tem$Текст)
    #tem$`Заголовок без знаков препинания` <- tolower(gsub(">[^<^>]+<", "> <", tem$Язык)) # remove all the text in between HTML tags, leaving only HTML tags (opening and closing)
    #tem$`Заголовок без знаков препинания` <- gsub("</[^<^>]+>", "", tem$Язык)
    progress$set(value = 1, detail="Видалено зайві символи")
    #tem$bigtext <- enc2utf8(mapply(function(x,y) paste(x,y,collapse=" "),tem$Язык,tem$Описание))
    tem$bigtext <- tem$Текст
    progress$set(value = 2, detail="Створено один текст і заголовків та описів")
    paperCorp <- Corpus(VectorSource(enc2native(tem$bigtext)),readerControl = list(language = "ru"))
    progress$set(value = 3, detail="Створено корпус документів")
    paperCorp <- tm_map(paperCorp, removePunctuation)
    progress$set(value = 4, detail="Видалено зайві символи")
    paperCorp <- tm_map(paperCorp, removeNumbers)
    progress$set(value = 5, detail="Видалено числа")
    paperCorp <- tm_map(paperCorp, removeWords, stopwords("english"))
    paperCorp <- tm_map(paperCorp, removeWords, stopwords("russian"))
    paperCorp <- tm_map(paperCorp, removeWords, stopwords)
    progress$set(value = 6, detail="Стоп-слова видалено")
    paperCorp <- tm_map(paperCorp, stemDocument,language="russian")
    progress$set(value = 7, detail="Видалено зайві символи")
    paperCorp <- tm_map(paperCorp, stripWhitespace)
    progress$set(value = 8, detail="Видалено зайві пробіли")
    paperCorp <- tm_map(paperCorp, stringi::stri_trans_general,"Ukrainian-Latin/BGN")
    progress$set(value = 9, detail="Створення матриці слів і документів")
    dtm <- DocumentTermMatrix(paperCorp,control=list(tokenize=scanner,wordLengths=c(4, 15)))
    dtm<-removeSparseTerms(dtm, sparse=0.99)
    progress$set(value = 10, detail="Видалення пустих рядків")
    rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
    dtm.new   <- dtm[rowTotals> 0, ]  
    progress$set(value = 11, detail=paste("Обчислення моделі, час для приготування кави","(Орієнтовно",time%/%60,"хвилин",round(time%%60),"секунд)"))
    control <- list(burnin = 100, iter = 100, keep = 100, seed = 2500)
    
    k <- input$cluster 
    
    lda <- LDA(dtm.new, k, method = "Gibbs", control = control)
    progress$set(value = 19, detail="Модель створено!")
    gammaDF <- as.data.frame(lda@gamma) 
    names(gammaDF) <- c(1:k)
    toptopics <- as.data.frame(cbind(document = row.names(gammaDF), 
                                     topic = as.numeric(apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))][1]))))
    toptext <- data_frame(text=tem$Заголовок[rowTotals> 0] ,Тема2=as.numeric(toptopics$topic))
    #toptext$Тема <- sapply(toptext$Тема, paste0, collapse=" ")
    toptext$`Рівень відповідності` <- apply(gammaDF,1,max)
    toptext <- toptext %>% group_by(text) %>% summarise(Тема2=min(Тема2,na.rm=T),`Рівень відповідності`=mean(`Рівень відповідності`,na.rm=T))
    #toptext$topic <- ldaOut.topics[,1]
    tem <- left_join(tem,toptext,by=c("Заголовок"="text"))
    
    slov <- left_join((tem %>% group_by(Тема2) %>% summarise(`Рівень відповідності`=max(`Рівень відповідності`)[1])),tem[c(5,9,10)],by=c("Рівень відповідності","Тема2"))
    names(slov)[3] <- "theme2"
    tem <- left_join(tem,slov[-2],by=c("Тема2"))
    tem$theme <- tem$theme2
    tem <- distinct(tem)
    progress$set(value = 20, detail="Масив створено, сторення Excel")
    tem[!duplicated(tem[,'Текст']),]
    #distinct(tem)
  })
  output$down <- downloadHandler(
    filename = function() {paste0("Topics_",input$cluster,"_",as.character(Sys.Date()), '.xlsx')},
    content = function(file) {
      #write.csv(as.data.frame(df()),file)
      xlsx::write.xlsx(as.data.frame(df()[-c(8,9,11)]), file, sheetName = "Теми", row.names = FALSE)
    }
  )}

options(shiny.maxRequestSize=30*1024^2) 
options(shiny.trace=TRUE)
shinyApp(ui,server)
