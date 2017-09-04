Sys.sleep(3600*1.5)
  
library(stringr)
library(topicmodels)
library(tm)
library(dplyr)
library(elastic)
library(RCurl)
library(text2vec)
library(gtools)

#Define functions

stopwords = unique(c("div", "href", "rel", "com", "relnofollow", "про", "что","dneprcity.net","novostimira.com","depo.ua","bbc.com",
                     "для", "relnofollow", "alt", "zero","img", "alignleft", "hspace", "vspace","studic.info","description","delo.ua",
                     "alignleft", "hspace", "vspace", "pbrp","altновини", "hrefhttpgsfmopinionhtml", "mode","strong", "pstrong", "targetblank",
                     "styletext-align", "justifi","altнапк", "classattachment-decosingl", "classreadmor","http:img.ura","pigua.info",
                     "ethereum", "hrefhttpresonanceua", "hrefhttpsinforesistorg","replyua.net","newsoboz.org","mc.async","manabalss.lv",
                     "hrefhttpblogiuaus", "hrefhttpvycherpnockua", "hrefhttpwwwkmugovuacontrolukpublisharticleartidampcatid","mid.ru",
                     "noneimg", "solid", "start", "stylefont-s", "stylefloat", "classfield-item","cackle.me","container","dtp.kiev.ua",
                     "classfield", "classfield-itemsdiv", "field-label-hiddendiv", "hrefhttpwwwaddthiscombookmarkphpv","replyua.netпро",
                     "srcbmimgcomuaberlinstoragefinancexebfcbeadfdeccjpg","cackle_widget.push","document.createelement","polytics","loading",
                     "eff", "i","classfield-item", "classfield", "classfield-itemsdiv", "field-label-hiddendiv", "datatyp","i.lb.ua",
                     "propertyrdfslabel", "datatyp", "propertyrdfslabel", "skospreflabel", "typeofskosconcept","mc.src","erve.ua","www.pravda.com.ua",
                     "reldcsubjecta", "classfield-tag", "field-type-taxonomy-term-refer","document.createelement","mc.type","lb.ua",
                     "classimagecach", "classlink", "classrtejustifyspan", "classtranslation-link", "classtranslationru","goo.gl","newsru.co.il",
                     "clearfixdiv", "data-colorbox-gallerygallery-node--ytzaljghei","document.getelementbyid","javascript","mon.gov.ua",
                     "propertyrdfslabel", "skospreflabel","этом","это","які","від","datatyp","-tag","function","innerhtml","sevastopolnews.info",
                     "hrefhttpkorupciyacom", "дуже", "там", "так", "але","span", "width", "classleftimgimg", "stylecolor", "stylefont-famili",
                     "hspace", "vspace", "clearal","classback-block","tabletrtd", "valigntop","document.location.protocol","zakarpatpost.net",
                     "hrefhttpwwwaddthiscombookmarkphpv", "even", "как", "titl","document.getelementsbytagname","text","true","sport.ua",
                     "sea", "black", "hold", "one","stylemargin", "color", "outlin", "pad", "none","nbsp","widget.js","www.unian.net",
                     "centerspan", "size", "stylefont-s", "font-siz", "divfont","s.nextsibling","s.parentnode.insertbefore","www.ukrinform.ru",
                     "justifi", "center", "width", "height", "classfeed-descriptionp","window.cackle_widget","xcoal","politics","newsru.co.il",
                     "pimg", "wp-post-imag", "margin", "sizesmax-width","justifystrong","joinfo.ua","1news","journal","unn.com.ua","newsonline24",
                     "srchttpimageunncomuaoriginaljpg", "altновости", "centerimg","styletextalign","stylefontsize","justify","fontsize","padding",
                     "helvetica","laquoР","raquo","httpnovostimiracomnewshtml","hrefhttprkrinuauploadspostsmatigeroyinyajpg","news_180574",
                     "hrefhttpskeletinfoorg","pem","leaders","hstrong","development","religious","targetblankstrong","politico","news_180572",
                     "che","glucosio","person","primarily","hrefhttpskeletinfoorg","classmetanav","clearall","newsoboz","politika","news_180571",
                     "stylefontfamily","arial","fontfamily","outline","sansserif","textalign","border","inherit","left","pspan","naviny.by",         
                     "justifyspan","rgb","styleboxsizing","small","googleadsectionend","womenbox.net","arianespace","polityka","porfyrios",
                     "classfielditem","classfielditemsdiv","fieldlabelhiddendiv", "also", "article", "Article","cellpadding","finanso.net",
                     "download", "google", "figure","fig", "groups","Google", "however","high", "human", "levels","alt","feed","image","src","http",
                     "jpg", "larger", "may", "number","class","новости","gazeta.ua","rossii.html","zn.ua","cellspacing","portal",
                     "shown", "study", "studies", "this","img","using", "two", "the", "Scholar","pubmedncbi", "PubMedNCBI","p","photocharles",
                     "view", "View", "the", "biol","div","via", "image", "doi", "one", "classbackblock","dubinsky.pro","posted","news_180560",
                     "analysis","nbspap","photocharl","dharapak","pimg", "srcbmimgcomuaberlinstoragenewsxabddbdbajpg", "alignleft", "nbspnbspnbsp", "href",                      
                     "fieldnamebody","fieldtypetextwithsummary","title","datatype","fieldtypeimage","typeoffoafimage","www.globallookpress.com",               
                     "classcolorbox","fieldnamefieldimage","fieldtypetaxonomytermreference","rdfsseealso","relogimage","evenidfirstp",
                     "sizesmaxwidth","wppostimage","sizedecosingle","styledisplayblock","classattachmentdecosingle","hrefhttpspolitekanet",                                 
                     "httpspolitekanetwpcontentuploadsefebimagexjpeg","httpspolitekanetwpcontentuploadsgettyimageskopiyaxjpg","replyua.net.так",
                     "httpspolitekanetwpcontentuploadsunianxjpg","httpspolitekanetwpcontentuploadsgettyimagesxjpg","joomla","slavdelo.dn.ua",
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
                     "стали","таким","этой","яка","якому","яку","script","the","var","ей","новини","одно","понад","проте","серед","такий","чому","вот","дати","есть",
                     "без", "більш", "більше", "буде", "начебто", "би", "був", "була", "були", "було", "бути", "вам", "вас", "адже", "увесь", "уздовж", "раптом", "замість", 
                     "поза", "униз", "унизу", "усередині", "в", "навколо", "от", "втім", "усі", "завжди", "усього", "усіх", "усю", "ви", "де", "так", "давай", "давати", "навіть", 
                     "для", "до", "досить", "інший", "його", "йому", "її", "її", "їй", "якщо", "є", "ще", "же", "за", "за винятком", "тут", "з", "через","або", "їм", "мати", 
                     "іноді", "їх", "якось", "хто", "коли", "крім", "хто", "куди", "чи", "або", "між", "мене", "мені", "багато", "може", "моє", "мої", "мій", "ми", "на", "назавжди",
                     "над", "треба", "нарешті", "нас", "наш", "не", "його", "ні", "небудь", "ніколи", "їм", "їх", "нічого", "але", "ну", "про", "однак", "він", "вона", "вони", 
                     "воно", "знову", "від", "тому", "дуже", "перед", "по", "під", "після", "потім", "тому", "тому що", "майже", "при", "про", "раз", "хіба", "свою", "себе", 
                     "сказати", "з", "зовсім", "так", "також", "такі", "такий", "там", "ті", "тебе", "тем", "тепер", "те", "тоді", "того", "теж", "тієї", "тільки", "тому", "той", 
                     "отут", "ти", "уже", "хоч", "хоча", "чого", "чогось", "чий", "чому", "через", "що", "щось", "щоб", "ледве", "чиє", "чия", "ця", "ці", "це", "цю", "цього", 
                     "цьому", "цей","і","у","та","я","а","й","як","які","бо","із","який","тим","нам","б","всі","ж","яку","зі","яких","всіх","цим","1997","1991","1992","1998","2008",
                     "2009","2010","2011","2012","2013","2014","2015","2016","2017","рік","все","роком","році","нехай","хай","року","яка","них","ним","1996","то"))

q = '{
"query": {
  "bool": {
    "must": [
              {
                "exists": {
                "field": "target"
                }
              }
            ]
          }
        }
    }'

es <- "13.59.27.188:9200"

result_date <- gsub("-","",as.character(Sys.Date()-1))

csv_to_json <- function(dat, pretty = F,na = "null",raw = "mongo",digits = 3,force = "unclass"){
  dat_to_json <- jsonlite::toJSON(dat,pretty = pretty,na = "null",raw = raw,digits = digits ,force = force )
  dat_to_json=substr(dat_to_json,start = 2,nchar(dat_to_json)-1) 
  return(dat_to_json)
}

text.clean = function(x){ 
  require("tm")
  x  =  gsub("<.*?>", " ", x)
  x  =  removeNumbers(x)  
  x  =  removeWords(x, stopwords("english"))
  x  =  removeWords(x, stopwords("russian"))
  x  =  removeWords(x, stopwords)
  x  =  stemDocument(x,language="russian")
  x  =  stripWhitespace(x)                  
  x  =  gsub("^\\s+|\\s+$", "", x)  
  x  =  removePunctuation(x)
  return(x)
}

download_elastic <- function(result_date,q){
  res <- Search(index = paste0("urls_",result_date), body=q , 
                type = 'news',source = paste("title","fullhtml",sep=",") ,scroll = "1m")
  
  out <- res$hits$hits
  
  hits <- 1
  
  while(hits != 0){
    res <- scroll(scroll_id = res$`_scroll_id`)
    hits <- length(res$hits$hits)
    if(hits > 0)
      out <- c(out, res$hits$hits)
  }
  
  as_tibble(do.call("smartbind",lapply(out,function(x) as.data.frame(x,stringsAsFactors=F))))
}

train_lda <- function(tem,k){
  it_train = itoken(text.clean(tem$X_source.fullhtml), 
                    preprocessor = tolower, 
                    ids = tem$X_id, 
                    progressbar = T)
  vocab = create_vocabulary(it_train,stopwords = stopwords, ngram = c(1L, 3L))
  
  pruned_vocab = prune_vocabulary(vocab, 
                                  term_count_min = 5, 
                                  doc_proportion_max = 0.5,
                                  doc_proportion_min = 0.001)
  
  vectorizer = vocab_vectorizer(pruned_vocab)
  
  dtm_train  = create_dtm(it_train, vectorizer)
  
  lda_model = LDA$new(n_topics = k)
  
  doc_topic_distr = 
    lda_model$fit_transform(x = dtm_train, n_iter = 2000, 
                            convergence_tol = 0.00001, n_check_convergence = 25, 
                            progressbar = T)
  
  gammaDF <- as_tibble(doc_topic_distr)
  names(gammaDF) <- c(1:k)
  
  toptopics <- tibble(X_id = attr(doc_topic_distr,"dimnames")[[1]],
                      theme = as.numeric(apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))][1])),
                      theme_quality = apply(gammaDF,1,max))
  
  tem <- left_join(tem,toptopics,by="X_id")
  slov <- left_join((tem %>% 
                       group_by(theme) %>% 
                       summarise(theme_quality=max(theme_quality)[1])),tem[c(6:8)],by=c("theme_quality","theme")) %>% 
    rename("theme2"="X_source.title")
  left_join(tem,(slov[!duplicated(slov[,'theme']),] %>% select(theme,theme2)),by=c("theme")) %>% 
    select(-theme) %>% 
    rename("theme"="theme2")
}

#Run code

print(paste("Start connection",Sys.time()))

connect(es_host = "13.59.27.188", es_port = 9200)

print(paste("Start download",Sys.time()))

out_df <- download_elastic(result_date,q)

print(paste("Downloaded",Sys.time()))

try(httpPUT(paste(es, paste0("urls_",result_date),"news","theme",sep="/"), csv_to_json(data.frame(dt=paste0(substr(as.character(Sys.time()),1,10),"T",substr(as.character(Sys.time()),12,20),"+0300")))))

startdate=paste0(substr(as.character(Sys.time()),1,10),"T",substr(as.character(Sys.time()),12,20),"+0300")

print(paste("Start training",Sys.time()))

out_df2 <- train_lda(out_df,nrow(out_df)/5)

gc()

print(paste("Trained",Sys.time()))

names(out_df2) <- c("_index","_type","_id","_score","title","fullhtml","theme_quality","theme")

print(paste("Start writing",Sys.time()))

con<-file(paste0("OU",result_date,".json"),open = 'a',encoding="UTF-8")

for(i in 1:nrow(out_df2)){
  write(print(paste0('{"update": {"_id": "',out_df2[i,3],'", "_type" : "news", "_index" : "',
                     out_df2[i,1],'"}}\n{"doc": {"theme": "',str_replace_all(gsub("|","",
                     str_replace_all(out_df2[i,8], "[[:punct:]]", "")), "[\r\n]" , ""),'", "theme_quality": ',as.numeric(out_df2[i,7]),'}}')),file=con,
        append=TRUE)
}

print(paste("Start bulking",Sys.time()))

closeAllConnections()

res <- withRestarts(docs_bulk(paste0("OU",result_date,".json")))

httpPUT(paste(es, paste0("urls_",result_date),"news","theme",sep="/"), csv_to_json(data.frame(dt=startdate,
                        dtpost=paste0(substr(as.character(Sys.time()),1,10),"T",substr(as.character(Sys.time()),12,20),"+0300"))))

print(paste("Bulked!",Sys.time()))
