library(stm)

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
              "classfielditem","classfielditemsdiv","fieldlabelhiddendiv",                       
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

tem$bigtext <- mapply(function(x,y) paste(x,y,collapse=" "),tem$`Заголовок без знаков препинания`,tem$Описание)
processed <- textProcessor(tem$bigtext, metadata = tem,customstopwords =stopwords)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

poliblogPrevFit <- stm(documents = out$documents, vocab = out$vocab,K = 200, 
                       max.em.its = 75, data = out$meta,init.type = "Spectral")

labelTopics(poliblogPrevFit)

mod.out.corr <- topicCorr(poliblogPrevFit)
plot(mod.out.corr)

z<-tem$bigtext[-processed$docs.removed]
length(z)

thoughts <- findThoughts(poliblogPrevFit,texts=z[-c(1,2)],topics=c(1:200),thresh=0.0,n=5718)
th <- thoughts$index
names(th)
tops <- data_frame()
for(i in 1:5718){
  tops <- rbind(tops,data_frame(
    topic=c(1:200),
    text=unname(sapply(th,function(x)x[i]))
  ))
}
tops <- tops[!duplicated(tops[,"text"]),]
tops$text <- tops$text+2
tops$zag <- tem$`Заголовок без знаков препинания`[tops$text]
