toptext <- readRDS("toptext.RDS")



tem <- readxl::read_excel("digest_test_R.xlsx",sheet = 2)
#tem$`Заголовок без знаков препинания` <- tolower(tem$`Заголовок без знаков препинания`)
tem<- distinct(tem)
tem <- left_join(tem,toptext,by=c("Заголовок"="text"))
#xlsx::write.xlsx(tem,"temy_net.xlsx")

library(XLConnect)


fileXls <- paste("new.xlsx")
unlink(fileXls, recursive = FALSE, force = FALSE)
exc <- loadWorkbook(fileXls, create = TRUE)
createSheet(exc,'Input')
saveWorkbook(exc)

writeWorksheet(exc, tem, sheet = "input", startRow = 1, startCol = 1)
saveWorkbook(exc)
