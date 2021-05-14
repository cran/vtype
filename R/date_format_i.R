

date_format_i <- function(x, qvalue=0.75, miss_values=NULL){
miss_str  <- tolower(c(miss_values, 'NULL', 'NA', 'N/A', 'NaN', '-Inf', 'Inf', '', ' ', '  '))
monthstr  <- "jan|feb|mrz|mar|apr|mai|may|jun|jul|aug|sep|okt|oct|nov|dez|dec"
x         <- tolower(as.character(x))
x[which(x%in%miss_str)] <- NA
nc        <- nchar(x[which(!is.na(x))])
x_num     <- as.numeric(x)
n         <- sum(!is.na(x))
n5        <- round(n*0.05)
probs     <- c((1-qvalue)/2, (1-qvalue)/2+qvalue)
pns    <- quantile(nc, prob=probs, na.rm=TRUE)
tren_1 <- length(grep('[-]', x, ignore.case = TRUE))>(n/2)
tren_2 <- length(grep('[/]', x, ignore.case = TRUE))>(n/2)
tren_3 <- length(grep('[.]', x, ignore.case = TRUE))>(n/2)
tren_4 <- length(grep('[[:blank:]]', x, ignore.case = TRUE))>(n/2)
tren_5 <- length(grep('[:]', x, ignore.case = TRUE))>(n/2)
months_tx <- (sum(grepl(monthstr, x))>(n/2))

form   <- '?'
if(all(pns==4) & min(x_num, na.rm=TRUE)>1900 & max(x_num, na.rm=TRUE)<2035){
form<- "%Y"
}
if(all(pns==5) & tren_5 & !tren_1 & !tren_2 & !tren_3 & !tren_4){
form<- "H:M"
}
if(all(pns==8) & tren_5 & !tren_1 & !tren_2 & !tren_3 & !tren_4){
form<- "H:M:S"
}
if(all(pns==8) & !tren_5){
part_1 <- as.numeric(gsub('[-/.[:blank:]].*', '',  x))
part_2 <- as.numeric(gsub('[-/.[:blank:]].*$', '', gsub('^.*?[-/.[:blank:]]', '', x)))
part_3 <- as.numeric(gsub('^.*[-/.[:blank:]]', '',  x))
part_1 <- quantile(part_1, prob=c(0.05, 0.95), na.rm=TRUE)
part_2 <- quantile(part_2, prob=c(0.05, 0.95), na.rm=TRUE)
part_3 <- quantile(part_3, prob=c(0.05, 0.95), na.rm=TRUE)

pf1 <- '%d'
if(part_1[1]<1 | part_1[2]>31) pf1 <- '%y'
pf2 <- '%m'
if(part_2[2]>12)  pf2 <- '%d'
if(part_2[2]>12 & pf1=='%d') pf1 <- '%m'
pf3 <- '%y'
if(pf1=='%y' & pf2=='%m') pf3 <- '%d'
if(pf1=='%y' & pf2=='%d') pf3 <- '%m'
if(tren_1) form<- paste0(pf1,'-', pf2, '-', pf3)
if(tren_2) form<- paste0(pf1,'/', pf2, '/', pf3)
if(tren_3) form<- paste0(pf1,'.', pf2, '.', pf3)
if(tren_4) form<- paste0(pf1,' ', pf2, ' ', pf3)
}

if(all(pns==9) & !months_tx){
part_1 <- as.numeric(gsub('[-/.[:blank:]].*', '',  x))
part_2 <- as.numeric(gsub('[-/.[:blank:]].*$', '', gsub('^.*?[-/.[:blank:]]', '', x)))
part_3 <- as.numeric(gsub('^.*[-/.[:blank:]]', '',  x))
part_1 <- quantile(part_1, prob=c(0.05, 0.95), na.rm=TRUE)
part_2 <- quantile(part_2, prob=c(0.05, 0.95), na.rm=TRUE)
part_3 <- quantile(part_3, prob=c(0.05, 0.95), na.rm=TRUE)

pf1 <- '%m'
if(part_1[2]>12 & part_1[2]<=31) pf1 <- '%d'
if(part_1[2]>31)                 pf1 <- '%Y'
pf2 <- '%d'
if(pf1=='%d')  pf2 <- '%m'
pf3 <- '%Y'
if(pf1=='%Y' & pf2=='%m') pf3 <- '%d'
if(pf1=='%Y' & pf2=='%d') pf3 <- '%m'

if(tren_1) form<- paste0(pf1,'-', pf2, '-', pf3)
if(tren_2) form<- paste0(pf1,'/', pf2, '/', pf3)
if(tren_3) form<- paste0(pf1,'.', pf2, '.', pf3)
if(tren_4) form<- paste0(pf1,' ', pf2, ' ', pf3)
}
if((pns[1]==8 | pns[1]==9) & (pns[2]==9 | pns[2]==10) &!months_tx & !tren_5){
part_1 <- as.numeric(gsub('[-/.[:blank:]].*', '',  x))
part_2 <- as.numeric(gsub('[-/.[:blank:]].*$', '', gsub('^.*?[-/.[:blank:]]', '', x)))
part_3 <- as.numeric(gsub('^.*[-/.[:blank:]]', '',  x))
part_1 <- quantile(part_1, prob=c(0.05, 0.95), na.rm=TRUE)
part_2 <- quantile(part_2, prob=c(0.05, 0.95), na.rm=TRUE)
part_3 <- quantile(part_3, prob=c(0.05, 0.95), na.rm=TRUE)

pf1 <- '%m'
if(part_1[2]>12 & part_1[2]<=31) pf1 <- '%d'
if(part_1[2]>31)                 pf1 <- '%Y'
pf2 <- '%d'
if(pf1=='%d')  pf2 <- '%m'
pf3 <- '%Y'
if(pf1=='%Y' & pf2=='%m') pf3 <- '%d'
if(pf1=='%Y' & pf2=='%d') pf3 <- '%m'

if(tren_1) form<- paste0(pf1,'-', pf2, '-', pf3)
if(tren_2) form<- paste0(pf1,'/', pf2, '/', pf3)
if(tren_3) form<- paste0(pf1,'.', pf2, '.', pf3)
if(tren_4) form<- paste0(pf1,' ', pf2, ' ', pf3)
}
if(all(pns==9) & months_tx & (tren_1 | tren_2 | tren_3 | tren_4)){
part_1 <- as.numeric(gsub('[-/.[:blank:]].*', '',  x))
part_2 <- as.numeric(gsub('[-/.[:blank:]].*$', '', gsub('^.*?[-/.[:blank:]]', '', x)))
part_3 <- as.numeric(gsub('^.*[-/.[:blank:]]', '',  x))
part_1 <- quantile(part_1, prob=c(0.05, 0.95), na.rm=TRUE)
part_2 <- quantile(part_2, prob=c(0.05, 0.95), na.rm=TRUE)
part_3 <- quantile(part_3, prob=c(0.05, 0.95), na.rm=TRUE)

pf1 <- '%d'
if(part_1[2]>31 | part_1[1]<1) pf1 <- '%y'
pf2 <- '%b'
pf3 <- '%y'
if(pf1=='%y') pf3 <- '%d'
if(tren_1) form<- paste0(pf1,'-', pf2, '-', pf3)
if(tren_2) form<- paste0(pf1,'/', pf2, '/', pf3)
if(tren_3) form<- paste0(pf1,'.', pf2, '.', pf3)
if(tren_4) form<- paste0(pf1,' ', pf2, ' ', pf3)
}

if(all(pns==9) & months_tx & !tren_1 & !tren_2 & !tren_3 & !tren_4){
part_1 <- as.numeric(gsub('[a-z].*', '',  x))
part_3 <- as.numeric(gsub('^.*[a-z]', '',  x))
part_1 <- quantile(part_1, prob=c(0.05, 0.95), na.rm=TRUE)
part_3 <- quantile(part_3, prob=c(0.05, 0.95), na.rm=TRUE)

pf1 <- '%d'
if(part_1[2]>31 | part_1[1]<1) pf1 <- '%Y'
pf2 <- '%b'
pf3 <- '%Y'
if(pf1=='%Y') pf3 <- '%d'
form<- paste0(pf1,'', pf2, '', pf3)
}

if(all(pns==10)){
part_1 <- as.numeric(gsub('[-/.[:blank:]].*', '',  x))
part_2 <- as.numeric(gsub('[-/.[:blank:]].*$', '', gsub('^.*?[-/.[:blank:]]', '', x)))
part_3 <- as.numeric(gsub('^.*[-/.[:blank:]]', '',  x))
part_1 <- quantile(part_1, prob=c(0.05, 0.95), na.rm=TRUE)
part_2 <- quantile(part_2, prob=c(0.05, 0.95), na.rm=TRUE)
part_3 <- quantile(part_3, prob=c(0.05, 0.95), na.rm=TRUE)

pf1 <- '%d'
if(part_1[2]>31) pf1 <- '%Y'
pf2 <- '%m'
if(part_2[2]>12)  pf2 <- '%d'
if(part_2[2]>12 & pf1=='%d') pf1 <- '%m'
pf3 <- '%Y'
if(pf1=='%Y' & pf2=='%m') pf3 <- '%d'
if(pf1=='%Y' & pf2=='%d') pf3 <- '%m'

if(tren_1) form<- paste0(pf1,'-', pf2, '-', pf3)
if(tren_2) form<- paste0(pf1,'/', pf2, '/', pf3)
if(tren_3) form<- paste0(pf1,'.', pf2, '.', pf3)
if(tren_4) form<- paste0(pf1,' ', pf2, ' ', pf3)
}
if(all(pns==11) & months_tx){
part_1 <- as.numeric(gsub('[-/.[:blank:]].*', '',  x))
part_3 <- as.numeric(gsub('^.*[-/.[:blank:]]', '',  x))
part_1 <- quantile(part_1, prob=c(0.05, 0.95), na.rm=TRUE)
part_3 <- quantile(part_3, prob=c(0.05, 0.95), na.rm=TRUE)
part_1[which(is.na(part_1))] <- -1

pf1 <- '%d'
if(part_1[1]<0 & part_1[2]<0)  pf1 <- '%b'
if(part_1[2]>31)               pf1 <- '%Y'
pf2 <- '%b'
if(pf1=='%b') pf2 <- '%d'
pf3 <- '%Y'
if(pf1=='%Y' & pf2=='%b') pf3 <- '%d'
if(pf1=='%Y' & pf2=='%d') pf3 <- '%b'
if(tren_1) form<- paste0(pf1,'-', pf2, '-', pf3)
if(tren_2) form<- paste0(pf1,'/', pf2, '/', pf3)
if(tren_3) form<- paste0(pf1,'.', pf2, '.', pf3)
if(tren_4) form<- paste0(pf1,' ', pf2, ' ', pf3)
}

if(all(pns==16) & tren_4 & tren_5){
x      <- gsub('.{3}[:].*$', '',  x)
part_1 <- as.numeric(gsub('[-/.[:blank:]].*', '',  x))
part_2 <- as.numeric(gsub('[-/.[:blank:]].*$', '', gsub('^.*?[-/.[:blank:]]', '', x)))
part_3 <- as.numeric(gsub('^.*[-/.[:blank:]]', '',  x))
part_1 <- quantile(part_1, prob=c(0.05, 0.95), na.rm=TRUE)
part_2 <- quantile(part_2, prob=c(0.05, 0.95), na.rm=TRUE)
part_3 <- quantile(part_3, prob=c(0.05, 0.95), na.rm=TRUE)

pf1 <- '%d'
if(part_1[2]>31) pf1 <- '%Y'
pf2 <- '%m'
if(part_2[2]>12)  pf2 <- '%d'
if(part_2[2]>12 & pf1=='%d') pf1 <- '%m'
pf3 <- '%Y'
if(pf1=='%Y' & pf2=='%m') pf3 <- '%d'
if(pf1=='%Y' & pf2=='%d') pf3 <- '%m'

if(tren_1) form<- paste0(pf1,'-', pf2, '-', pf3, ' %H:%M')
if(tren_2) form<- paste0(pf1,'/', pf2, '/', pf3, ' %H:%M')
if(tren_3) form<- paste0(pf1,'.', pf2, '.', pf3, ' %H:%M')
if(tren_4 & !tren_1 & !tren_2 & !tren_3) form<- paste0(pf1,' ', pf2, ' ', pf3, ' %H:%M')
}


if(all(pns==19) & tren_4 & tren_5){
x      <- gsub('.{3}[:].*$', '',  x)
part_1 <- as.numeric(gsub('[-/.[:blank:]].*', '',  x))
part_2 <- as.numeric(gsub('[-/.[:blank:]].*$', '', gsub('^.*?[-/.[:blank:]]', '', x)))
part_3 <- as.numeric(gsub('^.*[-/.[:blank:]]', '',  x))
part_1 <- quantile(part_1, prob=c(0.05, 0.95), na.rm=TRUE)
part_2 <- quantile(part_2, prob=c(0.05, 0.95), na.rm=TRUE)
part_3 <- quantile(part_3, prob=c(0.05, 0.95), na.rm=TRUE)

pf1 <- '%d'
if(part_1[2]>31) pf1 <- '%Y'
pf2 <- '%m'
if(part_2[2]>12)  pf2 <- '%d'
if(part_2[2]>12 & pf1=='%d') pf1 <- '%m'
pf3 <- '%Y'
if(pf1=='%Y' & pf2=='%m') pf3 <- '%d'
if(pf1=='%Y' & pf2=='%d') pf3 <- '%m'

if(tren_1) form<- paste0(pf1,'-', pf2, '-', pf3, ' %H:%M:%S')
if(tren_2) form<- paste0(pf1,'/', pf2, '/', pf3, ' %H:%M:%S')
if(tren_3) form<- paste0(pf1,'.', pf2, '.', pf3, ' %H:%M:%S')
if(tren_4 & !tren_1 & !tren_2 & !tren_3) form<- paste0(pf1,' ', pf2, ' ', pf3, ' %H:%M:%S')

}
return(form)
}
