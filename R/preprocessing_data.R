



preprocessing_data <- function(data, qvalue=0.75, miss_values=NULL){

#################################
get_dat_p <- function(x, qvalue, miss_values){
out       <- rep(NA, 20)
check_integer <- function(x){x == round(x)}
probs     <- c((1-qvalue)/2, (1-qvalue)/2+qvalue)
monthstr  <- "jan|feb|mrz|mar|apr|mai|may|jun|jul|aug|sep|okt|oct|nov|dez|dec"
miss_str  <- tolower(c(miss_values, 'NULL', 'NA', 'N/A', 'N A', 'NaN', '-Inf', 'Inf', '', ' ', '  '))
yearstr   <- paste0(192:203, collapse='[0-9]{1}([^0-9]|$)|')
x <- tolower(as.character(x))
x <- gsub(',', '.', x, fixed=TRUE)
x[which(x%in%miss_str)]  <- NA
nall  <- length(x)
n     <- sum(!is.na(x))
nmiss <- sum(is.na(x))
nchars<- nchar(x)
x_num <- as.numeric(x)
x_ppm <- as.numeric(gsub('[^.0-9]*', '', x))
maxid <- (max(x_ppm, na.rm=TRUE)-min(x_ppm, na.rm=TRUE)+1)
nnum  <- sum(!is.na(x_num))
ints  <- sum(check_integer(x_num), na.rm=TRUE)
ints2 <- sum(check_integer(x_ppm), na.rm=TRUE)
lagnum<- diff(sort(x_ppm))
lagnum[which(lagnum>100)]<- 100
luniq     <- length(unique(x[which(!is.na(x))]))
yearss_tx <- sum(grepl(yearstr, x))
months_tx <- sum(grepl(monthstr, x))
letter_tx <- length(grep('[a-z]', x, ignore.case = TRUE))
number_tx <- length(grep('[0-9]', x, ignore.case = TRUE))
#points_tx <- unlist(lapply(gregexpr('[.]', x), FUN=function(x){length(x[which(x>0)])}))
#trenns_tx <- unlist(lapply(gregexpr('[-:_/ ]', x), FUN=function(x){length(x[which(x>0)])}))
out[1]    <- n
out[2]    <- nmiss
out[3]    <- as.numeric(luniq==2)
out[4]    <- luniq/n
out[5]    <- as.numeric(luniq>2 & luniq<=12)
out[6]    <- as.numeric(luniq==1)
out[7]    <- nnum/n
out[8]    <- yearss_tx/n
out[9]    <- number_tx/n
out[10]   <- ints2/n
out[11]   <- as.numeric(n==0)
out[12]   <- as.numeric(luniq==n)
out[13]   <- letter_tx/n
out[14]   <- as.numeric(all(quantile(lagnum, probs, na.rm=TRUE)==1))
out[15]   <- ints/n
out[16]   <- as.numeric(quantile(nchars, probs[1], na.rm=TRUE)==quantile(nchars, probs[2], na.rm=TRUE))
out[17]   <- as.numeric(maxid>=nall & maxid<(nall+round(nall*0.05)))
out[18]   <- as.numeric(min(x_num, na.rm=TRUE)%in%c(0, 1))
out[19]   <- mad(nchars, na.rm=TRUE)
out[20]   <- months_tx/n
#out[21]   <- as.numeric(all(quantile(points_tx, probs, na.rm=TRUE)==1))
#out[22]   <- as.numeric(all(quantile(trenns_tx, probs, na.rm=TRUE)==2))
return(out)
}
#########################################################################
M  <- t(apply(data, MARGIN=2, FUN=get_dat_p, qvalue=qvalue, miss_values=miss_values))
M[which(is.na(M),      arr.ind = T)] <- 0
M[which(!is.finite(M), arr.ind = T)] <- 0
colnames(M)<-c('n', 'missings', 'binarity', 'uniqueness', 'categorical', 'constant', 'numeric', 'years', 'numbers', 'subintegers', 'allmiss', 'alluniq', 'letters', 'lag1', 'integer', 'nchar_stabl', 'range', 'start', 'nchar_mad', 'months')
vars        <-names(data)
M2          <-data.frame(vars, M)
return(M2)
}

