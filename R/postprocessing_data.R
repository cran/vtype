
postprocessing_data <- function(rf_model, agr_data, data, qvalue=0.75, miss_values=NULL){
miss_str  <- tolower(c(miss_values, 'NULL',  'NA', 'N/A', 'NaN', '-Inf', 'Inf', '', ' ', '  '))

####################################
num_format <- function(x, qvalue=0.75){
x <- suppressWarnings(as.numeric(x))
x <- x[which(!is.na(x))]
n <- length(x)
sum_integer <- function(x){sum(x == round(x), na.rm = TRUE)}
form <- 'floating'
if((sum_integer(x)/n)>=qvalue) form <- 'integer'
return(form)
}
####################################

####################################
binary_format <- function(x, miss_str=NULL){
x[which(x%in%miss_str)] <- NA
x <- x[which(!is.na(x))]
values <- substr(sort(names(sort(table(x), T))[1:2]), 1, 9)
form   <- paste0(values[1], '/', values[2])
return(form)
}
####################################

####################################
categ_format <- function(x, miss_str=NULL){
x[which(x%in%miss_str)] <- NA
x <- x[which(!is.na(x))]
nc        <- nchar(unique(x))
form      <- 'labels'
if(all(nc<=2)){
x <- suppressWarnings(as.numeric(x))
form   <- paste0(min(x, na.rm=TRUE), '-', max(x, na.rm=TRUE))
}
return(form)
}
####################################

####################################
constant_format <- function(x, miss_str=NULL){
x[which(x%in%miss_str)] <- NA
value <- unique(x[which(!is.na(x))])[1]
form  <- substr(value, 1, 9)
if(nchar(value)>9) form <- paste(form, '.')
return(form)
}
####################################


pred       <- predict(rf_model, newdata=agr_data, type='prob')
alternativ <- ptype <- prob <- formatx <- exampl <- rep(NA, nrow(pred))
vlevels <- colnames(pred)
for(i in 1:nrow(pred)){
probas  <- pred[i,]
idm     <- order(probas, decreasing=TRUE)
ptype[i]<- vlevels[idm[1]]
prob[i] <- probas[idm[1]]
if(probas[idm[2]]>0.01)  alternativ[i] <- paste0(vlevels[idm[2]], ' (', round(probas[idm[2]]*100, 1), '%)')
if(probas[idm[2]]<=0.01) alternativ[i] <- '--'
v <- tolower(as.character(data[, agr_data$vars[i]]))
v <- gsub(',', '.', v, fixed=TRUE)
formatx[i] <- ''
if(ptype[i]=='date'){
suppressWarnings(try(formatx[i]  <- date_format_i(v, qvalue, miss_str), silent = T))
}
if(ptype[i]=='continuous'){
suppressWarnings(try(formatx[i]  <- num_format(v, qvalue), silent = T))
}
if(ptype[i]=='binary'){
suppressWarnings(try(formatx[i]  <- binary_format(v, miss_str), silent = T))
}
if(ptype[i]=='categorical'){
suppressWarnings(try(formatx[i]  <- categ_format(v, miss_str), silent = T))
}
if(ptype[i]=='constant'){
suppressWarnings(try(formatx[i]  <- constant_format(v), silent = T))
}

exampl[i] <- as.character(v[which(!is.na(v))][1])
}
overclass <- ''
overclass[which(ptype%in%c('missing', 'constant'))]      <- 'uninformative'
overclass[which(ptype%in%c('binary', 'categorical'))]    <- 'qualitative'
overclass[which(ptype%in%c('continuous'))]               <- 'quantitative'
overclass[which(ptype%in%c('date', 'ID', 'text'))]       <- 'supportive'
OUT <- data.frame(variable=agr_data$vars, type=ptype, probability=round(prob, 3), format=formatx, class=overclass, alternative=alternativ, n=agr_data$n, missings=agr_data$missings)
return(OUT)
}


