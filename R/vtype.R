










vtype <-function(data, qvalue=0.75, miss_values=NULL){
if(class(data)!='data.frame') stop('data must be a data.frame')  
if(!is.numeric(qvalue))       stop('qvalue must be a numeric value from [0.1, 1.0]')  
if(qvalue<0.1)  warning('qvalue below 0.1 are not allowed, it was set to 0.1!')    
if(qvalue>1.0)  warning('qvalue above 1.0 are not allowed, it was set to 1.0!')    
miss_values <- as.character(miss_values)  
if(!is.vector(miss_values)) stop('miss_values must be a character vector or single string')  
   
aggregated      <- suppressWarnings(preprocessing_data(data, qvalue=qvalue, miss_values=miss_values))
OUT             <- postprocessing_data(fitted_model_rf, agr_data=aggregated, data=data, qvalue=qvalue, miss_values=miss_values)
return(OUT)
}

