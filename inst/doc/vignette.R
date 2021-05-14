## ---- echo=FALSE, warning=FALSE, message=FALSE--------------------------------
library(knitr)
library(vtype)

## ---- results='asis'----------------------------------------------------------
knitr::kable(head(sim_nqc_data, 20), caption='Artificial not quality controlled data')

## ---- results='asis'----------------------------------------------------------
tab <- vtype(sim_nqc_data, miss_values='9999')
knitr::kable(tab, caption='Application example of vtype')

## ---- results='asis'----------------------------------------------------------
knitr::kable(vtype(sim_nqc_data[1:10,]), caption='Application example with small sample size')

## ---- results='asis'----------------------------------------------------------
knitr::kable(vtype(mtcars),  caption='Application example on data without errors')

