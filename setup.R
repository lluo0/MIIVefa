install.packages('devtools')
install.packages('MIIVsem')
install.packages('roxygen2')
install.packages('lavaan')
library(devtools)
library(MIIVsem)
library(roxygen2)
library(lavaan)

devtools::document()
install.packages(EFAmiiv, repos = NULL, type="source")


testdata <- lavaan::HolzingerSwineford1939[,7:15]
EFAmiivR::EFAmiiv(testdata, .01)

