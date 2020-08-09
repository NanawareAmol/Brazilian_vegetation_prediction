library(rsconnect)

rsconnect::setAccountInfo(name='amolnanaware', token='0C7A9FF538F7980182E1BEE6EB4A6559', 
                          secret='nEPVN/XvREhOrKL6ysWElr7yUNUpo0xPpVkEiIMJ')
deployApp()

devtools::install_github("cran/SDMTools")

# username/repo[/subdir][@ref|#pull]
install.packages("caret")
