library(rsconnect)

rsconnect::setAccountInfo(name='amolnanaware', token='0C7A9FF538F7980182E1BEE6EB4A6559', 
                          secret='nEPVN/XvREhOrKL6ysWElr7yUNUpo0xPpVkEiIMJ')
deployApp()




devtools::install_github("")
devtools::install_github("https://github.com/NanawareAmol/Brazilian_vegetation_prediction/blob/master/ShinyDashboard/maxent_1.3.3.1.tar.gz", 
               auth_token = "38bc6af8b837310f0d1638826a11e260e6b42829")

devtools::install_github(repo = "NanawareAmol/repo['Brazilian_vegetation_prediction/blob/master/ShinyDashboard/maxent_1.3.3.1.tar.gz'][@ref|#master]", 
                         auth_token = "38bc6af8b837310f0d1638826a11e260e6b42829")

username/repo[/subdir][@ref|#pull]
