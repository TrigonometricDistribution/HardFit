
# Initial setup
usethis::use_gpl3_license("Lucas Gallindo")
usethis::use_readme_md()

# Prepping up
ctv::install.views("Distributions")
install.packages("reticulate")

# Dependencies
usethis::use_dev_package("CosW", type = "Imports")
usethis::use_package("SecKW", type = "Imports")
usethis::use_package("SinIW", type = "Imports")
usethis::use_package("TanB", type = "Imports")

# Suggests
usethis::use_package("ctv", type = "Suggests")
usethis::use_package("distr", type = "Suggests")
usethis::use_package("Compounding", type = "Suggests")
usethis::use_package("extraDistr", type = "Suggests")
usethis::use_package("LaplacesDemon", type = "Suggests")
usethis::use_package("VGAM", type = "Suggests")
usethis::use_package("DiscreteInverseWeibull", type = "Suggests")
usethis::use_package("gambin", type = "Suggests")
usethis::use_package("reliaR", type = "Suggests")
usethis::use_package("poweRlaw", type = "Suggests")
usethis::use_package("sadists", type = "Suggests")
usethis::use_package("actuar", type = "Suggests")
usethis::use_package("HistogramTools", type = "Suggests")
usethis::use_package("PDQutils", type = "Suggests")
usethis::use_package("distrEx", type = "Suggests")
usethis::use_package("distrMod", type = "Suggests")
usethis::use_package("AtelieR", type = "Suggests")
usethis::use_package("DistributionUtils", type = "Suggests")
usethis::use_package("lmomco", type = "Suggests")
usethis::use_package("Lmoments", type = "Suggests")
usethis::use_package("GMD", type = "Suggests")
usethis::use_package("MASS", type = "Suggests")
usethis::use_package("fitdistrplus", type = "Suggests")
usethis::use_package("EnvStats", type = "Suggests")
usethis::use_package("fitteR", type = "Suggests")
usethis::use_package("flexsurv", type = "Suggests")
usethis::use_package("msm", type = "Suggests")
usethis::use_package("benchden", type = "Suggests")
usethis::use_package("rlecuyer", type = "Suggests")
usethis::use_package("random", type = "Suggests")
usethis::use_package("RDieHarder", type = "Suggests")
usethis::use_package("randtoolbox", type = "Suggests")
usethis::use_package("SuppDists", type = "Suggests")
usethis::use_package("rust", type = "Suggests")
usethis::use_package("modeest", type = "Suggests")
usethis::use_package("ORDER2PARENT", type = "Suggests")
usethis::use_package("Newdistns", type = "Suggests")
usethis::use_package("kolmim", type = "Suggests")
usethis::use_package("sn", type = "Suggests")
usethis::use_package("rstream", type = "Suggests")

# Tests
usethis::use_test("build_trig_loglikelihood")
usethis::use_test("loglikelihood")
usethis::use_test("fit_poisson")
usethis::use_test("fit_CosW")
usethis::use_test("luciano-sinape2018")

# Test count data
# Reference dataset from: https://web.archive.org/web/20180413222159/https://www.r-bloggers.com/estimating-arrival-times-of-people-in-a-shop-using-r/
# Alternative link: https://web.archive.org/web/20170715010653/http://firsttimeprogrammer.blogspot.com/2015/07/estimating-arrival-times-of-people-in.html
# Alternative link: https://www.r-bloggers.com/estimating-arrival-times-of-people-in-a-shop-using-r/
customer_arrival <- read.csv(unzip('inst/testdata/data.zip'), header = TRUE)
# Data debug
# Total minutes ~ 60 minutes (~ 1 hour of observations)
total_minutes <- sum(customer_arrival$min)+sum(customer_arrival$sec)/60+sum(customer_arrival$cent)/100/60
print(total_minutes)
# Compute the inter-arrival times in minutes
interarrivals <- customer_arrival$min + customer_arrival$sec/60 + customer_arrival$cent/100/60
# Range of the bins for the histogram
bins.range <- 0.3
# Intervals of the histograms
breaks.points1 <- seq(0, max(interarrivals)+1, bins.range)
# Histogram of observed data
x.lab <- 'Minutes'
y.lab <- 'Frequencies'
main.lab <- 'Interarrival times'
tmp <- hist(
  interarrivals
  ,breaks = breaks.points1
  ,xlab = x.lab
  ,ylab = y.lab
  ,main = main.lab
  ,col = 'cyan'
  ,border = 'blue'
  ,density = 30
) 
# Boxplot of observed data
boxplot(interarrivals,col='cyan',border='blue',horizontal=TRUE,xlab='Minutes',
        main='Interarrival times')


URL<-"https://raw.githubusercontent.com/Lionel68/Jena_Exp/master/stats/ToLog_function.R"
download.file(URL,destfile=paste0(getwd(),"/ToLog_function.R"),method="curl")
source("ToLog_function.R")
