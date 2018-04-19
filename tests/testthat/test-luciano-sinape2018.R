context("test-luciano-sinape2018.R")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

require(pracma)
require(stats)
#Cot Gumbel

GG <- function(x, alpha, beta){
  (pi/2)*exp(-(exp(-(-x+alpha)/beta)*beta+alpha-x)/beta)/(beta*cos((1/4)*pi*exp(-exp(-(-x+alpha)/beta)))^2)
}

x <- seq(0, 4, length = 1000)
alpha  <- 1.0;   beta  <- 1.0;  #black
alpha1 <- 0.1;   beta1 <- 0.8;  #blue
alpha2 <- 1.5;   beta2 <- 1.0;  #red
alpha3 <- 4.0;   beta3 <- 1.0;  #pink
alpha4 <- 1.0;   beta4 <- 0.5;  #brown

curve(
  GG(x, alpha,beta)
  ,col = "black"
  ,cex.lab = 0.9
  ,cex.main = 0.5
  ,cex.axis = 0.9
  ,lty = 1
  ,lwd = 2
  ,ylim = c(0,1)
  ,type = "l"
  ,from = 0
  ,to = 4
  ,xlab = "x"
  ,ylab = "Probability"
)

G1 <- GG(x, alpha1, beta1)
G2 <- GG(x, alpha2, beta2)
G3 <- GG(x, alpha3, beta3)
G4 <- GG(x, alpha4, beta4)

lines(x, G1, lty=2, lwd=2, col="blue")
lines(x, G2, lty=3, lwd=2, col="red")
lines(x, G3, lty=4, lwd=2, col="pink")
lines(x, G4, lty=5, lwd=2, col="brown")

legend(
  'toprigh'
  ,c(expression(
     list(alpha==1.5 ,beta==1.5)
    ,list(alpha==1.5, beta==1.5)
    ,list(alpha==0.5, beta==1.5)
    ,list(alpha==2.0, beta==1.5)
    ,list(alpha==3.5, beta==1.5)
  ))
  ,ncol = 1
  ,bty = "n"
  ,lty =c (1, 2, 3, 4, 5)
  ,cex = 0.7
  ,lwd = 2
)


# temp = scan()
temp <- c(4.00, 3.06, 2.55, 3.19, 3.83, 3.96, 3.39, 2.93,
2.86, 4.56, 6.80, 8.35, 9.68, 10.81, 11.56, 12.22,
12.64, 12.71, 12.47, 11.88, 10.85, 9.95, 8.74, 7.34,
5.72, 4.40, 3.27, 2.50, 1.94, 1.74, 1.58, 1.29,
1.25, 3.06, 6.02, 8.77, 11.00, 11.93, 12.11, 12.75,
13.20, 13.34, 13.18, 12.52, 11.41, 11.01, 10.69, 9.89,
8.60, 8.03, 8.01, 7.74, 7.35, 6.86, 6.37, 5.83,
5.85, 6.21, 6.50, 7.84, 8.88, 9.78, 11.10, 11.70,
12.00, 12.07, 11.63, 11.09, 10.08, 9.32, 8.65, 8.12,
7.44, 6.32, 5.79, 4.83, 2.75, 1.84, 0.29 -1.14,
-0.53, 2.52, 5.65, 8.19, 9.24, 10.13, 10.83, 11.24,
11.25, 10.05, 9.34, 8.39, 7.61, 7.20, 6.92, 6.77,
6.73, 6.80, 7.14, 7.39, 7.30, 6.83, 6.62, 6.69,
7.06, 7.90, 8.55, 9.45, 9.76, 9.45, 11.00, 11.07,
9.20, 9.65, 9.97, 11.30, 9.92, 6.19, 4.88, 4.22,
3.39, 3.13, 3.07, 3.06, 3.30, 3.97, 4.37, 4.03,
3.61, 5.04, 5.92, 6.24, 6.57, 7.00, 8.66, 8.96,
9.34, 9.99, 10.22, 10.05, 9.54, 8.20, 6.23, 4.09,
3.02, 2.49, 2.80, 2.79, 2.16, 1.43, 1.86, 2.41,
3.52, 5.65, 7.91, 10.45, 12.33, 13.19, 14.37, 15.12,
15.48, 15.45, 15.01, 14.70, 13.35, 11.58, 11.24, 9.94,
8.86, 8.00, 7.29, 6.62, 5.65, 4.73, 3.86, 2.85,
2.44, 4.69, 7.37, 9.54, 10.93, 12.13, 13.04, 13.66,
13.96, 13.94, 13.65, 13.10, 12.00, 10.94, 10.04, 9.00)


# speed = scan()
speed <- c(10.50, 7.39, 9.26, 14.84, 8.22, 11.62, 33.83, 4.51, 26.65, 5.40,
9.50, 5.99, 10.39, 14.93, 10.95, 10.93, 28.55, 9.69, 26.76, 7.90,
10.74, 7.17, 11.84, 14.51, 12.39, 8.05, 23.62, 11.98, 24.72, 8.53,
11.20, 7.86, 13.44, 15.50, 14.58, 5.04, 18.75, 12.29, 23.69,10.59,
11.48, 8.31, 12.30, 15.88, 18.60, 7.75, 13.85, 12.18, 23.86,11.92,
12.77, 7.63, 9.03, 16.68, 22.32, 10.66, 14.15, 13.25, 20.66,12.68,
12.73, 7.42, 8.85, 15.19, 19.08, 7.95, 16.10, 14.76, 20.78,13.61,
12.77, 6.76, 15.11, 13.89, 19.83, 7.57, 14.51, 15.27, 19.21,13.04,
11.81, 6.29, 12.76, 10.69, 22.67, 9.69, 10.20, 13.75, 11.73,10.46,
11.02, 5.82, 11.01, 11.49, 22.08, 10.31, 13.36, 12.74, 11.59, 8.40,
10.97, 5.00, 9.47, 10.00, 20.93, 15.23, 16.98, 11.09, 8.65, 6.41,
11.79, 4.58, 9.42, 5.62, 18.36, 21.99, 18.35, 11.38, 6.29, 4.80,
12.66, 3.40, 10.54, 4.33, 18.42, 16.92, 18.79, 13.04, 7.70,
12.64, 1.61, 11.30, 6.57, 18.21, 21.27, 17.73, 14.26, 8.91,
12.54, 1.84, 11.87, 9.79, 17.45, 21.60, 14.51, 16.49, 7.41,
13.51, 3.83, 13.28, 11.09, 17.19, 13.91, 14.11, 17.37, 6.49,
14.77, 7.73, 14.51, 8.29, 13.49, 34.46, 12.43, 20.50, 6.44,
15.02, 8.31, 14.80, 5.00, 9.06, 41.38, 11.53, 21.31, 4.38,
13.76, 4.38, 13.28, 6.84, 6.92, 42.99, 8.71, 24.44, 3.62,
10.74, 7.17, 12.90, 8.77, 9.78, 36.86, 6.11, 27.00, 2.41)

hist(speed)

#x=speed; x=temp



pdf_CotGumb=function(x,alpha,beta){
  -(2/3)*pi*exp(-(exp(-(-x+alpha)/beta)*beta+alpha-x)/beta)*sin((1/3)*pi*(-1+exp(-exp(-(-x+alpha)/beta))))/(beta*cos((1/3)*pi*(-1+exp(-exp(-(-x+alpha)/beta))))^2)
}


cdf_CotGumbfunction(x,alpha,beta){
  sec((1/3)*pi*(1-exp(-exp((x-alpha)/beta))))-1
}


#            Cos Weibull - pdf distribution function.

pdf_cotG <- function(par,x){
  alpha      = par[1]
  beta       = par[2]
  -(2/3)*pi*exp(-(exp(-(-x+alpha)/beta)*beta+alpha-x)/beta)*sin((1/3)*pi*(-1+exp(-exp(-(-x+alpha)/beta))))/(beta*cos((1/3)*pi*(-1+exp(-exp(-(-x+alpha)/beta))))^2)
}

#         Cos Weibull- Cumulative distribution function.
cdf_cotG <- function(par,x){
  alpha      = par[1]
  beta       = par[2]
  sec((1/3)*pi*(1-exp(-exp((x-alpha)/beta))))-1
}
p4=goodness.fit(pdf=pdf_cotG, cdf=cdf_cotG, starts =c(1, 1), data = x, method="S",  domain=c(0,Inf),mle=NULL)
mle4=p4$mle
aic4=p4$AIC



require(stats)
require(pracma)
require(AdequacyModel)
#            Cos Weibull - pdf distribution function.

pdf_cosW <- function(par,x){
  alpha      = par[1]
  lambda     = par[2]
  ((pi/2))*((alpha*lambda^(alpha)*x^(alpha-1))*exp(-(lambda*x)^alpha))*sin((pi/2)*((1-exp(-(lambda*x)^alpha))))
}

#         Cos Weibull- Cumulative distribution function.
cdf_cosW <- function(par,x){
  alpha      = par[1]
  lambda     = par[2]
  1-cos((pi/2)*((1-exp(-(lambda*x)^alpha))))
}
p1=goodness.fit(pdf=pdf_cosW, cdf=cdf_cosW, starts =c(1, 1), data = x, method="S",  domain=c(0,Inf),mle=NULL)
mle1=p1$mle
aic1=p1$AIC
#------------------------------------------------------------


# Weibull - pdf distribution function.
pdf_W <- function(par,x){
  alpha      = par[1]
  lambda     = par[2]
  (alpha*lambda^(alpha)*x^(alpha-1))*exp(-(lambda*x)^alpha)
}
# Weibull- Cumulative distribution function.
cdf_W <- function(par,x){
  alpha      = par[1]
  lambda     = par[2]
  (1-exp(-(lambda*x)^alpha))
}
p2=goodness.fit(pdf=pdf_W, cdf=cdf_W,starts = c(1, 1), data = x, method="S", domain=c(0,1),mle=NULL)
mle2=p2$mle
aic2=p2$AIC


#------EXPONENTIAL EXPONENTIATED-----------

require(pracma)
require(AdequacyModel)
# EE - pdf distribution function.
pdf_EE <- function(par,x){
  alpha      = par[1]
  lambda     = par[2]
  (alpha*lambda)*exp(-(lambda*x))*(1-(exp(-(lambda*x))))^(alpha-1)
}
# EE- Cumulative distribution function.
cdf_EE <- function(par,x){
  alpha      = par[1]
  lambda     = par[2]
  (1-exp(-(x*lambda)))^alpha
}
p3=goodness.fit(pdf=pdf_EE, cdf=cdf_EE,starts =c(1,1), data = x,method="S", domain=c(0,Inf),mle=NULL)
mle3=p3$mle
aic3=p3$AIC

#------------------------------------------

pdf_W=function(x,alpha,lambda){
  (alpha*lambda^(alpha)*x^(alpha-1))*exp(-(lambda*x)^alpha)
}

pdf_EE=function(x,alpha,lambda){
  (alpha*lambda)*exp(-(lambda*x))*
    (1-(exp(-(lambda*x))))^(alpha-1)
}

pdf_cosW=function(x,alpha,lambda){
  ((pi/2))*((alpha*lambda^(alpha)*x^(alpha-1))*exp(-(lambda*x)^alpha))*sin((pi/2)*((1-exp(-(lambda*x)^alpha))))
}

par(pty="s")
hist(x,probability=T, col="white",ylim=c(0,0.25),cex.lab=0.9
    ,cex.axis=0.9,xlab="Time",main="(a)",
     ylab="Density")
curve(pdf_W(x,mle2[[1]], mle2[[2]]),
      lty = 4,lwd=2,col="blue",add=T)
curve(pdf_EE(x,mle3[[1]], mle3[[2]]),
      lty = 3,lwd=2,col="red",add=T)
curve(pdf_cosW(x, mle1[[1]], mle1[[2]]),
      cex.main=0.9,lty = 2,lwd=2,col="black",add=T)
curve(pdf_CotGumb(x, mle4[[1]], mle4[[2]]),
      cex.main=0.9,lty = 1, lwd=2,col="green",add=T)
legend('topright', c(expression(
  list(W),
  list(EE),
  list(Cos-W),
  list(Csc-Gumbell)
)),ncol=1,bty="n",col=c("blue","red","black","green"),
lty=c(4,3,2,1),cex =0.7,lwd=2)

aic1
aic2
aic3
aic4
#-----------------------------------------------

cdf_cosW=function(x,alpha,lambda){
  1-cos((pi/2)*(1-exp(-(lambda*x)^alpha)))
}

cdf_EE=function(x,alpha,lambda){
  (1-exp(-(x*lambda)))^alpha
}

cdf_W=function(x,alpha,lambda){
  (1-exp(-(lambda*x)^alpha))
}


require(stats)
plot.ecdf(x, col="violet",ylim=c(0,1),
          cex.lab=0.9,cex.axis=0.9,xlab="Time",
          main="(b)",ylab="Cumulative")
curve(cdf_W(x,1.27459359, 0.02312992),
      lty = 3,
      lwd=2,col="blue",add=T)
curve(cdf_EE(x,1.74593848, 0.03543284),
      lty = 2,lwd=2,
      col="red",add=T)
curve(cdf_cosW(x,0.93885070, 0.03599627),
      lty = 1,lwd=2,
      col="black",add=T)
legend('bottom', c(expression(
  list(W),
  list(EE),
  list(Cos-W)
)),ncol=1,bty="n",col=c("blue","red","black"),
lty=c(3,2,1),cex =0.7,lwd=2)

############################################ SIN-G POISSON 02-04-2018 ####################################################################################

# wind=scan()
wind <- c(12.76, 10.69, 22.67,  9.69, 10.20, 13.75, 11.73, 10.46, 23.77, 19.85,
11.01, 11.49, 22.08, 10.31, 13.36, 12.74, 11.59	, 8.40, 22.04, 21.61,
9.47, 10.00, 20.93, 15.23, 16.98, 11.09,  8.65	, 6.41, 19.59, 21.29,
9.42	, 5.62, 18.36, 21.99, 18.35, 11.38,  6.92	, 4.80, 17.97, 23.81,
10.54	, 4.33, 18.42, 16.92, 18.79, 13.04,  7.20	, 5.01, 15.19, 21.84,
11.30	, 6.57, 18.21, 21.27, 17.73, 14.26,  8.89	, 2.74, 15.04, 22.53,
11.87	, 9.79, 17.45, 21.60, 14.51, 16.49, 10.03	, 3.55, 15.83, 19.92,
13.28, 11.09, 17.19, 13.91, 14.11, 17.37, 10.37	, 5.01, 14.24, 17.08,
14.51	, 8.29, 13.49, 34.46, 12.43, 20.50, 11.17	, 4.69, 12.76, 15.48,
14.80	, 5.00,  9.06, 41.38, 11.53, 21.31, 10.39, 24.61, 10.04, 14.51,
13.28	, 6.84,  6.92, 42.99,  8.71, 24.44,  7.21, 13.68,  6.16, 16.14,
12.90	, 8.77,  9.78, 36.86,  6.11, 27.00, 10.50, 10.74,  7.56, 19.24,
14.84	, 8.22, 11.62, 33.83,  4.51, 26.65, 15.86, 13.44,  9.11,	
14.93, 10.95, 10.93, 28.55,  9.69, 26.76, 17.11, 14.11, 10.30,	
14.51, 12.39,  8.05, 23.62, 11.98, 24.72,  8.53, 14.92, 11.97,	
15.50, 14.58,  5.04, 18.75, 12.29, 23.69, 10.59, 12.76, 13.78,	
15.88, 18.60,  7.75, 13.85, 12.18, 23.86, 11.92, 12.86, 14.77,	
16.68, 22.32, 10.66, 14.15, 13.25, 20.66, 12.68, 15.98, 15.58,	
15.19, 19.08,  7.95, 16.10, 14.76, 20.78, 13.61, 21.33, 15.46,	
13.89, 19.83,  7.57, 14.51, 15.27, 19.21, 13.04, 23.69, 18.43)

###################################################################################################################


x=c(0.08, 2.09, 3.48, 4.87, 6.94, 8.66, 13.11, 23.63, 0.20, 2.23, 3.52, 4.98, 6.97, 9.02, 13.29, 0.40, 2.26,
    3.57, 5.06, 7.09, 9.22, 13.80, 25.74, 0.50, 2.46, 3.64, 5.09, 7.26, 9.47, 14.24, 25.82, 0.51, 2.54, 3.70,
    5.17, 7.28, 9.74, 14.76, 26.31, 0.81, 2.62, 3.82, 5.32, 7.32, 10.06, 14.77, 32.15, 2.64, 3.88, 5.32,
    7.39, 10.34, 14.83, 34.26, 0.90, 2.69, 4.18, 5.34, 7.59, 10.66, 15.96, 36.66, 1.05, 2.69, 4.23, 5.41,
    7.62, 10.75, 16.62, 43.01, 1.19, 2.75, 4.26, 5.41, 7.63, 17.12, 46.12, 1.26, 2.83, 4.33, 5.49, 7.66,
    11.25, 17.14, 79.05, 1.35, 2.87, 5.62, 7.87, 11.64, 17.36, 1.40, 3.02, 4.34, 5.71, 7.93, 11.79, 18.10,
    1.46, 4.40, 5.85, 8.26, 11.98, 19.13, 1.76, 3.25, 4.50, 6.25, 8.37, 12.02, 2.02, 3.31, 4.51, 6.54, 8.53,
    12.03, 20.28, 2.02, 3.36, 6.76, 12.07, 21.73, 2.07, 3.36, 6.93, 8.65, 12.63, 22.69)




# x=scan()
x <- c(115.14, 127.14,
69.00, 151.43,
43.86, 127.29,
67.80, 127.00,
51.29, 128.00,
46.27, 102.00,
61.20,  86.66,
44.97,  96.03,
88.16, 163.00,
61.19, 151.29,
71.84, 134.14,
77.31, 136.14,
186.43, 140.43,
156.14, 108.29,
148.00, 126.86,
146.43, 116.71,
150.86, 113.00,
148.43,  86.59,
146.43,  85.39)

#dados artigos gauss kumaraswamy-gumbel




# Sin-Gumbel Weibull Poisson- pdf distribution function.
pdf_SinGumWp <- function(par,x){
  alpha   = par[1]
  beta    = par[2]
  theta   = par[3]
  sigma   = par[4]
  lambda  = par[5]
  psi     = par[6]
  (1/2)*lambda*pi*exp(alpha/beta)*(exp((x/theta)^sigma-1))^(-1/psi)*(x/theta)^sigma*sigma*exp(-exp(alpha/beta)*(exp((x/theta)^sigma-1))^(-1/psi))*cos((1/2)*pi*exp(-exp(alpha/beta)*(exp((x/theta)^sigma-1))^(-1/psi)))*exp(-lambda*sin((1/2)*pi*exp(-exp(alpha/beta)*(exp((x/theta)^sigma-1))^(-1/psi))))/(psi*x*(1-exp(-lambda)))
}
#Sin-Gumbel Weibull Poisson - Cumulative distribution function.
cdf_SinGumWp <- function(par,x){
  alpha   = par[1]
  beta    = par[2]
  theta   = par[3]
  sigma   = par[4]
  lambda  = par[5]
  psi     = par[6]
  (1-exp(-lambda*sin((1/2)*pi*exp(-exp(alpha/beta)*(exp((x/theta)^sigma-1))^(-1/psi)))))/(1-exp(-lambda))
}

S=goodness.fit(pdf=pdf_SinGumWp, cdf=cdf_SinGumWp,starts =c(.1,.1,.1,.1,.1,.1),
               data = x, method="S",domain=c(0,Inf),mle=NULL)

SinGumWp=function(x,alpha,beta,theta, sigma,lambda,psi){
  (1/2)*lambda*pi*exp(alpha/beta)*(exp((x/theta)^sigma-1))^(-1/psi)*(x/theta)^sigma*sigma*exp(-exp(alpha/beta)*(exp((x/theta)^sigma-1))^(-1/psi))*cos((1/2)*pi*exp(-exp(alpha/beta)*(exp((x/theta)^sigma-1))^(-1/psi)))*exp(-lambda*sin((1/2)*pi*exp(-exp(alpha/beta)*(exp((x/theta)^sigma-1))^(-1/psi))))/(psi*x*(1-exp(-lambda)))
}

hist(x,probability=T, col="white",ylim=c(0,0.1),cex.lab=0.9
    ,cex.axis=0.9,xlab="Time",main="(a)",
     ylab="Density")
curve(SinGumWp(x,  mle[[1]],mle[[2]],mle[[3]],mle[[4]],mle[[5]],mle[[6]]),
      cex.main=0.9,lty = 1,lwd=2,col="black",add=T)
legend('top', c(expression(
  list(SinGumbelWeibullPoisson)
)),ncol=1,bty="n",col=c("blue"),
lty=c(3),cex =0.7,lwd=2)

mle=S$mle
aic=S$AIC
A=S$A
W=S$W
mle;aic;A;W

###################################################SIN H1 x H2 GUMBEL x WEIBULL ##########################################################
# SinH1H2- pdf distribution function.
pdf_SinH1H2 <- function(par,x){
  mu      = par[1]
  beta    = par[2]
  k       = par[3]
  lambda  = par[4]
  lambda*((1/2)*pi*exp(-(x-mu)/beta)*exp(-exp(-(x-mu)/beta))*(1-exp(-(x/lambda)^k))/beta+(1/2)*pi*exp(-exp(-(x-mu)/beta))*(x/lambda)^k*k*exp(-(x/lambda)^k)/x)*sin((1/2)*pi*exp(-exp(-(x-mu)/beta))*(1-exp(-(x/lambda)^k)))*exp(-lambda*(1-cos((1/2)*pi*exp(-exp(-(x-mu)/beta))*(1-exp(-(x/lambda)^k)))))/(1-exp(-lambda))
}
#SinH1H2 - Cumulative distribution function.
cdf_SinH1H2 <- function(par,x){
  mu      = par[1]
  beta    = par[2]
  k       = par[3]
  lambda  = par[4]
  (1-exp(-lambda*(1-cos((1/2)*pi*exp(-exp(-(x-mu)/beta))*(1-exp(-(x/lambda)^k))))))/(1-exp(-lambda))
}

S=goodness.fit(pdf=pdf_SinH1H2, cdf=cdf_SinH1H2,starts =c(.1,.1,.1,.1),
               data = x, method="S",domain=c(0,Inf),mle=NULL)

SinH1H2=function(x,mu,beta,k,lambda){
  lambda*((1/2)*pi*exp(-(x-mu)/beta)*exp(-exp(-(x-mu)/beta))*(1-exp(-(x/lambda)^k))/beta+(1/2)*pi*exp(-exp(-(x-mu)/beta))*(x/lambda)^k*k*exp(-(x/lambda)^k)/x)*sin((1/2)*pi*exp(-exp(-(x-mu)/beta))*(1-exp(-(x/lambda)^k)))*exp(-lambda*(1-cos((1/2)*pi*exp(-exp(-(x-mu)/beta))*(1-exp(-(x/lambda)^k)))))/(1-exp(-lambda))
}
hist(x,probability=T, col="white",ylim=c(0,0.015),cex.lab=0.9
    ,cex.axis=0.9,xlab="Time",main="(a)",
     ylab="Density")
curve(SinH1H2(x,  mle[[1]],mle[[2]],mle[[3]],mle[[4]]),
      cex.main=0.9,lty = 1,lwd=2,col="black",add=T)
legend('top', c(expression(
  list(SinH1xH2)
)),ncol=1,bty="n",col=c("blue"),
lty=c(3),cex =0.7,lwd=2)

mle=S$mle
aic=S$AIC
A=S$A
W=S$W
mle;aic;A;W

########################################################################################################

#############################cos Gumbel Weibull Poisson######################################################



# Cos-Gumbel Weibull Poisson- pdf distribution function.
pdf_CosGumWp <- function(par,x){
  alpha   = par[1]
  beta    = par[2]
  theta   = par[3]
  sigma   = par[4]
  lambda  = par[5]
  psi     = par[6]
  (1/2)*lambda*pi*exp(alpha/beta)*(exp((x/theta)^sigma-1))^(-1/psi)*(x/theta)^sigma*sigma*exp(-exp(alpha/beta)*(exp((x/theta)^sigma-1))^(-1/psi))*sin((1/2)*pi*exp(-exp(alpha/beta)*(exp((x/theta)^sigma-1))^(-1/psi)))*exp(-lambda*(1-cos((1/2)*pi*exp(-exp(alpha/beta)*(exp((x/theta)^sigma-1))^(-1/psi)))))/(psi*x*(1-exp(-lambda)))
}
#Cos-Gumbel Weibull Poisson - Cumulative distribution function.
cdf_CosGumWp <- function(par,x){
  alpha   = par[1]
  beta    = par[2]
  theta   = par[3]
  sigma   = par[4]
  lambda  = par[5]
  psi     = par[6]
  (1-exp(-lambda*(1-cos((1/2)*pi*exp(-exp(alpha/beta)*(exp((x/theta)^sigma-1))^(-1/psi))))))/(1-exp(-lambda))
}

S=goodness.fit(pdf=pdf_CosGumWp, cdf=cdf_CosGumWp,starts =c(.1,.1,.1,.1,.1,.1),
               data = x, method="B",domain=c(0,Inf),mle=NULL)



CosGumWp=function(x,alpha,beta,theta, sigma,lambda,psi){
  (1/2)*lambda*pi*exp(alpha/beta)*(exp((x/theta)^sigma-1))^(-1/psi)*(x/theta)^sigma*sigma*exp(-exp(alpha/beta)*(exp((x/theta)^sigma-1))^(-1/psi))*sin((1/2)*pi*exp(-exp(alpha/beta)*(exp((x/theta)^sigma-1))^(-1/psi)))*exp(-lambda*(1-cos((1/2)*pi*exp(-exp(alpha/beta)*(exp((x/theta)^sigma-1))^(-1/psi)))))/(psi*x*(1-exp(-lambda)))
}

hist(x,probability=T, col="white",ylim=c(0,0.015),cex.lab=0.9
    ,cex.axis=0.9,xlab="Time",main="(a)",
     ylab="Density")
curve(CosGumWp(x,  mle[[1]],mle[[2]],mle[[3]],mle[[4]],mle[[5]],mle[[6]]),
      cex.main=0.9,lty = 1,lwd=2,col="black",add=T)
legend('top', c(expression(
  list(CosGumbelWeibullPoisson)
)),ncol=1,bty="n",col=c("blue"),
lty=c(3),cex =0.7,lwd=2)

mle=S$mle
aic=S$AIC
A=S$A
W=S$W
mle;aic;A;W

#############################################################################################################


x=c(19.885, 20.940, 21.820, 23.700, 24.888, 25.460, 25.760, 26.720, 27.500, 28.100, 28.600, 30.200, 30.380,
    31.500, 32.600, 32.680, 34.400, 35.347, 35.700, 38.100, 39.020, 39.200, 40.000, 40.400, 40.400, 42.250, 44.020, 44.730,
    44.900, 46.300, 50.330, 51.442, 57.220, 58.700, 58.800, 61.200, 61.740, 65.440, 65.597, 66.000, 74.100, 75.800, 84.100,
    106.600, 109.700, 121.970, 121.970, 185.560) #dados artigo frecht-g

x=c( 5.9, 20.4, 14.9, 16.2, 17.2, 7.8, 6.1, 9.2, 10.2, 9.6, 13.3, 8.5, 21.6, 18.5, 5.1, 6.7, 17.0,
     8.6, 9.7, 39.2, 35.7, 15.7, 9.7, 10.0, 4.1, 36.0, 8.5, 8.0, 9.2, 26.2, 21.9, 16.7, 21.3, 35.4, 14.3, 8.5, 10.6, 19.1, 20.5, 7.1, 7.7,
     18.1, 16.5, 11.9, 7.0, 8.6, 12.5, 10.3, 11.2, 6.1, 8.4, 11.0, 11.6, 11.9, 5.2, 6.8, 8.9, 7.1, 10.8) #dados artigo frecht-g

require(AdequacyModel)
# HG1G2 exp and Weibull - pdf distribution function.
pdf_Wexp <- function(par,x){
  alpha  = par[1]
  beta   = par[2]
  lambda = par[3]
  (1/2)*lambda*pi*(beta*x)^alpha*alpha*exp(-(beta*x)^alpha)*cos((1/2)*pi*(1-exp(-(beta*x)^alpha)))*exp(-lambda*sin((1/2)*pi*(1-exp(-(beta*x)^alpha))))/(x*(1-exp(-lambda)))
  #(1/2)*lambda*pi*(beta*x)^alpha*alpha*exp(-(beta*x)^alpha)*alpha*cos((1/2)*pi*(1-exp(-(beta*x)^alpha)))*exp(-lambda*sin((1/2)*pi*(1-exp(-(beta*x)^alpha))))/(x*(1-exp(-lambda)))
  #(1/2)*lambda*pi*exp((x-alpha)/beta)*exp(-exp((x-alpha)/beta))*cos((1/2)*pi*(1-exp(-exp((x-alpha)/beta))))*exp(-lambda*sin((1/2)*pi*(1-exp(-exp((x-alpha)/beta)))))/(beta*(1-exp(-lambda)))
}
# weibull Exp- Cumulative distribution function.
cdf_Wexp <- function(par,x){
  alpha  = par[1]
  beta   = par[2]
  lambda = par[3]
  (1-exp(-lambda*sin((1/2)*pi*(1-exp(-(beta*x)^alpha)))))/(1-exp(-lambda))
  #(1-exp(-lambda*sin((1/2)*pi*(1-exp(-(beta*x)^alpha)))))/(1-exp(-lambda))
  #(1-exp(-lambda*sin((1/2)*pi*(1-exp(-exp((x-alpha)/beta))))))/(1-exp(-lambda))
}
A1=goodness.fit(pdf=pdf_Wexp, cdf=cdf_Wexp,starts = c(.1, .1, .1), data = x,method="S",domain=c(0,Inf),mle=NULL)

#A1=goodness.fit(pdf=pdf_Wexp, cdf=cdf_Wexp,starts = c(1, 1, 1), data = x,
#method="PSO",domain=c(0,Inf),mle=NULL,lim_inf = c(0,0,0),
#lim_sup = c(2,2,2), S = 250, prop=0.1, N=50)

mle1=A1$mle
aic1=A1$AIC
Af1=A1$A
Wf1=A1$W
mle1;aic1;Af1;Wf1


#---------------------------------------------------------------------------------------


# Weibull - pdf distribution function.
pdf_W <- function(par,x){
  alpha      = par[1]
  lambda     = par[2]
  (alpha*lambda^(alpha)*x^(alpha-1))*exp(-(lambda*x)^alpha)
}
# Weibull- Cumulative distribution function.
cdf_W <- function(par,x){
  alpha      = par[1]
  lambda     = par[2]
  (1-exp(-(lambda*x)^alpha))
}
B=goodness.fit(pdf=pdf_W, cdf=cdf_W,
               starts = c(1, 1), data = x, method="S", domain=c(0,Inf),mle=NULL)
mle2=B$mle
aic2=B$AIC
Af2=B$A
Wf2=B$W
mle2;aic2;Af2;Wf2
#------------------------EXPONENTIAL EXPONENTIATED-------------------------------

require(pracma)
require(AdequacyModel)
# EE - pdf distribution function.
pdf_EE <- function(par,x){
  alpha      = par[1]
  lambda     = par[2]
  (alpha*lambda)*exp(-(lambda*x))*(1-(exp(-(lambda*x))))^(alpha-1)
}
# EE- Cumulative distribution function.
cdf_EE <- function(par,x){
  alpha      = par[1]
  lambda     = par[2]
  (1-exp(-(x*lambda)))^alpha
}
C=goodness.fit(pdf=pdf_EE, cdf=cdf_EE,
               starts =c(1,1), data = x,
               method="S", domain=c(0,Inf),mle=NULL)

mle3=C$mle
aic3=C$AIC
Af3=C$A
Wf3=C$W
mle3;aic3;Af3;Wf3
#---------------------------------------------------------

pdf_W=function(x,alpha,lambda){
  (alpha*lambda^(alpha)*x^(alpha-1))*exp(-(lambda*x)^alpha)
}

pdf_EE=function(x,alpha,lambda){
  (alpha*lambda)*exp(-(lambda*x))*(1-(exp(-(lambda*x))))^(alpha-1)
}

pdf_cosW=function(x,alpha,beta,lambda){
  (1/2)*lambda*pi*(beta*x)^alpha*alpha*exp(-(beta*x)^alpha)*cos((1/2)*pi*(1-exp(-(beta*x)^alpha)))*exp(-lambda*sin((1/2)*pi*(1-exp(-(beta*x)^alpha))))/(x*(1-exp(-lambda)))
  #(1/2)*lambda*pi*(beta*x)^alpha*alpha*exp(-(beta*x)^alpha)*alpha*cos((1/2)*pi*(1-exp(-(beta*x)^alpha)))*exp(-lambda*sin((1/2)*pi*(1-exp(-(beta*x)^alpha))))/(x*(1-exp(-lambda)))
  #(1/2)*lambda*pi*exp((x-alpha)/beta)*exp(-exp((x-alpha)/beta))*cos((1/2)*pi*(1-exp(-exp((x-alpha)/beta))))*exp(-lambda*sin((1/2)*pi*(1-exp(-exp((x-alpha)/beta)))))/(beta*(1-exp(-lambda)))
}

par(pty="s")
hist(x,probability=T, col="white",ylim=c(0,0.1),cex.lab=0.9,cex.axis=0.9,xlab="Time",main="(a)",ylab="Density")
curve(pdf_W(x, mle2[[1]], mle2[[2]]),lty = 3,lwd=2,col="blue",add=T)
curve(pdf_EE(x,mle3[[1]], mle3[[2]]),lty = 2,lwd=2,col="red",add=T)
curve(pdf_cosW(x, mle1[[1]], mle1[[2]],mle1[[3]]),cex.main=0.9,lty = 1,lwd=2,col="black",add=T)
legend('top', c(expression(
  list(Sin-WP),
  list(W),
  list(EE)
)),ncol=1,bty="n",col=c("black","blue","red"),lty=c(1,2,3),cex =0.7,lwd=2)

aic1;aic2;aic3
Af1;Af2;Af3
Wf1;Wf2;Wf3
mle1;mle2;mle3