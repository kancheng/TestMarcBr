# R Test Code

# ARCH & GARCH -> fGarch
# ARCH 模型被廣泛用於金融數據分析的原因是可以集中反映變異數的變化，透過對於條件變異數的落後期數預測來最大程度上處理金融數據波動率的問題。
# 用 ARCH(p) 來代表擬合的落後期數。
# Packages fGarch
# Install
# install.packages("fGarch")

library("fGarch")

# garchFit
# Estimates the parameters of an univariate ARMA-GARCH/APARCH process.
# EX :
# garchFit(formula = ~ garch(1, 1), data = fGarch::dem2gbp, 
#	init.rec = c("mci", "uev"), 
#	delta = 2, skew = 1, shape = 4, 
#	cond.dist = c("norm", "snorm", "ged", "sged", "std", "sstd", 
#		"snig", "QMLE"), 
#	include.mean = TRUE, include.delta = NULL, include.skew = NULL, 
#	include.shape = NULL, leverage = NULL, trace = TRUE, 
#   
#	algorithm = c("nlminb", "lbfgsb", "nlminb+nm", "lbfgsb+nm"), 
#	hessian = c("ropt", "rcd"), control = list(), 
#	title = NULL, description = NULL, …)  
# garchKappa(cond.dist = c("norm", "ged", "std", "snorm", "sged", "sstd",
#	"snig"), gamma = 0, delta = 2, skew = NA, shape = NA)

N = 200
x.vec = as.vector(garchSim(garchSpec(rseed = 1985), n = N)[,1])
garchFit(~ garch(1,1), data = x.vec, trace = FALSE)

# An univariate timeSeries object with dummy dates:
x.timeSeries = dummyDailySeries(matrix(x.vec), units = "GARCH11")

garchFit(~ garch(1,1), data = x.timeSeries, trace = FALSE)

# GARCH 模型 (Generalized-ARCH Model)
# 模型處理均數的假設問題，假設均數是條件變異數的函數，等同於說均數是會受到波動影響的變化函數。
# EGARCH 指數模型 
# 針對資產報酬率在正負不同的時候，對財務波動產生的效果並非對稱的問題，而資產報酬率為負的時候對於未來波動的影響往往比正報酬率來得大。
# Packages rugarch
# Install
# install.packages("rugarch")

library("rugarch")

# EX 1.
# a standard specification
spec1 = ugarchspec()
spec1

# an example which keep the ar1 and ma1 coefficients fixed:
spec2 = ugarchspec(mean.model=list(armaOrder=c(2,2), 
fixed.pars=list(ar1=0.3,ma1=0.3)))
spec2

# an example of the EWMA Model
spec3 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)), 
		mean.model=list(armaOrder=c(0,0), include.mean=TRUE),  
		distribution.model="norm", fixed.pars=list(omega=0))

# EX 2.
# Data Name : sp500ret
# Data : Standard And Poors 500 Closing Value Log Return
# The SP500 index closing value log return from 1987-03-10 to 2009-01-30 from yahoo finance.

head(sp500ret)

# Granger Causality test
# Description
# grangertest is a generic function for performing a test for Granger causality.
# Usage
## Default S3 method:
# grangertest(x, y, order = 1, na.action = na.omit, ...)
## S3 method for class 'formula':
# grangertest(formula, data = list(), ...)

# EX :
## Which came first: the chicken or the egg?
data(ChickEgg)
grangertest(egg ~ chicken, order = 3, data = ChickEgg)
grangertest(chicken ~ egg, order = 3, data = ChickEgg)

## alternative ways of specifying the same test
grangertest(ChickEgg, order = 3)
grangertest(ChickEgg[, 1], ChickEgg[, 2], order = 3)



# Augmented Dickey-Fuller Test
# Packages tseries

# Install
# install.packages("tseries")

library("tseries")

# EX :
# adf.test(x, alternative = c("stationary", "explosive"), k = trunc((length(x)-1)^(1/3)))

x <- rnorm(1000)  # no unit-root

adf.test(x)

y <- diffinv(x)   # contains a unit-root

adf.test(y)


# VAR Modelling
# Estimation, lag selection, diagnostic testing, forecasting, causality analysis, 
# forecast error variance decomposition and impulse response functions of VAR models and estimation of SVAR and SVEC models.

# Install
# install.packages("vars")

library("vars")

# ARCH-LM tests
# EX :
# arch.test(x, lags.single = 16, lags.multi = 5, multivariate.only = TRUE)

data(Canada)
var.2c <- VAR(Canada, p = 2, type = "const")
arch.test(var.2c)

# Causality Analysis
# Computes the test statistics for Granger- and Instantaneous causality for a VAR(p).
# EX :
# causality(x, cause = NULL, vcov.=NULL, boot=FALSE, boot.runs=100)

data(Canada)
var.2c <- VAR(Canada, p = 2, type = "const")
causality(var.2c, cause = "e")

#use a robust HC variance-covariance matrix for the Granger test:
causality(var.2c, cause = "e", vcov.=vcovHC(var.2c))

#use a wild-bootstrap procedure to for the Granger test
causality(var.2c, cause = "e", boot=TRUE, boot.runs=1000)
