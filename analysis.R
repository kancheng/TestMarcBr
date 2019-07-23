# 套件匯入更新與判斷
# install.packages("broom")
# install.packages("mFilter")

use.package = function(){
	# install package name
	pkgs = c("broom", "mFilter")
	key = pkgs[!( pkgs %in% installed.packages()[,"Package"] )]
	if(length(key)) {
		install.packages(pkgs)
	}
	library("broom")
	library("mFilter")
}

use.package()

# Test Data

tde = c(
	99066.1,	# 2000
	109276.2,	# 2001
	120480.4,	# 2002
	136576.3,	# 2003
	161415.4,	# 2004
	185998.9,	# 2005
	219028.5,	# 2006
	270704,		# 2007
	321229.5,	# 2008
	347934.9,	# 2009
	410354.1,	# 2010
	483392.8,	# 2011
	537329,		# 2012
	588141.2,	# 2013
	642097.6,	# 2014
	683390.5,	# 2015
	737074,		# 2016
	820099.5,	# 2017
	896915.6	# 2018
)
tdo = c(
	109276.2,	# 2001
	120480.4,	# 2002
	136576.3,	# 2003
	161415.4,	# 2004
	185998.9,	# 2005
	219028.5,	# 2006
	270704,		# 2007
	321229.5,	# 2008
	347934.9,	# 2009
	410354.1,	# 2010
	483392.8,	# 2011
	537329,		# 2012
	588141.2,	# 2013
	642097.6,	# 2014
	683390.5,	# 2015
	737074,		# 2016
	820099.5,	# 2017
	896915.6	# 2018
)

ts.tdo = ts(tdo, frequency = 1, start = c(2001, 1) )
ts.tde = ts(tde, frequency = 1, start = c(2000, 1) )

# 敘述統計 -> minimum , q1, median, mean, q3, maximum, na, sd, var, range.

sumy = function(su.data){
	tem.su.all = data.frame(
		minimum = numeric(), q1 = numeric(), median = numeric(), 
		mean = numeric(), q3 = numeric(), maximum = numeric(), 
		na = numeric(), sd = numeric(), var = numeric(), range = numeric()
	)
	tem.sumy = summary( su.data, na.rm = TRUE)
	assign( "sd", sd(su.data, na.rm = TRUE))
	assign( "var", var(su.data, na.rm = TRUE))
	assign( "range", range( su.data, na.rm = TRUE)[2]-range( su.data, na.rm = TRUE)[1])
	assign( "na", c(0))
	tem.su.df = tidy(tem.sumy)
	if ( length(grep("na",names(tem.su.df)))>0){
		tem.su.df = cbind( tem.su.df, sd, var, range)
		tem.su.all = tem.su.df
	} else {
		tem.su.df = cbind( tem.su.df, na, sd, var, range)
		tem.su.all = tem.su.df
	}
	return(tem.su.all)
}

# EX
# sumy(ts.tde)

# 5 種濾波法處理與 TR 法判斷

filter.fiv = function(data, obj.env = TRUE){
	if (is.ts(data)){
		if( length(data)%% 2 == 0 ){
			tem.fiv.su.vna.even = c( "hp","bk","cf","bw","tr")
			tem.fiv.su.vna.fne = c( "HP","BK","CF","BW","TR")
			for(i in c(1:5)){
				tem.na = "tem.ts"
				tem.ts.filter = mFilter( data, filter = tem.fiv.su.vna.fne[i]) 
				tem.na.all = paste0(tem.na,".", tem.fiv.su.vna.even[i])
				if (obj.env == TRUE){
					assign( tem.na.all , tem.ts.filter , env = .GlobalEnv)
				} else if (obj.env == FALSE){
					assign( tem.na.all , tem.ts.filter)
				}
			}
			if (obj.env == FALSE){
				tem.list = list(filter.hp = tem.ts.hp,
					filter.bk = tem.ts.bk,
					filter.cf = tem.ts.cf,
					filter.bw = tem.ts.bw,
					filter.tr = tem.ts.tr)
			}
		} else {
			tem.fiv.su.vna.odd = c( "hp","bk","cf","bw")
			tem.fiv.su.vna.fno = c( "HP","BK","CF","BW")
			for(i in c(1:4)){
				tem.na = "tem.ts"
				tem.ts.filter = mFilter( data, filter=tem.fiv.su.vna.fno[i]) 
				tem.na.all = paste0(tem.na,".", tem.fiv.su.vna.odd[i])
				if (obj.env == TRUE){
					assign( tem.na.all , tem.ts.filter , env = .GlobalEnv)
				} else if (obj.env == FALSE){
					assign( tem.na.all , tem.ts.filter)
				}
			}
			if (obj.env == FALSE){
				tem.list = list(filter.hp = tem.ts.hp,
					filter.bk = tem.ts.bk,
					filter.cf = tem.ts.cf,
					filter.bw = tem.ts.bw)
			}
		}
		if (obj.env == FALSE){
			return(tem.list)
		}
	} else {
		stop("Data Error.")
	}
}

# EX
# filter.fiv(ts.tdo)
# filter.fiv(ts.tde)
# filter.fiv(ts.tdo, FALSE)
# filter.fiv(ts.tde, TRUE)

# 濾波法敘述統計

filter.fiv.su = function(data, scientific.notation = FALSE){
	if (scientific.notation == TRUE){
		options(scipen = 999)
	} else if (scientific.notation == FALSE){
		options(scipen = 0)
	}
	filter.fiv(data, TRUE)
	tem = filter.fiv(data, FALSE)
	tem.list.na.va = names(tem)
	tem.df = data.frame( 
			Filter = character(), Cycle = character(),
			minimum = numeric(), q1 = numeric(), median = numeric(), 
			mean = numeric(), q3 = numeric(), maximum = numeric(), 
			na = numeric(), sd = numeric(), var = numeric(), range = numeric())
	for( i in length(tem.list.na.va):1){
		Filter = as.character(tem.list.na.va[i])
		Class = as.character(c("Cycle"))
		comds = paste0( "tem$", tem.list.na.va[i], "$cycle")
		evalp = eval(parse(text = comds))
		tem.su = cbind( Filter, Class, sumy(as.numeric(evalp)))
		tem.df = rbind(tem.su, tem.df)
	}
	for( i in length(tem.list.na.va):1){
		Filter = as.character(tem.list.na.va[i])
		Class = as.character(c("Trend"))
		comds = paste0( "tem$", tem.list.na.va[i], "$trend")
		evalp = eval(parse(text = comds))
		tem.su = cbind(Filter, Class, sumy(as.numeric(evalp)))
		tem.df = rbind(tem.su, tem.df)
	}
	return(tem.df)
	options(scipen = 0)
}

# EX
# filter.fiv.su(ts.tdo, TRUE)
# filter.fiv.su(ts.tdo)


# ARCH & GARCH -> fGarch
# ...

# Granger Causality test
# ...

# Augmented Dickey-Fuller Test
# ...

######

# 時間序列 PIC
# par(mfrow=c(1,1))
# plot(cn.gdp.1y.ts)


# 檔科學記號
# options(scipen=999)

# opar <- par(no.readonly=TRUE)
# par(mfrow=c(1,1),mar=c(3,3,2,1))
# plot(cn.gdp.1y.ts,main="1953 - 2018 國民總收入 - 億元",col=1,ylab="")
# lines(cn.gdp.1y.ts.hp$trend,col=2)
# lines(cn.gdp.1y.ts.bk$trend,col=3)
# lines(cn.gdp.1y.ts.cf$trend,col=4)
# lines(cn.gdp.1y.ts.bw$trend,col=5)
# lines(cn.gdp.1y.ts.tr$trend,col=6)
# legend("topleft",legend=c("series", "HP","BK","CF","BW", "TR"),col=1:6, lty=rep(1,6),ncol=2)

# 自動繪圖

filter.plot = function( data, main.name) {

}

# 中國經濟分析 ALL

cn.mac = function() {

}


