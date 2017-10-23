
###Packages###
packs = c("dplyr","Quandl","lubridate")
lapply(packs,require,character.only=TRUE)

###Data###
dat <- Quandl("BCHARTS/BITSTAMPUSD")

dat <- dat %>% mutate(year = strftime(Date,"%Y"),mon =  strftime(Date,"%m"), 
				week = isoweek(Date)) %>%
				group_by(year,mon,week) %>% slice(which.min(Date)) %>% data.frame() %>%
				filter(Date >= as.Date("2014-01-01"))
			
dat <- dat[!( (dat$mon == "01" & dat$week%in%c("52","53")) | (dat$mon == "12" & dat$week%in%c("1","2")) ),]
				

###Calculation Function###
compare <- function(series,invest) {

		increment = invest / series

		result = list()

		for (i in 1:(nrow(dat)-series)) { 
			d = dat[i:(i+series-1),]
			inv = (invest / d$Close[1]) * d$Close[nrow(d)] 
			incrm = sum(increment / d$Close) * d$Close[nrow(d)] 
			result[[i]] = data.frame(inv, incrm, t0 = d$Date[1], t1 = d$Date[nrow(d)])
			}
		
		rslt <- do.call(rbind,result) %>% mutate(res = ifelse(inv > incrm,"invest","increment"),
					mar = inv - incrm,
					max_r = max(unlist(.[,1:2])),
					int_inv = cut(inv,seq(0,max_r[1]*1.2,invest/2),dig.lab=5),
					int_incrm = cut(incrm,seq(0,max_r[1]*1.2,invest/2),dig.lab=5)) %>%
					group_by(res) %>% arrange(mar) %>% data.frame() 
				

		rslt2 <- cbind.data.frame(inv=table(rslt$int_inv),incr=table(rslt$int_incrm))

		col_invest = "darkslategray4"

		col_increment = "gray30"

		pos=barplot(t(rslt2[,c(2,4)]),beside=TRUE,las=2,col=c(col_invest,col_increment))
		axis(1,colSums(pos)/2,rslt2[,1],las=2,cex.axis=0.7)
		legend("topright",fill=c(col_increment,col_invest),c("dollar cost averaging","lump sum"),bty="n")
		A <- recordPlot()
		
		with(rslt, barplot(abs(mar),col=ifelse(res=="increment",col_increment,col_invest),border=NA,las=2))
		legend("topleft",fill=c(col_increment,col_invest),c("dollar cost averaging","lump sum"),bty="n")
		B <- recordPlot()
		
	return(list(A,B,rslt,rslt2))

}

###Result###			
series = 52 #Consecutive Weeks to be considered

invest = 5000 #Initial Investment, DCA rate will be considered as a weekly invest: invest/series

compare(series,invest)




