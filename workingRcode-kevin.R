setwd("~/dataviz-fall-2013/insured-uninsured")

library(stringr)
library("RColorBrewer")


unins <- read.csv("data/remaining uninsured_enhanced.csv", stringsAsFactors=F)

unins$region <- as.character(unins$region)

#this removes empty white space
unins$region <- str_trim(unins$region)

#reshape my data to make slope graphing easier later
unins <- reshape(unins,timevar="year",idvar="region",direction="wide")

#rename my rows
rownames(unins) <- unins$region

# abbreviate region name
unins$region[unins$region=="Northern California and Sierra Counties"] <- "NorCal & Sierra"                  

####This is where the slopegraph code begins###



# slopeGraph <- function(unins,variable,color="Spectral",alpha=1){
	
	# columns <- paste(variable,c(2014,2019),sep=".")
	
	# if(is.null(ylim)) ylim<-range(unins[,columns])
	
	# col <- brewer.pal(n=nrow(unins),name=color)
	# col <- adjustcolor(col,alpha=alpha)
	# axis(1,at=c(1,2),labels=c("2014","2019"))
	# axis(2,at= seq(min(unins[,columns]),max(unins[,columns]),length.out=nrow(unins)))
	# lapply(1:nrow(unins),function(i){
	# 	lines(1:2,unins[i,columns],col=col[i],lwd=5)
	# 	})
# }


my_y_limit <- c(10,40)

plot_slope_graph <- function(col_name) {

	#the field I want
	field_name <- col_name

	#the column names I want.
	columns_I_want <- c("region", paste(field_name,c(2014,2019),sep=".") )

	# data at these columns
	df <- unins[,columns_I_want]


	#start a new plot
	plot(0,0,xlim=c(2014,2019),ylim=my_y_limit,ylab = "percent uninsured",xlab="year",type="n", main=field_name)

	#vector of 7 colors
	cols <- rainbow(7)
	for (i in 1:nrow(df)) {
		v_2014 <- df[i,2]
		v_2019 <- df[i,3]
		# segments(2014, v_2014, 2019, v_2019)
		lines(c(2014,2019), c(v_2014, v_2019), col=cols[i])
	}

}


all_fields <- c("percent.immigration.status", "percent.Medi.Cal", "percent.exchange.with.subsidies", "percent.exchange.without.subsidies")


par(mfrow=c(1,4))

for (i in all_fields) {
	plot_slope_graph(i)
}





	
# slopeGraph(unins,variable="immigration.status",alpha=1,where=c(1,280000))
# slopeGraph(unins,variable="percent.immigration.status",alpha=1,where="topleft",ylim=c(12,34))

# slopeGraph(unins,variable="percent.Medi.Cal",alpha=1,where=c(15,35.5))
# slopeGraph(unins,variable="Medi.Cal",alpha=1,where=c(15,350000.5))