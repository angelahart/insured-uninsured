rm(list=ls())
gc()

library("maptools")
library("maps")
library("RColorBrewer")


unins <- read.csv("remaining uninsured_enhanced.csv")
unins <- reshape(unins,timevar="year",idvar="region",direction="wide")
rownames(unins) <- unins$region


shapes <- readShapePoly("ca/counties.shp")


counties <- read.csv("counties.csv",header=FALSE)

counties<-do.call(rbind,apply(counties,1,function(x){
	if(x[1]!=""){
		county <- unlist(strsplit(as.character(x[2]),", ",fixed=TRUE))
		if(length(grep("County",county))==0) county <- paste(county,"County")
		
		county <- gsub("  "," ", county,fixed=TRUE)
		
		cbind("MacroArea"=as.character(x[1]),"County"=county)
		
		}
	}))
	
uninsLong <- cbind(unins[counties[,"MacroArea"],],County=counties[,"County"])


plotMap <- function(X,colors="Blues",breaks=NULL,border="black"){
	
	if(is.null(breaks)) breaks <- c(-Inf,unique(X) + c(0,diff(unique(X))/2))
	
	colors <- brewer.pal(n=length(breaks),name=colors)
	
	matchOrder <- match(as.character(shapes@data$COUNTY), as.character(uninsLong$County))
	
	X <- X[matchOrder]
	
	
	buckets <- cut(X,breaks=breaks)
	numeric_buckets <- as.numeric(buckets)
	
	#plot
	plot(shapes, col=colors[numeric_buckets],border=border)
	
	
	}

plotMap(uninsLong[,"percent.immigration.status.2014"],border="orange")



plotMap(uninsLong[,"percent.exchange.with.subsidies.2014"]-uninsLong[,"percent.exchange.with.subsidies.2019"])



col<-brewer.pal(n=7,name="Set3")[-1]
plot(0,0,xlim=c(1,2),ylim=c(17.3,29),axes=FALSE,xlab="year",ylab="% exchange with subsidies")
axis(1,at=c(1,2),labels=c("2014","2019"))
axis(2,at=17:29)
lapply(1:nrow(unins),function(i)lines(1:2,unins[i,c("percent.exchange.with.subsidies.2014","percent.exchange.with.subsidies.2019")],col=col[i]))
legend("topright",col=col,lty=1,legend=unins[,"region"])
box() # this is optional