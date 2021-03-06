
colors=c("blue","yellow","red")
colsteps=100

legend.gradient2 = function(pnts, cols=tim.colors(64),limits=c(0,1), title='Legend', CEX = 1, ROUND = 2, ...){
  	pnts = try(as.matrix(pnts),silent=T)
  	if(!is.matrix(pnts)) stop("you must have a 4x2 matrix")
  	if(dim(pnts)[1]!=4 || dim (pnts)[2]!=2) stop ("Matrix must have dimensions of 4 rows and 2 columms")
  	if(length(cols)<2) stop("You must have 2 or more colors")
  	#break up the min and max into a number of values == length(cols)
  	yvals = seq(min(pnts[, 2]), max(pnts[, 2]), length=length(cols)+1)
  	#cycle through each of the yvals and create polygons
  	for (i in 1:length(cols)){  #create the polygon for that color
    		polygon(x=pnts[,1],y=c(yvals[i],yvals[i],yvals[i+1],yvals[i+1]),col=cols[i],border=F)
  	}
  	#add the text
	if(length(limits) == 5){
  		locationn <- seq(min(pnts[,2]),max(pnts[,2]),length.out = 5)
  		text(max(pnts[,1]),locationn[1],labels=round(limits[1],ROUND),pos=4, cex = CEX)
  		text(max(pnts[,1]),locationn[2],labels=round(limits[2],ROUND),pos=4, cex = CEX)
  		text(max(pnts[,1]),locationn[3],labels=round(limits[3],ROUND),pos=4, cex = CEX)
  		text(max(pnts[,1]),locationn[4],labels=round(limits[4],ROUND),pos=4, cex = CEX)
  		text(max(pnts[,1]),locationn[5],labels=round(limits[5],ROUND),pos=4, cex = CEX)
	}else if(length(limits) == 3){
  		locationn <- seq(min(pnts[,2]),max(pnts[,2]),length.out = 3)
  		text(max(pnts[,1]),locationn[1],labels=round(limits[1],ROUND),pos=4, cex = CEX)
  		text(max(pnts[,1]),locationn[2],labels=round(limits[2],ROUND),pos=4, cex = CEX)
  		text(max(pnts[,1]),locationn[3],labels=round(limits[3],ROUND),pos=4, cex = CEX)
	}
}
