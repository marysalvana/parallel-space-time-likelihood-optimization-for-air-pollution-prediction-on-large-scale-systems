
source("./load_packages.R")
source("./auxiliary_functions.R")

YEAR = 2016
area = 'SAUDI'

if(area == 'US'){
  load(paste("../data/pm_US_", YEAR, '.Rdata', sep = ""))
}else if(area == 'SAUDI'){
  load(paste("../data/pm_", YEAR, '.Rdata', sep = ""))
}

dat <- data_matrix[["log_measurements"]]
locs <- data_matrix[["locations"]]

start_hr <- 1
subset_ind <- seq(start_hr, start_hr + 23, by = 1)

zlim_range1 <- range(dat[subset_ind,])

hr_count <- 0

for(hr in subset_ind){
	
	hr_count <- hr_count + 1

	if(area == 'US'){
	  jpeg(file = paste('../figures/pm-US-t', hr_count, '.jpg', sep = ''), width = 500, height = 500)
	}else if(area == 'SAUDI'){
	  jpeg(file = paste('../figures/pm-t', hr_count, '.jpg', sep = ''), width = 500, height = 500)
	}
	
	split.screen( rbind(c(0.05,0.95,0.1,0.95), c(0.90,0.99,0.1,0.95)))

	screen(1)

	par(pty = 's')
	par(mai=c(0.5, 0.5, 0.5, 0.5))
	
	quilt.plot(locs[, 1], locs[, 2], dat[, hr], zlim = zlim_range1, nx = 25, ny = 25, ylab = '', xlab = '', cex.lab = 4, add.legend = F, cex.axis = 2)
	
	if(area == 'US'){
	  map("state", xlim =  c(-120, -70), ylim = c(30, 50), lwd = 0.75, add = T)
	}else if(area == 'SAUDI'){
	  map("worldHires", xlim = c(26.719, 85.078), ylim = c(5.625, 42.188), lwd = 0.75, add = T)
	}
	
	mtext('Latitude', side = 2, line = 3.3, adj = 0.5, cex = 2.5, font = 2)
	mtext(paste(hr - 1, ':00', sep = ''), side = 3, line = 1, adj = 0.5, cex = 3, font = 2)
	mtext('Longitude', side = 1, line = 4, adj = 0.5,  cex = 2.5, font = 2)

	screen(2)

	x1 <- c(0.01,0.1,0.1,0.01)
	y1 <- c(0.2,0.2,0.8,0.8)
	legend.gradient2(cbind(x1,y1), title = "", limits = round(seq(round(zlim_range1[1], 0), round(zlim_range1[2], 0), length.out = 5), 1), CEX = 1.5)
	
	close.screen( all=TRUE)
	dev.off()

}

