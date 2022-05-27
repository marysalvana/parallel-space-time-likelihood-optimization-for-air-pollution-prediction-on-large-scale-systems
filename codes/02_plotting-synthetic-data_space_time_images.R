
source("./load_packages.R")
source("./auxiliary_functions.R")

type = 'nonseparable'

if(type == 'separable'){
  dat <- read.table(paste('../data/Z_separable.txt', sep = ''), header = FALSE, sep = " ") %>% as.matrix()
  locs <- read.table(paste('../data/LOC_separable.txt', sep = ''), header = FALSE, sep = ",") %>% as.matrix()
}else if(type == 'nonseparable'){
  dat <- read.table(paste('../data/Z_nonseparable.txt', sep = ''), header = FALSE, sep = " ") %>% as.matrix()
  locs <- read.table(paste('../data/LOC_nonseparable.txt', sep = ''), header = FALSE, sep = ",") %>% as.matrix()
}

start_hr <- 1
subset_ind <- seq(start_hr, start_hr + 4, by = 1)

zlim_range1 <- c(-3, 3)

hr_count <- 0

for(hr in subset_ind){
	
	hr_count <- hr_count + 1

	if(type == 'separable'){
	  jpeg(file = paste('../figures/synthetic-data-sep-t', hr_count, '.jpg', sep = ''), width = 500, height = 500)
	}else if(type == 'nonseparable'){
	  jpeg(file = paste('../figures/synthetic-data-nonsep-t', hr_count, '.jpg', sep = ''), width = 500, height = 500)
	}
	
	split.screen( rbind(c(0.05,0.95,0.1,0.95), c(0.90,0.99,0.1,0.95)))

	screen(1)

	par(pty = 's')
	par(mai=c(0.5, 0.5, 0.5, 0.5))
	
	quilt.plot(locs[(hr - 1) * 10000 + 1:10000, 1], locs[(hr - 1) * 10000 + 1:10000, 2], dat[(hr - 1) * 10000 + 1:10000, ], zlim = zlim_range1, nx = 60, ny = 60, ylab = '', xlab = '', cex.lab = 4, add.legend = F, cex.axis = 2)
	
	mtext(expression(s[y]), side = 2, line = 3, adj = 0.5, cex = 2.5, font = 2)
	mtext(paste('t = ', hr, sep = ''), side = 3, line = 1, adj = 0.5, cex = 3, font = 2)
	mtext(expression(s[x]), side = 1, line = 4, adj = 0.5,  cex = 2.5, font = 2)

	screen(2)

	x1 <- c(0.01,0.1,0.1,0.01)
	y1 <- c(0.2,0.2,0.8,0.8)
	legend.gradient2(cbind(x1,y1), title = "", limits = seq(-3, 3, length.out = 5), CEX = 1.5)
	
	close.screen( all=TRUE)
	dev.off()

}

