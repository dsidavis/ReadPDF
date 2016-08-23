plot.pdf: GNUmakefile
	echo 'pdf("plot.pdf", 10, 10);  plot(1:10, type = "n", axes = FALSE,xlab = "", ylab = ""); abline(v = 2:6, col = c("red", "blue", "green", "grey", "darkgrey"), lwd = 1:5, lty = 1:5) ; rect(7, 2, 9, 4, border = "red", col = "orange"); dev.off()' | R --no-save
