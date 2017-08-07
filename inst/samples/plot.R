pdf("plot.pdf")
plot(1:10, col = "red")
abline(a = 0, b = 1)
dev.off()



pdf("matplot.pdf")
d = data.frame(x = rnorm(10, 20, 4), y = rnorm(10, 15, sd = 2))
matplot(d, type = "b", col = c("red", "blue"), xlab = "Time", ylab = "Value")
dev.off()



# With no points plotted, the lines are connected so have a different form in the PDF and XML.
pdf("matplot2.pdf")
d = data.frame(x = rnorm(10, 20, 4), y = rnorm(10, 15, sd = 2))
matplot(d, type = "l", col = c("red", "blue"), xlab = "Time", ylab = "Value")
dev.off()


