library(ReadPDF)

doc = xmlParsePDFTOTHML("dayrpt.xml")
showPage(doc) # Fix Colors.
#renderPage(doc[[1]], showText = FALSE)

lbb = getBBox(getNodeSet(doc, "//line"))
rbb = getBBox(getNodeSet(doc, "//rect"))

hlines = lbb[ lbb[, "y0"] == lbb[, "y1"],]
vlines = lbb[ lbb[, "x0"] == lbb[, "x1"],]

txt.nodes = getNodeSet(doc, "//text")
tbb = getBBox2(txt.nodes)


dims = as.numeric(xmlAttrs(doc[[1]])[c("height", "width")])
h = dims[1]
# Get the rectangles that span at least 75% of the page
hrects = rbb[ ((rbb[, "x1"] - rbb[, "x0"])/dims[2]) > .75, ]
# Now turn these rectangles into two lines - top and bottom
tmp = lapply(1:nrow(hrects), function(i) {
                                 m = matrix( rep(hrects[i, ], 2), , 4, byrow = TRUE)
                                 m[1, 4] = m[1, 2]
                                 m[2, 2] = m[2, 4]
                                 m
                              })
hlines = rbind(hlines, do.call(rbind, tmp))
apply(hlines, 1, function(x) lines(x[c(1,3)], h-x[c(2, 4)],  col = "red", lwd = 4))


tbb = as.data.frame(tbb)
tbb$text = sapply(txt.nodes, xmlValue)

els = rev(split(txt.nodes, cut(h - (tbb[, "top"] + tbb[, "height"]), c(0, sort(unique(h - hlines[, "y0"])), Inf))))

els = els[sapply(els, length) > 0]

lapply(els, function(row) split(els


# display the rectangles 1 by 1
h = dims[1]
#apply(rbb, 1, function(r) {rect(r[1], h-r[2], r[3], h-r[4], border = "orange", lwd = 4) ; scan("", n = 1)})

# For identifying the headers of each table, we might use the shading of the rectangles.

fill.cols = unique(as.character(xpathSApply(doc, "//@fill.color")))
text(c(50, 100, 200), rep(1200, length(fill.cols)), LETTERS[seq(along = fill.cols)], col =  sapply(fill.cols, mkColor))
