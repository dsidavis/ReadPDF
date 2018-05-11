if(FALSE) {
doc = readPDFXML("Electric_A2A _DPL_08-19-13.xml")
page = doc[[2]]
#rr = getNodeSet(page, ".//rect")

hor = getHLines(page)
plot(page)
abline(h = dim(page)[2] - hor$y0, col = "red")

vert = getHLines(page, horiz = FALSE)
abline(v = unique(vert$x0), col = "red")
}


getLines = getHLines = 
function(page, nodes = getNodeSet(page, ".//rect"), mar = margins(page),# page is global here!
         bb = getBBox(nodes, asDataFrame = TRUE), 
         threshold = 5, lhThreshold = 6, minLineLength = 20, marThreshold = 10, horiz = TRUE)
{         
     # horizontal lines
    w = if(horiz)
           abs(bb$y1 - bb$y0) < lhThreshold 
        else
           abs(bb$x1 - bb$x0) < lhThreshold
    
    hbb = bb[w,]

    var = if(horiz) "y1" else "x1"
    
    uvals = unique(hbb[[var]])
    g = split(hbb, cut(hbb[[var]], c(0, uvals)))

    if(horiz) {
        wd = sapply(g, function(x) range(x$x0, x$x1))
        w2 = wd[1,] < (mar[1] + marThreshold) &  wd[2,] > ( mar[2] -  marThreshold)
    } else {
        ht = sapply(g, function(x) diff(range(x$y0, x$y1)))
        w2 = ht > dim(page)[2]*.1
    }

    g = g[w2]
    do.call(rbind, lapply(g, joinLines, horiz = horiz))
}


joinLines = 
function(xx, horiz = TRUE, maxGap = 5, vars = if(horiz) c("x0", "x1") else c("y0", "y1"))
{
    xx = xx[ order(xx[[ vars[2] ]]), ]
    d = xx[[vars[1]]][-1] - xx[[vars[2]]] [-nrow(xx)]
  
    w = c(0, cumsum(d >= maxGap))
    yy = split(xx, w)
    
    do.call(rbind, lapply(yy, function(x) {
                         cbind(x[which.min(x[[vars[1]]]), c("x0", "y0")], x[which.max(x[[vars[2]]]), c("x1", "y1")])
                   }))
}




#any(diff(uvals) < threshold)
