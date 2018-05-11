if(FALSE) {
doc = readPDFXML("Electric_A2A _DPL_08-19-13.xml")
page = doc[[2]]
#rr = getNodeSet(page, ".//rect")

getHLines(page)
}

getHLines = 
function(page, nodes = getNodeSet(page, ".//rect"), mar = margins(page),# page is global here!
         bb = getBBox(nodes, asDataFrame = TRUE), 
         threshold = 5, lhThreshold = 6, minLineLength = 20)
{         
  # horizontal lines
    w = abs(bb$y1 - bb$y0) < lhThreshold # & (bb$x1 - bb$x0) > minLineLength
    hbb = bb[w,]
    
    uvals = unique(hbb$y1)
    g = split(hbb, cut(hbb$y1, c(0, uvals)))

#    sapply(g, nrow)

    wd = sapply(g, function(x) range(x$x0, x$x1))

    w2 = wd[1,] < (mar[1] + 10) &  wd[2,] > ( mar[2] -  10)

    g = g[w2]
    do.call(rbind, lapply(g, joinLines))
}


joinLines = 
function(xx, horiz = TRUE, maxGap = 5, vars = if(horiz) c("x0", "x1") else c("y0", "y1"))
{
    d = xx[[vars[1]]][-1] - xx[[vars[2]]] [-nrow(xx)]
  
    w = c(0, cumsum(d >= maxGap))
    yy = split(xx, w)
    
    do.call(rbind, lapply(yy, function(x) {
                         cbind(x[which.min(x[[vars[1]]]), c("x0", "y0")], x[which.max(x[[vars[2]]]), c("x1", "y1")])
                   }))
}




#any(diff(uvals) < threshold)
