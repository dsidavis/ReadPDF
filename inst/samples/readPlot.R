if(FALSE) {
  p = readPDFXML("scatterplot.xml")[[1]]
  x = getPoints(p, asBBox = TRUE)
  pp = x$left + x$width/2
  mp = getXMap(p)
  round(linearMap(pp, mp), 1)

  
}    

# This reads a simple R scatterplot.

p = readPDFXML("scatterplot.xml")[[1]]


getPlotRegion =
function(p)
{
  getBBox(getNodeSet(p, ".//rect"))
}

getAxes =
function(p, region = getPlotRegion(p), pageHeight = dim(p)[2])
{
   
}

getHTicks =
function(p, region = getPlotRegion(p), pageHeight = dim(p)[2])        
{
    bt = region[, "y0"] # bottom of the plot region

    lines = getNodeSet(p, ".//line")
    bbl = getBBox(lines)
    
    w = (bbl[, "y0"] == region[1, "y0"] & bbl[, "x0"] == bbl[, "x1"])  # which vertical tick marks meet the plot region
    pos = bbl[w,]
    txt = getNodeSet(p, ".//text")
    bb2 = getBBox2(txt, asDataFrame = TRUE)
    d = (region[1, "y0"] - bb2$top)

    w =  d < 0
    # Below the bottom of the plot region.
    # Includes the x axis label. So let's get rid of that.
    # Different assumptions
    #  Get the first line of text below the plot region
    ll = nodesByLine(txt[w])[[1]]
    x = getBBox2(ll, asDataFrame = TRUE)
    data.frame(raw = pos[, "x0"], plot = as.numeric(x$text)) # or raw  (x$left + x$width)/2
}


getXMap =
function(p, region = getPlotRegion(p))
{
    h = getHTicks(p)
    # get the two end points and compute the number of units per PDF unit
    e = h[c(1, nrow(h)),]
#    scale = diff(e[[2]])/diff(e[[1]])

    r = region[1, c("x0", "x1")]    
    m = linearMap(r, e)
    data.frame(raw = r, x = m)
}

linearMap =
function(v, info, scale = diff(info[[2]])/diff(info[[1]]))   
{
    (v - info[,1])*scale + info[,2]
}

getPoints = 
function(p, region = getPlotRegion(p), pageHeight = dim(p)[2], asBBox = FALSE)
{
    #XXX Deal with the lines, etc. inside the plot region.
    text = getNodeSet(p, ".//text")
    bb = getBBox2(text, asDataFrame = TRUE)

    h = bb$left >= region[1, "x0"] & bb$left + bb$width <= region[1, "x1"]
    v = bb$top <= region[1, "y0"] & bb$top + bb$height > region[1, "y1"]
    
    if(asBBox)
        bb[h & v,]
    else
       text[ h & v]
}
