# This reads a simple R scatterplot.

if(FALSE) {
  p = readPDFXML("scatterplot.xml")[[1]]
  x = getPoints(p, asBBox = TRUE)
  
  pp = x$left + x$width/2
  mp = getMap(p)
  round(linearMap(pp, mp), 1)

  ppy = x$top + x$width/2
  mpy = getMap(p, FALSE)
  round(linearMap(ppy, mpy), 4)  
}    

getPlotRegion =
    #
    #  get the rectangle corresponding to the plot region 
    #
function(p)
{
  getBBox(getNodeSet(p, ".//rect"))
}


getHTicks =
function(p, region = getPlotRegion(p), pageHeight = dim(p)[2])        
{
    lines = getNodeSet(p, ".//line")
    bbl = getBBox(lines)

    bt = region[, "y0"] # bottom of the plot region    
    
    w = (bbl[, "y0"] == region[1, "y0"] & bbl[, "x0"] == bbl[, "x1"])  # which vertical tick marks meet the plot region
    pos = bbl[w,]
    txt = getNodeSet(p, ".//text")
    bb2 = getBBox2(txt, asDataFrame = TRUE)
    w = (region[1, "y0"] < bb2$top)

    # Below the bottom of the plot region.
    # Includes the x axis label. So let's get rid of that.
    # Different assumptions
    #  Get the first line of text below the plot region
    ll = nodesByLine(txt[w])[[1]]
    x = getBBox2(ll, asDataFrame = TRUE)
    data.frame(raw = pos[, "x0"], plot = as.numeric(x$text)) # or raw  (x$left + x$width)/2
}


getVTicks =
function(p, region = getPlotRegion(p), pageHeight = dim(p)[2])        
{
    lines = getNodeSet(p, ".//line")
    bbl = getBBox(lines)

    bt = region[, "x0"] # left of the plot region    
    
    w = (bbl[, "x0"] == bt & bbl[, "x0"] != bbl[, "x1"] & bbl[, "y0"] == bbl[, "y1"])  # which horizontal tick marks meet the plot region

    pos = bbl[w,]
    
    txt = getNodeSet(p, ".//text")
    bb2 = getBBox2(txt, asDataFrame = TRUE)
    w = bb2$left < region[1, "x0"] 

#    ll = nodesByLine(txt[w])[[1]]
#    x = getBBox2(ll, asDataFrame = TRUE)
    x = split(bb2[w,], cut(bb[w, "left"], 2))[[2]]
    data.frame(raw = pos[, "y0"], plot = as.numeric(x$text)) 
}

getMap =
function(p, horizontal = TRUE, region = getPlotRegion(p))
{
    h = (if(horizontal) getHTicks else getVTicks)(p)
    
    # get the two end points and compute the number of units per PDF unit
    e = h[c(1, nrow(h)),]
    cols = if(horizontal) c(1, 3) else c(2, 4)
    r = region[1, cols]    
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

#####################################

getAxes =
function(p, region = getPlotRegion(p), pageHeight = dim(p)[2])
{
   
}

getTitle =
function(p)
{

}

getAxisLabels =
function(p)
{

}

getDataLines = function(p) {}
