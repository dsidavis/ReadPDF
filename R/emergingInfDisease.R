isEID = isEmergingInfectDisease =
function(doc)
{
    doc = as(doc, "PDFToXMLDoc")

    foot = getPageFooter(doc[[1]])
    length(foot) && nchar(foot) && grepl("Emerging Infectious Diseases", foot)
}



getEIDAuthors =
function(doc, title = getDocTitle(doc, asNode = TRUE), colPos = getColPositions(doc[[1]]))
{
    getEIDHeadMaterialByFont(doc, '1', asNodes = TRUE, colPos = colPos)
}


findEIDAbstract =
function(doc, asNodes = TRUE, byLine = TRUE, colPos = getColPositions(doc[[1]]))
{
    getEIDHeadMaterialByFont (doc, '3', asNodes, colPos = colPos)
}

getEIDHeadMaterialByFont =
function(doc, font, asNodes = TRUE, byLine = TRUE, colPos = getColPositions(doc[[1]]))
{

    doc = as(doc, "PDFToXMLDoc")
    p1 = doc[[1]]
    lines = getBBox(p1)
    lines = lines[ lines[, "y0"] == lines[, "y1"] & lines[, "x1"] < colPos[2], ]
    tlines = lines[lines[,"y0"] == min(lines[, "y0"]),]
    ## We are going to assume this is @font = 1 for now.
    ## If this turns out to be a false assumption in all EID papers, we can
    ## get the font information  and find the text above the line in the first column
    ## and below the title that have a bold font.
    xpath.query = sprintf(".//text[ @top < %f and (@left + @width ) < %f and @font = '%s']", tlines["y0"], colPos[2], font )
    txt = getNodeSet(p1, xpath.query)
#    fontInfo = getFontInfo(p1)
#    fonts = sapply(txt, xmlGetAttr, "font")
#    browser()

    
#   doc = as(doc, "PDFToXMLDoc")
#   p1 = doc[[1]]
#   lines = getBBox(p1)
#   lines = lines[ lines[, "y0"] == lines[, "y1"] & lines[, "x1"] < colPos[2], ]
#   tlines = lines[lines[,"y0"] == min(lines[, "y0"]),]
#   xpath.query = sprintf(".//text[ @top < %f and (@left + @width ) < %f and @font = '3']", tlines["y0"], colPos[2] )
#   txt = getNodeSet(p1, xpath.query)
    if(asNodes)
       txt
    else
       paste(sapply(txt, xmlValue), collapse = " ")
}


findEIDAbstract =
function(doc, asNodes = TRUE, byLine = TRUE, colPos = getColPositions(doc[[1]]))
{
    doc = as(doc, "PDFToXMLDoc")
    p1 = doc[[1]]
    lines = getBBox(p1, asDataFrame = TRUE)

    if(nrow(lines) == 0 || max(lines$x1 - lines$x0) < dim(p1)["width"]*.5) {
        ## XXX So we use a different strategy when we implement it.
        warning("this is not a regular EID paper. No horizontal line at top")
        return(list())
    }
    

    if(length(colPos) == 0)
        stop("getColPosition() failed for this page")

    if(length(colPos) == 3)
        stop("This is a very different EID paper")
    
    if(length(colPos) == 1) 
       colPos = c(margins(p1)[1], colPos)

   
    lines = lines[ lines[, "y0"] == lines[, "y1"] & lines[, "x1"] < colPos[2], ]
    tlines = lines[lines[,"y0"] == min(lines[, "y0"]),]
    xpath.query = sprintf(".//text[ @top < %f and (@left + @width ) < %f]", tlines["y0"], colPos[2])
    txt = getNodeSet(p1, xpath.query)


    ll = orderByLine(txt)
    ll.bb = lapply(ll, getBBox2)
    tops = sapply(ll.bb, function(x) median(x[, "top"]))

    delta = diff(tops)
    threshold = getDocFont(doc)[1, "size"]*2
    blocks = split(ll, cumsum(c(0, delta) > threshold))
    return(blocks[[length(blocks)]])
browser()        
    
    ## Now we figure out which of the text is the title, the author names and the text of the abstract.
    ## The title may not be considered bold, as it may be Arial-Black which looks bold, but bold is not in the name.
    ## We also know that the abstract will be the smallest text in this group.
    fontInfo = getFontInfo(doc)
     ## Font id of 
    fi = sapply(txt, xmlGetAttr, "font")
    f2 = fontInfo[unique(fi),]
    ## This won't quite work as we will pick up the footnote markers which will be smaller.
    ## We may need to find the first the line that has non bold text
    ## It is possible the abstract text will have a bold item in it so we can't take the first bold.
    ##
    ## If the size of the fonts for the authors and the abstract are the same, we may select the wrong one!
    ##
    ## So we probably need to arrange by line and then find where there is more than interline space as we go back up.
#browser()
    fids = f2$id[ order(f2$size, decreasing = TRUE)] [-(1:2)]
    txt = txt[ fi %in% fids]

    if(asNodes)
       txt
    else
       paste(sapply(txt, xmlValue), collapse = " ")   
    
##    b = isBold(fontInfo)
##    f2 = fontInfo[!b,]
    
}
