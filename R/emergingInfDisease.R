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
