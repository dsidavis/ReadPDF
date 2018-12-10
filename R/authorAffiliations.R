f = "./3759643942/Drexler-2010-Genomic characterization of sever.xml"

getAuthorAffil =
    ##
    ## Also removing Received/Accepted dates .....
    ##
function(doc)
{
    if(is.character(doc))
        doc = readPDFXML(doc)

    if(isEID(doc)) 
        return(list(author = getEIDAuthors(doc), affiliations = getBelowLine(doc)))


    ti = getDocTitle(doc, asNode = TRUE)
    if(length(ti) == 0 || is.character(ti)) {
        stop("No title")
        return(NULL)
    }
    ab = findAbstract(doc)
    
    if(length(ab) == 0 || is.character(ab))
        stop("no abstract")
#        return(NULL)

    getNodesBetween(ti[[length(ti)]], ab[[1]], exclude = TRUE)
                                        #sapply(xs, xmlValue)
}



getBelowLine =
    ## check that the font for the content below is different from the document font
    ## XXX If line in 2nd column and lower than one in column 1, we won't get anything. So deal with both columns.
    ###
function(doc, col = 1, colPos = getColPositions(p1))
{
    if(is.character(doc))
        doc = readPDFXML(doc)

    p1 = doc[[1]]
    
    lines = getBBox(p1)
     # Get horizontal lines only
    lines = lines[ lines[, "y0"] == lines[, "y1"], ]

    tlines = if(col == length(colPos))
               lines[lines[, "x0"] >= colPos[2] ,]
            else
                lines[lines[, "x0"] < colPos[2] ,]
    tlines = tlines[tlines[,"y0"] == max(tlines[, "y0"]) ,]
    if(is.matrix(tlines))
        tlines = tlines[1,]

    xpath.query = sprintf(".//text[ @top > %f and (@left + @width ) < %f]", tlines["y0"], colPos[2] )
    txt = getNodeSet(p1, xpath.query)
    ## Check these are all in a different font than the document's regular text.

    txt
}




authorsAfterTitle =
    ## This is  currently a very simple minded function
    ## to group the nodes by line and then group those lines
    ## by interline skip to get blocks.
    ## Then we can take the block after the title. 
function(doc, lineskip = 20, title = getDocTitle(doc))
{
    g = getPageGroups(doc, lineskip)
    combine = function(x) paste(names(x), collapse = "\n")
    g.txt = sapply(g, combine)
    i = match(title, g.txt)
    
    if(is.na(i))
        g
    else
        g[[i + 1L]]
}
