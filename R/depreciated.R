################################################################################
# abstract.R

#
#
#
#
#
#

margins =
    # XXX Deal with rotation when the text is actually rotated.
    # Specifying @rotation = 0 for case when there is a rotate line of text identifying how
    # the document was retrieved, e.g. downloaded by UC Davis.....
    #XXX should we extend this to get top and bottom.
    #XXX  and deal with a set of nodes, a page, and also an entire document.
    #XXX remove header and footer
function(page, bbox = getBBox2(getNodeSet(page, ".//text[@rotation = 0]")), ...)
    UseMethod("margins")

margins.character =
function(page, bbox = getBBox2(getNodeSet(page, ".//text[@rotation = 0]")), ...)        
{
    margins(readPDFXML(page), ...)
}

margins.PDFToXMLDoc =
function(page, bbox = getBBox2(getNodeSet(page, ".//text[@rotation = 0]")), asDataFrame = TRUE, ...)        
{
    ans = lapply(getPages(page), margins)
    if(asDataFrame)
        as.data.frame(do.call(rbind, ans))
    else
        ans
}

margins.XMLInternalNode = margins.PDFToXMLPage =
function(page, bbox = getBBox2(), ...)
{
    margins(getNodeSet(page, ".//text[@rotation = 0]"))
}

margins.list = margins.XMLNodeSet =
function(page, bbox = getBBox2(unlist(page)), ...)
{    
   c(left = min(bbox[, 1]), right = max(bbox[,1] + bbox[,3]), top = min(bbox[,2]), bottom = max(bbox[,4]))
}

findAbstract =
function(doc, asNodes = TRUE, page = doc[[1 + hasCoverPage(doc) ]], byLine = TRUE)
{
    if(is.character(doc))
        doc = readPDFXML(doc)

    if(isEID(doc))
        return(findEIDAbstract(doc, asNodes, byLine))
    
    if(isBioOne(doc) && missing(page))
          # skip the first page and start at the second page
        page = doc[[2]]
    
    if(is.numeric(page))
        page = doc[[page]]
    
    nodes = list()

      # Start by finding a declaration of an abstract: Abstract or Summary
    a = findAbstractDecl(page)

    rect = getNodeSet(page, ".//rect | .//line")
    rect.bb = getBBox(rect, color = TRUE, asDataFrame = TRUE)    

    if(length(a) && any(w <- isNodeIn(a[[1]], rect.bb))) {
        # Calzolari
        # Is the Abstract within a colored box. If so, get the text in that box.

        #XXX Only taking one. Generalize
        nodes = getNodesBetween(a[[1]], rect[[which(w)[1]]])
        return(cleanAbstract(nodes, asNodes = asNodes, byLine = byLine))
    }
    
    
    kw = findKeywordDecl(page)
    if(length(kw) == 0 && !is.null(nxt <- getSibling(page)))
        kw = findKeywordDecl(nxt)

    
    kwIsSubmissionInfo = TRUE
    if(length(kw) == 0) {
        # Look for Received, Accepted, Published below the abstract, e.g. Alagaili
        kw = getSubmissionDateInfo(doc)
        kwIsSubmissionInfo = TRUE
        # Could be at the end of the paper, e.g. Khaiboullina
        if(length(kw) > 0 &&  (pageOf(kw[[1]]) - pageOf(page)) > 1)
            kw = NULL
    }
    
    if(length(a) && length(kw)) {  #XXX Should check kw is below a
        nodes = getNodesBetween(a[[1]], kw[[1]])
        return(cleanAbstract(nodes, a, asNodes, byLine = byLine))
    }

    w = (rect.bb[,3] - rect.bb[,1])/as.numeric(xmlGetAttr(page, "width")) > .54

    if(any(w)) {
        if(length(a))
            nodes = getNodesBetween(a[[1]], rect[[which(w)[1]]])
        else if(length(kw)) {
            nodes = getNodesBetween(kw[[1]], rect[[which(w)[1]]])
#browser()            
        } else {
#           browser()
        }
    }

       # If no declaration
    if(length(a) == 0) {
        # From Emerging Infectious Disease format.
        # See if there are two columns and a line across one column that 
        # separates some text from  regular document text
        # This is the title, author list, abstract, line and then regular text.
        # See Waldenstrom-2007, etc.
        cols = mostCommon(unlist(getColPositions(doc, docFont = FALSE)))
        colNodes = getTextByCols(page, asNodes = TRUE, breaks = cols)
        docFont = getDocFont(doc)
        if(length(cols) > 1) {
            lines = rect.bb # getBBox(getNodeSet(page, ".//line | .//rect"))
            if(nrow(lines)) {
                   # only lines that span the first column
                w = lines[,1] - cols[1] < 5 & lines[,3] < cols[2] & abs(lines[,3] - cols[2]) < 18 # The 18 should be based on the width of the column, not the location of the second column.

                if(any(w)) {
                    w = which(w)[1]
                    # the lines[w,2] + 2 gives us a little flexibility for the text going.
                    # Could possibly just use @top and ignore height.
                    nodes = getNodeSet(page, sprintf(".//text[@top + @height <= %f and @left < %d]", lines[w, 2] + 2, cols[2]))

                # XXX Might possibly want them all to be contiguous since in Wessenbrock, we include the word DISPATCHES.
                # So we can find all the nodes with smallest font and cumsum() them and take the last block
                # Need to order them by line/@top            
                # Should order from top to bottom in order to discard
                #   bb = getBBox2(nodes)
                    #   nodes = nodes[order(bb[, 2])]
                    # The abstract may have a single text node that has a smaller font and then we would throw the remainder away.
                    fontInfo = getFontInfo(doc)
                    fonts = sapply(nodes, xmlGetAttr, "font")
                    i = fontInfo[fonts, "size"] == min(fontInfo[fonts, "size"])
                    if(sum(i) < 5)  {
                        # probably a small font within the abstract, e.g. a superscript (see Mehla-2009)
                        # So need to separate title, authors, abstract in a different way.
                        i = fontInfo[fonts, "size"] == min(setdiff(fontInfo[fonts, "size"], min(fontInfo[fonts, "size"])))
#                        ll = nodesByLine(nodes)
                        # tops = as.numeric(sapply(ll, function(x) xmlGetAttr(x[[1]], "top")))
                        nodes = nodes[i]                        
                    } else
                        nodes = nodes[i]
                }  else {
                    nodes = spansColumns(page, cols, colNodes, doc)
                }

                if(length(nodes) == 0)
                    # try within single column, but w/o line.
                   stop("try within single column")
            } else {
                # See if there are lines that span columns

                # And the case where we have no Abstract declaration, but we do have a keyword but the abstract is in the first column.
                # See Gulati-2011

                nodes = spansColumns(page, cols, colNodes, doc)
            }
        }
    } else {

         # So we have an Abstract declaration  (or summary, etc.)

        mar = margins(page)
        bb = getBBox2(a)
#        browser()
        # See if we have a line just below the declaration and if so, see if there is one below the text.
        bb2 = getBBox(getNodeSet(page, ".//line | .//rect"))
        bb2 = bb2[ bb2[,2]> bb[1,2], ]
        

        # This test may be more appropriate: asking if there any text nodes within a few lines that is
        # significantly to the left of a[[1]].  This is for the Elsevier documents
        # such as Oliveira-2009.
        # However, causes problems for Buckley-2006 and similar that have a Summary not quite centered
        if(anyTextToLeft(a[[1]], bb)) {  # Used to be simply if(bb[1,1] > mar[1] * 1.1)
           nodes = getShiftedAbstract(page, bb)
        } else {
            # Flush with left margin.  Maybe not quite flush.

              # Dealt with this above now as first check.
            # kw = findKeywordDecl(page)
            if(length(kw))
                nodes = getNodesBetween(a[[1]], kw[[1]])
            else if(length(h <- findSectionHeaders(doc, onlyFirst = TRUE))) {
                nodes = getNodesBetween(a[[1]], h[[1]])
            } else if(length(nodes <- spansColumns(page, doc = doc, abstractDecl = a))) {
               # don't do anything, just fall through to the cleanAbstract() 
            } else {
                # Have Abstract declaration. In a single column and there is a line that ends it.
                # e.g. Pan 2014
                # Can generalize to find another way to find the end.
                cols = getColPositions(page)
                col = columnOf(a[[1]], cols)
                bot = min(bb2[ bb2[, "x1"] < cols[2] , "y0"])
                nodes = getNodeSet(page, sprintf(".//text[ @top >= %f and @top < %f and @left < %f]", as.numeric(xmlGetAttr(a[[1]], "top")) - 8,  bot, cols[2]))
            }
        }
    }

    cleanAbstract(nodes, a, asNodes, byLine = byLine)
}

cleanAbstract =
function(nodes, abstractDecl = NULL, asNodes = TRUE, byLine = TRUE)    
{
    if(length(nodes) == 0)
       return(nodes)
    
    if(length(abstractDecl)) {
        bb = getBBox2(abstractDecl)
        page = xmlParent(abstractDecl[[1]])
        ll = getBBox(getNodeSet(page, ".//rect|.//line"))

        # is there a line above the abstract and nothing between that line and the abstract text, e.g. Meister-2008
        w = ll[, "y0"] < bb[1, "top"]  # and wide enough to not be a line for some other purpose.
                          # add the max(, 0) here in case !any(w) and would get empty vector.
        z = getNodeSet(page, sprintf(".//text[@top > %f and @top < %f]", max(ll[w, "y0"], 0), bb[1, "top"])) 
        if(length(z) == 0) {
            bb = getBBox2(nodes, pages = TRUE)
            pageNum = pageOf(abstractDecl[[1]])
               # below the line, but only on this page, include all the nodes above this on the other pages
            nodes = nodes[ bb[, "top"] > max(ll[w, "y0"], 0) | bb[, "page"] != pageNum ]
        }
    }

      #XXX do other pages
    pos = getFooterPos(xmlParent( nodes[[1]] ) )
    if(!is.na(pos)) {
        bb = getBBox2(nodes, pages = TRUE)
        pageNum = pageOf(nodes[[1]])
        nodes = nodes[ bb[, "top"] < pos | bb[, "page"] != pageNum]
    }
        
    
    txt = sapply(nodes, xmlValue)
    nodes = nodes[ ! grepl("(19|20)[0-9]{2}.*elsevier|rights reserved|copyright" , tolower(txt)) ]

    
    if(byLine) 
       nodes = nodesByLine(nodes)

    if(!asNodes)
       return(names(nodes))
    
    nodes
}

findKeywordDecl =
    # Find Key Words, Keywords, etc. 
    # Could add test for bold font.
function(page)
{
    getNodeSet(page, ".//text[starts-with(lower-case(.), 'key words') or starts-with(., 'Keywords') or starts-with(., 'Key words') or starts-with(., 'Keyword Index')]")    
}

findAbstractDecl =
    # Find a node that identifies an abstract or summary. Add more as needed.
function(page)
{
    getNodeSet(page, ".//text[starts-with(lower-case(normalize-space(.)), 'abstract') or starts-with(lower-case(normalize-space(.)), 'summary')]")
}

anyTextToLeft =
function(node, bbox = getBBox2(list(node)), page = xmlParent(node))
{
  length(getNodeSet(page, sprintf(".//text[ abs(@top - %f) < 30 and @left <=  %f]", bbox[1,2], bbox[1,1]*.66))) > 0
}


getShiftedAbstract =
  # So not flush with left margin or even very close
            # e.g. 1.s2.0-S1090
            #
            # Check if it is centered
            #
        #
        # Can find the end of the abstract, i.e. vertical position below which
        #  content is no longer in abstract
        # Then find all nodes to the right of bb[1,1] and above this position.
        # OR find all lines flush with this bb[1,1] and the lowest of these 
        # defines the end. Then go back and find all the text to right
            #
            # If the "Abstract" line is indented, then need to allow for how far back??
            # Using 14.  Used to be 4. But we really should look at the next line down.        
function(page, bbox)
{
    nodes = getNodeSet(page, sprintf(".//text[abs(@left - %d) < 14]", bbox[1,1]))
    bb2 = getBBox2(nodes)
    bot = max(bb2[, 2] + bbox[,4])
    nodes = getNodeSet(page, sprintf(".//text[abs(@left - %d) < 14 and @top > %d and @top + @height <= %f + 9]", bbox[1,1] - 4, bbox[1,2] - 4, bot))       
}


getAbstractBySpan=
function(doc, col = getColPositions(doc[[1]]))
{
    if(length(col) == 1)
        return(NULL)

    
}




spansColumns =
function(page, cols = getColPositions(as(page, "XMLInternalDocument"), docFont = FALSE),
         colNodes = getTextByCols(page, asNodes = TRUE, breaks = cols),
         doc = as(page, "XMLInternalDocument"), abstractDecl = NULL)
{
    #
    # Find all the lines that span multiple columns.  This includes title, authors, and abstract and possibly centered
    #  section headers.
    # Find the ones that don't have a large gap between the text segments,
    #                    start in the first column
    # Within these lines, find the subset that are consecutive, left aligned (allowing for a small indentation) and span 
    #

    if(is.list(cols))
       cols = cols[[1]] # XXX make better

    # Get all the lines of text, across all columns
    ll = nodesByLine(getNodeSet(page, ".//text"))
    # get the left and right of each line 
    r = t(sapply(ll, function(x) { bb = getBBox2(x); c(start = min(bb[,1] ), end = max(bb[,1] + bb[,3]))}))
    #
    mdiff = sapply(ll, function(x) max(gapBetweenSegments(x)))
    gap = mostCommon(mdiff[mdiff > 0])
    w = r[, "start"] < cols[2] & r[, "end"] > cols[length(cols)] & mdiff < gap
    if(!any(w))
       return(list())
    
    idx = split((1:length(ll))[w], cumsum(!w)[w])
    # Pick the block with the most text. XXX  Need to make this more robust

    b = idx[[which.max(sapply(idx, length))]]

    # now patch up
    
    # XXX add an extra line after this as the final line may not span the entire width to be beyond the start of the last column.
    ans = unlist(ll[ c(b, max(b) + 1) ] )

    bb2 = getBBox2(ans)
    left = mostCommon(bb2[, "left"])
    i = abs(r[, "start"] - left) < 15
    ans = unlist(ll[i])

    if(length(abstractDecl)) {
        bb = getBBox2(ans)
        top = getBBox2(abstractDecl)
        ans = ans[ bb[, "top"] >= (top[1, "top"] - 5)]
    } 
    
    ans
}

mostCommon =
function(x, convert = as.numeric)
{
    tt = table(x)
    ans = names(tt)[tt == max(tt)]
    if(!is.null(convert))
        convert(ans)
    else
        ans
}

spansColumns2 =
function(page, cols = getColPositions(page, docFont = FALSE), colNodes = getTextByCols(page, asNodes = TRUE, breaks = cols),
            doc = as(page, "XMLInternalDocument") )
{
    bb = getBBox2(colNodes[[1]])
    w = bb[,1] + bb[,3] > cols[length(cols)] #XXXX!!!!
    nodes = NULL
   
    if(any(w)) {
        nodes = colNodes[[1]][w]
        # Different ways to filter just the abstract.
        # Last paragraph of these nodes with a big enough space in between
        # all the nodes with the same font as the last line.
        # The lines are right aligned (and left) as opposed to just centered
        # Below a line with Received

                       # taken from above for lines - merge.
        fontInfo = getFontInfo(doc)
        fonts = sapply(nodes, xmlGetAttr, "font")
#        browser()
        w = fonts == fonts[length(fonts)]
        nodes = nodes[w]
        # Now have to get all the nodes in this region, not just the ones
        # with these fonts as that drops, e.g., italics.
        nodes = getNodesBetween(nodes[[1]], nodes[[length(nodes)]])
    }

    nodes
}


gapBetweenSegments =
function(nodes, bbox = getBBox2(nodes))
{
   n = nrow(bbox)
   if(n == 1) return(-1)
   bb = bbox[order(bbox[,1]), ] 
   bb[-1, 1] - (bb[1:(n-1), 1] + bb[1:(n-1), 3])
}



context =
function(pos, vec, num = 10)
{
    if(length(pos) > 1)
        return(lapply(pos, context, vec))


    vec[seq(max(1, pos - num),  min(length(vec), pos + num))]
}



isNodeIn =
function(node, boxes, pos = getBBox2(list(node)))
{
    pos[,1] >= boxes[,1] & pos[,2] > boxes[,2] & (pos[,1] + pos[,3]) < boxes[,3] & (pos[,2] + pos[,4]) < boxes[,4] 
}



################################################################################
# AClasses.R
setOldClass(c("PDFToXMLDoc", "ConvertedPDFDoc", "XMLInternalDocument", "XMLAbstractDocument"))
setOldClass(c("PDFToXMLPage", "ConvertedPDFPage", "XMLInternalElement", "XMLInternalNode", "XMLAbstractNode"))


setAs("character", "PDFToXMLDoc", function(from) readPDFXML(from))


setAs("XMLInternalNode", "PDFToXMLPage",
      function(from) {
          if(xmlName(from) != 'page') 
              stop("XML node is not a <page>")

          class(from) = c("PDFToXMLPage", "ConvertedPDFPage", class(from))
          from
      })
################################################################################
# assembleLines.R


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

################################################################################
# authorAffiliations.R

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
################################################################################
# bibRefs.R
findBibCites =
    #
    # Find text that identifies citations in the bibliography
    # in the form of numbers, ranges of numbers, etc.
    # that are superscripts.
    #
function(doc, supThreshold = .66, fonts = getFontInfo(doc),
         textFont = getDocFont(doc))
{
    w = fonts$size < supThreshold*textFont$size
    if(any(w))
        fontq = paste(sprintf("@font = '%s'", fonts$id[w]), collapse = " or ")
    else
        fontq = "true"
    xp = sprintf("//text[(%s) and isBibSup(normalize-space(.))]", fontq)
    tt = getNodeSet(doc, xp,  xpathFuns = list(isBibSup = isBibSup))
}

isBibSup =
function(str)
{
    grepl("^[0-9]+((,[0-9]+)*|­[0-9]+)$", str)
                            # ^  non ascii character.
}
################################################################################
# emergingInfDisease.R

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

################################################################################
# datesPublished.R

#  See  ~/DSIProjects/Zoonotics-shared/PublicationDate.R for a better version.
getDatePublished =
function(doc)
{    
   nodes = getNodeSet(doc, "//text[contains(., 'Received:')] | //text[contains(., 'Published:')] | //text[contains(., 'Accepted:')]")
   if(length(nodes) == 0)
       return(NULL)

   txt = sapply(nodes, xmlValue)
   e = strsplit(unlist(strsplit(txt, " / ")), ":")
   structure(XML:::trim(sapply(e, `[[`, 2)), names = XML:::trim(sapply(e, `[[`, 1)))
}


getSubmissionDateInfo =
function(doc, phrases = c("Received", "Accepted", "Available online", "Published at", "Published online", "received for review"))
{
  cond = sprintf("starts-with(normalize-space(.), '%s')", t(cbind(phrases, paste0("(", phrases))))
  getNodeSet(doc, sprintf("//text[%s]", paste(cond, collapse = " or ")))
}

################################################################################
# PublicationDate.R

getPublicationDate1 =
    # OLD VERSION - SEE BELOW
function(doc, page = 1, words = c("accepted", "received", "volume", "copyright", "published", "submitted", "recebido", "aceito"))
{
  if(is.character(doc))
     doc = xmlParse(doc)

    # look for the words that often identify a year.
  cond = paste(sprintf("contains(lower-case(.), '%s')", words), collapse = " or ")
    
  ans = xpathSApply(doc, sprintf("//page[@number='%d']//text()[%s]", page, cond),  xmlValue)

  if(length(ans) == 0) {
      tmp = getNodeSet(doc, sprintf("//page[@number = '%d']//text()[contains(., 'Emerging Infectious Disease')]", page))
      if(length(tmp))
         ans = gsub(".* Vol\\. .*, (.*)", "\\1", xmlValue(tmp[[1]]))
  }


 if(length(ans) == 0) {
      tmp = getNodeSet(doc, sprintf("//page[@number = '%d']//text()[contains(., 'Journal of ')]", page))
      if(length(tmp))
         ans = xmlValue(tmp[[1]])
  }  

  # We don't do anything with the txt here.
 if(FALSE || length(ans) == 0) {
      tmp = getNodeSet(doc, sprintf("//page[@number = '%d']//text()", page))
      if(length(tmp)) {
         txt = sapply(tmp, xmlValue)
      }
  }  

  if(length(ans) == 0 && page == 1) {
           # Go to the last page
     npages = length(getNodeSet(doc, "//page"))
     if(npages > 1) {
       ans = getPublicationDate1(doc, npages, words)
       if(length(ans) == 0 && npages > 2)
           # try second page
          return(getPublicationDate1(doc, 2, words))
    }
  }

  ans
}


findVol =
function(doc)
{
   unlist(xpathSApply(doc, "//text()[contains(lower-case(.), 'vol')]", getVolume))
}

getVolume =
function(node)
{
   txt = xmlValue(node)
   grep("Volume|Vol\\.?[[:space:]]?[0-9]+", txt, value = TRUE)
}

#tmp = lapply(docs2, )



hasCoverPage =
function(doc)
{
  isBioOne(doc) || isMBio(doc) || isResearchGate(doc)
}

isMBio =
function(doc)    
{
   length(getNodeSet(doc, "//page[1]//text[contains(., 'mBio')]")) > 0 &&    length(getNodeSet(doc, "//page[1]//ulink[starts-with(@url, 'http://mbio.asm.org')]")) > 0
}

isBioOne =
    #
    # There are some documents from BioOne which are scanned documents with a front page that is not scanned.
    # isScanned() only looks at the first page. So we detect these explicitly.
    #
function(doc)
   length(getNodeSet(doc, "//text[starts-with(., 'BioOne sees sustainable scholarly ')]")) > 0




getPublicationDate =
    #
    # New version
    #
    #
    #@'return a named character  vector. The values contain a date, possibly with other text content.
    #     the names indicate which step/method was used to identify the date.  These will all be the same
    #     as we stop when we have any date.
    #
    # The steps are Scanned (no date)
    #               NIH Public Access  (particular format in which we can find the date)
    #               Title     (Date in the title)    
    #               Received  (information about when received, accepted, published)
    #               footer    (taken from the footer on the first page)
    #               header    (taken from the header of the first page)
    #               copyright (find the copyright symbol and the date after it)
    #               AboveTitle (text above the title of the paper)
    #               TextRegEx  (find a date of the form [number] NameOfMonth[,] Year anywhere in the text)
    #
function(doc, checkAbstract = TRUE)
{
  if(is.character(doc))
     doc = readPDFXML(doc)

  if(checkAbstract) {
      abstract = try(findAbstract(doc, FALSE))

      if(!is(abstract, 'try-error') && length(abstract)) {
          txt = paste(abstract, collapse = "\n")
          m = gregexpr("\\b(19|20)[0-9]{2}\\b", txt)
          if(any(m[[1]] > -1)) {
              y = unique(regmatches(txt, m)[[1]])
              return(structure(y, names = rep("abstract", length(y))))
          } 
      }
  }
  
  if(isBioOne(doc) && !is.na(tmp <- textAboveTitle(doc, 2)))
      return(tmp)

  if(isScanned(doc)) { # was isScanned2()
      y = getYearFromFileName(basename(docName(doc)))
      if(length(y))
          return(c(filename = y))
      else
          return(structure(NA, names = "Scanned"))
  }

  nih = getNodeSet(doc, "//text[. = 'NIH Public Access']")
  if(length(nih) > 0) {
      txt = unique(xpathSApply(nih[[1]], "./following-sibling::text[contains(., 'doi:')]", xmlValue))
      if(length(txt))
        return(structure(txt, names = "NIH Public Access")) # , journal = xmlValue(getSibling(nih[[1]]))))
  }

  if(length(getNodeSet(doc, "//text[starts-with(., 'www.oie.int/')]")) > 0) {
     date = xmlValue(getNodeSet(doc, "//text[. = 'Date of start of the event']/following-sibling::text[1]")[[1]])
     return(c(OIE = date))
  }
  
  title = getDocTitleString(doc)
  if(hasYear(title))
      return(structure(title, names = "Title"))

  rec = getSubmissionDateInfo(doc)
  if(length(rec) > 0) {
      txt = xmlValue(rec[[1]])
      if(!hasYear(txt)) {
          top = xmlGetAttr(rec[[1]], "top")
          txt = paste(xpathSApply(rec[[1]], sprintf("./following-sibling::text[@top = '%s']", top), xmlValue), collapse = " ")
      }
      if(hasYear(txt))
          return(structure(txt, names = rep('Received', length(txt))))
  }

  rec = getNodeSet(doc, "//text[contains(., 'received for review')]")
  if(length(rec) > 0) {
      txt = xmlValue(rec[[1]])
      if(!hasYear(txt)) {
          top = xmlGetAttr(rec[[1]], "top")
          txt = paste(xpathSApply(rec[[1]], sprintf("./following-sibling::text[@top = '%s']", top), xmlValue), collapse = " ")
      }
      if(hasYear(txt))
          return(structure(txt, names = rep('Received', length(txt))))
  }
  
  

  p1 = getNodeSet(doc, "//page")[[1]]
  footer = getPageFooter(p1)
 
  if(!grepl("Downloaded", footer) && any(w <- hasYear(footer)))
      return(structure(footer[w], names = rep("footer", sum(w))))

  footer = getPageHeader(p1)
  if(!grepl("Downloaded", footer) && any(w <- hasYear(footer)))
      return(structure(footer[w], names = rep("header", sum(w))))  

                                      #XXX  non-ASCII symbol
  cr = getNodeSet(doc, "//text[contains(., '©')]")
  if(length(cr)) {
      tt = sapply(cr, xmlValue)
      if(any(w <- hasYear(tt)))
          return(structure(tt[w], names = rep("copyright", sum(w))))
  }

  tt = textAboveTitle(doc, 1)
  if(any(w <- hasYear(tt)))
      return(structure(tt[w], names = rep("AboveTitle", sum(w))))


  txt = getDocText(doc)
  rx = sprintf("([0-9]{1,2} )?(%s),? (19|20)[0-9]{2}", paste(getMonthNames(), collapse = "|"))
  g = gregexpr(rx, txt, ignore.case = TRUE)
  if(g[[1]][1] > 0) {
      tt = unique(regmatches(txt, g)[[1]])
      return(structure(tt, names = rep("MonthNameYear.TextRegEx", length(tt))))
  }

  # Could go to the second page and start over with the headers, etc.

  tt = getNodeSet(doc, "//text[isDate(string(.))]",
                  xpathFuns = list(isDate = containsDate))
  if(length(tt)) #XXX Put the match type here.
      return(c(TextDate = unique(extractDate(sapply(tt, xmlValue)))))


  fname = basename(docName(doc))
  y = getYearFromFileName(fname)
  if(length(y)) 
     return(c(filename = y))
  
  NA
}

getYearFromFileName = getYearFromString =
    # getYearFromFileName("Kohl 1996.xml")
    # getYearFromFileName("Smithburn-1949-The susceptibility of African w.xml")
function(fname)
{
  #   "(^|[^0-9])[0-9]{4}([^[0-9]|$)"
  # But need to not include characters within the () ()
  m = gregexpr("(\\b|_)(19[0-9]{2}|20[01][0-9]{1})(\\b|_)", fname, perl = TRUE)
  if(any(m[[1]] > -1))
     gsub("(^_|_$)", "", regmatches(fname, m)[[1]])
  else
     character()
}

getMonthNames =
function(format = c("%b.", "%b", "%B"))
{    
  unlist(lapply(format, function(f) format(ISOdate(2017, 1:12, 1), f)))
}

containsDate =
function(str)
{
   grepl(mkDateRegexp(), str) 
}

mkDateRegexp =
function()
{
  sprintf("[0-9]{4} (%s)( [0-9]{,2})", paste(getMonthNames(), collapse = "|"))
}

extractDate =
function(str)
{
  unlist(regmatches(str, gregexpr(mkDateRegexp(), str)))
}


getDocText =
    # Too simple. See the one in ReadPDF
function(doc)
{
    if(is.character(doc))
       doc = readPDFXML(doc)
    
    paste(xpathSApply(doc, "//text", xmlValue), collapse = " ")
}

textAboveTitle =
    # Finds the title and then gets the text above that. This is useful when there is header material
    # in a sequence of lines.
    # Could find it in other ways also.
function(doc, page = 1)
{
    titleNodes = getDocTitle(doc, page)
    if(length(titleNodes) == 0 || is.character(titleNodes))
        return(NA)
   pos = min(as.integer(sapply(titleNodes, xmlGetAttr, "top")))
   page = getNodeSet(titleNodes[[1]], ".//ancestor::page")[[1]]
   bbox = getBBox2(getNodeSet(page, ".//text"))
   rownames(bbox)[ bbox[, "top"] < pos]
}

hasYear =
function(txt)
{
#      grepl("(^| )(19|20)[0-9]{2}( |$)", txt)
     grepl("\\b(19|20)[0-9]{2}\\b", txt)
}



firstIsolated =
function(doc, text = pdfText(doc))
{
    if(missing(text) && is.character(doc))
       doc = readPDFXML(doc)

    pageNum = rep(seq(along = text), sapply(text, length))
    text = unlist(text)
    i = grep("first[[:space:]]+isolated", text, ignore.case = TRUE)
    structure(text[i], names = pageNum[i])
}

################################################################################
# findSections.R

getSectionText =
    #
    #
    #
    #
function(doc, asNodes = FALSE, secHeaders = findSectionHeaders(doc, ...), maxNumPages = 30, cleanSectionNums = TRUE,
             addOmitted = TRUE, separateTables = TRUE, ... )
{
    if(is.character(doc))
        doc = readPDFXML(doc)

    if(getNumPages(doc) > maxNumPages) {
      warning("doc pages exceeds maxNumPages")
      return(list())      
    }


    if(separateTables) {
        tblNodes = getTables(doc)
        tbls = sapply(tblNodes, function(x) paste(names(x), collapse = "\n"))
        nn = unlist(tblNodes)
        if(!is.null(nn))
          removeNodes(nn[!sapply(nn, is.null)])
    }
    
    if(length(secHeaders) == 0) {
        ti = unlist(getDocTitle(doc, asNode = TRUE))
        end = getLastRealTextNode(doc)
        ans = getNodesBetween(ti[[length(ti)]], end)
        return(ans)
    }

    #if only one node supplied, add last node as end node, flag to return one thing
    if(length(secHeaders) == 1) {
      secHeaders = c(secHeaders, getLastNode(secHeaders))
      pasteTxt = TRUE
    } else {pasteTxt = FALSE}
    
    secHeaders = orderNodes(unlist(secHeaders))

    secs = lapply(seq(along = secHeaders),
                  function(i)
                    getNodesBetween(secHeaders[[i]], if(i == length(secHeaders)) NULL else secHeaders[[i+1]]))
    names(secs) = sapply(secHeaders, xmlValue)


    if(cleanSectionNums)
      names(secs) = removeNumPrefixes(names(secs))

    #XXX deal with the tables.
    if(asNodes)
       return(secs)
    
    txt = sapply(secs, xmlValue)

    if(addOmitted) {
        start = doc[[1]][["text"]]
        if(!is( a <- try(findAbstract(doc), silent = TRUE), 'try-error') && length(a))
            start = rev(unlist(a))[[1]]
#        try( { start = rev(unlist(findAbstract(doc)))[[1]]})
        onodes = getNodesBetween(start, secHeaders[[1]])
        txt["<other>"] = paste(sapply(onodes, xmlValue), collapse = " ")
    }

    if(separateTables && length(tbls)) 
        txt[paste0("Table", seq(along = tbls))] = tbls
    
    if (pasteTxt) {c(txt[[1]],txt[[2]])} else {txt}
}


#
# Find lines that
#  is one of the regular section header text
#  have a larger font than others on the page
#  shorter than most in the same column
#  have a larger font than most text nodes in the column
#  larger vertical distance between it and the next line
#

# Includes names
#    "LatestDocs/PDF/0629421650/Padula-2002-Andes virus and first case report1.xml"
# also doesn't get the subheadings for Patient 1 and Patient 2
#

findSectionHeaders =
    #
    # Given a node, find the node identifying the section in the paper
    # for this node.
    #
    #  Looking for text on its own line,  in bold or larger font.
    #  "../LatestDocs/PDF/2157652992/Calisher-2006-Bats_ Important Reservoir Hosts.xml"
    #  ""../LatestDocs/PDF/0000708828/Li-2009-Sensitive, qualitative detection of hu.xml"
    #  "../LatestDocs/PDF/2636860725/Růžek-2010-Omsk haemorrhagic fever.xml"
    #  "../LatestDocs/PDF/3757461900/Matsuzaki-2002-Antigenic and genetic characte1.xml"
    #
    #
    #
    #'@param checkCentered  if the nodes we identify as section
    #     using the "expected" names  are centered, then by default
    #     when we look for other text with the same font, we only
    #     include centered text.  However, if checkCentered = FALSE
    #     we include all text with the same section header font.
    #     Checking for centered is currently expensive.
    #
    #  See weaver-2001 for example of main section titles being
    #  centered but sub-sections are in the same font and not centered.
    #
#
#  Need to include the continuation text on the next lines if the section title
# spans multiple lines. See, e.g.,
#     2019 CItation/Papers/1-50/11 Bakir art%3A10.1007%2Fs00024-012-0482-8.xml
# in the CIG papers.
#
#  Subsections, see CIG_citation/2019 CItation/Papers/1-50/1 aagaard jgrb50217.xml
#
    
function(doc, sectionName = c('introduction', 'background',
                  'conclusions', 'discussion', 'materials and methods',
                  'literature cited', 'references cited', 'the study'),
            # For, e.g., Lahm-2007-Morbidity, with these 2 extra section names, we
            # match References and Ackno..  and these don't have
            # numbers.
            # Maybe go for numbered section titles first?         
         otherSectionNames = c('references', 'acknowledgements', 'acknowledgments', 'results', 'methods'),
         checkCentered = TRUE,
         discardAfterReferences = TRUE,
         allowRotated = FALSE, onlyFirst = FALSE,
         order = TRUE, groupByLine = FALSE
         )
{
    if(is.character(doc))
        doc = readPDFXML(doc)
    
       # Find section titles with numbers
    hasNum = FALSE    
    filter = paste(sprintf("(contains(lower-case(normalize-space(.)), '%s') and isNum(normalize-space(.)))", sectionName), collapse = " or ")
    xp = sprintf("//text[%s]", filter)
    intro = getNodeSet(doc, xp, xpathFuns = list(isNum = isSectionNum))
    hasNum = length(intro) > 0 

    if(!hasNum) {
       filter = paste(sprintf("lower-case(normalize-space(.)) = '%s'", sectionName), collapse = " or ")
       xp = sprintf("//text[%s]", filter)
       intro = getNodeSet(doc, xp)
    }

    if(onlyFirst)
        return(intro)
    

    if(!length(intro)) {
       filter = paste(sprintf("lower-case(normalize-space(.)) = '%s'", otherSectionNames), collapse = " or ")
       xp = sprintf("//text[%s]", filter)
       intro = getNodeSet(doc, xp)
    }

    if(length(intro))
        intro = intro[ ! isLowerCase(sapply(intro, xmlValue)) ]


    if(length(intro)) {

        if(hasNum)
            return(getNodeSet(doc, sprintf("//text[isNum(normalize-space(.)) and (%s)]",
                                             paste( sprintf("@font = '%s'", sapply(intro, xmlGetAttr, "font")), collapse = " or " )),
                               xpathFuns = list(isNum = isSectionNum)))


        i = sapply(intro, function(x) length(getNodeSet(x, "./preceding::text[ . = 'Supporting Online Material']"))) > 0
        intro = intro[!i]
        if(length(intro) == 0)
           return(NULL)
        
        fontID = unique(sapply(intro, xmlGetAttr, "font"))
        #XX Check if on line by itself and not just a word.
        # Check if these are centered on a column or on the page. If so,
        # other nodes we think are titles also better be centered.
        secs = getNodesWithFont(doc, fontID = fontID)

           # Discard elements that are table of contents, ie. have leaders ..... page number
        secs = secs[!grepl("\\.{5,}[[:space:]]*[0-9]+$", xmlValue(secs))]

        if(!allowRotated)
           secs = secs[as.numeric(sapply(secs, xmlGetAttr, "rotation")) == 0]
        
           # Discard elements that are just numbers
        secs = secs[!grepl("^[0-9]+$", xmlValue(secs))]            

        preRefs = sapply(secs, function(x) getNodeSet(x, ".//preceding::text[ lower-case(normalize-space(.)) = 'references' or lower-case(normalize-space(.)) = 'references cited' or lower-case(normalize-space(.)) = 'supporting online material']"))
        w = sapply(preRefs, length) > 0

        if(any(w) && hasCoverPage(doc))
            w[w] = sapply(preRefs[w], function(x) pageOf(x) != 1)

        secs = secs[!w]

           # if all the known section headers are all upper case
        if(all(isUpperCase(sapply(intro, xmlValue))))  {
            txt = sapply(secs, xmlValue)
            secs = secs[ i <- isUpperCase(txt)  ]
            secs = secs[ !grepl("^[[:punct:]]+$", txt[i]) ]
        }

        # For isCentered and isOnLineBySelf, we should determine which pages
        # we are dealing with and compute the getTextByCols() and
        # nodesByLine() for each of these just once and pass them to these
        # functions

        if(checkCentered)
           secs = secs[ sapply(secs, isCentered) == isCentered(intro[[1]])]

        if(isOnLineBySelf(intro[[1]])) {
            i = sapply(secs, isOnLineBySelf)
            secs = secs[ i ]
        }

        if(order) { # Do we need to do this?? 
            o = order(sapply(secs, pageOf), sapply(secs, inColumn))
            secs = secs[o]
        }
        if(groupByLine) {
            # XXX This changes the order of the nodes.
            # We really should group these by page and within column, except those that span multiple columns.
            # We now turn this off. What does this do to getSectionText().
           secs = nodesByLine(secs)
        }
        
        secs
    }
}

isUpperCase =
function(x, hasChars = TRUE)
    x == toupper(x) & grepl("[A-Z]", x)

isLowerCase =
function(x)
     x == tolower(x)

isSectionNum =
    #
    # For use in XPath test.
    #
function(x)
    grepl("^[0-9](\\.[0-9](\\.[0-9])?)?\\. ", x)


getNodesWithFont =
function(doc, fontID)
{
  getNodeSet(doc, sprintf("//text[%s]",  paste(sprintf("@font = %s", fontID), collapse = " or ")))
}

isOnLineBySelf =
function(node, pos = getColPositions(xmlParent(node)),
         textNodes = getNodeSet(xmlParent(node), ".//text"),
         bbox = getBBox2(textNodes, TRUE))
#         doc = as(node, "XMLInternalDocument"))
{
    colNodes = getTextByCols(pageOf(node, TRUE), breaks = pos, asNodes = TRUE)
       # determine which column this is in
    colNum = inColumn(node, colNodes)
    col = colNodes[[colNum]]
#    lines = split(col, as.integer(sapply(col, xmlGetAttr, "top")))
    h = as.integer(xmlGetAttr(node, "top"))
    npos = as.integer(sapply(col, xmlGetAttr, "top"))
    sum(npos == h) == 1
}

identicalInColumn = 
function(x, node)
{    
    if(length(x))
       any(sapply(x, identical, node))
    else
       FALSE
}
       
inColumn =
function(node, cols = getTextByCols(xmlParent(node), asNodes = TRUE))
{
    ans = which(sapply(cols, identicalInColumn, node))
    if(length(ans))
       return(ans)

    # We may end up with an empty answer if the node has a different font
    # than the docFont and getTextByCols() uses docFont.
    # In this case, we'll compute the spans of the columns and then
    # determine which one node falls in

    sp = lapply(cols,
           function(x) {
               ll = nodesByLine(x)
               le = getLineEnds(ll)
               apply(le, 2, median)
           })

    bb = getBBox2(list(node))
    start = bb[1, "left"]  
    end = bb[1, "left"]  + bb[1, "width"]
    w = sapply(sp, function(x) start >= x[1] || end >= x[1])
    which(w)
}

#XXX give proper name.
f = 
function(page, nodes = getNodeSet(page, ".//text"),
         bb = getBBox2(nodes, TRUE),
         cols = getColPositions(page))
{    
    b = split(bb, cut(bb$left, c(0, cols[-1], Inf) -2))
    k = lapply(b, function(x) x[order(x$top),])
}



getNodesBetween =
function(x = NULL, y = NULL, useLines = TRUE, exclude = FALSE, ...)
{
    if(is.null(x) && is.null(y))
        stop("need to specify either x or y or both")
    
    if(is.null(x))
        x = getFirstTextNode(as(y, "XMLInternalDocument"))
    else if(is.list(x))
        x = x[[length(x)]]
    
    if(is.null(y))
       y = getLastNode(x)
    else if(is.list(y))
       y = y[[length(y)]]
    
    s = pageOf(x)
    e = pageOf(y)
    
    #check if going from same node to same node, 
    if (identical(x,y) & s == e) {
      return(x)
    } else {

        ans = if(e > s) {
         # get all the nodes on each page up to e
          p1 = getTextAfter(x, useLines = useLines)
          if(e - s > 1) {
            pgs = getPages(as(x, "XMLInternalDocument"))[ seq(s + 1, e - 1)]
            pgs = lapply(pgs, getTextByCols, asNodes = TRUE)
          } else
            pgs = NULL
          pe = getTextAfter(, y, useLines = useLines)
          c(p1, unlist(pgs, recursive = FALSE), pe)
        } else {
          getTextAfter(x, y, useLines = useLines, ...)
        }

      if(exclude) 
           # drop x and y.  XXX need to handle if y is null in call and keep then.
        ans = ans[-c(1, length(ans))]

      ans
    }
    
}

getTextAfter =
    #
    # This is a somewhat brute-force approach to getting the <text> nodes
    # between one start node and an optional end node ON THE SAME <PAGE>!
    # getNodesBetween() is for multiple pages and calls this function
    # so can handle single pages also.
    # The name of this function is not entirely correct. We can
    # specify either x OR to so it can get the nodes before the to node.
    # One can specify x and not to, x and to, or just to.
    #

#XXX FIX THIS TO KEEP THE TEXT BY COLUMN.
function(x = NULL, to = NULL, before = FALSE, useLines = TRUE, ...)
{
    page = xmlParent(if(!is.null(x)) x else to)
    cols = getTextByCols(page, asNodes = TRUE, order = TRUE, ...)

    if(!is.null(x) && !is.null(to) && pageOf(to) < pageOf(x)) {
        warning("to node in getTextAfter() is on earlier page (", pageOf(to) , " versus ",  pageOf(x), "  Ignoring to node")
        to = NULL
    }

    original.to = to
    
    if(useLines) {
          # If to is a rect/line, find its location, otherwise find any lines on this page.
       if(!is.null(to) && xmlName(to) %in% c('rect', 'line')) {
           bb = getBBox(list(to))
           bb[1,2] = bb[1,4]
           to = NULL
#           useLines = FALSE
       } else
           bb = getBBox(getNodeSet(page, ".//rect | .//line"))
    }
    
    if(!is.null(x)) {
        # find the column and the index of the node matching x. Not the same as columnOf() as we want the index within the column.
        if(xmlName(x) == "text") {        
           i =  lapply(cols, function(n) if(length(n)) which(sapply(n, identical,  x)) else integer())
           colNum = which(sapply(i, length) > 0)        
                                        #        colNum = which(sapply(cols, identicalInColumn, x))
        } else {
            ## Force for now!!!  Not a <text> element, so presumable a <line> or <rect>
            warning("hard coded column for now")
            i = 1L
            colNum = 1L          
        }
    }

    if(!is.null(to)) {
        # to.colNum = which(sapply(cols, identicalInColumn, to))        

        j = lapply(cols, function(n) if(length(n)) which(sapply(n, identical,  to)) else integer())
        to.colNum = which(sapply(j, length) > 0)
        if(is.null(x))
           return( c(cols[ seq(1, length = to.colNum - 1) ],
                     cols[[to.colNum]][ seq(1, length = j[[to.colNum]] - 1) ]))
    }


    if(is.null(to)) {
        nodes = cols[[colNum]][ - (1:(i[[colNum]]-1)) ]
        if(colNum < length(cols))
            nodes = c(nodes, cols[(colNum+1):length(cols)])
    } else {

        
        if(colNum == to.colNum) {
           nn = cols[[ colNum ]]
           nodes = nn[  seq(i[[colNum]], j[[to.colNum]] - 1) ] 
        } else {
              # nodes in x's column
            nodes = cols[[colNum]][ - (1:(i[[colNum]]-1)) ]
            # in Becker-2012, Author Contributions is x and References is to
            # but References is actually in the 1st column of this page and Author Contributions
            # is in the second but slightly above. findSectionHeaders() is ordering them this way
            # but not taking into account References should probabl come first.
            # Is References a node that is after Author Contributions in document order?
            btwn = seq(colNum + 1, length = to.colNum  - colNum - 1)
            nodes =  c(nodes, cols[btwn],
                        cols[[to.colNum]][ seq(1, length = j[[to.colNum]] - 1) ])
        }
    }

    
    if(useLines) {
            #XXX FIX THIS - x or to is missing?
            # Handle the cases where we return earlier.        
        tmp = list(x)
        if(!is.null(to))
            tmp[[2]] = to
        bb2 = getBBox2(tmp)
###!!!!!        
        isShape = sapply(tmp, xmlName) != "text"
        if(any(isShape)) {
             # Convert getBBox to x0, y0, width and height, not x0, y0, x1, y1
             # Do this in getBBox() as an option.
            
            vv = getBBox(tmp[isShape], diffs = TRUE)
            bb2[isShape,] = vv

        }
        # bb is for rect/line.  So we are looking for lines that span at least half the page
        # and are further "down" the page than our x node (which is located at bb2[1,])
        # Was > .6 not .53
        w = (bb[,3] - bb[,1])/as.numeric(xmlGetAttr(page, "width")) > .53 & bb[,2] > bb2[1,2]

        if(any(w)) {
            bot = max(bb[w, 4])
            f = function(x) {
                            bb.n = getBBox2(x)
                            x[ bb.n[,2] + bb.n[,4] <= bot ]
                        }
            #browser()
            # Really it is if nodes is a list with all elements being XMLInternalElementNode
            # or
            
            nodes = if(length(nodes) != length(cols)) f(unlist(nodes)) else lapply(nodes, f) 
        } else if(!is.null(original.to)) {
            # This is different from w above. This is the which rect/line in bb and then
            # append which lines are below the first node.
            tmp2 = c((bb[,3] - bb[,1])/as.numeric(xmlGetAttr(page, "width")) > .53, bb[,2] > bb2[1,2])        
       #     if(any(tmp2))
       #        stop("check the threshold")
            #XXX  finish this off.
        }
    } 

    unlist(nodes, recursive = FALSE)
}


getLastNode =
    # get the final node in the document - last node in last page
    # Use this when getting the content for the last section
function(node, doc = as(node, "XMLInternalDocument"))
{
    #  getNodeSet(doc, "//text[last() and ancestor::page]")[[1]]    <- returns bad results
    ans = getNodeSet(doc, "//page[last()]/text[last()]")[[1]]  # above caused errors so I  put this back in. Whay was it replaced?
    if(pageOf(ans) == pageOf(node)) {
        # if on the same page, then we need to check which column node is in
        # and ensure that the ans node is in the same column.
        page = xmlParent(node)
        byCol = getTextByCols(page, asNodes = TRUE)
        w = inColumn(node, byCol)
        ans = byCol[[w]][[ length(byCol[[w]]) ]]
    }
    ans
}

getFirstTextNode =
    # get the final node in the document - last node in last page
    # Use this when getting the content for the last section
function(doc)
{
  getNodeSet(doc, "//page[1]/text[1]")[[1]]
}


findShortSectionHeaders =
function(colNodes, lines = nodesByLine(colNodes))
{
    short = which(findShortLines(colNodes, lines, asLogical = TRUE))

    # Now check if there is a little but more space between this line
    # relative to the others and/or is it in a different font/color
    
    tops = sapply(lines, function(x) min(as.numeric(sapply(x, xmlGetAttr, "top"))))
    lineskip = median(diff(sort(tops)))

    before = diff(tops)[short - 1]

    isShort = short[ before > lineskip * 1.1]
    lines[ isShort ]

    # Check for fonts here or in a separate function.
}


orderNodes =
    #
    # Take a list of nodes and order them by page and within each page by column
    # We'll assume they are ordered correctly within column already.
    #
function(nodes, pages = sapply(nodes, pageOf))
{
  unlist(tapply(nodes, pages, orderNodesInPage))
}

orderNodesInPage =
function(nodes, columnNum = sapply(nodes, inColumn, colNodes),
         colNodes = getTextByCols(page, breaks = colPos, asNodes = TRUE),
         colPos = getColPositions(if(colsAcrossPages) as(nodes[[1]], "XMLInternalDocument") else xmlParent(nodes[[1]]), acrossPages = colsAcrossPages),
         page = xmlParent(nodes[[1]]),
         colsAcrossPages = any(grepl("References", sapply(nodes, xmlValue))))
{
    # If this page includes a References section but the number of columns is 1 and the number of
    # columns on the previous page is 2, then use the previous page's columns. 
    if(colsAcrossPages && length(colPos) == 1 && length(x <- getColPositions(getSibling(page, FALSE))))
        colPos = x
    
    nodes[order(columnNum)]
}


removeNumPrefixes =
  #  removeNumPrefixes(c("1.2 abc", "  1.2 abc def", "1.x abc", " abc def") )
function(x)
{
  gsub("^[[:space:]]*[0-9.]+ ?", "", x)
}



getLastRealTextNode =
function(doc, docFont = getDocFont(doc), nodes = getNodeSet(doc, xpathQ("//text", doc)), textFonts = getTextFonts(doc, txtNodes = nodes))
{
    # Make this smarter by finding the text that comes after the main text of the
    # document. Acknowledgements, etc.
    # We are currently calling this because we have no section headers.
    # So we may look at text that is different from the regular document font.
    # and/or look for
#browser()
    byPage = split(nodes, sapply(nodes, pageOf))
    tmp = byPage[[ length(byPage) ]]
    cols = getTextByCols(doc[[ length(byPage) ]], txtNodes = tmp, asNodes = TRUE)
    tmp = cols[[length(cols)]]
    tmp = orderByLine(tmp)

    ans = tmp[[length(tmp)]]
    ans

    
#    ans[[length(ans)]]
#    bb = getBBox2(tmp)
#    o = order(bb[, "top"], bb[, "left"])
#    tmp = tmp[o]
#    tmp[[ length(tmp) ]]
    #  textFonts[[length(textFonts)]]
    
}
################################################################################
# gapFuns.R
groupLines =
    ##
    ##
    ## "LatestDocs/PDF/3475635737/Nakgoi-2014-Dengue,%20Japanese%20Encephalitis%20and.xml" - lineskip 13
function(ll, lineskip = 16) # 18 didn't separate the abstract from columns in Mackenzie-2001.pdf
{
    bbs = lapply(ll, getBBox2, asDataFrame = TRUE)
    tops = sapply(bbs, function(x) median(x$top))
    d = diff(tops)

    if(is.na(lineskip)) {
        tt = table(d)
        lineskip = as.integer(names(tt)[which.max(tt)])
    }
    
    grps = cumsum(c(0, d) > 2*lineskip)
    blocks = split(ll, grps)
    names(blocks) = seq(along = blocks)
    blocks
}


pageNodesByLine =
function(page)
{        
#doc = readPDFXML("NewPDFs/Dobrava Virus/Scharninghausen-1999.xml")
#    bb = getBBox(doc[[1]])
    tt = getNodeSet(page, ".//text")
    ll = nodesByLine(tt)
}


spansColumn =
function(nodes, gap, bbox = getBBox2(nodes, asDataFrame = TRUE), threshold = 20)
{
   start = bbox$left
   end = bbox$left +  bbox$width

   w = start < gap & end > gap
   
#   w = abs(gap - start) < threshold | abs(end - gap) < threshold
   any(w) # table(w)
}


getNodePos =
function(nodes, bbox = getBBox2(nodes, asDataFrame = TRUE))
{
   data.frame(start = bbox$left, end = bbox$left + bbox$width)
}





hasGap =
    #XXX Finish off.
function(nodes, col = getColPositions(xmlParent(nodes[[1]])), bbox = getBBox2(nodes, asDataFrame = TRUE))
{
   start = bbox$left
   end = bbox$left +  bbox$width
   n = length(nodes)
   d = start[-1] - end[-n]
#   browser()
}


getPageGroups =
function(page, lineskip = 20)
{
    ll = pageNodesByLine(page)
    g = groupLines(ll, lineskip)
}

################################################################################
# getTitle.R

# The pdf in 0698940064/ of the Zoonotics end note
# There is a 43 point font. That is one character that starts the text and spans 2 lines of text.
#

getDocTitleString =
function(f, nodes = getDocTitle(f, ...), ...)
   paste(sapply(nodes, function(x) if(is.character(x)) x else xmlValue(x)), collapse = " ")

getDocTitle =
    #
    # We may want to determine if this is scanned, and if so, does it have a cover page.
    #
    #  Identify a journal name and discard this from the results (after the meta)
    # e.g. "LatestDocs/PDF/2143276081/Kamhieh-2006-Borna disease virus (BDV) infect1.xml" - Veterinary Quaterly.
    # We can hard code these, but let's try to learn from the headers and footers.
    # 
    #
    #  See 1599857215/Learned-2005-Extended interhuman transmission.xml for a title in the meta that is just the name of the file.
    #
function(file, page = 1, doc = readPDFXML(file), meta = FALSE, minWords = 1, asNode = FALSE, scanned = isScanned(doc), ...)
{
  if(missing(doc) && is(file, "XMLInternalDocument"))
      doc = file


  if(isOIEDoc(doc)) 
     return(getOIETitle(doc, asNode = asNode))



  if(meta) {
      meta = getNodeSet(doc, "//docinfo/META[@name = 'title']")
      if(length(meta)) {
          ti = xmlGetAttr(meta[[1]], "content")
             # The first grepl() detects the name of the file. Should compare with docName(doc)
             #  !grepl("PDF/[0-9]+/", ti)          
          if(!grepl("$$", ti, fixed = TRUE) && !grepl("Microsoft", ti) && !grepl("PDF/PDF", ti) && !isMetaTitleFilename(ti, docName(doc)) &&
               !grepl("^doi:", ti) && !isTitleBad(ti))
              return( ti )
      }
  }
  
  if(missing(page) && (isResearchGate(doc)  ||
                        length(getNodeSet(doc, "//page[1]//text[. = 'PLEASE SCROLL DOWN FOR ARTICLE']")) > 0 ))
      page = 2

    # For what documents is this necessary - e.g. Puzelli et al
    # But need to be more specific for this cover page as other docs, e.g., A case of Crimean-Congo Ham.... .xml doesn't
    # have the cover page but does have the www.euro...org link.
  if(missing(page) && length(getNodeSet(doc, "//page[1][./text[contains(., 'www.eurosurveillance.org')] and ./text[contains(., 'Weekly')] ]")))
      page = 2
  
     # handle case where the first page is a cover sheet
  p1 = getNodeSet(doc, "//page")[[page]]

  if(length(getNodeSet(p1, ".//text")) == 0)
      return(list())

  if(scanned) # Should we use isScanned() ?
      return(if(asNode) list() else NA_character_)
  

  
  fonts = getFontInfo(p1)
  if(is.null(fonts))
     return(NULL)
  m = which(fonts$size == max(fonts$size))
  mx = fonts$size[m]
  id = fonts[m, "id"]
  
  txt = getFontText(p1, id)

  if(all(nchar(sapply(txt, xmlValue, trim = TRUE)) == 1)) {
    # XXX Need to deal with the first letter in each word being different.
  }


#
# getFontText() should reassemble text across lines, etc. and return
# the elements that are separate.
#

  isElsevier = isElsevierDoc(doc)
  if(isElsevier && !isTitleBad(txt)) {
      return(splitElsevierTitle(txt, p1, asNode = asNode))
#     tt = names(txt)
#     if(grepl("Journal", tt[1]))
#         tt = tt[-1]
#     return(paste(tt, collapse = " "))
  }
  
  ctr = 1L

  if(!all(w <- isTitleBad(txt, if(length(txt) > 1) 0 else 3))) {
      # order them by line. They may be upside down.
      # See 1834853125/394.full.xml
      txt = orderByLine(txt[!w])
        # Make certain to get the rest of the text in the tile that may be in a different font.
        # See why we are doing this in 4214927259/A case of Crimean-Congo haemorrhagic fever in.xml
        # 
      tmp = mkLines(txt, p1)
      if(asNode) return(tmp)
      
      title = paste(sapply(tmp, function(x) paste(if(is.list(x)) sapply(x, xmlValue) else xmlValue(x), collapse = " ")), collapse = "\n")
      return(title)
#      return(paste(names(txt), collapse = " "))
  }
      
  while( (length(txt) == 1 && nchar(names(txt)) == 1) || all(w <- isTitleBad(txt, minWords = minWords))) {

      # then a single character that is very large
      # get second largest font and

      mx = max(fonts$size[fonts$size < mx ])
      w = (fonts$size == mx)
      id = fonts$id[w]
# if multiple ones, see if any are bold and restrict to that.
# Doesn't work for Van Der Poel-2005
      if(length(id) > 1 & any(isb <- isBold(fonts[w,]))) #fonts$isBold[w]))
          id = id[isb]

      if(length(id) == 0)
          break  #XXXX
         
      txt = getFontText(p1, id)
      ctr = ctr + 1L
  }

  orderByLine(txt)
}


fixTitleNodes =
    ## We can compute the locations of the resulting nodes (with getBBox2),
    ## sort them and compute the differences. Or order them by line
    ## and see how far apart they are, OR see if there are other nodes in between lines.
    ## 
    ## We should compute the line spacing for the entire page or document and compare to that.
    ## For now, we'll use a threshold of 3 which is 3 height of text in a line, so that allows double spacing.
function(nodes, threshold = 3)
{
    ll = nodesByLine(nodes)
    txt = sapply(ll, function(x) trim(paste(sapply(unlist(x), xmlValue), collapse = " ")))
    ll = ll [ txt != ""]

    bb = lapply(ll, getBBox2)
    h = median(getBBox2(nodes)[,"height"])
    top = sapply(bb, function(x) median(x[, "top"]))
     ##XXX This is broken!
    d = diff(c(top[1] - h, top))

    unlist(ll[ d < h*threshold ])
}   


isTitleBad =
function(txtNodes, minWords = 3, filename = "")
 UseMethod("isTitleBad")

isTitleBad.list = isTitleBad.XMLNodeSet =
function(txtNodes, minWords = 3, filename = "")
{
     # One document (  0628126814/Chochlakis-2010-Human%20anaplasmosis%20and%20anaplas.xml)
     # uses rotation as a form of italics. So we don't check for non-zero rotation, but > 16
  if(any( as.numeric(sapply(txtNodes, xmlGetAttr, "rotation", 0.)) > 16))
      return(TRUE)

    # Check to see if this is really a watermark, etc. at the extreme of  a page, e.g.
    # 3364361467/Majumder-2016-Utilizing Nontraditional Data So.xml

  pos = as.numeric(sapply(txtNodes, xmlGetAttr, "top"))
  names(pos) = sapply(txtNodes, xmlValue)
  
  opos = as.numeric(unlist(getNodeSet(xmlParent(txtNodes[[1]]), ".//text/@top")))
  names(opos) = xpathSApply(xmlParent(txtNodes[[1]]), ".//text", xmlValue)
  i = match(names(pos), names(opos))
#  w = pos <= max(opos[ - i ])
# how many text nodes are below the txtNodes.
  w = opos[-i] > min(pos)
  if(sum(w) < 5)
     return(TRUE)


  bb = getBBox2(txtNodes, TRUE)

  if(diff(range(bb$top)) > 2 * median(bb$height)) {
      ll = nodesByLine(txtNodes, bbox = bb)
      bad = sapply(names(ll), isTitleBad, minWords, filename)
      if(!all(bad)) 
         return(rep(bad, sapply(ll, length)))
  }
    
if(FALSE) {    
   w = sapply(txtNodes, isTitleBad, minWords, filename)
   if(!all(w))
       return(w)
}
       
# Maybe put these back in if the above doesn't discriminate well.   
   txt = paste(sapply(txtNodes, xmlValue), collapse = " ")
   isTitleBad(txt, minWords)
}

isTitleBad.XMLInternalNode =
function(txtNodes, minWords = 3, filename = "")
  isTitleBad(xmlValue(txtNodes), minWords)

isTitleBad.character =
function(txtNodes, minWords = 3, filename = "", lowerCase = TRUE)
{
#  txtNodes = tolower(txtNodes)
  txtNodes = XML:::trim(txtNodes)

  
  #Test:
   grepl("^Review$", txtNodes)  ||
  grepl("^viruses$", txtNodes)  ||        
  grepl("Acta Veterinaria", txtNodes) ||
  grepl("BMC Infectious Diseases", txtNodes) ||
  nchar(txtNodes) < 6 ||   
  grepl("Letters? to the Editor", txtNodes, ignore.case = lowerCase) || grepl("^BRIEF COMMUNICATIONS$", txtNodes) ||
  grepl("^[-0-9, ]+$", txtNodes) ||          
  grepl("^Letters$", txtNodes) || grepl("table of contents", txtNodes) || grepl("^[A-Za-z]+ +\\bJournal$", txtNodes) ||
  txtNodes %in% c("DISPATCHES", "Research Paper", "KEYWORDS", "ScienceDirect", "Occasional Papers", "HHS Public Access", "Newsdesk", "Case Report", "LETTERS", "No Job Name", "NIH Public Access", "Special Report  Rapport spÃ©cial", "W J C C") ||
#^^^^ non-ASCII
      grepl("Rapid Communications|Research Articles|Weekly issue|Vol\\.[0-9]+|Volume [0-9]+|ournal of|Science Journals", txtNodes) ||
  length(strsplit(txtNodes, " ")[[1]]) < minWords
}



#################

splitElsevierTitle =
    #
    # For an elsevier document, we want to find the
    #     heavy black line above the title
    # or  the large grey box at the top of the page that contains the name of the journal
    # e.g. Kelly-2008-NIH
    #  Lau-2007 has no grey box and no line.  So we have to do further checks on the shaded box  to see it is large enough.
    #
function(nodes, page, asNode = FALSE)
{
    y = as.numeric(sapply(nodes, xmlGetAttr, "top"))
    lines = getNodeSet(page, ".//line") #XX??  and .//rect
    lw = as.numeric(sapply(lines, xmlGetAttr, "lineWidth", 0))
    yl = 0

    if(any(lw > 10)) {
        i = which.max(lw)
           # Is the 
        yl = as.numeric(strsplit(xmlGetAttr(lines[[i]], "bbox"), ",")[[1]])[2] + lw[i]/2
    } else {

        r = getNodeSet(page, ".//rect[@fill.color != '0,0,0']")
        if(length(r) > 0) {
                # check the height of these rectangles to ensure they aren't just thin lines.
            bb = t(sapply(r, function(x) as.numeric(strsplit(xmlGetAttr(x, "bbox"), ",")[[1]])))
            i = which(abs(bb[,2] - bb[,4]) > 10)
            if(length(i)) {
               left = min(as.numeric(unlist(getNodeSet(page, "./text/@left"))))
               if(bb[i[1],1] - left > 20)
                   yl = bb[i,2][1]
            }
        }
        
#           # get the two images.
#        img.dims = xpathSApply(page, ".//img", function(x) as.numeric(xmlAttrs[c("y", "width", "height")]))
        
    }
       # if none of the nodes are above the line, don't filter. See 0809541268/Kitajima-2009-First%20detection%20of%20genotype%203%20he.xml"
    if(any(y > yl))
        nodes = nodes[y > yl]

    if(asNode)
       return(nodes)
    
    paste(names(nodes), collapse = " ")

}


notWord =
function(x)
{
  grep
}



orderByLine =
function(nodes)
{
    o = order(as.numeric(sapply(nodes, xmlGetAttr, "top")))
    nodes[o]
}

isElsevierDoc =
function(doc)
{
    length(getNodeSet(doc, "//page[1]//text[ contains(lower-case(.), 'elsevier')]")) > 0
}

isResearchGate =
function(doc)
{
    length(getNodeSet(doc, "//text[contains(., 'www.researchgate.net')]")) > 0
}


isMetaTitleFilename =
function(ti, docName = NA)
{
  # return(grepl(ti, URLdecode(docName), fixed = TRUE))
  els = strsplit(ti, "/")[[1]]
  dels = strsplit(gsub("\\.xml$", "", URLdecode(docName)), "/")[[1]]
  return ( all (dels[seq(length(dels) - 1, length = 2)] == els[ seq(length(els) - 1, length = 2) ] ) )

          
 return (  gsub("\\.xml", "", basename(URLdecode(ti))) == els[ length(els) ] )  # ...
         
      
  length(gregexpr("/", ti)[[1]]) >= 3 && 
      gsub("\\.xml", "", basename(URLdecode(ti))) != basename(ti) 
}


###########################
#
#XXX
#
#  These functions look for text at approximately the same line and includes them.
#
# These are a little too liberal. They don't check the content is close to the original txt nodes.
#  So we can have text on one side of the page at the same "height" be connected to that on the right.
#  See 0007787963/Ko-2010-Prevalence of tick-borne encephalitis.xml
#
mkLines =
function(txt, page)
{
    pos = unique(as.numeric(sapply(txt, xmlGetAttr, "top")))
    h = max(as.numeric(sapply(txt, xmlGetAttr, "height")))    
    lapply(pos, mkLine, page, h)
}

mkLine =
  # getDocTitle("4214927259/A case of Crimean-Congo haemorrhagic fever in.xml")
function(pos, page, height)
{
# Could do this in XPath, but don't have abs() there.    
#   getNodeSet(page, sprintf("./text[@top = %f]", pos))
# So get the Bounding boxes for all text nodes.
#
    
    textNodes = getNodeSet(page, "./text")
    bb = getBBox2(textNodes)
              # added test that the height of any matching element is at least 70% of the original title node.
    w = abs(pos - bb[, "top"]) < .33*height & bb[, "height"]/height > .7
    textNodes[w]
}




isOIEDoc =
function(doc)      
{
    doc = as(doc, "PDFToXMLDoc")
    
    length(getNodeSet(doc, "//text[contains(., 'http://www.oie.int/wahis_2/')]")) > 0
}


getOIETitle =
function(doc, asNode = FALSE)
{
    doc = as(doc, "PDFToXMLDoc")
     # Find the image on page 1 and then the text that is not black that is to the right of the image"    
    p1 = doc[[1]]
    img = getNodeSet(p1, ".//img")[[1]]                                       #    bbi = getBBox2()
    atNames = c("x", "y", "width", "height")
    bbi = structure(as(xmlAttrs(img)[atNames], "numeric"), names = atNames)
    # not(@color = '#000000') and 
    xp = sprintf(".//text[@left > %f and @top > %f and @top < %f ]", bbi["x"] + bbi["width"], bbi["y"], bbi["y"] + bbi["height"])
    ans = getNodeSet(p1, xp)
    colors = getTextNodeColors(ans, doc = doc)

    ans = ans[colors != "#ababab"]

    if(asNode)
       ans
    else
       paste( sapply(ans, xmlValue), collapse = " ")

}


################################################################################
# headerFooter.R


getHeader =
function(doc)
{
    if(is.character(doc))
        doc = readPDFXML(doc)

    p1 = doc[[1]]
}

getPageFooter =
function(p,  bbox = getBBox2(getNodeSet(p, ".//text")), ignorePageNumber = TRUE)
{
    mx = max(bbox[, "top"], na.rm = TRUE)
    w = bbox[, "top"] == mx
    ans = rownames(bbox)[w][order(bbox[w, "left"])]

       # We have some docs with E57 as a page number (de la Torre-2009)
    if(length(ans) == 1 && (grepl("^[0-9]+$", ans) || grepl("Downloaded from", ans) || grepl("For +personal +use", ans) || (length(strsplit(ans, " +")[[1]]) == 1)))
        getPageFooter(, bbox[!w,])
    else
        paste(trim(ans), collapse = " ")
    
#    foot = bbox[ w , ]
     # Now combine the elements that are close
#    foot
}

getPageHeader =
    #' @param p an XML page node from the document
    #' @param bbox the
    #' @param nodes the list of XML nodes of interest in which to find possible header nodes. This allows us to filter the nodes, e.g., by font type/name/size, position.
    #' @lineThreshold integer vertical distance within which nodes are considered on the same line.
    #' interlineThreshold
function(p, bbox = getBBox2(nodes),
         nodes = getNodeSet(p, ".//text"),
         lineThreshold = 4, asNodes = FALSE,
         interlineThreshold = min(getDocFont(p)$size) * 2)
{
    mn = min(bbox[, "top"], na.rm = TRUE)
    w = bbox[, "top"] - mn <= lineThreshold
    #XXX Now check it is actually a header.
    # Find how far the nodes are from the other nodes not within the threshold
    # If this is sufficiently large (relative to the size of the text), then this is
    # a header.
    delta = min(bbox[!w, "top"] - mn)
    if(delta < interlineThreshold)
       return(if(asNodes) list() else character())
        
    if(asNodes)
       nodes[w]
    else
      rownames(bbox)[w]
}


lineSpacing =
function(doc)
{
   textNodes = getNodeSet(doc, "//text")
   bb = getBBox2(textNodes)
   
}


getFooterPos =
function(page, docFont = getDocFont(page), fontInfo = getFontInfo(page))
{
    ll = getNodeSet(page, ".//rect | .//line")
    if(length(ll)) {
        bb = getBBox(ll)
        bottom = max(bb[, "y0"])
        # look for a line with all the text below it being smaller than the the document font.
        nodes = getNodeSet(page, sprintf(".//text[ @top > %f]", bottom))
        if(length(nodes)) {
            fontId = sapply(nodes, xmlGetAttr, "font")
            if(all(fontInfo[fontId, "size"] < docFont$size))
                return(bottom)
        }
    }
    
    NA
}
################################################################################
# images.R


getTables =
function(doc)
{
    if(is.character(doc))
        doc = readPDFXML(doc)

    getNodeSet(doc, "//img")
}


getCaption =
function(node)
{

}
################################################################################
# tables.R

# Look at Buckley-2003  Rectangle around the table 1. Are these lines
# or a rectangle?


# A collection of alternative terms in a regular expression which we use
# to discard matches that are not the actual definition of a table
# but references to a table.
TableNodeRegex = c(
    "table[a-z]",
    "online .* table",
    "table of contents",
    "(using|also|and|with|for|in|from|to|by)( the)? +table",
    "table ([0-9]+ )?also", 
    "\\( ?table ([0-9]+)?\\)?",
    "cdc.gov/",
    "table ([0-9]+|I|II|III|IV|V|VI|VII|VIII|IX|X) *\\)",
    "see (online",
    "table)",
    "(tables",
    "supplementa(l|ry) table",
    "\\(available online", 
    "table [0-9]+\\.?\\))",
    "table ([0-9]+ +)?shows +th", 
    "[,;] table")

getTableNodes =
function(doc, drop = TRUE, useSiblings = c(FALSE, TRUE), dropHref = FALSE,
           rejectRegex = TableNodeRegex)
{
    if(is.character(doc))
        doc = readPDFXML(doc)
    
      # Some docs have T able as two separate text elements
    tt = getNodeSet(doc,
                     "//text[. = 'Table' or . = 'TABLE' or starts-with(., 'TABLE') or starts-with(., 'Table') or (. = 'T' and lower-case(following-sibling::text[1]) ='able') or contains(., ' Table')]")

    if(!drop)
       return(tt)

    txt = sapply(tt, getTextAround, useSiblings = rep(useSiblings, length = 2))
#    rx = "table[a-z]|online .* table|table of contents|(also|and|for|in|from)( the)? table|table ([0-9]+ )?also|\\( ?table ([0-9]+)?\\)?|table [0-9]+ *\\)|see (online|table)|(tables|supplementa(l|ry) table|\\(available online|in +table|table [0-9]+\\.?\\))|table ([0-9]+ +)?shows +th|[,;] table"

    rx = paste(rejectRegex, collapse = "|")
    w = grepl(rx, txt, ignore.case = TRUE) 
    if(dropHref) {
        hasHref = sapply(tt, function(x) "a" %in% names(x))
        w = w | hasHref
    }
#   browser()    
    tt[ !w ]
}

getTextAround =
function(x, useSiblings = c(TRUE, TRUE))
{
    v = c(if(useSiblings[1]) xmlValue(getSibling(x, FALSE)) else character(),
            xmlValue(x),
          if(useSiblings[2]) xmlValue(getSibling(x)))
    paste(v, collapse = " ")
}

getTables =
function(doc, tableNodes = getTableNodes(doc), ...)
{
    if(is.character(doc))
        doc = readPDFXML(doc)
    
    # Discard tables Table S1 (etc.) and if it is in the "section" named 'Supporting Online Material'
    # This doesn't show up as an actual section header, so we just look for it.  But it has to be on the same
    # page as the Table S text node so that we don't pick one up from another article.
    # We could be stricter that it has to be within a few lines of the Supporting ... and in the same column.
    # See Barrette-2009-Discovery...
    label = sapply(tableNodes, xmlValue)
    w = grepl("Table S[0-9]+", label)
    if(any(w))
       w[w] = z = sapply(tableNodes[w], function(x) length(getNodeSet(x, sprintf("./preceding::text[contains(., 'Supporting Online Material') and ../@number = %d]", pageOf(x)))) > 0)

    # Table( ?[0-9])
    w[!w] = grepl("\\(.*(online|Appendix)|(see|in) Table|Table [0-9] and\\)", label[!w])
    tableNodes = tableNodes[!w]
    

      # Now find those that are in text and not part of a separate block
    tbls = lapply(tableNodes, findTable, ...)
    names(tbls) = sapply(tableNodes, xmlValue)
    tbls
}

getRotation =
function(node)
{
    if(xmlName(node) != "page")
        node = pageOf(node, TRUE)

    xmlGetAttr(node, "rotation", 0, as.numeric)
}

findTable =
function(node, page = xmlParent(node),
         colNodes = getTextByCols(page, asNodes = TRUE, perPage = perPage, breaks = colPos), # docFont = docFont), # 
         docFont = getDocFont(node),
         perPage = TRUE,
         spansWithin = 20,
         rotated = !(getRotation(page) %in% c(0, 180)),
         colPos = getColPositions(page, perPage = perPage, docFont = docFont),         
         ...)
{
#if(pageOf(page) == 4) browser()
#browser()

    if(rotated)
        return(getRotatedTable(node, pageRotated = TRUE))

     # check if page is regular but most of the text is rotated.
    rot = table(unlist(getNodeSet(page, ".//text/@rotation")))
    if(!(as.numeric(names(rot)[which.max(rot)]) %in% c(0, 180)))
       return(getRotatedTable(node, pageRotated = FALSE, textRotated = TRUE))

      #XXXX if the table dominates the col positions, recompute with perPage = FALSE, docFont = TRUE to discard the table.
    # See Mehla-2009
# getTextByCols() uses docFont = FALSE , perPage = TRUE and gets 76 and 220 for the breaks.    
    colPos2 = getColPositions(page, perPage, docFont = TRUE)

    
    if(!perPage && length(colPos2) < 2)
        colNodes = getTextByCols(page, asNodes = TRUE, perPage = TRUE)
    
    colNum = inColumn(node, colNodes)
    centered = isCentered(node, colNodes)

    if(!centered) {
          # Check if the Table node is centered in the page since not the column
        pwidth = xmlGetAttr(page, "width",, as.integer)
        nwidth = xmlGetAttr(node, "width",, as.integer)
        nx = xmlGetAttr(node, "left",, as.integer)                
        if(abs( (nx + nwidth - pwidth)/2) < .1*pwidth) 
            centered = 2
    }


      # also look at rectangles.  J Infect Dis. 2015 has no lines, just rect.
    lines = getNodeSet(page, ".//line | .//rect")


    if(length(lines) == 0 && !is.null(node[["a"]]))
        return(list())
#browser()    
    lw = as.numeric(sapply(lines, xmlGetAttr, "lineWidth", 1))
    lines = lines[ lw >= 0 & lw < 30]
    bb = getBBox(lines, TRUE)
      # discard rectangles that are probably too tall to be lines, i.e. span more than half a letter.
      #XXXX 
    bb = bb[ abs(bb$y1 - bb$y0) < docFont$size * .5, ]
    
    nodeTop = as.numeric(xmlGetAttr(node, "top"))
       # recall we are going from top to bottom so these are below the node.
    bb = bb[pmin(bb$y0, bb$y1) >= nodeTop, ]

    if(nrow(bb) == 0)
       return(list())

    #XXX one of these is redundant, or they need to be merged.

    bb = combineBBoxLines(bb)    
    bb = mergeLines(bb)
    
    
#    doesSpan = rep(FALSE, nrow(bb))    
    if(centered == 1 || (colNum == length(colNodes))) {
       # Could span all columns.
      colLines = nodesByLine(colNodes[[colNum]])
      le = getLineEnds(colLines)
      ex = apply(le, 2, median)

      # if column 1, then x1 of line has to be <= ex[2]
      # if column 2, then x0 >= ex[1]

      if(colNum == 1) 
         bb = bb[bb$x1 <= ex[2]*1.15, ] # 1.075
      else
         bb = bb[bb$x0 >= ex[1]*.925, ]

      doesSpan = spansWidth(bb, ex, spansWithin)
      spansCols = colNum

      if(!any(doesSpan)) {
          # See if there are any text nodes to the right

          # get the widest lines
          m = max(bb$x1 - bb$x0)
          i = (bb$x1 - bb$x0 == m)
          wd = bb[i, ]
          right = max(wd$x1)
          ys = max(bb$y1)

          # tor = to right
          tor = sapply(unlist(colNodes),
                       function(x) {
                           b = getBBox2(list(x), TRUE, rotation = TRUE)
                           b$rotation == 0 & b$left > right & b$top > nodeTop & b$top < ys
                       })
          if(!any(tor)) {
              # nothing to the right
              doesSpan[i] = TRUE
          }
      }
    } else if(centered == 2 || centered == 0) {

        # For the columns, get the start and the end "margins"
        # by computing the start and end of each line and then computing
        # the 
 #        xpathSApply(page, ".//text")        
        # Get rid of any lines that are only within one column.
         colInfo = t(sapply(colNodes, function(x) {
            ll = nodesByLine(x)
            le = getLineEnds(ll)
            ex = mapply(function(i, q) quantile(le[,i], q),
                         1:2, c(.2, .75))
         }))
         ex = range(colInfo)

           # Which lines span the page.
           # same line as in earlier if() clause so should centralize. But may need it here now.
         doesSpan = spansWidth(bb, ex, spansWithin)

         # This may be a little cavalier and we may need to check.
         spansCols = seq(along = colNodes)
         
         if(!any(doesSpan)) {

             # If centered = 0 and no line spans more than one column, then
             # the table is in that column

             if(centered == 0) {
                 w = abs(bb[, "x0"] - colInfo[colNum,1]) < 10 & abs(bb[, "x1"] - colInfo[colNum,2]) < 10
                 doesSpan = w
                 spansCols = colNum
             }
             
             # Are there are text nodes to the right???  CHECK.
             # Example where the table doesn't span the entire page, but there is no text to the right of it.
             #  Table is on same page as image and there is nothing else so no text (other than figure caption).

#XX Fix this.  Far too specific to 2 columns.             
             if(length(colNodes) > 2) {
                 # do any of the lines span 2 or more contiguous columns
                doesSpan = spansWidth(bb, c(colInfo[1,1], colInfo[2, 2]), spansWithin)
                if(!any(doesSpan)) {
                    doesSpan = spansWidth(bb, c(colInfo[2,1], colInfo[3, 2]), spansWithin)
                    if(any(doesSpan))
                        spansCols = c(2, 3)
                    # If we don't define spansCols above, then we need to do it here as an else or else won't necessarily be defined.
                } else
                    spansCols = c(1,2)
             }
         }

         colLines = nodesByLine(unlist(colNodes[spansCols], recursive = FALSE))
         # colLines = nodesByLine(getNodeSet(page, ".//text"))
     }


    # Handle more than 2 columns
    # Then also find the lines that come after another table in the same column.
    # See Leroy-2004 - tables 2 and 3 in same column (1) (and spans 2 columns)

    
 
    # Only the lines that are close.
    # Aguilar-2007 has two tables in column 1 page 3 and we are merging
    # them both with all the text in between.
    spans =  bb[doesSpan,]
    if(nrow(spans) > 3) {
        # need the ones "close" to node.
        # under node but closest to it.
        # For, e.g. J Infect. Dist-2015, we have <rect> nodes and two of these are very close together. 866 and 868
        # So looking at the first 3 is too simplistic.
        # We need the next one which is the bottom of the table.
        # And there is another line across the page but that is the footer that is on each page - and only 5 units above the text of the footer.

        # so now group the "lines/rects" based on their y0 value into groups based on the document font size.
        # Take the min from each group.
        ii = seq(nodeTop, max(spans$y0, spans$y1) + docFont$size, by = docFont$size)
        tmp = unlist(tapply(spans$y0, cut(spans$y0, ii), min))
        tmp = tmp[!is.na(tmp)]

        spans = spans[spans$y0 %in% tmp, ]
        
        spans = spans[ order(spans[,"y0"])[1:3], ]
    }

    # we should now have the start, header and footer lines.    

    # Perhaps use getNodesBetween(). But no need.
    # But for 3 columns, maybe we need to be using that to avoid repeating all the code.
    b = max(spans[,2]) #XXX if spans is empty, what value should we return. The height of the page? or -Inf?
    ok = sapply(colLines, function(x) {
                            tp = as.numeric(xmlGetAttr(x[[1]], "top"))
                            tp <= b & tp >= nodeTop
                     })

    colLines[ok]
    # Find any text associated with the table as foonotes
}

spansWidth =
    #
    # Make if within < 1, treat it as a percentage and that the span
    # has to be be at least within % of the width of locs.
    #
    #  spansWidth(matrix(c(2, 0, 8, 0,
    #                      3, 0, 8, 0,
    #                      3, 0, 7, 0), , 4, byrow = TRUE), c(0, 10), .8)
    #
    # To use  a within < 1 as an actual distance and not a multiple use, e.g., I(.6)
    #
function(bbox, locs, within = 4) # within was 2 but somewhat arbitrary. Needed 4 for Padula-2002
{
    if(within < 1 && !is(within, "AsIs"))
       bbox[,3] - bbox[,1] >=  diff(locs)*within
    else
      abs(locs[1] - bbox[,1]) < within & abs(locs[2] - bbox[,3]) < within
}





nodesToTable =
function(nodes, colPos = getColPositions.PDFToXMLPage( txtNodes = unlist(nodes)), bind = TRUE)
{
    if(length(nodes) == 0)
        return(NULL)
    
    if(length(colPos) == 0) {
        ll = nodesByLine(unlist(nodes))
    }
    
    rows = lapply(nodes, function(x) getTextByCols( txtNodes = x, breaks = colPos))
    if(bind) 
        as.data.frame(unname( do.call(rbind, rows) ), stringsAsFactors = FALSE)
    else
        rows
}


getGap =
    # nodes organized by lines.
function(nodes, bbox = getBBox2(nodes))
{
    r = bbox[,1] + bbox[, 3]
    r[-1] - bbox[-length(r), 1]
}


getRotatedTable =
function(node, page = pageOf(node, TRUE), nodes = getNodeSet(page, ".//text"), bbox = getBBox2(nodes, asDataFrame = TRUE),
         pageRotated =  FALSE, textRotated = NA, asNodes = TRUE)
{
    if(!pageRotated)
        colPos = getColPositions(page, txtNodes = nodes, structure(bbox, names = c("top", "left", "height", "width", "text")), threshold = .05)
    else
        colPos = getColPositions(page, bbox = bbox)
#XXX ROTATE  - WONG    
    cols = getTextByCols(page, txtNodes = nodes, bbox = bbox, breaks = colPos, asNodes = TRUE)

    if(pageRotated) {
        # Take out text that is rotated the same amount as the page's rotation
        # e.g. Wekesa, although that is odd. That rotates the page 90, then rotates the text 180 and rotates
        # the header for the page 90.  So this is only done here if the page is rotated.
        prot = as.numeric(xmlGetAttr(page, "rotation"))
        cols = lapply(cols, function(x) x[ as.numeric(sapply(x, xmlGetAttr, "rotation")) != prot ])
        cols = cols[ sapply(cols, length) > 0 ]
    }
    
    v = lapply(cols, nodesByLine, asNodes = asNodes, rotate = TRUE)
    class(v) = "RotatedTableColumns"
    v
}



getRotatedDownloadNodes =
function(doc)
{
    nodes = getNodeSet(doc, "//text[@rotation = 90 and starts-with(., 'Downloaded from')]")
    if(length(nodes)) {
        unlist(lapply(nodes, function(x) {
                         l = xmlGetAttr(x, "left")
                         getNodeSet(xmlParent(x), sprintf(".//text[@rotation = 90 and @left >= %s]", l))
                      }))
    } else
        NULL
}


mergeLines =
function(df, y = "y0")
{
    h = split(df, df[[y]])
    ll = lapply(h, collapseLine)
    do.call(rbind, ll)
}

collapseLine =
function(x, gap = 5)
{
    d = x$x0[-1] - x$x1[-nrow(x)]
    do.call(rbind, by(x, cumsum(c(0, !(d < gap))), function(x) data.frame(x0 = min(x$x0), y0 = min(x$y0), x1 = max(x$x1), y1 = max(x$y1))))
}


showNodes =
function(nodes, showCircle = TRUE, text = sapply(nodes, xmlValue), ...)
{
    if(length(nodes) == 0)
        return(NULL)
    
    pages = sapply(nodes, pageOf, TRUE)
    pg = unique(pages)
    if(length(pg) > 1) {
        opar = par(no.readonly = TRUE)
        on.exit(par(opar))
        par(mfrow = c(1, length(pg)))
    }
    po = sapply(nodes, pageOf)
    invisible(mapply(function(tt, page, text, ...) {
               plot(page)
               showNode(tt, page, text = text, showCircle = showCircle, ...)
           }, split(nodes, po), pg, split(text, po)))
}

showNode =
function(node, page = pageOf(node, TRUE), showCircle = TRUE, text = sapply(node, xmlValue), ...)    
{
    #XX Deal with rotation.
    #XX Deal with line and rect nodes
    
    isText = (sapply(node, xmlName) == "text")
    if(length(unique(isText)) > 1) {
        showNode(node[isText], page, showCircle, text[isText])
        showNode(node[!isText], page, FALSE, text[!isText])
        invisible(return(NULL))
    }
    
    h = dim(page)["height"]
                 
    bb = if(any(isText)) getBBox2(node) else getBBox(node)

    if(any(isText)) {
        x = bb[,1] + bb[,3]/2
        y = h - (bb[,2] + bb[,4]/2)
    } else {
        x = (bb[,1] + bb[,3])/2
        y = h - (bb[,2] + bb[,4])/2        
    }
    
    if(length(text))
       text(x, y, text, col = "red", cex = 2)
    
    if(showCircle) {
       symbols(x, y, circles = rep(mean(bb[,4])*2, length(x)), fg = "red", lwd = 2, add = TRUE)
    }
}


showTb =
function(file, dropHref = FALSE, ...)
{
    tt = getTableNodes(file, dropHref = dropHref)
    showNodes(tt, ...)
    tt
}
