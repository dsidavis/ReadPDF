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
