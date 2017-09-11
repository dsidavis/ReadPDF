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
function(page, bbox = getBBox2(getNodeSet(page, ".//text[@rotation = 0]")))
{
   c(min(bbox[, 1]), max(bbox[,1] + bbox[,3]))
}

findAbstract =
function(doc, asNodes = TRUE, page = doc[[1]], byLine = TRUE)
{
    if(is.character(doc))
        doc = readPDFXML(doc)
    
    if(isBioOne(doc) && missing(page))
       page = doc[[2]]
    
    nodes = list()

      # Start by finding a declaration of an abstract: Abstract or Summary
    a = findAbstractDecl(page)
 browser()

    kw = findKeywordDecl(page)
    if(length(kw) == 0)
        kw = findKeywordDecl(getSibling(page))

    if(length(a) && length(kw)) {  #XXX Should check kw is below a
        nodes = getNodesBetween(a[[1]], kw[[1]])
        nodes = cleanAbstract(nodes)        
        if(byLine) {
            nodes = nodesByLine(nodes)
            if(!asNodes)
               return(names(nodes))
        } 
        return(nodes)
    }


    rect = getNodeSet(page, ".//rect")
    rect.bb = getBBox(rect)

       # If no declaration
    if(length(a) == 0) {
        # From Emerging Infectious Disease format.
        # See if there are two columns and a line across one column that 
        # separates some text from  regular document text
        # This is the title, author list, abstract, line and then regular text.
        # See Waldenstrom-2007, etc.
        cols = getColPositions(page, docFont = FALSE)
        colNodes = getTextByCols(page, asNodes = TRUE, breaks = cols)
        docFont = getDocFont(doc)
        if(length(cols) > 1) {
            lines = getBBox(getNodeSet(page, ".//line | .//rect"))
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
            } else {
                nodes = spansColumns(page, doc = doc)
            }
        }
    }

    nodes = cleanAbstract(nodes)
    
    if(byLine) {
        nodes = nodesByLine(nodes)
        if(!asNodes)
           return(names(nodes))
    }
    
    nodes
}

cleanAbstract =
function(nodes)    
{
   txt = sapply(nodes, xmlValue)
   nodes[ ! grepl("(19|20)[0-9]{2}.*elsevier|rights reserved|copyright" , tolower(txt)) ]
}

findKeywordDecl =
    # Find Key Words, Keywords, etc. 
    # Could add test for bold font.
function(page)
{
    getNodeSet(page, ".//text[starts-with(., 'Key Words') or starts-with(., 'Keywords') or starts-with(., 'Key words') or starts-with(., 'Keyword Index')]")    
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
function(page, cols = getColPositions(page, docFont = FALSE), colNodes = getTextByCols(page, asNodes = TRUE, breaks = cols),
            doc = as(page, "XMLInternalDocument") )
{
    #
    # Find all the lines that span multiple columns.  This includes title, authors, and abstract and possibly centered
    #  section headers.
    # Find the ones that don't have a large gap between the text segments,
    #                    start in the first column
    # Within these lines, find the subset that are consecutive, left aligned (allowing for a small indentation) and span 
    # 

    # Get all the lines of text, across all columns
    ll = nodesByLine(getNodeSet(page, ".//text"))
    # get the left and right of each line 
    r = t(sapply(ll, function(x) { bb = getBBox2(x); c(start = min(bb[,1] ), end = max(bb[,1] + bb[,3]))}))
    #
    mdiff = sapply(ll, function(x) max(gapBetweenSegments(x)))
    w = r[,1] < cols[length(cols)] & r[,2] > cols[length(cols)] & mdiff < .8*median(mdiff)
    if(!any(w))
       return(list())
    
    idx = split((1:length(ll))[w], cumsum(!w)[w])
    # Pick the block with the most text. XXX  Need to make this more robust
browser()    
    b = idx[[which.max(sapply(idx, length))]]
      # XXX add an extra line after this as the final line may not span the entire width to be beyond the start of the last column.
    unlist( ll[ c(b, max(b) + 1) ] )
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
