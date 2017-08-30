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
function(doc, asNodes = TRUE,  page = doc[[1]])
{
    if(is.character(doc))
        doc = readPDFXML(doc)


    nodes = list()
    a = getNodeSet(page, ".//text[starts-with(lower-case(normalize-space(.)), 'abstract') or starts-with(lower-case(normalize-space(.)), 'summary')]")
browser()
    if(length(a) == 0) {
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
                }  else
                    nodes = spansColumns(page, cols, colNodes, doc)

                if(length(nodes) == 0)
                    # try within single column, but w/o line.
                   stop("try within single column")
            } else {
                   # See if there are lines that span columns
                nodes = spansColumns(page, cols, colNodes, doc)
            }
        }
    } else {

        mar = margins(page)
        bb = getBBox2(a)

        # This test may be more appropriate as is there any text within a few lines that is
        # significantly to the left of a[[1]].  This is for the Elsevier documents
        # such as Oliveira-2009
        if(anyTextToLeft(a[[1]], bb)) {  # Used to be simply if(bb[1,1] > mar[1] * 1.1)
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
            nodes = getNodeSet(page, sprintf(".//text[abs(@left - %d) < 14]", bb[1,1]))
            bb2 = getBBox2(nodes)
            bot = max(bb2[, 2] + bb[,4])
            nodes = getNodeSet(page, sprintf(".//text[abs(@left - %d) < 14 and @top > %d and @top + @height <= %f + 9]", bb[1,1] - 4, bb[1,2] - 4, bot))       
        } else {
            # Flush with left margin

            kw = getNodeSet(page, ".//text[starts-with(., 'Key Words') or starts-with(., 'Keywords') or starts-with(., 'Key words')]")
            if(length(kw))
                nodes = getNodesBetween(a[[1]], kw[[1]])
            else
               nodes = spansColumns(page, doc = doc)
        }
    }
        
    nodesByLine(nodes)
}

anyTextToLeft =
function(node, bbox = getBBox2(list(node)), page = xmlParent(node))
{
  length(getNodeSet(page, sprintf(".//text[ abs(@top - %f) < 30 and @left <=  %f]", bbox[1,2], bbox[1,1]*.66))) > 0
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
        browser()
        w = fonts == fonts[length(fonts)]
        nodes = nodes[w]
        # Now have to get all the nodes in this region, not just the ones
        # with these fonts as that drops, e.g., italics.
        nodes = getNodesBetween(nodes[[1]], nodes[[length(nodes)]])
    }

    nodes
}
