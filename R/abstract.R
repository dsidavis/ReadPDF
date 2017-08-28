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
    a = getNodeSet(page, ".//text[lower-case(normalize-space(.)) = 'abstract' or lower-case(normalize-space(.)) = 'summary']")
browser()
    if(length(a) == 0) {
        # See if there are two columns and a line across one column that 
        # separates some text from  regular document text
        # This is the title, author list, abstract, line and then regular text.
        # See Waldenstrom-2007, etc.
        cols = getColPositions(page)
        colNodes = getTextByCols(page, asNodes = TRUE)
        docFont = getDocFont(doc)
        if(length(cols) > 1) {
            lines = getBBox(getNodeSet(page, ".//line | .//rect"))
              # only lines that span the first column
            w = lines[,1] - cols[1] < 5 & lines[,3] < cols[2]
#            browser()
            if(any(w))
                w = which(w)[1]
            nodes = getNodeSet(page, sprintf(".//text[@top + @height < %f and @left < %d]", lines[w, 2], cols[2]))

            # XXX Might possibly want them all to be contiguous since in Wessenbrock, we include the word DISPATCHES.
            # So we can find all the nodes with smallest font and cumsum() them and take the last block
            # Need to order them by line/@top            
            # Should order from top to bottom in order to discard
            #   bb = getBBox2(nodes)
            #   nodes = nodes[order(bb[, 2])]
            fontInfo = getFontInfo(doc)
            fonts = sapply(nodes, xmlGetAttr, "font")
            i = fontInfo[fonts, "size"] == min(fontInfo[fonts, "size"])
            nodes = nodes[i]            
#browser()            

        }
    } else {

        mar = margins(page)
        bb = getBBox2(a)
        if(bb[1,1] > mar[1] * 1.1) {
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
            nodes = getNodeSet(page, sprintf(".//text[abs(@left - %d) < 4]", bb[1,1]))
            bb2 = getBBox2(nodes)
            bot = max(bb2[, 2] + bb[,4])
            nodes = getNodeSet(page, sprintf(".//text[@left > %d - 4 and @top > %d and @top + @height <= %f + 9]", bb[1,1], bb[1,2], bot))       
        } else {
            # Flush with left margin

            kw = getNodeSet(page, ".//text[starts-with(., 'Key Words') or starts-with(., 'Keywords')]")
            if(length(kw))
                nodes = getNodesBetween(a[[1]], kw[[1]])
        }
    }
        
    nodesByLine(nodes)
}

getAbstractBySpan=
function(doc, col = getColPositions(doc[[1]]))
{
    if(length(col) == 1)
        return(NULL)

    
}




