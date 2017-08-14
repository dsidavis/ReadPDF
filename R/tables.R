# Look at Buckley-2003  Rectangle around the table 1. Are these lines
# or a rectangle?

getTables =
function(doc)
{
    if(is.character(doc))
        doc = readPDFXML(doc)

      # Some docs have T able as two separate text elements
    tableNodes = getNodeSet(doc,
        "//text[. = 'Table' or . = 'TABLE' or starts-with(., 'TABLE') or starts-with(., 'Table') or (. = 'T' and following-sibling::text[1] ='ABLE')]")

      # Now find those that are in text and not part of a separate block

#    tbls = lapply(tableNodes, findTable)
    
}

findTable =
function(node, page = xmlParent(node),
         colNodes = getTextByCols(page, asNodes = TRUE),
         docFont = getDocFont(node))
{
    colNum = inColumn(node, colNodes)
    centered = isCentered(node, colNodes)

browser()    
    if(!centered) {
          # Check if centered in the page since not the column
        pwidth = xmlGetAttr(page, "width",, as.integer)
        nwidth = xmlGetAttr(node, "width",, as.integer)
        nx = xmlGetAttr(node, "left",, as.integer)                
        if(abs((nx + nwidth) - pwidth/2) < .1*pwidth) 
            centered = 2
    }

      # also look at rectangles.  J Infect Dis. 2015 has no lines, just rect.
    lines = getNodeSet(page, ".//line | .//rect")
    bb = getBBox(lines, TRUE)
      # discard rectangles that are probably 
    bb = bb[ abs(bb$y1 - bb$y0) < docFont$size * .5, ]
    
    nodeTop = as.numeric(xmlGetAttr(node, "top"))
    bb = bb[bb$y0 >= nodeTop, ]
    
    doesSpan = rep(FALSE, nrow(bb))    
    if(centered == 1) {
       # Could span all columns.
      colLines = nodesByLine(colNodes[[colNum]])
      le = getLineEnds(colLines)
      ex = apply(le, 2, median)

      # if column 1, then x1 of line has to be <= ex[2]
      # if column 2, then x0 >= ex[1]
      colNum = inColumn(node, colNodes)

      if(colNum == 1) 
         bb = bb[bb$x1 <= ex[2]*1.05, ]
      else
         bb = bb[bb$x0 >= ex[1]*.95, ]

      doesSpan = apply(bb, 1, function(x) abs(ex[1] - x[1])  < 2 & abs(ex[2] - x[3]) < 2)
    } else if(centered == 2 || centered == 0) {

        # need the margins.
#        xpathSApply(page, ".//text")        
        # Get rid of any lines that are only within one column.
         colInfo = t(sapply(colNodes, function(x) {
            ll = nodesByLine(x)
            le = getLineEnds(ll)
            ex = mapply(function(i, q) quantile(le[,i], q),
                         1:2, c(.2, .75))
         }))
         ex = range(colInfo)
          # same as clause above so move out of both.
         doesSpan = apply(bb, 1, function(x) abs(ex[1] - x[1])  < 2 & abs(ex[2] - x[3]) < 2)
         if(!any(doesSpan)) {
             # Are there are text nodes to the right
         }
         colLines = nodesByLine(getNodeSet(page, ".//text"))
    }

 
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
#        if(length(tmp) > 3)
#           tmp = tmp[1:3]        
        spans = spans[spans$y0 %in% tmp, ]
        
        spans = spans[ order(spans[,"y0"])[1:3], ]
        # we should now have the start, header and footer lines.
    }

            # Perhaps use getNodesBetween(). But no need.
    b = max(spans[,2])
    ok = sapply(colLines, function(x) {
                            tp = as.numeric(xmlGetAttr(x[[1]], "top"))
                            tp <= b & tp >= nodeTop
                     })

    colLines[ok]
    # Find any text associated with the table as foonotes
}
