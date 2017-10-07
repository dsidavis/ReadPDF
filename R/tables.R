# Look at Buckley-2003  Rectangle around the table 1. Are these lines
# or a rectangle?

getTables =
function(doc, ...)
{
    if(is.character(doc))
        doc = readPDFXML(doc)

      # Some docs have T able as two separate text elements
    tableNodes = getNodeSet(doc,
        "//text[. = 'Table' or . = 'TABLE' or starts-with(., 'TABLE') or starts-with(., 'Table') or (. = 'T' and following-sibling::text[1] ='ABLE')]")

    # Discard tables Table S1 (etc.) and if it is in the "section" named 'Supporting Online Material'
    # This doesn't show up as an actual section header, so we just look for it.  But it has to be on the same
    # page as the Table S text node so that we don't pick one up from another article.
    # We could be stricter that it has to be within a few lines of the Supporting ... and in the same column.
    # See Barrette-2009-Discovery...
    label = sapply(tableNodes, xmlValue)
    w = grepl("Table S[0-9]+", label)
    if(any(w))
       w[w] = z = sapply(tableNodes[w], function(x) length(getNodeSet(x, sprintf("./preceding::text[contains(., 'Supporting Online Material') and ../@number = %d]", pageOf(x)))) > 0)

    w[!w] = grepl("\\(.*(online|Appendix)", label[!w])
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
         colNodes = getTextByCols(page, asNodes = TRUE, perPage = perPage), # docFont = docFont), # breaks = getColPositions(page, perPage = perPage, docFont = TRUE)),
         docFont = getDocFont(node),
         perPage = TRUE,
         spansWithin = 20,
         rotated = !(getRotation(page) %in% c(0, 180)),
         ...)
{
#if(pageOf(page) == 4) browser()
    #browser()

    if(rotated)
        return(getRotatedTable(node))

    
    if(!perPage && length(getColPositions(page, perPage)) < 2)
        colNodes = getTextByCols(page, asNodes = TRUE, perPage = TRUE)
    
    colNum = inColumn(node, colNodes)
    centered = isCentered(node, colNodes)

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
    lw = as.numeric(sapply(lines, xmlGetAttr, "lineWidth", 1))
    lines = lines[ lw >= 0 & lw < 30]
    bb = getBBox(lines, TRUE)
      # discard rectangles that are probably too tall to be lines, i.e. span more than half a letter.
      #XXXX 
    bb = bb[ abs(bb$y1 - bb$y0) < docFont$size * .5, ]
    
    nodeTop = as.numeric(xmlGetAttr(node, "top"))
       # recall we are going from top to bottom so these are below the node.
    bb = bb[pmin(bb$y0, bb$y1) >= nodeTop, ]

    bb = combineBBoxLines(bb)    

#browser()
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
          tor = sapply(unlist(colNodes),
                       function(x) {
                           b = getBBox2(list(x), TRUE)
                           b$left > right & b$top > nodeTop & b$top < ys
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
    browser()
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
function(node, page = pageOf(node, TRUE), nodes = getNodeSet(page, ".//text"), bbox = getBBox2(nodes, asDataFrame = TRUE))
{
    browser()
    colPos = getColPositions(page, bbox = bbox)
    cols = getTextByCols(page, txtNodes = nodes, bbox = bbox, breaks = colPos, asNodes = TRUE)
    v = lapply(cols, function(x) sapply(nodesByLine(x), function(x) paste(xmlValue(x), collapse = " ")))
    class(v) = "RotatedTableColumns"
    v
}
