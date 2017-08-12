getTables =
function(doc)
{
    if(is.character(doc))
        doc = readPDFXML(doc)

      # Some docs have T able as two separate text elements
    tableNodes = getNodeSet(doc, "//text[. = 'Table' or . = 'TABLE' or (. = 'T' and following-sibling::text[1] ='ABLE')]")

      # Now find those that are in text and not part of a separate block

    tbls = lapply(tableNodes, findTable)
    
}

findTable =
function(node, page = xmlParent(node), colNodes = getTextByCols(page, asNodes = TRUE))
{
    colNum = inColumn(node, colNodes)
    centered = isCentered(node, colNodes)
browser()                 
    if(!centered) {
          # Check if centered in the page since not the column
        pwidth = xmlGetAttr(page, "width",, as.integer)
        nwidth = xmlGetAttr(node, "width",, as.integer)        
        if(abs(nwidth - pwidth/2) < .1*pwidth) 
            centered = 2
    }

    lines = getNodeSet(page, ".//line")
    bb = getBBox(lines, TRUE)
    doesSpan = rep(FALSE, nrow(bb))
    

    nodeTop = as.numeric(xmlGetAttr(node, "top"))
    bb = bb[bb$y0 >= nodeTop, ]    
    
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
    } else if(centered == 2) {


    }

 
    # Only the lines that are close.
    # Aguilar-2007 has two tables in column 1 page 3 and we are merging
    # them both with all the text in between.
    spans =  bb[doesSpan,]
    if(nrow(spans) > 3) {

        # need the ones "close" to node.
        # under node but closest to it.

       spans = spans[ order(spans[,"y0"])[1:3], ]
        # we have the start, header and footer lines.
    }
 

# Perhaps use getNodesBetween(). But no need.

    b = max(spans[,2])
    ok = sapply(colLines, function(x) {
                            tp = as.integer(xmlGetAttr(x[[1]], "top"))
                            tp <= b & tp >= nodeTop
                     })

    colLines[ok]


    # Find any text associated with the table as foonotes

}



