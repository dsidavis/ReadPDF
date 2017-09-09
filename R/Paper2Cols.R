#3 column example /Users/duncan/DSIProjects/Zoonotics/NewData_Feb2017/Zoo_02_02_2017 Copy.Data/PDF/2586849323


if(FALSE) {
doc = xmlParsePDFTOHTML("2ColPaper.xml")

# Deal with the second page.
p = doc[[2]]
renderPage(p, cex = .5)

cols = getTextByCols(p)
nchar(cols)
}


##
# Todo
#[Done]  Deal with pages that have some text in a column that spans the entire width of the page
#   and then others in 2 or more columns, e.g., an abstract and then the 2 column text.
#
#
# Fix the fonts so that they are unique. Do this in pdftohtml

#
# Headers and footers on the pages.


pdfText = pdf_text =
function(doc, numPages = getNumPages(doc), docFont = getDocFont(doc))
{
   if(is.character(doc))
      doc = xmlParsePDFTOHTML(doc)
   
   lapply(getPages(doc)[seq(1, numPages)], getTextByCols, docFont = docFont)
}


isCenteredMargins =
function(node, margins = margins(xmlParent(node)), bbox = getBBox2(list(node)))
{
    mid = bbox[,1] + bbox[,3]/2
browser()    
    abs(mid - mean(margins)) < .2 * diff(margins)
}

isCentered =
    #
    # Determine if the node is centered within a column
    # This is for use in determining section titles
    # If we find a title that is centered and the find other text
    # with the same font but that is not centered, then that
    # additional text not a section title.
function(node, cols = getTextByCols(xmlParent(node), asNodes = TRUE),
         threshold = .2, colNum = inColumn(node, cols))
{
     # find out which column the node is in and get those columns and
     # their bounding boxes

  bb = getBBox2(cols[[ colNum[1] ]] )

     # This is now also done in nodesByLine and getLineEnds(). Leave
     # this here for now but eventually use those.
     # assemble the lines from these nodes and the horizontal bounding box for
     # each line and then the median position of each of these x1 x2
     # positions for the start and end of the line.
  byLine = by(bb, bb[, "top"], function(x) c(min(x[, "left"]),max(x[,"left"] + x[, "width"])))
  byLine2 = do.call(rbind, byLine)
  pos = apply(byLine2, 2, median)

      # and now the median middle of the lines
  mid = median(pos)

  top = xmlGetAttr(node, "top")
  lw = byLine[[ top ]]
    # lw could be NULL if we use docFont = TRUE when getting the nodes in getTextByCols().
  if(length(lw) && ((lw[1] - pos[1] < 5) || diff(pos) - diff(lw) < 40))
      return(FALSE)
  
     # now compute the middle of the string itself.
  textPos = as.numeric(xmlAttrs(node)[c("left", "width")])
  textMid = textPos[1] + textPos[2]/2
  
  textPos[1] - pos[1] > .1*diff(pos) & abs(textMid - mid) <  threshold *  median(byLine2[, 2])
}


nodesByLine =
    #
    # Group a collection of nodes in a column by lines, allowing them
    # to have a slightly different 
    #
function(nodes, asNodes = TRUE, bbox = getBBox2(nodes, TRUE),
         baseFont = getDocFont(as(nodes[[1]], "XMLInternalDocument")),
         fontSize = if(nrow(baseFont) > 0) baseFont$size else 11,
         addText = TRUE
        )
{
    if(length(nodes) == 1 && xmlName(nodes[[1]]) == "page")
        nodes = getNodeSet(nodes, ".//text")
    
    if(length(nodes) == 0)
       return(list())
    intv = seq(0, max(bbox$top)+ fontSize - 1, by = fontSize)
    topBins = cut(bbox$top, intv)
    byLine = tapply(nodes, topBins, arrangeLineNodes, asNodes, simplify = FALSE)

    names(byLine) = sapply(byLine, arrangeLineNodes, FALSE)
    byLine[ sapply(byLine, length) > 0]
}    

arrangeLineNodes =
    #
    # given the lines with the same top bin, arrange them from left to right.
    #
function(nodes, asNodes = TRUE)
{
    o = order(as.numeric(sapply(nodes, xmlGetAttr, "left")))
    if(asNodes)
        nodes[o]
    else
        paste(xmlValue(nodes[o]), collapse = " ")
}

getLineEnds =
    # Takes a list with each element a collection of nodes for that line.
    # Returns left and right end points.
function(lines)
{
   t(sapply(lines, function(x) {
                     b = getBBox2(x, TRUE)
                     c(min(b$left), max(b$left + b$width))
                  }))
}


#######
findShortLines =
    #
    # This finds lines that start at the left but are shorter than
    # other lines in the columns. This is one criterion that may identify
    # such a lines a section or sub-section header.
    # It is also the case for the final line in a paragraph.
    #
function(nodes, lines = nodesByLine(nodes),
         lw = getLineEnds(lines), asLogical = FALSE)            
{
    end = quantile(lw[, 2],  .75)
    w = end - lw[,2] > .1*median(lw[,2] - lw[,1])
    if(!asLogical && !missing(lines))
        lines[w]
    else
        w
}



##################

getTextByCols =
    #
    # Have to remove headers and footers first!
    #
    #  The nodes that are a little further to the right of the majority are indenations of the
    # first line in a paragraph, like this sentence!
    #
    #  Need to identify blocks of text that span the entire page and those that are columnar.
    #
    #
function(p, threshold = .1, asNodes = FALSE,
         txtNodes = getNodeSet(p, getXPathDocFontQuery(p, docFont)),
         bbox = getBBox2(txtNodes, TRUE),
         breaks = getColPositions(if(perPage) p else as(p, "XMLInternalDocument"), threshold = threshold, bbox = bbox, perPage = perPage, docFont = docFont, ...),
         perPage = FALSE, docFont = FALSE, ...)         
{

    if(length(txtNodes) == 0)
        return(character())
    
    bb = bbox
    bb$text = sapply(txtNodes, xmlValue)
    
    if(asNodes) {
        split(txtNodes, cut(bb$left, c(0, breaks[-1], Inf)))
    } else {
        cols = split(bb, cut(bb$left, c(0, breaks[-1], Inf)))
        cols = sapply(cols, function(x) paste(x$text[ order(x$top) ], collapse = "\n"))
    }
}




bodyLine =
function(doc)
{
   ll = getNodeSet(doc, "//line")
   bb = getBBox(ll, asDataFrame = TRUE)
     # get the page number for each line
   bb$pageNum = sapply(ll, function(x) xmlGetAttr(xmlParent(x), "number"))

      # now get all of the text nodes
   txtBB = getBBox2(getNodeSet(doc, "//text"))
      # get the extremes for the text, i.e., the left and right margins
   txtExt = c(left = min(txtBB[, "left"]), right = max(txtBB[, "left"] + txtBB[, "width"]))

      # All the lines that are greater than 95% of the 
   hll = bb[ ((bb[, "x1"] - bb[, "x0"])/ diff(txtExt)) > .95,  ]

   # is there the same y0 (or y1) on each page for these lines
   table(hll[, "y1"])
}




# See ~/Davis/UCDSISR/R/ for getTranscriptCourses2

getPageLines =
function(page, center = 465, nodes = getNodeSet(page, "./text"), bbox = getBBox2(nodes))
{
    cols = rev(split(as.data.frame(bbox), bbox[,"left"] < center))
    tmp = lapply(cols, reassembleLines)
    tmp = lapply(cols, reassembleLines)
    colLines = lapply(tmp, function(x) sapply(x, function(x) paste(rownames(x), collapse = " ")))    
}

reassembleLines =
function(box)
{
   by(box, box[, "top"], assembleLine)
}


assembleLine =
function(els)
{
   o = order(els$left)
   els[o,]
}




foo = 
function(page, nodes = getNodeSet(doc, ".//text"))
{    
    ll = nodesByLine(nodes)
    pos = getLineEnds(ll)
}


combineBBoxLines =
function(bbox, by = "y1")
{
  do.call(rbind, by(bbox, bbox[[by]], combineLines))
}

combineLines =
function(bbox, sameY = FALSE)
{

  sameHeight = length(unique(unlist(bbox[,c("y0", "y1")]))) == 1
     # This allows the right end of a line to go beyond the left part of the next line, i.e. overlap.     
     # Not simply meeting at the same point.  See Nittapatana-2008 paper.
  ok =  all((bbox$x0[-1] - bbox$x1[-nrow(bbox)]) < 0)
  
  if(ok && (!sameY || sameHeight)) {
      ans = bbox[1,]
      ans[c("x0", "x1")] = c(min(bbox$x0), max(bbox$x1))
      return(ans)
  } else
     bbox
}
