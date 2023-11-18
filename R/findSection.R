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
