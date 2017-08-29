getSectionText =
function(doc, asNodes = FALSE, secHeaders = findSectionHeaders(doc, ...), ...)
{
    if(is.character(doc))
        doc = readPDFXML(doc)

    secs = lapply(seq(along = secHeaders),
                  function(i)
                    getNodesBetween(secHeaders[[i]], if(i == length(secHeaders)) NULL else secHeaders[[i+1]]))
    names(secs) = sapply(secHeaders, xmlValue)
    if(asNodes)
       return(secs)
    
    txt = sapply(secs, xmlValue)    
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
         allowRotated = FALSE
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
    
#browser()
    if(!length(intro)) {
       filter = paste(sprintf("lower-case(normalize-space(.)) = '%s'", otherSectionNames), collapse = " or ")
       xp = sprintf("//text[%s]", filter)
       intro = getNodeSet(doc, xp)
    }

    if(length(intro))
        intro = intro[ ! isLowerCase(sapply(intro, xmlValue)) ]

    if(length(intro)) {
        if(hasNum)
           return(getNodeSet(doc, "//text[isNum(normalize-space(.))]", xpathFuns = list(isNum = isSectionNum)))


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

        w = sapply(secs, function(x) length(getNodeSet(x, ".//preceding::text[ lower-case(normalize-space(.)) = 'references' or lower-case(normalize-space(.)) = 'references cited' or lower-case(normalize-space(.)) = 'supporting online material']"))) > 0
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
        
        if(checkCentered && isCentered(intro[[1]]))
           secs = secs[ sapply(secs, isCentered) ]

        if(isOnLineBySelf(intro[[1]])) {
            i = sapply(secs, isOnLineBySelf)
            secs = secs[ i ]
        }
        return(secs)
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
function(node, pos = getColPositions(doc),
         textNodes = getNodeSet(xmlParent(node), ".//text"),
         bbox = getBBox2(textNodes, TRUE))
    # doc = as(node, "XMLInternalDocument"),
{
#browser()    
    colNodes = getTextByCols(pageOf(node, TRUE), asNodes = TRUE)
       # determine which column this is in
    colNum = inColumn(node, colNodes)
    col = colNodes[[colNum]]
#    lines = split(col, as.integer(sapply(col, xmlGetAttr, "top")))
    h = as.integer(xmlGetAttr(node, "top"))
    npos = as.integer(sapply(col, xmlGetAttr, "top"))
    sum(npos == h) == 1
}

inColumn =
function(node, cols = getTextByCols(xmlParent(node), asNodes = TRUE))
{
    ans = which(sapply(cols, function(x) any(sapply(x, identical, node))))
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
function(page, nodes = getNodeSet(page, ".//text"), bb = getBBox2(nodes, TRUE),
          cols = getColPositions(page))
{    
    b = split(bb, cut(bb$left, c(0, cols[-1], Inf) -2))
    k = lapply(b, function(x) x[order(x$top),])
}



getNodesBetween =
function(x = NULL, y = NULL)
{
    if(is.null(x) && is.null(y))
        stop("need to specify either x or y or both")
    
    if(is.null(x))
       x = getFirstTextNode(as(y, "XMLInternalDocument"))
    
    if(is.null(y))
       y = getLastNode(as(x, "XMLInternalDocument"))    
    s = pageOf(x)
    e = pageOf(y)

    if(e > s) {
        # get all the nodes on each page up to e
        p1 = getTextAfter(x)
        if(e - s > 1) {
            pgs = getPages(as(x, "XMLInternalDocument"))[ seq(s + 1, e - 1)]
            pgs = lapply(pgs, getTextByCols, asNodes = TRUE)
        } else
            pgs = NULL
        pe = getTextAfter(, y)
        c(p1, unlist(pgs, recursive = FALSE), pe)
    } else {
        getTextAfter(x, y)
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
    #
    
function(x = NULL, to = NULL, before = FALSE)
{
    page = xmlParent(if(!is.null(x)) x else to)
    cols = getTextByCols(page, asNodes = TRUE)
    
    if(!is.null(x)) {
          # find the column and the index of the node matching x
        i = lapply(cols, function(n) which(sapply(n, identical,  x)))
        colNum = which(sapply(i, length) > 0)
    }

    if(!is.null(to)) {
        j = lapply(cols, function(n) which(sapply(n, identical,  to)))
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
            btwn = seq(colNum + 1, length = to.colNum  - colNum - 1)
            nodes =  c(nodes, cols[btwn],
                        cols[[to.colNum]][ seq(1, length = j[[to.colNum]] - 1) ])
        }
    }

    unlist(nodes, recursive = FALSE)
}


getLastNode =
    # get the final node in the document - last node in last page
    # Use this when getting the content for the last section
function(doc)
{
  getNodeSet(doc, "//page[last()]/text[last()]")[[1]]
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
