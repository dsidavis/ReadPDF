# The pdf in 0698940064/ of the Zoonotics end note
# There is a 43 point font. That is one character that starts the text and spans 2 lines of text.
#

getDocTitleString =
function(f, nodes = getDocTitle(f, ...), ...)
   paste(sapply(nodes, function(x) if(is.character(x)) x else xmlValue(x)), collapse = " ")

getDocTitle =
    #
    # We may want to determine if this is scanned, and if so, does it have a cover page.
    #
    #  Identify a journal name and discard this from the results (after the meta)
    # e.g. "LatestDocs/PDF/2143276081/Kamhieh-2006-Borna disease virus (BDV) infect1.xml" - Veterinary Quaterly.
    # We can hard code these, but let's try to learn from the headers and footers.
    # 
    #
    #  See 1599857215/Learned-2005-Extended interhuman transmission.xml for a title in the meta that is just the name of the file.
    #
function(file, page = 1, doc = readPDFXML(file), meta = FALSE, minWords = 1, asNode = FALSE, scanned = isScanned(doc), ...)
{
  if(missing(doc) && is(file, "XMLInternalDocument"))
      doc = file


  if(isOIEDoc(doc)) 
     return(getOIETitle(doc, asNode = asNode))



  if(meta) {
      meta = getNodeSet(doc, "//docinfo/META[@name = 'title']")
      if(length(meta)) {
          ti = xmlGetAttr(meta[[1]], "content")
             # The first grepl() detects the name of the file. Should compare with docName(doc)
             #  !grepl("PDF/[0-9]+/", ti)          
          if(!grepl("$$", ti, fixed = TRUE) && !grepl("Microsoft", ti) && !grepl("PDF/PDF", ti) && !isMetaTitleFilename(ti, docName(doc)) &&
               !grepl("^doi:", ti) && !isTitleBad(ti))
              return( ti )
      }
  }
  
  if(missing(page) && (isResearchGate(doc)  ||
                        length(getNodeSet(doc, "//page[1]//text[. = 'PLEASE SCROLL DOWN FOR ARTICLE']")) > 0 ))
      page = 2

    # For what documents is this necessary - e.g. Puzelli et al
    # But need to be more specific for this cover page as other docs, e.g., A case of Crimean-Congo Ham.... .xml doesn't
    # have the cover page but does have the www.euro...org link.
  if(missing(page) && length(getNodeSet(doc, "//page[1][./text[contains(., 'www.eurosurveillance.org')] and ./text[contains(., 'Weekly')] ]")))
      page = 2
  
     # handle case where the first page is a cover sheet
  p1 = getNodeSet(doc, "//page")[[page]]

  if(length(getNodeSet(p1, ".//text")) == 0)
      return(list())

  if(scanned) # Should we use isScanned() ?
      return(if(asNode) list() else NA_character_)
  

  
  fonts = getFontInfo(p1)
  if(is.null(fonts))
     return(NULL)
  m = which(fonts$size == max(fonts$size))
  mx = fonts$size[m]
  id = fonts[m, "id"]
  
  txt = getFontText(p1, id)

  if(all(nchar(sapply(txt, xmlValue, trim = TRUE)) == 1)) {
    # XXX Need to deal with the first letter in each word being different.
  }


#
# getFontText() should reassemble text across lines, etc. and return
# the elements that are separate.
#

  isElsevier = isElsevierDoc(doc)
  if(isElsevier && !isTitleBad(txt)) {
      return(splitElsevierTitle(txt, p1, asNode = asNode))
#     tt = names(txt)
#     if(grepl("Journal", tt[1]))
#         tt = tt[-1]
#     return(paste(tt, collapse = " "))
  }
  
  ctr = 1L

  if(!all(w <- isTitleBad(txt, if(length(txt) > 1) 0 else 3))) {
      # order them by line. They may be upside down.
      # See 1834853125/394.full.xml
      txt = orderByLine(txt[!w])
        # Make certain to get the rest of the text in the tile that may be in a different font.
        # See why we are doing this in 4214927259/A case of Crimean-Congo haemorrhagic fever in.xml
        # 
      tmp = mkLines(txt, p1)
      if(asNode) return(tmp)
      
      title = paste(sapply(tmp, function(x) paste(if(is.list(x)) sapply(x, xmlValue) else xmlValue(x), collapse = " ")), collapse = "\n")
      return(title)
#      return(paste(names(txt), collapse = " "))
  }
      
  while( (length(txt) == 1 && nchar(names(txt)) == 1) || all(w <- isTitleBad(txt, minWords = minWords))) {

      # then a single character that is very large
      # get second largest font and

      mx = max(fonts$size[fonts$size < mx ])
      w = (fonts$size == mx)
      id = fonts$id[w]
# if multiple ones, see if any are bold and restrict to that.
# Doesn't work for Van Der Poel-2005
      if(length(id) > 1 & any(isb <- isBold(fonts[w,]))) #fonts$isBold[w]))
          id = id[isb]

      if(length(id) == 0)
          break  #XXXX
         
      txt = getFontText(p1, id)
      ctr = ctr + 1L
  }

  orderByLine(txt)
}


fixTitleNodes =
    ## We can compute the locations of the resulting nodes (with getBBox2),
    ## sort them and compute the differences. Or order them by line
    ## and see how far apart they are, OR see if there are other nodes in between lines.
    ## 
    ## We should compute the line spacing for the entire page or document and compare to that.
    ## For now, we'll use a threshold of 3 which is 3 height of text in a line, so that allows double spacing.
function(nodes, threshold = 3)
{
    ll = nodesByLine(nodes)
    txt = sapply(ll, function(x) trim(paste(sapply(unlist(x), xmlValue), collapse = " ")))
    ll = ll [ txt != ""]

    bb = lapply(ll, getBBox2)
    h = median(getBBox2(nodes)[,"height"])
    top = sapply(bb, function(x) median(x[, "top"]))
     ##XXX This is broken!
    d = diff(c(top[1] - h, top))

    unlist(ll[ d < h*threshold ])
}   


isTitleBad =
function(txtNodes, minWords = 3, filename = "")
 UseMethod("isTitleBad")

isTitleBad.list = isTitleBad.XMLNodeSet =
function(txtNodes, minWords = 3, filename = "")
{
     # One document (  0628126814/Chochlakis-2010-Human%20anaplasmosis%20and%20anaplas.xml)
     # uses rotation as a form of italics. So we don't check for non-zero rotation, but > 16
  if(any( as.numeric(sapply(txtNodes, xmlGetAttr, "rotation", 0.)) > 16))
      return(TRUE)

    # Check to see if this is really a watermark, etc. at the extreme of  a page, e.g.
    # 3364361467/Majumder-2016-Utilizing Nontraditional Data So.xml

  pos = as.numeric(sapply(txtNodes, xmlGetAttr, "top"))
  names(pos) = sapply(txtNodes, xmlValue)
  
  opos = as.numeric(unlist(getNodeSet(xmlParent(txtNodes[[1]]), ".//text/@top")))
  names(opos) = xpathSApply(xmlParent(txtNodes[[1]]), ".//text", xmlValue)
  i = match(names(pos), names(opos))
#  w = pos <= max(opos[ - i ])
# how many text nodes are below the txtNodes.
  w = opos[-i] > min(pos)
  if(sum(w) < 5)
     return(TRUE)


  bb = getBBox2(txtNodes, TRUE)

  if(diff(range(bb$top)) > 2 * median(bb$height)) {
      ll = nodesByLine(txtNodes, bbox = bb)
      bad = sapply(names(ll), isTitleBad, minWords, filename)
      if(!all(bad)) 
         return(rep(bad, sapply(ll, length)))
  }
    
if(FALSE) {    
   w = sapply(txtNodes, isTitleBad, minWords, filename)
   if(!all(w))
       return(w)
}
       
# Maybe put these back in if the above doesn't discriminate well.   
   txt = paste(sapply(txtNodes, xmlValue), collapse = " ")
   isTitleBad(txt, minWords)
}

isTitleBad.XMLInternalNode =
function(txtNodes, minWords = 3, filename = "")
  isTitleBad(xmlValue(txtNodes), minWords)

isTitleBad.character =
function(txtNodes, minWords = 3, filename = "", lowerCase = TRUE)
{
#  txtNodes = tolower(txtNodes)
  txtNodes = XML:::trim(txtNodes)

  
  #Test:
   grepl("^Review$", txtNodes)  ||
  grepl("^viruses$", txtNodes)  ||        
  grepl("Acta Veterinaria", txtNodes) ||
  grepl("BMC Infectious Diseases", txtNodes) ||
  nchar(txtNodes) < 6 ||   
  grepl("Letters? to the Editor", txtNodes, ignore.case = lowerCase) || grepl("^BRIEF COMMUNICATIONS$", txtNodes) ||
  grepl("^[-0-9, ]+$", txtNodes) ||          
  grepl("^Letters$", txtNodes) || grepl("table of contents", txtNodes) || grepl("^[A-Za-z]+ +\\bJournal$", txtNodes) ||
  txtNodes %in% c("DISPATCHES", "Research Paper", "KEYWORDS", "ScienceDirect", "Occasional Papers", "HHS Public Access", "Newsdesk", "Case Report", "LETTERS", "No Job Name", "NIH Public Access", "Special Report  Rapport spÃ©cial", "W J C C") ||
#^^^^ non-ASCII
      grepl("Rapid Communications|Research Articles|Weekly issue|Vol\\.[0-9]+|Volume [0-9]+|ournal of|Science Journals", txtNodes) ||
  length(strsplit(txtNodes, " ")[[1]]) < minWords
}



#################

splitElsevierTitle =
    #
    # For an elsevier document, we want to find the
    #     heavy black line above the title
    # or  the large grey box at the top of the page that contains the name of the journal
    # e.g. Kelly-2008-NIH
    #  Lau-2007 has no grey box and no line.  So we have to do further checks on the shaded box  to see it is large enough.
    #
function(nodes, page, asNode = FALSE)
{
    y = as.numeric(sapply(nodes, xmlGetAttr, "top"))
    lines = getNodeSet(page, ".//line") #XX??  and .//rect
    lw = as.numeric(sapply(lines, xmlGetAttr, "lineWidth", 0))
    yl = 0

    if(any(lw > 10)) {
        i = which.max(lw)
           # Is the 
        yl = as.numeric(strsplit(xmlGetAttr(lines[[i]], "bbox"), ",")[[1]])[2] + lw[i]/2
    } else {

        r = getNodeSet(page, ".//rect[@fill.color != '0,0,0']")
        if(length(r) > 0) {
                # check the height of these rectangles to ensure they aren't just thin lines.
            bb = t(sapply(r, function(x) as.numeric(strsplit(xmlGetAttr(x, "bbox"), ",")[[1]])))
            i = which(abs(bb[,2] - bb[,4]) > 10)
            if(length(i)) {
               left = min(as.numeric(unlist(getNodeSet(page, "./text/@left"))))
               if(bb[i[1],1] - left > 20)
                   yl = bb[i,2][1]
            }
        }
        
#           # get the two images.
#        img.dims = xpathSApply(page, ".//img", function(x) as.numeric(xmlAttrs[c("y", "width", "height")]))
        
    }
       # if none of the nodes are above the line, don't filter. See 0809541268/Kitajima-2009-First%20detection%20of%20genotype%203%20he.xml"
    if(any(y > yl))
        nodes = nodes[y > yl]

    if(asNode)
       return(nodes)
    
    paste(names(nodes), collapse = " ")

}


notWord =
function(x)
{
  grep
}



orderByLine =
function(nodes)
{
    o = order(as.numeric(sapply(nodes, xmlGetAttr, "top")))
    nodes[o]
}

isElsevierDoc =
function(doc)
{
    length(getNodeSet(doc, "//page[1]//text[ contains(lower-case(.), 'elsevier')]")) > 0
}

isResearchGate =
function(doc)
{
    length(getNodeSet(doc, "//text[contains(., 'www.researchgate.net')]")) > 0
}


isMetaTitleFilename =
function(ti, docName = NA)
{
  # return(grepl(ti, URLdecode(docName), fixed = TRUE))
  els = strsplit(ti, "/")[[1]]
  dels = strsplit(gsub("\\.xml$", "", URLdecode(docName)), "/")[[1]]
  return ( all (dels[seq(length(dels) - 1, length = 2)] == els[ seq(length(els) - 1, length = 2) ] ) )

          
 return (  gsub("\\.xml", "", basename(URLdecode(ti))) == els[ length(els) ] )  # ...
         
      
  length(gregexpr("/", ti)[[1]]) >= 3 && 
      gsub("\\.xml", "", basename(URLdecode(ti))) != basename(ti) 
}


###########################
#
#XXX
#
#  These functions look for text at approximately the same line and includes them.
#
# These are a little too liberal. They don't check the content is close to the original txt nodes.
#  So we can have text on one side of the page at the same "height" be connected to that on the right.
#  See 0007787963/Ko-2010-Prevalence of tick-borne encephalitis.xml
#
mkLines =
function(txt, page)
{
    pos = unique(as.numeric(sapply(txt, xmlGetAttr, "top")))
    h = max(as.numeric(sapply(txt, xmlGetAttr, "height")))    
    lapply(pos, mkLine, page, h)
}

mkLine =
  # getDocTitle("4214927259/A case of Crimean-Congo haemorrhagic fever in.xml")
function(pos, page, height)
{
# Could do this in XPath, but don't have abs() there.    
#   getNodeSet(page, sprintf("./text[@top = %f]", pos))
# So get the Bounding boxes for all text nodes.
#
    
    textNodes = getNodeSet(page, "./text")
    bb = getBBox2(textNodes)
              # added test that the height of any matching element is at least 70% of the original title node.
    w = abs(pos - bb[, "top"]) < .33*height & bb[, "height"]/height > .7
    textNodes[w]
}




isOIEDoc =
function(doc)      
{
    doc = as(doc, "PDFToXMLDoc")
    
    length(getNodeSet(doc, "//text[contains(., 'http://www.oie.int/wahis_2/')]")) > 0
}


getOIETitle =
function(doc, asNode = FALSE)
{
    doc = as(doc, "PDFToXMLDoc")
     # Find the image on page 1 and then the text that is not black that is to the right of the image"    
    p1 = doc[[1]]
    img = getNodeSet(p1, ".//img")[[1]]                                       #    bbi = getBBox2()
    atNames = c("x", "y", "width", "height")
    bbi = structure(as(xmlAttrs(img)[atNames], "numeric"), names = atNames)
    # not(@color = '#000000') and 
    xp = sprintf(".//text[@left > %f and @top > %f and @top < %f ]", bbi["x"] + bbi["width"], bbi["y"], bbi["y"] + bbi["height"])
    ans = getNodeSet(p1, xp)
    colors = getTextNodeColors(ans, doc = doc)

    ans = ans[colors != "#ababab"]

    if(asNode)
       ans
    else
       paste( sapply(ans, xmlValue), collapse = " ")

}


# Date of start of the event
