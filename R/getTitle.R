# The pdf in 0698940064/ of the Zoonotics end note
# There is a 43 point font. That is one character that starts the text and spans 2 lines of text.
#

getDocTitleString =
function(f, nodes = getDocTitle(f))
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
function(file, page = 1, doc = xmlParse(file), meta = TRUE, minWords = 3)
{
  if(missing(doc) && is(file, "XMLInternalDocument"))
      doc = file

  if(meta) {
      meta = getNodeSet(doc, "//docinfo/META[@name = 'title']")
      if(length(meta)) {
          ti = xmlGetAttr(meta[[1]], "content")
             # The first grepl() detects the name of the file. Should compare with docName(doc)
             #  !grepl("PDF/[0-9]+/", ti)          
          if(!grepl("$$", ti, fixed = TRUE) && !grepl("PDF/PDF", ti) && !isMetaTitleFilename(ti, docName(doc)) &&
               !grepl("^doi:", ti) && !isTitleBad(ti))
              return( ti )
      }
  }
  
  if(missing(page) && length(getNodeSet(doc, "//ulink[starts-with(@url, 'https://www.researchgate.net')]")) > 0)
      page = 2

  if(length(getNodeSet(doc, "//text[contains(., 'www.eurosurveillance.org')]")))
      page = 2
  
     # handle case where the first page is a cover sheet
  p1 = getNodeSet(doc, "//page")[[page]]

  if(length(getNodeSet(p1, ".//text")) == 0)
      return(list())

  if(isScanned2(doc))
      return(NA_character_)
  
  fonts = getFontInfo(p1)
  if(is.null(fonts))
     return(NULL)
  m = which(fonts$size == max(fonts$size))
  mx = fonts$size[m]
  id = fonts[m, "id"]
  
  txt = getFontText(p1, id)

  if(all(nchar(sapply(txt, xmlValue)) == 1)) {
    # XXX Need to deal with the first letter in each word being different.
  }


#
# getFontText() should reassemble text across lines, etc. and return
# the elements that are separate.
#

  isElsevier = isElsevierDoc(doc)
  if(isElsevier) {
      return(splitElsevierTitle(txt, p1))
      tt = names(txt)
      if(grepl("Journal", tt[1]))
          tt = tt[-1]
      return(paste(tt, collapse = " "))
  }
  
  ctr = 1L

  if(!all(w <- isTitleBad(txt, if(length(txt) > 1) 0 else 3))) {
      # order them by line. They may be upside down.
      # See 1834853125/394.full.xml
      return(paste(names(orderByLine(txt[!w])), collapse = " "))
  }
      
  
  while( (length(txt) == 1 && nchar(names(txt)) == 1) || all(w <- isTitleBad(txt, minWords = minWords))) {  
      # then a single character that is very large
      # get second largest font and

      mx = max(fonts$size[fonts$size < mx ])
      w = (fonts$size == mx)
      id = fonts$id[w]
         # if multiple ones, see if any are bold and restrict to that.
      if(length(id) > 1 & any(fonts$isBold[w]))
          id = id[fonts$isBold[w]]
      txt = getFontText(p1, id)
      ctr = ctr + 1L
  }

  orderByLine(txt)
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

isTitleBad =
function(txtNodes, minWords = 3, filename = "")
 UseMethod("isTitleBad")

isTitleBad.list = isTitleBad.XMLNodeSet =
function(txtNodes, minWords = 3, filename = "")
{
  if(any( as.numeric(sapply(txtNodes, xmlGetAttr, "rotation", 0.)) != 0.0))
      return(TRUE)
    
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
function(txtNodes, minWords = 3, filename = "")
{    
  nchar(txtNodes) < 6 || txtNodes %in% c("NIH Public Access", "Letter to the Editor", "Special Report  Rapport spÃ©cial", "W J C C") ||
     grepl("Rapid Communications|Research Articles|Weekly issue|Vol\\.[0-9]+|Volume [0-9]+|ournal of|Science Journals", txtNodes) || length(strsplit(txtNodes, " ")[[1]]) < minWords
}



#################

getFontText =
function(page, fontID, rotation = 0)
{
  xp = sprintf(".//text[ %s ]", paste(sprintf("@font = '%s'", fontID), collapse = " or "))
  txt = getNodeSet(page, xp)
  names(txt) = sapply(txt, xmlValue)
  txt
}

getFontInfo =
function(page)    
{
   nodes = getNodeSet(page, ".//fontspec")
   if(length(nodes) == 0)
       return(NULL)
   a = t(sapply(nodes, xmlAttrs))
   d = as.data.frame(a, stringsAsFactors = FALSE)

   d$size = as.integer(d$size)
   d[c("isItalic", "isBold", "isOblique")] = lapply(d[c("isItalic", "isBold", "isOblique")], function(x) as.logical(as.integer(x)))

   d
}



splitElsevierTitle =
function(nodes, page)
{
    y = as.numeric(sapply(nodes, xmlGetAttr, "top"))
    lines = getNodeSet(page, ".//line")
    lw = as.numeric(sapply(lines, xmlGetAttr, "lineWidth"))
    yl = 0
    if(any(lw > 10)) {
        i = which.max(lw)
        yl = as.numeric(strsplit(xmlGetAttr(lines[[i]], "bbox"), ",")[[1]])[2]
    } else {

        r = getNodeSet(page, ".//rect[@fill.color != '0,0,0']")
        if(length(r) > 0) {
            yl = as.numeric(strsplit(xmlGetAttr(r[[1]], "bbox"), ",")[[1]])[2]            
        }
        
#           # get the two images.
#        img.dims = xpathSApply(page, ".//img", function(x) as.numeric(xmlAttrs[c("y", "width", "height")]))
        
    }
    nodes = nodes[y > yl]
    
    paste(names(nodes), collapse = " ")

}

notWord =
function(x)
{
  grep
}
