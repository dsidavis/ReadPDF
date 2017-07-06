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
function(file, page = 1, doc = xmlParse(file), meta = TRUE)
{
  if(missing(doc) && is(file, "XMLInternalDocument"))
      doc = file

  if(meta) {
      meta = getNodeSet(doc, "//docinfo/META[@name = 'title']")
      if(length(meta)) {
          ti = xmlGetAttr(meta[[1]], "content")
             # The first grepl() detects the name of the file. Should compare with docName(doc)
             #  !grepl("PDF/[0-9]+/", ti)          
          if(!grepl("PDF/PDF", ti) && !isMetaTitleFilename(ti) &&
               !grepl("^doi:", ti) && !isTitleBad(ti))
              return( ti )
      }
  }
  
  if(missing(page) && length(getNodeSet(doc, "//ulink[starts-with(@url, 'http://www.researchgate.net')]")) > 0)
      page = 2
  
     # handle case where the first page is a cover sheet
  p1 = getNodeSet(doc, "//page")[[page]]

  if(length(getNodeSet(p1, ".//text")) == 0)
      return(list())
  
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

  ctr = 1L    
  while( (length(txt) == 1 && nchar(names(txt)) == 1) ||
          isTitleBad(txt)) {  
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

  txt
}

isMetaTitleFilename =
function(ti)
{
  length(gregexpr("/", ti)[[1]]) >= 3 && 
      gsub("\\.xml", "", basename(URLdecode(ti))) != basename(ti) 
}

isTitleBad =
function(txtNodes, minWords = 3, filename = "")
 UseMethod("isTitleBad")

isTitleBad.list = isTitleBad.XMLNodeSet =
function(txtNodes, minWords = 3, filename = "")
{
   txt = paste(sapply(txtNodes, xmlValue), collapse = "")
   isTitleBad(txt, minWords)
}

isTitleBad.XMLInternalNode =
function(txtNodes, minWords = 3, filename = "")
  isTitleBad(xmlValue(txtNodes), minWords)

isTitleBad.character =
function(txtNodes, minWords = 3, filename = "")
{    
   grepl("Journal of|Science Journals", txtNodes) || length(strsplit(txtNodes, " ")[[1]]) < minWords
}



#################

getFontText =
function(page, fontID)
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
