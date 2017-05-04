# The pdf in 0698940064/ of the Zoonotics end note
# There is a 43 point font. That is one character that starts the text and spans 2 lines of text.
#

getDocTitle =
function(file, page = 1, doc = xmlParse(file))
{
  if(missing(doc) && is(file, "XMLInternalDocument"))
      doc = file

  if(missing(page) && length(getNodeSet(doc, "//ulink[starts-with(@url, 'http://www.researchgate.net')]")) > 0)
      page = 2
  
     # handle case where the first page is a cover sheet
  p1 = getNodeSet(doc, "//page")[[page]]

  if(length(getNodeSet(p1, ".//text")) == 0)
      return(list())
  
  fonts = getFontInfo(p1)
  if(is.null(fonts))
     return(NULL)
  m = which.max(fonts$size)
  mx = fonts$size[m]
  id = fonts[m, "id"]
  txt = getFontText(p1, id)

  if(all(nchar(sapply(txt, xmlValue)) == 1)) {
    # XXX Need to deal with the first letter in each word being different.
  }
      
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
  }

  txt
}

isTitleBad =
    # Takes 
function(txtNodes, minWords = 3)
{    
   length(strsplit(paste(sapply(txtNodes, xmlValue), collapse = ""), " ")[[1]]) < minWords     
}

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
