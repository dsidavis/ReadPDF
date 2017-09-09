# Ryan found problems with
#  J Infect Dis.-2015-Ogawa-infdis-jiv063,
#  Holsomback-2009-Bayou virus detected in non-Or.pdf,
#  Frances-2004-Occurrence of Ross River virus an.pdf. 


# Does work:  0043026620/Kaba-2010-Detection of hepatitis E virus in w1.pdf  - Article History on left side of page. Accepted 4 August 2009

# "../0857285937/Vasconcelos-2003-%5BYellow%20Fever%5D.xml" is in Portuguese.
#    Date is mar-abr, 2003.

if(FALSE) {
  i = file.info(list.files("..", full.names = TRUE))
  dirs = rownames(i)[i$isdir]
  pubs = lapply(dirs, function(x) try(getPublicationDate(list.files(x, pattern = "xml", full = TRUE))))  
  dirs[!err][ sapply(pubs[!err], length) == 0]
}



if(FALSE) {

# After matching.R

hasPDF = (!is.na(ms$PDF) & ms$PDF != "")
pdfs = unique(ms$PDF[hasPDF])
cleanPdfs = gsub("\\.pdf(;|$)", ".xml\\1", gsub("internal-pdf:/", path.expand(PDFDir), pdfs))
docs = strsplit(cleanPdfs, ";")
#docs = sapply(e, function(x) gsub("internal-pdf://", "", x))

#tt = table(unlist(docs))

ff = unique(unlist(docs))
ex = file.exists(ff)
table(ex)

dates = lapply(ff[ex], function(x) try(getPublicationDate(x)))
table(sapply(dates, is, "try-error")) # None.


b = ff[ex][ sapply(dates, length) == 0 ]
bpdf = gsub("\\.xml$", ".pdf", b)



################
# Older



PDFDir = "NewData_Feb2017/Zoo_02_02_2017 Copy.Data/PDF"
dirs = dirname(unlist(docs))
docDirs = list.files(PDFDir)
all(dirs %in% docDirs)
fdirs = sprintf("%s/%s", PDFDir, dirs)
xmls = sapply(fdirs, list.files, pattern = "xml$", full.names = TRUE)

xdocs = lapply(xmls, function(x) try(xmlParse(x)))
w = sapply(xdocs, is, 'try-error')

#xdocs[!w]
pd = lapply(xdocs[!w], getPublicationDate)

table(sapply(pd, length) == 0)

# The ones that  didn't match.
nodate = sapply(pd, length) == 0 
xmls[!w][nodate]

tmp = lapply(xdocs[!w][nodate], getPublicationDate)
i = (sapply(tmp, length) == 0)
f = xmls[!w][nodate][i]
gsub("\\.xml$", ".pdf", f)
}


getPublicationDate =
    # OLD VERSION - SEE BELOW
function(doc, page = 1, words = c("accepted", "received", "volume", "copyright", "published", "submitted", "recebido", "aceito"))
{
  if(is.character(doc))
     doc = xmlParse(doc)

    # look for the words that often identify a year.
  cond = paste(sprintf("contains(lower-case(.), '%s')", words), collapse = " or ")
    
  ans = xpathSApply(doc, sprintf("//page[@number='%d']//text()[%s]", page, cond),  xmlValue)

  if(length(ans) == 0) {
      tmp = getNodeSet(doc, sprintf("//page[@number = '%d']//text()[contains(., 'Emerging Infectious Disease')]", page))
      if(length(tmp))
         ans = gsub(".* Vol\\. .*, (.*)", "\\1", xmlValue(tmp[[1]]))
  }


 if(length(ans) == 0) {
      tmp = getNodeSet(doc, sprintf("//page[@number = '%d']//text()[contains(., 'Journal of ')]", page))
      if(length(tmp))
         ans = xmlValue(tmp[[1]])
  }  

  # We don't do anything with the txt here.
 if(FALSE || length(ans) == 0) {
      tmp = getNodeSet(doc, sprintf("//page[@number = '%d']//text()", page))
      if(length(tmp)) {
         txt = sapply(tmp, xmlValue)
      }
  }  

  if(length(ans) == 0 && page == 1) {
           # Go to the last page
     npages = length(getNodeSet(doc, "//page"))
     if(npages > 1) {
       ans = getPublicationDate(doc, npages, words)
       if(length(ans) == 0 && npages > 2)
           # try second page
          return(getPublicationDate(doc, 2, words))
    }
  }

  ans
}


findVol =
function(doc)
{
   unlist(xpathSApply(doc, "//text()[contains(lower-case(.), 'vol')]", getVolume))
}

getVolume =
function(node)
{
   txt = xmlValue(node)
   grep("Volume|Vol\\.?[[:space:]]?[0-9]+", txt, value = TRUE)
}

#tmp = lapply(docs2, )


isBioOne =
    #
    # There are some documents from BioOne which are scanned documents with a front page that is not scanned.
    # isScanned() only looks at the first page. So we detect these explicitly.
    #
function(doc)
   length(getNodeSet(doc, "//text[starts-with(., 'BioOne sees sustainable scholarly ')]")) > 0




getPublicationDate =
    #
    # New version
    #
    #
    #@'return a named character  vector. The values contain a date, possibly with other text content.
    #     the names indicate which step/method was used to identify the date.  These will all be the same
    #     as we stop when we have any date.
    #
    # The steps are Scanned (no date)
    #               NIH Public Access  (particular format in which we can find the date)
    #               Title     (Date in the title)    
    #               Received  (information about when received, accepted, published)
    #               footer    (taken from the footer on the first page)
    #               header    (taken from the header of the first page)
    #               copyright (find the copyright symbol and the date after it)
    #               AboveTitle (text above the title of the paper)
    #               TextRegEx  (find a date of the form [number] NameOfMonth[,] Year anywhere in the text)
    #
function(doc, checkAbstract = TRUE)
{
  if(is.character(doc))
     doc = readPDFXML(doc)


  abstract = names(findAbstract(doc, FALSE))

  if(checkAbstract && length(abstract)) {
      txt = paste(abstract, collapse = "\n")
      m = gregexpr("\\b[0-9]{4}\\b", txt)
      if(any(m[[1]] > -1)) {
          return(structure(regmatches(txt, m)[[1]], names = rep("abstract", length(m[[1]]))))
      } 
  }
  
  if(isBioOne(doc))
      return(textAboveTitle(doc, 2))

  if(isScanned2(doc)) {
      y = getYearFromFileName(basename(docName(doc)))
      if(length(y))
          return(c(filename = y))
      else
          return(structure(NA, names = "Scanned"))
  }

  nih = getNodeSet(doc, "//text[. = 'NIH Public Access']")
  if(length(nih) > 0) {
      txt = unique(xpathSApply(nih[[1]], "./following-sibling::text[contains(., 'doi:')]", xmlValue))
      if(length(txt))
        return(structure(txt, names = "NIH Public Access")) # , journal = xmlValue(getSibling(nih[[1]]))))
  }

  if(length(getNodeSet(doc, "//text[starts-with(., 'www.oie.int/')]")) > 0) {
     date = xmlValue(getNodeSet(doc, "//text[. = 'Date of start of the event']/following-sibling::text[1]")[[1]])
     return(c(OIE = date))
  }
  
  title = getDocTitleString(doc)
  if(hasYear(title))
      return(structure(title, names = "Title"))

  words = c("Received", "Accepted", "Available online", "Published at", "Published online", "received for review")
  cond = sprintf("starts-with(normalize-space(.), '%s')", t(cbind(words, paste0("(", words))))
  
  rec = getNodeSet(doc, sprintf("//text[%s]", paste(cond, collapse = " or ")))
  
  if(length(rec) > 0) {
      txt = xmlValue(rec[[1]])
      if(!hasYear(txt)) {
          top = xmlGetAttr(rec[[1]], "top")
          txt = paste(xpathSApply(rec[[1]], sprintf("./following-sibling::text[@top = '%s']", top), xmlValue), collapse = " ")
      }
      if(hasYear(txt))
          return(structure(txt, names = rep('Received', length(txt))))
  }

  rec = getNodeSet(doc, "//text[contains(., 'received for review')]")
  if(length(rec) > 0) {
      txt = xmlValue(rec[[1]])
      if(!hasYear(txt)) {
          top = xmlGetAttr(rec[[1]], "top")
          txt = paste(xpathSApply(rec[[1]], sprintf("./following-sibling::text[@top = '%s']", top), xmlValue), collapse = " ")
      }
      if(hasYear(txt))
          return(structure(txt, names = rep('Received', length(txt))))
  }
  
  

  p1 = getNodeSet(doc, "//page")[[1]]
  footer = getPageFooter(p1)
 
  if(!grepl("Downloaded", footer) && any(w <- hasYear(footer)))
      return(structure(footer[w], names = rep("footer", sum(w))))

  footer = getPageHeader(p1)
  if(!grepl("Downloaded", footer) && any(w <- hasYear(footer)))
      return(structure(footer[w], names = rep("header", sum(w))))  

  cr = getNodeSet(doc, "//text[contains(., '©')]")
  if(length(cr)) {
      tt = sapply(cr, xmlValue)
      if(any(w <- hasYear(tt)))
          return(structure(tt[w], names = rep("copyright", sum(w))))
  }

  tt = textAboveTitle(doc, 1)
  if(any(w <- hasYear(tt)))
      return(structure(tt[w], names = rep("AboveTitle", sum(w))))


  txt = getDocText(doc)
  rx = sprintf("([0-9]{1,2} )?(%s),? (19|20)[0-9]{2}", paste(getMonthNames(), collapse = "|"))
  g = gregexpr(rx, txt, ignore.case = TRUE)
  if(g[[1]][1] > 0) {
      tt = unique(regmatches(txt, g)[[1]])
      return(structure(tt, names = rep("TextRegEx", length(tt))))
  }

  # Could go to the second page and start over with the headers, etc.

  tt = getNodeSet(doc, "//text[isDate(string(.))]",
                  xpathFuns = list(isDate = containsDate))
  if(length(tt)) 
      return(unique(extractDate(sapply(tt, xmlValue))))


  fname = basename(docName(doc))
  y = getYearFromFileName(fname)
  if(length(y))
     return(c(filename = y))
  
  NA
}

getYearFromFileName =
    # getYearFromFileName("Kohl 1996.xml")
    # getYearFromFileName("Smithburn-1949-The susceptibility of African w.xml")
function(fname)
{
  #   "(^|[^0-9])[0-9]{4}([^[0-9]|$)"
  # But need to not include characters within the () ()
  m = gregexpr("\\b[0-9]{4}\\b", fname, perl = TRUE)[[1]]
  if(any(m > -1))
     regmatches(fname, m)
  else
     character()
}

getMonthNames =
function(format = "%B")    
  unlist(lapply(format, function(f) format(ISOdate(2017, 1:12, 1), f)))

containsDate =
function(str)
{
   grepl(mkDateRegexp(), str) 
}

mkDateRegexp =
function()
{
  sprintf("[0-9]{4} (%s)( [0-9]{,2})", paste(getMonthNames(), collapse = "|"))
}

extractDate =
function(str)
{
  unlist(regmatches(str, gregexpr(mkDateRegexp(), str)))
}


getDocText =
    # Too simple. See the one in ReadPDF
function(doc)
{
    paste(xpathSApply(doc, "//text", xmlValue), collapse = " ")
}

textAboveTitle =
    # Finds the title and then gets the text above that. This is useful when there is header material
    # in a sequence of lines.
    # Could find it in other ways also.
function(doc, page = 1)
{
    titleNodes = getDocTitle(doc, page)
    if(length(titleNodes) == 0 || is.character(titleNodes))
        return(NA)
   pos = min(as.integer(sapply(titleNodes, xmlGetAttr, "top")))
   page = getNodeSet(titleNodes[[1]], ".//ancestor::page")[[1]]
   bbox = getBBox2(getNodeSet(page, ".//text"))
   rownames(bbox)[ bbox[, "top"] < pos]
}

hasYear =
function(txt)
{
#      grepl("(^| )(19|20)[0-9]{2}( |$)", txt)
     grepl("\\b(19|20)[0-9]{2}\\b", txt)
}

