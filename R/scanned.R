# Put this in ReadPDF


isScanned2 = 
function(doc, threshold = 154.95, words = getDocWords(doc))
{
  if(is.character(doc))
     doc = xmlParse(doc)

  length(words)/getNumPages(doc)  < threshold
}

isScanned =
    # This attempts to determine if the original PDF  file was scanned.
    # It is conservative about this so may have false negatives, but rarely if ever false positives.
    #
    # This returns either TRUE or FALSE.
    # For TRUE, the names attribute  indicates the reasoning for classifying this as such.
#
#
# The key statistical characteristic appears to be a low number of words per page.
# We may be able to do better with the words per page for each page and then the joint distribution
# of these taking into account order.
#
# When we finally label the test set, we fit an rpart tree with
# > f = rpart(scanned ~ numPages + numWords + numWordsPerPage, docInfo, method = "class")
# and the results are a perfect split
#n= 2636 
#node), split, n, loss, yval, (yprob)
#      * denotes terminal node
#
#1) root 2636 239 FALSE (0.90933232 0.09066768)  
#  2) numWordsPerPage>=154.95 2397   0 FALSE (1.00000000 0.00000000) *
#  3) numWordsPerPage< 154.95 239   0 TRUE (0.00000000 1.00000000) *
#
#
# 
# We determine if the are any text nodes.
# If there are, we check if every page has an image.
#   Later we take into account a cover page and final page.
# 
# This is from the EDA we were doing as we were labelling the documents as scanned or not.
# We used our structural information to identify the documents that were likely scanned.
#
#summary(docInfo$numWordsPerPage[ docInfo$scanned ])
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.000   0.000   0.000   7.077  14.875 117.400 
# summary(docInfo$numWordsPerPage[ !docInfo$scanned ])
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  61.25  649.05  754.58  759.82  864.82 2178.70 
#
# Just 2 non-scanned documents have a wordsPerPage < 117.0
# # scanned documents have a words per page > 61.25.
#
#
# These numbers miss 2 documents which are scanned. And these have the lowest words per page of the
# reported !scanned.
# LatestDocs/PDF/1834853125/394.full.xml - 4 pages and middle 2 scanned.
# LatestDocs/PDF/2577128036/3868345.xml  - 2 pages.
#
# We misclassified these and they have several characteristics.
# Ideally, we want to identify cover pages - beginning and end that aren't scanned 
# but all of the other pages are scanned.
# We also want to identify pages where the only text is on the edge (top, bottom or right)
#    (We don't yet have information about the rotation of the text so the right is tricky)
#
# The first misclassified doc. gets all the way to the images. However, there are more images than pages
# and we don't consider whether any of the pages are just pure images, perhaps with a tiny amount of text.
#
#
#

    #
    # See /Users/duncan/DSIProjects/Zoonotics-shared/NewData_Feb2017/Zoo_02_02_2017 Copy.Data/PDF/0201749332/Aviles-1992-Transmission of western equine enc.xml
    #  Scanned but there is text on the side rotated 270 degrees.
    # XXpdftohtml doesn't seem to have the rotation. Add this.
function(doc, textNodeThreshold = 10)
{
  if(is.character(doc))
     doc = xmlParse(doc)

   # if every page only has img, fontspec, rect or line elements, then there is no text and it is a scanned document.
  nodes = getNodeSet(doc, "//page/*[not(local-name(.) = 'img') and not(local-name(.) = 'fontspec') and not(local-name(.) = 'rect') and not(local-name(.) = 'line')]")
  if(length(nodes) == 0)
     return(c(NoText = TRUE))

   # Heuristic for The American Society of Tropical Medicine and Hygiene
  isTropicalMedHyg = length(getNodeSet(doc, "//text[ contains(., 'The American Society of Tropical Medicine and Hygiene')]")) > 0
  if(isTropicalMedHyg && sum(sapply(nodes, xmlName) ==  "text") > 30)
     return(FALSE)

     # the page nodes themselves.
   pg = getNodeSet(doc, "//page")  

   textWords = sapply(pg, isScannedPage, textNodeThreshold)
   if(!any(textWords))
      return(FALSE)

   pageHasImg = sapply(pg, function(x) "img" %in% names(x))

   if(all(textWords) && all(pageHasImg))
      return(TRUE)


   txt = lapply(pg, function(x) unique(getPageText(x)))
   if(length(unique(unlist(txt))) == 1)
      return(SameTextOnAllPages = TRUE)

   if(isBioOne(doc) && all(sapply(txt[-1], length) == 0))
      return(BioOneScanned = TRUE)


   if(length(pg) > 2 &&  length(unique(txt[-1])) == 1)
     return(SameTextOnAllPagesExceptFirst = TRUE)

    # <img> nodes on each pages and put the page number as a name so we can group them by page.
  img = getNodeSet(doc, "//page/img")
  names(img) = xpathSApply(doc, "//page/img", function(x) xmlGetAttr(xmlParent(x), "number"))

    # the part that drops the first and last page is for those with a cover page added by the 
    # service that produced the document, e.g. a library.
  if(length(pg) > 0 && (all(pageHasImg) || (length(pg) > 2) && all(pageHasImg[-c(1, length(pg))]))) {
         # check the size of the image relative to the page size for each page
    y = sapply(img, imgSpansPage)

    if(all(y))
       return(ImagesSpanAllPages = TRUE)

        # need to group images by page
    byPage = tapply(y, names(img), any) # any may be too aggressive!

    if(length(byPage) > 2 && all(byPage[-c(1, length(byPage))]))
        return(c(TwoCoverPages = TRUE))
 
      # If only one page and the first page seems like a cover page and the second page
      # has an image that spans it.
      # May be overfitting to 3868345.xml
    if(length(pg) == 2 && any(grepl("ResearchGate|Downloaded|JSTOR", txt[[1]])) && textWords[2] & byPage[2])
        return(OneCoverPage = TRUE)
  }

  FALSE     
}

getPageText = 
function(node)
{
  xpathSApply(node, ".//text", xmlValue)
}

containsFigureCaption = 
function(x)
{
   grepl("Figure [0-9]+", xmlValue(x))
}

isScannedPage =
function(p, textNodeThreshold = 10, wordThreshold = 75)
{
  containsFig = any(xpathSApply(p, ".//text[contains(., 'Figure ')]", containsFigureCaption))
  if(containsFig)
     return(FALSE)

  tt = table(names(p))
  if(is.na(tt["text"]) || tt["text"] < textNodeThreshold)
     return(TRUE)
 
 
  nwords = sapply(p[names(p) == "text"], function(x) length(strsplit(xmlValue(x), "[[:space:]]")[[1]]))
  sum(nwords) < wordThreshold
}

imgSpansPage =
function(img, threshold = .8)
{
  p = xmlParent(img)

  a = as.integer(xmlAttrs(p)[c("width", "height")])
  b = as.integer(xmlAttrs(img)[c("width", "height")])
  all(b > (a * threshold))
}

sameFileName =
function(x, threshold = 3, proportion = .95, useEditDist = TRUE) {
    # Could use adist
    if(useEditDist)
       return(max(adist(tolower(basename(x)))) < threshold)
   
    nms = substring(basename(x), 1, max(proportion*nchar(basename(gsub("\\.pdf", "", x) ))))
    length(unique(tolower(nms)))
}




xmlFile =
function(name, EndNotePDFDir = getOption("EndNotePDFDir", "../NewData_May30_2017/PDF"))
{
  name = gsub("pdf$", "xml", gsub("^internal-pdf://", "", name))
  paste(EndNotePDFDir, name, sep = .Platform$file.sep)
}


getDocWords =
function(doc, text = pdfText(doc))
{
    if(!missing(doc) && is.character(doc))
       doc = readPDFXML(doc)

      # we can be smarter about sections, and excluding title, bibliog, etc.
    words = unlist(strsplit(unlist(text), "[[:space:][:punct:]]+"))    
}

