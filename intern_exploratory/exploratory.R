##########################################################
#intersect(findGlobals(findTable,rpdf))
setwd("/Users/sssantos/Documents/DSI_Internship")
library(devtools)
load_all('ReadPDF')

##########################################################
#library(ReadPDF)
f = "ReadPDF/tests/Lahm-2007-Morbidity\ and\ mortality\ of\ wild\ anim.xml"

#txt = getSectionText(f)

#lm = readPDFXML(f)
#h = findSectionHeaders(lm)
#idx = seq(1, length = length(h) - 1)
#secs = lapply(idx, function(i) getNodesBetween(h[[i]], h[[i+1]]))
#txt = sapply(secs, xmlValue)

##########################################################
ftables = getTables(f)
length(ftables)
View(ftables)
str(ftables)
doc = readPDFXML(f)
getNodeSet(doc, "//text[. = 'Table' or . = 'TABLE' or starts-with(., 'TABLE') or starts-with(., 'Table') or (. = 'T' and following-sibling::text[1] ='ABLE') or contains(., ' Table')]")

View(ftables)
