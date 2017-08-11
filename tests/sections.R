library(ReadPDF)
f = "Lahm-2007-Morbidity and mortality of wild anim.xml"
txt = getSectionText(f)

#lm = readPDFXML(f)
#h = findSectionHeaders(lm)
#idx = seq(1, length = length(h) - 1)
#secs = lapply(idx, function(i) getNodesBetween(h[[i]], h[[i+1]]))
#txt = sapply(secs, xmlValue)


