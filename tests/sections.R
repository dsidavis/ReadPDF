library(ReadPDF)
f = system.file("Lahm-2007-Morbidity and mortality of wild anim.xml", "samples", package = "ReadPDF")
txt = getSectionText(f)

#lm = readPDFXML(f)
#h = findSectionHeaders(lm)
#idx = seq(1, length = length(h) - 1)
#secs = lapply(idx, function(i) getNodesBetween(h[[i]], h[[i+1]]))
#txt = sapply(secs, xmlValue)


