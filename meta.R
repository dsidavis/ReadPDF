library(XML); library(ReadPDF)
e = new.env()
invisible(lapply(list.files("R", pattern = "\\.R$", full = TRUE), source, local = e))
notFunc = ls(e)[!sapply(ls(e), function(x) is.function(get(x, e)))]
calls = lapply(setdiff(ls(e), notFunc), function(x) codetools::findGlobals(get(x, e)))

k = unlist(calls)
sort(table(k[ k %in% ls(e)]))
