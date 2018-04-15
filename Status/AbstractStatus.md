
# Abstracts

```
abs = structure(lapply(sp.xml, function(x) try(findAbstract(x, asNode = TRUE))), names = sp.xml)
```

There are many errors.
```
err = sapply(abs, is, 'try-error')
table(err)
```
```
FALSE  TRUE 
  302   106 
```


```
table(unlist(abs[err]))
```

```
                                                                      Error : 1: PCDATA invalid Char value 12\n\n 
                                                                                                                2 
                                                                Error : XML content does not seem to be XML: ''\n 
                                                                                                                2 
                            Error in dimnames(x) <- dn : \n  length of 'dimnames' [2] not equal to array extent\n 
                                                                                                                1 
                                             Error in findAbstract(x, asNode = TRUE) : try within single column\n 
                                                                                                               20 
                                                           Error in strsplit(tmp, ",") : non-character argument\n 
                                                                                                               80 
Error in tapply(nodes, pgnum, nodesByLine, asNodes, baseFont = baseFont,  : \n  arguments must have same length\n 
                                                                                                                1 
```

```
bad = names(abs)[err]
bad.groups = split(bad, unlist(abs[err]))
```

Let's focus most immediately on the 80 problems with strsplit.

By debugging a call to the first document in this group, we see that there
is a simple problem dealing with no nodes in getBBox2(). We create an
empty return object, but don't return it.  So we add a return().


## "try within single column"

By looking at these documents very quickly (simply opening them with opdf
and watching them dispaly), we recognize that many of them are papers
from the Emerging Infectious Disease journal. We already have an
isEID() function to check for these.
```
w= sapply(bad.groups[[4]], isEID)
table(w)
```
```
FALSE  TRUE 
    7    13 
```
So we can deal with these with special code.

The abstract is in the first column
and it is below the author list which is below the title,
but above a horizontal line in the first column.
So we can write a function to extract this.



Of the remaining 7, the last (LatestDocs/PDF/2608848674/1663497.xml)
has a cover page (from AAAS) and the actual paper is scanned.
And there is no abstract in the cover sheet or in the paper itself.


Of the remaining 6, the following three do not have an abstract:
```
[1] "LatestDocs/PDF/2364497871/leroy et al 2005.xml"                              
[4] "LatestDocs/PDF/1502738312/Lundkvist-1998-Human Dobrava hantavirus infect.xml"
[5] "LatestDocs/PDF/1966195136/Linden-2012-Tickborne encephalitis virus antib.xml"
```
and these three do 
```
[2] "LatestDocs/PDF/1594807244/Plowright-2008-Reproduction and nutritional st.xml"
[3] "LatestDocs/PDF/1556850690/Buckley-2003-Serological evidence of West Nile.xml"
[6] "LatestDocs/PDF/1991155481/87.xml"                                            
```
Of the 3 that do, 2 are from the Journal of General Virology
and have a  gray box on the left of the page and the title, abstract, etc. to the right of
this with horizontal lines separating the abstract text above and below.

For Plowright, the abstract is above Keywords and below the author list and affiliation
and is centered and slightly indented relative to the regular text.


## Checking Abstracts

1. Text length too small
1. Text length too short
1. Text is all or significant % of the first page.
1. Fonts in abstract appear to be section formatting, etc.,  rather than italics
1. Nodes have bold fonts.
