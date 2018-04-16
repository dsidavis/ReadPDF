
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
w = sapply(bad.groups[[4]], isEID)
table(w)
```
```
FALSE  TRUE 
    7    13 
```
So we can deal with these with special code.

## EID Documents
The abstract is in the first column
and it is below the author list which is below the title,
but above a horizontal line in the first column.
So we can write a function to extract this.

In fact, 
"LatestDocs/PDF/3569325249/Scherret-2001-The relationships between West N.pdf"
does not have this pattern. It uses a different format, but is an EID article.
Here the abstract is more traditionally centered.
So  we want to use the regular mechanism.


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



Let's start by looking at the abstracts for which we did not get an error.
```
table(sapply(abs[!err], class))
```
```
array  list 
  264   117 
```

The array comes from nodesByLine() and specifically the call to tapply().
Even with `simplify = FALSE`, this returns an array.  
So we move to lapply(split()) to ensure a list.



```
len = sapply(abs[!err], length)
summary(len)
```
```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    0.00    9.00   12.42   18.00  127.00 
```
113 of these have 0 elements.
Some of these we miss; some legitimately have no abstract; and 72 are scanned (so probably we cannot
extract the abstract  with these tools).

There are only 45 that 
```
scanned = sapply(names(abs)[!err], isScanned); names(scanned) = names(abs)[!err]
```

There are 45 documents which have no error, are not scanned and have 0 elements in the abstract we recovered:
```
names(scanned)[ !scanned & len == 0 ]
```

These are listed below and divided into which have an abstract and which do  not.

### Do not have an abstract.

 [1] "LatestDocs/PDF/3342055963/08-0359_appT-s1 (2).xml" - 1 page table                           
 [5] "LatestDocs/PDF/2853897527/978-1-4020-6106-6.xml"   - book
[10] "LatestDocs/PDF/2688324473/Beltrame-2006-Tickborne encephalitis virus, no.xml" - no abstract,short doc.
[13] "LatestDocs/PDF/0672362859/Bosch-2007-West Nile Virus, Venezuela.xml"          - no abstract,short doc
[22] "LatestDocs/PDF/0247828304/Hofle-2013-Usutu virus in migratory song thrus.xml"  - no abstract,short doc
[40] "LatestDocs/PDF/0818313444/vir.0.81576-0-SuppTableEdited.xml"                  - tables only supplementary material.


### Ambiguous
[36] "LatestDocs/PDF/2498380585/REUSKIN JOrdan.xml"      - first paragraph in a different color.
[37] "LatestDocs/PDF/0253022084/Rijks-2016-Widespread Usutu virus outbreak in.xml" - same as REUSKIN above


### Has an Abstract

 [2] "LatestDocs/PDF/3454570876/1-s2.0-S0042682200905634-main2.xml"          Virology centered across 2 columns
 [3] "LatestDocs/PDF/0649016555/1-s2.0-S0042682297988401-main.xml"                 virology
 [4] "LatestDocs/PDF/1249277242/177-3-529.xml"                                     
 [7] "LatestDocs/PDF/1288179177/Asper-2001-First outbreak of callitrichid hepa.xml" - virology again.
 [8] "LatestDocs/PDF/1217382941/Barrette-2009-Discovery of swine as a host fo1.xml" - 3 columns with title and abstract spanning the first two. Calculate the number of columns, find text that spans  more than one. This will work for 2 columns with abstract spanning both.
 [9] "LatestDocs/PDF/4154443567/Barrette-2009-Discovery of swine as a host for.xml" - duplicate of previous one with a back cover page
[12] "LatestDocs/PDF/0753887278/Blasse-2013-Mother-offspring transmission and1.xml" - spans 2 columns
[14] "LatestDocs/PDF/1436485989/Breman 1999.xml"                                   centered across 2
         cols. Complicated by author affiliation above in 2 separate horizontal blocks 
[16] "LatestDocs/PDF/3887698401/Chua-2001-Tioman virus, a novel paramyxovirus1.xml" - centered and  indented a little.
[18] "LatestDocs/PDF/0840057771/Degiorgis-2000-Borna disease in a free-ranging.xml" - centered,
      indented ends with a horizontal line.
[19] "LatestDocs/PDF/3551992108/Dumpis 1999.xml"                               like Breman above.
[20] "LatestDocs/PDF/3364104101/Ergönül-2006-Crimean-Congo haemorrhagic fever1.xml" - spans 2    columns. Complicated by 3rd "margin" column with text.
[21] "LatestDocs/PDF/0991626011/Formenty-1999-Ebola virus outbreak among wild.xml"  - like Breman
[24] "LatestDocs/PDF/4252077711/J. Virol.-2013-Galvin-JVI.03555-12.xml"            abstract on page 2.  Proofs version
[27] "LatestDocs/PDF/3982771992/Leroy-2004-Multiple Ebola virus transmission e.xml" - spans 2 of 3 columns
[33] "LatestDocs/PDF/0532875827/Peeters-2002-Risk to human health from a pleth.xml" - centered. is EID but different
[29] "LatestDocs/PDF/3475635737/Nakgoi-2014-Dengue, Japanese Encephalitis and.xml" - indented on  left, but not right. Spans 2 columns
[30] "LatestDocs/PDF/1082165137/Nandi-2000-A novel type D Simian retrovirus n1.xml" - spans 2 cols, centered and indented
[31] "LatestDocs/PDF/3291406011/Okamoto-2000-Species-specific TT viruses and c.xml" - spans 2 cols, centered and indented
[32] "LatestDocs/PDF/0567227266/Papa-2001-Isolation of Dobrava virus from Apo1.xml" - spans 2 cols, centered and indented and ends with a line
[34] "LatestDocs/PDF/3138036620/Philbey-2008-Inifection with Menangle virus in.xml" - info in left column. Has sections within this such as Results, Conclusions, Wildlife& Zoos
[35] "LatestDocs/PDF/1674687958/Remmers-2000-Longitudinal studies in the epide.xml" - spans 2 cols, centered and indented
[42] "LatestDocs/PDF/3607820776/Weaver-1997-Recombinational history and molecu.xml" - spans 2 cols, centered and indented
[43] "LatestDocs/PDF/3602790318/Weaver-2001-Extreme genetic diversity among Pi.xml" - spans 2 cols, centered and indented
[44] "LatestDocs/PDF/1052790441/Weissenböck-2002-Emergence of Usutu virus, an.xml"  - EID but different format. Spans 2 columns, centered.
[45] "LatestDocs/PDF/1384762250/Yob-2001-Nipah virus infection in bats (order.xml"  - EID but different format. Spans 2 columns, centered.



### Working EID
"LatestDocs/PDF/3025013874/Blasdell-2008-Host range and genetic diversity.xml" 
"LatestDocs/PDF/0873940951/Cao-2011-Tembusu virus in ducks, china.xml"       
"LatestDocs/PDF/2475418629/Iehlé-2007-Henipavirus and tioman virus antib1.xml" 
"LatestDocs/PDF/1161594151/Wang-2009-Japanese encephalitis viruses from b.xml"
"LatestDocs/PDF/0215227208/Klempa-2008-Hemorrhagic fever with renal synd1.xml" 
"LatestDocs/PDF/2243954052/Milazzo-2012-Geographic distribution of hantav.xml"
"LatestDocs/PDF/1827651569/Swanepoel-2007-Studies of reservoir hosts for.xml" 
"LatestDocs/PDF/3302321137/Aradaib-2010-Nosocomial outbreak of Crimean-C1.xml"
"LatestDocs/PDF/0683734075/Tagliapietra-2009-Spatial and temporal dynamic.xml"
   getColPositions() is giving only one value, not 2. So failing to get the text.

NewPDFs/El Moro Canyon Virus/Milazzo-2012.xml

Different format.
"LatestDocs/PDF/2987363901/Kirkland-2015-Hendra Virus Infection in Dog, A.xml"



"LatestDocs/PDF/3867373015/Coffey-2006-Serologic evidence of widespread E.xml" EID, but title and author list span 2 cols, lot of space between abstract text and title.




NewPDFs/Australian Bat Lyssavirus/Arai-2003.xml
NewPDFs/Australian Bat Lyssavirus/Arguin-2002.xml
NewPDFs/Australian Bat Lyssavirus/Mackenzie-2001.xml
NewPDFs/Australian Bat Lyssavirus/Warrilow-2003.xml
NewPDFs/Barmah Forest Virus/Lindsay-1995.xml
NewPDFs/Crimean Congo Hemorrhagic Fever Virus/Leblebicioglu-2014.xml
NewPDFs/Crimean Congo Hemorrhagic Fever Virus/Mustafa-2011.xml
NewPDFs/Crimean Congo Hemorrhagic Fever Virus/Nabeth-2004.xml
NewPDFs/Crimean Congo Hemorrhagic Fever Virus/Palomar-2013.xml
NewPDFs/Dobrava Virus/Oktem-2014.xml
NewPDFs/Dobrava Virus/Papa-2006.xml
NewPDFs/Dobrava Virus/Schlegel-2009.xml
NewPDFs/Duvenhage Virus/Paweska-2006.xml
NewPDFs/European Bat Lyssavirus 1/Serra-Cobo-2002.xml
NewPDFs/European Bat Lyssavirus 1/Van der Poel-2005.xml
NewPDFs/European Bat Lyssavirus 1/Vazquez-Moron-2011.xml
NewPDFs/Getah Virus/Nemoto-2015.xml
NewPDFs/Granada Virus/Arai-2003.xml
NewPDFs/Granada Virus/Lumlertdacha-2005.xml
NewPDFs/Guaroa Virus/Groseth-2015.xml
NewPDFs/Irkut Virus/Lumlertdacha-2005.xml
NewPDFs/Khujand Virus/Lumlertdacha-2005.xml
NewPDFs/Lagos Bat Virus/Hayman-2008.xml
NewPDFs/Lagos Bat Virus/Markotter-2006 2.xml
NewPDFs/Lagos Bat Virus/Markotter-2006.xml
NewPDFs/Lagos Bat Virus/Sabeta-2007.xml
NewPDFs/Laguna Negra Virus/Suzuki-2004.xml
NewPDFs/Laguna Negra Virus/Travassos da Rosa-2012.xml
NewPDFs/Mayaro Virus/de Thoisy-2003.xml
NewPDFs/New York Virus/Knust-2013.xml
NewPDFs/Oropouche/Nunes-2005.xml
NewPDFs/Seoul Virus/Wang-2016-Hemorrhagic Fever with Renal Syndrom.xml
NewPDFs/Seoul Virus/Zhang-2009-Hantaviruses in rodents and humans.xml
NewPDFs/Seoul Virus/Zhang-2010-Hantavirus infections in humans and.xml
NewPDFs/Tahyna Virus/Gould-2006.xml
NewPDFs/Tula Virus/Oktem-2014.xml
NewPDFs/West Causcasian Bat Virus/Kuzmin-2008.xml
NewPDFs/Western equine encephalitis/Roth-2010.xml


#### These three are quite different and have a centered, indented regular abstract
NewPDFs/Australian Bat Lyssavirus/Field-1999.xml
NewPDFs/Dobrava Virus/Scharninghausen-1999.xml
NewPDFs/Monongahela Virus/Rhodes-2000.xml

#### 3 column EID document.
NewPDFs/Seoul Virus/Yao-2013-Seoul virus in rats (Rattus norvegicu.xml


#### Bad getColPositions()
These give 71 and 475 respectively.
"NewPDFs/Australian Bat Lyssavirus/Mackenzie-2001.xml"                  
"NewPDFs/El Moro Canyon Virus/Milazzo-2012.xml"                         

MacKenzie has an image in the 2nd column.  Also, not regular format.Centered abstract and title.  Only one line.
Need getColPositions() to take the image into account. But our image data is wrong.

[done] When getColPositions() finds a column in the middle, it should check if it can figure out
       if there is a column before this.

These give an empty column position vector. There is nothing special about them.
"NewPDFs/New York Virus/Knust-2013.xml"                                 
"NewPDFs/Seoul Virus/Zhang-2009-Hantaviruses in rodents and humans.xml" 



### len == 2 & !scanned

+ XXX  "LatestDocs/PDF/4090273238/ch4.xml"  An introduction, but no abstract.
  But we get the title and author names in the abstract which is wrong.

+ "LatestDocs/PDF/4090273238/ch4.xml"]]  Accepted... and SUMMARY
