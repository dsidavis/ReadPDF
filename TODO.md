# EID Abstracts
1. Handle the foot note markers in the author list in EID abstracts.
1. [done] generalize findEIDAbstract() not to use specific font number but get the text in column 1 above the horizontal
 line that is not bold.
 [6] "LatestDocs/PDF/3302321137/Aradaib-2010-Nosocomial outbreak of Crimean-C1.xml"

# pdftohtml

1. Figure out what the coordinates are in an image.

1. In Easterbrook-2007, p4  The figure has lines and is PDF but we don't see the lines in the plot
   when we plot that page in R.
1. Beta character in Wong et al  supplement.  See BadCharacters in pdftohtml for analysis. 
1. The + characters in table3 of Leroy-2004 are not present in the XML. No <text> node for them at
   all.
1. Also in Leroy-2004 words are put together LossiDec 2002 when should be 
1. findTable().  For Aguilar-2007, missing the >= in table 3. Not in XML. This is not just > but a
   symbol in  font. And it is getting ignored/dropped!
1. Are the image locations and dimensions (x, y) correct?  Do we need to transform them?
   See Klein-2011
1. spaces in Table 2 of Aguilar-2007  The 5 and 80 run together.
1. Spaces within a string are getting lost in pdftohotml - e.g., Fulhorst-2002  - page 1 - 'TexasParksand'
1. Check the links in pdftohtml. In Lahm, we only get 14 links to the bibliography items.
  There are 519 /Link elements in the uncompressed PDF.
1. Are the dimensions for the shaded rectangles correct from pdftohtml. Is linewidth transformed also?
   See Lahm[[ page 5]]  

# Todo list for ReadPDF

+ FIX fixTitleNodes().

+ Generally, fix the getDocTitle() function to be more accurate. See Status/TitleStatus.md


1. pdfText messes up text in different columns.  Check.

1. [check - think done]showNode for lines|rect.
     location and color
	 
1. getNodesBetween():  work with a line/rect node.	 
     + done but fix the hard coding of the col and index as 1 in findSection.R:getTextAfter().
	 
## Spreadsheets of results

## Misc
1. Find super and subscripts.

## getPublicationDate

1. Footer is not pulling in all information, e.g. when multiple fonts
   occur, getPageFooter() only returns one font/bounding box baseline
   set. Add some "wiggle" room to match close enough bboxes
   ??Example doc in which problem occurs?
   
1. Currently bails out after a "successful" match. May want to return
   all possible places where there could be a match, but this might
   result in too many overall matches. Revisit if we have issues with
   the single match/ current approach.
   
1. TextRegex: actually MonthName Year match, does not match any year. 

1. Check if the page number is in footers/headers looks like a
   date. "1997.pdf"

1. Check title before abstract, etc. Title is most often correct.

1. Long dash is converted to A-, messing up some functions but not
   others.

Notes/Issues:

Title is a lock, abstract seems to be less reliable, 

- Okamoto-2002: Picking up "2064 neuclotides" from abstract, NLP
  should help with this.
- Naish-2009: published in 2009, abstract lists 1992-2001.
- Brauburger-2012: not sure what the correct answer is here - the
  title has "45 years" of the virus
- Weissenbock-2013, pulling out of the abstract. 
- Heang-2012 Abstract broken. "To the Editor:" - don't look for
   abstracts. 
- Header issues: WENSMAN_et_al-2008
- Bennett-2000: not actually the footer (but correct answer!)
- Puorger-2010: "between 2004 and 2006" should catch this
- J Infect Dis.-2000-Papa: Picking up the reference on 1st page as
  footnote
- Linden-2012 : "Downloaded from XXXX" in header
- Nitatpattana: left hand side of dash not returned
- Adrian Diaz-2008: To the editor, got correct answer, but don't know
  why. Check
- [INVESTIGATE] read.csv: "# blah blah, blah" read in after the comma as a cell

## getSectionText()
1. When there are no section headers, except REFERENCES, collect the text from the body into its own
   unnamed section. See Degiorgis-2000.
   
1. The findSectionHeader() function should order the sections according to column, not document
   order.  See Becker-2012
   
1. Calzolari, Becker-2012
     Picking up an author name with the same font as the sections header we were looking for.
	 And the last one is on a line by itself because it is in the second column, but all the other
     nodes on that line are in the first column. But the real problem is that these nodes span the
     width of the page.  So  a) find the gap between the columns (end of one, start of next) and
     then see which lines do not have this gap between adjacent nodes (in nodeByLine) and then mark
     these as spanned. Or b) find any line that has text with content in the gap between the
     columns. 
	 So basically fix isOnLineBySelf() and pass it the getTextByCol()
	 Hack: Also remove any node that occurs "before" Abstract.
	 
1. [Fixed] Figuerola - Conclusions is coming up not on a line by itself but it is.	 
    Is it the table's columns throwing us off and going a little  too far to the right in defining
    the start of the second column?
	We weren't getting the column positions from the page, but from the doc.
	  


## Abstract
1. Fix getTextAfter to keep the text by column, e.g.  Tong...
1. Detect cover page for mbio papers, see Algaili et al.
1. Remove the footer material from the text included in the abstract. And header if spans multiple
   pages.
    copyright, Elsevier all rights reserved
1. Bennet-2000.  Dropping the lat part of the last sentence in the abstract.
1. In weisenbock-2013, don't include DISPATCHES. See comment in the code.
1. findAbstract() seems to have become slower when dealing with multiple pages and using
   nodesByLine() multiple times in tapply().
   See if we can use the page in the bbox.
   findAbstract("LatestDocs/PDF/3637917410/Meister-2008-Serological evidence of continui1.xml")
1. Use findSectionHeaders(, onlyFirst = TRUE) to find the Introduction/Background. Abstract should
   be before this.
    + "LatestDocs/PDF/1727052847/Tong-2004-Ross River virus disease in Australi.xml"
	   Can find the introduction. getNodesBetween() only gives the first column.
1. Find abstract between two lines
     "LatestDocs/PDF/1727052847/Tong-2004-Ross River virus disease in Australi.xml"
1. "LatestDocs/PDF/1727052847/Tong-2004-Ross River virus disease in Australi.xml" has keywords
    within the abstract "region"


1. [works] In Wernery, get the col positions correct. Currently returning -2. Is this a docFont =
   FALSE issue.	
   
## Publication date

## Dates
1. Convert the text into dates, i.e. extract date from DOI, Received, ...
1. Figure out the dates that aren't years, e.g. 3644, 3395, 3805, 4029, 4059.
    + 4059  - LatestDocs/PDF/1727052847/Tong-2004-Ross River virus disease in Australi.xml.   Part
      of address. Getting the abstract wrong.
	+ 3131, 2085 LatestDocs/PDF/2939921293/Holsomback-2009-Bayou virus detected in non-Or.xml
	   Address again. Abstract problem.
    + 3644  LatestDocs/PDF/0567227266/Papa-2001-Isolation of Dobrava virus from Apo1.xml
	   In text, not abstract. Abstract incorrect.
1. [check] Exclude For dates in the footer, see if "Downloaded on ... " e.g. Buitrago-2012
1. [check] Exclude Check Rotated "Downloaded...." 
1. getPublicationDate("LatestDocs/PDF/3409903038/9b53223b0c8d1a98f4831a14cbb3e0a5fe64.xml")
    gets specific dates from the text of the document. Not publication related.
   ```
     TextRegEx       TextRegEx       TextRegEx       TextRegEx 
    "July 1999" "26 April 1999" "21 April 1999"  "20 July 1999" 
   ```
1. [done] Get Date from Abstract Buitrago-2012
1. [done] If we fail to get any date, get it from the name of the file.


## Section headers
 5 minutes to process 71 documents.
```r
tt = readRDS("SP_SectionText.rds")
names(tt) = gsub("^../", "", names(tt))
len = sapply(tt, length)
b = tt[len >= 10]
system.time({tmp = lapply(names(b), findSectionHeaders)})
names(tmp) = names(b)
isNumbered = sapply(tmp, function(x) all(grepl("^[0-9]+(\\.[0-9]+)?", sapply(x, xmlValue))))
tmp = tmp[!isNumbered]
b = b[!isNumbered]
order(sapply(tmp, length))
```

1. ```
sections = lapply(sp.xml, function(x) try(findSectionHeaders(x)))
table(sapply(sections, class))
```
```
     array       list       NULL  try-error XMLNodeSet 
        31        257         84          5         31 
```

The errors
```
[1] "LatestDocs/PDF/2828631744/art%253A10.1023%252FA%253A1008199800011.xml"       
[2] "LatestDocs/PDF/3529243761/Pattnaik-2006-Kyasanur Forest disease_ an epid.xml"
[3] "LatestDocs/PDF/2999137579/Wong et al 2007 supplement.xml"                    
[4] "LatestDocs/PDF/0148058638/Wong_et_al-2007-Reviews_in_Medical_Virology.xml"   
[5] "LatestDocs/PDF/3246714993/bok%253A978-3-540-70962-6.xml"                     
```
```
is.scanned = sapply(sp.xml[sapply(sections, is.null)], function(x) try(isScanned(x)))
table(is.scanned)
```

The documents for which we got NULL for the sections and which are NOT scanned:
```
sp.xml[sapply(sections, is.null)][!is.scanned]
 [1] "LatestDocs/PDF/1601876396/OIE Iran.xml"                                      
 [2] "LatestDocs/PDF/2430316441/OIE Kuwait.xml"                                    
 [3] "LatestDocs/PDF/3814962940/OIE Oman.xml"                                      
 [4] "LatestDocs/PDF/3982771992/Leroy-2004-Multiple Ebola virus transmission e.xml"
 [5] "LatestDocs/PDF/2364497871/leroy et al 2005.xml"                              
 [6] "LatestDocs/PDF/1217382941/Barrette-2009-Discovery of swine as a host fo1.xml"
 [7] "LatestDocs/PDF/4154443567/Barrette-2009-Discovery of swine as a host for.xml"
 [8] "LatestDocs/PDF/3267708254/Quaglia-2014-West Nile and st. Louis encephali.xml"
 [9] "LatestDocs/PDF/0818313444/vir.0.81576-0-SuppTableEdited.xml"                 
[10] "LatestDocs/PDF/3342055963/08-0359_appT-s1 (2).xml"                           
[11] "LatestDocs/PDF/1502738312/Lundkvist-1998-Human Dobrava hantavirus infect.xml"
[12] "LatestDocs/PDF/0613064798/Plyusnin-1999-Dobrava hantavirus in Russia.xml"    
```
1. For NULL values returned, indicate no sections, perhaps list().


1. Get keywords as a section.

1. [fixed] Leroy-2004  - finding "Materials and Methods" under the "Supporting Online Material". 
   For the same paper, we run into this with the "Table S1" - see below.

1. Brauburger-2012 - get header content page number and year (both look like years)
1. Develop getPageHeader/getHeader and footer versions.

1. Venter-2010 - findSectionHeaders() includes the header for the pages "VENTER AND SWANEPOEL" and
   "WNV LINEAGE 2 PATHOGENESIS"

1.  Blasdell - table 1 is a great example of containing all the data we want.
1.  Also Linke table 3 another example of where the data are that we want.

1. Klein - gets   getColPosition() wrong for perPage = TRUE or FALSE. Hence isOnLineBySelf() fails.
    And we need that for determining if the section titles are on their own line.

1.  LatestDocs/PDF/0212899111/Levis-2004-Hantavirus pulmonary syndrome in no.xml
    Matching CA).
	Check on line by itself.
	
1. Matching too many - References???   But more than that.

    + LatestDocs/PDF/0263146437/Biernat-2014-Study on the occurrence of tick-b.xml	
    + LatestDocs/PDF/0337534517/Andriamandimby-2011-Crimean-Congo hemorrhagic.xml"   	
    + LatestDocs/PDF/0672362859/Bosch-2007-West Nile Virus, Venezuela.xml	 + authors?
    + LatestDocs/PDF/2150982356/Brauburger-2012-Forty-five years of Marburg vi.xml
       50 page document. Sections are clear but picking up 92 of them	
    + LatestDocs/PDF/3982771992/Leroy-2004-Multiple Ebola virus transmission e.xml
	+ LatestDocs/PDF/4154443567/Barrette-2009-Discovery of swine as a host for.xml
    + LatestDocs/PDF/2939921293/Holsomback-2009-Bayou virus detected in non-Or.xml
	+ LatestDocs/PDF/0817727758/Klein-2011-Hantaan virus surveillance targetin.xml
	+ LatestDocs/PDF/2735769979/VandeWoude-2006-Going wild_ lessons from natur.xml
	+ LatestDocs/PDF/1609915988/McIntosh-1976-Culex (Eumelanomyia) rubinotus T.xml
	
    + [This may be correct as it is a 35 page doc with a table of contents]
	   LatestDocs/PDF/0817727758/Klein-2011-Hantaan virus surveillance targetin.xml

1. Fix isScanned - LatestDocs/PDF/1609915988/McIntosh-1976-Culex (Eumelanomyia) rubinotus T.xml
	 But isScanned() and isScanned2() say no!
	 Hjelle-1995 also scanned.
	 Rudnick also: LatestDocs/PDF/3257936385/Rudnick-1965-Studies of the ecology of Dengue.xml
	 
1. Getting the author names
    LatestDocs/PDF/0368782170/Chew-2000-Risk factors for Nipah virus infecti.xml	
    LatestDocs/PDF/0382058825/Rihtaric-2010-Identification of SARS-like Coro.xml

1. Combine the text on the same line.

1. Names of the sections have extra spaces within word
    LatestDocs/PDF/0851236576/Chevalier-2010-Environmental risk factors of W.xml

1. OKAY - 
    Numbered sections
    + LatestDocs/PDF/0415231817/Yang-2010-Simultaneous typing and HA_NA subtyp.xml
    + LatestDocs/PDF/0609202356/Charrel-2003-Arenaviruses other than Lassa vir.xml
	+ LatestDocs/PDF/0779208162/Bowden-2001-Molecular characterization of Mena.xml
	

## Tables

1. [check] When combining nodes on a line in, e.g., Forrester-2008, get nodesByLine() correct.
   The b/< characters have @top=149 & @height=12 and the number have @top=151 & @height=10
   We may want to group by @top + @height.
   
1.  useBase = TRUE in nodesByLine().  How does it perform with superscripts. See Alagaili...2014
    and Table 1's column headers.

1. Read the tables back to data frames, arranging each line into columns, but determining the columns
   across all lines first.

1. Read the footnotes. Make sense of them!!

1. Determine the caption, e.g., above the first line

1. **Remove** any footer line that spans the entire page on all pages before looking for tables.

1.  Recognize Table XX  in the text as not a table identifier.  See Leroy-2004 - "Table S1" at the
    very end of the article that refers to supporting online material.
	We'll just end up with 0 rows for the table and can discard.

1. identify tables and put the related nodes into a table node and then potentially write the result
  back to the original file so we have that information for subsequent reads of that document.

1. Look for lines separating rows in tables.

## Specific Tables

1. Schmaljohn-1997 - 2 complex tables. one which spans 1 1/4 columns.
    <br/>
	getTextByCols() is returning 4 elements, but getColPositions() gives just 2.
	<br/>
	Almost works out of the box, but doesn't include the right-most column.
	<br/>
	For table that partiall goes into another column, check the line endings of the text
	within the vertical region and see if there is a big enough gap/margin.
	<br/>
	Table 1 is on a page all by itself. Its contents are not in the doc font - not a single text element
	with the doc font on that page. So getTextByCols() and getColPositions() fail to return anything.
	We need getColPositions(, docFont = FALSE).  **ADDED NOW**
	
1. Brauburger-2012 - single column long paper.  Tables continued across pages.
   Kariwa-2007 also continued.

1. Neel-2010 - a rotated table.
   + Also, Nelson-2010
   + "LatestDocs/PDF/2999137579/Wong et al 2007 supplement.xml" - 6 pages of rotated tables and no
   text. 
   + For Neel, page 5:   all the text is rotated 90 except 5 nodes which are the header for that page.
   Can we detect this and then change the bbox to treat  x0 as y0 and x1 as y1 and reorder the
   dimensions of the page.

1.  [**!!**] "1351986620/J Infect Dis.-2015-Ogawa-infdis-jiv063.xml" - tables with rows with alternating  colors.
    + [used to work, I think] Table 2.  Has <rect> not <line>
	    Gets the header and first row, but not the remaining rows.
		The rows have alternative colors. Can we exploit this to identify 
   	  [previously] Now gets more than we want. Includes line from other column from Figure 3 and much f the caption
      and then from the 2nd  column below the table and the "Downloaded from " which is rotated text.

    + [works] Table 1  (which comes second)
	
1. [table 2 broken] Armien-2004 - good example of table<br/>	
 ```r
 names(findTable(getTables(ar)[[2]]))
 ```
 
## Tables Work

1.  [works] Nitatpattana-2008, 
     Table 1 - finds the table, but the bottom line is actually two half lines so the span code
     doesn't find it since neither span the entire column.
	 In fact, the two lines overlap and do not meet at the same point as in other document.s
	 See combineLines and combineBBoxLines.
    [works] Table 2
	 ```r
     tt = getTables(nit); findTable(tt[[1]])
      ```
1. [works] Armien-2004 - good example of table<br/>
     [**works**]
     [no caveats now afer adjusting getTextByCols(), etc. to compute getColPositions() across entire document.]
     Table 2.  Table2 is not considered centered. getColPositions() returns only 1. This is because
     References are in the second colun and are numbered and indented so very few in.
	 If we specify the column breaks ourself, based on page 4, it works <br/>
   ```r
     tt = getTables(ar)   
	 names(findTable(tt[[2]], colNodes = getTextByCols(ar[[5]], breaks = c(79, 474), asNodes = TRUE)))
	 names(findTable(tt[[2]]))
   ```
    [works]  Table 1 

1. [works] Can't detect Klein-2011 - lines don't span all the way across the page. But no text to the right.
    But many additional lines.
     `names(findTable(getNodeSet(k, "//text[contains(., 'Table 1')]")[[1]]))`

1. [works] Weaver-2001  - table 2 - getColPositions() has 5 columns because the table dominates.
      [this part fixed now.]  getColPositions() uses the id of the most common font (getDocFont()) to find the
      relevant text, and so excludes the tables, etc.

1. [works] Padula-2002 - 1 table spans 2 columns

1. [Works] table 3 in Fulhorst - spans width of page.
     Thinks there is only one column. So getColPositions() needs work because of the image in the
     second column. 
   ```r
     tt = getTables(fu)
	 names(findTable(tt[[3]]))
   ```

1. [works] 3 columns:  3982771992/Leroy-2004-Multiple Ebola virus transmission e.xml
     + [works] Table 2 and 3 span 2 columns.
     + [works] table 1 - spans entire page.

1. [works] NipahAsia



## No tables - Correct
1. [and none found by getTables() - correct] No tables,  Kang-2010, Halpin-2000, Culley-2003
	 
# General

1. [fixed with `perPage = FALSE`] getColPositions() - see Armien-2004 p5.

1. Section title: Look for text on a line on its own, a little separated from next line and not
   taking up the entire column width.
   ```r
   findShortLines(getTextByCols(wv[[2]], asNodes = TRUE)[[1]])
   ```
   Then we see the lines that don't span the entire column and also the ones that start with an
   indentation.
   See also findShortSectionHeaders()

1. Compute document-wide interlineskip.
   Get all the @top from the text nodes on a page.
   Group them by line
   order the lines
   compute difference 
   ```r
   ptops = as.numeric(unlist(getNodeSet(wv[[2]], "//text/@top")))   
   pcut = split(ptops, cut(ptops, seq(0, 1200,by=13)))   
   pcut = pcut[ sapply(pcut, length) > 0]
   diff(sapply(pcut, min))
   ```

1. identify abstract and put it in its own node.

1. Find text within shaded region.  Put the text nodes in a <shaded> node.
	 
1. Remove header and footer material from getTextByCols()

1. [check] Find superscripts that are citations and remove them from the text.
   See findBibCites()

1. [check] Group segments that have very close tops together.
   Implemeted in nodesByLine().
   See isCentered() where we combine segments into lines.  Move this code out to a separate function.

1.  So getColPositions() needs work because of the image in the
     second column.  See Fulhorst-2002 age 4.

1. Similarly, can add 1-column, 2-column, etc around the text, which column and where the columns
  start and end.  
  

1. In getNodesBetween(), we should arrange the text by line and within line from left to right.
  See
   getTextByCols() should do this.
   We do this for isCentered().
   Need to deal with the top values being one or two units apart for segments on the same line.

1. Identify section starts and ends, i.e. section titles. 
     + Got some extras and missed DISCUSSION in 3234834982/Fulhorst-2002-Natural host relationships
       and 1.xml
	       Also the s of Merriam's is running into pocket mice.   Two itaclic segments on that line.
		   The problem is that isCentered() is failing. The top for this text is 490. There is
           another set of tops at 489 which are the italic parts. So we need group these properly.
	    
	 + Nothing in 2688324473/Beltrame-2006-Tickborne encephalitis virus, no.xml but makes sense -
       letter to editor.
	   Same with 4021195741/Shepherd-1987-Antibody to Crimean-Congo hemorr.xml
	 + Extras we don't want in  2939921293/Holsomback-2009-Bayou virus detected in non-Or.xml
	     GOt rid of many. Remaining ones are in table on page 5.
	 + Not picking up The Study in LatestDocs/PDF/3574543168/Thoisy-2003-Mayaro virus in wild mammals, Fren.xml
     + 0657742708/Lai-2007-Cost-effective real-time reverse tran.xml 
	     pick up RESULTS and DISCUSSIONS.  MAterials and methods is smaller font both in section
	     title and text of that section.
	 + 3151144403/Luby-2006-Foodborne transmission of Nipah viru.xml
	     picking up names of authors as well as section titles.  These names span the entire page
	     but there are columns. So ignore these.
	 + 2796096355/Aguilar-2007-Endemic Eastern equine encephalit.xml picking up sentence that is
       first in paragraph and indented.  Same issue as with Fulhosrt above - two segments with very
       close top values that are not considered part of the same line.
	   So put them into the same line and all will be well.
	 + 3932331692/Nitatpattana-2008-Change in Japanese encephali.xml picking up names which are
       centered within the first column!
	 + LatestDocs/PDF/2081396765/Neel-2010-Molecular epidemiology of simian imm.xml
	     gets too much, some from the abstract which spans the two columns but is indented.
	   
     + LatestDocs/PDF/3385699523/Holzmann-2010-Impact of yellow fever outbreaks.xml
	   Gets some extra parts, e.g. 2010 Wiley-Liss, ``
	     Is the `` on page 2 on the line with a paragraph  indentation - "An outbreak has been
	   defined"  col 1, halfway down.
	     
	   
	 + 3136760279/Tauro-2012-Serological detection of St. Louis.xml" is correct, but there are also
       paragraph titles that are interesting/useful, e.g. Study site, Sample collection which are
       italics and followed by a - at the start of a paragraph.
	   
	 + 3982771992/Leroy-2004-Multiple Ebola virus transmission e.xml  - nothing and this is correct.

     + Good: 2956441632/Cui-2008-Detection of Japanese encephalitis vi.xml
	 
	 + Numbered sections: 1347402211/Luis et al_2014_A comparison of bats and roden.xml
	    Also has valuable sub-section titles.
		
		Numbered: 3512447895/Hara-2005-Isolation and characterization of a1.xml

1. Look for text at the start of a paragraph that starts with italics or a font.
    Aguilar-2007
	
1. Make isCentered() faster.

1. Not picking up sub-section titles, intentionally.
   See 3133228518/Murphy-2006-Implications of simian retroviruse.xml for example.

1. ?Include unnumbered sections in documents with numbered section headers, e.g. Lahm and
   Acknowledgements, References.  Do we care?

1. [manually check] For Weaver & Lahm, finish getting the text for sections.

1.  When finding section headers, check if the templates we find are centered annd check others that
    have the same font are also centered.
    See 3618741902/Armien-2004-High seroprevalence of hantavirus.xml   
	 Weaver and Klein also have centered sections.

1. [check works] in getColPositions() if values are too close together drop the right one.
    Weaver page 4.  470 and 471.
	To do with indendented first line of paragraph.  Increase threshold.
	But getTextByCols() has no nodes in the 470 one.

1.  When getting nodes in getTextAfter, recognize tables at the top or bottom of the page and skip
  over them. Weaver p6
 ```r
     h = findSectionHeaders("LatestDocs/PDF/0629421650/Padula-2002-Andes virus and first case report1.xml")  
     sapply(getTextAfter(h[[10]], h[[11]]), xmlValue)
 ```

1.  Find tables and figures
    1.  Captions, titles, etc.
    1.  Avoid references to tables.
	
1.  Implement getHeader and footer. See Lahm-2007 with lines at the top of the page.

1.  Find abstract and if it spans the entire page, don't include it when computing columns.	

1. For 2 or more columns, detect the part which is only one column spanning the entire page.
	

1.  For getColPositions() take the entire document into account and take the most common.
    Give the parts after References/Bibliography less weight. These are often indented due to the
    number so we don't get much text starting at that point
    See 3618741902/Armien-2004-High seroprevalence of hantavirus.xml
	
1.  exclude shaded boxes when computing column positions. And images. and tables.
    See Lahm-2007

1.  The box in the left side of the page doesn't appear to be as wide as in the PDF.
  This is the keyword box.
  "Zoonotics/...PDF/0809541268/Kitajima-2009-First detection of genotype 3 he.xml"
  This comes up in the splitElsevierTitle() and why we put the no filter of nodes if no y > yl.
    +  See 3569325249/Scherret-2001-The relationships between West N.xml for a good example.
  
1.  Get all of the elements in the title even if changed font
  i.e. identify title and then find all the elements near these that make up 
  the lines.
  Have to deal with spanning 2 columns and may not be part of the title and many other issues.
  e.g. 1834853125/394.full.xml  

1.  getColPositions:  when first line of paragraph is indented, we don't get the critical mass at the
  same point.
  See 0337534517/Andriamandimby-2011-Crimean-Congo%20hemorrhagic.xml

1.  Reassemble the elements of a word, line, paragraph from the different <text> elements
    **See nodesByLine()**
     + See code we had in an earlier package for this.

1. Detect 2 columns when one is mostly a figure and not words.
   Figure out columns for all pages and correct if one or two pages seems to be single column.



# Done

1. [done] Rationalize getFontInfo() and fontInfo() functions.
     fontInfo() gone. getFontInfo() now returns the full data frame and uses the font id as row
     names.

1. [done] Error from isScanned2("LatestDocs/PDF/2143276081/Kamhieh-2006-Borna disease virus (BDV)
   infect1.xml")
   
1. [fixed] getDatePublished() for Aguilar-2007 gives NULL but info at the end - April 8, 2006
    The version that was in Zoonotics-shared and now in ReadPDF works fine.

1.  [Done] Find font for the majority of the text.
