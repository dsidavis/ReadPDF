1. Are the dimensions for the shaded rectangles correct from pdftohtml. Is linewidth transformed also?
  See Lahm[[ page 5]]

1. [check works] in getColPositions() if values are too close together drop the right one.
    Weaver page 4.  470 and 471.
	To do with indendented first line of paragraph.  Increase threshold.
	But getTextByCols() has no nodes in the 470 one.

1.  When getting nodes  in getTextAfter, recognize tables at the top or bottom of the page and skip
  over them. Weaver p6
 ```r
     h = findSectionHeaders("LatestDocs/PDF/0629421650/Padula-2002-Andes virus and first case report1.xml")  
     sapply(getTextAfter(h[[10]], h[[11]]), xmlValue)
 ```

1.  For Weaver & Lahm, finish getting the text for sections.

1.  Find tables and figures
    1.  Captions, titles, etc.
    1.  Avoid references to tables.
	
1.  Implement getHeader and footer. See Lahm-2007 with lines at the top of the page.

1.  Find abstract and if it spans the entire page, don't include it when computing columns.	
	
1.  Find text within shaded region.

1.  For getColPositions() take the entire document into account and take the most common.
  Give the parts after References/Bibliography less weight. These are often indented due to the
  number so we don't get much text starting at that point
    See 3618741902/Armien-2004-High seroprevalence of hantavirus.xml
	
1.  exclude shaded boxes when computing column positions. And images. and tables.
  See Lahm-2007
	
1.  When finding section headers, check if the templates we find are centered and check others that
  have the same font are also centered.
    See 3618741902/Armien-2004-High seroprevalence of hantavirus.xml   

1.  The box in the left side of the page doesn't appear to be as wide as in the PDF.
  This is the keyword box.
  "Zoonotics/...PDF/0809541268/Kitajima-2009-First detection of genotype 3 he.xml"
  This comes up in the splitElsevierTitle() and why we put the no filter of nodes if no y > yl.
  
1.  Get all of the elements in the title even if changed font
  i.e. identify title and then find all the elements near these that make up 
  the lines.
  Have to deal with spanning 2 columns and may not be part of the title and many other issues.
  e.g. 1834853125/394.full.xml  


1.  getColPositions:  when first line of paragraph is indented, we don't get the critical mass at the
  same point.
  See 0337534517/Andriamandimby-2011-Crimean-Congo%20hemorrhagic.xml

1.  Reassemble the elements of a word, line, paragraph from the different <text> elements
  See code we had in an earlier package for this.
  
1.  Identify section starts and ends, i.e. section titles.

1.  For 2 or more columns, detect the part which is only one column spanning the entire page.

1.  Detect 2 columns when one is mostly a figure and not words.
  Figure out columns for all pages and correct if one or two pages seems to be single column.



# Done

1.  [Done] Find font for the majority of the text.
