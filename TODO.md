+ Find tables and figures

+ Find text within shaded region.

+ For getColPositions() take the entire document into account and take the most common.
  Give the parts after References/Bibliography less weight. These are often indented due to the
  number so we don't get much text starting at that point
    See 3618741902/Armien-2004-High seroprevalence of hantavirus.xml
	
+ When finding section headers, check if the templates we find are centered and check others that
  have the same font are also centered.
    See 3618741902/Armien-2004-High seroprevalence of hantavirus.xml   

+ The box in the left side of the page doesn't appear to be as wide as in the PDF.
  This is the keyword box.
  "Zoonotics/...PDF/0809541268/Kitajima-2009-First detection of genotype 3 he.xml"
  This comes up in the splitElsevierTitle() and why we put the no filter of nodes if no y > yl.
  
+ Get all of the elements in the title even if changed font
  i.e. identify title and then find all the elements near these that make up 
  the lines.
  Have to deal with spanning 2 columns and may not be part of the title and many other issues.
  e.g. 1834853125/394.full.xml  


+ getColPositions:  when first line of paragraph is indented, we don't get the critical mass at the
  same point.
  See 0337534517/Andriamandimby-2011-Crimean-Congo%20hemorrhagic.xml

+ Reassemble the elements of a word, line, paragraph from the different <text> elements
  See code we had in an earlier package for this.
  
+ Identify section starts and ends, i.e. section titles.

+ For 2 or more columns, detect the part which is only one column spanning the entire page.

+ Detect 2 columns when one is mostly a figure and not words.
  Figure out columns for all pages and correct if one or two pages seems to be single column.



# Done

+ [Done] Find font for the majority of the text.
