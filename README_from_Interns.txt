Summary
Our focus has mainly been on if the script correctly identifies the XML nodes corresponding to the tables, and less on how well the content was read. We ran the getTables() function on a variety of PDFs to observe the behavior of the function for different files, and sorted the files we tested into categories of whether or not the function identified the right nodes(summary below). Some files did not contain tables or had broken XML(no content in the XML files), which we noted. Common issues are documented below. We only implemented minimal code changes unrelated to the XML parsing, such as removing mistake nodes which were hyperlinks or empty lists.


 Common Issues
1. Misses a few tables
   1. Big offenders are documents with tables spanning both full pages and half pages 
1. Misses all tables
   1. Often the result of XML file missing content, so this could be an issue with the PDF > XML conversion
1. Misses rotated tables when straight
   1. getRotation function only returns if page is rotated, but occasionally a table is rotated within a straight page
      1. possible fix? check for rotated text instead of rotated pages
1. Multipage table issues
   1. Usually picks up all the tables going across pages, just need to be able to combined them into single table
1. Picks up lots of empty lists, null tables
   1. Not a significant issue, easy to drop these afterwards 
1. Picked up “Table” nodes in text which were hyperlinks
   1. Should be fixed with modification( See “Link Mod” below)
1. Combines tables together
   1. For example, gets top of Table 1 all the way until bottom of Table 2 as one table. Then gets top of Table 2 to bottom of Table 2 as second table.
   2. Consequence of missing the end of first table, happens a lot with tables with lots of horizontal lines
1. For PDFs with 2 articles in same file, may get Table 1 twice from first article instead of once from first article and once from second article
   1. Files of this form are not common
1. Erroneously Combining columns(So 1 | 1 | 1 | 1 might become 1 | 11 | 1
   1. Think it happens more on OCRd documents, but did not investigate thoroughly
1.  Sometimes picks up references in the text of the form “(TABLE)”








Other Notes
1. For files with no tables, throws error. Should add error handling
2. Sahu-2002: length of getTables() return object not consistent with what is shown with View in R Studio




          Code Modifications


Link Mod(in getTables() function in tables.R beginning after tableNodes  = getNodeSet( …. on line 12)
    w = sapply(tableNodes, function(x){
      is.null(xmlChildren(x)$a)
    })
    tableNodes = tableNodes[w]


Remove zero length results(at end of getTables() function)
tbls = tbls[lapply(tbls, length) > 0]






Summary of Subset of Files


Working ish(As in correct table nodes are found)


0086305645/Dacheux-2009-European bat lyssavirus transmiss.xml - Working
0373770779/Siembieda-2011-The role of wildlife in transbo.xml - Working pinnacle of technology, rotated, multi-page tables 
4045765069/Vikøren-2008-A\ severe\ outbreak\ of\ contagious\ e.xml
3008601466/Orozovic-2010-Degenerate primers for PCR ampli.xml - Working with <a> mod(hyperlink mod)
2420326149/Koopmans-2008-Progress in understanding norovi.xml - Working with <a> mod
0000708828/Li-2009-Sensitive, qualitative detection of hu.xml - Working with <a> mod
0039457211/Hidalgo-Vila-2007-Salmonella in free living t1.xml - Working with <a> mod
0063909007/Liu-2012-Genetic analysis of hantaviruses and.xml - Working? With <a>(nodes weird)
0096281726/Papa-2013-West Nile virus in mosquitoes in Gre.xml - Working
0149111586/Kuzmin-2008-Lagos bat virus in Kenya.xml - Working
0184308009/Ip-2015-Novel Eurasian highly pathogenic avian.xml - Working
0210768028/Thulke-2005-Pseudorabies virus infections in w.xml - Working with <a> mod
0066317565/Nemeth-2010-Serosurveillance\ for\ Japanese\ ence.xml - Working
0250433046/Soumahoro-2010-Imported Chikungunya virus infe.xml - Working
0251931699/Kariwa-2007-Hantavirus infection in east Asia.xml - Working with <a> mod. 1 table across 3 pages, returns 3 tables.
0257644206/Goff-2012-Roles of host species, geographic se.xml - Working with <a> mod
0324119822/Shirato-2012-Detection of bat coronaviruses fr.xml - Working
After this point, stopped making note if <a> mode


0382147657/Lecompte-2007-Genetic identification of Kodoko.xml - picks up 2 nulls
0401044977/Foy-2011-Probable non-vector-borne transmissio.xml - Working
0422138239/Chu-2003-The complex ecology of hantavirus in.xml - Working
0569977278/Manarolla-2010-Usutu virus in wild birds in n1.xml - picks up 1 null
0578511133/Gunes-2009-Crimean-Congo hemorrhagic fever vi1.xml - doesnt read content right
0644619555/Han 2005.xml - Working
0654249690/Huhn-2006-Vaccination coverage survey versus a.xml - Working
0666594809/Castillo-2004-Prevalence of antibodies to Hant.xml - Working
0686514095/Homaira-2010-Cluster of Nipah virus infection1.xml - Working
0903052420/Saksida-2008-Dobrava virus RNA load in patient.xml - doesn’t read right
0984326270/Warrilow-2003-Public health surveillance for A.xml - doesn’t read right
0991492247/Wang-2009-Isolation of Kyasanur Forest disease.xml - Working
1001791068/Bendinelli-2001-Molecular properties, biology1.xml - null
1041016642/Wang-2010-Methods for molecular surveillance o.xml - no content
1148744749/Carroll-2010-Ancient common ancestry of Crimea.xml - Working
1041016642/Wang-2010-Methods for molecular surveillance o.xml -  gets table name and header, no content. Table only contains strings
1109410615/Lyons-2012-Species association of hepatitis B.xml - gets title and headers but no content
1203161678/Garcia-2006-Evaluation of a Crimean-Congo hem1.xml - misreads some columns
1222782901/Niklasson-2009-Sudden infant death syndrome an.xml - cant parse some + symbols
1226481142/García-2000-Genetic diversity of the Junin vi1.xml - picks up 1 null
1252989057/Rodrigues-2010-Molecular epidemiology of Saint.xml - 1 null and 1 false positive
1290464086/Adams-2012-Venezuelan equine encephalitis viru.xml - lots of nulls, multipage table
1328233946/Codeço-2004-Risk assessment of yellow fever ur.xml
1431944296/Liu-2010-Banna Virus, China, 1987–2007.xml - cuts off table
1535546573/Legrand-2007-Understanding the dynamics of Eb1.xml - gets headers
1559584125/Homaira-2010-Cluster of Nipah virus infection.xml - doesnt get ends of tables
1561656400/Fukai-2007-Molecular characterization of a nov.xml
1571127013/Dimitrov-2010-Status of Wild Birds in Bulgaria.xml - picks up 1 empty list
1712587128/Le-2011-Development of one-step multiplex RT-P.xml
1757401522/Bausch-2006-Marburg hemorrhagic fever associa1.xml
1791326938/Inizan-2010-Genetic Evidence for a Tacaribe Se.xml
1835572122/Patz-2004-Unhealthy Landscapes_ Policy Recomme.xml
1866297532/Poidinger-2000-The molecular epidemiology of K.xml
1957721677/Carletti-2007-Short report_ Rapid detection an.xml
2015159535/Souza-2009-Baylisascaris procyonis in raccoons.xml
2049909283/Emmerich-2008-Strain-specific antibody respons.xml






No Tables


0288934667/Kilpatrick-2006-West Nile virus epidemics in N.xml - Working(No tables)
0299437043/Brito-2012-Ill nature_ disease hotspots as thr.xml - Working(No tables)
0304308204/Jacomy-2014-ForceAtlas2, a continuous graph l1.xml - Working(No tables)
0200329713/Bausch-2014-Outbreak of ebola virus disease in.xml - Working(No tables)
0092416264/Hemmer-2010-Human cowpox virus infection acqui.xml - Working(No tables)
0004976254/Langlois-2012-Towards a better integration of.xml - Working(No tables)
0013939837/Huang-2012-Simian foamy virus prevalence in Ma.xml - Working(No tables)
0326259264/Woolhouse-2008-Exemplary epidemiology.xml - Working(No tables)
0407499289/Page-2010-Emergence and characterization of s1.xml - Working(No tables)
0687324647/Harvala-2009-Case report_ Eastern equine ence1.xml - Working(No tables)
0691918507/Brownstein-2009-Digital disease detection--har.xml - Working(No tables)
0705018732/Hayman-2010-Long-term survival of an urban fru.xml - Working(No tables)
0739046830/Rabinowitz-Human and animal sentinels for shar.xml - Working(No tables)
0963120051/Mondal-2006-Detection of Orf virus from an out.xml - Working(No tables)
0989544072/Nowakowska-2009-The first established focus o1.xml - Working(No tables)
1003051749/Newman-2008-Human case of swine influenza A (1.xml - Working(No tables)
1016434953/Hayes-2009-Zika virus outside Africa.xml - Working(No tables)
1066657133/Nishiura-2016-Preliminary estimation of the ba.xml - Working(No tables)
4285229841/Holmes-2011-Zoonotic\ transmission\ of\ bovine\ pa.xml
1114060638/Douglas-2007-Murray Valley encephalitis in an.xml 
1147869462/Abdo-Salem-2006-Descriptive and spatial epidem.xml 1254594645/Lam-2003-Nipah virus--a potential agent of bio.xml
1277956743/Sirohi-2016-The 3.8 Å resolution cryo-EM stru1.xml - picks up empty list
1294942893/Lupulovic-2011-First serological evidence of W.xml
1305564348/Shinde-2009-The first Japanese patient with va.xml
1457669138/Peiris-2009-Emergence of a novel swine-origin.xml
1481056760/Morens-2012-Emerging infectious diseases in 20.xml
1588213317/Anthony et al_2015_Non-random patterns in vira.xml - picks up 5 empty lists
1634302113/Chen-2007-Prosthetic valve endocarditis caused.xml
1647842654/OH-0011-2012.xml
1680558593/Nougairede-2013-Sheep-to-human transmission of.xml
1712281630/Vaidyanathan-2011-Virus hunters_ catching bugs.xml
1734908500/West Nile Virus Documented in Indonesia…Specim.xml
1742471120/Lu-2008-Tick-borne encephalitis in mainland C1.xml
1790021971/Harrison-2010-Culling wildlife hosts to contr1.xml
1859659165/Favi-2002-First case of human rabies in Chile.xml 
1941178363/Field-2009-Bats and emerging zoonoses_ henipav.xml
3746005743/Meertens-2003-A\ novel\,\ divergent\ simian\ T-cell.xml
3222635810/Meertens-2003-A\ novel\,\ divergent\ simian\ T-cell.xml
1952072600/194008291000300107.xml
1987092987/Bogoch-2016-Anticipating the international spr.xml
2060038782/Bogovic-2015-Tick-borne encephalitis_ A review.xml




Not Working




Ergunay-2011-Confirmed exposure to tick-borne.xml - Got all tables, but also a non-table
4269303440/Puzelli\ et\ al\ 2014.xml - not picking up all tables, picking up multiples of same tables
2992342787/Hayman-2012-Endemic Lagos bat virus infection.xml - Issue with supplement table
0056567671/Hahn-1988-Western equine encephalitis virus i1.xml -XML broken(missing content)
0218616051/Malnati-2008-A universal real-time PCR assay f.xml - Gets tons of incorrect nodes
0258214140/Phoutrides-2011-Dengue\ virus\ seroprevalence\ am.xml - 4 tables, pulls just 1 (3 times)
0295627717/Biological YF Risk.xml - XML broken(missing content)
0063708233/Wolfe-2005-Bushmeat\,\ hunting\,\ deforestation\,\ a.xml - pulls six, only 1
0396850821/Zheng-2012-Japanese encephalitis and Japanese.xml - multi-page tables
0415231817/Yang-2010-Simultaneous typing and HA_NA subtyp.xml 
0489413033/Petri-2010-Tick-borne encephalitis (TBE) trend.xml - multi-page, rotated tables        
0532875827/Peeters-2002-Risk to human health from a pleth.xml - 2 out of 3 tables
0573375226/20866.xml  - table above “Table 1,” info not really table 
0584496035/Antoniou-2010-Rats as indicators of the prese1.xml -  misses 1 table, duplicates 1
0656704183/Coulibaly-2004-A natural asymptomatic herpes 1.xml - misses 1 table
0725311144/Brady-2009-Habitat attributes of landscape mos.xml - missing first 2 since “table” on left
0730080335/Kinnunen-2007-Serological evidence for Borna 1.xml - doesn’t pick up nodes
0859458376/McColl-2002-Pathogenesis studies with Australi.xml - doesn’t pick up nodes
1022794638/Armien-2004-High seroprevalence of hantavirus2.xml - misses 1 table, maybe columns
1076731270/Eisenberg-2006-Environmental change and infect.xml - gets 1 wrong table, misses end of some tables, combines table 4 and 5
1152517279/Wang-2005-SARS-CoV infection in a restaurant f.xml - gets no tables
1169473362/WHO_2011_Yellow fever in Africa and Central an.xml -  misses some tables, has french table titles as well as english
1306973429/bullwho00439-0090.xml - gets no tables
1316379333/Cardinal-2008-Molecular epidemiology of domest.xml - misses last 2 tables
1395764672/Srihongse-1967-The islolation of Ilheus virus.xml - XML is missing content
1441731167/Gruwell-2000-Role of peridomestic birds in the.xml - rotated and straight tables
1445638403/Antoniou-2010-Rats as indicators of the presen.xml - misses table 3 
1455217934/Morita-1996-Seroepidemiological survey of lymp.xml - misses table
1469272685/Burt-1996-Investigation of tick-borne viruses.xml - XML content missing
1489734669/Dwibedi-2010-Emergence of Chikungunya virus i1.xml - misses last table
1556850690/Buckley-2003-Serological evidence of West Nile.xml - one false positive, multi page table
1558215626/AL-TIKRITI-1981-Congo_Crimean haemorrhagic fev.xml - misses all tables
1590489227/Lojkic-2010-Phylogenetic analysis of Croatian1.xml - misses table 2
1609915988/McIntosh-1976-Culex (Eumelanomyia) rubinotus T.xml - misses all tables
1674687958/Remmers-2000-Longitudinal studies in the epide.xml - misses table 1
1697309369/Gibson-2011-Primary forests are irreplaceable.xml - many empty tables, gets some text
1725991326/Hammon-1952-California Encephalitis virus, a n.xml - XML is missing all content
1737457921/Coulibaly-2004-A natural asymptomatic herpes B.xml - misses table 1
3618741902/Armien-2004-High\ seroprevalence\ of\ hantavirus.xml - misses table 2
4267875357/Armien-2004-High\ seroprevalence\ of\ hantavirus.xml - duplicate
1022794638/Armien-2004-High\ seroprevalence\ of\ hantavirus.xml - duplicate 
1756816365/Epstein-2010-Identification of GBV-D, a novel.xml -  only gets name and header for 1 table, misreads second table, couple of empty lists
1847813219/Li-2014-Epidemiology of human infections with.xml - misses table 1 
1849067514/Coffey-2005-Susceptibility of Ochlerotatus tae.xml - gets 1 extra erroneous table from data
3917060957/Juricova-2009-Serological\ examination\ of\ song1.xml - gets first table twice, out of two tables
4260732476/Juricova-2009-Serological\ examination\ of\ song1.xml - gets first table twice, out of two tables
4219095796/Hoffmann-2001-Universal\ primer\ set\ for\ the\ ful.xml - misses the two rotated tables
1852921817/Klimes 2001.xml - misses all tables










Files with <=300 lines in the XML. Many contain no content(but some are legit)
Lines          File Name
      271 2889671516/Geevarghese-2005-Detection of chandipura virus.xml
      236 1725991326/Hammon-1952-California Encephalitis virus, a n.xml
      193 0295627717/Biological YF Risk.xml
      191 1147869462/Abdo-Salem-2006-Descriptive and spatial epidem.xml
      185 3755107097/8903239.xml
      148 2130385679/Olival-2013-Linking the Historical Roots of En.xml
      121 3809448505/JOHNSEN-1974-Study of Japanese encephalitis vi.xml
      119 1852921817/Klimes 2001.xml
      111 1306973429/bullwho00439-0090.xml
      108 4145850193/Tiensin 2007.xml
      102 2299536549/Madic-1993-Serologic survey for selected viral.xml
       81 3806472688/Lord-1970-Further evidence of southward transp.xml
       79 3764395215/Pennington-1977-Bunyamwera virus-induced polyp.xml
       66 2686868357/Burt-1996-Investigation of tick-borne viruses1.xml
       66 1469272685/Burt-1996-Investigation of tick-borne viruses.xml
       60 3101983999/Hummeler-1959-Encephalomyelitis due to infecti.xml
       52 3948630915/Kokernot-1969-Arbovirus studies in the Ohio-Mi.xml
       50 0056567671/Hahn-1988-Western equine encephalitis virus i1.xml
       47 1558215626/AL-TIKRITI-1981-Congo_Crimean haemorrhagic fev.xml
       43 4129638848/Miura-1970-Studies of arthropod-borne virus in.xml
       37 1395764672/Srihongse-1967-The islolation of Ilheus virus.xml
       27 1455217934/Morita-1996-Seroepidemiological survey of lymp.xml