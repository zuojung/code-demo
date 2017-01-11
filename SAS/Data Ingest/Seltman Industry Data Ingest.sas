/* CMU MSP 36-601 Fall 2016 */
/* HW_02 */
/* Zuojun Gong */
/* November 10, 2016 */


/* Comment out first line for Native SAS or second for Studio */
*%LET wd = /folders/myfolders/36601/IntroToSAS;
%LET wd = .;
LIBNAME indata "&wd/indata";
LIBNAME outdata "&wd/outdata";
FILENAME seltdata "&wd/indata/SeltmanMfg.txt";

/* Part 1: Setting up Fromats */
PROC FORMAT LIBRARY = indata;
	INVALUE matIFmt (UPCASE) 
			PET,PETE = 1
			PVC = 2
			PS = 3
			IRON = 4
			STEEL = 5
			COPPER = 6
			ALLOY = 7
			BABBITT = 8
			BRONZE = 9
			BRASS = 10
			OTHER = .
	;
	INVALUE boxIFmt
			single = 0
			boxed = 1
	;
	INVALUE testIFmt
			no = 0
			yes = 1
	;
	VALUE  matFmt
			1 = "PETE"
			2 = "PVC"
			3 = "PS"
			4 = "iron"
			5 = "steel"
			6 = "copper"
			7 = "alloy"
			8 = "alloy"
			9 = "alloy"
			10 = "alloy"
			OTHER = .
	;
	VALUE  alloyFmt
			1 = "bronze"
			2 = "brass"
			3 = "Babbitt"
	;	
RUN;

OPTIONS FMTSEARCH = (indata);

/* Part2: Ingest and Output Data */
DATA outdata.errors(KEEP=partNum material) 
	 outdata.singles(KEEP=partNum material alloy pctcopper)
	 outdata.boxed(KEEP=partNum material count alloy pctcopper)
	 outdata.tests(KEEP=partNum material alloy count testDate
						numTested pctPass);

	
	/* Formatting */
	INFORMAT boxed boxIFmt. tested testIFmt.;
	INFORMAT material matIFmt.;
	ATTRIB  testdate format = worddatx. label = "Test Date"
			material format = matFmt. label = "material"
			alloy format = alloyFmt. label = "alloy"
			pctPass format = 8.0;

	INFILE seltdata FIRSTOBS=1 MISSOVER;
	LENGTH partNum $9 sdate $11;
	INPUT partNum boxed tested;

	/* Read in rows condition to Boxed */
	IF boxed = 1 THEN
		INPUT count 
              material alloy_cnt 
			  mat1$ pct1 
			  mat2$ pct2 
              mat3$ pct3
	;
	ELSE 
		INPUT material alloy_cnt 
			  mat1$ pct1 
			  mat2$ pct2 
              mat3$ pct3
	;

	/* Read in the third row conditioned to Tested */
	IF tested = 1 THEN
		INPUT sdate numTested pctPass
	;

	/* Intake Mutiple Date Formats */
	IF UPCASE(SUBSTR(sdate, 4, 1)) >= 'A' & 
	   UPCASE(SUBSTR(sdate, 4, 1)) <= 'Z' THEN
       testdate = INPUT(sdate, DATE11.)
	;
    ELSE 
	   testdate = INPUT(sdate, MMDDYY10.)
	;

	/* Add up all alloys percentage */
	/* And calculate percent copper */
	/* And Use metals to determine Alloy Type */
	IF material >= 7 & material <= 10 THEN
		DO;
			/* Calculate Total Percentage */
			totalpct = pct1 + pct2;
			IF pct3 ^= . THEN totalpct = totalpct + pct3;

			/* Calculate percent copper */
			IF mat1 = "copper" THEN pctcopper = pct1;
			ELSE IF mat2 = "copper" THEN pctcopper = pct2;
			ELSE IF mat3 = "copper" THEN pctcopper = pct3;
			
			/* Use metals to determine Alloy Type */
			IF alloy_cnt = 2 & (mat1 = "tin" | mat2 = "tin" ) THEN alloy = 1;
			ELSE IF alloy_cnt = 2 & (mat1 = "zinc" | mat2 = "zinc" ) THEN alloy = 2;
			ELSE IF alloy_cnt = 3 THEN alloy = 3;
		END;

	/* Output Data */
	IF (totalpct ^= 100 & totalpct ^= .) | material = . THEN
			output outdata.errors;
	ELSE 
		DO;
			IF boxed = 0 THEN output outdata.singles;
			IF boxed = 1 THEN output outdata.boxed;	
	        IF tested = 1 THEN output outdata.tests;
		END;
RUN;

/* Part 3 */
title "Error Data" ;
PROC PRINT data = outdata.errors;
RUN; 

/* Part 4 */
PROC SGPLOT data = outdata.singles;
	HBAR material;
	TITLE "Barplots for Parts Sold Seperately - by Material" ;
RUN; 

PROC SGPLOT data = outdata.singles;
	HBAR alloy;
	TITLE "Barplots for Parts Sold Seperately - by Alloy" ;
RUN; 

DATA singlealloy;
	SET outdata.singles;
	IF alloy ^= . THEN output singlealloy;
RUN;

PROC SGPLOT DATA = singlealloy;
 HISTOGRAM count ;
 DENSITY pctcopper;
 DENSITY pctcopper / type=kernel;
 TITLE "Percent Copper in Alloys";
RUN; 

/* Part 5 */
PROC SGPANEL DATA = outdata.boxed;
 PANELBY material;
 HISTOGRAM count;
 TITLE "Histogram of Box Count by Material";
RUN; 

title "Regression: Count ~ Material" ;
PROC GLM DATA = outdata.boxed PLOTS=(ALL);
	class material;
	MODEL count = material / SOLUTION;
RUN;

/* Part 6 */
title "QC Method Comparsion" ;
PROC format;
  value testdatefmt low-'31AUG05'd = 'Old QC'
					'01SEP05'd-high = 'New QC';
  value isplastic 1-3 = "plastics"
  				  4-10 = "metals and alloys";
RUN;

title "Frequency Table for Test Methods and Material Type" ;
proc freq data=outdata.tests;
   format testdate testdatefmt.
		  material isplastic.;
	label testdate = "QC Method"
		  material = "Material Type";
   tables testdate*material / norow nocol nopct;
RUN;

/* Part 7 */
title "Regression: Percentage Passed ~ Material + Test Method" ;
PROC GLM DATA = outdata.tests PLOTS=(ALL);
    format testdate testdatefmt.
		   material isplastic.;
	class testdate material;
	MODEL pctPass = testdate material / SOLUTION;
RUN;
