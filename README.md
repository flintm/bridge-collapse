README file for git repository Flint.et.al.2016.Historical.Hydraulic.Bridge.Collapses
Related to "Historical Analysis of Hydraulic Bridge Collapses in the Continental United States", 
written by M.M. Flint, O. Fringer, S.L. Billington, D. Freyberg, and N.S. Diffenbaugh, 
published online by the ASCE Journal of Infrastructure Systems on February 23, 2017. (http://dx.doi.org/10.1061/(ASCE)IS.1943-555X.0000354).

This repository contains:
 (1) df.Fail.NBI.Gage.txt: a tab-delimited file containing data used in the analysis
 (2) R scripts compatible through R 3.2.1 for creating plots and tables used in the paper
 (3) RData files compatible through R 3.2.1 for creating plots and tables used in the paper

The SDR repository (http://purl.stanford.edu/xq579rb2654) contains:
 (4) USACE HEC-SSP software output files and plots
 (5) USGS data
 (6) NBI data
 (7) Plots from paper and supplemental plots
 (8) Notes on process of confirming collapses brige matches and collapse dates (exported from Evernote)
 (9) the accepted version of the manuscript.

A permanent version of these files is available in the Stanford Digital Repository at 
http://purl.stanford.edu/xq579rb2654

A live, updated version of this repository is available at https://git.it.vt.edu/mflint/Flint.et.al.2016.Historical.Hydraulic.Bridge.Collapses

Notes are also available at https://www.evernote.com/pub/maddie495/flint.etal.2016.historical.hydraulic.bridge.failures.notes

An interative map is currently hosted at https://mflint.shinyapps.io/FailMap/.

All content is licensed under the ODbL 1.0. The license is summarized at 
http://opendatacommons.org/licenses/odbl/summary/ and is available in this repository as "LICENSE.txt".

-----------------------------------------------------------------------------------------------------------------
(1) df.Fail.NBI.Gage.txt (and df.Fail.NBI.Gage.RData):
General description:
 - 35 entries of failed bridges, supplemental information, corresponding National Bridge Inventory (2012) entry, 
    USGS stream gauge station data, and flow and return period analysis
 - Columns 1-14 ("ID"-"COMMENTS") are from the New York State Department of Transporation Failed Bridge Database 
    (accessed June 2014) with some minor processing (e.g., all strings are lowercase)
 - Columns 15-37 ("MATER"-"BOOL_REGULATION") are the results of investigations conducted or supervised by M. Flint 
    from June 2014-January 2016
 - Columns 38-52 ("NBI_ROW"-"LATR") are from the 2012 National Data Atlas GIS archive related to the 2012 FHWA 
    National Bridge Inventory
 - Columns 53-62 ("STAID"-"LATR_GAGE") are from the USGS stream gauge inventory, accessed in 2014
 - Columns 63-108 ("MK_TAU"-"T_FAIL_IP_PARTDUR_USGS") are the results of various analysis conducted or
    supervised by M. Flint from June 2014 - January 2016

Naming convention:
Derived (analysis) variable names are determined in the order followed, linked by underscores ("_")
  - Variable type: 
	- Q: flow or discharge, cfs
	- T: return period, years
	- DATE: YYY-MM-DD 
	- BOOL: TRUE/FALSE  or factor 
    - COMMENT: string
	- COUNT: count, integer
  - Event category: 
	- FAIL
	- FAILPM2: fail +/- 2 days
	- MAX: max recorded
	- MAXPREFAIL: max recorded up to and including failure date in overlapping existence of bridge and gauge
    - MAXFAILYEAR: maximum in year of failure
	- FAIL_05: 5% confidence interval for failure
	- FAIL_95: 95% confidence interval for failure
  - Data source: 
	- D: daily mean
	- I: instantaneous
	- P: peak, usually instantaneous
	- IP: instantaneous and/or peak
  - Analysis type:
	- HECD: HEC-SSP Bulletin 17B analysis using daily mean data
	- HECP: HEC-SSP Bulletin 17B analysis using annual peaks data
	- PARTDUR: partial duration analysis (uses daily mean data)
  - Data source:
	- USGS

Most other variable names are taken directly from Fail, NBI, or USGS databases

Column descriptions: see tab-delimited "df.Fail.NBI.Gage.Colnames.txt"

-----------------------------------------------------------------------------------------------------------------
(2) R Scripts:
To recreate the figures and tables:
 - Ensure you have the following required packages (and their dependencies):
       - ggplot2
       - grid
       - gridExtra
       - plyr
       - reshape2
       - rgdal
       - extrafont (if font embedding in PDFs is desired. Will also need ghostscript installation.)
       - ggmap (if maps using satellite imagery are desired)
       - stats (for text in correlation plots)
       - Kendall (for text in correlation plots)
       - SuppDists (for text in correlation plots)
       - pspearman (for text in correlation plots)
       - nsRGA (for text in correlation plots and tables)
 - Open R and set the working directory to the location of the downloaded folder "Flint.et.al.2016.Historical.Hydraulic.Bridge.Collapses" (or 
    whatever folder name you have used)
 - Source "PlotsFlintEtAl2016.R" or open the file and source individual sections (plots will be saved as .PDFs)

To edit the figures:
 - Text in "PlotsFlintEtAl2016.R" describes plotting options 
 - The headers of the individual plotting scripts sourced in "PlotsFlintEtAl2016.R" describe subfuctions and
    provide additional information for customization
 - "SetupEncoding.R" controls the vast majority of plot styles, including colors, shapes, sizes, transparency
    (alpha), labels and titles
 - "MakeGridPlot.R" produces screen and saved versions of all multi-paneled plots (e.g., the correlation plot)
    and may be modified to produce saved files in formats other than .pdf

-----------------------------------------------------------------------------------------------------------------
(3) R Data:
 - "df.Fail.NBI.Gage.RData": R dataframe (df.Fail.NBI.Gage) described in section (1) of this README
 - "df.USbridges.water.RData": a minimally-processed R dataframe (df.USbridges.water) of the 2012 National Data 
    Atlas version of the 2012 Federal Highway Adminstration's National Bridge Inventory. Only bridges over water 
    are included. Column names, e.g. "ITEM3" are described in the FHWA NBI Recording Guide 
    ("nbi-coding-guide.pdf", provided), as well as in the National Data Atlas readme ("nbi.txt", provided).
 - "df.USgauges.RData": a minimally-processed R dataframe (df.USgauges) of the 2014 USGS list of all gauge 
    stations. Column names are provided in the original USGS tab-delimited file 
    ("usgs-station-description-inventory.txt", provided). An updated version of this list may be downloaded from
    http://nwis.waterdata.usgs.gov.
 - "ls.Discharge.USGS.RData": R lists of dataframes of USGS annual peak (ls.Discharge.USGS.Peaks), daily mean 
    (ls.Discharge.USGS.All), and instantaneous (ls.Discharge.USGS.Inst) flow values by USGS Station ID.
 - "ls.HEC.Bulletin17B.RData": R lists of dataframes of HEC-SSP output files for annual peak (ls.HECP.Bulletin17B)
    and daily mean (ls.HECD.Bulletin17B) analyses
 - "ls.PartialDur.USGS.RData": R lists of dataframes of USGS partial duration data (ls.PartialDur.USGS)
 - "df.US.states.RData": R dataframe describing polygons for US states (from Data Atlas GIS file)
 - "StateCountyCityData.RData" R dataframes of US Data Atlas state, county, and city abbreviations

-----------------------------------------------------------------------------------------------------------------
The following are available in the Stanford Digital Repository:

(4) USACE HEC-SSP output:
 - Results of Bulletin 17B flow frequency analyses ("Bulletin 17B Hydrology Subcommittee - 1982 - Guidelines for determining flood flow frequency.pdf", provided) conducted by or supervised by M. Flint 
    between August 2014 and January 2016 using v2.0 of the US Army Corps of Engineers HEC-SSP software 
    (http://www.hec.usace.army.mil/software/hec-ssp).
 - HEC-SSP reports, tab-delimited output, and plots are provided by USGS station ID (STAID) and input data type
    (annual peaks downloaded from USGS and annual maximum daily means, compiled by M. Flint from USGS daily
    mean data).

-----------------------------------------------------------------------------------------------------------------
(5) USGS station description and data:
 - "usgs-station-description-inventory.txt": data relevant to gauge sites, e.g., number of peak counts
 - "PartialDuration/pg-########-all.png": Partial duration curve downloaded from USGS for each STAID
 - "PartialDuration/pg-########-all.txt": tab-delimited partial duration curve from optical character 
    recognition

-----------------------------------------------------------------------------------------------------------------
(6) National Bridge Inventory data:
 - "nbi-coding-guide.pdf": FHWA NBI Recording Guide
 - "nbi.txt" US Data Atlas readme, which describes process by which latitude and longitude of bridges provided
    in the NBI were verified or augmented.

-----------------------------------------------------------------------------------------------------------------
(7) Plots:
Manuscript plots:
 - "Fig1.pdf": all bridges over water in US by bridge construction year
 - "Fig2.pdf": failed bridges with insets of mid-atlantic and northeast regions
 - "Fig3.pdf": compares structure type and material for failed bridges and US bridges over
    water
 - "Fig4a.pdf": failure, maximum, maximum pre-failure, and design flows for failed bridges
 - "Fig4b.pdf": return periods of failure, max, and max pre-fail events using daily mean
    and instantaneous/peaks data from USGS and the daily mean HEC-SSP Bulletin 17B analysis
 - "Fig5.pdf": correlation between various flow and return period measures
 - "Fig6.pdf": trends in annual peaks

Supplemental plots:
 - "Supplement/ID####.eps": zoomed-in maps of bridge and gauge location, labeled with ID from df.Fail.NBI.Gage.txt
    and with encodings consistent with the paper and governed by SetupEncoding.R

-----------------------------------------------------------------------------------------------------------------
(8) Notes:
 - "EvernoteNoteBookPDF.zip" Individual PDFs of entries and attached files of notes taken when confirming match of failed to NBI bridge and/or failure date, as well as gauge-related information
 - "HydraulicFailedBridgesNotesXML.enex" Evernote-format XML document
 - Notes are also available at https://www.evernote.com/pub/maddie495/flint.etal.2016.historical.hydraulic.bridge.failures.notes