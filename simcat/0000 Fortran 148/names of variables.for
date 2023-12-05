* =============================================================================================
* SIMCAT - Monte-Carlo simulation of water quality in a river ....
* =============================================================================================
* List of names of variables and descriptions of what they do
* =============================================================================================

* =============================================================================================
* TO BE ADDED
* =============================================================================================
* kount works
* identify works
* P1 .......
* P2 .......
* Pa .......
* Pb .......
* CD1
* CD2
* cpart1
* cpart2
* ===============================================================================================================

* ===============================================================================================================
* Parameters used for the dimensions of arrays
* ===============================================================================================================
* KR .... the maximum mumber of Reach data-sets kept in store for subsequent mixing with other Reaches
* MG .... the number of river flow gauges allowed in SIMCAT
* MI .... the maximum number of the old type of intermittent discharges (no longer used)
* MM .... quality monitoring stations allowed in SIMCAT
* MO .... the maximum number of statistics entered as flow and quality data...
* MPRF... the maximum number of data-points that can be used to define a non-parametric distribution 
* MR .... reaches allowed in SIMCAT (NREACH hold the actual number in the model being run)
* MS .... the maximum number of Monte-Carlo shots - the actual number in the current run will be NS
* MT..... the maximum size of utility array used in tests of input data etc (it is the maximum of NE, NV, NF etc)
* M7..... the maximum number of data-sets for Non-parametric Distributions 
* M8 .... the maximum number of data-sets for monthly data 
* NB .... the maximum number of bifurcations 
* NE .... the number of sets of data for effluents 
* NF .... the maximum number of sets of flow data for rivers 
* NM .... the number of summary statistics calculated from the shots: mean, standard deviation and percentiles
* NP10 .. the  number of river quality determinands
* NQ .... the maximum number of sets of data on river quality targets 
* NU .... the maximum number of Features 
* NUED... the number of effluent discharges
* NUW ... the number of sub-catchments
* NV .... the maximum number of sets of quality data for rivers allowed in SIMCAT
* =====================================================================================================================
*
* =====================================================================================================================
* Names of the variables
* =====================================================================================================================
* dcalflow ........ is the distance from the head of the current Reach, or from the last point on the Reach of gap filling of river flow
* dcalquality ..... is the distance from the head of the current Reach, or from the last point of gap filling of river quality
* DINT ............ is used when interpolating between features. The length of river is split into lengths of equal size, DINT
* LENGTH OF MAIN RIVER ... the current distance from the head of the first Reach.  It is used for the plotting of profiles
* DISTP ........... the current distance between successive Features
* DISTR ........... the distance from the head of the current Reach to the next Feature
* TDEG ............ the mean temperature in all the rivers and streams in the model
* TSDEV ........... the corresponding standard deviation for temperature 
* TCORF ........... the correlation coefficient between temperature and flow across the model
* master set used ... if set equal to 1 use a master set of certain data - that from from the first set of data in the batch
* NOTARG .......... the variable to count the number of sets of river targets
* iforce gap fill ... is a switch to force in the flow gap filling using the flow data entered for the feature even if there is no flow data-set up in JFCAL
* CALSPECIAL ...... this is a switch that specifies the use of existing gap filling files if calspecial = 1
* ===============================================================================================================
* Warnings about data
* ===============================================================================================================
* isuppress .... total number of warnings about data
* suppress1 .... the sequence of features 
* suppress3 .... huge change in flow 
* suppress4 .... no determinands for classification 
* suppress5 .... error reading flow gap filling data 
* suppress6 .... zero 95-percentile flow set to 1% 
* suppress7 .... unnecessary monthly structures 
* suppress8 .... quality data not set for gap fill 
* suppress9 .... problems in the monthly structure 
* suppress9 .... problems in the monthly temperature 
* suppress9 .... problems in the monthly sus.solids 
* suppress10 ... illegal data for river flow 
* suppress11 ... flow exceeds a billion 
* suppress12 ... problems with monthly structure 
* suppress13 ... zero entered as the 95-percentile river flow 
* suppress14 ... infeasibly huge concentration 
* suppress16 ... unneeded effluent data ignored 
* suppress17 ... deleted feature     
* suppress18 ... zero variation in river quality 
* suppress19 ... unneeded data ignored  
* suppress20 ... infeasible correlation coefficients 
* suppress00 ... all the others 
* maxistor ..... number of stored reaches 
* -----------------------------------------------------------------------------------------------------------------------------------------------------------
* Total RQS length 1 ...... total length of river with a river quality target
* Total length 2 .......... total length of river with a river quality target
* Total length 00 ......... total length of river complying with 50 per cent confidence
* Total length 50 ......... total length of river failing with 50 per cent confidence
* Total length 95 ......... total length of river failing with 95 per cent confidence
* Total length dets 00 .... total length of river complying with 50 per cent confidence for each determinand ... 
* Total length dets 50 .... total length of river failing with 50 per cent confidence for each determinand ... 
* Total length dets 95 .... total length of river failing with 95 per cent confidence for each determinand ... 
* -----------------------------------------------------------------------------------------------------------------------------------------------------------
* model number in batch ... number of the set of models being run in a batch ...
* -----------------------------------------------------------------------------------------------------------------------------------------------------------
* feeture ........... number of the feature
* feetcount ......... the number of features
* F1 ................ the working array to store text such as the name of output files
* folder ............ the name of the folder (directory) used to hold the data file and the output
* FUNIT ............. the units of flow
* FNAME ............. working array to store text such as the name of output files
* FNAME2 ............ working array to store text such as the name of output files
* =============================================================================================
* ICAL ....... this dictates the form of the calculation done for this data-file...
* =============================================================================================
* === 0 ...... mass balance of input data ...
* === 1 ...... compute changes to river flows giving exact fit ...
* === 2 ...... apply computed changes to flows (from ICAL=1)extrapolating and interpolating...
* === 3 ...... as 2 but also computing the changes in quality needed fo give a perfect fit...
* === 4 ...... apply computed changes to flow (ICAL=1) and computed changes to quality (from ICAL=3)... extrapolating and interpolating...
* === 5 ...... run 1 followed by run 3...
* === 6 ...... run 1 followed by runs 3 and 4...
* === 7 ...... compute effluent standards needed to meet river targets...
* === 8 ...... policy for river quality planning...
* =============================================================================================
* ifbatch .... if "ifbatch" is set equal to 1 this is a batch run ...
* noGIS ...... switch to request production of the huge GIS output files (when noGIS is set equal equal to zero)
* nobigout ... switch to request production of full output (if nobigout equals zero) ...
* IDIFF ...... switch for inputing global diffuse sources
* IEND ....... indicator that the end of this Reach has been reached
* IF ......... number of the dataset for river flow
* IE ......... number of the dataset for discharge flow and quality
* IPRINT ..... Variable to suppress output. Set to 0 to retain all the output.  Set to 1 to exclude output on non-effluent features ....
* IPUR ....... Switch for including or excluding natural purification
* IQ ......... number of the dataset for river quality
* IREACH ..... number of the reach
* IMM ........ indicator that there are too many monitoring stations
* IMONTH ..... number of the month from 1 to 12
* IHR ........ hour for the time the SIMCAT run was started
* IMIN ....... minute for the time the SIMCAT run was started
* ISEC ....... second for the time the SIMCAT run was started
* IHUN ....... hundredth of second for the time the SIMCAT run was started
* IYEAR ...... year for the date the SIMCAT run was started
* JMONTH ..... month for the date the SIMCAT run was started
* IDAY ....... day for the date the SIMCAT run was started
* JSKIP ...... suppress unnecessary output and calculations
* LMONP ...... number of sets of the results for the summaries ot output
* KEPOL15 .... switch diffuse pollution on or off
* KEPOL42 .... switch diffuse pollution on or off (aggregated STWs)
* KINT ....... switch gap filling on or off
* KFEAT ...... number of the feature
* KPTARG ..... percentile point
* KRFPOL13 ... switch on diffuse pollution (river type - flow)
* KRQPOL13 ... switch on diffuse pollution (river type - quality)
* KRFPOL25 ... switch on livestock diffuse pollution (river type - flow)
* KRQPOL25 ... switch on livestock diffuse pollution (river type - quality)
* KRFPOL27 ... switch on arable pollution (river type - flow)
* KRQPOL27 ... switch on arable pollution (river type - quality)
* KRFPOL29 ... switch on highway runoff (river type - flow)
* KRQPOL29 ... switch on highway runoff (river type - quality)
* KRFPOL31 ... switch on urban runoff (river type - flow)
* KRQPOL31 ... switch on urban runoff (river type - quality)
* KRFPOL33 ... switch on atmospheric deposition - (river type - flow)
* KRQPOL33 ... switch on atmospheric deposition - (river type - quality)
* KRFPOL35 ... switch on natural background - (river type - flow)
* KRQPOL35 ... switch on natural background - (river type - quality)
* KRFPOL46 ... switch on diffuse mines - (river type - flow)
* KRQPOL46 ... switch on diffuse mines - (river type - quality)
* KRFPOL37 ... switch on septic tanks - (river type - flow)
* KRQPOL37 ... switch on septic tanks - (river type - quality)
* KRFPOL39 ... switch on intermittent discharges - (river type - flow)
* KRQPOL39 ... switch on intermittent discharges - (river type - quality)
* KRFPOL40 ... switch on aggregate CSOs - (river type - flow)
* KRQPOL40 ... switch on aggregate CSOs - (river type - quality)
* masterdata ........... some of the data for the first model will apply to all the other models in the batch run ... masterdata is set to 1 when this happens
* master output mode ... master value of MORQ
* masterNS ............. master value of NS (the number of shots)
* masterIPUR ........... master value of IPUR
* masterIKINT .......... master value of IKINT
* masternocon .......... master value of nocon
* master no tables ..... master value of no tables 
* masterIPRINT ......... master value of IPRINT
* masterQTYPE .......... master value of QTYPE
* masterBASE ........... master value of BASE
* masterWEQ ............ master value of WEQ
* masterBEQ ............ master value of BEQ
* masterGEQ ............ master value of GEQ
* masterMRQS ........... master value of MRQS
* MPAR ............ set the number of determinands (in order to check all records are read in)
* MONF ............ request monitoring output for flow (if MONF equals 2 )
* MONQ ............ request monitoring output for quality (if MONQ equals 2 )
* output mode ..... switch for mean or percentile modes for output
* MU .............. number of users (features)
* NDET ............ number of determinands in current run
* NEXC ............ set to 1 to suppress the downstream extrapolation of corrections for quality
* NEXF ............ set to 1 to suppress the downstream extrapolation of corrections for flow
* NGAUGE .......... the number of flow gauges
* NI .............. the number of (old-style) intermittent discharge
* nocon ........... variable for suppressing calculation of confidence intervals
* NP .............. number of river quality determinands (parameters)  ... NDET
* NREACH .......... the number of reaches
* NS .............. the number of shots
* SEASD ........... the number of monthly data-sets
* NONPD ........... number of non-parametric distributions
* negreach ........ an indicator that only a subset of the reaches will be run ...
* negorder ........ number of the starting reach when only a subset is being run 
* No tables ....... switch to suppress the output of tables
* Globaldataname .. the file on global data changes
* Datfilename ..... the name of the data file
* Title ........... title of run given in the datafile
* Line of data .... item to hold a line of data
* ISCREEN ......... control on the amount of information written to the screen
* LAPSE ........... the time taken for the SIMCAT run
* virtualreach .... indicates a virtual reach when virtualreach = 1
* =============================================================================================
* Arrays
* =============================================================================================
* ADIST(MR) ....... The length of main river upstream of start of a reach.  This is used in assembling data for graphs.
* BIDENT(NB) ...... The sequence numbers of features that are bifurcations ...
* C(MP10,5) ....... Summary statistics for river quality for all determinands (MP10): mean, standard deviation, 90, 95 and 99-percentiles
* CMS(MP10,MS) .... The values of all the shots for all the determinands
* CMX(MP10,MS) .... Array used for temporary storage of CMS etc, in doing calculations for gap filling
* ---------------------------------------------------------------------------------------------
* LMS(nprop...) ... concentrations from various types of feature --------------------------
* nprop ........... number of different types of feature tracked for inputs ---------------
* EQD(nprop...).... Values of LMS for ends of reaches.  Stored for the mixing of reaches --
* L(nprop...)...... Summary statistics for the contributions ------------------------------
* ---------------------------------------------------------------------------------------------
* DIST(NU) ........ Distance of feature from the head of its reach
* DNA(MP10) ....... Short name of determinands (up to four characters)
* DNAME(MP10) ..... Long name of determinads (up to 11 characters)
* CLD(MP10,5) ..... Alternative version of C(MP10,5) used when the values in C need to be retained
* EFD(KR,MS) ...... Proportion of effluent in each shot of river flow - at end of reach
* F(NF,MO) ........ Summary statistics for input river flow data sets: mean, standard deviation etc
* FD(KR,MS) ....... Values of river flow shots at the end of the reach 
* FE(NE,MO) ....... Summary statistics for input discharge flow data sets: mean, standard deviation etc 
* FEND(MR) ........ Flow and quality statistics for the end ofthe reach. These are used for the calculation of the Effectiveand so for the calculation of confidence limits ...
* FLOW(NM) ........ Calculated values of the summary statistics of river flow: mean, standard deviation etc
* EFLOW(NM) ....... Summary statistics calculated for flow shots from a tributary: mean, standard deviation etc
* EFMS(MS) ........ Proportion of effluent in each shot of river flow
* FMS(MS) ......... The shots for river flow 
* FMX(MS) ......... Array used for temporary storage of FMS etc, in doing calculations for gap filling
* GEQ(3,MP10) ..... Summary statistics for the definition of good effluent quality ...
* IFRQS(NU) ....... Reference numbers for sets of river quality targets for each feature ...
* IPLAN(MR,3) ..... An array that shows which Reach is to be processed next so giving the structure of the catchment .... 
* IRHOLD(MR) ...... Used to store the data for a reach that is needed further on to mix with others reaches ...
* JF(NU) .......... Number of the set of river flow data for the feature 
* JFCAL(NU) ....... Number of the set of river flow data used for gap filling. If JFCAL equals zero SIMCAT will extrapolate if required ...
* JFUSER(NU) ...... Variable to suppress extrapolation of gap filling of river flow
* JQ(NU) .......... Number of the set of river quality data for this feature 
* JQCAL(NU) ....... Number of the set of river quality data used for gap filling
* JQUSER(NU) ...... Variable set to suppress extrapolation of gap filling of river quality
* JREACH(NU) ...... Number of the reach on which the feature is located
* JSTOR(KR) ....... Used to store the data for a reach that is needed further on to mix with others reaches ...
* JT(NU) .......... The type of feature - 3 for a swage treatment works etc ...
* KFCAL(NU) ....... A gap filling switch. Set to "1" downstream of the last gap filling point. And left at zero at and upstream of the last gap filling point...
* KGAUGES(MG) ..... List of features that are flow gauges
* KQCAL(NU) ....... A gap filling switch. Set to "1" downstream of the last gap filling point. And left at zero at and upstream of the last gap filling point...
* LSAMPOINTS(MM) .. List of features that are river quality monitoring points
* MK(MT) .......... Array used to store values for checking data are read in correctly
* PDEC(NE,MP10) ... Type of probability distribution for discharge quality ...
* PDEF(NE) ........ Type of probability distribution for discharge flow ...
* PDRC(NV,MP10) ... Type of probability distribution for river quality ...
* PDRF(NF) ........ Type of probability distribution for river flow ...
* pollution data  . Discharge quality data - mean, standard deviation etc ...
* FLOW(5) ......... Running values of summary statistics for river flow - mean, 95-percentile ...
* GISCODE(NU) ..... Unique identifier for help in using SIMCAT output with GIS
* reachstore(MR)  . Information for reduced SIMCAT run.  The first reach is now number NEGREACH.  Reachstore holds the numbers of the reaches to be included in this run 
* flowstats(9) .... Running values of the percentile points for the flow distribution
* propstats(9) .... Running values of the proportions of discharge
* NextReach(MR) ... number of the next reach to be processed
* kount reach (MR). number of features on each reach
* PNUM(NE,MP10) ... nmbers of samples of discharge quality
* QBASE(MP10) ..... limit to which natural purification can improve river quality
* QD(KR,MP10,MS) .. store for shots and the ends of reaches.  To allow mixing of reaches
* QDN(MP10,MR) .... store for effective sampling rates at ends of reaches.  To allow mixing of reaches
* QDIFFGAP(MP10) .. quality of diffuse inflows added by gap filling
* QE(MP10,MR) ..... store for quality statistics for the end of the Reach. Used for the calculation of the Effective Sampling Rate and so for the calculation of confidence limits 
* quolity data() .. Stores of quality data for rivers and streams
* QNAT(MP10) ...... per cent changes in quality brought about by gap filling
* QNUM(NV,MP10) ... Sample numbers for river water quality
* Qstations() ..... calculated quality at monitoring stations
* QTYPE(MP10) ..... Type of determinand (equals 4 to ignore determinand in the calculation)
* QUALN(MP10) ..... Effective sampling rate for river quality
* QZEROGAP(MP10) ..... Baseline quality for extrapolation of exponential decay during gap filling
* RATE(MP10) ...... Global rate constants
* RFDIFF(MR) ...... Code number for river flow data-set for diffuse inflow to reaches
* RCHA(MR) ........ Time-of-travel - the velocity at the mean flow for reaches
* RCHB(MR) ........ Time-of-travel - the exponent for reaches 
* Rchkay(MP10,MR) . Reach rate constants
* RLENGTH(MR) ..... Lengths of the reaches
* RNAME(MR) ....... Names of the reaches
* RQDIFF(MR) ...... Code number for river quality data-set for diffuse inflow to reaches
* RQS(NQ,MP10) .... River quality targets entered in the main .DAT file
* MRQS(MP10) ...... Codes for the summary statistics used for the river quality standards
* SFL(MG,2) ....... Calculated flows at flow gauges
* UNITS(MP10) ..... Units for determinands
* ELOAD(MP10) ..... Store for calculated loads of effluents, streams etc added by mass balance
* ELOAD03(MP10) ... Store for calculated loads of effluent (sewage works)
* ELOAD05(MP10) ... Store for calculated loads of effluent
* ELOAD12(MP10) ... Store for calculated loads of effluent
* ELOAD39(MP10) ... Store for calculated loads of effluent
* XA(MP10) ........ Store for estimates of mean quality for determinands
* WEQ(3,MP10) ..... Worst possible effluent quality (when setting quality needed to meet river targets)
* BEQ(3,MP10) ..... Best possible effluent quality (when setting quality needed to meet river targets)
* Detype(Mp10) .... Code numbers for determinands.  For example, BOD is 101..      
* confail(mp10) ... Calculated confidence of failure for determinands
* propeff2(mp10) .. Proportion of load as effluent for determinands
* propeff3(mp10) .. Proportion of load as effluent for determinands
* propeff5(mp10) .. Proportion of load as effluent for determinands
* propeff12(mp10) . Proportion of load as effluent for determinands
* X95(MP10) ....... Calculated values of 95-percentiles at monitoring point
* X90(MP10) ....... Calculated values of 90-percentiles at monitoring point       
* X99(MP10) ....... Calculated values of 99-percentiles at monitoring point 
* Xp95(MP10) ...... Calculated values of 95-percentiles at plotting point
* Xp90(MP10) ...... Calculated values of 90-percentiles at plotting point       
* Xp99(MP10) ...... Calculated values of 99-percentiles at plotting point 
* XLOAD ........... Calculated loads for determinands as means and percentiles
* XEFF(MP10) ...... Calculated 95-percentile of effluent quality
* BSPLIT(NB) ...... Split of flow for bifurcations
* DSTART(MP10) .... Random number starters for river quality
* PSTART(MP10) .... Random number starters for discharge quality
* RFCL(MP10) ...... Default correlation coeffiecients for river quality on river flow
* EFCL(MP10) ...... Default correlation coeffiecients for river quality on discharge flow
* PRAN(MP10,MS) ... Default random deviates for discharge quality
* CRAN(MP10,MS) ... Default random deviates for river quality  
* ERAN(MS) ........ Default random deviates for discharge flow
* FRAN(MS) ........ Default random deviates for river quality
* TRAN(MS) ........ Default random deviates for temperature
* ReachCode(MR) ... Store of the code numbers for reaches
* RQO(MP10) ....... River quality targets
* ABLOAD(MP10,13) . Loads removed by abstractions
* FTMS(MS) ........ Shots for river flow
* temkay(MP10) .... Temporary storage of rate constants for reaches
* MRQO(MP10) ...... Codes for the summary statistics used for the river quality standards      
* SQTART .......... Special random number starter for river quality
* CSTART .......... Random number starter for temperature
* USTART .......... Random number starter for suspended solids
* diffuse load .... Store for calculated loads of diffuse pollution (MP10)
* llmon ........... Monthly summaries of concentrations for different sectors (nprop,MP10,2,13)
* TGLODE2 ......... Running total of net loads down the model (MP10)
* ------------------------------------------------------------------------------------------------------------------------------------
* TELODE1 ............. Accumulated loads of effluent for the current model run (MP10)
* TELODE2 ............. Accumulated loads of effluent for the current model run allowing for natural purification (MP10)
* TELOAD3 ............. Values of TELODE2 stored for later mixing of reaches (MP10)
* TELOAD303 ........... Values of TELODE2 for sewage works stored for later mixing of reaches (MP10)
* TELOAD305 ........... Values of TELODE2 for industrial discharges stored for later mixing of reaches (MP10)
* TELOAD312 ........... Values of TELODE2 stored for later mixing of reaches (MP10)
* TELOAD339 ........... Values of TELODE2 stored for later mixing of reaches (MP10)
* TELOAD4 ............. Values of TELODE1 stored for later mixing of reaches (MP10)
* TELOADS ............. As TELODE2 but for individual upstream discharges (NUED,MP10,13)
* TEloadsrch .......... monthly values of TELOADS at end of each reach (NUED,MR,MP10)
* TELOADAV              
* LLMON
* LLMON2
* -----------------------------------------------------------------------------------------------------------------------------------------
* TWLOADS ............. monthly loads from upstream sub-catchments (NUW,MP10,13)
* TWloadsrch .......... monthly loads from upstream sub-catchments at the ends of reaches (NUW,MR,MP10)
* TWLOADSapp .......... breakdown of monthly loads from upstream sub-catchments (NUW,MP10,13,NPROP)
* TWloadsrchapp ....... breakdown of monthly loads from sub-catchments at ends of reaches (NUW,MR,MP10,NPROP) 
* -----------------------------------------------------------------------------------------------------------------------------------------
* TNLOADUP1             Total load introduced by natural purification (MP10)
* TNLOADUP2             Total load introduced by natural purification (MP10)
* TNLOADDN1(MP10) ..... Running total of load removed by natural purification
* TNLOADDN2(MP10) ..... Running total of load removed by natural purification
* TDDLOAD2(MP10) ...... Total load introduced by clean diffuse sources
* TILOADUP2(MP10) ..... Total load introduced by gap filling for river flows
* TILOADDN2(MP10) ..... Total load removed by gap filling for river flows
* TALOADUP2(MP10) ..... Total load added by gap filling on river quality
* TALOADDN2(MP10) ..... Total load removed by gap filling on river quality
* T13LOAD1(MP10) ...... Total load from diffuse features (river flow type)
* T15LOAD1(MP10) ...... Total load from diffuse features (discharge flow type)
* T13LOAD2(MP10) ...... Net total load from diffuse features (river flow type)
* T15LOAD2(MP10) ...... Net total from diffuse features (discharge flow type)
* T--LOAD2(MP10) ...... Total load from diffuse features (discharge flow type)
* TBLOAD2(MP10) ....... Total load removed by abstractions ... 
* T13LOAD3(MR,MP1) .... Values of T13LOAD2 stored for later mixing of reaches
* T15LOAD3(MR,MP10) ... Values of T15LOAD2 stored for later mixing of reaches
* TxxLOAD3(MR,MP1) .... Values of TxxLOAD2 stored for later mixing of reaches
* TGLODE3(MR,MP10) .... Values of TGLODE2 stored for later mixing of reaches
* TRLOAD3(MR,MP10) .... Values of TRLODE2 stored for later mixing of reaches
* TNLOADUP3(MR,MP10) .. Values of TNLOADUP2 stored for later mixing of reaches
* TNLOADDN3(MR,MP10) .. Values of TNLOADDN2 stored for later mixing of reaches
* TDDLOAD3(MR,MP10) ..  Values of TDDLOAD2 stored for later mixing of reaches
* TALOADUP3(MR,MP10) .. Values of TALOADUP2 stored for later mixing of reaches
* TALOADDN3(MR,MP10) .. Values of TALOADDN2 stored for later mixing of reaches
* TILOADUP3(MR,MP10) .. Values of TILOADUP2 stored for later mixing of reaches
* TILOADDN3(MR,MP10) .. Values of TILOADDN2 stored for later mixing of reaches
* TBLOAD3(MR,MP10) .... Values of TBLOAD2 stored for later mixing of reaches
* T13LOAD4(MR,MP1) .... Values of T13LOAD1 stored for later mixing of reaches
* T15LOAD4(MR,MP10) ... Values of T15LOAD1 stored for later mixing of reaches
* TGLODE4(MR,MP10) .... Values of TGLODE1 stored for later mixing of reaches
* TRLOAD4(MR,MP10) .... Values of TRLODE1 stored for later mixing of reaches
* TNLOADUP4(MR,MP10) .. Values of TNLOADUP1 stored for later mixing of reaches
* TNLOADDN4(MR,MP10) .. Values of TNLOADDN1 stored for later mixing of reaches
* TDDLOAD4(MR,MP10) ... Values of TDDLOAD1 stored for later mixing of reaches
* TALOADUP4(MR,MP10) .. Values of TALOADUP1 stored for later mixing of reaches
* TALOADDN4(MR,MP10) .. Values of TALOADDN1 stored for later mixing of reaches
* TILOADUP4(MR,MP10) .. Values of TILOADUP1 stored for later mixing of reaches
* TILOADDN4(MR,MP10) .. Values of TILOADDN1 stored for later mixing of reaches
* TBLOAD3(MR,MP10) .... Values of TBLOAD1 stored for later mixing of reaches
* prune load (13) ..... Factor to scale back loads after abstractions and other losses
* kprune det .......... Identity of determinand for load pruning
* Check total(MP10) ... Array used to check totals of loads
* xupload(MP10,13) .... The increases in mean load from gap filling of river quality
* xdownload(MP10,13) .. The decreases in mean load from gap filling of river quality
* xupflow ............. The additions of flow from gap filling of flow
* xdownflow ........... The removals of flow from gap filling of flow
* Check sub total...... Sub-totals of load
* k90 ................. Index for obtaining 90-percentile from an ordered list of NS items
* k95 ................. Index for obtaining 95-percentile from an ordered list of NS items
* k99 ................. Index for obtaining 99-percentile from an ordered list of NS items
* k10 ................. Index for obtaining 10-percentile from an ordered list of NS items
* k05 ................. Index for obtaining 5-percentile from an ordered list of NS items
* k01 ................. Index for obtaining 1-percentile from an ordered list of NS items
* BM(4) ............... Scaling factors for correcting Monte Carlo sampling bias in the mean
* BS(4) ............... Scaling factors for correcting Monte Carlo sampling bias in the standard deviation or the 95-percentile lows flow
* spcorr .............. Take up the value of a correlation coefficient
* kf99 ................ Index for obtaining 99-percentile from an ordered list of NS items 
* kf95 ................ Index for obtaining 95-percentile from an ordered list of NS items
* kf90 ................ Index for obtaining 90-percentile from an ordered list of NS items
* kf80 ................ Index for obtaining 80-percentile from an ordered list of NS items 
* kf50 ................ Index for obtaining 50-percentile from an ordered list of NS items
* kf20 ................ Index for obtaining 20-percentile from an ordered list of NS items
* kf10 ................ Index for obtaining 10-percentile from an ordered list of NS items
* kf05 ................ Index for obtaining 5-percentile from an ordered list of NS items
* kf01                  Index for obtaining 1-percentile from an ordered list of NS items
* ===============================================================================================================
* Mass Balance ----------------------------------------------------------------------------
* ===============================================================================================================
* JP .................. Determinand number
* EFM ................. Mean discharge flow
* EFS ................. Standard deviation of discharge flow
* ECM ................. Mean discharge quality
* ECS ................. Standard deviation of discharge quality
* RF3 ................. Shift parameter for the distribution of river flow
* RC3 ................. Shift parameter for the distribution of river quality
* EF3 ................. Shift parameter for the distribution of effluent flow
* EC3 ................. Shift parameter for the distribution of effluent quality
* CO1 ................. Correlation coeffiecient between river flow and river quality
* CO2 ................. Correlation of tributary flow on main river flow; effluent flow and river flow
* CO3 ................. Correlation coefficient for river flow on discharge quality
* CO4 ................. Correlation coefficient for river quality on discharge flow
* CO5 ................. Correlation coefficient for discharge flow and discharge quality
* CO6 ................. Correlation coefficient for river quality on discharge quality
* ECX ................. Calculated 95-percentile of effluent quality
* TECM ................ Calculated mean effluent quality, for example to meet a river quality standard
* TECS ................ Calculated standard deviation of effluent quality, for example to meet a river quality standard
* TECX ................ Calculated 95-percentile of effluent quality, for example to meet a river quality standard
* TRCX ................ Calculated 95-percentile of river quality, for example downstream of an effluent
* FSTART .............. Random number starter for the generation of river flows   
* ESTART .............. Random number starter for the generation of effluent flows 
* GEFM ................ Mean discharge flow in the log domain
* GEFS ................ Standard deviation of discharge flow in the log domain         
* GECM ................ Mean discharge quality in the log domain
* GECS ................ Standard deviation of discharge quality in the log domain
* current downstream quality ....... Percentile of river quality downstream of current effluent
* IQDIST .............. Code setting the statistical distribution of river quality
* IFDIST .............. Code setting the statistical distribution of flow
* ECV ................. The coefficient of variation of effluent quality
* TIME ................ The time-of-travel in days
* b1 .................. Factor for combining of correlation coefficients 
* b2 .................. Factor for combining correlation coefficients 
* c1 .................. Factor for combining  correlation coefficients 
* c2 .................. Factor for combining correlation coefficients 
* d1 .................. Factor for combining correlation coefficients 
* d2 .................. Factor for combining correlation coefficients 
* EFshots  ............ The shots for effluent flow
* ECshots ............. The shots for effluent quality
* kstop ............... Suppress excessive print outs of infeasible correlation coeffiecients 
* EF5 ................. The 95-percentile low flow for tributaries
* IFEAT1 .............. With IFEAT2 the identity of the features used to define a stretch for gap filling
* IFEAT2 .............. With IFEAT2 the identity of the features used to define a stretch for gap filling
* ICTOP ............... Switch of indicator for the beginning of a reach
* KSIM ................ Counter of number of interative simulations used for gap filling
* FDUMP(MS) ........... Temporary strorage of flow shots
* CDUMP(MP10,MS) ...... Temporary strorage of quality shots
* UNAME(NU) ........... The names of the features
* VNAME(NU) ........... The names of the features
* ===============================================================================================================
* Non-parametric distributions -------------------------------------------------------------------------------------------
* ===============================================================================================================
* rfnpvl(mprf) ......... The class marks - the values read as data into SIMCAT.  There are "nprf" of these
* rfnpfd(mprf) ......... The cumulative frequencies that coorespond to the values entered
* nprf ................. The number of values read in
* rnpar ................ The uniform variate 
* RFN .................. The returned variate 
* IDENP(2,M7,MP10+1) ... identifier of the set of non-parametric data
* ===============================================================================================================
* monthly data -----------------------------------------------------------------------------------------------------------
* ===============================================================================================================
* seas1(12) ............... Monthly mean
* seas2(12) ............... Monthly standard deviation or 95-percentile low flow
* seas3(12) ............... Monthly shift parameter
* seas4(12) ............... Monthly corelation coefficient
* BSM(12) ................. Monthly version of BM
* BSS(12) ................. Monthly version of BS
* ISEASP(2,M8,MP10+1) ..... Identifier of the set of seasonal data
* FLMONTH(4,m8) ........... Names of the files holding the seasonal (monthly) data on river flow
* FLname(4,m7) ............ Names of the files holding the non-parametric data
* ihalt diffuse river ..... Counter to stop printing out excess measures on data for diffuse river-type flows 
* ihalt diffuse discharge . Counter to stop printing out excess measures on data for diffuse effluent-type flows
* xshots (MP10, MS) ....... Available to store all the river quality, as required 
* =======================================================================================================================
* Nclass ............................... Number of classes
* NC ................................... Dimensions of arrays used for classification
* Class limmits (NC,MP10) .............. The class boundaries
* total length in classes(NC,MP10) ..... Total lengths in classes in base model run 
* total length over all(NC) ............ Total lengths in classes in base model run (all determinands)
* =======================================================================================================================
* Items for choosing to run parts of models =============================================================================
* =======================================================================================================================
* Included() ........................... included reaches
* IncludedList() ....................... included reaches
* NFIN() ............................... array used to check the structure of reaches in the model
* NumIncluded .......................... number of reaches included
* =======================================================================================================================
* in class (NC, MP10) .. Calculated confidence in a class 
* conf in class (NC) ... Calculated confidence in a class 
* conf of class (NC) ... Calculated confidence in a class of better
* in or better (NC) .... Calculated confidence in a class of better
* Face Value ........... Face value class
* Face Value dets ...... Face value class for each determinand           
* QD90 temperature ..... Calculated 90-percentile of river temperature
* exclude BOD .......... Switch to exclude BOD from the classification
* exclude NO3 .......... Switch to exclude Nitrate from the classification
* exclude PO4 .......... Switch to exclude Phosphate from the classification
* exclude DOX .......... Switch to exclude Dissolved Oxygen from the classification
* exclude AMM .......... Switch to exclude Ammonia from the classification
* CL(12,MP10) .......... calculated concentrations for plotting ...
* -----------------------------------------------------------------------------------------------------------------------
*                        CL(1... is the mean
*                        CL(2... is the lower confidence limit on the mean
*                        CL(3... is the upper confidence limit on the mean
*                        CL(4... is the calculated value of the 95-percentile
*                        CL(5... is the lower confidence limit on the percentile
*                        CL(6... is the upper confidence limit on the percentile
*                        CL(7... is the calculated value of the 90-percentile
*                        CL(8... is the lower confidence limit on the percentile
*                        CL(9... is the upper confidence limit on the percentile
*                        CL(10.. is the calculated value of the 99-percentile
*                        CL(11.. is the lower confidence limit on this percentile
*                        CL(12.. is the upper confidence limit on this percentile
* -----------------------------------------------------------------------------------------------------------------------
* CD(12,MP10) .......... Calculated loads for plotting
* -----------------------------------------------------------------------------------------------------------------------
*                        CD(1... is the mean
*                        CD(2... is the lower confidence limit on the mean
*                        CD(3... is the upper confidence limit on the mean
*                        CD(4... is the calculated value of the 95-percentile
*                        CD(5... is the lower confidence limit on the percentile
*                        CD(6... is the upper confidence limit on the percentile
*                        CD(7... is the calculated value of the 90-percentile
*                        CD(8... is the lower confidence limit on the percentile
*                        CD(9... is the upper confidence limit on the percentile
*                        CD(10.. is the calculated value of the 99-percentile
*                        CD(11.. is the lower confidence limit on this percentile
*                        CD(12.. is the upper confidence limit on this percentile
* -----------------------------------------------------------------------------------------------------------------------
* COB(4,MP10+1) ........ Observed concentrations for plotting ... plus flows
* -----------------------------------------------------------------------------------------------------------------------
*                        COB(1... is the mean
*                        COB(2... is the calculated value of the 90-percentile
*                        COB(3... is the calculated value of the 95-percentile
*                        COB(4... is the calculated value of the 99-percentile
* ------------------------------------------------------------------------------------------------------------------------
* COD(4,MP10+1) ........ Observed loads for plotting ... plus flows
* -----------------------------------------------------------------------------------------------------------------------
*                        COD(1... is the mean
*                        COD(2... is the calculated value of the 90-percentile
*                        COD(3... is the calculated value of the 95-percentile
*                        COD(4... is the calculated value of the 99-percentile
* -----------------------------------------------------------------------------------------------------------------------
* elapsed days in year ..... accumulative number of days in the year
* days in months (12) ...... number of days in the month
* fraction of year (12) .... fraction of year formed by a month
* names of status class .... names of WFD classes: "High", "Good", etc
* ECM2 ..................... calculated mean discharge quality
* tony warn overide ........ switch to look for strange data and correct them
* check for infeasible quality .... flag set when massive concentrations are encountered.  Used to look for unexpected zero flows, especially in gap filling
* kswitch .................. records whether file SWITCHES.GPD exists
* bagset ................... cut-off point for including a discharge in back-tracking
* baggest .................. percentage impact of discharge used to decide whether to back-track
* =======================================================================================================================

