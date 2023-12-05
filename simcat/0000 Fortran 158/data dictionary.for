* ==============================================================================
* SIMCAT - Monte-Carlo simulation of water quality in a river ....
* ==============================================================================
* This file is a list of names of variables and descriptions of what they do ---
* An item in brackets indicates how many items there are ....
* ========================================================================================================================
* File DATA DICTIONARY.FOR ...    879 lines (all are comments) -----------------------------------------------------------
* ========================================================================================================================
* Total river length ... total length of river across the current river being modelled
* Grand river length ... total length of river across all batch runs 
* =============================================================================================
* ical ................. dictates the form of the calculation being done 
* =============================================================================================
*      === Run Type 0 .. basic run: mass balance of the input data
*      === Run Type 1 .. compute changes to river flows giving exact fit
*      === Run Type 2 .. apply computed changes to flows (from ICAL=1) extrapolating and interpolating
*      === Run Type 3 .. as 2 but also computing the changes in quality needed fo give a perfect fit
*      === Run Type 4 .. apply computed changes to flow (ICAL=1) and computed changes to quality (from ICAL=3)
*                     .. extrapolating and interpolating
*      === Run Type 5 .. run 1 followed by run 3 ...
*      === Run Type 6 .. run 1 followed by runs 3 and 4 ...
*      === Run Type 7 .. compute effluent standards that meet river targets
*      === Run Type 8 .. as 7 but with more emphasis on no deterioration 
*      === Run Type 9 .. as 8 but using a point within the target class as  upstream quality 
* ------------------------------------------------------------------------------------------------------------------------
* model number in batch  number of the set of models being run in a batch ...
* ------------------------------------------------------------------------------------------------------------------------
* feeture .............. number of the feature
* feetcount ............ the number of features
* nameprop ............. the names of all the types of Features used in apportionment
* numprop .............. feature type code linked to nameprop - the names of the types of Features
* F1 ................... the working array to store text such as the name of output files
* DatName .............. the name of the DAT file 
* folder ............... the name of the folder (directory) used to hold the data file and the output
* FUNIT ................ the units of flow
* FNAME ................ working array to store text such as the name of output files
* FNAME2 ............... working array to store text such as the name of output files
* BIGLAPSE ............. used to measure the time taken for a run of SIMCAT
* ------------------------------------------------------------------------------------------------------------------------
* classfraction ........ percentage of target class set as upstream quality within Run Type 9         
* jcycle ............... cycles of calculation with a single set of data
* ISTART ............... indicator of the start of a Reach
* KSTART ............... another indicator of the start of a Reach
* ICAL13 ............... a control to limit output when running cycles of gap filling
* ========================================================================================================================
* Mass Balance -----------------------------------------------------------------------------------------------------------
* ========================================================================================================================
* ITER ................. iteration counter for calculating discharge quality needed to meet the river target
* JP ................... determinand number
* JP1 .................. temporary reference number for a determinand
* EFM .................. mean discharge flow
* EFS .................. standard deviation of discharge flow
* ECM .................. mean discharge quality
* ECS .................. standard deviation of discharge quality
* RF3 .................. shift parameter for the distribution of river flow
* RC3 .................. shift parameter for the distribution of river quality
* EF3 .................. shift parameter for the distribution of effluent flow
* EC3 .................. shift parameter for the distribution of effluent quality
* CO1 .................. correlation coefficient between river flow and river quality
* CO2 .................. correlation of tributary flow on main river flow; effluent flow and river flow
* CO3 .................. correlation coefficient for river flow on discharge quality
* CO4 .................. correlation coefficient for river quality on discharge flow
* CO5 .................. correlation coefficient for discharge flow and discharge quality
* CO6 .................. correlation coefficient for river quality on discharge quality
* ECX .................. calculated 95-percentile of effluent quality
* TECM ................. calculated mean effluent quality, for example to meet a river quality standard
* TECS ................. calculated standard deviation of effluent quality, eg to meet a river quality standard
* TECX ................. calculated 95-percentile of effluent quality, eg to meet a river quality standard
* TRCX ................. calculated 95-percentile of river quality, for example downstream of an effluent
* FSTART ............... random number starter for the generation of river flows   
* ESTART ............... random number starter for the generation of effluent flows 
* GEFM ................. mean discharge flow in the log domain
* GEFS ................. standard deviation of discharge flow in the log domain         
* GECM ................. mean discharge quality in the log domain
* GECS ................. standard deviation of discharge quality in the log domain
* current ds quality ... percentile of river quality downstream of current effluent
* IQDIST ............... code setting the statistical distribution of river quality
* IFDIST ............... code setting the statistical distribution of flow
* ECV .................. the coefficient of variation of effluent quality
* TIME ................. the time-of-travel in days
* b1 ................... factor for combining correlation coefficients - upstream river flow and upstream river quality
* b2 ................... factor for combining correlation coefficients - upstream river flow and upstream river quality 
* c1 ................... factor for combining correlation coefficients - discharge flow & river flow
* c2 ................... factor for combining correlation coefficients - discharge flow & river flow 
* d1 ................... factor for combining correlation coefficients - discharge quality & discharge flow
* d2 ................... factor for combining correlation coefficients - discharge quality & discharge flow 
* EFshots  ............. the shots for effluent flow (or added river flows from streams etc)
* ECshots .............. the shots for effluent quality
* kstop ................ suppress excessive print outs of infeasible correlation coeffiecients 
* EF5 .................. the 95-percentile low flow for tributaries
* IFEAT1 ............... with IFEAT2 the identity of the features used to define a stretch for gap filling
* IFEAT2 ............... with IFEAT2 the identity of the features used to define a stretch for gap filling
* ICTOP ................ switch of indicator for the beginning of a reach
* FDUMP ................ temporary strorage of flow shots (MS)
* CDUMP ................ temporary strorage of quality shots (MP10,MS)
* UNAME ................ the names of the features (NU)
* unamex ............... name of the current feature or "end of reach" or "end of the model" etc
* VNAME ................ the names of the features (boxed to the right) 
* =============================================================================================================
* Non-parametric distributions --------------------------------------------------------------------------------
* ========================================================================================================================
* rfnpvl ............... the class marks - the values read as data into SIMCAT.There are "nprf" of these (mprf)
* rfnpfd ............... the cumulative frequencies that coorespond to the values entered (mprf)
* nprf ................. the number of values read in
* rnpar ................ the uniform variate 
* RFN .................. the returned variate 
* IDENP ................ identifier of the set of non-parametric data (2,M7,MP10+1)
* ========================================================================================================================
* monthly data and non-parametric data -----------------------------------------------------------------------------------
* ========================================================================================================================
* seas0 ................ type of monthly data (12)
* seas1 ................ monthly mean (12)
* seas2 ................ monthly standard deviation or 95-percentile low flow (12)
* seas3 ................ monthly shift parameter (12)
* seas4 ................ monthly corelation coefficient (12)
* BSM .................. monthly version of BM (12)
* BSS .................. monthly version of BS (12)
* ISEASP ............... identifier of the set of seasonal data (2,M8,MP10+1)
* FLMONTH .............. names of the files holding the seasonal (monthly) data on river flow (4,m8)
* FLMONTHsmall ......... names of files FLMONTH without the name of the folder that holds the DAT file (4,m8)
* FLSTRUCT ............. file names for monthly structure data (4,m9)
* FLSTRUCTsmall ........ names of files FLMONTH without the name of the folder that holds the DAT file (4,m9)
* FLname ............... names of the files holding the non-parametric data (4,m7)
* FLNAMEsmall .......... names of files FLNAME without the name of the folder that holds the DAT file (4,m7)
* FLTEMP ............... files names for temperature and suspended solida etc (4,m10)
* FLTEMPsmall .......... names of files FLTEMP without the name of the folder that holds the DAT file (4,m10)
* ihalt diffuse river .. counter to stop printing out excess messages on data for diffuse river-type flows 
* ihalt diff disch ..... counter to stop printing out excess messages on data for diffuse effluent-type flows 
* xshots ............... available to store all the river quality shots(MP10, MS) (MP10, MS)
* ========================================================================================================================
* Nclass ............... number of classes sequence of all NS
* NC ................... dimensions of arrays used for classification 
* class limmits ........ definitions of classification of river water quality (NC,MP10)
* class limmits2  ...... background class limits (based on the set of data in the targets section of the DAT file)
* totals in classes .... total lengths in classes in base model run (NC,MP10)
* totals over all ...... total lengths in classes in base model run (all determinands) (NC)
* ------------------------------------------------------------------------------------------------------------------------
* K50,K90,K95,K98,K99 .. the numbers of the shots that is each percentile in a 
* K995,K999 ............ ranked list of all NS values
* JS50,JS90,JS95,JS98 .. numbers used in the apportionment of percentiles - the 
* JS99,JS995,JS999 ..... the number of the shot that is each percentile 
* KFEET ................ the number identifying a Feature
* ifdiffuse ............ switch identifying types of diffuse pollution - river type or effluent type
* kerror ............... control on the output on errors in data
* NDETlast ............. number of the last determinand in the current run 
* ndetfirst ............ number of the first determinand in the current run
* kmunth ............... number of the month (1 to 12)
* NDETBOD .............. determinand number for BOD
* kountd1 .............. a counter for duplicate dataset code numbers for river quality
* kountd2 .............. a counter for duplicate dataset code numbers (effluent flow and quality)
* NMORQ ................ item within store of summary statistic corresponding to a particular "output mode" 
* ucms ................. stores of the shots of upstream river quality (MP10,MS) 
* ufms ................. stores of the shots of upstream river flows (MS) 
* accum load ........... acculated gain in load gap filling (MS)
* UCTMS ................ upstream concentrations from types of feature (n2prop,MP10,MS) 
* ifeffcsv ............. switch for setting up CSV output for effluents 
* background class ..... river class that is set as a default standard
* add conc ............. control on the addition of concentration without flow
* SKIPPEFF ............. a device for ignoring discharges in Modes 7 and 8 (NU)
* classobj ............. number of the river class used as an objective
* diffheadqual ......... diffuse sources to be added to flows for the headwaters of a Reach (MR,20,2)
* kdecision ............ storage of the type of decision taken (eg retain the current discharge quality) (MP10)
* kreplace  ............ indicator that upstream river quality has been replaced in Mode 9 (MP10) ...161
* ========================================================================================================================
* YY ................... array used to manipulate Monte Carlo shots (MS)
* perkmini11 ........... the cut off percentile for the power curve (values below this are set to zero)
* powermini ............ stored values of the cut off percentiles (values below this are set to zero)
* baselode ............. the base load for the power curve (specified as a percentage of the mean)  
* powerbase ............ stored values of baseloads for the power curve (specified as a percentage of the mean)  
* rerkmini11 ........... normal deviate for perkmini11
* avloadp .............. average load used in the power equation
* avconcp .............. mean produced by the power curve
* baseconc ............. the base concentration for the power curve (specified as a percentage of the mean) 
* ========================================================================================================================
* XLOAD1 ............... summary statistics for dissolved metal
* XLOAD2 ............... summary statistics for solid metal
* XECM ................. mean effluent quality 
* SPLIT ................ used to split river into bifurcations
* cut off zero flow .... cut off point for zero flows from an intermittent
* cut off zero quality . cut off point for zero quality from an intermittent
* SRAN ................. normal deviates for suspended solids (MS)
* ERANR ................ correlated random normal deviates from ERAN function (MS)
* ERANR2 ............... truncated random numbers (MS)
* ERANR3 ............... truncated random normal deviates (MS)
* ========================================================================================================================
* TLOSSES2 ............. total load lost with abstractions, natural purification and gap filling (MP10,13) 
* ------------------------------------------------------------------------------------------------------------------------
* n13 .................. the number of months in the year plus one
* i13 .................. the index value in the number of 13 that holds the annual total
* NSM .................. the number of shots per month (MP10,13)
* ========================================================================================================================
* cmon ................. statistical summaries of monthly quality (MP10,2,13)
* fmon ................. statistical summaries of monthly flow data (2,13)
* tmon ................. statistical summaries of monthly temperature (2,13)
* smon ................. statistical summaries of monthly data on suspended solids (2,13)
* P1mon ................ statistical summaries of monthly data for dissolved metal
* P2mon ................ statistical summaries of monthly data for solid metal 
* ========================================================================================================================
* struckd .............. counter of the monthly structure sets 
* struct0 .............. type of distributions for monthly data
* TEMPD ................ counter of the monthly structure data-sets for temperature 
* shot day ............. a counter for placing shots within months
* seas0,1,2 etc ........ monthly data
* ========================================================================================================== 158
* T03LOAD1 ....158...... total loads from sewage effluents (03) (MP10,13)
* T05LOAD1.....158...... total loads from industrial effluents (05) (MP10,13)
* T12LOAD1 ....158...... total loads from intermittent discharges (12) (MP10,13)
* T39LOAD1 ....158...... total loads from point discharge from mine waters (39) (MP10,13)
* T60LOAD1 ....158...... total loads from (60) (MP10,13)
* T61LOAD1 ....158...... total loads from (61) (MP10,13)
* T62LOAD1 ....158...... total loads from (62) (MP10,13)
* T03LOAD2 ....158...... net loads from sewage effluents (03) (MP10,13)  
* T05LOAD2 ....158...... net loads from industrial discharges (05) (MP10,13)
* T12LOAD2 ....158...... net loads from from intermittent discharges (12) (MP10,13)
* T39LOAD2 ....158...... net loads from point discharge from mine waters (39) (MP10,13)
* T60LOAD2 ....158...... net loads from other point source discharges (60) (MP10,13)
* T61LOAD2 ....158...... net loads from private wastewaters (61) (MP10,13)
* T62LOAD2 ....158...... net loads from fish farms (62) (MP10,13)
* T03LOAD3 ....158...... net at d/s ends of Reaches - stored for mixing to form new Reaches (MR,MP10,13)
* T05LOAD3 ....158...... net at d/s ends of Reaches - stored for mixing to form new Reaches (MR,MP10,13)
* T12LOAD3 ....158...... net at d/s ends of Reaches - stored for mixing to form new Reaches (MR,MP10,13)
* T39LOAD3 ....158...... net at d/s ends of Reaches - stored for mixing to form new Reaches (MR,MP10,13)
* T60LOAD3 ....158...... net at d/s ends of Reaches - stored for mixing to form new Reaches (MR,MP10,13)
* T61LOAD3 ....158...... net at d/s ends of Reaches - stored for mixing to form new Reaches (MR,MP10,13)
* T62LOAD3 ....158...... net at d/s ends of Reaches - stored for mixing to form new Reaches (MR,MP10,13)
* T03LOAD4 ....158...... total at d/s ends of Reaches - stored for mixing to form new Reaches (MR,MP10,13) 
* T05LOAD4 ....158...... total at d/s ends of Reaches - stored for mixing to form new Reaches (MR,MP10,13)
* T12LOAD4 ....158...... total at d/s ends of Reaches - stored for form new Reaches (MR,MP10,13)
* T39LOAD4 ....158...... total at d/s ends of Reaches - stored for mixing to form new Reaches (MR,MP10,13)
* T60LOAD4 ....158...... total at d/s ends of Reaches - stored for mixing to form new Reaches (MR,MP10,13)
* T61LOAD4 ....158...... total at d/s ends of Reaches - stored for mixing to form new Reaches (MR,MP10,13)
* T62LOAD4 ....158...... total at d/s ends of Reaches - stored for mixing to form new Reaches (MR,MP10,13)
* ========================================================================================================== 158
* NUED ................. the number of effluent discharges for back-tracking
* TELOADS .......158.... as TELODE2 but for individual upstream discharges (NUED,MP10,13)
* TELOADAVrch ...158.... values of TELOADAV ... stored for mixing to form new Reaches (NUED,KR,MP10)
* TELOADAV ............. average loads from individual upstream effluents (NUED,MP10)           
* NUW .................. the maximum number of sub-catchments (water-bodies)
* TDLOADAV  ............ average load from each sub-catchment (NUW,MP10) 
* TWLOADS .............. breakdown of monthly loads from upstream sub-catchments (NUW,MP10,13)
* TWloadsrch ........... monthly loads from upstream sub-catchments at the ends of reaches (NUW,MR,MP10)
* TWLOADSapp ........... breakdown of monthly loads from upstream sub-catchments (NUW,MP10,13,N2PROP)
* TWloadsrchapp ........ breakdown of monthly loads from sub-catchments at the ends of Reaches (NUW,MR,MP10,N2PROP) 
* xworksAV ............. % of river load from individual discharges 
* ========================================================================================================================
* mark works ........... save the works for back-tracking
* kountworks ........... count the number of works
* identify werks ....... feature numbers for identified works (NUED)
* need works ........... count the number of works for back-tracking
* kill works ........... count the number of works too small to be retained for back-tracking
* kount bodies ......... count the number of sub-catchments (water bodies)
* identify bodies ...... a list of the water-bodies (the sequence numbers assigned to the water-body Feature) (NUW) 
* identify reaches ..... reaches affected by the water-bodies (3,NUW)
* spcorrRFaf ........... correlation of flow on added flow
* spcorrRFRC ........... correlation of river flow on river quality
* spcorrfa ............. correlation of effluent (or added) flow on quality
* kstat2 ............... register of whether summary statistics have been specified
* ========================================================================================================================
* itdist ............... type of distribution cited for monthly data
* struct1 .............. monthly means for January, February etc (12)
* struct2 .............. monthly 95-percentile low flows or standard deviatiions for January, February etc (12)
* struct3 .............. monthly shifs for January, February etc (12) 
* struct4 .............. monthly correlations for January, February etc (12) 
* struct6 .............. standard deviation for each month (12)
* istruct .............. store of the dataset numbers for the monthly structure data (2,M9,MP10+1)
* idfcheck ............. marker used to guide checking for correct data when imposing a monthly structure
* itempp ............... stored reach number for datasets used for the 
*                        monthly structure for suspended solids and temperature (2,M9,2)
* FLsequenc ............ stored requests whether to sequence non-parametric input data (4,m7) 
* jstructure message ... controls messages about monthly straucture
* ========================================================================================================================
* CP1 .................. summary statistics (mean etc) for the dissolved metal (12,MP10) 
* CP2 .................. summary statistics (mean etc) for the solid metal (12,MP10) 
* addid load ........... summation of loads (MP10)
* BMS .................. shots for temperature and suspended solids (3,MS)
* BC ................... calculated summary statistics for temperature and suspended solids (3,5)   
* BMS global  .......... global shots for temperature and suspended solids (3,MS)
* PMS1  ................ shots for a dissolved metal (MS) 
* PMS2  ................ shots for a total metal (MS)
* ========================================================================================================================
* jxtype ............... indicator of start of reach (111) or end of reach (999)
* running mean ......... calculation of changing mean as various types of Features are considered
* zero check ........... value used to decide whether to print out small values 
* exceedences50 ........ calculate days exceeding annual mean (MP10,13)
* exceedences90 ........ calculate days exceeding 90-percentile (MP10,13),
* exceedences95 ........ calculate days exceeding 95-percentile (MP10,13)
* CMcontrib ............ summaries of contribution to annual mean concentration for different sectors (n2prop,MP10,5)
* LMcontrib ............ summaries of contribution to annual mean load for different sectors (n2prop,MP10,2)
* munthly structure .... switch to set up monthly structure on annual flow data
* month write 1 ........ used to limit error repeats of error messages for temperature
* lake45 ............... indicator of flow from a lake
* number of stds ....... number of standards for a Reach
* standards for reach .. reach specific standards that over-write others (MR,NC)
* Creg ................. store the data for regression(4,MS)
* ========================================================================================================================
* Pa ................... shots of dissolved metal (NS)
* Pb ..................  shots of solid metal (NS)
* CD1 .................. monthly statistics for dissolved metal (12,MP10)
* CD2 .................. monthly statistics for solid metal (12,MP10)
* cpart1 ............... partition - dissolved metal (MP10,5)
* cpart2 ............... partition - solid metal (MP10,5)
* Partishun ............ partition coefficients (MP10)
* ========================================================================================================================
* Parameters used for the dimensions of arrays
* ========================================================================================================================
* KR ................... the maximum mumber of Reach data-sets kept in store  for subsequent mixing with other Reaches
* MG ................... the number of river flow gauges allowed in SIMCAT
* MI ................... the maximum number of the old type of intermittent discharges (no longer used)
* MM ................... the maximum number of river quality monitoring stations allowed in SIMCAT
* MO ................... the maximum number of statistics entered as flow and quality data...
* MPRF.................. the maximum number of data-points that can be used to define a non-parametric distribution 
* MR ................... reaches allowed in SIMCAT (NREACH hold the actual number in the model being run)
* MS ................... the maximum number of Monte-Carlo shots - the actual number in the current run will be NS
* MT.................... the maximum size of utility array used in tests of input data etc (the maximum of NE, NV, NF etc)
* M7.................... the maximum number of data-sets for non-parametric distributions 
* M8 ................... the maximum number of data-sets for monthly data 
* NB ................... the maximum number of bifurcations 
* NE ................... the maximum number of sets of data for effluents 
* NF ................... the maximum number of sets of flow data for rivers 
* NM ................... the number of summary statistics calculated from the shots: mean, standard deviation. percentiles
* MP10 ................. the maximum number of river quality determinands
* NQ ................... the maximum number of sets of data on river quality targets 
* NU ................... the maximum number of Features 
* NV ................... the maximum number of sets of quality data for rivers allowed in SIMCAT
* =========================================================================================================================
* KR ................... number of reach data-sets kept in store --------
* M9 ................... number of data-sets for monthly structure data -
* M10 .................. number of data-sets for temperature data -------
* MT ................... size of utility array ( Maximum of NE, etc) ----
* MG ................... maximum number of flow gauges ------------------
* NC ................... maximum classes in the classification system ---
* NPROP ................ number of types of feature tracked for inputs --
* n2prop ............... as NPROP but with the addition of data from different types of diffuse pollution ----------------
* NTD .................. array number that holds total pollution in LMcontrib and CMcontrib ------------------------------
* NTF .................. array number that holds diffuse pollution in LMcontrib and CMcontrib ----------------------------
* ========================================================================================================================
* Names of the variables
* ========================================================================================================================
* dcalflow ............. is the distance from the head of the current Reach, or 
*                        from the last point on the Reach of gap filling of river flow
* dcalquality .......... is the distance from the head of the current Reach, or 
*                        from the last point of gap filling of river quality
* DINT ................. is used when interpolating between features. The length 
*                        of river is split into lengths of equal size, DINT
* LENGTH OF MAIN RIVER . the current distance from the head of the first Reach.It is used for the plotting of profiles
* DISTP ................ the current distance between successive Features
* DISTR ................ the distance from the head of the current Reach to the next Feature
* TDEG ................. the mean temperature in all the rivers and streams in the model
* TSDEV ................ the corresponding standard deviation for temperature 
* TCORF ................ the correlation coefficient between temperature and flow across the model
* master set used ...... if set equal to 1 use a master set of certain data - that from from 
*                        the first set of data in the batch
* Switchesname ......... a file (Switches.GPD) containing various switches, 
*                        which, if it exists, is imposed in each of a set of Batch runs 
* NOTARG ............... the variable to count the number of sets of river targets
* iforce gap fill ...... is a switch to force in the flow gap filling using the flow data entered
*                        for the feature even if there is no flow data-set up in JFCAL
* CALSPECIAL ........... this is a switch that specifies the use of existing gap filling files if calspecial = 1
* ========================================================================================================================
* Warnings about data
* ========================================================================================================================
* isupprezz ............ total number of warnings about data
* suppress1 ............ illegal data for river flow  
* suppress3 ............ huge change in flow 
* suppress4 ............ no determinands for classification 
* suppress5 ............ error reading flow gap filling data 
* suppress6 ............ zero 95-percentile flow set to 1% of mean 
* suppress7 ............ unnecessary monthly structures 
* suppress8 ............ quality data not set for gap fill 
* suppress9 ............ problems in the monthly structure 
* suppress9a ........... problems in the monthly temperature 
* suppress9b ........... problems in the monthly sus.solids 
* suppress9c ........... shot greater than a billion for
* suppress9d ........... calculated discharge quality exceeds a huge number
* suppress10 ........... illegal data for river flow 
* suppress11 ........... flow exceeds a billion 
* suppress12 ........... problems with monthly structure 
* suppress13 ........... zero entered as the 95-percentile river flow 
* suppress14 ........... infeasibly huge concentration 
* suppress15 ........... infeasibly huge loads produced by power-curves  
* suppress16 ........... unneeded effluent data ignored 
* suppress17 ........... deleted feature     
* suppress18 ........... zero variation in river quality 
* suppress19 ........... unneeded data ignored  
* suppress20 ........... infeasible correlation coefficients 
* suppress21 ........... no gap-filling information 
* suppress22 ........... data cannot be used to add diffuse pollution headwaters 
* suppress23 ........... unneeded data ignored 
* suppress00 ........... all the others 
* maxistor ............. maximum number of stored reaches 
* ------------------------------------------------------------------------------------------------------------------------
* Total RQS length 1 ... total length of river with a river quality target
* Total length 2 ....... total length of river with a river quality target
* Total length 00 ...... total length of river complying with 50 per cent confidence
* Total length 50 ...... total length of river failing with 50 per cent confidence
* Total length 95 ...... total length of river failing with 95 per cent confidence
* Total length dets 00   total length of river complying with 50 per cent confidence for each determinand ... 
* Total length dets 50   total length of river failing with 50 per cent confidence for each determinand 
* Total length dets 95   total length of river failing with 95 per cent confidence for each determinand ... 
* ========================================================================================================================
* ifbatch .............. if "ifbatch" is set equal to 1 this is a batch run 
* noGIS ................ switch to request production of the huge GIS output files (when noGIS is set equal equal to zero)
* nobigout ............. switch to request production of full output (if nobigout equals zero)
* IDIFFREACH ........... switch for inputing global diffuse sources as part of the Reach data
* IEND ................. indicator that the end of this Reach has been reached
* IF ................... number of the dataset for river flow
* IE ................... number of the dataset for discharge flow and quality
* IPRINT ............... variable to suppress output. Set to 0 to retain all the oitput - 
*                        set to 1 to exclude output on non-effluent features
* IPUR ................. switch for including or excluding natural purification
* IQ and IQMON ......... number of the dataset for river quality
* IREACH ............... number of the reach
* IMM .................. indicator that there are too many monitoring stations
* IMONTH ............... number of the month from 1 to 12
* IHR .................. hour for the time the SIMCAT run was started
* IMIN ................. minute for the time the SIMCAT run was started
* ISEC ................. second for the time the SIMCAT run was started
* IHUN ................. hundredth of second for the time the SIMCAT run was started
* IYEAR ................ year for the date the SIMCAT run was started
* JMONTH ............... month for the date the SIMCAT run was started
* IDAY ................. day for the date the SIMCAT run was started
* JSKIP ................ suppress unnecessary output and calculations
* LMONP ................ number of sets of the results for the summaries of output
* KEPOL15 .............. switch discharge-based diffuse pollution on or off
* KEPOL42 .............. switch diffuse pollution on or off (aggregated STWs)
* KINT ................. switch for adding interpolation points
* KFEAT ................ number of the feature
* KPTARG ............... percentile point
* diffuse type ......... code number for the type of diffuse pollution (25,27,29, etc)
* discharge type ....... code number for the type of discharge types of point pollution (15,42 etc)
* diffuse heading ...... switch to set up a diffuse heading within the output
* KRFPOL13 ............. switch on diffuse pollution (river type - flow)
* KRQPOL13 ............. switch on diffuse pollution (river type - quality)
* KRFPOL25 ............. switch on livestock pollution (river type - flow)
* KRQPOL25 ............. switch on livestock pollution (river type - quality)
* KRFPOL27 ............. switch on arable pollution (river type - flow)
* KRQPOL27 ............. switch on arable pollution (river type - quality)
* KRFPOL29 ............. switch on highway runoff (river type - flow)
* KRQPOL29 ............. switch on highway runoff (river type - quality)
* KRFPOL31 ............. switch on urban runoff (river type - flow)
* KRQPOL31 ............. switch on urban runoff (river type - quality)
* KRFPOL33 ............. switch on atmospheric deposition (river type - flow)
* KRQPOL33 ............. switch on atmospheric deposition (river type - quality)
* KRFPOL35 ............. switch on natural background (river type - flow)
* KRQPOL35 ............. switch on natural background (river type - quality)
* KRFPOL46 ............. switch on diffuse mines (river type - flow)
* KRQPOL46 ............. switch on diffuse mines (river type - quality)
* KRFPOL37 ............. switch on septic tanks (river type - flow)
* KRQPOL37 ............. switch on septic tanks (river type - quality)
* KRFPOL39 ............. switch on intermittent discharge (river type - flow)
* KRQPOL39 ............. switch on intermittent discharge (river type - quality)
* KRFPOL40 ............. switch on aggregate CSOs (river type - flow)
* KRQPOL40 ............. switch on aggregate CSOs (river type - quality)
* KRFPOL46 ............. switch on diffuse pollution (river type - flow)
* KRQPOL46 ............. switch on diffuse pollution (river type - quality)
* KRFPOL48 ............. switch on diffuse pollution (river type - flow)
* KRQPOL48 ............. switch on diffuse pollution (river type - quality)
* KRFPOL50 ............. switch on user-defined diffuse (river type - flow)
* KRQPOL50 ............. switch on user-defined diffuse (river type - quality)
* KRFPOL52 ............. switch on user-defined diffuse (river type - flow)
* KRQPOL52 ............. switch on user-defined diffuse (river type - quality)
* KRFPOL54 ............. switch on user-defined diffuse (river type - flow)
* KRQPOL54 ............. switch on user-defined diffuse (river type - quality)
* KRFPOL56 ............. switch on user-defined diffuse (river type - flow)
* KRQPOL56 ............. switch on user-defined diffuse (river type - quality)
* KRFPOL58 ............. switch on user-defined diffuse (river type - flow)
* KRQPOL58 ............. switch on user-defined diffuse (river type - quality)
* masterdata ........... some of the data for the first model will apply to all the other models in the batch run
*                        masterdata is set to 1 when this happens
* master output mode ... master value of MORQ
* masterNS ............. master value of NS (the number of shots)
* masterIPUR ........... master value of IPUR
* masterIKINT .......... master value of IKINT
* masterIPRINT ......... master value of IPRINT
* masterQTYPE .......... master value of QTYPE
* masterBASE ........... master value of BASE
* masterWEQ ............ master value of WEQ
* masterBEQ ............ master value of BEQ
* masterGEQ ............ master value of GEQ
* masterMRQS ........... master value of MRQS
* MPAR ................. set the number of determinands (in order to check all records are read in)
* MONF ................. request monitoring output for flow (if MONF equals 2)
* MONQ ................. request monitoring output for quality (if MONQ equals 2)
* output mode .......... for mean or percentile modes for output
* NMORQ ................ item within store of summary statistic corresponding to a particular "output mode" 
* MU ................... number of users (Features)
* NDET ................. number of determinands in current run
* NEXC ................. set to 1 to suppress the downstream extrapolation of corrections for quality
* NEXF ................. set to 1 to suppress the downstream extrapolation of corrections for flow
* NGAUGE ............... the number of flow gauges
* NI ................... the number of (old-style) intermittent discharge
* NP ................... number of river quality determinands (use to set up arrays)
* NREACH ............... the number of reaches
* NS ................... the number of shots
* SEASD ................ the number of monthly data-sets
* NONPD ................ number of non-parametric distributions
* negorder ............. number of the starting reach when only a subset is being run 
* Globaldataname ....... the file on global data changes
* Datfilename .......... the name of the data file
* Title ................ title of run given in the datafile
* Line of data ......... item to hold a line of data
* ISCREEN .............. control on the amount of information written to the screen
* LAPSE ................ the time taken for the SIMCAT run
* flowchars ............ seven characters used to produce tidy output of flows
* Sevenchars ........... seven characters used to produce tidy output of quality
* valchars10 ........... batch of characters used to produce tidy output
* valchars11 ........... batch of characters used to produce tidy output
* valchars12 ........... batch of characters used to produce tidy output
* B44 .................. array used used to produce tidy output of quality
* virtualreach ......... indicates a virtual reach when virtualreach = 1
* bday,bmon ............ day and month  of current run of SIMCAT
* bh,bmin .............. hour and minute of current run of SIMCAT
* ========================================================================================================================
* arrays
* ========================================================================================================================
* ADIST................. the length of main river upstream of start of a reach.  
*                        This is used in assembling data for graphs (MR)
* BIDENT(NB) ........... the sequence numbers of features that are bifurcations 
* C(MP10,5) ............ summary statistics for river quality for all determinands (MP10)
* CLD(MP10,5) .......... alternative version of C(MP10,5) used when the values need to be retained
* CMS(MP10,MS) ......... the values of all the shots for all the determinands
* CMX(MP10,MS) ......... array used for temporary storage of CMS etc, in doing calculations for gap filling
* ------------------------------------------------------------------------------------------------------------------------
* CTMS(n2prop...) ...... concentrations from various types of feature 
* n2prop ............... number of different types of feature tracked for inputs 
* seqappbodies(n2prop) . the sequence of the features that used for output in files like ACL
* EQD(n2prop...)........ values of CTMS for ends of reaches.  Stored for the mixing of reaches 
* jorder(n2prop)........ sequencing of contribution used for output files
* nbodyprop(n2prop) .... numbers of feature types in a current water body
* numpfeet(62) ......... number of apportionment group for each type of feature
* ------------------------------------------------------------------------------------------------------------------------
* DIST(NU) ............. distance of feature from the head of its reach
* DNA .................. short name of determinands (up to four characters) (MP10)
* DNAME ................ long name of determinads (up to 11 characters) (MP10)
* EFD(KR,MS) ........... proportion of effluent in each shot of river flow - at the end of thr reach
* F(NF,MO) ............. summary statistics for input river flow data sets: mean, standard deviation etc
* DFMS(MS) ............. store of downstream flows for extra calculations ----
* FD(KR,MS) ............ values of river flow shots at the end of the reach 
* FE(NE,MO) ............ summary statistics for input discharge flow data sets: mean, standard deviation etc 
* FEND.................. flow and quality statistics for the end ofthe reach. These are used for the calculation 
*                        of the Effective and so for the calculation of confidence limits (MR)
* FLOW ................. calculated values of the summary statistics of river flow: mean, standard deviation etc (NM)
* EFLOW ................ summary statistics calculated for flow shots from a tributary or a discharge (NM)
* EFMS ................. proportion of effluent flow in each shot of river flow (MS)
* FMS .................. the shots for river flow (MS)
* FMX .................. array used for temporary storage of FMS etc, in doing calculations for gap filling (MS)
* GEQ .................. summary statistics for the definition of good effluent quality (3,MP10)
* IFRQS ................ reference numbers for sets of river quality targets for each feature (NU)
* add diffconc ......... negative values indicate no extra river flow accompanies the diffuse inflow (NU)
* IGNORE NUMBER ........ the calculated number of types of diffuse sources to a Reach that add no extra flow 
* IPLAN ................ an array that shows which Reach is to be processed 
*                        next so giving the structure of the catchment (MR,3) 
* IRHOLD................ used to store the data for a reach that is needed 
*                        further on to mix with others reaches (MR)
* JF ................... number of the set of river flow data for the feature (NU)
* JFCAL ................ number of the set of river flow data used for gap filling.
*                        If JFCAL equals zero SIMCAT will extrapolate if required (NU)
* JFUSER ............... variable to suppress extrapolation of gap filling of river flow (NU)
* suppress ............. suppress gap filling
* no gap filling 78 .... internal switch for gap filling
* ------------------------------------------------------------------------------------------------------------------------
* JQ ................... number of the set of river quality data for this feature (NU)
* JQCAL ................ number of the set of river quality data used for gap filling (NU)
* JQUSER ............... switch set to suppress extrapolation of gap filling of river quality (NU)
* JREACH ............... number of the reach on which the feature is located (NU)
* JSTOR ................ a store of data for a reach that is needed further on to mix with others reaches (KR)
* JT ................... the type of feature - 3 for a swage treatment works etc (NU)
* KFCAL ................ gap filling switch -set to "1" d/s of the last gap 
*                        filling point - and zero at and u/s of this  (NU)
* KGAUGES .............. list of features that are flow gauges (MG)
* KQCAL ................ gap filling switches - set to "1" d/s of the last gap
*                        filling point - and zero at and u/s of this (NU)
* LSAMPOINTS ........... list of features that are river quality monitoring 
*                        points (MM)
* MK ................... array used to store values for checking data are read
*                        in correctly (MT)
* PDEC ................. type of probability distribution for discharge 
*                        quality (NE,MP10)
* PDEF ................. type of probability distribution for discharge flow (NE)
* PDRC ................. type of probability distribution for river quality (NV,MP10)
* PDRF ................. type of probability distribution for river flow (NF)
* ENUM ................. numbers of samples for effluent flows (NE)
* pollution data........ discharge quality data - mean, standard deviation etc 
*                        (NE,MP10,MO)
* FLOW ................. running values of summary statistics for river flow: 
*                        mean, 95-percentile (5)
* GISCODE .............. unique identifier for help in using SIMCAT output with
*                        GIS (NU)
* negreach ............. an indicator that only a subset of the reaches will be 
*                        run ... first reach is now number NEGREACH
* reachstore ........... information for a reduced run ... reachstore holds the 
*                        numbers of the reaches to be included (MR)
* flowstats ............ running values of the percentile points for the flow 
*                        distribution (9)
* propstats ............ running values of the proportions of discharge (9)
* NextReach............. number of the next reach to be processed (MR)
* start reach .......... reach number used to connect reaches
* kount reach .......... number of features on each reach (MR)
* BMAT ................. the mean, standard deviation etc for the temperatures 
*                        of Reaches (MR,3,3)
* BNUM ................. number of samples for temperature and suspended solids 
*                        for Reaches (MR,3)  
* PDBC ................. identification of the data on temperature and suspended 
*                        solids for this reach (MR,3)
* PNUM ................. numbers of samples of discharge quality (NE,MP10)
* QBASE ................ limit to which natural purification can improve river 
*                        quality (MP10)
* QD ................... store for shots and the ends of reaches.  To allow 
*                        mixing of reaches (KR,MP10,MS)
* QDN .................. store for effective sampling rates at ends of reaches.  
*                        To allow mixing of reaches (MP10,MR)
* QDIFFGAP ............. quality of diffuse inflows added by gap filling (MP10)
* QE ................... mean quality at the ends of Reaches ... used to 
*                        calculate the Effective Sampling Rates for mixed 
*                        Reaches (MP10,MR)
* quolity data ......... stores of quality data for rivers and streams (NV,MP10,MO+2)
* QNAT ................. per cent changes in quality brought about by gap filling (MP10)
* QNUM ................. sample numbers for river water quality (NV,MP10)
* Qstations ............ calculated quality at monitoring stations (MM,MP10,3)
* QTYPE ................ type of determinand (equals 4 to ignore determinand)
*                        the calculation) (MP10)
* QUALN ................ effective sampling rate for river quality (MP10)
* QUALNB ............... effective sampling rate for contributions to river quality (MP10,n2prop)
* RQUALNB .............. effective sampling rate for contributions to river quality at end of reach
* QZEROGAP ............. baseline quality for extrapolation of exponential decay
*                        during gap filling (MP10)
* RATE ................. global rate constants (MP10)
* RFDIFF ............... code number for river flow data-set for diffuse inflow 
*                        to reaches (MR)
* RCHA ................. time-of-travel - the velocity at the mean flow for reaches (MR)
* RCHB ................. time-of-travel - the exponent for reaches (MR)
* Rchkay ............... reach rate constants (MP10,MR)
* RLENGTH............... lengths of the reaches (MR)
* RNAME................. names of the reaches (MR)
* rnamex ............... name using terms like "upstream of works"
* EQS reach ............ river quality target for Reaches (MR,MP10)
* RQDIFF................ code number for river quality data-set for diffuse inflow to reaches (MR)
* RQS .................. river quality targets entered in the main .DAT file (NQ,MP10)
* MRQS ................. codes for the summary statistics used for the river quality standards (MP10)
* SFL .................. calculated flows at flow gauges (MG,2)
* UNITS ................ units for determinands (MP10)
* LUNITS ............... units for loads (MP10)
* ELOAD ................ store for calculated mean loads of effluents, streams etc added by mass balance (MP10,13)
* ELOADS ............... standard deviation of loads (MP10)
* ELOAD03 ....158....... store for calculated mean loads of effluent (Type 3: sewage works) (MP10)
* ELOAD05 ....158....... store for calculated mean loads of effluent (Type 5: industrial works) (MP10)
* ELOAD12 ....158....... store for calculated mean loads of effluent (Type 12:) (MP10)
* ELOAD39 ....158....... store for calculated mean loads of effluent (Type 39:) (MP10)
* ELOAD60 ....158....... store for calculated mean loads of effluent (Type 60:) (MP10)
* ELOAD61 ....158....... store for calculated mean loads of effluent (Type 61:) (MP10)
* ELOAD62 ....158....... store for calculated mean loads of effluent (Type 62:) (MP10)
* XA .........158....... store for estimates of mean quality for determinands (MP10)
* WEQ .................. worst possible effluent quality (when setting quality needed to meet river targets) (3,MP10) 
* BEQ .................. best possible effluent quality (when setting quality needed to meet river targets) (3,MP10) 
* Detype ............... code numbers for determinands.  For example, BOD is 101 (MP10)      
* confail .............. calculated confidence of failure for determinands (MP10)
* propeff2 ............. proportions of loads as effluent for determinands (MP10) 
* propeff3 ............. proportions of loads as effluent for determinands (from Feature Type 03) (MP10) 
* propeff5 ............. proportions of loads as effluent for determinands (from Feature Type 05) (MP10) 
* propeff12 ............ proportions of loads as effluent for determinands (from Feature Type 12) (MP10) 
* propeff39 ............ proportions of loads as effluent for determinands (from Feature Type 39) (MP10)
* propeff60 ............ proportions of loads as effluent for determinands (from Feature Type 60) (MP10)
* propeff61 ............ proportions of loads as effluent for determinands (from Feature Type 61) (MP10)
* propeff62 ............ proportions of loads as effluent for determinands (from Feature Type 62) (MP10)
* prop25 ............... proportions of loads from Feature Type 25 (MP10)
* prop27 ............... proportions of loads from Feature Type 27 (MP10)
* prop13 ............... proportions of loads from Feature Type 13 (MP10)
* prop15 ............... proportions of loads from Feature Type 15 (MP10)                         
* prop29 ............... proportions of loads from Feature Type 29 (MP10)
* prop31 ............... proportions of loads from Feature Type 31 (MP10)
* prop33 ............... proportions of loads from Feature Type 33 (MP10)
* prop35 ............... proportions of loads from Feature Type 35 (MP10)
* prop37 ............... proportions of loads from Feature Type 37 (MP10)
* prop40 ............... proportions of loads from Feature Type 40 (MP10)
* prop42 ............... proportions of loads from Feature Type 42 (MP10)
* prop46 ............... proportions of loads from Feature Type 46 (MP10)
* prop48 ............... proportions of loads from Feature Type 48 (MP10)
* prop50 ............... proportions of loads from Feature Type 50 (MP10)
* prop52 ............... proportions of loads from Feature Type 52 (MP10)
* prop54 ............... proportions of loads from Feature Type 54 (MP10)
* prop56 ............... proportions of loads from Feature Type 56 (MP10)
* prop58 ............... proportions of loads from Feature Type 58 (MP10)
* prop10 ............... proportions of loads from Feature Type 10 (MP10)
* prop21 ............... proportions of loads added from flow gap filling (MP10)
* prop22 ............... proportions of loads from quality gap filling (MP10)
* propall .............. sum of the proportions of loads from Feature (MP10)
* propRT ............... sum of net loads from Reach-type diffuse sources (MP10)
* propNPU .............. proportions of loads added by natural purification (mp10)
* propNPD .............. proportions of loads removed by natural purification (mp10)
* propfra .............. proportions of loads removed by gap filling for flows (mp10)
* propqra ...............proportions of loads removed by gap filling of river quality (mp10)
* propfrl .............. proportions of loads introduced by gap filling for river flows (mp10)
* propqrl .............. proportions of loads introduced by gap filling for river quality (mp10)
* propabs .............. proportions of loads from abstractions (mp10)
* prolosses ............ proportions of loads introduced by total losses of load (mp10)
* proadds .............. proportions of loads introduced by total additions of load (mp10)
* X95 .................. calculated values of 95-percentiles at monitoring point (MP10) 
* X90 .................. calculated values of 90-percentiles at monitoring point (MP10)        
* X99 .................. calculated values of 99-percentiles at monitoring point (MP10)  
* Xp95 ................. calculated values of 95-percentiles at plotting point (MP10) 
* Xp90 ................. calculated values of 90-percentiles at plotting point (MP10)        
* Xp99 ................. calculated values of 99-percentiles at plotting point (MP10) 
* XpA .................. calculated values of 99-percentiles at plotting point (MP10) 
* XLOAD ................ calculated loads for determinands as means and percentiles
* XEFF ................. calculated 95-percentile of effluent quality (MP10) 
* BSPLIT ............... split of flow for bifurcations (NB)
* DSTART ............... random number starters for river quality (MP10)
* PSTART ............... random number starters for discharge quality (MP10)
* RFCL ................. default correlation coeffiecients for river quality on river flow (MP10)
* EFCL ................. default correlation coeffiecients for discharge quality on discharge flow (MP10)
* PRAN ................. default random deviates for discharge quality (MP10,MS)
* CRAN ................. default random deviates for river quality (MP10,MS)
* ERAN ................. default random deviates for discharge flow (MS)
* FRAN ................. default random deviates for river flow (MS)
* TRAN ................. default random deviates for temperature (MS)
* ReachCode............. store of the code numbers for reaches (MR)
* REACHCODE2 ........... store of the code numbers for reaches (MR) 
* RQO .................. river quality targets (MP10)
* ABLOAD ............... loads removed by abstractions (MP10,13)
* FTMS ................. shots for river flow (MS)
* temkay ............... temporary storage of rate constants for reaches (MP10)
* MRQO ................. codes for the summary statistics used for the river quality standards (MP10)     
* SQTART ............... special random number starter for river quality
* CSTART ............... random number starter for temperature
* USTART ............... random number starter for suspended solids
* diffuse load ......... store for calculated loads of diffuse pollution (MP10)
* TGLODE2 ....158....... running total of net loads down the model (MP10)
* TGLODE1 ....158....... total loads (MP10,13)
* TRLODE1 ....158....... total load introduced upstream boundaries and tributaries (MP10,13)
* TRLODE2 ....158....... net load introduced upstream boundaries and tributaries (MP10,13)
* TELODE1 ....158....... accumulated total loads of effluent for the current model run (MP10) (Feature Types 3 12 5 39 60 61 62)
* TELODE2 ....158....... accumulated net total loads of effluent for the current model run allowing for natural purification (MP10)
* TELODE3 ....158....... values of net total TELODE2 stored for later mixing of reaches (MP10)
* TELODE4 ....158....... values of net total TELODE1 stored for later mixing of reaches (MP10)
* loadmon .............. summaries of contribution to annual load for different sectors (n2prop,MP10,2,13)
* lconmon .............. summaries of contribution to annual mean concentration for different sectors (n2prop,MP10,2,13)
* -----------------------------------------------------------------------------------------------------------------------
* TNLOADUP2 ............ total load introduced by natural purification (MP10)
* TNLOADDN2 ............ running total of load removed by natural purification (MP10)
* TILOADDN2 ............ net total load removed by gap filling for river flows (MP10)
* TALOADDN2 ............ net total load removed by gap filling on river quality (MP10)
* TBLODE2 .............. net total load removed by abstractions (MP10,13) for later reaches (MR,MP10,13)
* TDDLOAD2 ....158...... net total load introduced by clean diffuse sources specified for Reaches (MP10)
* TILOADUP2 ...158...... net total load introduced by gap filling for river flows (MP10)
* TRLODE1......158...... values of total loads from headwaters, streams etc (MR,MP10)
* TRLODE2......158...... values of net total loads from headwaters, streams etc (MR,MP10)
* T13LOAD1 ....158...... total load from diffuse features (river flow type) (MP10)
* T15LOAD1 ....158...... total load from diffuse features (discharge flow type) (MP10)
* T13LOAD2 ....158...... net total load from diffuse features (river flow type) (MP10)
* T15LOAD2 ....158...... net total from diffuse features (discharge flow type) (MP10)
* T--LOAD2 ....158...... net total load from diffuse features (discharge flow type) (MP10)
* T13LOAD3 ....158...... values of net loads T13LOAD2 stored for later mixing of reaches (MR,MP10)
* T15LOAD3.....158...... values of net loads T15LOAD2 stored for later mixing of reaches (MR,MP10)
* TGLODE3 .....158...... values of net loads TGLODE2 stored for later mixing of reaches (MR,MP10)
* TRLODE3 .....158...... values of net loads TRLODE2 stored for later mixing of reaches (MR,MP10)
* TGLODE4 .....158...... values of total loads TGLODE1 stored for later mixing reaches (MR,MP10)
* TRLODE4 .....158...... values of total loads TRLODE1 stored for later mixing of reaches (MR,MP10)
* TILOADUP3 ...158...... values of net loads TILOADUP2 stored for later mixing of reaches (MR,MP10,13)
* TALOADUP3 ...158...... values of net loads TALOADUP2 stored for later mixing of reaches (MR,MP10,13)
* TILOADUP3 ...158...... values of net loads TILOADUP2 stored for later mixing of reaches (MR,MP10,13)
* TNLOADUP3 ............ values of net loads TNLOADUP2 stored for later mixing of reaches (MR,MP10,13)
* TNLOADDN3 ............ values of net loads TNLOADDN2 stored for later mixing of reaches (MR,MP10,13)
* TALOADDN3 ............ values of net loads TALOADDN2 stored for later mixing of reaches (MR,MP10,13)
* TILOADDN3 ............ values of net loads TILOADDN2 stored for later mixing of reaches (MR,MP10,13)
* TBLODE3 .............. values of net loads TBLODE2 stored for later mixing of reaches (MR,MP10,13)
* kprune det ........... identity of determinand for load pruning
* prune load ....158.... factor to scale back loads after abstractions and other losses (13)
* Check total .......... array used to check totals of loads (MP10)
* xupload .............. the increases in mean load from gap filling of river quality (MP10,13)
* xdownload ............ the decreases in mean load from gap filling of river quality (MP10,13)
* xupflow .............. the additions of flow from gap filling of flow (MP10,13)
* xdownflow ............ the removals of flow from gap filling of flow (MP10,13)
* Check sub total....... sub-totals of load (MP10)
* k90 .................. index for obtaining 90-percentile from an ordered list of NS items
* k95 .................. index for obtaining 95-percentile from an ordered list of NS items
* k99 .................. index for obtaining 99-percentile from an ordered list of NS items
* k10 .................. index for obtaining 10-percentile from an ordered list of NS items
* k05 .................. index for obtaining 05-percentile from an ordered list of NS items
* k01 .................. index for obtaining 01-percentile from an ordered list of NS items
* k98 .................. shot indentifying the 98-percentile in a ranked list of all NS shots
* k995 ................. shot indentifying the 99.5-percentile in a ranked list of all NS shots
* k999 ................. shot indentifying the 99.9-percentile in a ranked list of all NS shots
* BM ................... scaling factors for correcting Monte Carlo sampling bias in the mean (4)
* BS ................... scaling factors for correcting Monte Carlo sampling bias in the standard deviation or the 95-percentile lows flow (4)
* spcorr ............... take up the value of a correlation coefficient
* kf99 ................. index for obtaining 99-percentile from an ordered list of NS items 
* kf95 ................. index for obtaining 95-percentile from an ordered list of NS items
* kf90 ................. index for obtaining 90-percentile from an ordered list of NS items
* kf80 ................. index for obtaining 80-percentile from an ordered list of NS items 
* kf50 ................. index for obtaining 50-percentile from an ordered list of NS items
* kf20 ................. index for obtaining 20-percentile from an ordered list of NS items
* kf10 ................. index for obtaining 10-percentile from an ordered list of NS items
* kf05 ................. index for obtaining 05-percentile from an ordered list of NS items
* kf01 ................. index for obtaining 01-percentile from an ordered list of NS items
* ==============================================================================
* Items for choosing to run parts of the models set up in a DAT file ===========
* ==============================================================================
* Included ............. included reaches (9999)
* IncludedList ......... included reaches (9999)  
* NFIN ................. array used to check the structure of reaches in the model (9999,2)
* NumIncluded .......... number of reaches included 
* ===========================================================================================
* in class.............. calculated confidence in a class (NC,MP10)
* conf in class ........ calculated confidence in a class (NC)
* conf of class ........ calculated confidence in a class or better (NC)
* in or better ......... calculated confidence in a class or better (NC)
* Face Value ........... face value class - the one suggested by the face values 
*                        for determinands
* Face Value dets ...... face value class for each determinand (MP10)          
* QD90 temperature ..... calculated 90-percentile of river temperature
* exclude BOD .......... switch to exclude BOD from the classification
* exclude NO3 .......... switch to exclude Nitrate from the classification
* exclude PO4 .......... switch to exclude Phosphate from the classification
* exclude DOX .......... switch to exclude Dissolved Oxygen from the classification
* exclude AMM .......... switch to exclude Ammonia from the classification
* -------------------------------------------------------------------------------------
* CL ................... calculated concentrations for graph plotting etc (12,MP10)
* -------------------------------------------------------------------------------------
*                        CL(1... is the mean concentration
*                        CL(2... is the lower confidence limit on the mean
*                        CL(3... is the upper confidence limit on the mean
*                        CL(4... is the calculated value of the 90-percentile
*                        CL(5... is the lower confidence limit on the 90-percentile
*                        CL(6... is the upper confidence limit on the 90-percentile
*                        CL(7... is the calculated value of the 95-percentile
*                        CL(8... is the lower confidence limit on the 95-percentile
*                        CL(9... is the upper confidence limit on the 95-percentile
*                        CL(10.. is the calculated value of the 99-percentile
*                        CL(11.. is the lower confidence limit on the 95-percentile
*                        CL(12.. is the upper confidence limit on the 95-percentile
* -------------------------------------------------------------------------------------
* CD ................... calculated loads for plotting etc(12,MP10)
* -------------------------------------------------------------------------------------
*                        CD(1... is the mean load
*                        CD(2... is the lower confidence limit on the mean load
*                        CD(3... is the upper confidence limit on the mean load
*                        CD(4... is the calculated value of the 90-percentile load
*                        CD(5... is the lower confidence limit on the 90-percentile
*                        CD(6... is the upper confidence limit on the 90-percentile
*                        CD(7... is the calculated value of the 95-percentile load
*                        CD(8... is the lower confidence limit on the 95-percentile
*                        CD(9... is the upper confidence limit on the 95-percentile
*                        CD(10.. is the calculated value of the 99-percentile load
*                        CD(11.. is the lower confidence limit on the 99-percentile
*                        CD(12.. is the upper confidence limit on the 99-percentile
* -------------------------------------------------------------------------------------------
* COB .................. observed concentrations for plotting etc ... plus flows (4,MP10+1)
* -------------------------------------------------------------------------------------------
*                        COB(1... is the mean
*                        COB(2... is the calculated value of the 90-percentile
*                        COB(3... is the calculated value of the 95-percentile
*                        COB(4... is the calculated value of the 99-percentile
* -------------------------------------------------------------------------------------------
* COD .................. observed loads for plotting ... plus flows (4,MP10+1)
* -------------------------------------------------------------------------------------------
*                        COD(1... is the mean
*                        COD(2... is the calculated value of the 90-percentile
*                        COD(3... is the calculated value of the 95-percentile
*                        COD(4... is the calculated value of the 99-percentile
* -------------------------------------------------------------------------------------------
* elapsed days ......... accumulative number of days in the year
* days in months ....... number of days in the months (12)
* fraction of year ..... fraction of a year formed by a month (12)
* ECM2 ................. calculated mean discharge quality
* toveride ............. switch to look for strange data and correct them
* chekk infeezible ..... flag set when massive concentrations are encountered
* kswitch .............. records whether file SWITCHES.GPD exists
* bagset ............... cut-off for including a discharge in back-tracking
* baggest .............. % impact of discharge used to decide to back-track
* TWLENGTH ............. the length of river in a water body
* ===========================================================================================
* xfact ................ multiplier used to adjust catchment loads for bifurcations
* bifurcr .............. indicator that the reach is a bifurcation
* flowhead ............. flow at the head of a bifurcation
* itypbir .............. type of bifurcation (20, 21, 22, etc)
* KSIM ................. counter for iterations of Gap Filling for water quality 
* MAXNITZ .............. maximum number of iterations in Gap Filling
* JUgap ................ used to identify a Feature used for Gap Filling
* ==============================================================================