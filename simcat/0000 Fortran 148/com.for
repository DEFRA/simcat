*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     common blocks   423 lines
*     --------------------------------------------------------------------------
      parameter (MR = 4999  ) ! number of reaches ------------------------------
      parameter (NU = 19999 ) ! number of features -----------------------------
      parameter (NF = 10999 ) ! number of sets of flow data for rivers ---------
      parameter (NV = 19999 ) ! number of sets of quality data for rivers ------
      parameter (NE = 10999 ) ! number of sets of data for effluents -----------
      parameter (MS = 2000  ) ! number of Monte-Carlo shots --------------------
      parameter (KR = 30    ) ! number of reach data-sets kept in store --------
      parameter (NM = 5     ) ! number of summary statistics for distributions -
      parameter (MP10 = 10  ) ! number of pollutants ---------------------------
      parameter (NQ = 50    ) ! number of data sets on river quality targets ---
      parameter (MM = 1999  ) ! number of quality monitoring stations ----------
      parameter (NUED = 1800) ! number of effluent discharges ------------------
      parameter (NUW = 100  ) ! number of sub-catchments -----------------------
      parameter (MO = 4     ) ! statistics entered as flow and quality data ----
      parameter (M7 = 9000  ) ! data-sets for non-parametric distributions -----
      parameter (M8 = 9000  ) ! number of data-sets for monthly data -----------
      parameter (M9 = 9000  ) ! number of data-sets for monthly structure data -
      parameter (M10 = 9000 ) ! number of data-sets for temperature data -------
      parameter (NB = 400   ) ! maximum number of bifurcations -----------------
      parameter (MT = 19999 ) ! size of utility array ( Maximum of NE, etc) ----
      parameter (MG = 1999  ) ! maximum number of flow gauges ------------------
      parameter (MPRF = 2000) ! maximum data for non-parametric distribution ---
      parameter (NC = 5     ) ! maximum classes in the classification system ---
      parameter (NPROP = 22 ) ! number of types of feature tracked for inputs --

      character *44  UNAME,RNAME*16,DNAME*11,DNA*4,UNITS*4,FUNIT*4,
     &               LUNITS*4
      character *43  VNAME
      character *40  GISCODE,GISCODE last,unamex
      character *20  rnamex
	character *09  names of months

      character *136 FLNAME ! file names for non-parametric data -------------
	character *064 FLNAMEsmall
      character *136 FLMONTH ! file names for monthly data --------------------
      character *064 FLMONTHsmall
      character *136 FLSTRUCT ! file names for monthly structure data ---------- 
      character *064 FLSTRUCTsmall
      character *136 FLTEMP ! files names for temperature etc ----------------
      character *064 FLTEMPsmall
      
      character *136 FNAME,DatName,F1,
     &               Globaldataname,Switchesname
      character *200 line of data
	character *167 FNAME2
      character *130 datfilename
      character *100 folder
      character *070 TITLE
      character *037 nameprop
      character *010 valchars10,valchars11,valchars12
      character *007 flowchars,Sevenchars,B44
      character *002 bday,bmon,bh,bmin

      integer BIDENT,QTYPE,PDEC,PDEF,PDRC,PDRF,ENUM,PNUM,QNUM,FSTART,
     &BNUM,PDBC,RFDIFF,RQDIFF,QNAT,DSTART,ESTART,PSTART,CSTART,
     &SEASD,struckd,TEMPD,USTART,DETYPE,REACHCODE2,start reach,
     &feeture,feetcount,NOTARG,model number in batch,ifbatch,ReachCode,
     &output mode,effcsv,background class

	integer reachstore, calspecial
	integer shot day, BIGLAPSE, suppress, 
     &suppress1,suppress3,suppress4,suppress5,
     &suppress6,suppress7,suppress8,suppress9,suppress10,
     &suppress11,suppress12,suppress13,suppress14,
     &suppress16,suppress17,suppress18,suppress19,suppress20,
     &suppress21,suppress00,suppress9a,suppress9b,suppress9c,suppress9d
      integer diffuse type, discharge type
	integer virtualreach 
      integer IYEAR,JMONTH,IDAY,IHR,IMIN,ISEC,IHUN
	integer Face Value, Face Value dets, exclude BOD,exclude NO3, 
     &        exclude PO4, exclude DOX, exclude AMM 
	integer elapsed days in year, days in months,seas0,struct0
	integer master set used
      integer tony warn overide,check for infeasible quality
      integer diffuse heading
      integer EQS reach,SKIPPEFF,classobj

	real Length of main river
	real masterBASE, masterWEQ, masterBEQ, masterGEQ
	real lmon,in class,LMS,L,llmon,llmon2
	double precision CMX

      common/AR/ dcalflow,dcalquality,DINT,Length of main river,DISTP,
     &DISTR,TDEG,TSDEV,TCORF,TCORF2,NOTARG,kdecision(MP10),
     &iforce gap fill,calspecial,no gap filling 78,krite1,
     &wloadkeep,Grand river length,
     &Total RQS length 1,Total length 2,Total river length,
     &Total length 00,Total length 50,Total length 95,
     &Total length dets 00 (MP10),Total length dets 50 (MP10), 
     &Total length dets 95 (MP10),effcsv,output mode,
     &model number in batch,ifbatch, 
     &masterdata,master output mode,maxistor,
     &masterNS,noGIS,nobigout,masterIPUR,jcycle,masterIDIFF,
     &masterIKINT,masternocon,master no tables,masterIPRINT,
     &masterQTYPE(MP10),masterBASE(MP10),masterWEQ(3,MP10),
     &masterBEQ(3,MP10),masterGEQ(3,MP10),masterMRQS(MP10),
     &masterdetype(MP10)

	common /GR1/ virtualreach

      common /AI/
     &ICAL,IDIFF,IEND,ISTART,IF,IE,IPRINT,IPUR,IQ,IREACH,KSTART,
     &ICAL13,feeture,feetcount,NONPD,negreach,negorder,classobj,
     &JS90,JS95,JS98,JS99,JS995,JS999,ndshot, ! 33333333333333333333333333333333
     &No tables,JSKIP,LMONP,KINT,KFEAT,KFEET,KPTARG,jhead,start reach,
     &diffuse type,ifdiffuse,discharge type,diffuse heading,
     &kerror,KEPOL15,KEPOL42,
     &KRFPOL13,KRQPOL13,KRFPOL25,KRQPOL25,KRFPOL27,KRQPOL27,
     &KRFPOL29,KRQPOL29,KRFPOL31,KRQPOL31,KRFPOL33,KRQPOL33,
     &KRFPOL35,KRQPOL35,KRFPOL37,KRQPOL37,KRFPOL39,KRQPOL39,
     &KRFPOL40,KRQPOL40,KRFPOL46,KRQPOL46,KRFPOL48,KRQPOL48,
     &KEPOL15x,KEPOL42x,
     &KRFPOL13x,KRQPOL13x,KRFPOL25x,KRQPOL25x,KRFPOL27x,KRQPOL27x,
     &KRFPOL29x,KRQPOL29x,KRFPOL31x,KRQPOL31x,KRFPOL33x,KRQPOL33x,
     &KRFPOL35x,KRQPOL35x,KRFPOL37x,KRQPOL37x,KRFPOL39x,KRQPOL39x,
     &KRFPOL40x,KRQPOL40x,KRFPOL46x,KRQPOL46x,KRFPOL48x,KRQPOL48x,
     &IMM,MPAR,MONF,MONQ,MU,NDET,NEXC(MP10),NEXF,NGAUGE,NDETA,
     &NDETlast,NI,nocon,NP,NREACH,NS,SEASD,struckd,IMONTH,IQMON,
     &shot day,kmunth,TEMPD,ndetfirst,NDETBOD,kount22,kountd1,kountd2

      common /AT/ bday,bmon,bh,bmin,
     &F1,folder,FUNIT,FNAME,FNAME2,DatName,
     &Globaldataname,Datfilename,Title,Line of data,
     &Sevenchars(MP10),Switchesname,
     &flowchars(2),valchars10,valchars11,valchars12,B44(MP10)

	common /ATT/ISCREEN,LAPSE,IHR,IMIN,ISEC,IHUN,IYEAR,JMONTH,IDAY,
     &BIGLAPSE
     
      common /BTT/NMORQ,suppress (9,MP10), 
     &suppress1,suppress3,suppress4,suppress5,
     &suppress6,suppress7,suppress8,suppress9, suppress10,
     &suppress11,suppress12,suppress13,suppress14,
     &suppress16,suppress17,suppress18,suppress19,suppress20,
     &suppress21,
     &suppress00,suppress9a,suppress9b,suppress9c,suppress9d

      common /B/ ADIST(MR),BIDENT(NB),
     &C(MP10,5),CMS(MP10,MS),ucms(MP10,MS),DIST(NU),
     &DNA(MP10),DNAME(MP10)
      common /BDP/ CMX(MP10,MS)
      common /BL/ LMS(nprop,MP10,MS),ulms(nprop,MP10,MS)

      common /BM/ cmon(MP10,2,13),lmon(MP10,2,13),fmon(2,13),tmon(2,13),
     &smon(2,13),P1mon(MP10,2,13),P2mon(MP10,2,13)
      common /B4/ names of months(12)

      common /B1/EFD(KR,MS),
     &F(NF,MO),FD(KR,MS),FE(NE,MO),FEND(MR),FLOW(NM),EFLOW(NM)

	common /B2/EFMS(MS),ufms(MS),YY(MS),dfms(MS),
     &FMS(MS),FMX(MS),GEQ(3,MP10),IFRQS(NU),
     &IPLAN(MR,3),IRHOLD(MR),JF(NU),JFCAL(NU),SKIPPEFF(NU),
     &JFUSER(NU),JQ(NU),JQCAL(NU),JQUSER(NU),JREACH(NU),JSTOR(KR),
     &JT(NU),KFCAL(NU),KGAUGES(MG),KQCAL(NU),LSAMPOINTS(MM),MK(MT),
     &PDEC(NE,MP10),PDEF(NE),PDRC(NV,MP10),PDRF(NF),
     &pollution data(NE,MP10,MO),
     &GISCODE(NU),reachstore(MR),GISCODE last

	common /CC/ flowstats(9),propstats(9), step in discharge flow
	common /b3/ NextReach(MR), kount reach (MR)

      common /B9/ ENUM(NE),
     &PNUM(NE,MP10),QBASE(MP10),QD(KR,MP10,MS),QDN(MP10,MR),
     &QDIFFGAP(MP10),QE(MP10,MR),quolity data(NV,MP10,MO),QNAT(MP10),
     &QNUM(NV,MP10),Qstations(MM,MP10,3),QTYPE(MP10),QUALN(MP10),
     &QZEROGAP(MP10),Dcoeff(MP10),Dcorr(MP10), 
     &RATE(MP10),RFDIFF(MR),RCHA(MR),RCHB(MR),Rchkay(MP10,MR),
     &RLENGTH(MR),RNAME(MR),RQDIFF(MR),RQS(NQ,MP10),MRQS(MP10),
     &SFL(MG,2),UNITS(MP10),WEQ(3,MP10),LUNITS(MP10),
     &BEQ(3,MP10),Detype(Mp10),confail(mp10),
     &XA(MP10),XpA(MP10),X(MP10),X95(MP10),X90(MP10),X99(MP10),
     &Xp95(MP10),Xp90(MP10),Xp99(MP10),
     &XLOAD (MP10,5,13),XLOAD1 (MP10,5,13),XLOAD2 (MP10,5,13),
     &XEFF(MP10),XECM(MP10),
     &BSPLIT(NB),SPLIT,DSTART(MP10),PSTART(MP10),
     &RFCL(MP10),EFCL(MP10),
     &CRAN(MP10,MS),PRAN(MP10,MS),cut off zero flow,
     &cut off zero quality(MP10),
     &ERAN(MS),FRAN(MS),TRAN(MS),SRAN(MS),
     &ERANR(MS),ERANR2(MS),ERANR3(MS),
     &ReachCode(MR),RQO(MP10),FTMS(MS),MRQO(MP10),REACHCODE2(MR),
     &USTART,CSTART

      common /B9/
     &propeff2(mp10),propeff3(mp10),
     &propeff5(mp10),propeff12(mp10),propeff39(mp10),
     &prop25(mp10),prop27(mp10),prop13(mp10),prop15(mp10),
     &prop29(mp10),prop31(mp10),
     &prop33(mp10),prop35(mp10),prop46(mp10),prop48(mp10),
     &prop37(mp10),prop40(mp10),prop42(mp10),
     &prop10(mp10),propall(mp10),propRT(mp10),
     &propNPU(mp10),propNPD(mp10),
     &propfra(mp10),propqra(mp10),propfrl(mp10),propqrl(mp10),
     &propabs(mp10),prolosses(mp10),proadds(mp10),
     &diffuse load(MP10,13),nameprop(nprop)

*     total loads                                               ... TGLODE1
*     total load introduced upstream boundaries and tributaries ... TRLODE1
*     total load from discharges (3 12 5 and 39)                ... TELODE1
*     total load removed by abstractions                        ... TBLOAD1
*     total load introduced by natural purification             ... TNLOADUP1
*     total load removed by natural purification                ... TNLOADDN1
*     total load introduced by clean diffuse sources            ... TDDLOAD1
*     net loads                                                 ... TGLODE2
*     net load from discharges (3 12 5 and 39)                  ... TELODE2
*     net load introduced upstream boundaries and tributaries   ... TRLODE2
*     net load introduced by natural purification               ... TNLOADUP2
*     net load removed by natural purification                  ... TNLOADDN2
*     net load introduced by clean diffuse sources              ... TDDLOAD2
*     net load introduced by gap filling of river flows         ... TILOADUP2
*     net load removed by gap filling for river flows           ... TILOADDN2
*     net load added by gap filling for river quality           ... TALOADUP2
*     net load removed by gap filling for river quality         ... TALOADDN2
*     net load from diffuse features (river flow type)          ... T13LOAD2
*     net load from diffuse features (discharge flow type)      ... T15LOAD2
*     net load from diffuse features (river flow type)          ... TxxLOAD2
*     net load removed by abstractions                          ... TBLOAD2

      common /L9/
     &ELOAD    (MP10,13), ELOADS(MP10),
     &ELOAD03  (MP10,13), 
     &ELOAD05  (MP10,13), ELOAD12  (MP10,13),
     &ABLOAD   (MP10,13), ELOAD39  (MP10,13)

      common /l3a1/ 
     &TGLODE1   (MP10,13), TELODE1   (MP10,13),
     &TRLODE1   (MP10,13), TDDLOAD1  (MP10,13),
     &TNLOADUP1 (MP10,13), TNLOADDN1 (MP10,13),
     &TALOADUP1 (MP10,13), TALOADDN1 (MP10,13),
     &TILOADUP1 (MP10,13), TILOADDN1 (MP10,13),
     &TBLOAD1   (MP10,13),  
     &T13LOAD1  (MP10,13), T15LOAD1  (MP10,13),
     &T25LOAD1  (MP10,13), T27LOAD1  (MP10,13),
     &T29LOAD1  (MP10,13), T31LOAD1  (MP10,13),
     &T33LOAD1  (MP10,13), T35LOAD1  (MP10,13), 
     &T37LOAD1  (MP10,13), T40LOAD1  (MP10,13),
     &T42LOAD1  (MP10,13), T46LOAD1  (MP10,13), 
     &T48LOAD1  (MP10,13)
      common /l3a2/ 
     &TGLODE2   (MP10,13), TELODE2   (MP10,13),
     &TRLODE2   (MP10,13), TDDLOAD2  (MP10,13),
     &TNLOADUP2 (MP10,13), TNLOADDN2 (MP10,13),
     &TALOADUP2 (MP10,13), TALOADDN2 (MP10,13),
     &TILOADUP2 (MP10,13), TILOADDN2 (MP10,13),
     &TBLOAD2   (MP10,13), TLOSSES   (MP10,13), TLOSSES1(MP10,13),   
     &T13LOAD2  (MP10,13), T15LOAD2  (MP10,13),
     &T25LOAD2  (MP10,13), T27LOAD2  (MP10,13),
     &T29LOAD2  (MP10,13), T31LOAD2  (MP10,13),
     &T33LOAD2  (MP10,13), T35LOAD2  (MP10,13), 
     &T37LOAD2  (MP10,13), T40LOAD2  (MP10,13),
     &T42LOAD2  (MP10,13), T46LOAD2  (MP10,13), 
     &T48LOAD2  (MP10,13)
      common /l3a3/ 
     &TGLODE3   (MR,MP10,13), TELOAD3   (MR,MP10,13),
     &TRLOAD3   (MR,MP10,13), TDDLOAD3  (MR,MP10,13),
     &TNLOADUP3 (MR,MP10,13), TNLOADDN3 (MR,MP10,13),
     &TALOADUP3 (MR,MP10,13), TALOADDN3 (MR,MP10,13),
     &TILOADUP3 (MR,MP10,13), TILOADDN3 (MR,MP10,13),
     &TBLOAD3   (MR,MP10,13), 
     &T13LOAD3  (MR,MP10,13), T15LOAD3  (MR,MP10,13),
     &T25LOAD3  (MR,MP10,13), T27LOAD3  (MR,MP10,13),
     &T29LOAD3  (MR,MP10,13), T31LOAD3  (MR,MP10,13),
     &T33LOAD3  (MR,MP10,13), T35LOAD3  (MR,MP10,13),
     &T37LOAD3  (MR,MP10,13), T40LOAD3  (MR,MP10,13),
     &T42LOAD3  (MR,MP10,13), T46LOAD3  (MR,MP10,13), 
     &T48LOAD3  (MR,MP10,13)
*     ==========================================================================
      common /l3a4/ 
     &TGLODE4   (MR,MP10,13), TELOAD4   (MR,MP10,13),
     &TRLOAD4   (MR,MP10,13), TDDLOAD4  (MR,MP10,13),
     &TNLOADUP4 (MR,MP10,13), TNLOADDN4 (MR,MP10,13),
     &TALOADUP4 (MR,MP10,13), TALOADDN4 (MR,MP10,13),
     &TILOADUP4 (MR,MP10,13), TILOADDN4 (MR,MP10,13),
     &TBLOAD4   (MR,MP10,13),
     &T13LOAD4  (MR,MP10,13), T15LOAD4  (MR,MP10,13),
     &T25LOAD4  (MR,MP10,13), T27LOAD4  (MR,MP10,13),
     &T29LOAD4  (MR,MP10,13), T31LOAD4  (MR,MP10,13),
     &T33LOAD4  (MR,MP10,13), T35LOAD4  (MR,MP10,13),
     &T37LOAD4  (MR,MP10,13), T40LOAD4  (MR,MP10,13),
     &T42LOAD4  (MR,MP10,13), T46LOAD4  (MR,MP10,13),
     &T48LOAD4  (MR,MP10,13)

      common /l3g/ i13, n13, NSM(MP10,13)
     
      common /l2e/ 
     &T03LOAD1 (MP10,13),    T05LOAD1 (MP10,13),
     &T03LOAD2 (MP10,13),    T05LOAD2 (MP10,13),
     &T12LOAD1 (MP10,13),    T12LOAD2 (MP10,13),
     &T39LOAD1 (MP10,13),    T39LOAD2 (MP10,13),
     &T03LOAD4 (MR,MP10,13), T05LOAD4 (MR,MP10,13),
     &T03LOAD3 (MR,MP10,13), T05LOAD3 (MR,MP10,13),
     &T12LOAD3 (MR,MP10,13), T12LOAD4 (MR,MP10,13),
     &T39LOAD3 (MR,MP10,13), T39LOAD4 (MR,MP10,13)
      
      common /l4a/ TELOADAV (NUED,MP10),TELOADAVrch (NUED,KR,MP10)
      
*     33333333333333333333333333333333333333333333333333333333333333333333333333
      common /l4b/ TELOADshots (NUED,MP10,MS) ! 33333333333333333333333333333333
      common /l4c/ TELOADrchshots (NUED,KR,1,MS) ! 33333333333333333333333333333
*     33333333333333333333333333333333333333333333333333333333333333333333333333

*     55555555555555555555555555555555555555555555555555555555555555555555555555
      common /l4e/ TDLOADshots (NUW,MP10,MS,nprop) ! 555555555555555555555555555
      common /l4f/ TDLOADrchshots (NUW,KR,1,MS,nprop) ! 555555555555555555555555
*     55555555555555555555555555555555555555555555555555555555555555555555555555

      common /l4y/ mark works, kount works, need works, kill works,
     &identify works (NUED)

*     monthly loads from upstream sub-catchments -------------------------------
      common /l5a/ TWLOADS (NUW,MP10,13), TWlength(NUW)
*     breakdown of monthly loads from upstream sub-catchments ------------------
      common /l5b/ TWLOADSapp (NUW,MP10,13,NPROP)
*     monthly loads from upstream sub-catchments at the ends of reaches --------
      common /l5c/ TWloadsrch (NUW,KR,MP10)
*     breakdown of monthly loads from sub-catchments at ends of reaches --------
      common /l5d/ TWloadsrchapp (NUW,KR,MP10,NPROP)
      common /l5e/ kount bodies, identify bodies (NUW), 
     &identify reaches (3,NUW)

      common /l6f/ prune load (13), kprune det,
     &Check total(MP10),Check sub total (MP10),
     &xupload(MP10,13),xdownload(MP10,13),xupflow,xdownflow

*     data generation ----------------------------------------------------------
      common /M/ k90,k95,k98,k99,k995,k999,k10,k05,k01,BM(4),BS(4), 
     &spcorrRFaf,spcorrRFRC,spcorrfa,kstat2,
     &kf99,kf95,kf90,kf80,kf50,kf20,kf10,kf05,kf01  

*     mass balance -------------------------------------------------------------
      common /W/
     &JP,JP1,EFM,EFS,ECM,ECS,RF3,RC3,EF3,EC3,CO1,CO2,CO3,CO4,CO5,
     &CO6,ECX,TECM,TECS,TECX,TRCX,FSTART,ESTART,GEFM,GEFS,GECM,GECS,E80,
     &current downstream quality,IQDIST,IFDIST,ECV,TIME,b1,b2,c1,
     &c2,d1,d2,EFshots(MS),ECshots(MS),kstop,EF5

*     gap filling --------------------------------------------------------------
      common /L/IFEAT1,IFEAT2,ICTOP,KSIM,FDUMP(MS),CDUMP(MP10,MS)
      
      common /U/ UNAME(NU),VNAME(NU)

*     non-parametric distributions ---------------------------------------------
      common /npar/ rfnpvl(mprf),rfnpfd(mprf),nprf,IDENP(2,M7,MP10+1)

*     monthly data -------------------------------------------------------------
      common /seas/ seas0(12),seas1(12),seas2(12),seas3(12), 
     &seas4(12),BSM(12),BSS(12),ISEASP(2,M8,MP10+1)

      common /struct/ itdist,
     &struct0(12),struct1(12),struct2(12),struct3(12), 
     &struct4(12),istruct(2,M9,MP10+1),struct6(12),idfcheck

      common /tem/ temp1(12),temp2(12),itempp(2,M9,2)

	common /z1/ FLMONTH(4,m8),FLMONTHsmall(4,m8),
     &FLSTRUCT (4,m9),FLSTRUCTsmall (4,m9),
     &FLTEMP(4,m10),FLTEMPsmall (4,m10)
	common /y/ FLname (4,m7),FLNAMEsmall (4,m7),FLsequence(4,m7)
      common /Z/ ihalt diffuse river, ihalt diffuse discharge
	common /ZS/ xshots (MP10, MS)

	common /CL/ nclass, background class,
     &class limmits (NC,MP10), class limmits2 (NC,MP10),
     &total length in classes (NC,MP10),total length over all (NC)

*     items for choosing to run parts of models --------------------------------
	common /CP2008/ Included(9999), IncludedList(9999), NFIN(9999,2),
     &NumIncluded

      common /Class/ in class (NC, MP10), conf in class (NC),
     &conf of class (NC), Face Value, Face Value dets (Mp10),
     &QD90 temperature,exclude BOD,exclude NO3, 
     &exclude PO4, exclude DOX, exclude AMM, 
     &jstructure message, iprime
      common/Class2/ CL(12,MP10),CD(12,MP10),CD1(12,MP10),CD2(12,MP10),
     &COB(4,MP10+1),CP1(12,MP10),CP2(12,MP10),COD(4,MP10)

      common /Year/ elapsed days in year (12), days in months (12),
     &fraction of year (12), Cmonth (12), Cstdev (12)

      common /planning/ addid load (MP10)

      common /overide/ tony warn overide, 
     &check for infeasible quality, bagset, kswitch

*     background quality data for reaches - temperature, suspended solids ------
	common /BQ/ Bmat(MR,3,3),PDBC(MR,3),Bnum(MR,3),BMS(3,MS),BC(3,5),
     &BMS global (3,MS)
*     partitioning -------------------------------------------------------------
	common /par/ cpart1(MP10,5),cpart2(MP10,5),Partition(MP10),
     &PMS1(MS),PMS2(MS)

      common /months/ unamex,rnamex,jxtype,running mean,zero check,
     &exceedences50 (MP10,13),exceedences90 (MP10,13),
     &exceedences95 (MP10,13)

      common /app/ EQD(nprop,KR,MP10,MS),L(nprop,MP10,5),
     &llmon(nprop,MP10,2,13), llmon2(nprop,MP10,2,13),
     &munthly structure, month write 1
      
	common /bug/ master set used
	common /lak/ lake45
	common /str/ number of stds, EQS reach (MR,MP10), 
     &standards for reach (MR,NC)
      common /new/ n146,n147,n148,n149,n150,n151 ! development versions
      common /reg/ Creg(4,MS), COD1(MP10)

