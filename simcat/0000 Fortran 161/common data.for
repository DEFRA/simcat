*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     File: common data.for ... 385 lines 
*     --------------------------------------------------------------------------
*     This file contains blocks of data used throughout the calculations -------
*     It is the "COMMON BLOCK" used by:      include 'COMMON DATA.FOR' ---------
*     The file contains no sub-routines ----------------------------------------
*     --------------------------------------------------------------------------
      parameter (MR = 4999  ) ! maximum number of reaches ----------------------
      parameter (NU = 29999 ) ! maximum number of features ----------------- 161
      parameter (NF = 10999 ) ! max number of sets of flow data for rivers -----
      parameter (NV = 19999 ) ! max number of sets of river quality ------------
      parameter (NE = 10999 ) ! max number of sets of data for effluents -------
      parameter (MS = 5000  ) ! max number of Monte-Carlo shots ----------------
      parameter (KR = 100   ) ! number of reach data-sets kept in store --------
      parameter (NM = 5     ) ! number of summary statistics for distributions -
      parameter (MP10 = 10  ) ! number of pollutants ---------------------------
      parameter (NQ = 50    ) ! number of data sets on river quality targets ---
      parameter (MM = 1999  ) ! number of quality monitoring stations ----------
      parameter (NUED = 3000) ! number of discharges for back-tracking ----- 161
      parameter (NUW =  900 ) ! number of sub-catchments -----------------------
      parameter (MO = 4     ) ! statistics entered as flow and quality data ----
      parameter (M7 = 9000  ) ! sets for non-parametric data -------------------
      parameter (M8 = 9000  ) ! number of data-sets for monthly data -----------
      parameter (M9 = 9000  ) ! data-sets for monthly structure data -----------
      parameter (M10 = 9000 ) ! number of data-sets for temperature data -------
      parameter (NB = 400   ) ! maximum number of bifurcations -----------------
      parameter (MT = 19999 ) ! size of utility array ( Max of NE, etc) --------
      parameter (MG = 1999  ) ! maximum number of flow gauges ------------------
      parameter (MPRF = 2000) ! maximum data for non-parametric distribution --- 
      parameter (NC = 5     ) ! maximum classes in the classification system ---
      parameter (NPROP = 30 ) ! number of types of feature tracked for inputs --
      parameter (n2prop = 33) ! an NPROP that adds totals of diffuse features --
      parameter (NTD = 32   ) ! identify storage that holds total pollution ----
      parameter (NTF = 33   ) ! identify storage that holds diffuse pollution --

      character *44  UNAME,RNAME*16,DNAME*11,DNA*4,UNITS*4,FUNIT*4,
     &               LUNITS*4
      character *43  VNAME
      character *40  GIScode,GIScode last,unamex
      character *20  rnamex

      character *136 FLNAME ! file names for non-parametric data ---------------
      character *064 FLNAMEsmall
      character *136 FLMONTH ! file names for monthly data ---------------------
      character *064 FLMONTHsmall
      character *136 FLSTRUCT ! file names for monthly structure data ---------- 
      character *064 FLSTRUCTsmall
      character *136 FLTEMP ! files names for temperature etc ------------------
      character *064 FLTEMPsmall
      
      character *136 FNAME,DatName,F1,
     &Globaldataname,Switchesname,output_folder
      character *200 line of data
      character *167 FNAME2
      character *130 datfilename
      character *100 folder
      character *070 TITLE
      character *037 nameprop
      character *010 valchars10,valchars11,valchars12,valchars13
      character *007 flowchars,Sevenchars,B44
      character *002 bday,bmon,bh,bmin

      integer BIDENT,QTYPE,PDEC,PDEF,PDRC,PDRF,ENUM,PNUM,QNUM,FSTART,
     &BNUM,PDBC,RFDIFF,RQDIFF,DSTART,ESTART,PSTART,CSTART,
     &SEASD,struckd,TEMPD,USTART,DETYPE,REACHCODE2,start reach,
     &feeture,feetcount,NOTARG,model number in batch,ifbatch,ReachCode,
     &output mode,background class,add conc,add diffconc

      integer QNAT,calspecial,suppress5,suppress8,suppress21 ! ggggggggggggggggg
      integer reachstore,diffreachstop
      integer shot day,BIGLAPSE,suppress, 
     &suppress1,suppress3,suppress4,
     &suppress6,suppress7,suppress9,suppress10,
     &suppress11,suppress12,suppress13,suppress14,suppress15,
     &suppress16,suppress17,suppress18,suppress19,suppress20,
     &suppress22,suppress23,
     &suppress00,suppress9a,suppress9b,suppress9c,suppress9d

      integer diffuse type, discharge type
      integer virtualreach, bifurcr 
      integer IYEAR,JMONTH,IDAY,IHR,IMIN,ISEC,IHUN
      integer Face Value,Face Value dets,exclude BOD,exclude NO3, 
     &        exclude PO4,exclude DOX,exclude AMM 
      integer elapsed days,days in months,seas0,struct0
      integer master set used
      integer toveride,chekk infeezible
      integer diffuse heading
      integer EQS reach,SKIPPEFF,classobj
      
      integer diffheadqual
      integer propsequence
      integer correctcorrel
      integer seqappbodies

      real Length of main river
      real masterBASE, masterWEQ, masterBEQ, masterGEQ
      real lmon,in class,loadmon,lconmon,LMcontrib
      double precision CMX ! ggggggggggggggggggggggggggggggggggggggggggggggggggg

      common/AR/ DINT,Length of main river,DISTP,
     &DISTR,TDEG,TSDEV,TCORF,TCORF2,NOTARG,kdecision(MP10),krite1,
     &Grand river length,
     &Total RQS length 1,Total length 2,Total river length,icp,
     &Total length 00,Total length 50,Total length 95,
     &Total length dets 00 (MP10),Total length dets 50 (MP10), 
     &Total length dets 95 (MP10),ifeffcsv,output mode,
     &model number in batch,ifbatch,
     &masterdata,master output mode,maxistor,
     &masterNS,noGIS,nobigout,masterIPUR,jcycle,
     &masterIKINT,masterIPRINT,
     &masterQTYPE(MP10),masterBASE(MP10),masterWEQ(3,MP10),
     &masterBEQ(3,MP10),masterGEQ(3,MP10),masterMRQS(MP10),
     &masterdetype(MP10)

      common /GR1/ virtualreach,bifurcr,xfact,itypbir,flowhead,ibifurcr2
      common /AI/
     &ICAL,IDIFFREACH,IEND,ISTART,IF,IE,IPRINT,IPUR,IQ,IREACH,KSTART,
     &ICAL13,feeture,feetcount,NONPD,negreach,negreach2,negorder,
     &classobj,kountworks35,ireplace,
     &nostruct,diffreachstop,ITER,
     &LASTREACH,LASTREACH2, ! ---------------------------------------------- 161
     &JS50,JS90,JS95,JS98,JS99,JS995,JS999, 
     &JSKIP,LMONP,KINT,KFEAT,KFEET,KPTARG,jhead,start reach,
     &diffuse type,ifdiffuse,discharge type,diffuse heading,
     &kerror,KEPOL15,KEPOL42,KEPOL15x,KEPOL42x,zeroEF,
      
     &KRFPOL13,KRQPOL13,KRFPOL25,KRQPOL25,KRFPOL27,KRQPOL27,
     &KRFPOL29,KRQPOL29,KRFPOL31,KRQPOL31,KRFPOL33,KRQPOL33,
     &KRFPOL35,KRQPOL35,KRFPOL37,KRQPOL37,KRFPOL39,KRQPOL39,
     &KRFPOL40,KRQPOL40,KRFPOL46,KRQPOL46,KRFPOL48,KRQPOL48,

     &KRFPOL50,KRQPOL50,KRFPOL52,KRQPOL52,KRFPOL54,KRQPOL54,
     &KRFPOL56,KRQPOL56,KRFPOL58,KRQPOL58,

     &KRFPOL13x,KRQPOL13x,KRFPOL25x,KRQPOL25x,KRFPOL27x,KRQPOL27x,
     &KRFPOL29x,KRQPOL29x,KRFPOL31x,KRQPOL31x,KRFPOL33x,KRQPOL33x,
     &KRFPOL35x,KRQPOL35x,KRFPOL37x,KRQPOL37x,KRFPOL39x,KRQPOL39x,
     &KRFPOL40x,KRQPOL40x,KRFPOL46x,KRQPOL46x,KRFPOL48x,KRQPOL48x,
 
     &KRFPOL50x,KRQPOL50x,KRFPOL52x,KRQPOL52x,KRFPOL54x,KRQPOL54x,
     &KRFPOL56x,KRQPOL56x,KRFPOL58x,KRQPOL58x,
     
     &KRAPOL13,KRAPOL25,KRAPOL27,KRAPOL29,KRAPOL31,KRAPOL33,
     &KRAPOL35,KRAPOL37,KRAPOL39,KRAPOL40,KRAPOL46,KRAPOL48,
     &KRAPOL50,KRAPOL52,KRAPOL54,KRAPOL56,KRAPOL58,
     
     &KRAPOL13x,KRAPOL25x,KRAPOL27x,KRAPOL29x,KRAPOL31x,KRAPOL33x,
     &KRAPOL35x,KRAPOL37x,KRAPOL39x,KRAPOL40x,KRAPOL46x,KRAPOL48x,
     &KRAPOL50x,KRAPOL52x,KRAPOL54x,KRAPOL56x,KRAPOL58x,
      
     &IMM,MPAR,MONF,MONQ,MU,NDET,NGAUGE,ipc,ipl,
     &NDETlast,NI,NP,NREACH,NS,SEASD,struckd,IMONTH,IQMON,
     &shot day,kmunth,TEMPD,ndetfirst,NDETBOD,kountd1,kountd2,
     &kountworks,mark works,need works,kill works, ! ----------------------- 161
     &kount bodies 

      common /AT/ bday,bmon,bh,bmin,
     &F1,folder,FUNIT,FNAME,FNAME2,DatName,
     &Globaldataname,Datfilename,Title,Line of data,
     &Sevenchars(MP10),Switchesname,output_folder,
     &flowchars(2),valchars10,valchars11,valchars12,valchars13,
     &B44(MP10)

      common /ATT/ISCREEN,LAPSE,IHR,IMIN,ISEC,IHUN,IYEAR,JMONTH,IDAY,
     &BIGLAPSE
     
      common /BTT/NMORQ,isupprezz,suppress1,suppress3,suppress4,
     &suppress6,suppress7,suppress9,suppress10,
     &suppress11,suppress12,suppress13,suppress14,suppress15,
     &suppress16,suppress17,suppress18,suppress19,suppress20,
     &suppress22,suppress23,
     &suppress00,suppress9a,suppress9b,suppress9c,suppress9d

      common /B/ ADIST(MR),BIDENT(NB),NOBACK(MP10),
     &C(MP10,5),CLD(MP10,5),CMS(MP10,MS),ucms(MP10,MS),DIST(NU),
     &DNA(MP10),DNAME(MP10)
      common /BL/ CTMS(n2prop,MP10,MS),UCTMS(n2prop,MP10,MS),
     &jorder(n2prop),nbodyprop(n2prop),numpfeet(62)

      common /BM/ cmon(MP10,2,13),lmon(MP10,2,13),fmon(2,13),tmon(2,13),
     &smon(2,13),P1mon(MP10,2,13),P2mon(MP10,2,13),seqappbodies(n2prop)

      common /B1/EFD(KR,MS),
     &F(NF,MO),FD(KR,MS),FE(NE,MO),FEND(MR),FLOW(NM),EFLOW(NM)

      common /B2/EFMS(MS),ufms(MS),YY(MS),dfms(MS),
     &FMS(MS),GEQ(3,MP10),IFRQS(NU),
     &IPLAN(MR,3),IRHOLD(MR),JF(NU),JFCAL(NU),SKIPPEFF(NU),
     &JQ(NU),JQCAL(NU),JREACH(NU),JSTOR(KR),
     &JT(NU),KGAUGES(MG),LSAMPOINTS(MM),
     &MK(MT),MKF(MT),MKQ(MT),MKE(MT),NMKF,NMKQ,NMKE,
     &PDEC(NE,MP10),PDEF(NE),PDRC(NV,MP10),PDRF(NF),
     &pollution data(NE,MP10,MO),
     &GIScode(NU),reachstore(MR),GIScode last,kreplace(MP10)

      common /CC/ flowstats(9),propstats(9), step in discharge flow
      common /b3/ NextReach(MR), kount reach (MR)

      common /B9/ ENUM(NE),
     &PNUM(NE,MP10),QBASE(MP10),QD(KR,MP10,MS),QDN(MP10,MR),
     &QDIFFGAP(MP10),QE(MP10,MR),quolity data(NV,MP10,MO+3),
     &QNUM(NV,MP10),Qstations(MM,MP10,3),QTYPE(MP10),QUALN(MP10),
     &QZEROGAP(MP10),QUALNB(MP10,n2prop),UPC(MP10,5),
     &powerbase(NV,MP10),powermini(NV,MP10),
     &avloadp,perkmini11,baselode,rerkmini11,
     &avconcp,perkmini10,baseconc,rerkmini10,
     &RATE(MP10),RFDIFF(MR),RCHA(MR),RCHB(MR),Rchkay(MP10,MR),
     &RLENGTH(MR),RNAME(MR),RQDIFF(MR),RQS(NQ,MP10),MRQS(MP10),
     &SFL(MG,2),UNITS(MP10),WEQ(3,MP10),LUNITS(MP10),
     &BEQ(3,MP10),Detype(Mp10),confail(mp10),
     &XA(MP10),XpA(MP10),X(MP10),X95(MP10),X90(MP10),X99(MP10),
     &XAl(MP10),X95l(MP10),X90l(MP10),X99l(MP10),
     &XAu(MP10),X95u(MP10),X90u(MP10),X99u(MP10),
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
     &propeff2(mp10),propeff3(mp10),propeff5(mp10),propeff12(mp10),
     &propeff39(mp10),propeff60(mp10),propeff61(mp10),propeff62(mp10),
     &prop25(mp10),prop27(mp10),prop13(mp10),prop15(mp10),
     &prop29(mp10),prop31(mp10),prop33(mp10),prop35(mp10),
     &prop46(mp10),prop48(mp10),prop50(mp10),prop52(mp10),
     &prop54(mp10),prop56(mp10),prop58(mp10),prop37(mp10),
     &prop40(mp10),prop42(mp10),prop10(mp10),propall(mp10),
     &propRT(mp10),propNPU(mp10),propNPD(mp10),
     &propabs(mp10),prolosses(mp10),proadds(mp10),
     &diffuse load(MP10,13),nameprop(n2prop)
 


*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg
      common/GP1/ dcalflow,dcalquality,
     &iforce gap fill,calspecial,no gap filling 78,nogap
     &,xupflow,xdownflow ! ------------------------------------------------- 161
      common /GP2/NEXC(MP10),NEXF
      common /GP3/suppress(9,MP10),suppress5,suppress8,suppress21
      common /GP4/CMX(MP10,MS),accum load(MS)
      common /GP5/FMX(MS),JFUSER(NU),JQUSER(NU),KFCAL(NU),KQCAL(NU),
     &QNAT(MP10)
      common /GP6/prop21(mp10),prop22(mp10),
     &propfra(mp10),propqra(mp10),propfrl(mp10),propqrl(mp10)
      common /GP7/IFEAT1,IFEAT2,ICTOP,MAXNITZ,never fitted,JUgap,
     &FDUMP(MS),CDUMP(MP10,MS)
*     gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg


     
      common /B10/propsequence(n2prop),numprop(n2prop)

*     load introduced by natural purification                      ... TNLOADUP2
*     load removed by natural purification                         ... TNLOADDN2
*     load removed by gap filling for river flows                  ... TILOADDN2
*     load removed by gap filling for river quality                ... TALOADDN2
*     load removed by abstractions                                 ... TBLODE2

      common /l3a2/ 
     &ELOAD (MP10,13), ELOADS (MP10), ABLOAD (MP10,13),
     &TNLOADUP2 (MP10,13),TNLOADDN2 (MP10,13),
     &TBLODE2 (MP10,13),
     &TNLOADUP3 (MR,MP10,13), TNLOADDN3 (MR,MP10,13),
     &TBLODE3 (MR,MP10,13),
     &TALOADDN3(MR,MP10,13), TILOADDN3(MR,MP10,13),
     &TLOSSES2(MP10,13), TALOADDN2(MP10,13), TILOADDN2(MP10,13)

      common /l3a6/ RQUALNB(MR,MP10,n2prop),CQUALNB(MR,MP10,n2prop)
      common /l3g/ i13,n13,NSM(MP10,13)
      
      common /l4a/ TELOADAV (NUED,MP10),
     &identify werks(NUED), ! ---------------------------------------------- 161
     &TDLOADAV (NUW,MP10)
      
*     apportionment of percentiles ----------------------------------------- 161
      common /l4b/ TELODEshots(NUED,MP10,MS), ! ---------------------------- 161
     &TABODYshots1(NUW,MP10,2000), ! ----------- testing --------- 161
     &TABODYshots2(NUW,MP10,2000), ! ----------- testing --------- 161
     &TABODYshots3(NUW,MP10,2000)  ! ----------- testing --------- 161
*     monthly loads from upstream sub-catchments -------------------------------
      common /l5a/ TWLOADS(NUW,MP10,13),TWlength(NUW)
*     breakdown of monthly loads from upstream sub-catchments ------------------
      common /l5b/ TWLOADSapp(NUW,MP10,13,N2PROP)
*     monthly loads from upstream sub-catchments at the ends of reaches --------
      common /l5c/ TWloadsrch(NUW,KR,MP10)
*     breakdown of monthly loads from sub-catchments at ends of reaches --------
      common /l5d/ TWloadsrchapp(NUW,KR,MP10,N2PROP)
      common /l5e/ identify bodies(NUW),identify reaches(3,NUW)

      common /l6f/ prune load(13),kprune det,
     %prune shots (MP10,MS), ! --------------------------------------------- 161
     &Check total(MP10),Check sub total(MP10),
     &xupload(MP10,13),xdownload(MP10,13)

*     data generation ----------------------------------------------------------
      common /M/ k90,k95,k98,k99,k995,k999,k10,k05,k01, 
     &k50,k80, ! ----------------------------------------------------------- 161
     &BM(4),BS(4),spcorrRFaf,spcorrRFRC,spcorrfa,kstat2,
     &kf99,kf95,kf90,kf80,kf50,kf20,kf10,kf05,kf01  

*     mass balance -------------------------------------------------------------
      common /W/
     &JP,JP1,EFM,EFS,ECM,ECS,RF3,RC3,EF3,EC3,CO1,CO2,CO3,CO4,CO5,
     &CO6,ECX,TECM,TECS,TECX,TRCX,FSTART,ESTART,GEFM,GEFS,GECM,GECS,
     &current ds quality,IQDIST,IFDIST,ECV,TIME,IMDIST8,IMDIST5,
     &b1,b2,c1,c2,d1,d2,classfraction,correctcorrel,
     &EFshots(MS),ECshots(MS),kstop,EF5,RRRegression,CO1master

      common /L/KSIM
      common /U/UNAME(NU),VNAME(NU)

*     non-parametric distributions ---------------------------------------------
      common /npar/ rfnpvl(mprf),rfnpfd(mprf),nprf,IDENP(2,M7,MP10+1)

*     monthly data -------------------------------------------------------------
      common /seas/ seas0(12),seas1(12),seas2(12),seas3(12), 
     &seas4(12),BSM(12),BSS(12),ISEASP(2,M8,MP10+1)

      common /struct/ itdist,
     &struct0(12),struct1(12),struct2(12),struct3(12), 
     &struct4(12),istruct(2,M9,MP10+1),struct6(12),idfcheck

      common /tem/ itempp(2,M9,2)

      common /z1/ FLMONTH(4,m8),FLMONTHsmall(4,m8),
     &FLSTRUCT (4,m9),FLSTRUCTsmall (4,m9),
     &FLTEMP(4,m10),FLTEMPsmall (4,m10)
      common /y/ FLname (4,m7),FLNAMEsmall (4,m7),FLsequence(4,m7)
      common /Z/ ihalt diffuse river, ihalt diff disch
      common /ZS/ xshots (MP10, MS)

      common /CL/ nclass, background class,
     &class limmits (NC,MP10), class limmits2 (NC,MP10),
     &totals in classes (NC,MP10),totals over all (NC)

*     items for choosing to run parts of models --------------------------------
      common /CP2008/ Included(9999), IncludedList(9999), NFIN(9999,2),
     &NumIncluded

      common /Class/ in class (NC, MP10), conf in class (NC),
     &conf of class (NC), Face Value, Face Value dets (Mp10),
     &QD90 temperature,exclude BOD,exclude NO3, 
     &exclude PO4, exclude DOX, exclude AMM,jstructure message
      common/Class2/ CL(12,MP10),CD(12,MP10),CD1(12,MP10),CD2(12,MP10),
     &COB(4,MP10+1),CP1(12,MP10),CP2(12,MP10),COD(4,MP10)

      common /Year/ elapsed days (12), days in months (12),
     &fraction of year (12)

      common /planning/ addid load (MP10)

      common /overide/ toveride, 
     &chekk infeezible,bagset,kswitch

*     background quality data for reaches - temperature, suspended solids ------
      common /BQ/ Bmat(MR,3,3),PDBC(MR,3),Bnum(MR,3),BMS(3,MS),BC(3,5),
     &BMS global (3,MS)
*     partitioning -------------------------------------------------------------
      common /par/ cpart1(MP10,5),cpart2(MP10,5),Partishun(MP10),
     &PMS1(MP10,MS),PMS2(MP10,MS)

      common /months/ unamex,rnamex,jxtype,running mean,
     &exceedences50 (MP10,13),exceedences90 (MP10,13),
     &exceedences95 (MP10,13)

      common /app/ EQD(n2prop,KR,MP10,MS),
     &CMcontrib(n2prop,MP10,5),LMcontrib(n2prop,MP10,2),
     &loadmon(n2prop,MP10,2,13),lconmon(n2prop,MP10,2,13),
     &munthly structure, month write 1
      
      common /bug/ master set used
      common /lak/ lake45
      common /str/ number of stds, EQS reach (MR,MP10), 
     &standards for reach (MR,NC)
      common /reg/ Creg(4,MS)
      common /ign/ add diffconc (NU), add conc
      common /hed/ diffheadqual(MR,20,4)