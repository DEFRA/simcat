*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of river quality in a river ....
*     ==========================================================================
*     written in FORTRAN.  With the screen displays in VISUAL BASIC .....
*     ==========================================================================

      subroutine proportion of works (kdirect)
      include 'COM.FOR'
      character *10 TenAV(MP10),TenAVT(MP10),TenTE(MP10),
     &Ten90 (MP10), ! 3333333333333333333333333333333333333333333333333333333333
     &Ten95 (MP10),Ten98 (MP10),Ten99 (MP10),Ten995(MP10),Ten999(MP10), ! 333333
     &Ten90t(MP10), ! 3333333333333333333333333333333333333333333333333333333333
     &Ten95t(MP10),Ten98t(MP10),Ten99t(MP10),Ten995t(MP10),Ten999t(MP10) ! 33333
      
      real total effluent prop (MP10), total effluent load (MP10)
      real xworksAV(MP10),xworksTE(MP10),xworks(MP10)

*     kdirect = 0 .... the start of the reach
*     kdirect = 1 .... at feature
*     kdirect = 3 .... the end of the reach
*     kdirect = 9 .... the end of the model
 
      if ( ical13 .eq. 1 ) return
	unamex = 'nothing'
	if ( kdirect .eq. 1 ) unamex = uname(feeture)
	if ( kdirect .eq. 9 ) unamex = 'End of the model'
	if ( kdirect .eq. 0 ) then
      write(unamex,1700)rname(IREACH)
 1700 format('Start of the reach: ',a16)
      endif
	if ( kdirect .eq. 3 ) then
      write(unamex,1701)rname(IREACH)
 1701 format('End of the reach: ',a16)
      endif
	ihead = 0
      
*     ( 1) = 'Mean from all discharges (3,12,5,39)'
*     ( 2) = 'Added by sewage effluents (3)'
*     ( 3) = 'Intermittent discharges of sewage (12)'
*     ( 4) = 'Added by industrial discharges (5)'
*     ( 5) = 'Added by mine waters (39)         '
*     ( 6) = 'Diffuse pollution from livestock (25)'
*     ( 7) = 'Diffuse pollution from arable (27)'
*     ( 8) = 'Highway runoff (29)'
*     ( 9) = 'Urban runoff (31)'
*     (10) = 'Atmosphere deposition (33)'
*     (11) = 'Natural background (35)'
*     (12) = 'Septic tanks (37)'
*     (13) = 'Aggregate CSOs (40)'
*     (14) = 'Aggregated STWs (42)'
*     (15) = 'Diffuse mines (46)'
*     (16) = 'Birds, boats and angling (48)'

      do kdet = 1, mp10
	total effluent load (kdet) = 0.0
	total effluent prop (kdet) = 0.0
      enddo      

      tot12AV = 0.0
      tot1290 = 0.0
      tot1295 = 0.0
      tot1298 = 0.0
      tot1299 = 0.0
      tot12995 = 0.0
      tot12999 = 0.0
      tot03AV = 0.0
      tot0390 = 0.0
      tot0395 = 0.0
      tot0398 = 0.0
      tot0399 = 0.0
      tot03995 = 0.0
      tot03999 = 0.0
      
*     loop on the number of upstream works -------------------------------------
      do 1000 iworks = 1, kount works
      iline = 0
      jworks = identify works (iworks)
	jjreach = JREACH (jworks)
      xworksd = 0.0

      do idet = 1, MP10
*     initialise the proportion of effluent ------------------------------------
      TenAV(idet)   = '     .....' ! % mean of the total load
      TenAVT(idet)  = '     .....' ! % of the total load
      TenTE(idet)   = '     .....' ! % of the total effluent load
      Ten90(idet)   = '     .....' ! % of load for the 95-percentile in the river
      Ten95(idet)   = '     .....' ! % of load for the 95-percentile in the river
      Ten98(idet)   = '     .....' ! % of load for the 95-percentile in the river
      Ten99(idet)   = '     .....' ! % of load for the 99-percentile in the river
      Ten995(idet)  = '     .....' ! % of load for the 99-percentile in the river
      Ten999(idet)  = '     .....' ! % of load for the 99-percentile in the river
      Ten90t(idet)  = '     .....' ! total 95-percentile load in the river
      Ten95t(idet)  = '     .....' ! total 95-percentile load in the river
      Ten98t(idet)  = '     .....' ! total 95-percentile load in the river
      Ten99t(idet)  = '     .....' ! total 99-percentile load in the river
      Ten995t(idet) = '     .....' ! total 99-percentile load in the river
      Ten999t(idet) = '     .....' ! total 99-percentile load in the river
      enddo

*     loop on determinands .....................................................
      do 1001 idet = 1, ndet
	if ( QTYPE (idet) .eq. 4 ) goto 1001

      xworksAV (idet) = 0.0 ! % of the total load
	if ( TGLODE2 (idet,i13) .gt. 1.0e-15 ) then
      xworksAV(idet) = 100.0*TELOADAV(iworks,idet)/TGLODE2(idet,i13)
      
*     if ( n148 .eq. 1 .and. ndshot .eq. idet ) then ! 3333333333333333333333333     
      call identify the shots for percentiles (idet) ! 3333333333333333333333333
*     endif ! if ( n148 .eq. 1 .and. ndshot .eq. idet ) 333333333333333333333333

      total effluent prop (idet) = total effluent prop (idet) 
     &+ xworksAV(idet)
      
      if ( JT(jworks) .eq. 12 ) then ! septic tanks  ! 3333333333333333333333333
      tot12AV = tot12AV + xworksAV(idet)
      endif
      if ( JT(jworks) .eq. 3 ) then ! sewage works ! 333333333333333333333333333
      tot03AV = tot03AV + xworksAV(idet)
      endif
      call sort format 2 (xworksAV(idet),TGLODE2(idet,i13))
	TenAV(idet) = valchars10
	TenAVT(idet) = valchars11
      
      xworksTE (idet) = 0.0 ! % of the total effluent load
      if ( TELODE2 (idet,i13) .gt. 1.0e-15 ) then
      xworksTE(idet) = 100.0*TELOADAV(iworks,idet)/TELODE2(idet,i13)
	total effluent load (idet) = total effluent load (idet)
     & + xworksTE(idet)
	xworksd = amax1 ( xworksd, xworksTE(idet) )
      call sort format 1 (xworksTE(idet))
	TenTE(idet) = valchars10
      endif ! if ( TELODE2 (idet,i13) .gt. 1.0e-15 )
      endif ! if ( TGLODE2 (idet,i13) .gt. 1.0e-15 )
      
*     33333333333333333333333333333333333333333333333333333333333333333333333333
      if ( n148 .eq. 1 .and. idet .eq. ndshot ) then ! 3333333333333333333333333
      XXLD = CMS(idet,JS90) * FMS(JS90) ! 33333333333333333333333333333333333333
      xworks (idet) = 0.0 ! % of load for the 90-percentile in the river 3333333
 	if ( XXLD .gt. 1.0e-15 ) then
      xworks(idet) = 100.0*TELOADshots(iworks,idet,JS90)/XXLD ! 3333333333333333
      if ( JT(jworks) .eq. 12 ) then ! septic tanks 3333333333333333333333333333
      tot1290 = tot1290 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 3 ) then ! sewage works 33333333333333333333333333333
      tot0390 = tot0390 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      call sort format 2 (xworks(idet),XXLD)
	Ten90(idet) = valchars10 ! 33333333333333333333333333333333333333333333333
	Ten90t(idet) = valchars11 ! 3333333333333333333333333333333333333333333333
      endif ! if ( XXLD .gt. 1.0e-15 ) ! 333333333333333333333333333333333333333
      
      XXLD = CMS(idet,JS95) * FMS(JS95) ! 33333333333333333333333333333333333333
      xworks (idet) = 0.0 ! % of load for the 95-percentile in the river 3333333
 	if ( XXLD .gt. 1.0e-15 ) then
      xworks(idet) = 100.0*TELOADshots(iworks,idet,JS95)/XXLD ! 3333333333333333
      if ( JT(jworks) .eq. 12 ) then ! septic tanks 3333333333333333333333333333
      tot1295 = tot1295 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 3 ) then ! sewage works 33333333333333333333333333333
      tot0395 = tot0395 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      call sort format 2 (xworks(idet),XXLD)
	Ten95(idet) = valchars10 ! 33333333333333333333333333333333333333333333333
	Ten95t(idet) = valchars11 ! 3333333333333333333333333333333333333333333333
      endif ! if ( XXLD .gt. 1.0e-15 )! 3333333333333333333333333333333333333333

      XXLD = CMS(idet,JS98) * FMS(JS98) ! 33333333333333333333333333333333333333
      xworks (idet) = 0.0 ! % of load for the 98-percentile in the river 3333333
 	if ( XXLD .gt. 1.0e-15 ) then ! 333333333333333333333333333333333333333333
      xworks(idet) = 100.0*TELOADshots(iworks,idet,JS98)/XXLD ! 3333333333333333
      if ( JT(jworks) .eq. 12 ) then ! septic tanks 3333333333333333333333333333
      tot1298 = tot1298 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 3 ) then ! sewage works 33333333333333333333333333333
      tot0398 = tot0398 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      call sort format 2 (xworks(idet),XXLD)
	Ten98(idet) = valchars10 ! 33333333333333333333333333333333333333333333333
	Ten98t(idet) = valchars11 ! 3333333333333333333333333333333333333333333333
      endif ! if ( XXLD .gt. 1.0e-15 ) 33333333333333333333333333333333333333333

      XXLD = CMS(idet,JS99) * FMS(JS99) ! 33333333333333333333333333333333333333
      xworks (idet) = 0.0 ! % of load for the 99-percentile in the river 3333333
 	if ( XXLD .gt. 1.0e-15 ) then ! 333333333333333333333333333333333333333333
      xworks(idet) = 100.0*TELOADshots(iworks,idet,JS99)/XXLD ! 3333333333333333
      if ( JT(jworks) .eq. 12 ) then ! septic tanks 3333333333333333333333333333
      tot1299 = tot1299 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 3 ) then ! sewage works 33333333333333333333333333333
      tot0399 = tot0399 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      call sort format 2 (xworks(idet),XXLD)
	Ten99(idet) = valchars10 ! 33333333333333333333333333333333333333333333333
	Ten99t(idet) = valchars11 ! 3333333333333333333333333333333333333333333333
      endif ! if ( XXLD .gt. 1.0e-15 ) 33333333333333333333333333333333333333333
      
      XXLD = CMS(idet,JS995) * FMS(JS995) ! 333333333333333333333333333333333333
      xworks (idet) = 0.0 ! % of load for the 99.5-percentile in the river 33333
 	if ( XXLD .gt. 1.0e-15 ) then
      xworks(idet) = 100.0*TELOADshots(iworks,idet,JS995)/XXLD ! 333333333333333
      if ( JT(jworks) .eq. 12 ) then ! septic tanks 3333333333333333333333333333
      tot12995 = tot12995 + xworks(idet) ! 3333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 3 ) then ! sewage works 33333333333333333333333333333
      tot03995 = tot03995 + xworks(idet) ! 3333333333333333333333333333333333333
      endif
      call sort format 2 (xworks(idet),XXLD)
	Ten995(idet) = valchars10 ! 3333333333333333333333333333333333333333333333
	Ten995t(idet) = valchars11 ! 333333333333333333333333333333333333333333333
      endif ! if ( XXLD .gt. 1.0e-15 ) ! 333333333333333333333333333333333333333

      XXLD = CMS(idet,JS999) * FMS(JS999) ! 333333333333333333333333333333333333
      xworks (idet) = 0.0 ! % of load for the 99.9-percentile in the river 33333
 	if ( XXLD .gt. 1.0e-15 ) then
      xworks(idet) = 100.0*TELOADshots(iworks,idet,JS999)/XXLD ! 333333333333333
      if ( JT(jworks) .eq. 12 ) then ! septic tanks 3333333333333333333333333333
      tot12999 = tot12999 + xworks(idet) ! 3333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 3 ) then ! sewage works 33333333333333333333333333333
      tot03999 = tot03999 + xworks(idet) ! 3333333333333333333333333333333333333
      endif
      call sort format 2 (xworks(idet),XXLD)
	Ten999(idet) = valchars10 ! 3333333333333333333333333333333333333333333333
	Ten999t(idet) = valchars11 ! 333333333333333333333333333333333333333333333
      endif ! if ( XXLD .gt. 1.0e-15 ) ! 333333333333333333333333333333333333333
      endif ! if ( n148 .eq. 1 .and. idet .eq. ndshot ) ! 3333333333333333333333
*     33333333333333333333333333333333333333333333333333333333333333333333333333

*     check that the percentage load does not exceed 100 -----------------------
	if ( xworksTE (idet) .gt. 100.0001 ) then
	call change colour of text (20) ! bright red
	write( *,7300)uname(jworks)
      call set screen text colour
	write(01,7300)uname(jworks)
	write(09,7300)uname(jworks)
	write(33,7300)uname(jworks) ! to ERR file
      write(40,7300)uname(jworks) ! to APT file
 7300 format(/77('-')/
     &'Error in the calculation of loads attributable to ',
     &'effluents ... '/
     &'Percentage exceeds 100.0 ... for: ',a37/77('-')) 
	write(01,7000)TELOADAV(iworks,idet),dname(idet)
	write(09,7000)TELOADAV(iworks,idet),dname(idet)
      write(40,7000)TELOADAV(iworks,idet),dname(idet) ! to APT file
	write(33,7000)TELOADAV(iworks,idet),dname(idet) ! to ERR file
 7000 format('   Load from works =',f15.6,' for ',a11)
	write(01,7001)TELODE2(idet,i13)
	write(09,7001)TELODE2(idet,i13)
      write(40,7001)TELODE2(idet,i13) ! to APT file
	write(33,7001)TELODE2(idet,i13) ! to ERR file
 7001 format('    Total net load =',f15.6)
	write(01,7002)xworksTE(idet)
	write(09,7002)xworksTE(idet)
      write(40,7002)xworksTE(idet) ! to APT file
	write(33,7002)xworksTE(idet) ! to ERR file
 7002 format('        Percentage =',f15.6/77('-'))
      call stop
      endif ! if ( xworksTE (idet) .gt. 100.0001 )
 1001 continue ! end of loop on determinands ...................................

 	if ( xworksd .gt. 0.00001 ) then
      iline = 1
 	if ( ihead .ne. 2 ) ihead = 1
      endif ! if ( xworksd .gt. 0.00001 )

*     write heading -----------------------------------------------------------2
      if ( ihead .eq. 1 ) then
      if ( nobigout .le. 0 ) then
	write(01,2)unamex
    2 format(/150('-')/'The percentage of the total in-river load and ',
     &'the total effluent load supplied by individual discharges at: ',
     &a37/150('-'))
      write(01,8766)(Dna(idet),idet=1,ndet)
 8766 format(46x,10(a4,6x))
      write(01,7654)
 7654 format(150('-'))
      write(40,2)unamex ! to APT file
      write(40,8766)(Dna(idet),idet=1,ndet) ! to APT file
      write(40,7654) ! to APT file
      do idet = 1, ndet
      if ( qtype(idet) .ne. 4 ) then
      write(120+idet,2)unamex ! to APT file for individual determinands
      write(120+idet,8766)Dna(idet) ! to APT file
      write(120+idet,7654) ! to APT file
      endif
      enddo
      
      
      if ( n148 .eq. 1 ) then ! 333333333333333333333333333333333333333333333333
      write(39,4443)unamex ! 333333333333333333333333333333333333333333333333333
 4443 format(///150('-')/'The percentage of the load for the ','
     &percentile concentrations supplied by individual discharges ',
     &'at: ',a37/150('-'))
      if ( ndet .le. 5 ) then ! 333333333333333333333333333333333333333333333333
      write(39,8776) ! to a95 file 333333333333333333333333333333333333333333333
 8776 format(46x,'mean   90%tile   95%tile   98%tile',
     &'   99%tile 99.5%tile 99.9%tile')
      write(39,7654) ! to a95 file 333333333333333333333333333333333333333333333
      write(39,4394)JS90,JS95,JS98,JS99,JS995,JS999 ! 33333333333333333333333333
 4394 format('Monte Carlo shot',33x,'-',10i10) ! 3333333333333333333333333333333
      write(39,4294)FLOW(1),FMS(JS90),FMS(JS95),FMS(JS98),FMS(JS99), ! 333333333
     &FMS(JS995),FMS(JS999) ! 33333333333333333333333333333333333333333333333333
 4294 format('River flow',30x,10f10.2) ! 333333333333333333333333333333333333333
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      write(140+idet,4443)unamex ! 333333333333333333333333333333333333333333333
      write(140+idet,8776) ! to a95 file 333333333333333333333333333333333333333
      write(140+idet,7654) ! to a95 file 333333333333333333333333333333333333333
      write(140+idet,4394)JS90,JS95,JS98,JS99,JS995,JS999 ! to a95 file 33333333
      write(140+idet,4294)FLOW(1),FMS(JS90),FMS(JS95),FMS(JS98), ! 3333333333333
     &FMS(JS99),FMS(JS995),FMS(JS999) ! 3333333333333333333333333333333333333333
      write(140+idet,4494)dname(idet),C(idet,1),CMS(idet,JS90), ! 33333333333333
     &CMS(idet,JS95),CMS(idet,JS98), ! 33333333333333333333333333333333333333333
     &CMS(idet,JS99),CMS(idet,JS995),CMS(idet,JS999) ! 3333333333333333333333333
      write(39,4494)dname(idet),C(idet,1),CMS(idet,JS90),CMS(idet,JS95), ! 33333
     &CMS(idet,JS98), ! 33333333333333333333333333333333333333333333333333333333
     &CMS(idet,JS99),CMS(idet,JS995),CMS(idet,JS999) ! 3333333333333333333333333
 4494 format('River concentration: ',a11,8x,10f10.2) ! 3333333333333333333333333
      write(140+idet,7654) ! to CMT file 333333333333333333333333333333333333333
      endif
      enddo
      endif ! 333333333333333333333333333333333333333333333333333333333333333333
      write(39,7654) ! to a95 file 333333333333333333333333333333333333333333333
      endif ! if ( n148 .eq. 1 ) ! 333333333333333333333333333333333333333333333
      ihead = 2
      endif ! if ( nobigout .le. 0 )
      endif ! if ( ihead .eq. 1 )

      if ( iline .eq. 1 ) then
      iline = 0
      if ( nobigout .le. 0 ) then
      if ( ndet .gt. 5 ) then
      write(01,47)uname(jworks),(TenAV(jdet),jdet=1,mp10),iworks
   47 format(a40,10a10,1x,i6)
 	write(01,6)(TenTE(jdet),jdet=1,ndet)
    6 format('(percentage of the total discharge load)',10a10)
      write(40,47)uname(jworks),(TenAV(jdet),jdet=1,mp10),iworks ! to APT file
      do jdet = 1, ndet
      if ( qtype(jdet) .ne. 4 ) then
      write(120+jdet,3347)uname(jworks),TenAV(jdet),TenTE(jdet) ! to APT
 3347 format(a40,2a10)
      endif
      enddo
      else ! if ( ndet .le. 5 )
      write(01,94)uname(jworks),(TenAV(jd),jd=1,5),
     &(TenTE(jd),jd=1,5),iworks
   94 format(a40,10a10,1x,i6)
      write(40,94)uname(jworks),(TenAV(jd),jd=1,5), ! to APT file
     &(TenTE(jd),jd=1,5),iworks
      do jdet = 1, ndet
      if ( qtype(jdet) .ne. 4 ) then
      write(120+jdet,3347)uname(jworks),TenAV(jdet),TenTE(jdet) ! to APT
      endif
      enddo
      endif ! if ( ndet gt/lt 5 )
      
      if ( n148 .eq. 1 ) then ! 333333333333333333333333333333333333333333333333
      write(39,4194)uname(jworks),TenAV(ndshot),Ten90(ndshot), ! 333333333333333
     &Ten95(ndshot),Ten98(ndshot), ! 3333333333333333333333333333333333333333333
     &Ten99(ndshot),Ten995(ndshot),Ten999(ndshot) ! 3333333333333333333333333333
 4194 format(a40,10a10,1x,i6) ! 333333333333333333333333333333333333333333333333
      write(140+ndshot,4194)uname(jworks),TenAV(ndshot),Ten90(ndshot), ! 3333333
     &Ten95(ndshot),Ten98(ndshot), ! 3333333333333333333333333333333333333333333
     &Ten99(ndshot),Ten995(ndshot),Ten999(ndshot) ! 3333333333333333333333333333
      endif ! if ( n148 .eq. 1 ) 33333333333333333333333333333333333333333333333

      endif ! if ( nobigout .le. 0 )
      
	do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
      
      write(110+idet,9099)GIScode(feeture),unamex,uname(jworks),
     &dname(idet),xworksAV(idet),xworksTE(idet),jxtype
 9099 format(a40,',',a40,',',a40,',',
     &'% Annual contribution from individual works',',',a11,2(',',
     &1pe11.4),',,,,,,,,,,,,',i4,', 3,12, 5,39')
      endif ! if ( QTYPE (idet) .ne. 4 )
      enddo ! idet = 1, ndet
      endif ! if ( iline .eq. 1 )

 1000 continue ! end of loop on the number of works

      
*     print out a summary if any lines were printed ---------------------------A
      if ( ihead .eq. 2 ) then
      if ( ndet .le. 5 ) then
      do kdet = 1, mp10
      TenAV(kdet) = '          '
      TenTE(kdet) = '          '
      enddo ! do kdet = 1, mp10     
      do jdet = 1, ndet
      TenAV(jdet) = '     .....'
      TenTE(jdet) = '     ......'
      if ( total effluent prop (jdet) .gt. 1.0e-20 ) then
      call sort format 1 (total effluent prop (jdet))
      TenAV(jdet) = valchars10
      endif ! if ( total effluent prop (jdet) .gt. 1.0e-20 )
      if ( total effluent load (jdet) .gt. 1.0e-20 ) then
      call sort format 1 (total effluent load (jdet))
      TenTE(jdet) = valchars10
      
      endif ! if ( total effluent load (jdet) .gt. 1.0e-20 )
      enddo ! do jdet = 1, ndet
 	write(01,2146)(TenAV(jdet),jdet=1,5), ! to OUT file
     &(TenTE(jdet),jdet=1,5)
 	write(40,2146)(TenAV(jdet),jdet=1,5), ! to APT file
     &(TenTE(jdet),jdet=1,5)
 2146 format(150('-')/
     &'Total percentage discharge load         ',10a10)
      write(01,49)
   49 format(150('-'))
      write(40,49)
 
*     kkstop = 0
*     if ( total effluent load (jdet) .lt. 99.999 .or. 
*    &total effluent load (jdet) .gt. 100.001 ) then
*     Write(*,*)'ERROR IN ADDING STWS LOADS ...',
*    &total effluent load (jdet)
*     if (total effluent load (jdet) .gt. 0.00001) kkstop = 1
*     endif


*     if ( kkstop .eq. 1 ) call stop
      do jdet = 1, ndet
          
      if ( qtype(jdet) .ne. 4 ) then
  	write(120+jdet,2146)TenAV(jdet),TenTE(jdet) ! APT file
      write(120+jdet,49)
      endif
      enddo
      else ! ndet .gt. 5 
 	write(01,2288)(TenAV (jdet),jdet=1,ndet)
 	write(40,2288)(TenAV (jdet),jdet=1,ndet) ! to APT file
 2288 format(150('-')/
     &'Total percentage discharge proportion   ',10a10)
 	write(01,2148)(TenTE (jdet),jdet=1,ndet)
 	write(40,2148)(TenTE (jdet),jdet=1,ndet) ! to APT file
 2148 format(150('-')/
     &'Total percentage discharge load         ',10a10)
	write(01,49)
 	write(40,49) ! to APT file
      do jdet = 1, ndet
      if ( qtype(jdet) .ne. 4 ) then
 	write(120+jdet,2148)TenAV(jdet),TenTE(jdet) ! to APT file
      write(120+jdet,49)
      endif
      enddo
      endif

      if ( n148 .eq. 1 ) then ! 333333333333333333333333333333333333333333333333
      write(39,2546)tot12av,tot1290,tot1295, ! 333333333333333333333333333333333
     &tot1298,tot1299,tot12995,tot12999 ! 33333333333333333333333333333333333333
      write(140+ndshot,2546)tot12av,tot1290,tot1295, ! 3333333333333333333333333
     &tot1298,tot1299,tot12995,tot12999 ! 33333333333333333333333333333333333333
 2546 format(150('-')/
     &'Total % contributions from intermittents',10f10.2)
      write(39,3546)tot03av,tot0390,tot0395, ! 333333333333333333333333333333333
     &tot0398,tot0399,tot03995,tot03999
      write(140+ndshot,3546)tot03av,tot0390,tot0395, ! 3333333333333333333333333
     &tot0398,tot0399,tot03995,tot03999
 3546 format(150('-')/
     &'Total % contributions from sewage works ',10f10.2)
      write(39,2246)TenAVt(ndshot),Ten90t(ndshot),Ten95t(ndshot), ! 333333333333
     &Ten98t(ndshot),Ten99t(ndshot),Ten995t(ndshot),Ten999t(ndshot) ! 3333333333
      write(140+ndshot,2246)TenAVt(ndshot),Ten90t(ndshot), ! 3333333333333333333
     &Ten95t(ndshot), ! 33333333333333333333333333333333333333333333333333333333
     &Ten98t(ndshot),Ten99t(ndshot),Ten995t(ndshot),Ten999t(ndshot) ! 3333333333
 2246 format(150('-')/
     &'Total load in the percentile            ',10a10)
      write(39,7654) ! 333333333333333333333333333333333333333333333333333333333
      write(140+ndshot,7654) ! 3333333333333333333333333333333333333333333333333
      endif ! if ( n148 .eq. 1 ) ! 333333333333333333333333333333333333333333333

      do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
      if ( nobigout .le. 0 ) then
      write(110+idet,9299)GIScode(feeture),unamex,
     &dname(idet),total effluent prop (idet),total effluent load (idet),
     &jxtype
 9299 format(a40,',',a40,',','All works',',',
     &'% annual contribution from works',',',a11,2(',',
     &1pe11.4),',,,,,,,,,,,,',i4,', 3,12, 5,39')
      
      endif ! if ( nobigout .le. 0 )
      endif ! if ( QTYPE (idet) .ne. 4 )
	enddo ! do idet = 1, ndet
      endif ! if ( ihead .eq. 2 ) 

      return
      end

      
      
      
      subroutine proportion of catchments (kdirect) 
      include 'COM.FOR'
      character *10 Tenchars1(MP10)
      
      character *10 TenAV(MP10),TenAVT(MP10),TenTE(MP10),
     &Ten90 (MP10), ! 5555555555555555555555555555555555555555555555555555555555
     &Ten95 (MP10),Ten98 (MP10),Ten99 (MP10),Ten995(MP10),Ten999(MP10), ! 555555
     &Ten90t(MP10), ! 5555555555555555555555555555555555555555555555555555555555
     &Ten95t(MP10),Ten98t(MP10),Ten99t(MP10),Ten995t(MP10),Ten999t(MP10) ! 55555
     
      real total effluent prop (MP10), total effluent load (MP10)
      real xworksAV(MP10),xworksTE(MP10),xworks(MP10)

      dimension xbodies (NUW,MP10), xwload (MP10)
      real total bodies prop (MP10), total bodies load (MP10), 
     &total bodies length (MP10)
      real brakedown(NUW,MP10,nprop+1),braketotal(nprop+1)

      real totAV(nprop),tot90(nprop),tot95(nprop),tot98(nprop),
     &tot99(nprop),tot995(nprop),tot999(nprop)

      do idet = 1, MP10
      Tenchars1(idet) = '     .....'
      enddo
      
      if ( ical13 .eq. 1 ) return
     
*     kdirect = 0 .... start of the reach --------------------------------------
*     kdirect = 1 .... at feature ----------------------------------------------
*     kdirect = 3 .... end of the reach ----------------------------------------
*     kdirect = 9 .... end of the model ----------------------------------------

	unamex = 'nothing'
	if ( kdirect .eq. 1 ) unamex = uname(feeture)
	if ( kdirect .eq. 9 ) unamex = 'End of the model'
	if ( kdirect .eq. 0 ) then
      write(unamex,1276)rname(IREACH)
 1276 format('Start of the reach - ',a16)
      endif
	if ( kdirect .eq. 3 ) then
      write(unamex,1277)rname(IREACH)
 1277 format('End of the reach - ',a16)
      endif

*     initialise the totals ----------------------------------------------------
      do kdet = 1, mp10
      total bodies load (kdet) = 0.0
      total bodies prop (kdet) = 0.0
      total bodies length (kdet) = 0.0
      enddo      

*     initialise the load in each sub-catchment (xbodies) ----------------------
      do 2507 ibodies = 1, kount bodies
      do 2508 idet = 1, ndet
      brakedown ( ibodies, idet, nprop + 1) = 0.0
      if ( QTYPE (idet) .ne. 4 ) then
      xbodies (ibodies,idet) = 0.0
      if ( TGLODE2 (idet,i13) .gt. 1.0e-15 ) then
      xbodies (ibodies,idet) = TWLOADS (ibodies,idet,i13)
      total bodies load (idet) = total bodies load (idet) 
     & + xbodies (ibodies,idet)
      endif
      endif
 2508 continue ! ibodies = 1, kount bodies
 2507 continue ! idet = 1, ndet

      
      
      if ( n148 .eq. 1 ) then ! 333333333333333333333333333333333333333333333333
      write(41,4443)unamex ! 555555555555555555555555555555555555555555555555555
 4443 format(/150('-')/'The percentage apportionment of the load from ',
     &'catchments for',
     &' the percentile concentrations at: ',a37/150('-'))
      if ( ndet .le. 5 ) then ! 555555555555555555555555555555555555555555555555
      write(41,8776) ! 555555555555555555555555555555555555555555555555555555555
 8776 format(46x,'mean   90%tile   95%tile   98%tile',
     &'   99%tile 99.5%tile 99.9%tile')
      write(41,7654) ! 555555555555555555555555555555555555555555555555555555555
 7654 format(150('-'))
      write(41,4394)JS90,JS95,JS98,JS99,JS995,JS999 ! 55555555555555555555555555
 4394 format('Monte Carlo shot',33x,'-',10i10) ! 3333333333333333333333333333333
      write(41,4294)FLOW(1),FMS(JS90),FMS(JS95),FMS(JS98),FMS(JS99), ! 555555555
     &FMS(JS995),FMS(JS999) ! 55555555555555555555555555555555555555555555555555
 4294 format('River flow',30x,10f10.2) ! 333333333333333333333333333333333333333
      do idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then
      write(41,4494)dname(idet),C(idet,1),CMS(idet,JS90),CMS(idet,JS95), ! 55555
     &CMS(idet,JS98), ! 55555555555555555555555555555555555555555555555555555555
     &CMS(idet,JS99),CMS(idet,JS995),CMS(idet,JS999) ! 5555555555555555555555555
 4494 format('River concentration: ',a11,8x,10f10.2) ! 3333333333333333333333333
      endif
      enddo
      endif ! 333333333333333333333333333333333333333333333333333333333333333333
      write(41,7654) ! 555555555555555555555555555555555555555555555555555555555
      endif ! if ( n148 .eq. 1 ) ! 333333333333333333333333333333333333333333333
      
*     if ( n148 .eq. 1 ) then ! 555555555555555555555555555555555555555555555555
*     write(41,4194)uname(feeture),TenAV(ndshot),Ten90(ndshot), ! 33333333333333
*    &Ten95(ndshot),Ten98(ndshot), ! 3333333333333333333333333333333333333333333
*    &Ten99(ndshot),Ten995(ndshot),Ten999(ndshot) ! 3333333333333333333333333333
*4194 format(a40,10a10,1x,i6) ! 333333333333333333333333333333333333333333333333
*     endif ! if ( n148 .eq. 1 ) 33333333333333333333333333333333333333333333333

      
      
*     write headings and the loads from each sub-catchment ---------------------
      if ( nobigout .gt. 0 ) return
      ihead = 0
      do 2000 ibodies = 1, kount bodies
      jbodies = identify bodies (ibodies)
      xltot = 0.0
      do 2513 idet = 1, ndet
      Tenchars1(idet) = '      ....'
	if ( QTYPE (idet) .ne. 4 ) then
*     write heading ------------------------------------------------------------
	if ( ihead .eq. 0 ) then
	write(44,112)unamex ! to APC file
  112 format(150('=')/'The total river load supplied by the ',
     &'individual sub-catchments at ',
     &'... ',a37/150('='))
	write(01,12)unamex
   12 format(///150('=')/'The total river load supplied by the ',
     &'individual sub-catchments at ',
     &'... ',a37/150('='))
      ihead = 1
      endif ! if ( ihead .eq. 0 ) 
      if ( n148 .eq. 1 ) then ! 333333333333333333333333333333333333333333333333
      write(140+idet,12)unamex ! to a95 file 33333333333333333333333333333333333
      endif ! if ( n148 .eq. 1 ) 33333333333333333333333333333333333333333333333
      xltot = xltot + xbodies (ibodies,idet)
      call sort format 1 (xbodies (ibodies,idet))
*     --------------------------------------------------------------------------
      Tenchars1(idet) = valchars10
      endif ! if ( QTYPE (idet) .ne. 4 )
 2513 continue ! idet = 1, ndet
      
      if ( xltot .gt. 1.0e-09 ) then
      write(44,4)uname(jbodies),(Tenchars1(jdet),jdet=1,MP10)
    4 format(5x,a40,5x,10a10)
      write(01,41)uname(jbodies),(Tenchars1(jdet),jdet=1,MP10)
   41 format(a40,10a10)
      endif
      
      do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
	total bodies length (idet) = total bodies length (idet) + 
     &TWlength(ibodies)
	xload per km = 0.0
	if ( TWlength(ibodies) .gt. 0.0001 ) then
	xload per km = xbodies (ibodies,idet) / TWlength(ibodies)
      xload per km = amax1 ( 0.0, xload per km )
	endif
      xper = 0.0
	if ( TGLODE2(idet,i13) .gt. 0.0001 ) then
      xper = 100.0 * xbodies (ibodies,idet) / TGLODE2(idet,i13)
      xper = amax1 ( 0.0, xper )
      endif

      call sort format 2 (xload per km, xper)
      if ( n148 .eq. 1 ) then ! 333333333333333333333333333333333333333333333333
      write(140+idet,42)uname(jbodies),Tenchars1(idet),xper, ! to a95 file 33333
     &TWlength(ibodies),valchars10
   42 format('Load from: ',a40,6x,a10,'(',f7.2,' %)',
     &' over',f7.1,' km (',a10,' per km) ... All contributions')
      endif ! if ( n148 .eq. 1 ) 33333333333333333333333333333333333333333333333
     
      write(110+idet,43)GIScode(feeture),unamex,uname(jbodies),
     &dname(idet),xbodies (ibodies,idet),xper,TWlength(ibodies),
     &xload per km,jxtype
   43 format(a40,',',a40,',',a16,',',
     &'Annual load from sub-catchment',
     &',',a11,4(',',1pe11.4),',,,,,,,,,,',i4,',24')
     
      endif
      enddo
 2000 continue ! ibodies = 1, kount bodies
      write(44,1115) ! to APC file
      write(01,1115)
    5 format(150('='))

*     write the totals over all sub-catchments ---------------------------------
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
	if ( QTYPE (idet) .ne. 4 ) then
      call sort format 1 (total bodies load (idet))
      Tenchars1(idet) = valchars10
      endif
      enddo
      write(44,8)(Tenchars1(jdet),jdet=1,ndet) ! to APC file
      write(01,82)(Tenchars1(jdet),jdet=1,ndet)
    8 format(5x,'Total load ...                          ',5x,10a10)
   82 format('Total load ...                          ',10a10)
      write(44,5) ! to APC file
      write(01,5)

      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
	if ( QTYPE (idet) .ne. 4 ) then

	xload per km = 0.0
	if ( total bodies length (idet) .gt. 0.0001 ) then
	xload per km = total bodies load (idet) /
     &total bodies length (idet)
      xload per km = amax1 ( 0.0, xload per km )
	endif
      xper = 0.0
	if ( TGLODE2(idet,i13) .gt. 0.0001 ) then
      xper = 100.0 * total bodies load (idet) / TGLODE2(idet,i13)
      xper = amax1 ( 0.0, xper )
      endif

      call sort format 1 (total bodies load (idet))
      Tenchars1(idet) = valchars10

      call sort format 2 (xload per km, xper)

      if ( n148 .eq. 1 ) write(140+idet,812)Tenchars1(idet),xper, ! 333333333333
     &total bodies length (idet),valchars10
  812 format(150('-')/'Total load from all sub-catchments',23x,a10,
     &'(',f7.2,' %)',' over',f7.1,' km (',
     &a10,' per km) ... All contributions')
     
      write(110+idet,813)GIScode(feeture),unamex,dname(idet),
     &total bodies load (idet),xper,
     &total bodies length (idet),xload per km,jxtype
  813 format(a40,',',a40,',','All sub-catchments',',',
     &'Annual load from all sub-catchments',',',a11,4(',',
     &1pe11.4),',,,,,,,,,,',i4,',24')
     
      call sort format 1 (TGLODE2(idet,i13))
      if ( n148 .eq. 1 ) write(140+idet,872)valchars10 ! to a95 file 33333333333
  872 format(150('-')/'Total load',47x,a10)
      if ( n148 .eq. 1 ) write(140+idet,5) ! to a95 file 33333333333333333333333
      endif
      enddo

*     write the load per kilometre per sub-catchment ---------------------------  
      ihead = 0
      do 2001 ibodies = 1, kount bodies
      jbodies = identify bodies (ibodies)
      xltot = 0.0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
	if ( QTYPE (idet) .ne. 4 ) then
	if ( ihead .eq. 0 ) then
	write(44,62) ! to APC file
	write(01,62)
   62 format('The total river load supplied per kilometre ',
     &'by the individual sub-catchments upstream of ',
     &'this point'/150('='))
      ihead = 1
      endif
      xwrite = 0.0
      if ( TWlength(ibodies) .gt. 0.0001 ) then
      xwrite = xbodies(ibodies,idet)/TWlength(ibodies)
      xltot = xltot + xwrite
      endif
      call sort format 1 (xwrite)
      Tenchars1(idet) = valchars10
      endif
      enddo
      if ( xltot .gt. 1.0e-09 ) then
      write(44,4)uname(jbodies),(Tenchars1(jdet),jdet=1,MP10)
      write(01,41)uname(jbodies),(Tenchars1(jdet),jdet=1,MP10)
   34 format(5x,a40,5x,10a10)
   44 format(a40,10a10)
      endif
 2001 continue

      
      
      
      if ( kount bodies .gt. 99999 ) then
      xltot = 0.0
      do 2500 idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
      do 2501 ibody = 1, kount bodies - 1
      xbodies (kount bodies,idet) = xbodies (kount bodies,idet) 
     &                            - xbodies (ibody,idet) 
 2501 continue
      call sort format 1 (xbodies (kount bodies,idet))
      Tenchars1(idet) = valchars10

      xltot = xltot + xbodies (kount bodies,idet)
      endif
 2500 continue
      if ( xltot .gt. 1.0e-09 ) then
      write(44,4)uname(jbodies),(Tenchars1(jdet),jdet=1,ndet)
      write(01,41)uname(jbodies),(Tenchars1(jdet),jdet=1,ndet)
      endif
      endif
      write(44,1115) ! to APC file
      write(01,1115)
 1115 format(150('-'))

*     write the load per kilometre over all sub-catchments --------------------- 
      xltot = 0.0
      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
	if ( QTYPE (idet) .ne. 4 ) then
	xload per km = 0.0
	if ( total bodies length (idet) .gt. 0.0001 ) then
	xload per km = total bodies load (idet) /
     &total bodies length (idet)
      xload per km = amax1 ( 0.0, xload per km )
	endif
      call sort format 1 (xload per km)
      Tenchars1(idet) = valchars10
      xltot = xltot + xload per km
      endif
      enddo
      if ( xltot .gt. 1.0e-09 ) then
      write(44,3008)(Tenchars1(jdet),jdet=1,ndet)
      write(01,3082)(Tenchars1(jdet),jdet=1,ndet)
 3008 format(5x,'Total load per kilometre ...            ',5x,10a10)
 3082 format('Total load per kilometre ...            ',10a10)
      write(44,5) ! to APC file
      write(01,5)
      endif
      
*     write out the percentages ------------------------------------------------
      do 2503 idet = 1, ndet
	xwload (idet) = 0.0
 2503 continue
      ihead = 0
      do 2504 ibodies = 1, kount bodies
      jbodies = identify bodies (ibodies)
      xltot = 0.0
      do 2502 idet = 1, ndet
      Tenchars1(idet) = '      ....'
	if ( QTYPE (idet) .ne. 4 ) then
	if ( ihead .eq. 0 ) then
	write(44,92) ! to APC file
	write(01,92)
   92 format('The percentage of the total river load ',
     &'supplied by the individual sub-catchments upstream of ',
     &'this point'/150('='))
	ihead = 1
	endif

      xpload = 0.0
	if ( TGLODE2(idet,i13) .gt. 0.000001 ) then
	xpload = 100.0 * xbodies (ibodies,idet) / TGLODE2(idet,i13)
      xwload (idet) = xwload (idet) + xpload
      brakedown ( ibodies, idet, nprop + 1) = xpload
      call sort format 1 (xpload)
      Tenchars1(idet) = valchars10
      xltot = xltot + xpload
	endif
	endif
 2502 continue
      if ( xltot .gt. 1.0e-09 ) then
	write(44,4)uname(jbodies),(Tenchars1(jdet),jdet=1,ndet)
	write(01,41)uname(jbodies),(Tenchars1(jdet),jdet=1,ndet)
      endif
 2504 continue

      do idet = 1, ndet
      Tenchars1(idet) = '      ....'
	if ( QTYPE (idet) .ne. 4 ) then
      call sort format 1 (xwload (idet))
      Tenchars1(idet) = valchars10
      endif
      enddo

 	write(44,1115) ! to APC file
 	write(01,1115)
	write(44,8)(Tenchars1(jdet),jdet=1,ndet) ! to APC file
	write(01,82)(Tenchars1(jdet),jdet=1,ndet)
 	write(44,5) ! to APC file
 	write(01,5)
      
*     repeat for the breakdown of contributions to sub-catchments --------------   
*     ( 1) = 'Mean from all discharges (3,12,5,39)'
*     ( 2) = 'Added by sewage effluents (3)'
*     ( 3) = 'Intermittent discharges of sewage (12)'
*     ( 4) = 'Added by industrial discharges (5)'
*     ( 5) = 'Added by mine waters (39)         '
*     ( 6) = 'Diffuse pollution from livestock (25)'
*     ( 7) = 'Diffuse pollution from arable (27)'
*     ( 8) = 'Highway runoff (29)'
*     ( 9) = 'Urban runoff (31)'
*     (10) = 'Atmosphere deposition (33)'
*     (11) = 'Natural background (35)'
*     (12) = 'Septic tanks (37)'
*     (13) = 'Aggregate CSOs (40)'
*     (14) = 'Aggregated STWs (42)'
*     (15) = 'Diffuse mines (46)'
*     (16) = 'Birds, boats and angling (48)'
*     --------------------------------------------------------------------------
*     loop on all the types of input and source --------------------------------
      do idet = 1, MP10
      Tenchars1(idet) = '          '
      enddo

      tot12AV = 0.0
      tot1290 = 0.0
      tot1295 = 0.0
      tot1298 = 0.0
      tot1299 = 0.0
      tot12995 = 0.0
      tot12999 = 0.0
      tot03AV = 0.0
      tot0390 = 0.0
      tot0395 = 0.0
      tot0398 = 0.0
      tot0399 = 0.0
      tot03995 = 0.0
      tot03999 = 0.0
      tot05AV = 0.0
      tot0590 = 0.0
      tot0595 = 0.0
      tot0598 = 0.0
      tot0599 = 0.0
      tot05995 = 0.0
      tot05999 = 0.0
      tot39AV = 0.0
      tot3990 = 0.0
      tot3995 = 0.0
      tot3998 = 0.0
      tot3999 = 0.0
      tot39995 = 0.0
      tot39999 = 0.0
      
*     loop on the number of upstream works -------------------------------------
      do 1000 iworks = 1, kount works
      iline = 0
      jworks = identify works (iworks)
	jjreach = JREACH (jworks)
      xworksd = 0.0

*     loop on determinands .....................................................
      do 1001 idet = 1, ndet
	if ( QTYPE (idet) .eq. 4 ) goto 1001
      
*     initialise the proportion of effluent ------------------------------------
      TenAV(idet)   = '     .....' ! % mean of the total load
      TenAVT(idet)  = '     .....' ! % of the total load
      TenTE(idet)   = '     .....' ! % of the total effluent load
      Ten90(idet)   = '     .....' ! % of load for the 95-percentile in the river
      Ten95(idet)   = '     .....' ! % of load for the 95-percentile in the river
      Ten98(idet)   = '     .....' ! % of load for the 95-percentile in the river
      Ten99(idet)   = '     .....' ! % of load for the 99-percentile in the river
      Ten995(idet)  = '     .....' ! % of load for the 99-percentile in the river
      Ten999(idet)  = '     .....' ! % of load for the 99-percentile in the river
      Ten90t(idet)  = '     .....' ! total 95-percentile load in the river
      Ten95t(idet)  = '     .....' ! total 95-percentile load in the river
      Ten98t(idet)  = '     .....' ! total 95-percentile load in the river
      Ten99t(idet)  = '     .....' ! total 99-percentile load in the river
      Ten995t(idet) = '     .....' ! total 99-percentile load in the river
      Ten999t(idet) = '     .....' ! total 99-percentile load in the river

      
      if ( n148 .eq. 1 ) then ! 333333333333333333333333333333333333333333333333     
      call identify the shots for percentiles (idet) ! 3333333333333333333333333
      endif ! 333333333333333333333333333333333333333333333333333333333333333333

      xworksAV (idet) = 0.0 ! % of the total load
	if ( TGLODE2 (idet,i13) .gt. 1.0e-15 ) then
      xworksAV(idet) = 100.0*TELOADAV(iworks,idet)/TGLODE2(idet,i13)
	total effluent prop (idet) = total effluent prop (idet) 
     &+ xworksAV(idet)
      if ( JT(jworks) .eq. 12 ) then ! septic tanks (12) ! 33333333333333333333
      tot12AV = tot12AV + xworksAV(idet)
      endif
      if ( JT(jworks) .eq. 3 ) then ! sewage works (3)
      tot03AV = tot03AV + xworksAV(idet)
      endif
      if ( JT(jworks) .eq. 5 ) then ! industry (5)
      tot05AV = tot05AV + xworksAV(idet)
      endif
      if ( JT(jworks) .eq. 39 ) then ! mine waters (39) 
      tot39AV = tot39AV + xworksAV(idet)
      endif
      call sort format 2 (xworksAV(idet),TGLODE2(idet,i13))
	TenAV(idet) = valchars10
	TenAVT(idet) = valchars11
      endif ! if ( TGLODE2 (idet,i13) .gt. 1.0e-15 )
      
      xworksTE (idet) = 0.0 ! % of the total effluent load
      if ( TELODE2 (idet,i13) .gt. 1.0e-15 ) then
      xworksTE(idet) = 100.0*TELOADAV(iworks,idet)/TELODE2(idet,i13)
	total effluent load (idet) = total effluent load (idet)
     & + xworksTE(idet)
	xworksd = amax1 ( xworksd, xworksTE(idet) )
      call sort format 1 (xworksTE(idet))
	TenTE(idet) = valchars10
      endif ! if ( TELODE2 (idet,i13) .gt. 1.0e-15 )

*     33333333333333333333333333333333333333333333333333333333333333333333333333
      if ( n148 .eq. 1 .and. idet .eq. ndshot ) then ! 3333333333333333333333333
      XXLD = CMS(idet,JS90) * FMS(JS90) ! 33333333333333333333333333333333333333
      xworks (idet) = 0.0 ! % of load for the 90-percentile in the river 3333333
 	if ( XXLD .gt. 1.0e-15 ) then
      xworks(idet) = 100.0*TELOADshots(iworks,idet,JS90)/XXLD ! 3333333333333333
      if ( JT(jworks) .eq. 12 ) then ! septic tanks 3333333333333333333333333333
      tot1290 = tot1290 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 3 ) then ! sewage works 33333333333333333333333333333
      tot0390 = tot0390 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 5 ) then ! industry (5) 33333333333333333333333333333
      tot0590 = tot0590 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 39 ) then ! mine waters (39) 333333333333333333333333
      tot3990 = tot3990 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      call sort format 2 (xworks(idet),XXLD)
	Ten90(idet) = valchars10 ! 33333333333333333333333333333333333333333333333
	Ten90t(idet) = valchars11 ! 3333333333333333333333333333333333333333333333
      endif ! if ( XXLD .gt. 1.0e-15 ) ! 333333333333333333333333333333333333333
      
      XXLD = CMS(idet,JS95) * FMS(JS95) ! 33333333333333333333333333333333333333
      xworks (idet) = 0.0 ! % of load for the 95-percentile in the river 3333333
 	if ( XXLD .gt. 1.0e-15 ) then
      xworks(idet) = 100.0*TELOADshots(iworks,idet,JS95)/XXLD ! 3333333333333333
      if ( JT(jworks) .eq. 12 ) then ! septic tanks 3333333333333333333333333333
      tot1295 = tot1295 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 3 ) then ! sewage works 33333333333333333333333333333
      tot0395 = tot0395 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 5 ) then ! industry (5) 33333333333333333333333333333
      tot0595 = tot0595 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 39 ) then ! mine waters (39) 333333333333333333333333
      tot3995 = tot3995 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      call sort format 2 (xworks(idet),XXLD)
	Ten95(idet) = valchars10 ! 33333333333333333333333333333333333333333333333
	Ten95t(idet) = valchars11 ! 3333333333333333333333333333333333333333333333
      endif ! if ( XXLD .gt. 1.0e-15 )! 3333333333333333333333333333333333333333

      XXLD = CMS(idet,JS98) * FMS(JS98) ! 33333333333333333333333333333333333333
      xworks (idet) = 0.0 ! % of load for the 98-percentile in the river 3333333
 	if ( XXLD .gt. 1.0e-15 ) then ! 333333333333333333333333333333333333333333
      xworks(idet) = 100.0*TELOADshots(iworks,idet,JS98)/XXLD ! 3333333333333333
      if ( JT(jworks) .eq. 12 ) then ! septic tanks 3333333333333333333333333333
      tot1298 = tot1298 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 3 ) then ! sewage works 33333333333333333333333333333
      tot0398 = tot0398 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 5 ) then ! industry (5) 33333333333333333333333333333
      tot0598 = tot0598 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 39 ) then ! mine waters (39) 333333333333333333333333
      tot3998 = tot3998 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      call sort format 2 (xworks(idet),XXLD)
	Ten98(idet) = valchars10 ! 33333333333333333333333333333333333333333333333
	Ten98t(idet) = valchars11 ! 3333333333333333333333333333333333333333333333
      endif ! if ( XXLD .gt. 1.0e-15 ) 33333333333333333333333333333333333333333

      XXLD = CMS(idet,JS99) * FMS(JS99) ! 33333333333333333333333333333333333333
      xworks (idet) = 0.0 ! % of load for the 99-percentile in the river 3333333
 	if ( XXLD .gt. 1.0e-15 ) then ! 333333333333333333333333333333333333333333
      xworks(idet) = 100.0*TELOADshots(iworks,idet,JS99)/XXLD ! 3333333333333333
      if ( JT(jworks) .eq. 12 ) then ! septic tanks 3333333333333333333333333333
      tot1299 = tot1299 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 3 ) then ! sewage works 33333333333333333333333333333
      tot0399 = tot0399 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 5 ) then ! industry (5) 33333333333333333333333333333
      tot0599 = tot0599 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 39 ) then ! mine waters (39) 333333333333333333333333
      tot3999 = tot3999 + xworks(idet) ! 333333333333333333333333333333333333333
      endif
      call sort format 2 (xworks(idet),XXLD)
	Ten99(idet) = valchars10 ! 33333333333333333333333333333333333333333333333
	Ten99t(idet) = valchars11 ! 3333333333333333333333333333333333333333333333
      endif ! if ( XXLD .gt. 1.0e-15 ) 33333333333333333333333333333333333333333
      
      XXLD = CMS(idet,JS995) * FMS(JS995) ! 333333333333333333333333333333333333
      xworks (idet) = 0.0 ! % of load for the 99.5-percentile in the river 33333
 	if ( XXLD .gt. 1.0e-15 ) then
      xworks(idet) = 100.0*TELOADshots(iworks,idet,JS995)/XXLD ! 333333333333333
      if ( JT(jworks) .eq. 12 ) then ! septic tanks 3333333333333333333333333333
      tot12995 = tot12995 + xworks(idet) ! 3333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 3 ) then ! sewage works 33333333333333333333333333333
      tot03995 = tot03995 + xworks(idet) ! 3333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 5 ) then ! industry (5) 33333333333333333333333333333
      tot05995 = tot05995 + xworks(idet) ! 3333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 39 ) then ! mine waters (39) 333333333333333333333333
      tot39995 = tot39995 + xworks(idet) ! 3333333333333333333333333333333333333
      endif
      call sort format 2 (xworks(idet),XXLD)
	Ten995(idet) = valchars10 ! 3333333333333333333333333333333333333333333333
	Ten995t(idet) = valchars11 ! 333333333333333333333333333333333333333333333
      endif ! if ( XXLD .gt. 1.0e-15 ) ! 333333333333333333333333333333333333333

      XXLD = CMS(idet,JS999) * FMS(JS999) ! 333333333333333333333333333333333333
      xworks (idet) = 0.0 ! % of load for the 99.9-percentile in the river 33333
 	if ( XXLD .gt. 1.0e-15 ) then
      xworks(idet) = 100.0*TELOADshots(iworks,idet,JS999)/XXLD ! 333333333333333
      if ( JT(jworks) .eq. 12 ) then ! septic tanks 3333333333333333333333333333
      tot12999 = tot12999 + xworks(idet) ! 3333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 3 ) then ! sewage works 33333333333333333333333333333
      tot03999 = tot03999 + xworks(idet) ! 3333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 5 ) then ! industry (5) 33333333333333333333333333333
      tot05999 = tot05999 + xworks(idet) ! 3333333333333333333333333333333333333
      endif
      if ( JT(jworks) .eq. 39 ) then ! mine waters (39) 333333333333333333333333
      tot39999 = tot39999 + xworks(idet) ! 3333333333333333333333333333333333333
      endif
      call sort format 2 (xworks(idet),XXLD)
	Ten999(idet) = valchars10 ! 3333333333333333333333333333333333333333333333
	Ten999t(idet) = valchars11 ! 333333333333333333333333333333333333333333333
      endif ! if ( XXLD .gt. 1.0e-15 ) ! 333333333333333333333333333333333333333
      endif ! if ( n148 .eq. 1 .and. idet .eq. ndshot ) ! 3333333333333333333333
*     33333333333333333333333333333333333333333333333333333333333333333333333333

*     check that the percentage load does not exceed 100 -----------------------
	if ( xworksTE (idet) .gt. 100.0001 ) then
	call change colour of text (20) ! bright red
	write( *,7300)uname(jworks)
      call set screen text colour
	write(01,7300)uname(jworks)
	write(09,7300)uname(jworks)
	write(33,7300)uname(jworks) ! to ERR file
      write(40,7300)uname(jworks) ! to APT file
 7300 format(/77('-')/
     &'Error in the calculation of loads attributable to ',
     &'effluents ... '/
     &'Percentage exceeds 100.0 ... for: ',a37/77('-')) 
	write(01,7000)TELOADAV(iworks,idet),dname(idet)
	write(09,7000)TELOADAV(iworks,idet),dname(idet)
      write(40,7000)TELOADAV(iworks,idet),dname(idet) ! to APT file
	write(33,7000)TELOADAV(iworks,idet),dname(idet) ! to ERR file
 7000 format('   Load from works =',f15.6,' for ',a11)
	write(01,7001)TELODE2(idet,i13)
	write(09,7001)TELODE2(idet,i13)
      write(40,7001)TELODE2(idet,i13) ! to APT file
	write(33,7001)TELODE2(idet,i13) ! to ERR file
 7001 format('    Total net load =',f15.6)
	write(01,7002)xworksTE(idet)
	write(09,7002)xworksTE(idet)
      write(40,7002)xworksTE(idet) ! to APT file
	write(33,7002)xworksTE(idet) ! to ERR file
 7002 format('        Percentage =',f15.6/77('-'))
      call stop
      endif ! if ( xworksTE (idet) .gt. 100.0001 )
 1001 continue ! end of loop on determinands ...................................

 	if ( xworksd .gt. 0.00001 ) then
      iline = 1
 	if ( ihead .ne. 2 ) ihead = 1
 	endif ! if ( xworksd .gt. 0.00001 )

*     write heading -----------------------------------------------------------2
      if ( ihead .eq. 1 ) then
      if ( nobigout .le. 0 ) then
      
      ihead = 2
      endif ! if ( nobigout .le. 0 )
      endif ! if ( ihead .eq. 1 )

      if ( iline .eq. 1 ) then
      iline = 0
      if ( nobigout .le. 0 ) then
      if ( n148 .eq. 1 ) then ! 333333333333333333333333333333333333333333333333
      write(41,4194)uname(jworks),TenAV(ndshot),Ten90(ndshot), ! 333333333333333
     &Ten95(ndshot),Ten98(ndshot), ! 3333333333333333333333333333333333333333333
     &Ten99(ndshot),Ten995(ndshot),Ten999(ndshot) ! 3333333333333333333333333333
 4194 format(a40,10a10,1x,i6) ! 333333333333333333333333333333333333333333333333
      endif ! if ( n148 .eq. 1 ) 33333333333333333333333333333333333333333333333
      endif ! if ( nobigout .le. 0 )
      
*     do idet = 1, ndet
*	if ( QTYPE (idet) .ne. 4 ) then
*     write(110+idet,9099)GIScode(feeture),unamex,uname(jworks),
*    &dname(idet),xworksAV(idet),xworksTE(idet),jxtype
*9099 format(a40,',',a40,',',a40,',',
*    &'% Annual contribution from individual works',',',a11,2(',',
*    &1pe11.4),',,,,,,,,,,,,',i4,', 3,12, 5,39')
*     endif ! if ( QTYPE (idet) .ne. 4 )
*     enddo ! idet = 1, ndet
*     do idet = 1, ndet
*	if ( QTYPE (idet) .ne. 4 ) then
*     if ( nobigout .le. 0 ) then
*     write(140+idet,44)uname(jworks),TenAV(idet),TenTE(idet)
*  44 format('Net % load from: ',a40,a10,'  or ',a10,' (expressed ',
*    &'as % of the total of the net discharge load)')
*     endif ! if ( nobigout .le. 0 )
*     endif ! if ( QTYPE (idet) .ne. 4 )
*	enddo ! idet = 1, ndet
      endif ! if ( iline .eq. 1 )

 1000 continue ! end of loop on the number of works
  
      if ( n148 .eq. 1 ) then ! 555555555555555555555555555555555555555555555555
      write(41,2246)TenAVt(ndshot),Ten90t(ndshot),Ten95t(ndshot), ! 555555555555
     &Ten98t(ndshot),Ten99t(ndshot),Ten995t(ndshot),Ten999t(ndshot) ! 5555555555
 2246 format(150('-')/
     &'Total load in the percentile            ',10a10/150('-'))
      write(41,3546)nameprop(2),tot03av,tot0390,tot0395, ! to a95 file 555555555
     &tot0398,tot0399,tot03995,tot03999
 3546 format('Total % from ',a27,10f10.2) ! 555555555555555555555555555555555555
      write(41,3546)nameprop(3),tot12av,tot1290,tot1295, ! 555555555555555555555
     &tot1298,tot1299,tot12995,tot12999 ! 55555555555555555555555555555555555555
      write(41,3546)nameprop(4),tot05av,tot0590,tot0595, ! 555555555555555555555
     &tot0598,tot0599,tot05995,tot05999 ! 55555555555555555555555555555555555555
      write(41,3546)nameprop(5),tot39av,tot3990,tot3995, ! to a95 file 555555555
     &tot3998,tot3999,tot39995,tot39999 ! 55555555555555555555555555555555555555
      endif ! if ( n148 .eq. 1 ) ! 555555555555555555555555555555555555555555555
      
      do 6000 ip = 1, nprop ! ##################################################
      itrap2 = 0
*     initialise the totals ----------------------------------------------------
      do idet = 1, mp10
      total bodies load (idet) = 0.0
      total bodies prop (idet) = 0.0
      total bodies length (idet) = 0.0
      enddo      

      totAV(ip) = 0.0
      tot90(ip) = 0.0
      tot95(ip) = 0.0
      tot98(ip) = 0.0
      tot99(ip) = 0.0
      tot995(ip) = 0.0
      tot999(ip) = 0.0

*     initialise the load in each sub-catchment (xbodies) for this type of input
      do 4507 ibodies = 1, kount bodies
      do 4508 idet = 1, ndet
      if ( QTYPE (idet) .ne. 4 ) then

      xworksAV (idet) = 0.0 ! % of the total load ! 6666666666666666666666666666
      
      if ( n148 .eq. 1 ) write(41,8000)ip,idet,ibodies, ! 5555555555555555555555
     &TWLOADSAPP (ibodies,idet,i13,ip), ! 55555555555555555555555555555555555555
     &TGLODE2 (idet,i13)
      if ( n148 .eq. 1 ) write(01,8000)ip,idet,ibodies, ! 5555555555555555555555
     &TWLOADSAPP (ibodies,idet,i13,ip), ! 55555555555555555555555555555555555555
     &TGLODE2 (idet,i13)
 8000 format('ip =',i4,' idet =',i3,' ibodies =',i3,2f15.5)
      
	if ( TGLODE2 (idet,i13) .gt. 1.0e-15 ) then ! check total net load 6666666
      xworksAV(idet) = 100.0*TWLOADSAPP (ibodies,idet,i13,ip)/
     &TGLODE2(idet,i13)
	total bodies prop (idet) = total bodies prop (idet) 
     &+ xworksAV(idet)
      totav(ip) = totAV(ip) + xworksAV(idet)
      call sort format 2 (xworksAV(idet),TGLODE2(idet,i13))
	TenAV(idet) = valchars10
	TenAVT(idet) = valchars11
      endif ! if ( TGLODE2 (idet,i13) .gt. 1.0e-15 ) 666666666666666666666666666

      xworksTE (idet) = 0.0 ! % of the total effluent load
      if ( TELODE2 (idet,i13) .gt. 1.0e-15 ) then
      xworksTE(idet) = 100.0*TWLOADSAPP (ibodies,idet,i13,ip)/
     &TELODE2(idet,i13)
	total bodies load (idet) = total bodies load (idet)
     & + xworksTE(idet)
	xworksd = amax1 ( xworksd, xworksTE(idet) )
      call sort format 1 (xworksTE(idet))
	TenTE(idet) = valchars10
      endif ! if ( TELODE2 (idet,i13) .gt. 1.0e-15 ) ! 6666666666666666666666666

 
*     TDLOADshots (NUW,MP10,MS,nprop)
      if ( n148 .eq. 1 .and. idet .eq. ndshot ) then ! 5555555555555555555555555
      XXLD = CMS(idet,JS90) * FMS(JS90) ! 55555555555555555555555555555555555555
      xworks (idet) = 0.0 ! % of load for the 90-percentile in the river 5555555
 	if ( XXLD .gt. 1.0e-15 ) then
      xworks(idet) = 100.0*TDLOADshots(ibodies,idet,JS90,ip)/XXLD ! 555555555555
      tot90(ip) = tot90(ip) + xworks(idet) ! 55555555555555555555555555555555555
      call sort format 2 (xworks(idet),XXLD)
	Ten90(idet) = valchars10 ! 55555555555555555555555555555555555555555555555
	Ten90t(idet) = valchars11 ! 5555555555555555555555555555555555555555555555
      endif ! if ( XXLD .gt. 1.0e-15 ) ! 555555555555555555555555555555555555555
      XXLD = CMS(idet,JS90) * FMS(JS90) ! 55555555555555555555555555555555555555
 
      Ten90(idet)   = '     .....' ! % of load for the 95-percentile in the river
      Ten95(idet)   = '     .....' ! % of load for the 95-percentile in the river
      Ten98(idet)   = '     .....' ! % of load for the 95-percentile in the river
      Ten99(idet)   = '     .....' ! % of load for the 99-percentile in the river
      Ten995(idet)  = '     .....' ! % of load for the 99-percentile in the river
      Ten999(idet)  = '     .....' ! % of load for the 99-percentile in the river

      xworks (idet) = 0.0 ! % of load for the 95-percentile in the river 5555555
 	if ( XXLD .gt. 1.0e-15 ) then
      xworks(idet) = 100.0*TDLOADshots(ibodies,idet,JS95,ip)/XXLD ! 555555555555
      tot95(ip) = tot95(ip) + xworks(idet) ! 55555555555555555555555555555555555
      call sort format 2 (xworks(idet),XXLD)
	Ten95(idet) = valchars10 ! 55555555555555555555555555555555555555555555555
	Ten95t(idet) = valchars11 ! 5555555555555555555555555555555555555555555555
      endif ! if ( XXLD .gt. 1.0e-15 ) ! 555555555555555555555555555555555555555

      xworks (idet) = 0.0 ! % of load for the 98-percentile in the river 5555555
 	if ( XXLD .gt. 1.0e-15 ) then
      xworks(idet) = 100.0*TDLOADshots(ibodies,idet,JS98,ip)/XXLD ! 555555555555
      tot98(ip) = tot98(ip) + xworks(idet) ! 55555555555555555555555555555555555
      call sort format 2 (xworks(idet),XXLD)
	Ten98(idet) = valchars10 ! 55555555555555555555555555555555555555555555555
	Ten98t(idet) = valchars11 ! 5555555555555555555555555555555555555555555555
      endif ! if ( XXLD .gt. 1.0e-15 ) ! 555555555555555555555555555555555555555

      xworks (idet) = 0.0 ! % of load for the 99-percentile in the river 5555555
 	if ( XXLD .gt. 1.0e-15 ) then
      xworks(idet) = 100.0*TDLOADshots(ibodies,idet,JS99,ip)/XXLD ! 555555555555
      tot99(ip) = tot99(ip) + xworks(idet) ! 55555555555555555555555555555555555
      call sort format 2 (xworks(idet),XXLD)
	Ten99(idet) = valchars10 ! 55555555555555555555555555555555555555555555555
	Ten99t(idet) = valchars11 ! 5555555555555555555555555555555555555555555555
      endif ! if ( XXLD .gt. 1.0e-15 ) ! 555555555555555555555555555555555555555

      xworks (idet) = 0.0 ! % of load for the 99.5-percentile in the river 55555
 	if ( XXLD .gt. 1.0e-15 ) then
      xworks(idet) = 100.0*TDLOADshots(ibodies,idet,JS995,ip)/XXLD ! 55555555555
      tot995(ip) = tot995(ip) + xworks(idet) ! 555555555555555555555555555555555
      call sort format 2 (xworks(idet),XXLD)
	Ten995(idet) = valchars10 ! 5555555555555555555555555555555555555555555555
	Ten995t(idet) = valchars11 ! 555555555555555555555555555555555555555555555
      endif ! if ( XXLD .gt. 1.0e-15 ) ! 555555555555555555555555555555555555555

      xworks (idet) = 0.0 ! % of load for the 99.9-percentile in the river 55555
 	if ( XXLD .gt. 1.0e-15 ) then
      xworks(idet) = 100.0*TDLOADshots(ibodies,idet,JS999,ip)/XXLD ! 55555555555
      tot999(ip) = tot999(ip) + xworks(idet) ! 555555555555555555555555555555555
      call sort format 2 (xworks(idet),XXLD)
	Ten999(idet) = valchars10 ! 5555555555555555555555555555555555555555555555
	Ten999t(idet) = valchars11 ! 555555555555555555555555555555555555555555555
      endif ! if ( XXLD .gt. 1.0e-15 ) ! 555555555555555555555555555555555555555

      endif ! if ( n148 .eq. 1 .and. idet .eq. ndshot ) 555555555555555555555555

          
      brakedown ( ibodies, idet, ip) = 0.0 ! RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
      xbodies (ibodies,idet) = 0.0
      if ( TGLODE2 (idet,i13) .gt. 1.0e-15 ) then
      xbodies (ibodies,idet) = TWLOADSapp (ibodies,idet,i13,ip)
      total bodies load (idet) = total bodies load (idet) 
     & + xbodies (ibodies,idet)
      if ( xbodies (ibodies,idet) .gt. 0.000001 ) itrap2 = 1
      endif
      endif ! if ( QTYPE (idet) .ne. 4 ) RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
 4508 continue ! ibodies = 1, kount bodies
 4507 continue ! idet = 1, ndet

*     write the headings for each sub-catchment for this type of input load ----
      ihead = 0
      if ( itrap2 .eq. 1 ) then
      do 4000 ibodies = 1, kount bodies
      jbodies = identify bodies (ibodies)
      do 4513 idet = 1, ndet
          
      Tenchars1(idet) = '      ----'
	if ( QTYPE (idet) .ne. 4 ) then
*     write heading for this type of input -------------------------------------
	if ( ihead .eq. 0 ) then
	write(44,4012)nameprop(ip)
	write(01,4012)nameprop(ip)
 4012 format(/150('=')/'The river load in individual sub-catchments ',
     &'upstream of this point from: ',a37/150('='))
      ihead = 1
*     write the heading to the APP files for this type of input ----------------
*     if ( nobigout .le. 0 ) then
*     write(140+idet,49)
*  49 format(/150('='))
*     endif ! if ( nobigout .le. 0 )
      endif ! if ( ihead .eq. 0 ) 
*     --------------------------------------------------------------------------
      call sort format 1 (xbodies (ibodies,idet))
      Tenchars1(idet) = valchars10
      endif ! if ( QTYPE (idet) .ne. 4 )
 4513 continue ! idet = 1, ndet

      write(44,4)uname(jbodies),(Tenchars1(jdet),jdet=1,MP10) ! to file APT
      write(01,41)uname(jbodies),(Tenchars1(jdet),jdet=1,MP10)
      
      do 4600 idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
	total bodies length (idet) = total bodies length (idet) + 
     &TWlength(ibodies)
	xload per km = 0.0
	if ( TWlength(ibodies) .gt. 0.0001 ) then
	xload per km = xbodies (ibodies,idet) / TWlength(ibodies)
      xload per km = amax1 ( 0.0, xload per km )
	endif
      xper = 0.0
	if ( TGLODE2(idet,i13) .gt. 0.0001 ) then
      xper = 100.0 * xbodies (ibodies,idet) / TGLODE2(idet,i13)
      xper = amax1 ( 0.0, xper )
      endif
      call sort format 2 (xload per km, xper)
      if ( n148 .eq. 1 ) write(140+idet,4042)uname(jbodies), ! 33333333333333333
     &Tenchars1(idet),xper,
     &TWlength(ibodies),valchars10,nameprop(ip)
 4042 format('Load from: ',a40,6x,a10,'(',f7.2,' %)',
     &' over',f7.1,' km (',a10,' per km) ... ',a37) ! 33333333333333333333333333
     
      endif ! if ( QTYPE (idet) .ne. 4 )
 4600 continue ! idet = 1, ndet
 4000 continue ! ibodies = 1, kount bodies
      endif ! if ( itrap2 .eq. 1 )
      
*     write the data for each sub-catchment for this type of input load --------
*     write to the CSV file picked up by SAGIS ---------------------------------
      do 4800 ibodies = 1, kount bodies
      jbodies = identify bodies (ibodies)
      do 4680 idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
	xload per km = 0.0
	if ( TWlength(ibodies) .gt. 0.0001 ) then
	xload per km = xbodies (ibodies,idet) / TWlength(ibodies)
      xload per km = amax1 ( 0.0, xload per km )
	endif
      xper = 0.0
	if ( TGLODE2(idet,i13) .gt. 0.0001 ) then
      xper = 100.0 * xbodies (ibodies,idet) / TGLODE2(idet,i13)
      xper = amax1 ( 0.0, xper )
      endif
      write(110+idet,6643)GIScode(feeture),unamex,uname(jbodies),
     &nameprop(ip),xbodies (ibodies,idet),xper,TWlength(ibodies),
     &xload per km,jxtype
 6643 format(a40,',',a40,',',a16,',',
     &'Annual load from sub-catchment',
     &',',a37,4(',',1pe11.4),',,,,,,,,,,',i4,',24')
      endif
 4680 continue
 4800 continue ! ibodies = 1, kount bodies
      
*     write the data for each sub-catchment for this type of input load --------
*     write to the CSV file picked up by SAGIS ---------------------------------
      if ( itrap2 .eq. 1 ) then
      do 4880 idet = 1, ndet
      Tenchars1(idet) = '      ....'
	if ( QTYPE (idet) .ne. 4 ) then

	xload per km = 0.0
	if ( total bodies length (idet) .gt. 0.0001 ) then
	xload per km = total bodies load (idet) /
     &total bodies length (idet)
      xload per km = amax1 ( 0.0, xload per km )
	endif
      xper = 0.0
	if ( TGLODE2(idet,i13) .gt. 0.0001 ) then
      xper = 100.0 * total bodies load (idet) / TGLODE2(idet,i13)
      endif

      call sort format 1 (total bodies load (idet))
      Tenchars1(idet) = valchars10
      
      call sort format 2 (xload per km, xper)
      if ( n148 .eq. 1 ) write(140+idet,4812)Tenchars1(idet),xper, ! 33333333333
     &total bodies length (idet),valchars10,nameprop(ip)
 4812 format(150('-')/'Total load from all sub-catchments',23x,a10,
     &'(',f7.2,' %)',' over',f7.1,' km (',a10,' per km) ... ',a37/
     &150('-'))

      endif ! if ( QTYPE (idet) .ne. 4 )
 4880 continue ! idet = 1, ndet
      endif ! if ( itrap2 .eq. 1 )      
      
      do 6880 idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then

	xload per km = 0.0
	if ( total bodies length (idet) .gt. 0.0001 ) then
	xload per km = total bodies load (idet) /
     &total bodies length (idet)
      xload per km = amax1 ( 0.0, xload per km )
	endif
      xper = 0.0
	if ( TGLODE2(idet,i13) .gt. 0.0001 ) then
      xper = 100.0 * total bodies load (idet) / TGLODE2(idet,i13)
      xper = amax1 ( 0.0, xper )
      endif

      write(110+idet,4813)GIScode(feeture),unamex,nameprop(ip),
     &total bodies load (idet),xper,
     &total bodies length (idet),xload per km,jxtype
 4813 format(a40,',',a40,',All sub-catchments',
     &',Total load from all sub-catchments',',',
     &a37,4(',',1pe11.4),',,,,,,,,,,',i4,',24')
      endif ! if ( QTYPE (idet) .ne. 4 )
 6880 continue ! idet = 1, ndet

      
      
*     write the breakdown over all sub-catchments ------------------------------
      do 4700 idet = 1, ndet

      if ( n148 .eq. 1 ) then ! 555555555555555555555555555555555555555555555555     
      call identify the shots for percentiles (idet) ! 5555555555555555555555555
      endif ! if ( n148 .eq. 1 ) 55555555555555555555555555555555555555555555555

      Tenchars1(idet) = '      ----'
	if ( QTYPE (idet) .ne. 4 ) then
      call sort format 1 (total bodies load (idet))
      Tenchars1(idet) = valchars10
      endif
 4700 continue
      if ( itrap2 .eq. 1 ) then
 	write(44,1115)
 	write(01,1115)
      write(44,4008)nameprop(ip),(Tenchars1(jdet),jdet=1,ndet)
      write(01,4082)nameprop(ip),(Tenchars1(jdet),jdet=1,ndet)
 4008 format(5x,'Load: ',a37,2x,10a10)
 4082 format('Load: ',a34,10a10)
      write(44,5)
      write(01,5)
      endif

*     write the monthly structures of breakdown to the MON and CSV files -------
      do 3401 idet = 1, ndet
      if ( xbodies (ibodies,idet) .lt. 0.0001 ) goto 3401
      Tenchars1(idet) = '      ----'
	if ( QTYPE (idet) .ne. 4 ) then

	xload per km = 0.0
	if ( total bodies length (idet) .gt. 0.0001 ) then
	xload per km = total bodies load (idet) /
     &total bodies length (idet)
      xload per km = amax1 ( 0.0, xload per km )
	endif

      call sort format 1 (total bodies load (idet))
      Tenchars1(idet) = valchars10
      call sort format 1 (xload per km)

      if ( n148 .eq. 1 ) write(140+idet,5812)Tenchars1(idet), ! 3333333333333333
     &total bodies length (idet),
     &valchars10,nameprop(ip)
 5812 format(150('-')/'Load from sub-catchments',33x,a10,
     &'    over',f7.1,' km (',a10,' per km)',1x,a37)
     
      write(110+idet,5813)GIScode(feeture),unamex,nameprop(ip),
     &total bodies load (idet),xper,
     &total bodies length (idet),xload per km,jxtype
 5813 format(a40,',',a40,',Load from sub-catchments',',',
     &'Total load from all sub-catchments',
     &',',a37,4(',',1pe11.4),',,,,,,,,,,',i4,',24')
     
      if ( n148 .eq. 1 ) write(140+idet,5872),nameprop(ip), ! 333333333333333333
     &total bodies load (idet)
 5872 format(150('-')/'Load from ',a37,9x,f11.4)
      if ( n148 .eq. 1 ) write(140+idet,5) ! 33333333333333333333333333333333333
      endif
 3401 continue

*     write the breakdown of load per kilometre --------------------------------  
      ihead = 0
      if ( itrap2 .eq. 1 ) then
      do 4001 ibodies = 1, kount bodies
      jbodies = identify bodies (ibodies)
      do 4801 idet = 1, ndet
      Tenchars1(idet) = '      ----'
	if ( QTYPE (idet) .ne. 4 ) then
	if ( ihead .eq. 0 ) then
	write(44,4062)nameprop(ip)
	write(01,4062)nameprop(ip)
 4062 format('The load supplied per kilometre by water ',
     &'bodies from: ',a37/150('='))
      ihead = 1
      endif
      xwrite = 0.0
      if ( TWlength(ibodies) .gt. 0.0001 ) then
      xwrite = xbodies(ibodies,idet)/TWlength(ibodies)
      endif
      call sort format 1 (amax1( 0.0, xwrite))
      Tenchars1(idet) = valchars10
      endif
 4801 continue
      write(44,4)uname(jbodies),(Tenchars1(jdet),jdet=1,MP10)
      write(01,41)uname(jbodies),(Tenchars1(jdet),jdet=1,MP10)
 4001 continue
      endif

      if ( kount bodies .gt. 99999 ) then
      do 5500 idet = 1, ndet
      if ( xbodies (ibodies,idet) .lt. 0.0001 ) goto 5500
      do 5501 ibody = 1, kount bodies - 1
      xbodies (kount bodies,idet) = xbodies (kount bodies,idet) 
     &                            - xbodies (ibody,idet) 
 5501 continue
      call sort format 1 (xbodies (kount bodies,idet))
      Tenchars1(idet) = valchars10
 5500 continue
      if ( itrap2 .eq. 1 ) then
      write(44,4)uname(jbodies),(Tenchars1(jdet),jdet=1,ndet)
      write(01,41)uname(jbodies),(Tenchars1(jdet),jdet=1,ndet)
      endif
      endif

*     write the load per kilometre for breakdowns over all sub-catchments ------
      do idet = 1, ndet
      Tenchars1(idet) = '      ----'
	if ( QTYPE (idet) .ne. 4 ) then
	xload per km = 0.0
	if ( total bodies length (idet) .gt. 0.0001 ) then
	xload per km = total bodies load (idet) /
     &total bodies length (idet)
      xload per km = amax1 ( 0.0, xload per km )
	endif
      call sort format 1 (xload per km)
      Tenchars1(idet) = valchars10
      endif
      enddo
      if ( itrap2 .eq. 1 ) then
 	write(44,1115)
 	write(01,1115)
      write(44,3208)(Tenchars1(jdet),jdet=1,ndet)
      write(01,3282)(Tenchars1(jdet),jdet=1,ndet)
 3208 format(5x,'Load per kilometre ... ',22x,10a10)
 3282 format('Load per kilometre ... ',17x,10a10)
      endif
      
*     write out the percentages of the breakdown supplied by sub-catchments ----
      do 5503 idet = 1, ndet
	xwload (idet) = 0.0
 5503 continue
      ihead = 0
      if ( itrap2 .eq. 1 ) then
      do 5504 ibodies = 1, kount bodies
      jbodies = identify bodies (ibodies)
      do 5502 idet = 1, ndet
      Tenchars1(idet) = '      ----'
	if ( QTYPE (idet) .ne. 4 ) then
	if ( ihead .eq. 0 ) then
	write(44,5092)nameprop(ip)
	write(01,5092)nameprop(ip)
 5092 format(150('=')/'The percentage of the river load ',
     &'supplied by sub-catchments from: ',a37/150('='))
	ihead = 1
	endif

      xpload = 0.0
	if ( total bodies load (idet) .gt. 0.000001 ) then
	xpload = 100.0 * xbodies (ibodies,idet) / total bodies load (idet)
      xwload (idet) = xwload (idet) + xpload 
      endif ! if ( total bodies load (idet) .gt. 0.000001 )
      call sort format 1 (xpload)
      Tenchars1(idet) = valchars10
      
      xpload = 0.0
	if ( TGLODE2(idet,i13) .gt. 0.000001 ) then
	xpload = 100.0 * xbodies (ibodies,idet) / TGLODE2(idet,i13)
      brakedown ( ibodies, idet, ip) = amax1 ( 0.0, xpload )
      endif ! if ( TGLODE2(idet,i13) .gt. 0.000001 )
	endif ! if ( QTYPE (idet) .ne. 4 )
      
 5502 continue ! idet = 1, ndet
      if ( itrap2 .eq. 1 ) then
	write(44,4)uname(jbodies),(Tenchars1(jdet),jdet=1,ndet)
	write(01,41)uname(jbodies),(Tenchars1(jdet),jdet=1,ndet)
      endif
 5504 continue ! ibodies = 1, kount bodies
      endif

      do 4559 idet = 1, ndet
      Tenchars1(idet) = '      ----'
	if ( QTYPE (idet) .ne. 4 ) then
      call sort format 1 (xwload (idet))
      Tenchars1(idet) = valchars10
      endif
 4559 continue

      if ( itrap2 .eq. 1 ) then
 	write(44,1115) ! to APC file
 	write(01,1115)
	write(44,4108)nameprop(ip),(Tenchars1(jdet),jdet=1,ndet)
	write(01,4182)nameprop(ip),(Tenchars1(jdet),jdet=1,ndet)
 4108 format(5x,'% Load: ',a37,10a10)
 4182 format('% Load: ',a32,10a10)
 	write(44,5) ! to APC file
 	write(01,5)
      endif
 6000 continue ! ip = 1, nprop #################################################
      
      do idet = 1, ndet
	if ( QTYPE (idet) .ne. 4 ) then
      
      ihedd = 1
    
      do ip = 2, nprop + 1
      braketotal(ip) = 0.0  
      enddo
      grund total1 = 0.0
      grund total2 = 0.0
      
      do ibodies = 1, kount bodies
      rowtotal = 0.0
      jbodies = identify bodies (ibodies)
      do ip = 2, nprop + 1
      braketotal(ip) = braketotal(ip) + brakedown(ibodies,idet,ip)
      enddo
      do ip = 2, 16
      rowtotal = rowtotal + brakedown(ibodies,idet,ip) 
      enddo
      grund total = grund total + rowtotal 
      x1 = amax1(0.0,brakedown(ibodies,idet,nprop+1)-rowtotal)
      x2 = x1 + rowtotal
      if ( x2 .gt. 1.0e-9 ) then
      
      if ( ihedd .eq. 1 ) then
      ihedd = 0
      
      if ( idet .eq. ndetfirst ) then
      write(01,7749)
 7749 format(/150('=')/'Percentage breakdown of sources of ',
     &'pollution ... '/150('='))
      write(40,7749) ! to APT file
      
      write(01,7780)nameprop(02),nameprop(10),nameprop(18)
      write(01,7781)nameprop(03),nameprop(11),nameprop(19)
      write(01,7782)nameprop(04),nameprop(12),nameprop(20)
      write(01,7783)nameprop(05),nameprop(13),nameprop(21)
      write(01,7784)nameprop(06),nameprop(14),nameprop(22)
      write(01,7785)nameprop(07),nameprop(15)
      write(01,7786)nameprop(08),nameprop(16)
      write(01,7787)nameprop(09)
 7780 format(4x,' A ',a35,4x,' I ',a35,4x,' P ',a35)
 7781 format(4x,' B ',a35,4x,' J ',a35,4x,' Q ',a35)
 7782 format(4x,' C ',a35,4x,' K ',a35,4x,' R ',a35)
 7783 format(4x,' D ',a35,4x,' L ',a35,4x,' S ',a35)
 7784 format(4x,' E ',a35,4x,' M ',a35,4x,' T ',a35)
 7785 format(4x,' F ',a35,4x,' N ',a35,4x,' U ','Sub-catchment total')
 7786 format(4x,' G ',a35,4x,' O ',a35)
 7787 format(4x,' H ',a35,4x,' * ','TOTAL',30x)
      write(40,7780)nameprop(02),nameprop(10),nameprop(18)
      write(40,7781)nameprop(03),nameprop(11),nameprop(19)
      write(40,7782)nameprop(04),nameprop(12),nameprop(20)
      write(40,7783)nameprop(05),nameprop(13),nameprop(21)
      write(40,7784)nameprop(06),nameprop(14),nameprop(22)
      write(40,7785)nameprop(07),nameprop(15)
      write(40,7786)nameprop(08),nameprop(16)
      write(40,7787)nameprop(09)
      endif

      if ( n148 .eq. 1 ) then ! 333333333333333333333333333333333333333333333333
      write(140+idet,7149)dname(idet) ! 3333333333333333333333333333333333333333
 7149 format('Percentage breakdown of sources of ',
     &'pollution for ',a11/150('-'))
      write(140+idet,7173)(ip,nameprop(ip),ip=2,nprop-1)
 7173 format(i6,1x,a28,i6,1x,a28,i6,1x,a28,i6,1x,a28)
      write(140+idet,7171)nameprop(ip)
 7171 format(4x,'22 ',a28,4x,'23 Sub-catchment total',13x,
     &'24 Other contributions'/150('-'))
      endif ! if ( n148 .eq. 1 ) 33333333333333333333333333333333333333333333333

      write(01,7251)dname(idet)
      write(40,7251)dname(idet) ! to APT file
 7251 format(150('=')/'For ',a11,'     A     B     C     D     E',
     &'     F     G     H     I     J     K     L     M     ',
     &'N     O     *   P-T',
     &'     U'/150('='))

      if ( n148 .eq. 1 ) write(140+idet,7751) ! 55555555555555555555555555555555
 7751 format(150('=')/'    A    B    C    D    E    F    ',
     &'     F     G     H     I     J     K     L     M     ',
     &'N     O     *   P-T',
     &'     U'/150('-'))
      
      if ( n148 .eq. 1 ) then ! 555555555555555555555555555555555555555555555555
      do ip = 2, nprop
      write(41,7851)ip,nameprop(ip),brakedown(ibodies,idet,ip) ! 555555555555555
 7851 format(i6,1x,a33,f10.2) ! 555555555555555555555555555555555555555555555555
      write(41,7546)nameprop(ip),totAV(ip),tot90(ip),tot95(ip), ! 55555555555555
     &tot98(ip),tot99(ip),tot995(ip),tot999(ip) ! 555555555555555555555555555555
 7546 format('Total % from ',a27,10f10.2)
      enddo
      write(41,7721)rowtotal,x1,x2 ! to AP2 file 5555555555555555555555555555555
 7721 format(4x,'17 Sub-catchment total',11x,f13.3/
     &4x,'18 Other contributions',14x,f10.2/150('-')/
     &4x,'   TOTAL              ',14x,f10.2/150('-'))
      endif ! if ( n148 .eq. 1 ) 55555555555555555555555555555555555555555555555
      
      endif ! if ( ihedd .eq. 1 )


      write(01,7770)uname(jbodies),
     &(brakedown(ibodies,idet,ip),ip=2,16),rowtotal,
     &x1,brakedown(ibodies,idet,nprop+1)
      write(40,7770)uname(jbodies), ! to APT file
     &(brakedown(ibodies,idet,ip),ip=2,16),rowtotal,
     &x1,brakedown(ibodies,idet,nprop+1)
 7770 format(a15,24f6.1)
      if ( n148 .eq. 1 ) write(140+idet,7170) ! 55555555555555555555555555555555
     &(brakedown(ibodies,idet,ip),ip=2,16),
     &x1,brakedown(ibodies,idet,nprop+1),uname(jbodies)
 7170 format(18f6.1,a37)
      endif
      enddo
      
      do ip = 2, nprop
      grund total2 = grund total2 + braketotal(ip)
      enddo
      
      x1 = amax1(0.0,braketotal(nprop+1)-grund total2) 
      x2 = x1 + grund total2
      if ( x2 .gt. 1.0e-9 ) then
      write(01,7772)(braketotal(ip),ip=2,16),grund total2,
     &x1,braketotal(nprop+1)
      write(40,7772)(braketotal(ip),ip=2,16),grund total2,
     &x1,braketotal(nprop+1) ! to APT file
 7772 format(150('-')/'TOTALS ...',5x,23f6.1)
      if ( n148 .eq. 1 ) write(140+idet,7272) ! 5555555555555555555555555555555555
     &(braketotal(ip),ip=2,16),grund total2,
     &x1,braketotal(nprop+1)
 7272 format(150('-')/18f6.1,' Totals ...')
      write(01,5)
      write(40,5)
      if ( n148 .eq. 1 ) write(140+idet,1157) ! 5555555555555555555555555555555555
 1157 format(150('-'))
      endif

      write(110+idet,7779)GIScode(feeture),unamex,uname(jbodies),
     &dname(idet),(braketotal(ip),ip=2,16),grund total2,
     &x1,braketotal(nprop+1)
 7779 format(a40,',',a40,',',a37,',',
     &'Percentage breakdown of sources of pollution',',',a11,
     &18(',',1pe11.4),',',
     &'Percentage breakdown of sources of pollution')
      
      if ( TGLODE2(idet,i13) .gt. 0.000001 ) then
	TGX = TGLODE2(idet,i13)
*     TILOADUP2 ... load introduced by gap filling for river flows -------------
      pgf1 = 100.0 * TILOADUP2(idet,i13)/TGX
      pgf2 = 100.0 * TILOADDN2(idet,i13)/TGX
      pgf3 = 100.0 * TALOADUP2(idet,i13)/TGX
      pgf4 = 100.0 * TALOADDN2(idet,i13)/TGX
      pgf5 = abs (pgf1) + abs (pgf2) + abs (pgf3) + abs (pgf4)
      igf = 1
      if ( pgf5 .lt. 1.0e-09) igf = 0
      
      if ( igf .eq. 1 .and. x2 .gt. 1.0e-9 ) then
      write(01,8771)pgf1,pgf2,pgf3,pgf4
      write(40,8771)pgf1,pgf2,pgf3,pgf4   
      if ( n148 .eq. 1 ) write(140+idet,8771)pgf1,pgf2,pgf3,pgf4 ! 5555555555555
 8771 format('% Load added by gap filling for river flows   ',f12.2,6x,
     &'% Load removed by gap filling for river flows ',f12.2/
     &'% Load added by gap filling on river quality  ',f12.2,6x,
     &'% Load removed by gap filling on river quality',f12.2/150('='))
      else
      if ( ihedd .eq. 0 .and. ical .ne. 0 ) then 
	if ( no gap filling 78 .ne. 1 ) then
      write(01,8974)   
      write(40,8974) ! to APT file   
      if ( n148 .eq. 1 ) write(140+idet,8974) ! 55555555555555555555555555555555   
 8974 format('No loads added or removed by gap filling ...')
      endif
      endif
      endif
      
      write(110+idet,8779)GIScode(feeture),unamex,uname(jbodies),
     &dname(idet),pgf1 
 8779 format(a40,',',a40,',',a37,',',
     &'% Load added by gap filling for river flows',',',a11,
     &1(',',1pe11.4),',',
     &'% Load added by gap filling for river flows') ! GIS output
      write(110+idet,8729)GIScode(feeture),unamex,uname(jbodies),
     &dname(idet),pgf2 
 8729 format(a40,',',a40,',',a37,',',
     &'% Load removed by gap filling for river flows',',',a11,
     &1(',',1pe11.4),',',
     &'% Load removed by gap filling for river flows')
      write(110+idet,8739)GIScode(feeture),unamex,uname(jbodies),
     &dname(idet),pgf3 
 8739 format(a40,',',a40,',',a37,',',
     &'% Load added by gap filling on river quality',',',a11,
     &1(',',1pe11.4),',',
     &'% Load added by gap filling on river quality')
      write(110+idet,8749)GIScode(feeture),unamex,uname(jbodies),
     &dname(idet),pgf4
 8749 format(a40,',',a40,',',a37,',',
     &'% Load removed by gap filling on river quality',',',a11,
     &1(',',1pe11.4),',',
     &'% Load removed by gap filling on river quality')
	endif ! if ( TGLODE2(idet,i13) .gt. 0.000001 ) 

      endif ! if ( QTYPE (idet) .ne. 4 )
      enddo
      
      return
      end

