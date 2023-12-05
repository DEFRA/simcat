      subroutine check the correlation coefficients by regression
      include 'COMMON DATA.FOR'
      
      k1 = 1 ! river flow
      k2 = 2 ! river quality

      call regres the calculated shots (k1,k2,RRR)

      k1 = 1 ! river flow
      k2 = 3 ! discharge flow
      call regres the calculated shots (k1,k2,RRR)
      if ( ical13 .eq. 0 ) then 
      if ( nobigout .le. 0 ) write(33,2)RRR,CO2
    2 format('        Added flow on river flow =',F7.3,' (',f7.3,')')
      endif
      k1 = 1 ! river flow
      k2 = 4 ! discharge quality
      call regres the calculated shots (k1,k2,RRR)
      if ( ical13 .eq. 0 ) then 
      if ( nobigout .le. 0 ) write(33,3)RRR
    3 format('     Added quality on river flow =',F7.3)
      endif
    
      k1 = 3 ! discharge flow
      k2 = 2 ! river quality
      call regres the calculated shots (k1,k2,RRR)
      if ( ical13 .eq. 0 ) then 
      if ( nobigout .le. 0 ) write(33,4)RRR
    4 format('     Added flow on river quality =',F7.3)
      endif
    
      k1 = 4 ! discharge quality
      k2 = 3 ! discharge flow
      call regres the calculated shots (k1,k2,RRR)
      if ( ical13 .eq. 0 ) then 
      if ( nobigout .le. 0 ) write(33,5)RRR,CO5
    5 format('     Added quality on added flow =',F7.3,' (',f7.3,')')
      endif
    
      k1 = 4 ! discharge quality
      k2 = 2 ! river quality
      call regres the calculated shots (k1,k2,RRR)
      if ( ical13 .eq. 0 ) then 
      if ( nobigout .le. 0 ) write(33,7)RRR
    7 format('  Added quality on river quality =',F7.3/77('-'))
      endif
    
      return
      end

      
      subroutine check the correlation by regression (ick)
      include 'COMMON DATA.FOR'
      
      k1 = 1 ! river flow
      k2 = 2 ! river quality
      
      do is = 1, NS
      Creg(k1,IS) = FMS(IS) ! store the river flow for regression ------
      Creg(k2,IS) = CMS(JP,is) ! river quality for regression --------
      enddo

      call regres the calculated shots (k1,k2,RRR)
      RRRegression = RRR
      if ( nobigout .le. 0 ) then
      if ( ical13 .eq. 0 ) then 
      if ( ick .eq. 0 ) then
      cxper = RRR-CO1
      if ( munthly structure .eq. 1 ) then 
      if ( cxper .lt. 4.0 .and. CO1 .gt. 0.2 ) then 
      write(33,3)cxper,dname(JP) ! quolity data (IQ,JP,MO)
    3 format(77('=')/
     &'*** Correlation of quality on flow changed by ',f6.2,
     &' for ',a10/
     &'*** This is caused by the attempt to impose monthly variation'/
     &77('='))
      endif
      endif
      !write(33,1)dname(JP),RRR,CO1,cxper ! quolity data (IQ,JP,MO)
    1 format(77('-')/
     &'Results of regression between sets of shots ... ',a11/
     &'River quality on river flow =',F7.3,
     &'  (value entered:',f6.3,')','   Error =',f7.4)
      else
      if ( correctcorrel .eq. 1 ) then ! correct the correlation ===============
      write(01,2)RRR ,CO1, RRR - CO1master 
      write(33,2)RRR ,CO1, RRR - CO1master 
    2 format(
     &'River quality on river flow =',F7.3,
     &'  (adjusted value:',f6.3,')',' (',f7.4,')')
      endif ! if ( correctcorrel .eq. 1 ) correct the correlation ==============
      endif
      endif
      endif
      
      return
      end
 
      
      
      subroutine regres the calculated shots (K1,K2,RRR)
      include 'COMMON DATA.FOR'
      dimension xmb(NS),ymb(NS)

      log1 = 1
      log2 = 1
      if ( K1 .lt. 0 ) then 
      log1 = 0
      K1 = -K1
      endif
      if ( K2 .lt. 0 ) then
      log2 = 0
      K2 = -K2
      endif
	sumx = 0.0
	sumy = 0.0
	do is = 1, NS   
      if (log1 .eq. 1 ) then
      ymb(is) = alog (amax1(1.0e-10,Creg(K1,is)))
      else
      ymb(is) = Creg(K1,is)
      endif
      if (log2 .eq. 1 ) then
      xmb(is) = alog (amax1(1.0e-10,Creg(K2,is)))
      else
      xmb(is) = Creg(K2,is)
      endif
	sumx = sumx + xmb(is)
	sumy = sumy + ymb(is)
	enddo
      ybar = sumy / float (NS)  
      xbar = sumx / float (NS)  
      
      if ( ybar .lt. -23.0 .and. xbar .lt. -23.0 ) then
      RRR = 0.0
      else
      SXY = 0.0
      SXX = 0.0
      SYY = 0.0
      do i = 1, float (NS)
      YY1 = ymb(i) - ybar
      XX1 = xmb(i) - xbar
      SXY = SXY + YY1 * XX1
      SXX = SXX + XX1 * XX1
      SYY = SYY + YY1 * YY1
      enddo
      RRR = SXY / SQRoot (194443, SXX * SYY)
      endif
   
      return 
      end
