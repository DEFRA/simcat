*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     Random number generators   
*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for river flow ----------
*     --------------------------------------------------------------------------
      function GAS1(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET=0
      if ( ISET .eq. 0 ) then
    1 continue     
      V1=2.*RAN1(IDUM)-1.
      V2=2.*RAN1(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1009,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS1=V2*FAC
      ISET=1
      else
      GAS1=GSET
      ISET=0
      endIF
      return
      end



*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for river quality -------
*     --------------------------------------------------------------------------
      function GAS2(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET=0
      if ( ISET .eq. 0 ) then
    1 continue 
      V1=2.*RAN2(IDUM)-1.
      V2=2.*RAN2(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1010,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS2=V2*FAC
      ISET=1
      else
      GAS2=GSET
      ISET=0
      endIF
      return
      end

*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for discharge flow ------
*     --------------------------------------------------------------------------
      function GAS3(IDUM)
      save iset,gset
      data ISET /0/
      if (IDUM .lt. 0) ISET=0
      if (ISET .eq. 0) then
    1 continue     
      V1=2.*RAN3(IDUM)-1.
      V2=2.*RAN3(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1012,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS3=V2*FAC
      ISET=1
      else
      GAS3=GSET
      ISET=0
      endIF
      return
      end


*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for discharge quality ---
*     --------------------------------------------------------------------------
      function GAS4(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET=0
      if ( ISET .eq. 0 ) then
    1 continue     
      V1=2.*RAN4(IDUM)-1.
      V2=2.*RAN4(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1013,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS4=V2*FAC
      ISET=1
      else
      GAS4=GSET
      ISET=0
      endIF
      return
      end


*     --------------------------------------------------------------------------
*     random normal deviates for temperature -----------------------------------
*     --------------------------------------------------------------------------
      function GAS5(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET=0
      if ( ISET .eq. 0 ) then
    1 continue     
      V1=2.*RAN5(IDUM)-1.
      V2=2.*RAN5(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1014,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS5=V2*FAC
      ISET=1
      else
      GAS5=GSET
      ISET=0
      endIF
      return
      end


      function GAS7(IDUM) ! random normal deviates for suspended solids
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET=0
      if ( ISET .eq. 0 ) then
    1 continue     
      V1=2.*RAN7(IDUM)-1.
      V2=2.*RAN7(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1014,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS7=V2*FAC
      ISET=1
      else
      GAS7=GSET
      ISET=0
      endIF
      return
      end



