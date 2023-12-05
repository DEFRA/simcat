*     ========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ========================================================================
*     Written in FORTRAN.  With the screen displays in VISUAL BASIC .....
*     ========================================================================
*     ------------------------------------------------------------------------
*     File name: generate mass balance data.for    6008

*     set up the shots from a non-parametric distributions -------------------
	subroutine set up the shots for non parametric stream flow
      include 'COM.FOR'

      if ( IFDIST .ne. 4 ) return
      JTYPE = JT (KFEAT)

*     for a stream -------------------------------------------------------------
	if ( JTYPE .eq. 2 ) call non parametric stream flows
	if ( JTYPE .eq. 13) call non parametric stream flows
      if ( JTYPE .eq. 18) then
      JP = Ndetfirst
      call non parametric stream flows
      endif
      if ( JTYPE .eq. 20) call non parametric stream flows
      if ( JTYPE .eq. 21) call non parametric stream flows
      if ( JTYPE .eq. 25) call non parametric stream flows
      if ( JTYPE .eq. 27) call non parametric stream flows
      if ( JTYPE .eq. 29) call non parametric stream flows
      if ( JTYPE .eq. 31) call non parametric stream flows
      if ( JTYPE .eq. 33) call non parametric stream flows
      if ( JTYPE .eq. 35) call non parametric stream flows
      if ( JTYPE .eq. 46) call non parametric stream flows
      if ( JTYPE .eq. 48) call non parametric stream flows
      if ( JTYPE .eq. 37) call non parametric stream flows
      if ( JTYPE .eq. 40) call non parametric stream flows

	return
	end





	subroutine set up the shots for non parametric discharge flow
      include 'COM.FOR'

      if ( IFDIST .ne. 4 ) return
      JTYPE = JT (KFEAT)

*     for a discharge ----------------------------------------------------------
      if ( JTYPE .eq. 3 ) call non parametric effluent flows
      if ( JTYPE .eq. 5 ) call non parametric effluent flows
      if ( JTYPE .eq. 12) call non parametric effluent flows
      if ( JTYPE .eq. 15) call non parametric effluent flows
      if ( JTYPE .eq. 19) call non parametric effluent flows
      if ( JTYPE .eq. 22) call non parametric effluent flows
      if ( JTYPE .eq. 23) call non parametric effluent flows
      if ( JTYPE .eq. 39) call non parametric effluent flows
      if ( JTYPE .eq. 42) call non parametric effluent flows

	return
	end


	subroutine set up the shots for non parametric stream quality
      include 'COM.FOR'

	if ( qtype (jp) .ne. 4 ) then
      if ( IQDIST .eq. 4 .or. IQDIST .eq. 9 ) then
      JTYPE = JT (KFEAT)

*     the stream ---------------------------------------------------------------
      if ( JTYPE .eq. 2 ) call non parametric stream quality
      if ( JTYPE .eq. 13) call non parametric stream quality
      if ( JTYPE .eq. 18) call non parametric stream quality
      if ( JTYPE .eq. 20) call non parametric stream quality
      if ( JTYPE .eq. 21) call non parametric stream quality
      if ( JTYPE .eq. 25) call non parametric stream quality
      if ( JTYPE .eq. 27) call non parametric stream quality
      if ( JTYPE .eq. 29) call non parametric stream quality
      if ( JTYPE .eq. 31) call non parametric stream quality
      if ( JTYPE .eq. 33) call non parametric stream quality
      if ( JTYPE .eq. 35) call non parametric stream quality
      if ( JTYPE .eq. 46) call non parametric stream quality
      if ( JTYPE .eq. 48) call non parametric stream quality
      if ( JTYPE .eq. 37) call non parametric stream quality
      if ( JTYPE .eq. 40) call non parametric stream quality
      endif
	endif

	return
	end




	subroutine set up the shots for non parametric discharge quality
      include 'COM.FOR'

	if ( qtype (jp) .ne. 4 ) then
      if ( IQDIST .eq. 4 .or. IQDIST .eq. 9 ) then
      JTYPE = JT (KFEAT)

*     effluents ----------------------------------------------------------------
      if ( JTYPE .eq. 3 ) call non parametric discharge quality
      if ( JTYPE .eq. 5 ) call non parametric discharge quality
      if ( JTYPE .eq. 12) call non parametric discharge quality
      if ( JTYPE .eq. 15) call non parametric discharge quality
      if ( JTYPE .eq. 19) call non parametric discharge quality
      if ( JTYPE .eq. 22) call non parametric discharge quality
      if ( JTYPE .eq. 23) call non parametric discharge quality
      if ( JTYPE .eq. 39) call non parametric discharge quality
      if ( JTYPE .eq. 42) call non parametric discharge quality
      endif
	endif
	return
	end







*     set up the shots from monthly data on discharge flow ---------------------
*     called from mass balance ---------------------------------------------------
	subroutine set up monthly data for stream flow
	include 'COM.FOR'

      if ( IFDIST .eq. 5 ) then
 	if ( KFEAT .eq. 0  ) return
      JTYPE = JT (KFEAT)

*     for a stream -------------------------------------------------------------
	if ( JTYPE .eq. 2 ) call generate monthly stream flow data
	if ( JTYPE .eq. 13) call generate monthly stream flow data
	if ( JTYPE .eq. 18) call generate monthly stream flow data
	if ( JTYPE .eq. 20) call generate monthly stream flow data
	if ( JTYPE .eq. 21) call generate monthly stream flow data
	if ( JTYPE .eq. 25) call generate monthly stream flow data
	if ( JTYPE .eq. 27) call generate monthly stream flow data
	if ( JTYPE .eq. 29) call generate monthly stream flow data
	if ( JTYPE .eq. 31) call generate monthly stream flow data
	if ( JTYPE .eq. 33) call generate monthly stream flow data
	if ( JTYPE .eq. 35) call generate monthly stream flow data
	if ( JTYPE .eq. 46) call generate monthly stream flow data
	if ( JTYPE .eq. 48) call generate monthly stream flow data
	if ( JTYPE .eq. 37) call generate monthly stream flow data
	if ( JTYPE .eq. 40) call generate monthly stream flow data

      call write out the added flow shots

      endif
	return
	end






*     set up the shots from monthly data on discharge flow ---------------------
*     called from mass balance ---------------------------------------------------
	subroutine set up monthly data for discharge flow
	include 'COM.FOR'

      if ( IFDIST .eq. 5 ) then
 	if ( KFEAT .eq. 0  ) return
      JTYPE = JT (KFEAT)

      if ( JTYPE .eq. 3  ) call monthly discharge flow
      if ( JTYPE .eq. 5  ) call monthly discharge flow
      if ( JTYPE .eq. 12 ) call monthly discharge flow
      if ( JTYPE .eq. 15 ) call monthly discharge flow
      if ( JTYPE .eq. 19 ) call monthly discharge flow
      if ( JTYPE .eq. 22 ) call monthly discharge flow
	if ( JTYPE .eq. 23 ) call monthly discharge flow
	if ( JTYPE .eq. 39 ) call monthly discharge flow
	if ( JTYPE .eq. 42 ) call monthly discharge flow

      call write out the added flow shots

      endif
	return
	end








*     set up the shots from times series data on discharge flow ----------------
*     called from mass balance -------------------------------------------------
	subroutine set up monthly structure for stream flow 8
	include 'COM.FOR'

      if ( IFDIST .ne. 8 ) return
      if ( munthly structure .eq. 0 ) return
	if ( KFEAT .eq. 0  ) return
      JTYPE = JT (KFEAT)

      if ( JTYPE .eq. 3  ) return
      if ( JTYPE .eq. 5  ) return
      if ( JTYPE .eq. 12 ) return
      if ( JTYPE .eq. 15 ) return
      if ( JTYPE .eq. 19 ) return
      if ( JTYPE .eq. 22 ) return
      if ( JTYPE .eq. 23 ) return
      if ( JTYPE .eq. 39 ) return
      if ( JTYPE .eq. 43 ) return


*     for a stream -------------------------------------------------------------
      if ( ifdiffuse .eq. 0 .or. ifdiffuse .eq. 1 ) then

	if ( JTYPE .eq. 2 ) then
      call generate monthly structure for stream flows 8
      endif

	if ( JTYPE .eq. 13 .or. KRFPOL13 .gt. 0 ) then
      if ( KRFPOL13 .eq. IF ) then
 	call generate monthly structure for stream flows 8
      endif
	endif

	if ( JTYPE .eq. 18) then
      call generate monthly structure for stream flows 8
      endif

	if ( JTYPE .eq. 21) then
	call generate monthly structure for stream flows 8
      endif

	if ( JTYPE .eq. 25 .or. KRFPOL25 .gt. 0 ) then
      if ( KRFPOL25 .eq. IF ) then
	call generate monthly structure for stream flows 8
      endif
      endif

	if ( JTYPE .eq. 27 .or. KRFPOL27 .gt. 0 ) then
      if ( KRFPOL27 .eq. IF ) then
	call generate monthly structure for stream flows 8
      endif
      endif

	if ( JTYPE .eq. 29 .or. KRFPOL29 .gt. 0 ) then
      if ( KRFPOL29 .eq. IF ) then
	call generate monthly structure for stream flows 8
      endif
      endif

	if ( JTYPE .eq. 31 .or. KRFPOL31 .gt. 0 ) then
      if ( KRFPOL31 .eq. IF ) then
	call generate monthly structure for stream flows 8
      endif
      endif

	if ( JTYPE .eq. 33 .or. KRFPOL33 .gt. 0 ) then
      if ( KRFPOL33 .eq. IF ) then
	call generate monthly structure for stream flows 8
      endif
      endif

	if ( JTYPE .eq. 35 .or. KRFPOL35 .gt. 0 ) then
      if ( KRFPOL35 .eq. IF ) then
	call generate monthly structure for stream flows 8
      endif
      endif

	if ( JTYPE .eq. 46 .or. KRFPOL46 .gt. 0 ) then
      if ( KRFPOL46 .eq. IF ) then
	call generate monthly structure for stream flows 8
      endif
      endif

	if ( JTYPE .eq. 48 .or. KRFPOL48 .gt. 0 ) then
      if ( KRFPOL48 .eq. IF ) then
	call generate monthly structure for stream flows 8
      endif
      endif

	if ( JTYPE .eq. 37 .or. KRFPOL37 .gt. 0 ) then
      if ( KRFPOL37 .eq. IF ) then
	call generate monthly structure for stream flows 8
      endif
      endif

	if ( JTYPE .eq. 40 .or. KRFPOL40 .gt. 0 ) then
      if ( KRFPOL40 .eq. IF ) then
	call generate monthly structure for stream flows 8
      endif
      endif

      call write out the added flow shots 
      endif
*     --------------------------------------------------------------------------
      return
	end






*     set up the shots from times series data on discharge flow ----------------
*     called from mass balance -------------------------------------------------
	subroutine set up monthly structure for stream flow 2
	include 'COM.FOR'

      if ( IFDIST .eq. 8 ) return
      if ( munthly structure .eq. 0 ) return
	if ( KFEAT .eq. 0  ) return
      JTYPE = JT (KFEAT)

      if ( JTYPE .eq. 3  ) return
      if ( JTYPE .eq. 5  ) return
      if ( JTYPE .eq. 12 ) return
      if ( JTYPE .eq. 15 ) return
      if ( JTYPE .eq. 19 ) return
      if ( JTYPE .eq. 22 ) return
      if ( JTYPE .eq. 23 ) return
      if ( JTYPE .eq. 39 ) return
      if ( JTYPE .eq. 42 ) return

      if ( IFDIST .eq. 2  .or.  IFDIST .eq. 3  ) then

*     for a stream -------------------------------------------------------------
      if ( ifdiffuse .eq. 0 .or. ifdiffuse .eq. 1 ) then

	if ( JTYPE .eq. 2 ) then
	call generate monthly structure for stream flows 2
      endif

	if ( JTYPE .eq. 13 .or. KRFPOL13 .gt. 0 ) then
      if ( KRFPOL13 .eq. IF ) then
 	call generate monthly structure for stream flows 2
      endif
	endif

	if ( JTYPE .eq. 18) then
	call generate monthly structure for stream flows 2
      endif

	if ( JTYPE .eq. 21) then
	call generate monthly structure for stream flows 2
      endif

	if ( JTYPE .eq. 25 .or. KRFPOL25 .gt. 0 ) then
      if ( KRFPOL25 .eq. IF ) then
	call generate monthly structure for stream flows 2
      endif
	endif

	if ( JTYPE .eq. 27 .or. KRFPOL27 .gt. 0 ) then
      if ( KRFPOL27 .eq. IF ) then
	call generate monthly structure for stream flows 2
      endif
	endif

	if ( JTYPE .eq. 29 .or. KRFPOL29 .gt. 0 ) then
      if ( KRFPOL29 .eq. IF ) then
	call generate monthly structure for stream flows 2
      endif
	endif

	if ( JTYPE .eq. 31 .or. KRFPOL31 .gt. 0 ) then
      if ( KRFPOL31 .eq. IF ) then
	call generate monthly structure for stream flows 2
      endif
	endif

	if ( JTYPE .eq. 33 .or. KRFPOL33 .gt. 0 ) then
      if ( KRFPOL33 .eq. IF ) then
	call generate monthly structure for stream flows 2
      endif
	endif

	if ( JTYPE .eq. 35 .or. KRFPOL35 .gt. 0 ) then
      if ( KRFPOL35 .eq. IF ) then
	call generate monthly structure for stream flows 2
      endif
	endif

	if ( JTYPE .eq. 46 .or. KRFPOL46 .gt. 0 ) then
      if ( KRFPOL46 .eq. IF ) then
	call generate monthly structure for stream flows 2
      endif
	endif

	if ( JTYPE .eq. 48 .or. KRFPOL48 .gt. 0 ) then
      if ( KRFPOL48 .eq. IF ) then
	call generate monthly structure for stream flows 2
      endif
	endif

	if ( JTYPE .eq. 37 .or. KRFPOL37 .gt. 0 ) then
      if ( KRFPOL37 .eq. IF ) then
	call generate monthly structure for stream flows 2
      endif
	endif

	if ( JTYPE .eq. 40 .or. KRFPOL40 .gt. 0 ) then
      if ( KRFPOL40 .eq. IF ) then
	call generate monthly structure for stream flows 2
      endif
	endif
 
      call write out the added flow shots 
      endif
	endif

*     --------------------------------------------------------------------------
      return
	end





	subroutine set up monthly structure for discharge flow 8
	include 'COM.FOR'

      if ( munthly structure .eq. 0 ) return
	if ( KFEAT .eq. 0  ) return
      JTYPE = JT (KFEAT)

	if ( JTYPE .eq. 2  ) return
	if ( JTYPE .eq. 13 ) return
	if ( JTYPE .eq. 18 ) return
	if ( JTYPE .eq. 20 ) return
	if ( JTYPE .eq. 21 ) return
	if ( JTYPE .eq. 25 ) return
	if ( JTYPE .eq. 27 ) return
	if ( JTYPE .eq. 29 ) return
	if ( JTYPE .eq. 31 ) return
	if ( JTYPE .eq. 33 ) return
	if ( JTYPE .eq. 35 ) return
	if ( JTYPE .eq. 46 ) return
	if ( JTYPE .eq. 48 ) return ! birds, boats and angling
	if ( JTYPE .eq. 37 ) return
	if ( JTYPE .eq. 40 ) return

      if ( IFDIST .eq. 8 ) then  ! monthly structure
      if ( ifdiffuse .eq. 0 .or. ifdiffuse .eq. 2 ) then

*     or for effluents ---------------------------------------------------------
      if ( JTYPE .eq. 3  ) then
	call generate monthly structure for discharge flows 8
	endif
      if ( JTYPE .eq. 5  ) then
	call generate monthly structure for discharge flows 8
	endif
      if ( JTYPE .eq. 12  ) then
	call generate monthly structure for discharge flows 8
	endif
      if ( JTYPE .eq. 15 .and. KEPOL15 .gt. 0  ) then
	call generate monthly structure for discharge flows 8
	endif
      if ( JTYPE .eq. 42 .and. KEPOL42 .gt. 0  ) then
	call generate monthly structure for discharge flows 8
	endif
      if ( JTYPE .eq. 19 ) then
	call generate monthly structure for discharge flows 8
	endif
      if ( JTYPE .eq. 22 ) then
	call generate monthly structure for discharge flows 8
	endif
	if ( JTYPE .eq. 23 ) then
	call generate monthly structure for discharge flows 8
	endif
	if ( JTYPE .eq. 39 ) then
	call generate monthly structure for discharge flows 8
	endif

      call write out the added flow shots 
      endif
	endif

	return
	end





	subroutine set up monthly structure for discharge flow 2
	include 'COM.FOR'

      if ( munthly structure .eq. 0 ) return
	if ( KFEAT .eq. 0 ) return
      JTYPE = JT (KFEAT)

	if ( JTYPE .eq. 2  ) return
	if ( JTYPE .eq. 13 ) return
	if ( JTYPE .eq. 18 ) return
	if ( JTYPE .eq. 20 ) return
	if ( JTYPE .eq. 21 ) return
	if ( JTYPE .eq. 25 ) return
	if ( JTYPE .eq. 27 ) return
	if ( JTYPE .eq. 29 ) return
	if ( JTYPE .eq. 31 ) return
	if ( JTYPE .eq. 33 ) return
	if ( JTYPE .eq. 35 ) return
	if ( JTYPE .eq. 46 ) return
	if ( JTYPE .eq. 48 ) return
	if ( JTYPE .eq. 37 ) return
	if ( JTYPE .eq. 40 ) return

      if ( IFDIST .eq. 2 .or. IFDIST .eq. 3 ) then
      if ( ifdiffuse .eq. 0 .or. ifdiffuse .eq. 2 ) then

*     or for effluents ---------------------------------------------------------
      if ( JTYPE .eq. 3  ) then
	call generate monthly structure for discharge flows 2
	endif
      if ( JTYPE .eq. 5  ) then
 	call generate monthly structure for discharge flows 2
 	endif
      if ( JTYPE .eq. 12 ) then
	call generate monthly structure for discharge flows 2
	endif
      if ( JTYPE .eq. 15 .and. KEPOL15 .gt. 0  ) then
	call generate monthly structure for discharge flows 2
	endif
      if ( JTYPE .eq. 42 .and. KEPOL42 .gt. 0  ) then
	call generate monthly structure for discharge flows 2
	endif
      if ( JTYPE .eq. 19 ) then
	call generate monthly structure for discharge flows 2
	endif
      if ( JTYPE .eq. 22 ) then
	call generate monthly structure for discharge flows 2
	endif
	if ( JTYPE .eq. 23 ) then
	call generate monthly structure for discharge flows 2
	endif
	if ( JTYPE .eq. 39 ) then
	call generate monthly structure for discharge flows 2
	endif

      call write out the added flow shots 
      endif
	endif

	return
	end









*     set up the shots from monthly data on discharge flow ---------------------
*     called from mass balance -------------------------------------------------
	subroutine set up monthly structure for stream quality
	include 'COM.FOR'

      if ( munthly structure .eq. 0 ) return
      if ( KFEAT .eq. 0 ) return
      JTYPE = JT (KFEAT)

	if ( qtype (jp) .ne. 4 ) then
      if ( IQDIST .eq. 8 ) then ! monthly structure
      if ( ifdiffuse .eq. 0 .or. ifdiffuse .eq. 1 ) then
          
*     for a stream -------------------------------------------------------------
	if ( JTYPE .eq. 2 ) then
	call generate monthly structure for river quality (1)
      endif

	if ( JTYPE .eq. 13 .or. KRQPOL13 .gt. 0 ) then
      if ( KRQPOL13 .eq. IQ ) then
	call generate monthly structure for river quality (1)
      endif
      endif

	if ( JTYPE .eq. 18) then
	call generate monthly structure for river quality (1)
      endif
	if ( JTYPE .eq. 21) then
	call generate monthly structure for river quality (1)
      endif

	if ( JTYPE .eq. 25 .or. KRQPOL25 .gt. 0 ) then
      if ( KRQPOL25 .eq. IQ ) then
	call generate monthly structure for river quality (1)
      endif
	endif

	if ( JTYPE .eq. 27 .or. KRQPOL27 .gt. 0 ) then
      if ( KRQPOL27 .eq. IQ ) then
	call generate monthly structure for river quality (1)
      endif
	endif

	if ( JTYPE .eq. 29 .or. KRQPOL29 .gt. 0 ) then
      if ( KRQPOL29 .eq. IQ ) then
	call generate monthly structure for river quality (1)
      endif
	endif

	if ( JTYPE .eq. 31 .or. KRQPOL31 .gt. 0 ) then
      if ( KRQPOL31 .eq. IQ ) then
	call generate monthly structure for river quality (1)
      endif
	endif

	if ( JTYPE .eq. 33 .or. KRQPOL33 .gt. 0 ) then
      if ( KRQPOL33 .eq. IQ ) then
	call generate monthly structure for river quality (1)
      endif
      endif

	if ( JTYPE .eq. 35 .or. KRQPOL35 .gt. 0 ) then
      if ( KRQPOL35 .eq. IQ ) then
	call generate monthly structure for river quality (1)
      endif
      endif

	if ( JTYPE .eq. 46 .or. KRQPOL46 .gt. 0 ) then
      if ( KRQPOL46 .eq. IQ ) then
	call generate monthly structure for river quality (1)
      endif
	endif

	if ( JTYPE .eq. 48 .or. KRQPOL48 .gt. 0 ) then
      if ( KRQPOL48 .eq. IQ ) then ! birds, boats and angling
	call generate monthly structure for river quality (1)
      endif
	endif

	if ( JTYPE .eq. 37 .or. KRQPOL37 .gt. 0 ) then
      if ( KRQPOL37 .eq. IQ ) then
	call generate monthly structure for river quality (1)
      endif
	endif

	if ( JTYPE .eq. 40 .or. KRQPOL40 .gt. 0 ) then
      if ( KRQPOL40 .eq. IQ ) then
	call generate monthly structure for river quality (1)
      endif
	endif

      call write out the added flow shots 

      endif
	endif
	endif
	return
	end



*     set up the shots from monthly data on discharge flow ---------------------
*     called from mass balance -------------------------------------------------
	subroutine set up monthly structure for discharge quality
	include 'COM.FOR'

      if ( munthly structure .eq. 0 ) return
      if ( KFEAT .eq. 0 ) return
      JTYPE = JT (KFEAT)
	if ( qtype (jp) .ne. 4 ) then
      if ( IQDIST .eq. 8 ) then ! monthly structure
      if ( ifdiffuse .eq. 0 .or. ifdiffuse .eq. 2 ) then

*     or for effluents ---------------------------------------------------------
      if ( JTYPE .eq. 3 ) then
	call generate monthly structure for discharge quality
	endif
      if ( JTYPE .eq. 5 ) then
	call generate monthly structure for discharge quality
	endif
      if ( JTYPE .eq. 12 ) then
	call generate monthly structure for discharge quality
	endif
      if ( JTYPE .eq. 15 .and. KEPOL15 .gt. 0 ) then
	call generate monthly structure for discharge quality
	endif
      if ( JTYPE .eq. 42 .and. KEPOL42 .gt. 0 ) then
	call generate monthly structure for discharge quality
	endif
      if ( JTYPE .eq. 19 ) then
	call generate monthly structure for discharge quality
	endif
      if ( JTYPE .eq. 22 ) then
	call generate monthly structure for discharge quality
	endif
	if ( JTYPE .eq. 23 ) then
	call generate monthly structure for discharge quality
	endif
	if ( JTYPE .eq. 39 ) then
	call generate monthly structure for discharge quality
	endif
      call write out the added flow shots 

      endif
	endif
      endif
	return
	end






*     set up the shots from monthly data on discharge quality ------------------
*     called from mass balance -------------------------------------------------
	subroutine set up monthly data for stream quality
      include 'COM.FOR'

      if ( KFEAT .eq. 0 ) return
      JTYPE = JT (KFEAT)

*     set up shots from monthly data on discharge quality ----------------------

	if ( qtype (jp) .ne. 4  ) then 
      if ( IQDIST .eq. 5  ) then ! monthly data --------------------------------
      if ( ifdiffuse .eq. 0 .or. ifdiffuse .eq. 1 ) then

*     the stream ---------------------------------------------------------------
	if ( JTYPE .eq. 2  ) call monthly stream quality
	if ( JTYPE .eq. 13 ) call monthly stream quality
	if ( JTYPE .eq. 18 ) call monthly stream quality
	if ( JTYPE .eq. 20 ) call monthly stream quality
	if ( JTYPE .eq. 21 ) call monthly stream quality
	if ( JTYPE .eq. 25 ) call monthly stream quality
	if ( JTYPE .eq. 27 ) call monthly stream quality
	if ( JTYPE .eq. 29 ) call monthly stream quality
	if ( JTYPE .eq. 31 ) call monthly stream quality
	if ( JTYPE .eq. 33 ) call monthly stream quality
	if ( JTYPE .eq. 35 ) call monthly stream quality
	if ( JTYPE .eq. 46 ) call monthly stream quality
	if ( JTYPE .eq. 48 ) call monthly stream quality
	if ( JTYPE .eq. 37 ) call monthly stream quality
	if ( JTYPE .eq. 40 ) call monthly stream quality

      endif
	endif
	endif
	return
	end


	subroutine set up monthly data for discharge quality
      include 'COM.FOR'

      if ( KFEAT .eq. 0 ) return
      JTYPE = JT (KFEAT)

*     set up shots from monthly data on discharge quality ----------------------

	if ( qtype (jp) .ne. 4  ) then
      if ( IQDIST .eq. 5  ) then ! monthly data --------------------------------
      if ( ifdiffuse .eq. 0 .or. ifdiffuse .eq. 2 ) then

*     effluents ----------------------------------------------------------------

      if ( JTYPE .eq. 3  ) call monthly discharge quality
      if ( JTYPE .eq. 5  ) call monthly discharge quality
      if ( JTYPE .eq. 12 ) call monthly discharge quality
      if ( JTYPE .eq. 15 ) call monthly discharge quality
      if ( JTYPE .eq. 19 ) call monthly discharge quality
      if ( JTYPE .eq. 22 ) call monthly discharge quality
      if ( JTYPE .eq. 23 ) call monthly discharge quality
      if ( JTYPE .eq. 39 ) call monthly discharge quality
      if ( JTYPE .eq. 42 ) call monthly discharge quality

      endif
	endif
	endif
	return
	end






*     non-parametric distribution of stream flow -------------------------------
      subroutine non parametric stream flows
      include 'COM.FOR'
      logical exists

      cut off zero flow = 0.0

*     load the previously calculated summary statitics -------------------------
*     these were calculated when the data were read in -------------------------
*     mean and 95-percentile low flow ------------------------------------------
	RRFM = F(IF,1)
	RRFS = F(IF,2)
      spcorrRFaf = F(IF,4) ! correlation of flow on added flow ------------ FRAN
      
      if ( MONF .gt. 1 .and. JP .eq. ndetfirst ) then
      call sort format 2 (RRFM,RRFS)
      if ( ifdiffuse .eq. 0 ) then
      write(01,51)
   51 format(77('-')/
     &'Generated stream flows: non-parametric '/77('-'))
      write(01,52) valchars10, FUNIT, valchars11, FUNIT
   52 format('Input flow data:',33x,'Annual mean =',a10,1x,a4/
     &39x,'95-percent exceedence =',a10,1x,a4/77('-'))
      else
      write(01,6651)
 6651 format(33x,77('-')/33x,
     &'GENERATED diffuse inflows: non-parametric '/33x,77('-'))
      write(01,6652) valchars10, FUNIT, valchars11, FUNIT
 6652 format(33x,'Input flow data:',33x,'Annual mean =',a10,1x,a4/33x,
     &39x,'95-percent exceedence =',a10,1x,a4/33x,77('-'))
      endif
      endif

*     calculate the Monte-Carlo sampling errors --------------------------------
      call bias in non parametric stream flows
      if ( BM(1) .gt. 1.0E-08 ) BM(1) = RRFM/BM(1)
      BS(1) = 1.0
      if ( MONF .gt. 1 .and. JP .eq. ndetfirst ) then
      if ( ifdiffuse .eq. 0 ) then
      write(01,42)BM(1)
   42 format(
     &'Correction for Monte-Carlo sampling errors for stream ',
     &'flow:',F12.4/77('-'))
      else
      write(01,421)BM(1)
  421 format(33x,
     &'CORRECTION for Monte-Carlo sampling errors for diffuse ',
     &'inflow:',F12.4/33x,77('-'))
      endif
      endif

*     identify the file with the non-parametric data ---------------------------
*     loop on the number of non-parametric datafiles ---------------------------
      do 1 i = 1, M7
      icod = i
*     check for a code for the datafile ----------------------------------------
      if ( idenp ( 1, i, 1 ) .eq. IF ) goto 2
    1 continue

*     no valid code found for the datafile -------------------------------------
      write( *,3)
      write(01,3)
      write(09,3)
      write(33,3)
    3 format(77('-')/'*** Error in tributary flow data ...'/
     &'*** No valid code for the non-parametric data ...'/77('-'))
      call stop

*     valid code found ... check the datafile exists ---------------------------
    2 continue
      inquire ( FILE = Flname(1,icod), EXIST = exists )
      if ( .NOT. exists ) then
	call change colour of text (11) ! turquoise
      write( *,8163) flnamesmall(1,icod)
      call set screen text colour
 8163 Format('* Error in flow data: ',
     &'data file does not exist ... ',a64)
      write(01,7163) flnamesmall(1,icod)
      write(09,7163) flnamesmall(1,icod)
      write(33,7163) flnamesmall(1,icod)
 7163 format(/77('-')/
     &'*** Error in flow data ...  '/
     &'*** The data file does not exist ... ',a64/
     &'*** Run halted ...'/77('-'))
      if ( ifbatch .eq. 1 ) return
      call stop
      endif

*     open the file ------------------------------------------------------------
      open(12,FILE = Flname(1,icod), STATUS='OLD' )

*     read the file ------------------------------------------------------------
      read(12, *, ERR=7500) nprf
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(1,icod) = 1
      endif
      kprf = nprf

*     check the number of data points - are there too many ? -------------------
      if ( nprf .gt. mprf) then
      write( *,7166) flnamesmall(1,icod)
      write(01,7166) flnamesmall(1,icod)
      write(09,7166) flnamesmall(1,icod)
      write(33,7166) flnamesmall(1,icod)
 7166 format(/77('-')/'*** Error in tributary flow data ...'/
     &'*** Too many data values specified for non-parametric  '/
     &'*** distribution. Check the file ',a64/77('-'))
      call stop
      endif

*     check the number of data points - are there too few ? --------------------
      if ( nprf .lt. 5 ) then
      write( *,7167) flnamesmall(1,icod)
      write(01,7167) flnamesmall(1,icod)
      write(09,7167) flnamesmall(1,icod)
      write(33,7167) flnamesmall(1,icod)
      write(34,7167) flnamesmall(1,icod)
 7167 format(/77('-')/'*** Error in tributary flow data ...'/
     &'*** Too few data values specified for non-parametric   '/
     &'*** distribution. Check the file ',a64/77('-'))
      call stop
      endif

      backspace (12)
*     read the data ------------------------------------------------------------
      read(12, *, ERR=7500) nprf, (rfnpvl(i),i=1 , kprf)
      close (12)
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(1,icod) = 1
	endif
      kprf = nprf

*     arrange the data in sequence ---------------------------------------------
      if ( flsequence(1,nonpd) .eq. 0 ) then
      do i = 1,   nprf-1
      do j = i+1, nprf
      if ( rfnpvl(i) .ge. rfnpvl(j) ) then
      xtemp = rfnpvl (j)
      rfnpvl (j) = rfnpvl (i)
      rfnpvl (i) = xtemp
	endif
      enddo
      enddo
      endif

*     compute cumulative frequencies and store them in RFNPFD ------------------
      CUMUL = 1.0 / float (nprf)
      do i = 1, nprf
      rfnpfd(i) = float(i) * CUMUL
      enddo

*     compute the cut-off for zero for intermittent flows ----------------------
      cut off zero flow = 0.0 
      imark = 0
      do i = 1, nprf
*     write(01,2633)i,rfnpvl(i),rfnpfd(i)
*2633 format(i3,2f15.8)
      if ( imark .eq. 0 .and. rfnpvl(i) .gt. 1.0e-10 ) imark = i
	enddo
	if ( imark .gt. 1 ) then
	if ( imark .le. nprf ) then
      cut off zero flow = 0.5 * (rfnpfd (imark) + rfnpfd (imark - 1))
	else
      cut off zero flow = rfnpfd (nprf)
	endif
	endif
*     if ( ifdiffuse .eq. 0 ) then
*     write(01,2311)100.0*(1.0-cut off zero flow)
*2311 format('Percent of the year that flow is added =',f7.3)
*     else
*     write(01,2331)100.0*(1.0-cut off zero flow)
*2331 format(33x,'Percent of the year that flow is added =',f7.3)
*     endif

*     sample the distributions -------------------------------------------------
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
*     get the random normal deviate --------------------------------------------
*     and impose correlation with flow in main river ---------------------------
      EDNP = ERAN function (is)
	ERANR(IS) = EDNP
*     compute the value of the flow for this shot ------------------------------
      call get non parametric shot ( EDNP, RFSHOT )
*     convert the deviate to a percentage point --------------------------------
      ednppc = TDIST2 (9999.0, ERANR(IS) )
*     substract the percentage point for the cut-off flow ----------------------
	ERANR2(IS) = amax1 (0.0, ednppc - cut off zero flow)
*     express the result as a new percentage -----------------------------------
      ERANR2(IS) = ERANR2(IS) / (1.0 - cut off zero flow)
	if ( ERANR2(IS) .gt. 1.0e-12 ) then
*     convert to a deviate -----------------------------------------------------
      ERANR3(IS) = TDIST1 (9999.0, ERANR2(IS) )
	else
      ERANR3(IS) = 0.0
	endif
      EFshots(IS) = VALPF ( RFSHOT, RRFM, BM(1) )
*     EFshots(IS) = EFshots(IS) * BM(1)
      FTMS(IS) = EFshots(IS) 
      enddo

      if ( MONQ .gt. 1 .and. JP .eq. ndetfirst ) then
      call write the shots for the flow from the stream
 	endif
      return

 7500 write( *,7168)flnamesmall(1,icod)
      write(01,7168)flnamesmall(1,icod)
      write(09,7168)flnamesmall(1,icod)
      write(33,7168)flnamesmall(1,icod)
 7168 format(/77('-')/'*** Error in tributary flow data ...'/
     &'*** Error in reading non-parametric data ...'/
     &'*** Check the file ',a64/77('-'))
      call stop
      end



*     compute Monte-Carlo sampling errors ... non-parametric data --------------
      subroutine bias in non parametric stream flows
      include 'COM.FOR'
      logical exists

      BM(1) = 0.0
      BS(1) = 0.0

*     identify the file with the non-parametric data ---------------------------
      do 1 i = 1, M7
      icod = i
      if ( idenp ( 1, i, 1 ) .eq. IF ) goto 2
    1 continue

*     no valid code found for the datafile -------------------------------------
      write( *,3)
      write(01,3)
      write(09,3)
      write(33,3)
    3 format(77('-')'*** Error in flow data ...'/
     &'*** No valid code for the non-parametric dataset ...'/77('-'))
      call stop

*     valid code found ... check datafile exists -------------------------------
    2 continue
      inquire( FILE = Flname(1,icod), EXIST = exists )
      if ( .NOT. exists) then
	call change colour of text (20) ! bright red
      write( *,8163) flnamesmall(1,icod)
      call set screen text colour
 8163 Format('* Error in stream flow data: ',
     &'data file does not exist ... ',a64)
      write(01,7163) flnamesmall(1,icod)
      write(09,7163) flnamesmall(1,icod)
      write(33,7163) flnamesmall(1,icod)
 7163 Format(/77('-')/
     &'*** Error in stream flow data ...'/
     &'*** The data file does not exist ... ',a64/
     &'*** Run halted ... '/77('-'))
      if ( ifbatch .eq. 1 ) return
      call stop
      else
      if ( MONF .gt. 1 .and. JP .eq. ndetfirst ) then
      write(01,7863) flnamesmall(1,icod)
      write(33,7863) flnamesmall(1,icod)
 7863 format(77('-')/'Non-parametric river flow data ... ',
     &'File: ',a64/77('-'))
      endif
      endif

*     open the file ------------------------------------------------------------
      open(12,FILE = Flname(1,icod), STATUS='OLD')

*     read the file ------------------------------------------------------------
      read(12, *, ERR=7500) nprf
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(1,icod) = 1
	endif
      kprf = nprf

*     check the number of data points - are there too many ? -------------------
      if ( nprf .gt. mprf) then
      write( *,7166) flnamesmall(1,icod)
      write(01,7166) flnamesmall(1,icod)
      write(09,7166) flnamesmall(1,icod)
      write(33,7166) flnamesmall(1,icod)
 7166 Format(/77('-')/'*** Error in stream flow data ...'/
     &'*** Too many data values specified for non-parametric  '/
     &'*** distribution. Check the file ',a64/77('-'))
      call stop
      endif

*     check the number of data points - are there too few ? --------------------
      if ( nprf .lt. 5 ) then
      write( *,7167) flnamesmall(1,icod)
      write(01,7167) flnamesmall(1,icod)
      write(09,7167) flnamesmall(1,icod)
      write(33,7167) flnamesmall(1,icod)
 7167 Format(/77('-')/
     &'*** Error in stream flow data ...'/
     &'*** Too few data values specified for non-parametric   '/
     &'*** distribution. Check the file ',a64/77('-'))
      call stop
      endif

      backspace (12)
      read(12, *, ERR=7500) nprf, (rfnpvl(i),i=1 , kprf)
      close (12)
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(1,icod) = 1
	endif
      kprf = nprf

*     arrange the data in sequence ---------------------------------------------
      if ( flsequence(1,nonpd) .eq. 0 ) then
      do i = 1,   nprf-1
      do j = i+1, nprf
      if ( rfnpvl(i) .ge. rfnpvl(j) ) then
      xtemp = rfnpvl (j)
      rfnpvl (j) = rfnpvl (i)
      rfnpvl (i) = xtemp
	endif
      enddo
      enddo
      endif

*     compute cumulative frequencies and store them in cfd ---------------------
      CUMUL = 1.0 / float (nprf)
      do i = 1, nprf
      rfnpfd(i) = float(i) * CUMUL
      enddo

*     compute the cut-off for zero for intermittent flows ----------------------
      cut off zero flow = 0.0 
      imark = 0
      do i = 1, nprf
      if ( imark .eq. 0 .and. rfnpvl(i) .gt. 1.0e-10 ) imark = i
	enddo
	if ( imark .gt. 1 ) then
	if ( imark .le. nprf ) then
      cut off zero flow = 0.5 * (rfnpfd (imark) + rfnpfd (imark - 1))
      else
      cut off zero flow = rfnpfd (nprf)
      endif
      endif
      if ( MONF .gt. 1 .and. JP .eq. ndetfirst ) then
      write(01,2311)cut off zero flow
 2311 format('Cut-off flow = ',f15.8)
      endif

*     sample the distributions -------------------------------------------------
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
*     get the random normal deviate --------------------------------------------
*     and impose correlation with flow in main river ---------------------------
      EDNP = ERAN function (is)
	ERANR(IS) = EDNP
*     compute the value of the flow for this shot ------------------------------
      call get non parametric shot ( EDNP, RF )
      FTMS (IS) = amax1 ( 0.0, RF )
      enddo

      call calculate summaries for removed flows
      if ( monf .gt. 1 ) call shots for diffuse flows

      BM(1) = flow(1)
      BS(1) = flow(5)

      return

 7500 write( *,7168)flnamesmall(1,icod)
      write(01,7168)flnamesmall(1,icod)
      write(09,7168)flnamesmall(1,icod)
      write(33,7168)flnamesmall(1,icod)
      call stop
 7168 Format(/77('-')/
     &'*** Error in stream flow data ...'/
     &'*** Error in reading non-parametric data ...           '/
     &'*** Check the file ',a64/77('-'))

      end





*     set up all the shots for a non-parametric distribution of discharge flow -
*     this routine is called from Mass Balance ---------------------------------
      subroutine non parametric effluent flows
      include 'COM.FOR'
      logical exists

	EEFM = FE ( IQ, 1 ) ! mean -----------------------------------------------
	EEFS = FE ( IQ, 2 ) ! standard deviation ---------------------------------
      spcorrRFaf = FE ( IQ, 4 ) ! correlation of flow on added flow ------- ERAN

      if ( MONQ .eq. 1 .and. jp .eq. ndetfirst ) then
      write(01,51)
   51 format(77('-')/
     &'Generated discharge flow data (non-parametric)   '/77('-'))
      call sort format 2 (EEFM,EEFS)
      write(01,52) valchars10, FUNIT, valchars11, FUNIT
   52 format('Input data:',24x,'          Mean =',a10,1x,a4/
     &'           ',20x,'Standard deviation =',a10,1x,a4/77('-'))
	endif

*     calculate the Monte-Carlo sampling errors for discharge flow -------------
      call bias in non parametric discharge flows
      if (BM(1) .gt. 1.0E-08) BM(1) = EEFM/BM(1)
      BS(1) = 1.0
      if ( MONQ .eq. 1 .and. JP .eq. ndetfirst ) then
      write(01,42)BM(1)
   42 format(
     &'Correction for Monte-Carlo sampling errors for discharge ',
     &'flow:',F12.4/77('-'))
      endif

*     identify the file with the non-parametric data ---------------------------
*     loop on the number of non-parametric datafiles ---------------------------

      do 1 i = 1, M7
      icod = i

*     check for a code for the datafile ----------------------------------------
      if ( idenp ( 2, i, 1 ) .eq. IQ ) goto 2
    1 continue

*     no valid code found for the datafile -------------------------------------
      write( *,3)
      write(01,3)
      write(09,3)
      write(33,3)
    3 format(/77('-')/
     &'*** Error in non-parametric discharge flow data (1) '/
     &'*** No valid code for the non-parametric dataset ...'/77('-'))
      call stop

*     valid code found. Check the datafile exists ------------------------------
    2 continue

      inquire ( FILE = Flname(3,icod), EXIST = exists )
      if ( .NOT. exists ) then
      write( *,7163) flnamesmall(3,icod)
      write(01,7163) flnamesmall(3,icod)
      write(09,7163) flnamesmall(3,icod)
      write(33,7163) flnamesmall(3,icod)
 7163 format(/77('-')/
     &'*** Error in non-parametric discharge flow data (2) '/
     &'*** Non-parametric data file does not exist ... ',a64/
     &'*** Run halted ...'/77('-'))
      call stop
*     else
*     write(01,7863) flnamesmall(3,icod)
*     write(09,7863) flnamesmall(3,icod)
*     write(33,7863) flnamesmall(3,icod)
*7863 format(77('-')/
*    &'Non-parametric discharge flow data ... File: 'a64/77('-'))
      endif

*     open the file ------------------------------------------------------------
      open(12,FILE = Flname(3,icod), STATUS='OLD' )

*     read the file ------------------------------------------------------------
      read(12, *, ERR=7500) nprf
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(3,icod) = 1
	endif
      kprf = nprf

*     check the number of data points - are there too many ? -------------------
      if ( nprf .gt. mprf) then
      write( *,7166) flnamesmall(3,icod)
      write(01,7166) flnamesmall(3,icod)
      write(09,7166) flnamesmall(3,icod)
      write(33,7166) flnamesmall(3,icod)
 7166 Format(/77('-')/
     &'*** Error in non-parametric discharge flow data (3)'/
     &'*** Too many data values specified for non-parametric'/
     &'*** distribution. Check the file ',a64/77('-'))
      call stop
      endif

*     check the number of data points - are there too few ? --------------------
      if ( nprf .lt. 5 ) then
      write( *,7167) flnamesmall(3,icod)
      write(01,7167) flnamesmall(3,icod)
      write(09,7167) flnamesmall(3,icod)
      write(33,7167) flnamesmall(3,icod)
 7167 Format(/77('-')/
     &'*** Error in non-parametric discharge flow data (4) '/
     &'*** Too few data values specified for non-parametric   '/
     &'*** distribution. Check the file ',a64/77('-'))
      call stop
      endif
      backspace (12)

*     read the data ------------------------------------------------------------
      read(12, *, ERR=7500) nprf, (rfnpvl(i),i=1 , kprf)
      close (12)
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(3,icod) = 1
	endif
      kprf = nprf
*     --------------------------------------------------------------------------

*     arrange the data in sequence ---------------------------------------------
      if ( flsequence(3,nonpd) .eq. 0 ) then
      do i = 1,   nprf-1
      do j = i+1, nprf
      if ( rfnpvl(i) .ge. rfnpvl(j) ) then
      xtemp = rfnpvl (j)
      rfnpvl (j) = rfnpvl (i)
      rfnpvl (i) = xtemp
	endif
      enddo
      enddo
      endif

*     compute cumulative frequencies and store them in RFNPFD ------------------
      CUMUL = 1.0 / float (nprf)
      do i = 1, nprf
      rfnpfd(i) = float(i) * CUMUL
      enddo
*     --------------------------------------------------------------------------
*     compute the cut-off for zero for intermittent flows ----------------------
      cut off zero flow = 0.0 
	imark = 0
      do i = 1, nprf
      if ( imark .eq. 0 .and. rfnpvl(i) .gt. 1.0e-10 ) imark = i
	enddo
	if ( imark .gt. 1 ) then
	if ( imark .le. nprf ) then
      cut off zero flow = 0.5 * (rfnpfd (imark) + rfnpfd (imark - 1))
	else
      cut off zero flow = rfnpfd (nprf)
	endif
	endif
	if ( ifdiffuse .eq. 0 ) then
*	if ( nobigout .le. 0 ) write(01,2876)cut off zero flow,
*    &flnamesmall(3,icod)
 2876 format('Cut off percentile for added flow (input):',f7.4,1x,a64)
      else
*	if ( nobigout .le. 0 ) write(01,2896)cut off zero flow,
*    &flnamesmall(3,icod)
 2896 format(33x,'Cut off percentile for added flow (input):',f7.4,
     &1x,a64)
	endif

*     non-parametric distribution of discharge flow ----------------------------
*     sample the distributions -------------------------------------------------
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
*     get the normal deviate ---------------------------------------------------
      ednp = ERAN function (is)
	ERANR2(IS) = ednp
*     compute the value of the flow for this shot ------------------------------
      call get non parametric shot ( ednp, ef )
	ERANR(IS) = ednp
*     convert the deviate to a percentage point --------------------------------
      ednppc = TDIST2 (9999.0, ERANR(IS) )
*     substract the percentage point for the cut-off flow ----------------------
      ERANR2(IS) = amax1 (0.0, ednppc - cut off zero flow)
*     express the result as a new percentage -----------------------------------
      ERANR2(IS) = ERANR2(IS) / (1.0 - cut off zero flow)
	if ( ERANR2(IS) .gt. 1.0e-09 ) then
*     comvert to a deviate -----------------------------------------------------
      ERANR3(IS) = TDIST1 (9999.0, ERANR2(IS) )
	else
      ERANR3(IS) = 0.0
	endif
      EFshots (IS) = VALPF ( EF, EEFM, BM(1) )
      enddo

      if ( monf .gt. 1 ) then
      call write the shots for the flow from the discharge
      endif

      return

 7500 write( *,7168)flnamesmall(3,icod)
      write(01,7168)flnamesmall(3,icod)
      write(09,7168)flnamesmall(3,icod)
      write(33,7168)flnamesmall(3,icod)
 7168 Format(/77('-')/
     &'*** Error in non-parametric discharge flow data (5) '/
     &'*** Error in reading non-parametric data ...'/
     &'*** Check the file ',a64/77('-'))
      call stop
      end



*     compute the Monte-Carlo sampling errors for effluent flow ----------------
*     non-parametric data ------------------------------------------------------
*     --------------------------------------------------------------------------
      subroutine bias in non parametric discharge flows
      include 'COM.FOR'
      logical exists

      BM(1) = 0.0
      BS(1) = 0.0

*     identify the file with the non-parametric data ---------------------------
      do i = 1, M7
      icod = i
      if ( idenp ( 2, i, 1 ) .eq. IQ ) goto 2
      enddo

*     no valid code found for the datafile -------------------------------------
      write( *,3)IQ
      write(01,3)IQ
      write(09,3)IQ
      write(33,3)IQ
    3 format(77('-')/'*** Error in the discharge flow data ... ',i6/
     &'*** No valid code for the non-parametric dataset ...'/77('-'))
      call stop

*     valid code found ... check datafile exists -------------------------------
    2 continue

      Inquire( FILE = Flname(3,icod), EXIST = exists )

      if ( .NOT. exists) then
      write( *,7163) flnamesmall(3,icod)
      write(01,7163) flnamesmall(3,icod)
      write(09,7163) flnamesmall(3,icod)
      write(33,7163) flnamesmall(3,icod)
 7163 Format(/77('-')/
     &'*** Error in non-parametric discharge flow data (6)'/
     &'*** Non-parametric data file does not exist ... ',a64/
     &'*** Run halted ...'/77('-'))
      call stop
*     else
*     write(01,7863) flnamesmall(3,icod)
*     write(09,7863) flnamesmall(3,icod)
*     write(33,7863) flnamesmall(3,icod)
*7863 format(77('-')/
*    &'Non-parametric discharge flow data ... File: 'a64/77('-'))
      endif

*     open the file
      open(12,FILE = Flname(3,icod), STATUS='OLD')
*     read the file
      read(12, *, ERR=7500) nprf
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(3,icod) = 1
	endif
*     --------------------------------------------------------------------------
      kprf = nprf

*     check the number of data points - are there too many ? -------------------
      if ( nprf .gt. mprf) then
      write( *,7166) flnamesmall(3,icod)
      write(01,7166) flnamesmall(3,icod)
      write(09,7166) flnamesmall(3,icod)
      write(33,7166) flnamesmall(3,icod)
 7166 Format(/77('-')/
     &'*** Error in non-parametric discharge flow data (7) '/
     &'*** Too many data values specified for non-parametric  '/
     &'*** distribution. Check the file ',a64/77('-'))
      call stop
      endif

*     check the number of data points - are there too few ? --------------------

      if ( nprf .lt. 5 ) then
      write( *,7167) flnamesmall(3,icod)
      write(01,7167) flnamesmall(3,icod)
      write(09,7167) flnamesmall(3,icod)
      write(33,7167) flnamesmall(3,icod)
 7167 Format(/77('-')/
     &'*** Error in non-parametric discharge flow data (8) '/
     &'*** Too few data values specified for non-parametric   '/
     &'*** distribution. Check the file ',a64/77('-'))
      call stop
      endif

      backspace (12)

      read(12, *, ERR=7500) nprf, (rfnpvl(i),i=1 , kprf)
      close (12)
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(3,icod) = 1
	endif
      kprf = nprf
*     --------------------------------------------------------------------------

*     arrange the data in sequence ---------------------------------------------
      if ( flsequence(3,nonpd) .eq. 0 ) then
      do i = 1,   nprf-1
      do j = i+1, nprf
      if ( rfnpvl(i) .ge. rfnpvl(j) ) then
      xtemp = rfnpvl (j)
      rfnpvl (j) = rfnpvl (i)
      rfnpvl (i) = xtemp
	endif
      enddo
      enddo
      endif

*     compute cumulative frequencies and store them ----------------------------
      CUMUL = 1.0 / float (nprf)
      do  i = 1, nprf
      rfnpfd(i) = float(i) * CUMUL
      enddo

*     compute the cut-off for zero for intermittent flows ----------------------
      cut off zero flow = 0.0 
	imark = 0
      do i = 1, nprf
*     write(01,2633)i,rfnpvl(i),rfnpfd(i)
*2633 format(i3,2f12.6)
      if ( imark .eq. 0 .and. rfnpvl(i) .gt. 1.0e-10 ) imark = i
	enddo
	if ( imark .gt. 1 ) then
	if ( imark .le. nprf ) then
      cut off zero flow = 0.5 * (rfnpfd (imark) + rfnpfd (imark - 1))
	else
      cut off zero flow = rfnpfd (nprf)
	endif
	endif
*     --------------------------------------------------------------------------

*     non-parametric distribution of discharge flow ----------------------------
*     sample the distributions -------------------------------------------------
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
*     get the normal deviate ---------------------------------------------------
      ednp = ERAN function (is)
	ERANR2(IS) = ednp
*     compute the value of the flow for this shot ------------------------------
      call get non parametric shot ( ednp, EF )
*     --------------------------------------------------------------------------
      BM (1) = BM (1) + EF
      BS (1) = BS (1) + EF * EF
      enddo
      BS(1)=(BS(1)-BM(1)*BM(1)/NS)/(NS-1)
      if ( BS(1) .gt. 1.0e-10 ) then
      BS(1)=SQRoot(1249,BS(1))
      else
      BS(1) = 0.0
      endif
      BM(1)=BM(1)/NS
      return

 7500 write( *,7168)flnamesmall(3,icod)
      write(01,7168)flnamesmall(3,icod)
      write(09,7168)flnamesmall(3,icod)
      write(33,7168)flnamesmall(3,icod)
 7168 Format(77('-')/
     &'*** Error in non-parametric discharge flow data (8)'/
     &'*** Error in reading non-parametric data ...'/
     &'*** Check the file: ',a64/77('-'))

      call stop
      end











*     set up all the shots for monthly distributions of discharge flow ---------
      subroutine monthly discharge flow
      include 'COM.FOR'
      logical exists

*     deal with bifurcations ---------------------------------------------------
      if ( JT (KFEAT+1) .eq. 22) IQ = IF
      if ( JT (KFEAT+1) .eq. 23) IQ = IF

	EEFM = FE ( IQ, 1 ) ! mean -----------------------------------------------
	EEFS = FE ( IQ, 2 ) ! standard deviation ---------------------------------
      spcorrRFaf = FE ( IQ, 4 ) ! correlation of flow on added flow ------- ERAN

*     if ( MONQ .gt. 1 .and. JP .eq. ndetfirst ) then
*     *************************************************************************1
      if ( jp .ne. ndet+3 ) then

*     write(01,51)
*  51 format(77('-')/
*    &'Generated discharge flow data (defined monthly data)   '/77('-'))
*     call sort format 2 (EEFM,EEFS)
*     write(01,52) valchars10, FUNIT, valchars11, FUNIT
*  52 format('Input data:',24x,'          Mean =',a10,1x,a4/
*    &'           ',20x,'Standard deviation =',a10,1x,a4/77('-'))

*     identify the file with the monthly data ----------------------------------
*     loop on the number of monthly datafiles ----------------------------------

      do 1 i = 1, M8
      icod = i
      if ( iseasp ( 2, i, 1 ) .eq. IQ ) goto 2
    1 continue

*     no valid code found for the datafile 
      write( *,3)
      write(01,3)
      write(09,3)
      write(33,3)
    3 format(/77('-')/'*** Error in monthly discharge flow data ...'/
     &'*** No valid code for the monthly dataset ...'/77('-'))
      call stop

*     Valid code found. Check the monthly datafile exists ...
    2 continue
      Inquire( FILE = flmonth(3,icod), EXIST = exists )

*     ==========================================================================
      if ( .NOT. exists) then
      write( *,7163) FLMONTHsmall(3,icod)
      write(01,7163) FLMONTHsmall(3,icod)
      write(09,7163) FLMONTHsmall(3,icod)
      write(33,7163) FLMONTHsmall(3,icod)
 7163 Format(/77('-')/
     &'*** Error in monthly discharge flow data ...'/
     &'*** The data file does not exist ... ',a64/
     &'*** Run halted ...'/77('-')/)
      call stop
*     else
*     if ( JP .eq. ndetfirst) then
*     write(01,7863) FLMONTHsmall(3,icod)
*     write(09,7863) FLMONTHsmall(3,icod)
*     write(33,7863) FLMONTHsmall(3,icod)
*7863 format(/77('-')/
*    &'Monthly data on discharge flows ... ',
*    &'File: ',a64/77('-'))
*     endif
	endif
*     ==========================================================================

*     The file exists.  Open it ...

      open(11,FILE = flmonth(3,icod), STATUS='OLD')








*     **************************************************************************
*     data on abstractions -----------------------------------------------------
*     **************************************************************************
*     19 - an abstraction which removes a set distribution of flow feature. A 
*          sort of negative discharge. The distribution to be abstracted is
*          entered with the effluent discharge data sets 
*     **************************************************************************
      if ( JT (KFEAT) .eq. 19) then
      write(01,2398)
 2398 format(//77('-')/
     &'Monthly data on abstractions ...'/77('-')/
     &'Month        ','       Mean','   Standard','      Shift',
     &'  Correlation'/
     &'             ','           ','  Deviation'/77('-'))
*     read the file containing the monthly data --------------------------------
      call read monthly data (3,icod) ! abstraction - type 5
      write(01,2399)(seas1(i),seas2(i),seas3(i),seas4(i),i=1 ,12)
*     **************************************************************************

*     other data  --------------------------------------------------------------

      else

*     data on discharge flows ==================================================
      if ( JP .eq. ndetfirst) then
      write(01,4398)
 4398 format('Month        ','       Mean','   Standard','      Shift',
     &'  Correlation'/25x,'  Deviation'/77('-'))
      endif

*     read the file containing the monthly data --------------------------------
      call read monthly data (3,icod) ! discharge flow - type 5

      if ( JP .eq. ndetfirst) then
      write(01,2399)(seas1(i),seas2(i),seas3(i),seas4(i),seas0(i),
     &i=1 ,12)
 2399 format(
     &'January ...  ',3f11.2,f13.4,i18/
     &'February ... ',3f11.2,f13.4,i18/
     &'March ...    ',3f11.2,f13.4,i18/
     &'April ...    ',3f11.2,f13.4,i18/
     &'May ...      ',3f11.2,f13.4,i18/
     &'June ...     ',3f11.2,f13.4,i18/
     &'July ...     ',3f11.2,f13.4,i18/
     &'August ...   ',3f11.2,f13.4,i18/
     &'September ...',3f11.2,f13.4,i18/
     &'October ...  ',3f11.2,f13.4,i18/
     &'November ... ',3f11.2,f13.4,i18/
     &'December ... ',3f11.2,f13.4,i18/77('-')/)
      endif
      endif
*     *************************************************************************2

*     calculate the Monte-Carlo sampling errors for montly data on discharge ---
*     flow.  Loop through the months -------------------------------------------

*     write(01,43)
*  43 format(/77('-')/
*    &'Calculated values for discharge flows in individual months ...'/
*    &77('-'))

	do 13 imon = 1, 12

*     special version of BIAS for monthly data ---------------------------------
      call bias and values of monthly discharge flows (imon)

      if ( BSM(imon) .gt. 1.0E-08)
     &BSM(imon) = seas1(imon) / BSM(imon)
      if ( BSS(imon) .gt. 1.0E-08) then
      BSS(imon) = seas2(imon)/BSS(imon)
      else
      BSS(imon) = 1.0
      endif
      if ( BSS(imon) .lt. 1.0E-08) BSS(imon) = 1.0
*     if ( MONQ .gt. 1 .and. JP .eq. ndetfirst ) then
*     if ( imon .eq. 1 ) write(01,12)
*  12 format(/
*    &'Correction factors for sampling errors on discharge ',
*    &'flow data:'/)
*     write(01,92)imon,BSM(imon),BSS(imon)
*  92 format(
*    &'Monthly Mean',i3,' =',F8.3,
*    &'   Standard deviation =',F8.3)
*     endif
   13 continue

*     create the distributions of monthly discharge flows ----------------------
      do 22 is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      EFM = seas1 (imonth) ! mean ----------------------------------------------
      EFS = seas2 (imonth) ! standard deviiation -------------------------------
      EF3 = seas3 (imonth) ! shift flow ----------------------------------------
      spcorrRFaf = seas4 (imonth) ! correlation of flow on added flow ----- ERAN
	EM3 = ( EFM + EF3 ) * ( EFM + EF3 )

*     mean and standard deviation of logged data -------------------------------
      GEFM = 0.0
      GEFS = 0.0
      if ( EM3 .gt. 1.0e-9) then
      GEFM = ALOG ( EM3 / SQRoot(1247, EM3 + EFS * EFS ) )
      GEFS = SQRoot(1248, ALOG ( 1.0 + ( EFS * EFS ) / EM3 ) )
      endif

*     get the normal deviate ---------------------------------------------------
      edmm = ERAN function (is)
	ERANR2(IS) = edmm
      EF = exp ( edmm * GEFS + GEFM ) + EF3
*     compute the value of the flow shot ---------------------------------------
      EFshots (IS) = VALPF ( EF, EFM, BSM(imonth) )
   22 continue

      endif
      return

 7500 write( *,7168)FLMONTHsmall(3,icod)
      write(01,7168)FLMONTHsmall(3,icod)
      write(09,7168)FLMONTHsmall(3,icod)
      write(33,7168)FLMONTHsmall(3,icod)
      call stop
 7168 Format(77('-')/'*** Error in monthly discharge flow data ...'/
     &'*** Error in reading monthly data ... '/
     &'*** Check the file ',a64/77('-'))
      end


*     compute Monte Carlo sampling errors for monthly discharge flow data
      subroutine bias and values of monthly discharge flows (imon)
      include 'COM.FOR'

      BSM (imon) = 1.0
      BSS (imon) = 1.0
      do IS = 1, NS
      EFshots(IS) = 0.0
      enddo      

      do 2 is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
	jmonth = imonth
      if ( jmonth .ne. imon ) goto 2

      EFM = seas1 (imonth) ! mean ----------------------------------------------
      EFS = seas2 (imonth) ! standard deviiation -------------------------------
      EF3 = seas3 (imonth) ! shift flow ----------------------------------------
      spcorrRFaf = seas4 (jmonth) ! correlation of flow on added flow ----- ERAN

*     mean and standard deviation of logged data -------------------------------
      GEFM = 0.0
      GEFS = 0.0
      EM3 = (EFM + EF3) * (EFM + EF3)
      if ( EM3 .gt. 1.0e-9) then
      GEFM = ALOG ( EM3 / SQRoot(1260, EM3 + EFS * EFS ) )
      GEFS = SQRoot(1261, ALOG ( 1.0 + ( EFS * EFS ) / EM3 ) )
      endif

*     get the normal deviate ---------------------------------------------------
      edmm = ERAN function (is)
	ERANR2(IS) = edmm
*     call get the correlated random numbers (IS, R1, R2, R3, R4)
*     compute the value of the shot --------------------------------------------
      EF = exp ( edmm * GEFS + GEFM ) + EF3
	EFshots ( IS ) = EF
    2 continue

      call statistics for one month of discharge flows ( imon,EM1,EM2)
      BSM (imon) = EM1
      BSS (imon) = EM2

      return
      end












*     set up all the shots for -------------------------------------------------
*     monthly structure distributions of discharge flow ------------------------
*     this routine is called from Mass Balance ---------------------------------
      subroutine generate monthly structure for discharge flows 2
      include 'COM.FOR'

      do is = 1, NS
	YY(is) = 0.0
      EFshots (is) = 0.0
	enddo

      if ( JT (KFEAT+1) .eq. 22) IQ = IF
      if ( JT (KFEAT+1) .eq. 23) IQ = IF

*     annual mean and standard deviation ---------------------------------------
	EEFM = FE ( IQ, 1 )
	if ( EEFM .lt. 1.0e-10) return
	EEFS = FE ( IQ, 2 )
	if ( EEFS .lt. 1.0e-10) return
	EE3 = FE ( IQ, 3 )

*     correlation coefficient --------------------------------------------------
*     flow on added flow -------------------------------------------------------
      tcorr = FE ( IQ,4 )

      if ( MONF .eq. 1 ) then
      if ( JP .eq. ndetlast ) then
      write(01,51)
      write(34,51)
   51 format(77('-')/
     &'Generated discharge flow data (imposed monthly structure)'/
     &77('-'))
      call sort format 2 (EEFM,EEFS)
      write(01,52) valchars10, FUNIT, valchars11, FUNIT
      write(34,52) valchars10, FUNIT, valchars11, FUNIT
   52 format('Input data:',24x,'          Mean =',a10,1x,a4/
     &'           ',20x,'Standard deviation =',a10,1x,a4)
      endif
	endif

      tmean = EEFM
	tstdev = EEFS
	t3 = EE3

*     set log-normal distributions by default ----------------------------------
      do i = 1,12
      struct1(i) = tmean
	struct2(i) = tstdev
*     struct0(i) = 2
      struct3(i) = t3
	struct4(i) = tcorr
	enddo
      tratio = tstdev / tmean

      if ( JT (kfeat) .eq. 3 .or. JT (kfeat) .eq. 12 ) then
      struct1( 1) = 1.3
      struct1( 2) = 1.2
      struct1( 3) = 1.0
      struct1( 4) = 1.0
      struct1( 5) = 0.8
      struct1( 6) = 0.8
      struct1( 7) = 0.8
      struct1( 8) = 0.8
      struct1( 9) = 0.8
      struct1(10) = 1.0
      struct1(11) = 1.2
      struct1(12) = 1.3
      do i = 1,12
      struct1 (i) = struct1 (i) * tmean
      struct2 (i) = struct1 (i) * tratio
      struct3(i) = 0.0
	struct4(i) = tcorr
	enddo
	endif

      test1 = 0.0
      test2 = 0.0
	do imon = 1, 12
      test1 = test1 + struct1(imon) 
	enddo
	test1 = test1 / 12.0
	test1 = test1 - tmean

      spcorrRFaf = tcorr ! correlation of flow on added flow -------------- ERAN

*     set up a first set of shotes for subsequent adjustment ------------------- 
*     sample the distributions of data on discharge flow -----------------------
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      EFshots (is) = 0.0
      EFM = struct1 (imonth) ! mean discharge flow -----------------------------
	if ( EFM .gt. 1.0e-9 ) then
      EFS = struct2 (imonth) ! mean discharge flow and standard deviation ------
      EF3 = struct3 (imonth) ! shift flow --------------------------------------
      spcorrRFaf = struct4 (imonth) ! correlation of flow on added flow --- ERAN

*     mean and standard deviation for logged variables -------------------------
      GEFM = 0.0
      GEFS = 0.0
      EM3 = (EFM + EF3) * (EFM + EF3)
      if ( EM3 .gt. 1.0e-9) then
      GEFM = ALOG ( EM3 / SQRoot(150001,EM3+EFS*EFS) )
      xxxy = ALOG (1.0+(EFS*EFS)/EM3)
      if ( xxxy .lt. 1.0e-20 ) then
      GEFS = 0.0
      else
      GEFS = SQRoot3(102301, ALOG (1.0+(EFS*EFS)/EM3) )
      endif
      endif

*     monthly structure distributions of discharge flow ------------------------
*     get the normal deviate ---------------------------------------------------
      edms2 = ERAN function (is)
	ERANR2(IS) = edms2
      EFshots (IS) = exp ( edms2 * GEFS + GEFM ) - EF3
	endif
      enddo

*     --------------------------------------------------------------------------
      call statistics for monthly discharge flows ( 0, CM1, CM2 )
      do imon = 1, 12
      BSM(imon) = tmean  / CM1 
	BSS(imon) = tstdev / CM2 
	enddo

*     check feasibility of monthly structure -----------------------------------
      if ( BSS(1) .lt. 0.5 ) then
      if ( JP .eq. ndetlast ) then
	if ( nobigout .le. 0 ) write(01,5388)uname(feeture)
 5388 format(110('+')/
     &'Unreasonable monthly structure provided for discharge flows ...',
     &' Retained the annual structure ...'/
     &'The variation in the monthly means is too small for ',a40/
     &110('+'))
	write(33,5388)uname(feeture)
	call change colour of text (22) ! light blue
	if ( JSKIP .eq. 0 ) write( *,5588)uname(feeture),IQ
 5588 format('* Poor monthly data for ',
     &'discharge flows',12x,'...',7x,a40,4x,'Set number',i7,1x,7('.'))
      call set screen text colour
	endif
	return
	endif

*     iterate to get to monthly shots that match the annual --------------------
      iterate = 0
 5555 continue
      iterate = iterate + 1

*     sample the distributions of data on river quality ------------------------
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      EFshots (is) = 0.0
      EFM = struct1 (imonth) * BSM (1) ! mean discharge flow -------------------
	if ( EFM .gt. 1.0e-9 ) then
      EFS = struct2 (imonth) * BSS (1) ! standard deviation --------------------
      EF3 = struct3 (imonth) ! shift flow --------------------------------------
      spcorrRFaf = struct4 (imonth) ! correlation of flow on added flow --- ERAN

	GEFM = 0.0 ! mean for logged variables -----------------------------------
      GEFS = 0.0 ! standard deviation for logged variables ---------------------

      EM3 = (EFM + EF3) * (EFM + EF3)
      if ( EM3 .gt. 1.0e-9) then
      GEFM = ALOG ( EM3 / SQRoot(150002,EM3+EFS*EFS) )
      xxxy = ALOG (1.0+(EFS*EFS)/EM3)
      if ( xxxy .lt. 1.0e-20 ) then
      GEFS = 0.0
      else
      GEFS = SQRoot3(102302, ALOG (1.0+(EFS*EFS)/EM3) )
      endif
      endif

*     get the normal deviate ---------------------------------------------------
*     monthly structure distributions of discharge flow ------------------------
      edms2 = ERAN function (is)
	ERANR2(IS) = edms2
*     call get the correlated random numbers (IS,R1,R2,R3,R4)
*     compute the value of the shot --------------------------------------------
      EFshots (IS) = Vlogn month ( edms2,GEFS,GEFM,EF3,EFM,EFS)
      EFshots (IS) = amax1 (0.0, EFshots (IS))
	endif
      enddo
*     --------------------------------------------------------------------------
      call statistics for monthly discharge flows ( 0, CM1, CM2 )
*     write(01,4128)tmean,tstdev,NS
*4128 format('True quality tmean,tstdev ... ',2f11.2,i11/77('-'))
*     write(01,4499)CM1,CM2
*4499 format('CM1 CM2 mean and st dev ...   ',2f11.5)
*     write(01,4799)BSM(1),BSS(1),iterate
*4799 format('BSM and BSS .1.',2f11.5,i7)

      BSM(1) = BSM(1) * tmean  / CM1 
 	BSS(1) = BSS(1) * tstdev / CM2 
      if ( BSS(1) .lt. 0.5 ) then
	if ( JP .eq. ndetlast ) then
	write(01,5388)uname(feeture)
	write(33,5388)uname(feeture)
	call change colour of text (22) ! light blue
	write( *,5588)uname(feeture),IQ
      call set screen text colour
	endif
	return
	endif

      rat1 = CM1 / tmean
	rat2 = CM2 / tstdev
      if ( rat1 .lt. 1.0001 .and. rat1 .gt. 0.9999) then
      if ( rat2 .lt. 1.0001 .and. rat2 .gt. 0.9999) goto 5558
      if ( iterate .ne. 100 ) goto 5555
      else
      if ( iterate .ne. 100 ) goto 5555
	endif
 5558 continue

      if ( iterate .gt. 99 ) then
      if ( KSIM .lt. 1 .and. jp1 .eq. 1 ) then
      if ( ifbatch .ne. 1 ) then 
  	call change colour of text (14) ! bright yellow
	write( *,5566)IQ
 5566 format('Unable to set up the specified ',
     &'monthly structure of discharge flow (2) ...',i6)
      call set screen text colour
      endif
	write(33,5266)IQ
	write(01,5266)IQ
	write(09,5266)IQ
 5266 format(100('-')/'Unable to set up the specified ',
     &'monthly structure of discharge flow (2) ...',i6/100('-'))
      if ( rat1 .lt. 1.0001 .and. rat1 .gt. 0.9999) then
      if ( ifbatch .ne. 1 ) write( *,5366)uname(kfeat),IQ
      write(33,5366)uname(kfeat),IQ
	write(01,5366)uname(kfeat),IQ
      write(09,5366)uname(kfeat),IQ
 5366 format('The annual mean is correct ... ',
     &'The variation between the monthly means is ',
     &'too big for the ',
     &'declared annual standard deviation ... Retained a constant'/
     &'quality within each month ... Feature: ',a20,' Set:',i6/100('-'))
	endif
	endif
	endif

      call statistics for monthly discharge flows ( 0, CM1, CM2 )

*     write(01,4499)CM1,CM2
*     write(01,4699)BSM(1),BSS(1),iterate
*     write(01,4128)tmean,tstdev,NS
*     --------------------------------------------------------------------------
*     if ( MONF .gt. 1 ) call write shots for discharge flow

      return

 7500 write( *,7168) FLSTRUCTsmall(3,icod)
      write(01,7168) FLSTRUCTsmall(3,icod)
      write(09,7168) FLSTRUCTsmall(3,icod)
      write(33,7168) FLSTRUCTsmall(3,icod)
      call stop
 7168 Format(/77('-')/'*** Error in monthly discharge flow data ...'/
     &'*** Error in reading monthly data for monthly structure ...'/
     &'*** Check the file ',a64/77('-'))
      end



*     set up all the shots for -------------------------------------------------
*     monthly structure distributions of discharge flow ------------------------
*     this routine is called from Mass Balance ---------------------------------

      subroutine generate monthly structure for discharge flows 8
      include 'COM.FOR'
      logical exists

      do is = 1, NS
	YY(is) = 0.0
      EFshots (is) = 0.0
	enddo

      if ( JT (KFEAT+1) .eq. 22) IQ = IF
      if ( JT (KFEAT+1) .eq. 23) IQ = IF

	EEFM = FE ( IQ, 1 ) ! mean -----------------------------------------------
	if ( EEFM .lt. 1.0e-10) return
	EEFS = FE ( IQ, 2 ) ! standard deviiation --------------------------------
	if ( EEFS .lt. 1.0e-10) return
	EE3 = FE ( IQ, 3 )

      spcorrRFaf = FE ( IQ,4 ) ! correlation of flow on added flow -------- ERAN

*     set log-normal distributions by default ----------------------------------
      do i = 1,12
      struct1(i) = EEFM
	struct2(i) = EEFS
      struct0(i) = 2
      struct3(i) = EE3
      struct4(i) = spcorrRFaf ! correlation of flow on added flow --------- ERAN
	enddo

*     if ( JP .eq. ndetlast ) then
*     write(01,51)
*  51 format(77('-')/
*    &'Generated discharge flow data (monthly structure)   '/77('-'))
*     call sort format 2 (EEFM,EEFS)
*     write(01,52) valchars10, FUNIT, valchars11, FUNIT
*  52 format('Input data:',24x,'          Mean =',a10,1x,a4/
*    &'           ',20x,'Standard deviation =',a10,1x,a4/77('-'))
*     endif

*     identify the file with the monthly monthly structure on discharge flow ---
      do 1 i = 1, M9
      icod = i
      if ( istruct ( 2, i, 1 ) .eq. IQ ) goto 2
    1 continue

*     no valid code found for the datafile -------------------------------------
      write( *,3)
      write(01,3)
      write(09,3)
      write(33,3)
    3 format(77('-')/'*** Error in the monthly structure for discharge',
     &' flow ...'/'*** No valid code for the set of data ...'/77('-'))
      call stop

*     Valid code found. Check the monthly structure datafile exists ------------
    2 continue
      Inquire( FILE = FLSTRUCT(3,icod), EXIST = exists )
      if ( .NOT. exists) then
      write( *,7163) FLSTRUCTsmall(3,icod)
      write(01,7163) FLSTRUCTsmall(3,icod)
      write(09,7163) FLSTRUCTsmall(3,icod)
      write(33,7163) FLSTRUCTsmall(3,icod)
 7163 Format(/77('-')/
     &'*** Error in monthly structure for discharge flow (a)'/
     &'*** The data file does not exist ... ',a64/
     &'*** Run halted ...'/77('-')/)
      write( *,7009) icod,FLSTRUCTsmall(3,icod),FLSTRUCT(3,icod)
      write(01,7009) icod,FLSTRUCTsmall(3,icod),FLSTRUCT(3,icod)
 7009 format(i3,2x,a64,a80)
      call stop
      else
      if ( JP .eq. ndetfirst) then
*     write(01,7863) FLSTRUCTsmall(3,icod)
*     write(09,7863) FLSTRUCTsmall(3,icod)
      write(33,7863) FLSTRUCTsmall(3,icod)
 7863 format(
     &'The data have a defined monthly structure ... ',
     &'File: ',a64/77('-'))
      endif
	endif
*     ==========================================================================

*     The file exists.  Open it ...

      open(11,FILE = FLSTRUCT(3,icod), STATUS='OLD')

*     **************************************************************************
*     data on abstractions -----------------------------------------------------
*     **************************************************************************
*     19 - an abstraction which removes a set distribution of flow feature. A 
*          sort of negative discharge. The distribution to be abstracted is
*          entered with the effluent discharge data sets 
*     **************************************************************************
      if ( JT (KFEAT) .eq. 19) then
      write(01,2398)
 2398 format(//77('-')/
     &'Monthly structure on abstractions ...'/77('-')/
     &'Month        ','       Mean','   Standard','      Shift',
     &'  Correlation'/
     &'             ','           ','  Deviation'/77('-'))
*     read the file containing the monthly data --------------------------------
*     read the file containing the monthly structure ----------------------
      call read monthly structure ! abstractions ! type 8
     &(3,0,icod,tmean,tstdev,t3,tcorr,itest12)
	if ( itest12 .eq. 12 ) then
      write( *,4311) FLSTRUCTsmall(3,icod)
      write(33,4311) FLSTRUCTsmall(3,icod)
      write(01,4311) FLSTRUCTsmall(3,icod)
      write(09,4311) FLSTRUCTsmall(3,icod)
 4311 format(77('*')/
     &'*** Null monthly structure has escaped detection ...'/
     &'*** File: ',a64/77('*'))
	endif
*     **************************************************************************


*     other data  --------------------------------------------------------------
      else


*     data on discharge flows ==================================================
*     read the file containing the monthly structure ----------------------
      call read monthly structure ! effluent flow - type 8
     &(3,0,icod,tmean,tstdev,t3,tcorr,itest12)
	if ( itest12 .eq. 12 ) then
      write( *,4321) FLSTRUCTsmall(3,icod)
      write(33,4321) FLSTRUCTsmall(3,icod)
      write(01,4321) FLSTRUCTsmall(3,icod)
      write(09,4321) FLSTRUCTsmall(3,icod)
 4321 format(77('*')/
     &'*** Null monthly structure has escaped detection ...'/
     &'*** File: ',a64/77('*'))
	endif

      spcorrRFaf = tcorr ! correlation of flow on added flow -------------- ERAN

*     set up a first set of shotes for subsequent adjustment ------------------- 
*     sample the distributions of data on discharge flow -----------------------
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      EFshots (is) = 0.0
      EFM = struct1 (imonth) ! mean discharge flow -----------------------------
	if ( EFM .gt. 1.0e-9 ) then
      EFS = struct2 (imonth) ! standard deviation ------------------------------
      EF3 = struct3 (imonth) ! shift flow --------------------------------------
      spcorrRFaf = struct4 (imonth) ! correlation of flow on added flow --- ERAN

*     mean and standard deviation for logged variables -------------------------
	GEFM = 0.0
      GEFS = 0.0

      EM3 = (EFM + EF3) * (EFM + EF3)
      if ( EM3 .gt. 1.0e-9) then
      GEFM = ALOG ( EM3 / SQRoot(150003,EM3+EFS*EFS) )
      GEFS = SQRoot(102303, ALOG (1.0+(EFS*EFS)/EM3) )
      endif

*     get the normal deviate ---------------------------------------------------
*     monthly structure distributions of discharge flow ------------------------
      edms8 = ERAN function (is)
	ERANR2(IS) = edms8
*     call get the correlated random numbers (IS,R1,R2,R3,R4)
      EFshots (IS) = exp ( edms8 * GEFS + GEFM ) - EF3
	endif
      enddo

      call statistics for monthly discharge flows ( 0, CM1, CM2 )
*     write(01,4228)tmean,tstdev,NS
*4228 format('True quality ',2f11.2,i11/46('-'))
      do imon = 1, 12
      BSM(imon) = tmean  / CM1 
	BSS(imon) = tstdev / CM2 
	enddo
*     write(01,4699)BSM(1),BSS(1)
*4399 format('Bias ---     ',2f11.5)

*     iterate to get to monthly shots that match the annual --------------------
      iterate = 0
 5555 continue
      iterate = iterate + 1

*     sample the distributions of data on river quality ------------------------
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      EFshots (is) = 0.0
      EFM = struct1 (imonth) * BSM (1) ! mean discharge flow -------------------
	if ( EFM .gt. 1.0e-9 ) then
      EFS = struct2 (imonth) * BSS (1) ! standard deviation --------------------
      EF3 = struct3 (imonth) ! shift flow --------------------------------------
      spcorrRFaf = struct4 (imonth) ! correlation of flow on added flow --- ERAN

	GEFM = 0.0 ! mean for logged variables -----------------------------------
      GEFS = 0.0 ! standard deviation for logged variables ---------------------
      EM3 = (EFM + EF3) * (EFM + EF3)
      if ( EM3 .gt. 1.0e-9) then
      GEFM = ALOG ( EM3 / SQRoot(150004,EM3+EFS*EFS) )
      GEFS = SQRoot(102304, ALOG (1.0+(EFS*EFS)/EM3) )
      endif

*     get the normal deviate ---------------------------------------------------
*     monthly structure distributions of discharge flow ------------------------
      edms8 = ERAN function (is)
	ERANR2(IS) = edms8
*     call get the correlated random numbers (IS,R1,R2,R3,R4)
*     compute the value of the shot --------------------------------------------
      EFshots (IS) = Vlogn month ( edms8,GEFS,GEFM,EF3,EFM,EFS)
      EFshots (IS) = amax1 (0.0, EFshots (IS))
	endif
      enddo
*     --------------------------------------------------------------------------
      call statistics for monthly discharge flows ( 0, CM1, CM2 )
*     write(01,4128)tmean,tstdev,NS
*4128 format('True quality ',2f11.2,i11/77('-'))
*     write(01,4499)CM1,CM2
*4499 format('Bias ...',2f12.6)
*     write(01,4699)BSM(1),BSS(1),iterate
*4699 format('Correction 11',2f11.5,i7)

      BSM(1) = BSM(1) * tmean  / CM1 
	BSS(1) = BSS(1) * tstdev / CM2 

      rat1 = CM1 / tmean
	rat2 = CM2 / tstdev
      if ( rat1 .lt. 1.0001 .and. rat1 .gt. 0.9999) then
      if ( rat2 .lt. 1.0001 .and. rat2 .gt. 0.9999) goto 5558
      if ( iterate .ne. 100 ) goto 5555
      else
      if ( iterate .ne. 100 ) goto 5555
	endif
 5558 continue
      if ( iterate .gt. 99 ) then
	if ( ifbatch .ne. 1 ) then
	call change colour of text (22) ! light blue
	write( *,5866)IQ,FLSTRUCTsmall(3,icod)
 5866 format('* Failure to set up the specified ',
     &'monthly structure of discharge flow',i4,3x,a64)
      call set screen text colour
	endif
	write(33,5266)IQ,FLSTRUCTsmall(3,icod)
 5266 format(100('-')/'Failure in setting up the specified ',
     &'monthly structure of discharge flow',i4,3x,a64/100('-'))
	endif
      call statistics for monthly discharge flows ( 0, CM1, CM2 )
*     write(01,4499)CM1,CM2
*     write(01,4699)BSM(1),BSS(1),iterate
*     write(01,4128)tmean,tstdev,NS

*     if ( MONF .gt. 1 ) call write shots for discharge flow
      endif

      return

 7500 write( *,7168) FLSTRUCTsmall(3,icod)
      write(01,7168) FLSTRUCTsmall(3,icod)
      write(09,7168) FLSTRUCTsmall(3,icod)
      write(33,7168) FLSTRUCTsmall(3,icod)
      call stop
 7168 Format(/77('-')/'*** Error in monthly discharge flow data ...'/
     &'*** Error in reading monthly data for monthly structure ...'/
     &'*** Check the file ',a64/77('-'))
      end




*     set up all the shots for -------------------------------------------------
*     ... a monthly structure distribution of discharge quality ----------------
*     this routine is called from Mass Balance ---------------------------------
      subroutine generate monthly structure for discharge quality
      include 'COM.FOR'
      logical exists

	EECM = pollution data (IQ,JP,1) ! mean effluent quality ------------------
	EECS = pollution data (IQ,JP,2) ! standard deviation ---------------------
      spcorrfa = pollution data(IQ,jp,4) ! effluent flow on its quality --- cRAN

*     identify the file with the monthly monthly structure on river quality ----
      do 1 i = 1, M9
      icod = i
      if ( istruct ( 2, i, JP + 1 ) .eq. IQ ) goto 2
    1 continue

*     no valid code is found for the data file ---------------------------------
      write( *,3)
      write(01,3)
      write(09,3)
      write(33,3)
    3 format(77('-')/'*** Error in monthly structure of discharge ',
     &'quality ...'/
     &'*** No valid code for the dataset ...'/
     &77('-'))
      call stop

*     a valid code has been found. Check the datafile exists -------------------
    2 continue
      inquire( FILE = FLSTRUCT(4,icod), EXIST = exists )
      if ( .NOT. exists) then
      write( *,7163) FLSTRUCTsmall(4,icod),icod,IQ,
     &istruct(2,icod,JP+1)
      write(01,7163) FLSTRUCTsmall(4,icod),icod,IQ,
     &istruct(2,icod,JP+1)
      write(09,7163) FLSTRUCTsmall(4,icod),icod,IQ,
     &istruct(2,icod,JP+1)
      write(33,7163) FLSTRUCTsmall(4,icod),icod,IQ,
     &istruct(2,icod,JP+1)
 7163 format(/77('-')/
     &'*** Error in monthly structure of discharge quality ... '/
     &'*** The data file does not exist ... ',a64/
     &'*** Run halted ... Code =',3i4/77('-'))
      call stop
*     else
*     write( *,7863) Dname(jp),FLSTRUCTsmall(4,icod)
*     write(01,7863) Dname(jp),FLSTRUCTsmall(4,icod)
*     write(09,7863) Dname(jp),FLSTRUCTsmall(4,icod)
*     write(33,7863) Dname(jp),FLSTRUCTsmall(4,icod)
*7863 format(77('-')/
*    &'Monthly structure on discharge quality ... ',a11/
*    &'File: ',a64/77('-'))
      endif

*     open the file ------------------------------------------------------------
      open(11,FILE = FLSTRUCT(4,icod), STATUS='OLD')

*     read the file containing the monthly structure ---------------------------
      call read monthly structure ! effluent quality - type 8
     &(4,0,icod,tmean,tstdev,t3,tcorr,itest12)
	if ( itest12 .eq. 12 ) then
      write( *,4311) FLSTRUCTsmall(4,icod)
      write(33,4311) FLSTRUCTsmall(4,icod)
      write(01,4311) FLSTRUCTsmall(4,icod)
      write(09,4311) FLSTRUCTsmall(4,icod)
 4311 format(77('*')/
     &'*** Null monthly structure has escaped detection ...'/
     &'*** File: ',a64/77('*'))
	endif

*     set up a first set of shots for subsequent adjustment -------------------- 
*     sample the distributions of data on discharge quality --------------------
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      ECshots(is) = 0.0
	NBS = 0
	if ( EFshots(IS) .gt. 1.0e-09 ) then

      ECM = struct1 (imonth) ! mean effluent quality ---------------------------
      ECS = struct2 (imonth) ! standard deviation ------------------------------
      ECC = struct3 (imonth)
      spcorrfa = struct4 (imonth) ! effluent flow on effluent quality ----- cRAN

*     mean and standard deviation for logged variables -------------------------
	GECM = 0.0
	GECS = 0.0

	EM3 = ( ECM + EC3 ) * ( ECM + EC3 )
      if ( EM3 .gt. 1.0e-9) then
      GECM = ALOG ( EM3 / SQRoot(1258, EM3 + ECS * ECS ) )
      GECS = SQRoot(1259, ALOG ( 1.0 + (ECS*ECS )/EM3 ) )
	endif

*     get the normal deviate for monthly structured discharge quality ----------
      js = JSET ( IQ, NS, is )
      cdms = cRAN function ( JP, js, is ) ! added flow on added quality --- cRAN
*     call get the correlated random numbers (IS, R1, R2, R3, R4)
*     compute the value of the  shot -------------------------------------------
      ECshots(is) = exp ( cdms * GECS + GECM ) - EC3
	endif
      enddo

      call statistics for monthly discharge quality ( 0, CM1, CM2 )
*     write(01,4228)tmean,tstdev,NS
*4228 format('True quality ',2f11.2,i11/46('-'))
      do imon = 1, 12
      BSM(imon) = tmean  / CM1 
	BSS(imon) = tstdev / CM2 
	enddo
*     write(01,4399)BSM(1),BSS(1)
*4399 format('Bias ---     ',2f11.5)

*     iterate to get to monthly shots that match the annual --------------------
      iterate = 0
 5555 continue
      iterate = iterate + 1

*     sample the distributions of data on effluent quality ---------------------
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      ECshots(is) = 0.0
      if ( EFshots (is) .gt. 1.0e-09 ) then

      ECM = struct1 (imonth) * BSM (1) ! mean effluent quality -----------------
      ECS = struct2 (imonth) * BSS (1) ! standard deviation --------------------
      EC3 = struct3 (imonth) ! shift -------------------------------------------
      spcorrfa = struct4 (imonth) ! effluent flow on effluent quality ----- cRAN

	GECM = 0.0 ! mean for logged variables -----------------------------------
	GECS = 0.0 ! standard deviation for logged variables ---------------------
	EM3 = ( ECM + EC3 ) * ( ECM + EC3 )
      if ( EM3 .gt. 1.0e-9) then
      GECM = ALOG ( EM3 / SQRoot(1258, EM3 + ECS * ECS ) )
      GECS = SQRoot(1259, ALOG ( 1.0 + (ECS*ECS )/EM3 ) )
	endif

*     get the normal deviate for monthly structured discharge quality ----------
      js = JSET ( IQ, NS, is )
      cdms = cRAN function ( JP, js, is ) ! added flow on added quality --- cRAN
*     compute the value of the shot --------------------------------------------
      ECshots(is) = Vlogn month ( cdms,GECS,GECM,EC3,ECM,ECS)
      ECshots(is) = amax1 (0.0, ECshots(is))
      endif
	enddo
*     --------------------------------------------------------------------------
      call statistics for monthly discharge quality ( 0, CM1, CM2 )
*     write(01,4128)tmean,tstdev,NS
*4128 format('True quality ',2f11.2,i11/46('-'))
*     write(01,4499)CM1,CM2
*4499 format('Bias ...',2f12.6)
*     write(01,4699)BSM(1),BSS(1),iterate
*4699 format('Correction discharge quality',2f11.5,i7)

      BSM(1) = BSM(1) * tmean  / CM1 
	BSS(1) = BSS(1) * tstdev / CM2 

      rat1 = CM1 / tmean
	rat2 = CM2 / tstdev
      if ( rat1 .lt. 1.0001 .and. rat1 .gt. 0.9999) then
      if ( rat2 .lt. 1.0001 .and. rat2 .gt. 0.9999) goto 5558
      if ( iterate .ne. 100 ) goto 5555
      else
      if ( iterate .ne. 100 ) goto 5555
	endif
 5558 continue
      if ( iterate .gt. 99 ) then
	if ( ifbatch .ne. 1 ) then
	call change colour of text (22) ! light blue
	write( *,5866)IQ,FLSTRUCTsmall(4,icod)
 5866 format('* Failed to set up ',
     &'monthly structure of discharge quality',i4,3x,a64/100('-'))
      call set screen text colour
	endif
	write(33,5266)IQ,FLSTRUCTsmall(4,icod)
 5266 format(100('-')/'Failure to set up the specified ',
     &'monthly structure of discharge quality',i4,3x,a64/100('-'))
	endif
      call statistics for monthly discharge quality ( 0, CM1, CM2 )
*     write(01,4499)CM1,CM2
*     write(01,4699)BSM(1),BSS(1),iterate
*     write(01,4128)tmean,tstdev,NS

      if ( MONQ .gt. 1 ) call write shots for discharge quality
      return

 7500 write( *,7168) FLSTRUCTsmall(4,icod)
      write(01,7168) FLSTRUCTsmall(4,icod)
      write(09,7168) FLSTRUCTsmall(4,icod)
      write(33,7168) FLSTRUCTsmall(4,icod)
      call stop
 7168 Format(77('-')/'*** Error in monthly structure for discharge ',
     &'quality ...'/
     &'*** Error in reading monthly structure ...'/
     &'*** Check the file ',a64/77('-'))
      end


















*     non-parametric distribution of tributary quality -------------------------
      subroutine non parametric stream quality
      logical exists
      include 'COM.FOR'

      cut off zero quality (JP) = 0.0 

*     mean quality and standard deviation --------------------------------------
      RRCM = quolity data (IQ,JP,1)
	RRCS = quolity data (IQ,JP,2)

      spcorrfa = quolity data(IQ,jp,4) ! added river flow on quality ------ cRAN

      if ( MONQ .gt. 1 ) then
      write(01,51)Dname(Jp)
   51 format(77('-')/
     &'Generated stream quality data: ',a11,' (Non-parametric)'/
     &77('-'))
      call sort format 2 (RRCM,RRCS)
      write(01,52) valchars10, FUNIT, valchars11, FUNIT
   52 format('Input data:',24x,'          Mean =',a10,1x,a4/
     &'           ',20x,'Standard deviation =',a10,1x,a4/77('-'))
      endif

*     calculate the Monte-Carlo sampling errors in river quality ---------------
      call bias in non parametric stream quality
      if (BM(2) .gt. 1.0E-08) BM(2) = RRCM/BM(2)
      BS(2) = 1.0
      if ( MONQ .gt. 1 ) write(01,42)BM(2)
   42 format(77('-')/
     &'Correction for Monte-Carlo sampling errors for stream ',
     &'quality:',F12.4/77('-'))

*     identify the file with the non-parametric data ---------------------------
      do 1 i = 1, M7
      icod = i
      if ( idenp ( 1, i, JP + 1 ) .eq. IQ ) goto 2
    1 continue

*     no valid code found for the datafile -------------------------------------
      write( *,3)
      write(01,3)
      write(09,3)
      write(33,3)
    3 format(77('-')/'*** Error in tributary quality data ...'/
     &'*** No valid code for the non-parametric dataset ...'/77('-'))
      call stop

*     valid code found. Check datafile exists ----------------------------------
    2 continue
      inquire( FILE = Flname(2,icod), EXIST = exists )
      if ( .NOT. exists) then
      write( *,7163) flnamesmall(2,icod)
      write(01,7163) flnamesmall(2,icod)
      write(09,7163) flnamesmall(2,icod)
      write(33,7163) flnamesmall(2,icod)
 7163 Format(/77('-')/
     &'*** Error in tributary quality data ...'/
     &'*** The data file does not exist ... ',a64/
     &'*** Run halted ...'/77('-'))
      call stop
*	else
*     write(01,7863) flnamesmall(2,icod)
*     write(09,7863) flnamesmall(2,icod)
*     write(33,7863) flnamesmall(2,icod)
*7863 format(77('-')/
*    &'Non-parametric tributary quality data ... '/
*    &'File: ',a64/77('-'))
      endif

*     open the file ------------------------------------------------------------
      open(12,FILE = Flname(2,icod), STATUS='OLD')
*     read the first number on the file ----------------------------------------
      read(12, *, ERR=7500) nprf
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(2,icod) = 1
	endif
      kprf = nprf

*     check the number of data points - are there too many ? -------------------
      if ( nprf .gt. mprf) then
      write( *,7166) flnamesmall(2,icod)
      write(01,7166) flnamesmall(2,icod)
      write(09,7166) flnamesmall(2,icod)
      write(33,7166) flnamesmall(2,icod)
      call stop
      endif
*     check the number of data points - are there too few ? --------------------
      if ( nprf .lt. 5 ) then
      write( *,7167) flnamesmall(2,icod)
      write(01,7167) flnamesmall(2,icod)
      write(09,7167) flnamesmall(2,icod)
      write(33,7167) flnamesmall(2,icod)
      call stop
      endif

*     read the full file -------------------------------------------------------
      backspace (12)
      read(12, *, ERR=7500) nprf, (rfnpvl(i),i=1 , kprf)
      close (12)
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(2,icod) = 1
	endif
      kprf = nprf
*     --------------------------------------------------------------------------

      if ( MONQ .gt. 8 ) then
      write(01,813) dname(jp)
  813 format('Non-parametric data for: ',a11)
      write(01,819)
  819 format(150('-'))
      write(01,814) (rfnpvl(Ip),Ip=1,kprf)
  814 format(f10.4,20F7.4)
      write(01,819)
      endif
      
*     arrange the data in sequence ---------------------------------------------
      if ( flsequence(2,nonpd) .eq. 0 ) then
      do i = 1,   nprf-1
      do j = i+1, nprf
      if ( rfnpvl(i) .ge. rfnpvl(j) ) then
      xtemp = rfnpvl (j)
      rfnpvl (j) = rfnpvl (i)
      rfnpvl (i) = xtemp
	endif
      enddo
      enddo
      endif
*     --------------------------------------------------------------------------

      if ( MONQ .gt. 8 ) then
      write(01,913) dname(jp)
  913 format('Sequenced data for: ',a11)
      write(01,919)
  919 format(150('-'))
      write(01,914) (rfnpvl(Ip),Ip=1,kprf)
  914 format(f10.4,20F7.4)
      write(01,919)
      endif
      
*     compute cumulative frequencies and store them ----------------------------
      CUMUL = 1.0 / float (nprf)
      do i = 1, nprf
      rfnpfd(i) = float(i) * CUMUL
	enddo

      if ( MONQ .gt. 8 ) then
      write(01,713) dname(jp)
  713 format('Cumulative frequencies: ',a11)
      write(01,919)
      write(01,714) (rfnpfd(Ip),Ip=1,kprf)
  714 format(f10.4,20F7.4)
      write(01,919)
      endif
      
*     compute the cut-off for zero for intermittent quality --------------------
      cut off zero quality (JP) = 0.0 
	imark = 0
      do i = 1, nprf
*     write(01,2633)i,rfnpvl(i),rfnpfd(i)
 2633 format(i3,2f12.6)
      if ( imark .eq. 0 .and. rfnpvl(i) .gt. 1.0e-10 ) imark = i
	enddo
	if ( imark .gt. 1 ) then
	if ( imark .le. nprf ) then
      cut off zero quality (JP) = 0.5 * (rfnpfd (imark) 
     &   + rfnpfd (imark - 1))
	else
      cut off zero quality (JP) = rfnpfd (nprf)
	endif
	endif
   
      if ( MONQ .ge. 99 ) then
      write(01,20)DNAME(JP)
   20 format(//120('=')/'Ordered data entered for a non-parametric ',
     &'distribution ',a11/120('='))
      write(01,24)(rfnpvl(IS),IS = 1,nprf)
   24 format(15F8.4)
      write(01,55)
   55 format(120('='))
      endif
      
      if ( cut off zero quality (JP) .gt. 1.0e-09 ) then
* 	write(01,2376)100.0*(1.0-cut off zero quality (JP)),ifdiffuse
 2376 format(21x,'Percent of time stream quality operates =',f7.2,i4)
      endif
*     --------------------------------------------------------------------------
     
*     sample the distributions -------------------------------------------------
      if ( IQDIST .eq. 4 ) then ! non-parametric -------------------------------
      do 25 is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      ECshots(IS) = 0.0
*	if ( EFshots(IS) .gt. 1.0e-09 ) then
*     get the normal deviate for non-parametric stream quality -----------------
      js = jset ( IQ, NS, IS )
      cdnp = cRAN function ( JP, js, is ) ! added flow on added quality --- cRAN
*     compute the value of the flow for this shot ------------------------------
      call get non parametric shot ( cdnp, RC )
*     ECshots(IS) = VALPF ( RC, RRCM, BM(2) )
      ECshots(IS) = RC
*	endif
   25 continue
      endif
      if ( IQDIST .eq. 9 ) then ! non-parametric load --------------------------
      do 65 is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      ECshots(IS) = 0.0
*     get the normal deviate for non-parametric stream quality -----------------
      js = jset ( IQ, NS, IS )
      cdnp = cRAN function ( JP, js, is ) ! added flow on added quality --- cRAN
*     compute the value of the flow for this shot ------------------------------
      call get non parametric shot ( cdnp, RC )
*     ECshots(IS) = VALPF ( RC, RRCM, BM(2) )
      ECshots(IS) = RC
   65 continue
      endif
*     --------------------------------------------------------------------------

      if ( MONQ .gt. 8 ) then 
      write(01,13) dname(jp)
   13 format('Shots for non-parametric stream quality for: ',a11)
      write(01,19)
   19 format(150('-'))
      write(01,14) (ECshots(IS),IS=1,NS)
   14 format(f10.4,20F7.4)
      write(01,19)
      endif
      
      return
 7500 write( *,7168)flnamesmall(2,icod)
      write(01,7168)flnamesmall(2,icod)
      write(09,7168)flnamesmall(2,icod)
      write(33,7168)flnamesmall(2,icod)
 7168 format(/77('-')/
     &'*** Error in stream quality data (6)'/
     &'*** Error in reading non-parametric data ...'/
     &'*** Check the file ',a64/77('-'))
      call stop

 7166 format(/77('-')/
     &'*** Error in stream quality data (8)'/
     &'*** Too many data values specified for non-parametric  '/
     &'*** distribution. Check the file ',a64/77('-'))
 7167 format(/77('-')/
     &'*** Error in stream quality data (7)'/
     &'*** Too few data values specified for non-parametric   '/
     &'*** distribution. Check the file ',a64/77('-'))

      end





*     calculate the Monte-Carlo sampling errors in river quality ---------------
      subroutine bias in non parametric stream quality
      include 'COM.FOR'
      logical exists

      BM(2) = 0.0
      BS(2) = 0.0

*     identify the file with the non-parametric data ---------------------------
      do 1 i = 1, M7
      icod = i
      if ( idenp ( 1, i, JP + 1 ) .eq. IQ ) goto 2
    1 continue

*     no valid code found for the datafile -------------------------------------
      write( *,3)
      write(01,3)
      write(09,3)
      write(33,3)
    3 format(77('-')/'*** Error in stream quality data (1)'/
     &'*** No valid code for the non-parametric dataset ...'/
     &77('-'))
      call stop

*     Valid code found. Check datafile exists ...

    2 continue

      inquire( FILE = Flname(2,icod), EXIST = exists )
      if ( .NOT. exists) then
      write( *,7163) flnamesmall(2,icod)
      write(01,7163) flnamesmall(2,icod)
      write(09,7163) flnamesmall(2,icod)
      write(33,7163) flnamesmall(2,icod)
 7163 Format(/77('-')/
     &'*** Error in stream quality data (2) '/
     &'*** The data file does not exist ... ',a64/
     &'*** Run halted ...   '/77('-'))
      call stop
*	else
*     write(01,7863) flnamesmall(2,icod)
*     write(09,7863) flnamesmall(2,icod)
*     write(33,7863) flnamesmall(2,icod)
*7863 format(77('-')/
*    &'Non-parametric tributary quality data ... '/
*    &'File: ',a64/77('-'))
      endif

*     open the file ------------------------------------------------------------
      open(12,FILE = Flname(2,icod), STATUS='OLD')

*     read the file ------------------------------------------------------------
      read(12, *, ERR=7500) nprf
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(2,icod) = 1
	endif
      kprf = nprf

*     check the number of data points - are there too many ? -------------------
      if ( nprf .gt. mprf) then
      write( *,7166) flnamesmall(2,icod)
      write(01,7166) flnamesmall(2,icod)
      write(09,7166) flnamesmall(2,icod)
      write(33,7166) flnamesmall(2,icod)
      call stop
      endif

*     check the number of data points - are there too few ? --------------------
      if ( nprf .lt. 5 ) then
      write( *,7167) flnamesmall(2,icod)
      write(01,7167) flnamesmall(2,icod)
      write(09,7167) flnamesmall(2,icod)
      write(33,7167) flnamesmall(2,icod)
      call stop
      endif

      backspace (12)
      read(12, *, ERR=7500) nprf, (rfnpvl(i),i=1 , kprf)
      close (12)
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(2,icod) = 1
	endif
      kprf = nprf
*     --------------------------------------------------------------------------

*     arrange the data in sequence ---------------------------------------------
      if ( flsequence(2,nonpd) .eq. 0 ) then
      do i = 1,   nprf-1
      do j = i+1, nprf
      if ( rfnpvl(i) .ge. rfnpvl(j) ) then
      xtemp = rfnpvl (j)
      rfnpvl (j) = rfnpvl (i)
      rfnpvl (i) = xtemp
	endif
      enddo
      enddo
      endif

*     compute cumulative frequencies and store them in cfd ---------------------
      CUMUL = 1.0 / float (nprf)
      do i = 1, nprf
      rfnpfd(i) = float(i) * CUMUL
      enddo

*     compute the cut-off for zero for intermittent quality --------------------
      cut off zero quality (JP) = 0.0 
	imark = 0
      do i = 1, nprf
*     write(01,2633)i,rfnpvl(i),rfnpfd(i)
*2633 format(i3,2f12.6)
      if ( imark .eq. 0 .and. rfnpvl(i) .gt. 1.0e-10 ) imark = i
	enddo
	if ( imark .gt. 1 ) then
	if ( imark .le. nprf ) then
      cut off zero quality (JP) = 0.5 * (rfnpfd (imark) 
     &    + rfnpfd (imark - 1))
	else
      cut off zero quality (JP) = rfnpfd (nprf)
	endif
	endif
*     ----------------------------------------------------------------------BIAS

*     sample the distributions ---------------------------------------------BIAS
      if ( IQDIST .eq. 4 ) then ! non-parametric ---------------------------BIAS
      do 25 is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
*     get the normal deviate for non-parametric stream quality -------------BIAS
      js = JSET ( IQ, NS, IS )
      cdnp = cRAN function ( JP, js, is ) ! added flow on added quality --- cRAN
*     compute the value of the flow for this shot --------------------------BIAS
      call get non parametric shot ( cdnp, RC )
      BM(2) = BM(2) + RC
      BS(2) = BS(2) + RC * RC
   25 continue
      endif
      if ( IQDIST .eq. 9 ) then ! non-parametric load ----------------------BIAS
      do 65 is = 1, NS
*     set the month for this shot ------------------------------------------BIAS
      imonth = qmonth (is)
*     get the normal deviate for non-parametric stream quality -------------BIAS
      js = JSET ( IQ, NS, IS )
      cdnp = cRAN function ( JP, js, is ) ! added flow on added quality --- cRAN
*     compute the value of the flow for this shot --------------------------BIAS
      call get non parametric shot ( cdnp, RC )
      BM(2) = BM(2) + RC
      BS(2) = BS(2) + RC * RC
   65 continue
      endif
      
      BS(2)=(BS(2)-BM(2)*BM(2)/NS)/(NS-1)
      if ( BS(2) .gt. 1.0e-10 ) then
      BS(2)=SQRoot(1254,BS(2))
      else
      BS(2) = 0.0
      endif
      BM(2)=BM(2)/NS
      return

 7500 write( *,7168)flnamesmall(2,icod)
      write(01,7168)flnamesmall(2,icod)
      write(09,7168)flnamesmall(2,icod)
      write(33,7168)flnamesmall(2,icod)
 7168 Format(/77('-')/
     &'*** Error in stream quality data (5)'/
     &'*** Error in reading non-parametric data ... '/
     &'*** Check the file ',a64/77('-'))
      call stop

 7166 Format(/77('-')/
     &'*** Error in stream quality data (3)'/
     &'*** Too many data values specified for non-parametric  '/
     &'*** distribution. Check the file ',a64/77('-'))
 7167 Format(/77('-')/
     &'*** Error in stream quality data (4)'/
     &'*** Too few data values specified for non-parametric   '/
     &'*** distribution. Check the file ',a64/77('-'))

      end







*     --------------------------------------------------------------------------
*     set up all the shots for -------------------------------------------------
*     a non-parametric distribution of discharge quality -----------------------
*     this routine is called from Mass Balance ---------------------------------
*     --------------------------------------------------------------------------
      subroutine non parametric discharge quality
      include 'COM.FOR'
      logical exists

	EECM = pollution data (IQ,JP,1) ! mean effluent quality ------------------
	EECS = pollution data (IQ,JP,2) ! standard deviation ---------------------
      spcorrfa = pollution data(IQ,jp,4) ! effluent flow on quality ------- cRAN

      if ( MONQ .gt. 1 ) then
      write(01,51)Dname(Jp)
   51 format(77('=')/
     &'Generated discharge quality data: ',a11,' (Non-parametric)  '/
     &77('='))
      call sort format 2 (EECM, EECS)
      write(01,52) valchars10, UNITs(JP), valchars11, UNITs(JP)
   52 format('Input data:',38x,'Annual mean =',a10,1x,a4/
     &42x,'Standard deviation =',a10,1x,a4/77('='))
      endif

*     calculate the Monte-Carlo sampling errors --------------------------------
      call sampling errors for non parametric discharge quality

      if ( BM(2) .gt. 1.0E-08 ) BM(2) = EECM/BM(2)
      BS(2) = 1.0
      if ( MONQ .gt. 1 ) write(01,42)BM(2)
   42 format(
     &'Correction for Monte-Carlo sampling errors for discharge ',
     &'quality:',F12.4/77('-'))

*     identify the file with the non-parametric data ---------------------------
*     loop on the number of non-parametric datafiles ---------------------------

      do 1 i = 1, M7
      icod = i

*     new line -----------------------------------------------------------------
      if ( idenp ( 2, i, JP + 1 ) .eq. IQ ) goto 2
    1 continue

*     no valid code found for the datafile -------------------------------------
      write( *,3)
      write(01,3)
      write(09,3)
      write(33,3)
    3 format(77('-')/
     &'*** Error in non-parametric discharge quality data ...'/
     &'*** No valid code for the non-parametric dataset ...'/
     &77('-'))
      call stop

*     valid code found. Check datafile exists ----------------------------------
    2 continue
      inquire( FILE = Flname(4,icod), EXIST = exists )
      if ( .NOT. exists) then
      write( *,7163) flnamesmall(4,icod)
      write(01,7163) flnamesmall(4,icod)
      write(09,7163) flnamesmall(4,icod)
      write(33,7163) flnamesmall(4,icod)
 7163 Format(/77('-')/
     &'*** Error in non-parametric discharge quality data ...'/
     &'*** The data file does not exist ... ',a64/
     &'*** Run halted ... '/77('-'))
      call stop
*	else
*     write(01,7863) flnamesmall(4,icod)
*     write(09,7863) flnamesmall(4,icod)
*     write(33,7863) flnamesmall(4,icod)
*7863 format(77('-')/
*    &'Non-parametric discharge quality data ... '/
*    &'File: ',a64/77('-'))
      endif

*     open the file ------------------------------------------------------------
      open(12,FILE = Flname(4,icod), STATUS='OLD')

*     read the file ------------------------------------------------------------
      read(12, *, ERR=7500) nprf
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(4,icod) = 1
	endif
      kprf = nprf
*     --------------------------------------------------------------------------

*     check the number of data points - are there too many ? -------------------
      if ( nprf .gt. mprf) then
      write( *,7166) flnamesmall(4,icod)
      write(01,7166) flnamesmall(4,icod)
      write(09,7166) flnamesmall(4,icod)
      write(33,7166) flnamesmall(4,icod)
 7166 Format(/77('-')/
     &'*** Error in non-parametric discharge quality data ...'/
     &'*** Too many data values specified for non-parametric  '/
     &'*** distribution. Check the file ',a64/77('-'))
      call stop
      endif
*     --------------------------------------------------------------------------

*     check the number of data points - are there too few ? --------------------
      if ( nprf .lt. 5 ) then
      write( *,7167) flnamesmall(4,icod)
      write(01,7167) flnamesmall(4,icod)
      write(09,7167) flnamesmall(4,icod)
      write(33,7167) flnamesmall(4,icod)
 7167 Format(/77('-')/
     &'*** Error in non-parametric discharge quality data ...'/
     &'*** Too few data values specified for non-parametric   '/
     &'*** distribution. Check the file ',a64/77('-'))
      call stop
      endif
      backspace (12)
      read(12, *, ERR=7500) nprf, (rfnpvl(i),i=1 , kprf)
      close (12)
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(4,icod) = 1
	endif
      kprf = nprf
*     --------------------------------------------------------------------------

*     arrange the data in sequence ---------------------------------------------
      if ( flsequence(4,nonpd) .eq. 0 ) then
      do i = 1,   nprf-1
      do j = i+1, nprf
      if ( rfnpvl(i) .ge. rfnpvl(j) ) then
      xtemp = rfnpvl (j)
      rfnpvl (j) = rfnpvl (i)
      rfnpvl (i) = xtemp
	endif
      enddo
      enddo
      endif

*     compute cumulative frequencies and store them ----------------------------
      CUMUL = 1.0 / float (nprf)
      do i = 1, nprf
      rfnpfd(i) = float(i) * CUMUL
      enddo

*     compute the cut-off for zero for intermittent quality --------------------
      cut off zero quality (JP) = 0.0 
	imark = 0
      do i = 1, nprf
*     write(01,2633)i,rfnpvl(i),rfnpfd(i)
*2633 format(i3,2f12.6)
      if ( imark .eq. 0 .and. rfnpvl(i) .gt. 1.0e-10 ) imark = i
	enddo
	if ( imark .gt. 1 ) then
	if ( imark .le. nprf ) then
      cut off zero quality (JP) = 0.5 * (rfnpfd (imark) 
     &+ rfnpfd (imark - 1))
	else
      cut off zero quality (JP) = rfnpfd (nprf)
	endif
	endif

*     sample the distributions -------------------------------------------------
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      ECshots(IS) = 0.0
*	if ( EFshots(IS) .gt. 1.0e-09 ) then
*     get the normal deviate for non-parametric discharge quality --------------
      js = JSET ( IQ, NS, is )
      cdnp = cRAN function ( JP, js, is ) ! added flow on added quality --- cRAN
*     compute the value of the flow for this shot ------------------------------
      call get non parametric shot ( cdnp, ec )
      ECshots(IS) = VALPF ( EC, EECM, BM(2) )
      ECshots(IS) = EC
*	endif
      enddo
*     --------------------------------------------------------------------------

      if ( MONQ .gt. 1 ) then
      write(01,13) dname(jp)
   13 format('Shots for non-parametric discharge quality for: ',a11)
      write(01,19)
   19 format(150('-'))
      write(01,14) (ECshots(IS),IS=1,40)
   14 format(f10.2,20F7.2)
      write(01,19)
	endif
      return

 7500 write( *,7168)flnamesmall(4,icod)
      write(01,7168)flnamesmall(4,icod)
      write(09,7168)flnamesmall(4,icod)
      write(33,7168)flnamesmall(4,icod)
      call stop
 7168 Format(/
     &77('-')/
     &'*** Error in non-parametric discharge quality data ...'/
     &'*** Error in reading non-parametric data ... '/
     &'*** Check the file ',a64/
     &77('-'))
      end







*     compute Monte-Carlo sampling errors for non-parametric discharge quality -
      subroutine sampling errors for non parametric discharge quality
      include 'COM.FOR'
      logical exists

      BM(2) = 0.0
      BS(2) = 0.0

*     identify the file with the non-parametric data ---------------------------
      do 1 i = 1, M7
      icod = i
      if ( idenp ( 2, i, JP + 1) .eq. IQ ) goto 2
    1 continue

*     no valid code found for the datafile -------------------------------------
      write( *,3)uname(KFEAT),IQ
      write(01,3)uname(KFEAT),IQ
      write(09,3)uname(KFEAT),IQ
      write(33,3)uname(KFEAT),IQ
    3 format(110('*')/
     &'*** Error in non-parametric discharge quality data ... 'a40/
     &'*** No valid code for the non-parametric dataset ...',i5/
     &110('*'))
      call stop

*     valid code found. Check datafile exists ----------------------------------
    2 continue
      inquire( FILE = Flname(4,icod), EXIST = exists )

      if ( .NOT. exists) then
      write( *,7163) flnamesmall(4,icod)
      write(01,7163) flnamesmall(4,icod)
      write(09,7163) flnamesmall(4,icod)
      write(33,7163) flnamesmall(4,icod)
 7163 Format(/77('-')/
     &'*** Error in non-parametric discharge quality data ...'/
     &'*** The data file does not exist ... ',a64/
     &'*** Run halted ...  '/77('-'))
      call stop
*     else
*     write(01,7863) flnamesmall(4,icod)
*     write(09,7863) flnamesmall(4,icod)
*     write(33,7863) flnamesmall(4,icod)
*7863 format(77('-')/
*    &'Non-parametric discharge quality data ... '/
*    &'File: 'a64/77('-'))
      endif

*     open the file ------------------------------------------------------------
      open(12,FILE = Flname(4,icod), STATUS='OLD')
*     read the file ------------------------------------------------------------
      read(12, *, ERR=7500) nprf
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(4,icod) = 1
	endif
*     --------------------------------------------------------------------------
      kprf = nprf

*     check the number of data points - are there too many ? -------------------
      if ( nprf .gt. mprf) then
      write( *,7166) flnamesmall(4,icod)
      write(01,7166) flnamesmall(4,icod)
      write(09,7166) flnamesmall(4,icod)
      write(33,7166) flnamesmall(4,icod)
      call stop
      endif

*     check the number of data points - are there too few ? --------------------
      if ( nprf .lt. 5 ) then
      write( *,7167) flnamesmall(4,icod)
      write(01,7167) flnamesmall(4,icod)
      write(09,7167) flnamesmall(4,icod)
      write(33,7167) flnamesmall(4,icod)
      call stop
      endif

      backspace (12)
      read(12, *, ERR=7500) nprf, (rfnpvl(i),i=1 , kprf)
      close (12)
*     store request not to sequence the non-parametric data --------------------
      if ( nprf .lt. 0 ) then
	nprf = - nprf
      flsequence(4,icod) = 1
	endif
      kprf = nprf
*     --------------------------------------------------------------------------

*     arrange the data in sequence ---------------------------------------------
      if ( flsequence(4,nonpd) .eq. 0 ) then
      do i = 1,   nprf-1
      do j = i+1, nprf
      if ( rfnpvl(i) .ge. rfnpvl(j) ) then
      xtemp = rfnpvl (j)
      rfnpvl (j) = rfnpvl (i)
      rfnpvl (i) = xtemp
	endif
      enddo
      enddo
      endif

*     compute cumulative frequencies and store them ----------------------------
      CUMUL = 1.0 / float (nprf)
      do i = 1, nprf
      rfnpfd(i) = float(i) * CUMUL
      enddo

*     compute the cut-off for zero for intermittent quality --------------------
      cut off zero quality (JP) = 0.0 
	imark = 0
      do i = 1, nprf
*     write(01,2633)i,rfnpvl(i),rfnpfd(i)
*2633 format(i3,2f12.6)
      if ( imark .eq. 0 .and. rfnpvl(i) .gt. 1.0e-10 ) imark = i
	enddo
	if ( imark .gt. 1 ) then
	if ( imark .le. nprf ) then
      cut off zero quality (JP) = 0.5 * (rfnpfd (imark) 
     &    + rfnpfd (imark - 1))
	else
      cut off zero quality (JP) = rfnpfd (nprf)
	endif
	endif
*     --------------------------------------------------------------------------

*     sample the distributions -------------------------------------------------
      do is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      ECshots(IS) = 0.0
*	if ( EFshots(IS) .gt. 1.0e-09 ) then
*     get the normal deviate for non-parametric discharge quality --------------
      js = JSET ( IQ, NS, is )
      cdnp = cRAN function ( JP, js, is ) ! added flow on added quality --- cRAN
*     call get the correlated random numbers (IS, R1, R2, R3, rnpar)
*     compute the value of the flow for this shot ------------------------------
      call get non parametric shot ( cdnp, EC )
      BM (2) = BM (2) + EC
      BS (2) = BS (2) + EC * EC
*	endif
      enddo

      BS(2)=(BS(2)-BM(2)*BM(2)/NS)/(NS-1)
      if ( BS(2) .gt. 1.0e-10 ) then
      BS(2)=SQRoot(1251,BS(2))
      else
      BS(2) = 0.0
      endif
      BM(2)=BM(2)/NS
      return

 7500 write( *,7168)flnamesmall(4,icod)
      write(01,7168)flnamesmall(4,icod)
      write(09,7168)flnamesmall(4,icod)
      write(33,7168)flnamesmall(4,icod)
      call stop

 7166 Format(/77('-')/
     &'*** Error in non-parametric discharge quality data ...'/
     &'*** Too many data values specified for non-parametric  '/
     &'*** distribution. Check the file ',a64/77('-'))
 7167 Format(/77('-')/
     &'*** Error in non-parametric discharge quality data ...'/
     &'*** Too few data values specified for non-parametric   '/
     &'*** distribution. Check the file ',a64/77('-'))
 7168 Format(/77('-')/
     &'*** Error in non-parametric discharge quality data ...'/
     &'*** Error in reading non-parametric data ...           '/
     &'*** Check the file ',a64/77('-'))

      end















*     monthly data for tributary quality ---------------------------------------
      subroutine monthly stream quality
      include 'COM.FOR'
      logical exists

*     ==========================================================================
*     mean and standard deviation ----------------------------------------------
      RRCM = quolity data (IQ,JP,1)
	RRCS = quolity data (IQ,JP,2)
      if ( MONQ .gt. 1 ) then
      write(01,51)Dname(Jp)
   51 format(77('-')/
     &'Generated stream quality data: ',a11,' (monthly data)'/
     &77('-'))
      call sort format 2 (RRCM, RRCS)
      write(01,52) valchars10, UNITs(JP), valchars11, UNITs(JP)
   52 format('Input data:',24x,'          Mean =',a10,1x,a4/
     &26x,'     Standard deviation =',a10,1x,a4/77('-'))
      endif
*     ==========================================================================



*     identify the file with the monthly data ----------------------------------
*     loop on the number of monthly datafiles ----------------------------------
      do 1 i = 1, M8
      icod = i
      if ( iseasp ( 1, i, JP + 1 ) .eq. IQ ) goto 2
    1 continue

*     no valid code found for the datafile -------------------------------------
      write( *,3)
      write(01,3)
      write(09,3)
      write(33,3)
    3 format(77('-')/'*** Error in monthly river quality data ...'/
     &'*** No valid code for the monthly dataset ...'/77('-'))
      call stop
*     --------------------------------------------------------------------------
*     valid code found. Check datafile exists ----------------------------------
    2 continue
      Inquire( FILE = flmonth(2,icod), EXIST = exists )
      if ( .NOT. exists) then
      write( *,7163) FLMONTHsmall(2,icod)
      write(01,7163) FLMONTHsmall(2,icod)
      write(09,7163) FLMONTHsmall(2,icod)
      write(33,7163) FLMONTHsmall(2,icod)
 7163 Format(/77('-')/
     &'*** Error in monthly river quality data ...'/
     &'*** The data file does not exist ... ',a64/
     &'*** Run halted ...'/77('-'))
      call stop
      else
      if ( ifdiffuse .eq. 0 ) then
      if ( nobigout .le. 0 ) then
      write(01,7863) Dname(jp),FLMONTHsmall(2,icod)
      endif
      write(33,7863) Dname(jp),FLMONTHsmall(2,icod)
 7863 format(77('-')/
     &'Monthly data on river quality ... ',a11/
     &'File: 'a64/77('-'))
      endif
      endif

*     open the file of monthly data --------------------------------------------
      open(11,FILE = flmonth(2,icod), STATUS='OLD')

*     read the file containing the monthly data --------------------------------
      call read monthly data (2,icod) ! river quality - type 5

      spcorrfa = quolity data (IQ,JP,4) ! added flow on added quality ----- cRAN

      if ( ifdiffuse .eq. 0 ) then
      write(01,4398)
 4398 format(
     &'Month        ','       Mean','   Standard','      Shift',
     &'  Correlation'/25x,'  Deviation'/77('-'))
      write(01,2399)(seas1(i),seas2(i),seas3(i),seas4(i),seas0(i),
     &i=1 ,12)
 2399 format(
     &'January ...  ',3f11.2,f13.4,i18/
     &'February ... ',3f11.2,f13.4,i18/
     &'March ...    ',3f11.2,f13.4,i18/
     &'April ...    ',3f11.2,f13.4,i18/
     &'May ...      ',3f11.2,f13.4,i18/
     &'June ...     ',3f11.2,f13.4,i18/
     &'July ...     ',3f11.2,f13.4,i18/
     &'August ...   ',3f11.2,f13.4,i18/
     &'September ...',3f11.2,f13.4,i18/
     &'October ...  ',3f11.2,f13.4,i18/
     &'November ... ',3f11.2,f13.4,i18/
     &'December ... ',3f11.2,f13.4,i18/77('-')/)
      endif
      if ( ifdiffuse .eq. 1 ) then
*     write(01,4388)  
*4388 format(33x,45('-')/33x,'Month        ','       Mean',
*    &'   Standard Deviation'/33x,45('-'))
*     write(01,2389)(seas1(i),seas2(i),i=1 ,12)  
 2389 format(
     &33x,'January ...  ',2f11.2/
     &33x,'February ... ',2f11.2/
     &33x,'March ...    ',2f11.2/
     &33x,'April ...    ',2f11.2/
     &33x,'May ...      ',2f11.2/
     &33x,'June ...     ',2f11.2/
     &33x,'July ...     ',2f11.2/
     &33x,'August ...   ',2f11.2/
     &33x,'September ...',2f11.2/
     &33x,'October ...  ',2f11.2/
     &33x,'November ... ',2f11.2/
     &33x,'December ... ',2f11.2/)
      endif
*     ==========================================================================

*     calculate the sampling errors for the monthly data on river quality ------

	do imon = 1, 12 ! loop through the months 

*     special version of BIAS for monthly data ---------------------------------
      call bias in monthly stream quality (imon)

      if (BSM(imon) .gt. 1.0E-08) BSM(imon) = seas1(imon)/BSM(imon)
      if (BSS(imon) .gt. 1.0E-08) then
      BSS(imon) = seas2(imon)/BSS(imon)
      else
      BSS(imon) = 1.0
      endif

      if ( MONQ .gt. 1 ) then 
      if ( imon .eq. 1 ) then
      if ( nobigout .le. 0 ) write(01,12)
   12 format(/
     &'Correction factors for sampling errors on stream qua',
     &'lity data:')
      write(01,92)imon,BSM(imon),BSS(imon)
   92 format('Monthly Mean',i3,' =',F8.3,
     &'   Standard deviation =',F8.3)
      endif
	endif
      enddo

*     sample the distributions -------------------------------------------------
      do 22 is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      ECshots(IS) = 0.0
	if ( EFshots(IS) .gt. 1.0e-09 ) then ! Richard III

      RCM = seas1 (imonth) ! mean added quality --------------------------------
      RCS = seas2 (imonth) ! standard deviation --------------------------------
      RC3 = seas3 (imonth) ! shift ---------------------------------------------
      spcorrfa = seas4 (imonth) ! added flow on added quality ------------- cRAN

	GRCM = 0.0
	GRCS = 0.0
      RM3 = (RCM + RC3) * (RCM + RC3)

*     mean and standard deviation of logged data -------------------------------
      if ( RM3 .gt. 1.0e-9) then
      GRCM = ALOG ( RM3 / SQRoot(1000,RM3+RCS*RCS) )
      GRCS = SQRoot(1025, ALOG (1.0+(RCS*RCS)/RM3) )
      endif

*     get the normal deviate for monthly stream quality ------------------------
      js = JSET ( IQ, NS, is )
      cdmm = cRAN function ( JP, js, is ) ! added flow on added quality --- cRAN
*     compute the value of the shot --------------------------------------------
      ECshots(is) = Vlogn (cdmm, GRCS, GRCM, RC3,
     &BSM(imonth),BSS(imonth),RCM)
      endif ! if ( EFshots(IS) .gt. 1.0e-09 )
   22 continue ! loop on shots

      if ( MONQ .gt. 1 ) call write shots for river quality 
      return

 7500 write( *,7168) FLMONTHsmall(2,icod)
      write(01,7168) FLMONTHsmall(2,icod)
      write(09,7168) FLMONTHsmall(2,icod)
      write(33,7168) FLMONTHsmall(2,icod)
 7168 Format(/77('-')/'*** Error in river quality data ...'/
     &'*** Error in reading monthly data ...'/
     &'*** Check the file ',a64/77('-'))
      write(01,2399)(seas1(i),seas2(i),seas3(i),seas4(i),i=1 ,12)
      write( *,2399)(seas1(i),seas2(i),seas3(i),seas4(i),i=1 ,12)
      call stop

      end







*     compute Monte Carlo sampling errors for monthly stream quality data ------
      subroutine bias in monthly stream quality (imon)
      include 'COM.FOR'

      BSM (imon) = 0.0
      BSS (imon) = 0.0
      do IS = 1, NS
      ECshots(IS) = 0.0
      enddo      

      do 2 is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
	jmonth = imonth
      if ( jmonth .ne. imon ) goto 2
      RCM = seas1 (jmonth) ! mean added quality --------------------------------
      RCS = seas2 (jmonth) ! standard deviation --------------------------------
      RC3 = seas3 (jmonth) ! shift ---------------------------------------------
      spcorrfa = seas4 (jmonth) ! added flow on added quality ------------- cRAN

	GRCM = 0.0
	GRCS = 0.0
      RM3 = (RCM + RC3) * (RCM + RC3)

*     mean and standard deviation of logged data -------------------------------
      if ( RM3 .gt. 1.0e-9) then
      GRCM = ALOG ( RM3 / SQRoot(1264, RM3 + RCS * RCS ) )
      GRCS = SQRoot(1265, ALOG ( 1.0 + ( RCS * RCS ) / RM3 ) )
      endif

*     get the normal deviate for monthly stream quality ------------------------
      js = JSET ( IQ, NS, is )
      cdmm = cRAN function ( JP, js, is ) ! added flow on added quality --- cRAN
*     compute the value of the shot --------------------------------------------
      RC = exp ( cdmm * GRCS + GRCM ) - RC3
      ECshots(IS ) = RC
    2 continue

      call statistics for one month of discharge quality (imon,EM1,EM2)

      BSM (imon) = EM1
      BSS (imon) = EM2

      return
      end









*     Set up all the shots for -------------------------------------------------
*     ... a monthly distribution of discharge quality --------------------------

      subroutine monthly discharge quality
      include 'COM.FOR'
	dimension Z1(MS), Z2(MS)
      logical exists

	EECM = pollution data (IQ,JP,1) ! mean effluent quality ------------------
	EECS = pollution data (IQ,JP,2) ! standard deviation ---------------------
      spcorrfa = pollution data(IQ,jp,4) ! effluent flow and quality ------ cRAN

      if ( MONQ .gt. 1 ) then
      write(01,51)Dname(Jp)
   51 format(/66('X')/'In subroutine monthly discharge quality'/
     &66('X')/
     &'Averaged input discharge quality data: ',a11,' (monthly)'/
     &66('X'))
      call sort format 2 (EECM, EECS)
      write(01,52) valchars10, UNITs(JP), valchars11, UNITs(JP)
   52 format('Input data:',24x,'          Mean =',a10,1x,a4/
     &26x,'     Standard deviation =',a10,1x,a4/66('X'))
      endif

*     identify the file with the monthly data ----------------------------------
*     loop on the number of monthly datafiles ----------------------------------

      do 1 i = 1, M8
      icod = i
      if ( iseasp ( 2, i, JP + 1 ) .eq. IQ ) goto 2
    1 continue

*     no valid code found for the datafile -------------------------------------
      write( *,3)
      write(01,3)
      write(09,3)
      write(33,3)
    3 format(77('-')/
     &'*** Error in monthly discharge quality data ...'/
     &'*** No valid code for the monthly dataset ...'/77('-'))
      call stop

*     valid code found. Check datafile exists ----------------------------------
    2 continue

      Inquire( FILE = flmonth(4,icod), EXIST = exists )

      if ( .NOT. exists) then
      write( *,7163) FLMONTHsmall(4,icod)
      write(01,7163) FLMONTHsmall(4,icod)
      write(09,7163) FLMONTHsmall(4,icod)
      write(33,7163) FLMONTHsmall(4,icod)
 7163 Format(/77('-')/
     &'*** Error in monthly discharge quality data ... '/
     &'*** The data file does not exist ... ',a64/
     &'*** Run halted ...'/77('-'))
      call stop
      else
*     write( *,7863) Dname(jp),FLMONTHsmall(4,icod)
      write(01,7863) Dname(jp),FLMONTHsmall(4,icod)
*     write(09,7863) Dname(jp),FLMONTHsmall(4,icod)
      write(33,7863) Dname(jp),FLMONTHsmall(4,icod)
 7863 format(77('-')/
     &'Monthly data on discharge quality ... ',a11,
     &' File: 'a64/77('-'))
      endif

*     open the file ------------------------------------------------------------
      open(11,FILE = flmonth(4,icod), STATUS='OLD')

*     read the file containing the monthly data --------------------------------
      call read monthly data (4,icod) ! discharge quality - type 5

      if ( JT (KFEAT) .eq. 19) then
      write(01,2398)
 2398 format(//77('-')/
     &'Monthly data on abstractions ...'/77('-')/
     &'Month        ','       Mean','   Standard','      Shift',
     &'  Correlation'/25x,'  Deviation'/77('-'))
      write(01,2399)(seas1(i),seas2(i),seas3(i),seas4(i),seas0(i),
     &i=1 ,12)
 2399 format(
     &'January ...  ',3f11.2,f13.4,i18/
     &'February ... ',3f11.2,f13.4,i18/
     &'March ...    ',3f11.2,f13.4,i18/
     &'April ...    ',3f11.2,f13.4,i18/
     &'May ...      ',3f11.2,f13.4,i18/
     &'June ...     ',3f11.2,f13.4,i18/
     &'July ...     ',3f11.2,f13.4,i18/
     &'August ...   ',3f11.2,f13.4,i18/
     &'September ...',3f11.2,f13.4,i18/
     &'October ...  ',3f11.2,f13.4,i18/
     &'November ... ',3f11.2,f13.4,i18/
     &'December ... ',3f11.2,f13.4,i18/77('-')/)
      else

      write(01,4398)
 4398 format(
     &'Month        ','       Mean','   Standard','      Shift',
     &'  Correlation'/
     &'             ','           ','  Deviation'/77('-'))
      write(01,8399)(seas1(i),seas2(i),seas3(i),seas4(i),seas0(i),
     &i=1 ,12)
 8399 format(
     &'January ...  ',3f11.2,f13.4,i18/
     &'February ... ',3f11.2,f13.4,i18/
     &'March ...    ',3f11.2,f13.4,i18/
     &'April ...    ',3f11.2,f13.4,i18/
     &'May ...      ',3f11.2,f13.4,i18/
     &'June ...     ',3f11.2,f13.4,i18/
     &'July ...     ',3f11.2,f13.4,i18/
     &'August ...   ',3f11.2,f13.4,i18/
     &'September ...',3f11.2,f13.4,i18/
     &'October ...  ',3f11.2,f13.4,i18/
     &'November ... ',3f11.2,f13.4,i18/
     &'December ... ',3f11.2,f13.4,i18/77('-')/)
      endif

*     --------------------------------------------------------------------------
*     Calculate the Monte-Carlo sampling errors for monthly discharge quality --
*     data.  Loop through the months -------------------------------------------
*     --------------------------------------------------------------------------

	do 13 imon = 1, 12

*     special version of BIAS for the monthly data -----------------------------
      call sampling errors for monthly discharge quality data (imon)

      if (BSM(imon) .gt. 1.0E-08) BSM(imon) = seas1(imon)/BSM(imon)
      if (BSS(imon) .gt. 1.0E-08) then
      BSS(imon) = seas2(imon)/BSS(imon)
      else
      BSS(imon) = 1.0
      endif
      if (BSS(imon) .lt. 1.0E-08) BSS(imon) = 1.0

   13 continue

*     sample the distributions -------------------------------------------------
      do 22 is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      ECshots(IS) = 0.0
	if ( EFshots(IS) .gt. 1.0e-09 ) then

      ECM = seas1 (imonth) ! mean quality --------------------------------------
      ECS = seas2 (imonth) ! standard deviation --------------------------------
      ECF = seas3 (imonth) ! shift ---------------------------------------------
	EC3 = ( ECM + ECF ) * ( ECM + ECF )
      spcorrfa = seas4 (imonth) ! effluent flow and quality --------------- cRAN

*     mean and standard deviation of logged data -------------------------------
	GECM = 0.0
	GECS = 0.0
      if ( EC3 .gt. 1.0e-9) then
      GECM = ALOG ( EC3 / SQRoot(1258, EC3 + ECS * ECS ) )
      GECS = SQRoot(1259, ALOG ( 1.0 + ( ECS * ECS ) / EC3 ) )
	endif

*     get the normal deviate for monthly discharge quality ---------------------
      js = JSET ( IQ, NS, is )
      cdmm = cRAN function ( jp, js, is) ! added flow on added quality ---- cRAN
*     call get the correlated random numbers (IS, R1, R2, R3, R4)
      Z1(is) = cdmm
*     compute the value of the  shot -------------------------------------------
      ECshots(is) = Vlogn (cdmm, GECS,GECM,ECF,
     &BSM(imonth),BSS(imonth),ECM)
      Z2(is) = ECshots(is)
	endif
   22 continue

*     ==========================================================================
*     if ( nobigout .le. 0 ) write(01,95)
*     if ( nobigout .le. 0 ) write(01,92) dname(jp)
*  92 format('Random numbers: ',a11)
*     if ( nobigout .le. 0 ) write(01,95)
*  95 format(77('X'))
*     if ( nobigout .le. 0 ) write(01,94) (Z1(IS),IS=1,50)
*  94 format(7x,10F7.4,'  ')
*     if ( nobigout .le. 0 ) write(01,95)

      CM1 = 0.0
	CM2 = 0.0
	do is = 1, NS
      CM1 = CM1 + Z1(IS)
      CM2 = CM2 + Z1(IS) * Z1(IS)
	enddo
      CM2=(CM2-CM1*CM1/NS)/(NS-1)
      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(1267,CM2)
      else
      CM2 = 0.0
      endif
      CM1 = CM1 / FLOAT(NS)

*     if ( nobigout .le. 0 ) write(01,339) CM1, CM2
* 339 Format('Calculated mean =',f8.5,8x,
*    &       '           ...  Standard deviation =',f8.5)       
*     if ( nobigout .le. 0 ) write(01,95)
*     ==========================================================================

*     ==========================================================================
*     if ( nobigout .le. 0 ) write(01,85)
*     if ( nobigout .le. 0 ) write(01,82) dname(jp)
*  82 format('Discharge quality: ',a11)
*     if ( nobigout .le. 0 ) write(01,85)
*  85 format(77('X'))
*     if ( nobigout .le. 0 ) write(01,84) (Z2(IS),IS=1,50)
*  84 format(7x,10F7.2,'  ')
*     if ( nobigout .le. 0 ) write(01,85)

      CM1 = 0.0
	CM2 = 0.0
	do is = 1, NS
      CM1 = CM1 + Z2(IS)
      CM2 = CM2 + Z2(IS) * Z2(IS)
	enddo
      CM2=(CM2-CM1*CM1/NS)/(NS-1)
      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(1267,CM2)
      else
      CM2 = 0.0
      endif
      CM1 = CM1 / FLOAT(NS)
*     if ( nobigout .le. 0 ) write(01,239) CM1, CM2
* 239 Format('Calculated mean =',f8.5,8x,
*    &       '           ...  Standard deviation =',f8.5)       
*     if ( nobigout .le. 0 ) write(01,85)
*     ==========================================================================

*     write(01,258)Dname(Jp)
* 258 format(66('X')/
*    &'Calculated discharge quality: ',a11,' (from monthly shots)'/
*    &66('X'))

      CM1 = 0.0
	CM2 = 0.0
	do is = 1, NS
      CM1 = CM1 + ECshots(IS)
      CM2 = CM2 + ECshots(IS) * ECshots(IS)
	enddo
      CM2=(CM2-CM1*CM1/NS)/(NS-1)
      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(1267,CM2)
      else
      CM2 = 0.0
      endif
      CM1 = CM1 / FLOAT(NS)

*     write(01,58)Dname(Jp)
*  58 format(66('X')/
*    &'Calculated discharge quality: ',a11,' (from monthly shots)'/
*    &66('X'))

*     write(01,59) CM1, UNITs(JP), CM2, UNITs(JP)
*  59 format('From shots:',26x,'          Mean =',F8.2,1x,a4/
*    &28x,'     Standard deviation =',F8.2,1x,a4/
*    &66('X')/'Leaving subroutine monthly discharge quality ... '/
*    &66('X'))

      if ( MONQ .gt. 1 ) call write shots for discharge quality

      return

 7500 write( *,7168)FLMONTHsmall(4,icod)
      write(01,7168)FLMONTHsmall(4,icod)
      write(09,7168)FLMONTHsmall(4,icod)
      write(33,7168)FLMONTHsmall(4,icod)
      call stop
 7168 Format(/
     &77('-')/
     &'*** Error in monthly discharge quality data ... '/
     &'*** Error in reading monthly data ....                '/
     &'*** Check the file ',a64/
     &77('-'))

      end







*     compute Monte Carlo sampling errors for monthly discharge quality data
      subroutine sampling errors for monthly discharge quality data
     &(imon)
      include 'COM.FOR'

      BSM(imon) = 0.0
      BSS(imon) = 0.0

      do 2 is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      ECshots(IS) = 0.0
	if ( EFshots(IS) .gt. 1.0e-09 ) then
	jmonth = imonth
      if ( jmonth .ne. imon ) goto 2
      ECM = seas1 (jmonth) ! mean quality --------------------------------------
      ECS = seas2 (jmonth) ! mean quality --------------------------------------
      EC3 = seas3 (jmonth) ! shift flow ----------------------------------------
*     special correlation coefficient ... added flow on added quality ----------
      spcorrfa = seas4 (jmonth) ! effluent flow and quality --------------- cRAN

	GECM = 0.0
	GECS = 0.0
      EM3 = (ECM + EC3) * (ECM + EC3)

*     mean and standard deviation of logged data -------------------------------
      if ( EM3 .gt. 1.0e-9) then
      GECM = ALOG ( EM3 / SQRoot(1262, EM3 + ECS * ECS ) )
      GECS = SQRoot(1263, ALOG ( 1.0 + ( ECS * ECS ) / EM3 ) )
      endif

*     get the normal deviate ---------------------------------------------------
      js = JSET ( IQ, NS, is )
      cdmm = cRAN function ( JP, js, is ) ! added flow on added quality --- cRAN
*     compute the value of the shot --------------------------------------------
      EC = exp ( cdmm * GECS + GECM ) - EC3
      ECshots(IS) = EC
	endif
    2 continue

      call statistics for one month of discharge quality (imon,EM1,EM2)

      BSM (imon) = EM1
      BSS (imon) = EM2

      return
      end






*     Special version of STATC for monthly discharge flow and quality data ....
*     Calculation of mean and standard deviation ...
*     From the values of the shots stored in EFshots ....
      subroutine statistics for one month of discharge flows 
     &(imon,CM1,CM2)
      include 'COM.FOR'
      dimension Y(MS)

	CM1 = 0.0
      CM2 = 0.0
	NUM = 0

      do IS = 1 , NS
      imonth = qmonth (is) ! set the month for this shot
	jmonth = imonth

	if ( jmonth .eq. imon ) then
      NUM = NUM + 1
      Y(NUM) = EFshots ( IS )
      CM1 = CM1 + Y(NUM)
	CM2 = CM2 + Y(num) * Y(num)
	endif

      enddo

      CM2=(CM2-CM1*CM1/NUM)/(NUM-1)

      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(1267,CM2)
      else
      CM2 = 0.0
      endif

      CM1 = CM1 / FLOAT(NUM)

      return
      end






*     Special version of STATC for monthly discharge quality data ....
*     Calculation of mean and standard deviation for monthly discharge quality ...
*     From the values of the shots stored in ECshots ....

      subroutine statistics for one month of discharge quality 
     &(imon,CM1,CM2)
      include 'COM.FOR'
      dimension Y(MS)

 	CM1 = 0.0
      CM2 = 0.0

	NUM = 0

      do IS = 1 , NS
      imonth = qmonth (is) ! set the month for this shot
	jmonth = imonth
	if ( jmonth .eq. imon ) then
      NUM = NUM + 1
      Y(NUM) = ECshots( IS )
      CM1 = CM1 + Y(NUM)
      CM2 = CM2 + Y(num) * Y(num)
	endif
      enddo

      CM2=(CM2-CM1*CM1/NUM)/(NUM-1)

      if ( CM2 .gt. 1.0e-10 ) then
      CM2=SQRoot(1268,CM2)
      else
      CM2 = 0.0
      endif

      CM1 = CM1 / FLOAT(NUM)

      return
      end

      
      
      
      subroutine set up the correlation coefficients ! 4444444444444444444444444
      include 'COM.FOR'

      b1 = CO1 ! upstream river flow and upstream river quality 4444444444444444
      sq = 1.0 - b1*b1
      if (sq .gt. 0.00001) then
      b2 = SQRT (sq)
      else
      b2 = 0.0
      endif ! upstream river flow and upstream river quality 4444444444444444444
      
      c1 = CO2 ! upstream river flow on discharge flow 4444444444444444444444444
      sq = 1.0 - c1*c1
      if (sq .gt. 0.00001) then
      c2 = SQRT (sq)
      else
      c2 = 0.0
      endif ! upstream river flow on discharge flow 4444444444444444444444444444

      d1 = CO5 ! discharge flow on discharge quality 444444444444444444444444444
      sq = 1.0 - d1*d1
      if (sq .gt. 0.00001) then
      d2 = SQRT (sq)
      else
      d2 = 0.0
      endif ! upstream river flow on discharge flow 4444444444444444444444444444

 	return
      end

      
      subroutine impose correlation 4 (RR1, RR2, RR3, RR4)
      include 'COM.FOR'
      R1 = RR1                 ! river flow
      R2 = b1 * RR1 + b2 * RR2 ! correlate river quality and river flow
      R3 = c1 * RR1 + c2 * RR3 ! correlate discharge flow and river flow
      R4 = d1 * R3  + d2 * RR4 ! correlate discharge quality and discharge flow
      RR1 = R1
      RR2 = R2
      RR3 = R3
      RR4 = R4
      return
      end



*     compute Monte Carlo sampling errors for monthly river flow data ----------
      subroutine bias in monthly stream flows (imon)
      include 'COM.FOR'
	dimension Y(NS)

      BSM (imon) = 0.0
      BSS (imon) = 0.0
      do IS = 1, NS
      Y(IS) = 0.0
      enddo      

      do 2 is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
	jmonth = imonth
      if ( jmonth .ne. imon ) goto 2
      RFM = seas1 (jmonth) ! mean
      RF5 = seas2 (jmonth) ! 95-percentile low flow
      RF3 = seas3 (jmonth) ! shift flow
      spcorrRFaf = seas4 (jmonth) ! correlation of flow on added flow ----- ERAN

      GRFM = 0.0 ! mean for logged variables -----------------------------------
      GRFS = 0.0 ! standard deviation for logged variables ---------------------
	rex = 1.0e-9
	if ( RF5 + RF3 .gt. 1.0e-9 ) rex = RF5 + RF3
      GRFS = SQRoot(1021,2.7057+2.*ALOG((RFM+RF3)/(rex)))-1.6449
      GRFM = ALOG(RFM+RF3)-.5*GRFS*GRFS

      EDMM = ERAN function (is) ! get the random normal deviate ------------BIAS
      Y (is) = EXP ( GRFM + EDMM * GRFS) - RF3
    2 continue

	FM1 = 0.0
      FM2 = 0.0
	NUM = 0

      do IS = 1 , NS
      imonth = qmonth (is) ! set the month for this shot
	jmonth = imonth
	if ( jmonth .eq. imon ) then
      NUM = NUM + 1
	FM1 = FM1 + Y(IS)
	endif
      enddo

	NUM95 = amax1( 1.0, 0.05 * float (NUM))

      do 9 I = 1,num95
      do 8 J = I + 1 , NUM
      if (Y(I) .lt. Y(J)) goto 8
      FX = Y(I)
      Y(I) = Y(J)
      Y(J) = FX
    8 continue
    9 continue

      FM2 = Y (NUM95)
      FM1 = FM1 / FLOAT(NUM)

      BSM(imon) = FM1
      BSS(imon) = FM2
      return
      end


*     generate monthly data for river and tributary flow -----------------------
      subroutine generate monthly stream flow data
      include 'COM.FOR'
      logical exists
	dimension Y(NS)

      do is = 1, NS
	Y(is) = 0.0
	EFshots(is) = 0.0
	enddo

*     identify the file with the monthly data ----------------------------------
*     loop on the number of monthly datafiles ----------------------------------
      do 1 i = 1, M8
      icod = i
      if ( iseasp ( 1, i, 1 ) .eq. IF ) goto 2
    1 continue

*     no valid code found for the datafile -------------------------------------
      write( *,3)
      write(01,3)
      write(09,3)
      write(33,3)
    3 format(77('-')/
     &'*** Error in river flow data ...'/
     &'*** No valid code for the monthly dataset ...'/77('-'))
      call stop

*     valid code found. Check datafile exists ...
    2 continue

      inquire( FILE = flmonth(1,icod), EXIST = exists )
      if ( .NOT. exists) then
      write( *,7163) FLMONTHsmall(1,icod)
      write(01,7163) FLMONTHsmall(1,icod)
      write(09,7163) FLMONTHsmall(1,icod)
      write(33,7163) FLMONTHsmall(1,icod)
 7163 Format(/77('-')/
     &'*** Error in river flow data (monthly)...'/
     &'*** The data file does not exist ... ',a64/
     &'*** Run halted ...'/77('-'))
      call stop
      else
      if ( JP .eq. ndetlast  ) then
      if ( nobigout .le. 0 ) write(01,7863) FLMONTHsmall(1,icod)
 7863 format(77('-')/'Monthly data on river flow ... ',
     &'File: ',a64/77('-'))
	endif
	endif
      
      if ( nobigout .le. 0 ) write(01,4398)
 4398 format(77('-')/
     &'Monthly data on river flows ... ...'/77('-')/
     &'Month        ','       Mean','        95%','      Shift',
     &'  Correlation'/25x,'  exeedence'/77('-'))

*     get the file containing the monthly data ---------------------------------
      open(11,FILE = flmonth(1,icod), STATUS='OLD')
 
*     read the file containing the monthly data --------------------------------
      call read monthly data (1,icod) ! river flow - type 5

      if ( nobigout .le. 0 ) write(01,2399)(seas1(i),seas2(i),
     &seas3(i),seas4(i),seas0(i),i=1,12)
 2399 format(
     &'January ...  ',3f11.2,f13.4,i18/
     &'February ... ',3f11.2,f13.4,i18/
     &'March ...    ',3f11.2,f13.4,i18/
     &'April ...    ',3f11.2,f13.4,i18/
     &'May ...      ',3f11.2,f13.4,i18/
     &'June ...     ',3f11.2,f13.4,i18/
     &'July ...     ',3f11.2,f13.4,i18/
     &'August ...   ',3f11.2,f13.4,i18/
     &'September ...',3f11.2,f13.4,i18/
     &'October ...  ',3f11.2,f13.4,i18/
     &'November ... ',3f11.2,f13.4,i18/
     &'December ... ',3f11.2,f13.4,i18/77('-'))
 
      spcorrRFaf = F(IF,4) ! correlation of flow on added flow ------------ ERAN

*     calculate the Monte-Carlo sampling errors for monthly data on river flow -
*     loop through the months --------------------------------------------------

	do 13 imon = 1, 12

*     special version of BIAS for monthly data ---------------------------------
      call bias in monthly stream flows (imon)

      if (BSM(imon) .gt. 1.0E-08) BSM(imon) = seas1(imon)/BSM(imon)
      if (BSS(imon) .gt. 1.0E-08) then
      BSS(imon) = seas2(imon)/BSS(imon)
      else
      BSS(imon) = 1.0
      endif

      if ( MONF    .gt. 1 ) then
      if ( imon .eq. 1 ) then
      if ( nobigout .le. 0 ) write(01,12)
	endif
   12 format(/77('-')/
     &'Correction factors for sampling errors on river flow data:'/
     &77('-'))
      if ( nobigout .le. 0 ) write(01,92)imon,BSM(imon),BSS(imon)
   92 format('Monthly Mean',i3,' =',F8.3,
     &'        95-percentile =',F8.3)
	endif
   13 continue
*     write(01,93)
   93 format(77('-')/)

*     create the distribution of river flow -----------------------------------
      do 22 is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      RFM = seas1 (imonth) ! mean flow -----------------------------------------
      RF5 = seas2 (imonth) ! 95-percentile low flow ----------------------------
      RF3 = seas3 (imonth) ! shift flow ----------------------------------------
      spcorrRFaf = seas4 (imonth) ! correlation of flow on added flow ----- ERAN

      GRFM = 0.0 ! mean for logged variables -----------------------------------
      GRFS = 0.0 ! standard deviation for logged variables ---------------------
	rex = 1.0e-9
	if ( RF5 + RF3 .gt. 1.0e-9 ) rex = RF5 + RF3
      GRFS = SQRoot(1245,2.7057+2.*ALOG((RFM+RF3)/(rex)))-1.6449
      GRFM = ALOG(RFM+RF3)-.5*GRFS*GRFS

      EDMM = ERAN function (is) ! get the random normal deviate ----------------
*     compute the value of the flow shot ---------------------------------------
      Y(is) = VALFL ( EDMM, GRFS,GRFM,RF3, BSM(imonth),
     &BSS(imonth), RF5 )
      Y(is) = amax1(0.0, Y(is))
   22 continue

      do is = 1, NS
	EFshots (is) = Y (is)
      FTMS(is) = Y(is)
	enddo

      return

 7500 write( *,7168)FLMONTHsmall(1,icod)
      write(01,7168)FLMONTHsmall(1,icod)
      write(09,7168)FLMONTHsmall(1,icod)
      write(33,7168)FLMONTHsmall(1,icod)
 7168 Format(/77('-')/'*** Error in river flow data ...'/
     &'*** Error in reading monthly data ...'/
     &'*** Check the file ',a64/77('-'))
      call stop
      end

      
      

*     generate monthly structure on river flow ---------------------------------
      subroutine generate monthly structure for stream flows 8
      include 'COM.FOR'
      logical exists

	if ( F(IF,1) .lt. 1.0e-09 ) return
      do is = 1, NS
	YY(is) = 0.0
	enddo

*     identify the file with the monthly data ----------------------------------
      do 1 i = 1, M9
      icod = i
      if ( istruct ( 1, i, 1 ) .eq. IF ) goto 2
    1 continue

*     no valid code found for the datafile -------------------------------------
      write( *,3)
      write(01,3)
      write(09,3)
      write(33,3)
    3 format(77('-')/'*** Error in river flow data ...'/
     &'*** No valid code for the monthly structure ...'/77('-'))
      call stop

*     valid code found ... check the datafile exists ---------------------------
    2 continue
      Inquire( FILE = FLSTRUCT(1,icod), EXIST = exists )
      if ( .NOT. exists) then
      write( *,7163) FLSTRUCTsmall(1,icod)
      write(01,7163) FLSTRUCTsmall(1,icod)
      write(09,7163) FLSTRUCTsmall(1,icod)
      write(33,7163) FLSTRUCTsmall(1,icod)
 7163 Format(/77('-')/'*** Error in river flow data ... '/
     &'*** Monthly structure file does not exist ... ',a64/
     &'*** Run halted ... '/77('-'))
	call stop
      else
      if ( JP .eq. ndetlast ) then
      write(33,7863) FLSTRUCTsmall(1,icod)
 7863 format(77('-')/
     &'Monthly structure on river flows ... ',
     &'File: ',a64/77('-'))
	endif
      endif

*     get the file containing the monthly structure ----------------------------
      open(11,FILE = FLSTRUCT(1,icod), STATUS='OLD')

*     read the file containing the monthly structure ---------------------------
      call read monthly structure river flow data ! type 8
     &(1,0,icod,tmean,t95,t3,tcorr,itest12)
	if ( itest12 .eq. 12 ) then
      write( *,4311) FLSTRUCTsmall(1,icod)
      write(33,4311) FLSTRUCTsmall(1,icod)
      write(01,4311) FLSTRUCTsmall(1,icod)
      write(09,4311) FLSTRUCTsmall(1,icod)
 4311 format(77('*')/
     &'*** Null monthly structure has escaped detection ...'/
     &'*** File: ',a64/77('*'))
	endif

      ktest = 0
	do i = 1, 12
	if ( struct0 (i) .ne. 2 ) then
      ktest = ktest + 1
	struct0 (i) = 2
	endif
	enddo
	if ( ktest .gt. 0 ) then
      if ( ifbatch .eq. 0 ) then
      write( *,4011) FLSTRUCTsmall(1,icod)
	endif
      write(33,4011) FLSTRUCTsmall(1,icod)
      if ( nobigout .le. 0 ) write(01,4011) FLSTRUCTsmall(1,icod)
      write(09,4011) FLSTRUCTsmall(1,icod)
 4011 format(77('*')/
     &'*** Monthly structures have been over-written as ',
     &'log-normal ...'/'*** File: ',a64/77('*'))
	endif

      spcorrRFaf = tcorr ! correlation of flow on added flow -------------- ERAN

*     sample the distributions of data on river flow ---------------------------
      do 24 is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      YY(is) = 0.0
      RFM = struct1 (imonth) ! mean flow ---------------------------------------
	if ( RFM .gt. 1.0e-9 ) then
      RF5 = t95 * RFM / tmean ! 95-percentile low flow -------------------------
      RF3 = struct3 (imonth) ! shift flow --------------------------------------
      spcorrRFaf = struct4 (imonth) ! correlation of flow on added flow --- ERAN

      GRFM = 0.0 ! mean for logged variables -----------------------------------
      GRFS = 0.0 ! standard deviation for logged variables ---------------------
	rex = 1.0e-9
	if ( RF5 + RF3 .gt. 1.0e-9 ) rex = RF5 + RF3
      GRFS = SQRoot(301965,2.7057+2.*ALOG((RFM+RF3)/(rex)))-1.6449
      GRFM = ALOG(RFM+RF3)-.5*GRFS*GRFS

      EDMS8 = ERAN function (is) ! get the random normal deviate ---------------
*     compute the value of the shot --------------------------------------------
      YY(is) = EXP ( GRFM + EDMS8 * GRFS) - RF3
	endif
   24 continue
*     --------------------------------------------------------------------------
       
      call statistics for monthly river flows (0, CM1, CM2 )
*     write(33,4228)tmean,t95,NS
*4228 format('True flow ...',2f11.2,i11/46('-'))
      do imon = 1, 12
      BSM(imon) = tmean / FLOW(1)  
      BSS(imon) = t95 / FLOW(2)  
	enddo
*     write(01,4399)BSM(1),BSS(1)
*     write(33,4399)BSM(1),BSS(1)
*4399 format('Bias ...',2f12.6)

*     iterate to get to monthly shots that match the annual --------------------
      iterate = 0
 5555 continue
      iterate = iterate + 1

*     sample the distributions of data on river flow ---------------------------
      do 22 is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      YY(is) = 0.0
      RFM = struct1 (imonth) * BSM(1) ! mean flow ------------------------------
	if ( RFM .gt. 1.0e-9 ) then
      RF5 = struct2 (imonth) * BSS(1) ! 95-percentile low flow -----------------
      RF3 = struct3 (imonth) ! shift flow --------------------------------------
      spcorrRFaf = struct4 (imonth) ! correlation of flow on added flow --- ERAN

      GRFM = 0.0 ! mean for logged variables -----------------------------------
      GRFS = 0.0 ! standard deviation for logged variables ---------------------
	rex = 1.0e-9
	if ( RF5 + RF3 .gt. 1.0e-9 ) rex = RF5 + RF3
      GRFS = SQRoot(101966,2.7057+2.*ALOG((RFM+RF3)/(rex)))-1.6449
      GRFM = ALOG(RFM+RF3)-0.5*GRFS*GRFS

      EDMS8 = ERAN function (is) ! get the random normal deviate ---------------
*     compute the value of the shot --------------------------------------------
      YY(is) = VALFL month ( EDMS8,GRFS,GRFM,RF3)
      YY(is) = amax1 (0.0, YY(is))
	endif
   22 continue
*     --------------------------------------------------------------------------

      call statistics for monthly river flows (0, CM1, CM2 )
*     write(33,4128)tmean,t95,NS
*4128 format('True flow ...',2f11.2,i11/46('-'))
*     write(33,4528)CM1,CM2
*4528 format('CM        ...',2f11.2/46('-'))

      BSM(1) = BSM(1) * tmean  / CM1 
      if ( CM2 .gt. 0.000001 ) then 
      if ( t95 / CM2 .lt. 99.9 ) then
	BSS(1) = BSS(1) * t95    / CM2
      endif
      endif 
*     write(33,4699)BSM(1),BSS(1),iterate
*4699 format('Correction river flows 8',2f20.2,i7)

      rat1 = CM1 / tmean
	rat2 = CM2 / t95
*     write(33,4199)rat1,rat2,iterate
*4199 format('Convergence ',2f11.5,i7)

      if ( rat1 .lt. 1.0001 .and. rat1 .gt. 0.9999) then
      if ( rat2 .lt. 1.0001 .and. rat2 .gt. 0.9999) goto 5558
      if ( iterate .ne. 100 ) goto 5555
      else
      if ( iterate .ne. 100 ) goto 5555
	endif
 5558 continue

*     write(33,4199)rat1,rat2,iterate

      if ( iterate .gt. 99 ) then
      if ( ifbatch .ne. 1 ) then 
 	call change colour of text (14) ! bright yellow
	if ( ifbatch .ne. 1 ) write( *,5166)FLSTRUCTsmall(1,icod)
 5166 format('Unable to set up the specified ',
     &'monthly structure of river flow (8) ... ',a64)
      call set screen text colour
      endif
	write(33,5266)FLSTRUCTsmall(1,icod)
 5266 format(110('-')/'Unable to set up the specified ',
     &'monthly structure of river flow (8) ... ',a64/110('-'))
	endif

      do is = 1, NS
	EFshots (is) = YY(is)
	enddo
 
      if ( MONF .gt. 1 ) then
	call write the shots for the flow from the stream
	endif

      call statistics for monthly river flows (0, CM1, CM2 )
*     write(33,4499)CM1,CM2
*     write(33,4699)BSM(1),BSS(1),iterate
*4499 format('Bias ...',2f12.6)
*     write(33,4128)tmean,t95,NS

      if ( MONF .gt. 1 ) call write shots for river flow
      return

 7500 write( *,7168) FLSTRUCTsmall(1,icod)
      write(01,7168) FLSTRUCTsmall(1,icod)
      write(09,7168) FLSTRUCTsmall(1,icod)
      write(33,7168) FLSTRUCTsmall(1,icod)
      call stop
 7168 Format(/77('-')/'*** Error in river flow data ...'/
     &'*** Error in reading data for monthly structure ...'/
     &'*** Check the file ',a64/77('-'))
      end









*     generate monthly structure on data for river flow ------------------------
      subroutine generate monthly structure for stream flows 2
      include 'COM.FOR'

	if ( F(IF,1) .lt. 1.0e-09 ) return
      do is = 1, NS
	YY(is) = 0.0
	enddo

	tmean = F(IF,1)
	t95 = F(IF,2)
	if ( PDRF(IF) .eq. 3 ) t3 = F(IF,3)

      if ( t95 .gt. tmean ) then
	if ( idfcheck .eq. 1 ) then
      write(01,8151)t95,IF,tmean
      
      if ( ifbatch .ne. 1 .and. kerror .eq. 1 ) then
	call change colour of text (21) ! dull red
      write( *,8851)
 8851 format('* Unworkable river flow data found when ',
     &'imposing monthly structure ...')
      call set screen text colour
      endif
      
      write(09,8151)t95,IF,tmean
      write(33,8151)t95,IF,tmean
 8151 format(77('-')/'*** Unworkable ',
     &'river flow data found when imposing monthly structure ...'/
     &'*** The 95-percentile low flow of ',f11.5,' in data set',i5/
     &'*** is too big for a mean of ',f11.5/
     &'*** The 95-percentile was reset to 10% of the mean ...'/77('-'))
	endif
	t95 = 0.1 * tmean
	t3 = 0.0
	endif

*     correlation coefficient --------------------------------------------------
	tcorr = F(IF,4)
      trat = t95/tmean     

*     set log-normal distributions by default ----------------------------------
      do i = 1,12
*     struct0(i) = 2
      struct3(i) = 0.0
	struct4(i) = -9.9
	enddo

      struct1( 1) = 1.9
      struct1( 2) = 1.6
      struct1( 3) = 1.4
      struct1( 4) = 1.2
      struct1( 5) = 0.6
      struct1( 6) = 0.5
      struct1( 7) = 0.3
      struct1( 8) = 0.4
      struct1( 9) = 0.4
      struct1(10) = 0.8
      struct1(11) = 1.2
      struct1(12) = 1.7

*     calculate ration of 95-percentile to mean --------------------------------
      tratio = t95 / tmean

*     compare the ration with the smallest average monthly flow ---------------- 
	if ( tratio + 0.5 .gt. struct1 ( 7) ) then
	tadd = tratio - struct1 (7) + 0.2
      struct1( 1) = struct1( 1) - tadd
      struct1( 2) = struct1( 2) - tadd
      struct1( 3) = struct1( 3) - tadd
      struct1( 4) = struct1( 4) - tadd
      struct1( 5) = struct1( 5) + tadd
      struct1( 6) = struct1( 6) + tadd
      struct1( 7) = struct1( 7) + tadd
      struct1( 8) = struct1( 8) + tadd
      struct1( 9) = struct1( 9) + tadd
      struct1(10) = struct1(10) + tadd
      struct1(11) = struct1(11) - tadd
      struct1(12) = struct1(12) - tadd
	endif

      test1 = 0.0
      do imon = 1,12
      test1 = test1 + struct1(imon) - 1.0
 	enddo
 
   
	struct2( 1) = 0.4
      struct2( 2) = 0.4
      struct2( 3) = 0.4
      struct2( 4) = 0.4
      struct2( 5) = 0.4
      struct2( 6) = 0.4
      struct2( 7) = 0.4
      struct2( 8) = 0.4
      struct2( 9) = 0.4
      struct2(10) = 0.4
      struct2(11) = 0.4
      struct2(12) = 0.4

*     set the means ... set the 95-percentiles ---------------------------------
      do i = 1, 12
      struct1 (i) = struct1 (i) * tmean
      struct2 (i) = struct1 (i) * struct2 (i)
	enddo
*     --------------------------------------------------------------------------

      test2 = 0.0

      call check and correct monthly structure correlation coefficients

*     test the monthly data ----------------------------------------------------
      if ( test1 .gt. 0.0001 .or. test1 .lt. -0.0001 ) then
      write(01,2000)test1
      write( *,2000)test1
      write(09,2000)test1
      write(33,2000)test1
 2000 Format(/77('-')/'*** Error in the monthly structure ...'/
     &'*** The monthly adjustments do not sum to zero ...'/
     &'*** Look at the monthly averages ...',f12.6)
      do i = 1,12
      write(01,2101)i,struct1(i)
 2101 format(i4,f12.4)
	enddo
      call stop
	endif

      if ( test2 .gt. 0.0001 .or. test2 .lt. -0.001 ) then
      write(01,2001)test2
      write( *,2001)test2
      write(09,2001)test2
      write(33,2001)test2
 2001 Format(/77('-')/
     &'*** Error in the monthly structure for river flow ...'/
     &'*** The monthly adjustments do not sum to zero ...'/
     &'*** Look at the monthly percentiles ...',f12.6/77('-'))
*     call stop
	endif
*     --------------------------------------------------------------------------



*     correct monthly data for the number of days in a month -------------------
      do imon = 1, 12
      struct1(imon) = (365.0/12.0) * struct1(imon) / 
     &                days in months (imon)
	struct6(imon) = struct4(imon)
	if ( struct6 (imon) .lt. -2.0 ) struct6 (imon) = 0.0
	enddo
*     if ( nobigout .le. 0 ) write(01,3245)
 3245 format(35('-')/'MONTHLY data on river flow'/35('-'))
*     if ( nobigout .le. 0 ) write(01,2398)
 2398 format('Month',15x,'Mean',8x,'95%'/26x,'exeedence'/35('-'))
*     if ( nobigout .le. 0 ) write(01,8599)(struct1(i),
*    &struct2(i),i=1 ,12)
 8599 format(
     &'January ...  ',2f11.2/
     &'February ... ',2f11.2/
     &'March ...    ',2f11.2/
     &'April ...    ',2f11.2/
     &'May ...      ',2f11.2/
     &'June ...     ',2f11.2/
     &'July ...     ',2f11.2/
     &'August ...   ',2f11.2/
     &'September ...',2f11.2/
     &'October ...  ',2f11.2/
     &'November ... ',2f11.2/
     &'December ... ',2f11.2/35('-')/)
*     --------------------------------------------------------------------------

      spcorrRFaf = tcorr ! correlation of flow on added flow -------------- ERAN

*     sample the distributions of data on river flow ---------------------------
      do 24 is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      YY(is) = 0.0
      RFM = struct1 (imonth) ! mean flow ---------------------------------------
	if ( RFM .gt. 1.0e-9 ) then
      RF5 = struct2 (imonth) ! 95-percentile low flow --------------------------
      RF3 = struct3 (imonth) ! shift -------------------------------------------
      spcorrRFaf = struct4 (imonth) ! correlation of flow on added flow --- ERAN

      GRFM = 0.0 ! mean for logged variables -----------------------------------
      GRFS = 0.0 ! standard deviation for logged variables ---------------------
	rex = 1.0e-9
	if ( RF5 + RF3 .gt. 1.0e-9 ) rex = RF5 + RF3
      xxx = amax1 (1.0e-14,(RFM+RF3)/(rex))
	xx7 = 2.7057+2.*ALOG(xxx)
      GRFS = SQRoot(2019,2.7057+2.*ALOG(xxx))-1.6449
      GRFM = ALOG(RFM+RF3)-.5*GRFS*GRFS

      EDMS2 = ERAN function (is) ! get the random normal deviate ---------------
*     compute the value of the shot --------------------------------------------
      YY(is) = EXP ( GRFM + EDMS2 * GRFS) - RF3
	endif
   24 continue

      call statistics for monthly river flows (0, CM1, CM2 )
*     write(33,4228)tmean,t95,NS
*4228 format('True flow ...',2f11.2,i11/46('-'))
      do imon = 1, 12
      BSM(imon) = tmean / FLOW(1)  
      BSS(imon) = t95 / FLOW(2)  
	enddo
*     write(33,4399)BSM(1),BSS(1)
*4399 format('Bias ...',2f12.6)

*     iterate to get to monthly shots that match the annual --------------------
      iterate = 0
 5555 continue
      iterate = iterate + 1

*     sample the distributions of data on river flow ---------------------------
      do 22 is = 1, NS
      imonth = qmonth (is) ! set the month for this shot
      YY(is) = 0.0
      RFM = struct1 (imonth) * BSM(1) ! mean flow ------------------------------
	if ( RFM .gt. 1.0e-9 ) then
      RF5 = struct2 (imonth) * BSS(1) ! 95-percentile low flow -----------------
      RF3 = struct3 (imonth) ! shift flow --------------------------------------
      if ( RF5 .gt. RFM ) then
	RF5 = 0.99 * RFM
	endif
      spcorrRFaf = struct4 (imonth) ! correlation of flow on added flow --- ERAN

      GRFM = 0.0 ! mean for logged variables -----------------------------------
      GRFS = 0.0 ! standard deviation for logged variables ---------------------
	rex = 1.0e-9
	rex = 1.0e-9
	if ( RF5 + RF3 .gt. 1.0e-9 ) rex = RF5 + RF3
      GRFS = SQRoot(301962,2.7057+2.*ALOG((RFM+RF3)/(rex)))-1.6449
      GRFM = ALOG(RFM+RF3)-0.5*GRFS*GRFS

      EDMS2 = ERAN function (is) ! get the random normal deviate ---------------
*     compute the value of the shot --------------------------------------------
      YY(is) = VALFL month ( EDMS2,GRFS,GRFM,RF3)
      YY(is) = amax1 (0.0, YY(is))
	endif
   22 continue

      call statistics for monthly river flows (0, CM1, CM2 )
*     write(01,4128)tmean,t95,NS
*4128 format('True flow ...',2f11.2,i11/46('-'))

      BSM(1) = BSM(1) * tmean  / CM1 
	BSS(1) = BSS(1) * t95    / CM2 

      rat1 = CM1 / tmean
	rat2 = CM2 / t95
*     write(01,4199)rat1,rat2,iterate
*4199 format('Convergence ',2f11.5,i7)

      if ( rat1 .lt. 1.0001 .and. rat1 .gt. 0.9999) then
      if ( rat2 .lt. 1.0001 .and. rat2 .gt. 0.9999) goto 5558
      if ( iterate .ne. 100 ) goto 5555
      else
      if ( iterate .ne. 100 ) goto 5555
	endif
 5558 continue

      if ( iterate .eq. 100 .and. JSKIP .eq. 0 ) then
      if ( idfcheck .eq. 1 .and. ical .ne. 3 ) then
      if ( ifbatch .ne. 1 .and. kerror .eq. 1 ) then
  	call change colour of text (14) ! bright yellow
	write( *,5466)IF
 5466 format('* Unable to set up an imposed ',
     &'monthly structure of river flow:',i7)
      call set screen text colour
      endif
	write(33,5266)IF
 5266 format(100('-')/'Unable to set up the imposed ',
     &'monthly structure of the river flow set:',i7/100('-'))
	write(33,5966)iterate
 5966 format(100('-')/'Iterations in setting up the imposed ',
     &'monthly structure of river flow ...',i4/100('-'))
	endif
	endif

      do is = 1, NS
	EFshots (is) = YY(is)
	enddo
 
      if ( MONF .gt. 1 ) then
	call write the shots for the flow from the stream
	endif

      call statistics for monthly river flows (0, CM1, CM2 )
*     write(01,4499)CM1,CM2
*     write(01,4699)BSM(1),BSS(1),iterate
*4499 format('Bias ...',2f12.6)
*     write(01,4128)tmean,t95,NS

      if ( MONF .gt. 1 ) call write shots for river flow
      return

 7500 write( *,7168)
      write(01,7168)
      write(09,7168)
      write(33,7168)
      call stop
 7168 Format(/77('-')/'*** Error in river flow data ...'/
     &'*** Error in reading monthly structure ...'/
     &77('-'))
      end



*     get random normal deviate for added flow ---------------------------------
*     imposing the correct correlation -----------------------------------------
      function ERAN function ( IS )
      include 'COM.FOR'
      itest = 0
	cr = spcorrRFaf ! correlation of flow on added flow ----------------- ERAN
*     default deviate for flow in the main river -------------------------------
      RRS = FRAN (IS)
*     check there is no special correlation ------------------------------------
      if ( cr .lt. -9.899 .and. cr .gt. -9.901 ) itest = 1
      if ( cr .lt.  0.001 .and. cr .gt. -0.001 ) itest = 1
      if ( itest .eq. 1 ) then
*     CO2 is the correlation of river flow on discharge flow -------------------
      xERAN = CO2 * RRS + ERAN (IS) * SQRMB(117, 1.0 - CO2*CO2 )
      else
*     non-standard correlation -------------------------------------------------
      xERAN = cr * RRS + ERAN (IS) * SQRMB(118, 1.0 - cr*cr )
      endif
      ERAN function = xERAN
      return
      end



      subroutine write the shots for the flow from the stream
      include 'COM.FOR'

      if ( Qtype (JP) .eq. 4 ) return
      if ( if diffuse .eq. 0 ) then
      if ( jt (feeture) .eq.  2 ) then
      if ( nobigout .le. 0 )  then
      write(01,13)
   13 format(150('-')/'Shots for the flow added from the tributary ',
     &'...'/150('-'))
	endif

      write(01,14)(EFshots(IS),IS=1,NS)
   14 format(f7.3,20F7.3)
*     write(01,93)
*  93 format(150('-')/'Random normal deviates (ERAN) ...'/
*    &150('-'))
*     write(01,2214)(ERAN(IS),IS=1,40)
*     write(01,63)
*  63 format(150('-')/'Random normal deviates (FRAN) ...'/
*    &150('-'))
*     write(01,2214)(FRAN(IS),IS=1,40)
*     write(01,33)
*  33 format(150('-')/'Correlated random normal deviates from ',
*    &'ERAN function (ERANR) ...'/150('-'))
*     write(01,2214)(ERANR(IS),IS=1,40)
*     write(01,34)
*  34 format(150('-')/'Truncated random numbers (ERANR2)',
*    &'...'/150('-'))
*     write(01,2214)(ERANR2(IS),IS=1,40)
*     write(01,37)Cut off zero flow
*  37 format(150('-')/'Truncated random normal deviates (ERANR3) ',
*    &'... ',f12.6/150('-'))
*     write(01,2214)(ERANR3(IS),IS=1,40)
*2214 format(f10.3,20F7.3)
      write(01,19)
   19 format(150('-'))

      call get statistics for the flows of the discharge or stream
	if ( nobigout .le. 0 ) then
      write(01,29)EFlow(1),EFlow(2)
   29 Format('Calculated mean flow =',f8.2/
     &       'Standard deviation   =',f8.2)       
      if ( nobigout .le. 0 ) write(01,19)
      endif

      CM=0.0
      CS=0.0
	KS=0
      do IS=1,NS
	if ( abs(EFshots(IS)) .gt. 1.0e-12 ) then
      CM=CM+EFshots(IS)
      CS=CS+EFshots(IS)*EFshots(IS)
	KS=KS+1
	endif
      enddo
      if (KS .gt. 0) then
      if (KS .gt. 1) then
      if (CS .gt. 1.0E-10) goto 55
      CS=0.0
      goto 56
   55 CS=(CS-CM*CM/KS)/(KS-1)
      if (CS .gt. 1.0E-20) goto 69
      CS=0.0
      goto 56
   69 CS=SQRoot(11099,CS)
   56 CM=CM/KS
      else
	CS=0.0
      endif
	endif

*     if ( KS .lt. NS .and. .gt. 0 ) then
*     if ( nobigout .le. 0 ) write(01,25) CM, KS,CS
   25 Format(48x,'Calculated mean (for ',
     &'non-zero flows) =',f18.6,i6,
     &'    ...  standard deviation =',f8.4)       
      if ( nobigout .le. 0 ) write(01,19)
*     endif
      endif

      endif

      if ( if diffuse .gt. 0 ) then
	if ( jt (KFEET) .eq. 15 .or. jt (KFEET) .eq. 13   .or.
     &     jt (KFEET) .eq. 25 .or. jt (KFEET) .eq. 27   .or.
     &     jt (KFEET) .eq. 29 .or. jt (KFEET) .eq. 31   .or.
     &     jt (KFEET) .eq. 33 .or. jt (KFEET) .eq. 35   .or.
     &     jt (KFEET) .eq. 46 .or. jt (KFEET) .eq. 48   .or.
     &     jt (KFEET) .eq. 37 .or. jt (KFEET) .eq. 40 ) then  

      if ( nobigout .le. 0 ) then
      write(01,88)distp
   88 format(150('-')/'Shots for the flow added from the diffuse',
     &' inflow over',f7.2,' kilometres ...'/150('-'))

*     write(01,914)(EFshots(IS),IS=1,40)
*     write(01,19)
      write(01,914)(distp*EFshots(IS),IS=1,NS)
  914 format(f7.2,20F7.2)
*     write(01,93)
*     write(01,2214)(ERAN(IS),IS=1,40)
*     write(01,63)
*     write(01,2214)(FRAN(IS),IS=1,40)
*     write(01,33)
*     write(01,2214)(ERANR(IS),IS=1,40)
*     write(01,34)
*     write(01,2214)(ERANR2(IS),IS=1,40)
*     write(01,37)Cut off zero flow
*     write(01,2214)(ERANR3(IS),IS=1,40)
      write(01,19)
	endif

      call get statistics for the flows of the discharge or stream
	if ( nobigout .le. 0 ) write(01,49)EFlow(1),EFlow(2)
   49 Format(44x,'Calculated mean flow added per kilometre =',f18.6,10x,
     &'...  standard deviation =',f8.2)       
      if ( nobigout .le. 0 ) write(01,19)

      CM=0.0
      CS=0.0
	KS=0
      do IS=1,NS
	if ( abs(EFshots(IS)) .gt. 1.0e-12 ) then
      CM=CM+EFshots(IS)
      CS=CS+EFshots(IS)*EFshots(IS)
	KS=KS+1
	endif
      enddo
      if (KS .gt. 0) then
      if (KS .gt. 1) then
      if (CS .gt. 1.0E-10) goto 155
      CS=0.0
      goto 156
  155 CS=(CS-CM*CM/KS)/(KS-1)
      if (CS .gt. 1.0E-20) goto 169
      CS=0.0
      goto 156
  169 CS=SQRoot(11099,CS)
  156 CM=CM/KS
      else
	CS=0.0
      endif
	endif

      if ( KS .lt. NS .and. KS .gt. 0 ) then
	if ( nobigout .le. 0 ) write(01,25) CM, KS,CS
      if ( nobigout .le. 0 ) write(01,19)
	endif
	endif

	endif

      return
      end




*     get random normal deviate for river quality ------------------------------
*     imposing the correct correlation -----------------------------------------
      function cRAN function ( JDET, JS, IS ) ! added flow on added quality ----
      include 'COM.FOR'
      if ( cut off zero quality (JDET) .lt. 1.0e-08 ) then
      itest = 0
      cr = CO5 ! correlation  for added flow on added discharge quality ----cRAN
*     check there is no special correlation of flow on quality -----------------
      if ( cr .lt. -9.899 .and. cr .gt. -9.901 ) itest = 1
      if ( cr .lt.  0.001 .and. cr .gt. -0.001 ) itest = 1
      if ( itest .eq. 1 ) then
      xx = CRAN (JDET, JS)
      else
*     non-standard correlation -------------------------------------------------
*     take note of the truncated normal deviates held in ERANR3 ----------------
      RRS = FRAN (IS)
      xx = cr * RRS + CRAN (JDET,JS) * SQRMB(121, 1.0 - cr * cr )
      endif
      cRAN function = xx
	else
      
*     the added concentrations or loads are intermittent -----------------------
      itest = 0
*     correlation coefficient for added flow on added discharge quality --------
	cr = CO5
*     check there is no special correlation of flow on quality -----------------
      if ( cr .lt. -9.899 .and. cr .gt. -9.901 ) itest = 1
      if ( cr .lt.  0.001 .and. cr .gt. -0.001 ) itest = 1
      if ( itest .eq. 1 ) then
      xx = CRAN (JDET,JS)
      else
*     non-standard correlation -------------------------------------------------
*     take note of the truncated normal deviates held in ERANR3 ----------------
      RRS = FRAN (IS)
      xx = cr * RRS + CRAN (JDET,JS) * SQRMB(122, 1.0 - cr * cr )
      endif
      cRAN function = xx
	endif
      return
      end






*     set up the shots from monthly data on discharge flow ---------------------
	subroutine set up monthly data for bifurcation
      include 'COM.FOR'
 
	if ( JFEAT .lt. 0  ) return
      JFEAT = KFEAT + 1
      JTYPE = JT (JFEAT)

      if ( IFDIST .eq. 5  ) then

*     for a stream -------------------------------------------------------------
	if ( JTYPE .eq. 2 ) call generate monthly stream flow data
	if ( JTYPE .eq. 2 ) call generate monthly stream flow data
	if ( JTYPE .eq. 13) call generate monthly stream flow data
	if ( JTYPE .eq. 18) call generate monthly stream flow data
	if ( JTYPE .eq. 20) call generate monthly stream flow data
	if ( JTYPE .eq. 21) call generate monthly stream flow data
	if ( JTYPE .eq. 25) call generate monthly stream flow data
	if ( JTYPE .eq. 27) call generate monthly stream flow data
	if ( JTYPE .eq. 29) call generate monthly stream flow data
	if ( JTYPE .eq. 31) call generate monthly stream flow data
	if ( JTYPE .eq. 33) call generate monthly stream flow data
	if ( JTYPE .eq. 35) call generate monthly stream flow data
	if ( JTYPE .eq. 46) call generate monthly stream flow data
	if ( JTYPE .eq. 48) call generate monthly stream flow data
	if ( JTYPE .eq. 37) call generate monthly stream flow data
	if ( JTYPE .eq. 40) call generate monthly stream flow data


*     or for effluents ---------------------------------------------------------
      if ( JTYPE .eq. 3  ) call monthly discharge flow
      if ( JTYPE .eq. 5  ) call monthly discharge flow
      if ( JTYPE .eq. 12 ) call monthly discharge flow
      if ( JTYPE .eq. 15 ) call monthly discharge flow
      if ( JTYPE .eq. 19 ) call monthly discharge flow
      if ( JTYPE .eq. 22 ) call monthly discharge flow
      if ( JTYPE .eq. 23 ) call monthly discharge flow
      if ( JTYPE .eq. 39 ) call monthly discharge flow
      if ( JTYPE .eq. 42 ) call monthly discharge flow

      endif

*     set up shots from monthly data on discharge quality ----------------------
	if ( qtype (jp) .ne. 4 ) then
      if ( IQDIST .eq. 5 ) then

*     the stream ---------------------------------------------------------------
	if ( JTYPE .eq. 2  ) call monthly stream quality
	if ( JTYPE .eq. 13 ) call monthly stream quality
	if ( JTYPE .eq. 18 ) call monthly stream quality
	if ( JTYPE .eq. 20 ) call monthly stream quality
	if ( JTYPE .eq. 21 ) call monthly stream quality
      call write out the added flow shots

*     effluents ----------------------------------------------------------------
      if ( JTYPE .eq. 3  ) call monthly discharge quality
      if ( JTYPE .eq. 5  ) call monthly discharge quality
      if ( JTYPE .eq. 12 ) call monthly discharge quality
      if ( JTYPE .eq. 15 ) call monthly discharge quality
      if ( JTYPE .eq. 19 ) call monthly discharge quality
      if ( JTYPE .eq. 22 ) call monthly discharge quality
      if ( JTYPE .eq. 23 ) call monthly discharge quality
      if ( JTYPE .eq. 39 ) call monthly discharge quality
      if ( JTYPE .eq. 42 ) call monthly discharge quality
      call write out the added flow shots
      endif
	endif

	return
	end




*     write the shots for the flow of the discharge ----------------------------
      subroutine write the shots for the flow from the discharge
      include 'COM.FOR'

	if ( jt (feeture) .eq.  3 .or. jt (feeture) .eq. 5   .or.
     &     jt (feeture) .eq. 15 .or. jt (feeture) .eq. 16  .or.
     &     jt (feeture) .eq. 43 .or. 
     &     jt (feeture) .eq. 39 .or. jt (feeture) .eq. 12) then

*     call write the correlation coefficients ----------------------------------

      if ( nobigout .le. 0 ) write(01,13)
   13 format(150('-')/'Generated shots for the flow of discharge into ',
     &'the main river ...'/150('-'))
      if ( nobigout .le. 0 ) write(01,14)(EFshots(IS),IS=1,NS)
   14 format(f7.2,20F7.2)
*     if ( IFDIST .eq. 4 ) then
*     if ( nobigout .le. 0 ) write(01,33)
*  33 format(150('-')/'Random normal deviates (ERAN)...'/150('-'))
*	write(01,2214)(ERANR(IS),IS=1,NS)
*     if ( nobigout .le. 0 ) write(01,43)
*  43 format(150('-')/'Truncated random numbers (ERANR2)...'/150('-'))
*     write(01,2214)(ERANR2(IS),IS=1,NS)
*2214 format(f10.2,20F7.4)
*     write(01,37)Cut off zero flow
*  37 format(150('-')/'Truncated random normal discharge flow ',
*    &'deviates (ERANR3) ...',f12.6,' (cut-off flow)'/150('-'))
*     write(01,2224)(ERANR3(IS),IS=1,NS)
*2224 format(7x,20F7.3)
*     endif
      if ( nobigout .le. 0 ) write(01,15)
   15 format(150('-'))

      call get statistics for the flows of the discharge or stream
	if ( nobigout .le. 0 ) write(01,29)EFlow(1),EFlow(2)
   29 Format(65x,'Calculated annual mean flow =',f8.2,
     &       '            ...  standard deviation =',f8.2)       
      if ( nobigout .le. 0 ) write(01,15)
*	write(33,29)EFlow(1),EFlow(2)
*  59 Format('Calculated annual mean flow   =',f8.2,
*    &'            ...  standard deviation =',f8.2)       
*     write(33,15)

      if ( IFDIST .eq. 4 ) then
      CM=0.0
      CS=0.0
	KS=0
      do IS=1,NS
	if ( abs(ERANR3(IS)) .gt. 1.0e-12 ) then
      CM=CM+ERANR3(IS)
      CS=CS+ERANR3(IS)*ERANR3(IS)
	KS=KS+1
	endif
      enddo
      if (KS .gt. 0) then
      if (KS .gt. 1) then
      if (CS .gt. 1.0E-10) goto 55
      CS=0.0
      goto 56
   55 CS=(CS-CM*CM/KS)/(KS-1)
      if (CS .gt. 1.0E-20) goto 69
      CS=0.0
      goto 56
   69 CS=SQRoot(12099,CS)
   56 CM=CM/KS
      else
	CS=0.0
      endif
	endif

*     if ( nobigout .le. 0 ) write(01,25) CM, KS, CS
   25 Format(48x,'Calculated MEAN deviate (for ',
     &'non-zero flows) =',f8.4,I6,6x,
     &'...  standard deviation =',f8.4)       
      if ( nobigout .le. 0 ) write(01,15)

      CM=0.0
      CS=0.0
	KS=0
      do IS=1,NS
	if ( EFshots(IS) .gt. 1.0e-9 ) then
      CM=CM+EFshots(IS)
      CS=CS+EFshots(IS)*EFshots(IS)
	KS=KS+1
	endif
      enddo
      if (KS .gt. 0) then
      if (KS .gt. 1) then
      if (CS .gt. 1.0E-10) goto 155
      CS=0.0
      goto 156
  155 CS=(CS-CM*CM/KS)/(KS-1)
      if (CS .gt. 1.0E-20) goto 169
      CS=0.0
      goto 156
  169 CS=SQRoot(13099,CS)
  156 CM=CM/KS
      else
	CS=0.0
      endif
	endif

	if ( nobigout .le. 0 ) write(01,125) CM, KS, CS
  125 Format(56x,'Calculated MEAN (for ',
     &'non-zero flows) =',f8.2,I6,6x,
     &'...  standard deviation =',f8.2)       
      if ( nobigout .le. 0 ) write(01,15)

*     do is = 1, NS
*     if ( EFshots(IS) .gt. 1.0e-09) then
*	if ( ERANR3(IS) .lt. 1.0e-09 .and. ERANR2(IS) .lt. 1.0e-09) then
*	write(01,8433)IS,EFshots(IS),ERANR3(IS),ERANR2(IS)
*8433 format(i4,3f10.4)
*     endif
*	endif
*	enddo
      endif

	endif
      return
      end




*     write out the shots for tributary quality --------------------------------
      subroutine write shots for stream quality
      include 'COM.FOR'
      dimension xxx(MS)
      
      if ( qtype(JP).eq. 4 ) return
      if ( if diffuse .eq. 0 ) then
      if ( jt (feeture) .eq.  2 ) then
*     -------------------------------------------------------------------------a
      if ( nobigout .le. 0 ) then
      if ( IQDIST .eq. 6 .or. IQDIST .eq. 7 .or. IQDIST .eq. 9 ) then
      write(01,13) dname(jp)
   13 format(150('-')/'Generated shots for added LOAD: ',a11/150('-'))
      else
      write(01,12) dname(jp)
   12 format(150('-')/'Generated shots for added stream quality: ',a11/
     &150('-'))
	endif
*     -------------------------------------------------------------------------a
      write(01,14) (ECshots(IS),IS=1,NS)
   14 format(f7.3,20F7.3)
      endif
      if ( nobigout .le. 0 ) write(01,15)
   15 format(150('-'))

      CM=0.0
      CS=0.0
      do IS=1,NS
      CM=CM+ECshots(IS)
      CS=CS+ECshots(IS)*ECshots(IS)
      enddo

      if (CS .gt. 1.0E-10) goto 5
      CS=0.0
      goto 6
    5 CS=(CS-CM*CM/NS)/(NS-1)
      if (CS .gt. 1.0E-20) goto 9
      CS=0.0
      goto 6
    9 CS=SQRoot(14099,CS)
    6 CM=CM/NS

      if ( nobigout .le. 0 ) then
      if ( IQDIST .eq. 6 .or. IQDIST .eq. 7 .or. IQDIST .eq. 9 ) then
	write(01,2922) CM, CS
 2922 Format(62x,'   Calculated annual mean load =',f8.2,
     &       '           ...  Standard deviation =',f8.2)       
      write(01,15)
	else
	write(01,29) CM, CS
   29 Format(62x,'Calculated annual mean quality =',f8.2,
     &       '           ...  Standard deviation =',f8.2)       
      write(01,15)
      endif
      endif

*     calculate statistics for non-zero quality --------------------------------
      CM=0.0
      CS=0.0
	KS=0
      do IS=1,NS
      if (ECshots(IS) .gt. 1.0e-9 ) then
      CM=CM+ECshots(IS)
      CS=CS+ECshots(IS)*ECshots(IS)
	KS=KS+1
	endif
      enddo
*     -------------------------------------------------------------------------a
      if (KS .gt. 0) then
*     -------------------------------------------------------------------------b
      if (KS .gt. 1) then
      if (CS .gt. 1.0E-10) goto 655
      CS=0.0
      goto 656
  655 CS=(CS-CM*CM/KS)/(KS-1)
      if (CS .gt. 1.0E-20) goto 669
      CS=0.0
      goto 656
  669 CS=SQRoot(15099,CS)
  656 CM=CM/KS
      else
	CS=0.0
      endif
*     -------------------------------------------------------------------------b
*     -------------------------------------------------------------------------c
      if ( nobigout .le. 0 ) then
      if ( IQDIST .eq. 6 .or. IQDIST .eq. 7 .or. IQDIST .eq. 9 ) then
	write(01,2522) CM, CS, KS, NS
 2522 Format(53x,'Calculated mean load (for ',
     &'non-zero flows) =',f8.2,10x,
     &'...  Standard deviation =',f8.2,2i6)       
      write(01,15)
	else
	write(01,25) CM, CS, KS, NS
   25 Format(49x,'Calculated mean quality (for ',
     &'non-zero flows) =',f8.2,10x,
     &'...  Standard deviation =',f8.2,2i6)       
      write(01,15)
	endif
	endif
	endif

*     write(01,66)IQ,JP,quolity data(IQ,JP,1)
   66 format(2i3,f12.6)

	CMlast = CM
	CMwant = quolity data(IQ,JP,1)
      CMratio = CMwant / CMlast

*     calculate statistics for non-zero quality --------------------------------
      CM=0.0
      CS=0.0
	KS=0
      do IS=1,NS
      if (ECshots(IS) .gt. 1.0e-9 ) then
      ECshots(IS) = ECshots(IS) * CMratio
      CM=CM+ECshots(IS)
      CS=CS+ECshots(IS)*ECshots(IS)
	KS=KS+1
	endif
      enddo
*     -------------------------------------------------------------------------a
      if (KS .gt. 0) then
*     -------------------------------------------------------------------------b
      if (KS .gt. 1) then
      if (CS .gt. 1.0E-10) goto 255
      CS=0.0
      goto 256
  255 CS=(CS-CM*CM/KS)/(KS-1)
      if (CS .gt. 1.0E-20) goto 269
      CS=0.0
      goto 256
  269 CS=SQRoot(16099,CS)
  256 CM=CM/KS
      else
	CS=0.0
      endif
	endif
	if ( nobigout .le. 0 ) write(01,225) CM, CS
  225 Format(49x,'Calculated mean quality (for ',
     &'non-zero flows) =',f8.2,
     &       '           ...  Standard deviation =',f8.2)       
      if ( nobigout .le. 0 ) write(01,15)

	endif  !if ( jt (feeture) .eq.  2 )
	endif  !if ( if diffuse .eq. 0 ) 


*     diffuse inputs -----------------------------------------------------------
      if ( if diffuse .gt. 0 ) then
      if ( jt (KFEET) .eq. 15 .or. jt (KFEET) .eq. 13   .or.
     &     jt (KFEET) .eq. 25 .or. jt (KFEET) .eq. 27   .or.
     &     jt (KFEET) .eq. 29 .or. jt (KFEET) .eq. 31   .or.
     &     jt (KFEET) .eq. 33 .or. jt (KFEET) .eq. 35   .or.
     &     jt (KFEET) .eq. 46 .or. jt (KFEET) .eq. 48   .or.
     &     jt (KFEET) .eq. 37 .or. jt (KFEET) .eq. 40 ) then  

      if ( nobigout .le. 0 ) then
      if ( IQDIST .eq. 6 .or. IQDIST .eq. 7 .or. IQDIST .eq. 9) then
      write(01,83) dname(jp)
      write(33,83) dname(jp)
   83 format(150('-')/'Generated shots for added diffuse LOAD: ',
     &a11/150('-'))
      else
      write(01,82) dname(jp)
      write(33,82) dname(jp)
   82 format(150('-')/'Generated shots for added diffuse stream ',
     &'quality: ',a11/150('-'))
	endif
      write(01,84) (ECshots(IS),IS=1,NS)  
      write(33,84) (ECshots(IS),IS=1,NS)
   84 format(f7.3,20F7.3)
      write(01,15)
      write(33,15)
      endif

      CM=0.0
      CS=0.0
      do IS=1,NS
      CM=CM+ECshots(IS)
      CS=CS+ECshots(IS)*ECshots(IS)
      enddo

      if (CS .gt. 1.0E-10) goto 75
      CS=0.0
      goto 76
   75 CS=(CS-CM*CM/NS)/(NS-1)
      if (CS .gt. 1.0E-20) goto 79
      CS=0.0
      goto 76
   79 CS=SQRoot(14099,CS)
   76 CM=CM/NS
      
*     write out the ordered shots ----------------------------------------------      
      if ( MONQ .ge. 99 ) then
      do i = 1, NS
      xxx (i) = ECshots(i)
      enddo
*     arrange the data in sequence ---------------------------------------------
      do i = 1,   NS-1
      do j = i+1, NS
      if ( xxx(i) .ge. xxx(j) ) then
      xtemp = xxx (j)
      xxx (j) = xxx (i)
      xxx (i) = xtemp
	endif
      enddo
      enddo
*     --------------------------------------------------------------------------
      write(01,20)DNAME(JP)
   20 format(//120('=')/'Ordered data ... ',a11/120('='))
      write(01,24)(xxx(IS),IS = 1,NS)
   24 format(15F8.4)
      write(01,95)
   95 format(120('='))
      endif
      
	if ( nobigout .le. 0 ) then
      if ( IQDIST .eq. 6 .or. IQDIST .eq. 7 .or. IQDIST .eq. 9 ) then
	write(01,7922) CM,CS
	write(33,7922) CM,CS
 7922 Format(51x,'   Calculated annual mean diffuse load =',f8.3,
     &           '               ...  Standard deviation =',f8.3)       
      write(01,15)
      write(33,15)
	else
	write(01,829) CM,CS
	write(33,829) CM,CS
  829 Format(51x,'Calculated annual mean diffuse quality =',f8.2,
     &           '               ...  Standard deviation =',f8.2)       
      write(01,15)
      write(33,15)
	endif
      endif

*     calculate statistics for non-zero values ----------------------------------
      CM=0.0
      CS=0.0
	KS=0
      do IS=1,NS
      if (ECshots(IS) .gt. 1.0e-9 ) then
      CM=CM+ECshots(IS)
      CS=CS+ECshots(IS)*ECshots(IS)
	KS=KS+1
	endif
      enddo
      if (KS .gt. 0) then
      if (KS .gt. 1) then
      if (CS .gt. 1.0E-10) goto 55
      CS=0.0
      goto 56
   55 CS=(CS-CM*CM/KS)/(KS-1)
      if (CS .gt. 1.0E-20) goto 69
      CS=0.0
      goto 56
   69 CS=SQRoot(15099,CS)
   56 CM=CM/KS
      else
	CS=0.0
      endif
      if ( KS .ne. NS ) then
      xks = 100.0 * float (KS) / float (NS)
	if ( nobigout .le. 0 ) then
      if ( IQDIST .eq. 6 .or. IQDIST .eq. 7 .or. IQDIST .eq. 9 ) then
	write(01,7522) CM, CS, xks
	write(33,7522) CM, CS, xks
 7522 Format(29x,'Calculated mean load (for ',
     &'non-zero values only) =',f8.2,6x,
     &'...  Standard deviation =',f8.2,' (',f6.2,'% of the year)')
      write(01,15)
      write(33,15)
	else
	write(01,1255) CM, CS, KS, NS
	write(33,1255) CM, CS, KS, NS
 1255 Format(41x,'Calculated mean quality (for ',
     &'non-zero DIFFUSE flows) =',f8.2,
     &       '           ...  Standard deviation =',f8.2,2i5)       
      write(01,15)
      write(33,15)
	endif
	endif
	endif
      endif
*     --------------------------------------------------------------------------

	endif  !if ( jt (KFEET) .eq. 15 etc ...
	endif  !if ( if diffuse .gt. 0 )

      return
      end





*     write out the shots for discharge quality --------------------------------
      subroutine write shots for discharge quality
      include 'COM.FOR'

      if ( qtype(JP) .eq. 4 ) return

	if ( jt (feeture) .eq.  3 .or. jt (feeture) .eq. 5   .or.
     &     jt (feeture) .eq. 15 .or. jt (feeture) .eq. 16  .or.
     &     jt (feeture) .eq. 43 .or. 
     &     jt (feeture) .eq. 39 .or. jt (feeture) .eq. 12) then

      if ( nobigout .le. 0 ) then ! ============================================
      if ( IQDIST .eq. 6 .or. IQDIST .eq. 7 .or. IQDIST .eq. 9 ) then
      write(01,13) dname(jp)
      write(31,13) dname(jp)
   13 format(150('-')/'Generated shots for added load: ',a11/150('-'))
      else
      write(01,12) dname(jp)
      write(31,12) dname(jp)
   12 format(150('-')/'Generated shots for added discharge quality: ',
     &a11/150('-'))
	endif
      write(01,14) (ECshots(IS),IS=1,NS)
      write(31,14) (ECshots(IS),IS=1,NS)
   14 format(f7.2,20F7.2)
      write(01,15)
      write(31,15)
   15 format(150('-'))
      endif ! if ( nobigout .le. 0 ) ===========================================

      CM = 0.0
      CS = 0.0
      do IS = 1, NS
      CM = CM + ECshots(IS)
      CS = CS + ECshots(IS) * ECshots(IS)
      enddo

      if ( CS .gt. 1.0E-10 ) goto 5
      CS = 0.0
      goto 6
    5 CS = (CS-CM*CM/NS)/float(NS-1)
      if ( CS .gt. 1.0E-20 ) goto 9
      CS = 0.0
      goto 6
    9 CS = SQRoot(18099,CS)
    6 CM = CM / float(NS)

	if ( nobigout .le. 0 ) then
      write(01,29) CM, CS
	if ( nobigout .le. 0 ) write(31,29) CM, CS
   29 Format(63x,'Calculated annual mean quality =',f8.2,
     &       '           ...  Standard deviation =',f8.2)       
      write(01,15)
      endif

      CM=0.0
      CS=0.0
	KS=0
      do IS=1,NS
      if (ECshots(IS) .gt. 1.0e-9 ) then
      CM=CM+ECshots(IS)
      CS=CS+ECshots(IS)*ECshots(IS)
	KS=KS+1
	endif
      enddo

      if (KS .eq. NS) return
	if (KS .eq. 0 ) return
      if (KS .gt. 1) then
      if (CS .gt. 1.0E-10) goto 55
      CS=0.0
      goto 56
   55 CS=(CS-CM*CM/KS)/float(KS-1)
      if (CS .gt. 1.0E-20) goto 69
      CS=0.0
      goto 56
   69 CS=SQRoot(19099,CS)
   56 CM=CM/float(KS)
      else
	CS=0.0
      endif

 	if ( nobigout .le. 0 ) write(01,25) CM, CS
 	if ( nobigout .le. 0 ) write(31,25) CM, CS
   25 Format(39x,'Calculated discharge mean quality (for ',
     &'non-zero flows) =',f8.2,
     &'           ...  Standard deviation =',f8.2)       
      if ( nobigout .le. 0 ) write(01,15)

	CMlast = CM
	CMwant = pollution data(IQ,JP,1)
      CMratio = CMwant / CMlast

      write(01,5399)IQ,JP,CMlast,CMwant,CMratio
      write(31,5399)IQ,JP,CMlast,CMwant,CMratio
 5399 format(2i4,3f12.4)

      CM=0.0
      CS=0.0
	KS=0
      do IS=1,NS
      if (ECshots(IS) .gt. 1.0e-9 ) then
      ECshots(IS) = ECshots(IS)*CMratio
      CM=CM+ECshots(IS)
      CS=CS+ECshots(IS)*ECshots(IS)
	KS=KS+1
	endif
      enddo

      if (KS .gt. 0) then
      if (KS .gt. 1) then
      if (CS .gt. 1.0E-10) goto 185
      CS=0.0
      goto 186
  185 CS=(CS-CM*CM/KS)/float(KS-1)
      if (CS .gt. 1.0E-20) goto 189
      CS=0.0
      goto 186
  189 CS=SQRoot(20099,CS)
  186 CM=CM/float(KS)
      else
	CS=0.0
      endif
	endif

	if ( nobigout .le. 0 ) write(01,285) CM, CS
	if ( nobigout .le. 0 ) write(31,285) CM, CS
  285 Format(39x,'Calculated discharge MEAN quality (for ',
     &'non-zero flows) =',f8.2,
     &'           ...  Standard deviation =',f8.2)       
      if ( nobigout .le. 0 ) write(01,15)

      CM=0.0
      CS=0.0
	KS=0
      do IS=1,NS
	if ( abs(ERANR3(IS)) .gt. 1.0e-9 ) then
      CM=CM+ERANR3(IS)
      CS=CS+ERANR3(IS)*ERANR3(IS)
	KS=KS+1
	endif
      enddo
      if (KS .gt. 0) then
      if (KS .gt. 1) then
      if (CS .gt. 1.0E-10) goto 85
      CS=0.0
      goto 86
   85 CS=(CS-CM*CM/KS)/(KS-1)
      if (CS .gt. 1.0E-20) goto 89
      CS=0.0
      goto 86
   89 CS=SQRoot(21099,CS)
   86 CM=CM/KS
      else
	CS=0.0
      endif
	endif

	endif

      return
      end
