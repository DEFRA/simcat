*     ==========================================================================
*     SIMCAT - Monte-Carlo simulation of water quality in a river ....
*     ==========================================================================
*     File: random numbers for Monte Carlo.for ... 1468 lines ------------------
*     --------------------------------------------------------------------------
*     This file generates random numbers for Monte Carlo Simulation ------------
*     --------------------------------------------------------------------------
*     It contains 0 subroutines and 52 functions -----------------------------------
*     --------------------------------------------------------------------------

      function RAN1(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      enddo
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation A',JJ
      call stop
      endif
      RAN1=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end

      
      
*     --------------------------------------------------------------------------
*     generate random numbers for river quality ---------------------------------
*     --------------------------------------------------------------------------
      function RAN201(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      enddo
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation B',JJ
      call stop
      endif
      RAN201=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end
      function RAN202(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      enddo
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation B',JJ
      call stop
      endif
      RAN202=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end
      function RAN203(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      enddo
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation B',JJ
      call stop
      endif
      RAN203=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end
      function RAN204(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      enddo
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation B',JJ
      call stop
      endif
      RAN204=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end
      function RAN205(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      enddo
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation B',JJ
      call stop
      endif
      RAN205=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end
      function RAN206(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      enddo
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation B',JJ
      call stop
      endif
      RAN206=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end
      function RAN207(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      enddo
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation B',JJ
      call stop
      endif
      RAN207=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end
      function RAN208(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      enddo
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation B',JJ
      call stop
      endif
      RAN208=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end
      function RAN209(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      enddo
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation B',JJ
      call stop
      endif
      RAN209=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end
      function RAN210(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      enddo
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation B',JJ
      call stop
      endif
      RAN210=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end
      

      
      
*     --------------------------------------------------------------------------
*     generate random numbers for discharge flow (old version) -----------------
*     --------------------------------------------------------------------------
      function RAN3(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      enddo
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation C',JJ
      call stop
      endif
      RAN3=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end
*     --------------------------------------------------------------------------
*     generate random numbers for discharge quality ----------------------------
*     --------------------------------------------------------------------------
      function RAN4(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do 11 J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
11    continue
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation D',JJ
      call stop
      endif
      RAN4=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end
      function RAN401(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do 11 J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
11    continue
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation D',JJ
      call stop
      endif
      RAN401=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end
      function RAN402(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do 11 J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
11    continue
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation D',JJ
      call stop
      endif
      RAN402=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end
      function RAN403(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do 11 J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
11    continue
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation D',JJ
      call stop
      endif
      RAN403=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end
      function RAN404(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do 11 J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
11    continue
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation D',JJ
      call stop
      endif
      RAN404=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end
      function RAN405(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do 11 J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
11    continue
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation D',JJ
      call stop
      endif
      RAN405=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end
      function RAN406(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do 11 J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
11    continue
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation D',JJ
      call stop
      endif
      RAN406=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end
      function RAN407(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do 11 J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
11    continue
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation D',JJ
      call stop
      endif
      RAN407=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end
      function RAN408(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do 11 J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
11    continue
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation D',JJ
      call stop
      endif
      RAN408=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end
      function RAN409(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do 11 J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
11    continue
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation D',JJ
      call stop
      endif
      RAN409=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end
      function RAN410(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do 11 J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
11    continue
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation D',JJ
      call stop
      endif
      RAN410=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end

      
*     --------------------------------------------------------------------------
*     generate random numbers for temperature (old version) --------------------
*     --------------------------------------------------------------------------
      function RAN5(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do 11 J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
11    continue
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)'Stopped in random number generation ND',JJ
      call stop
      endif
      RAN5=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end
*     --------------------------------------------------------------------------
*     random numbers for suspended solids (old version) ------------------------
*     --------------------------------------------------------------------------	
      function RAN7(IDUM)
      dimension R(97)
      parameter (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      parameter (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      parameter (M3=243000,IA3=4561,IC3=51349)
      save IX1,IX2,IX3
      if ( IDUM .lt. 0 ) then
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      do J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      enddo
      IDUM=1
      endIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      JJ=1+(97*IX3)/M3
      J = iabs (JJ)
      if (J .gt. 97 .or. J .lt. 1) then
      write( *,*)' Stopped in random number generation 7',JJ
      call stop
      endif
      RAN7=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      return
      end
      
      
      
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

      subroutine gas2dets (IDUM,idet,GAS)
      goto (1,2,3,4,5,6,7,8,9,10),idet
    1 GAS = GAS201 (IDUM)
      return
    2 GAS = GAS202 (IDUM)
      return
    3 GAS = GAS203 (IDUM)
      return
    4 GAS = GAS204 (IDUM)
      return
    5 GAS = GAS205 (IDUM)
      return
    6 GAS = GAS206 (IDUM)
      return
    7 GAS = GAS207 (IDUM)
      return
    8 GAS = GAS208 (IDUM)
      return
    9 GAS = GAS209 (IDUM)
      return
   10 GAS = GAS210 (IDUM)
      return
      end


*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for river quality -------
*     --------------------------------------------------------------------------
      function GAS201(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET = 0
      if ( ISET .eq. 0 ) then
    1 continue 
      V1=2.*RAN201(IDUM)-1.
      V2=2.*RAN201(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1010,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS201=V2*FAC
      ISET=1
      else
      GAS201=GSET
      ISET=0
      endIF
      return
      end
*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for river quality -------
*     --------------------------------------------------------------------------
      function GAS202(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET = 0
      if ( ISET .eq. 0 ) then
    1 continue 
      V1=2.*RAN202(IDUM)-1.
      V2=2.*RAN202(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1010,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS202=V2*FAC
      ISET=1
      else
      GAS202=GSET
      ISET=0
      endIF
      return
      end
*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for river quality -------
*     --------------------------------------------------------------------------
      function GAS203(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET = 0
      if ( ISET .eq. 0 ) then
    1 continue 
      V1=2.*RAN203(IDUM)-1.
      V2=2.*RAN203(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1010,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS203=V2*FAC
      ISET=1
      else
      GAS203=GSET
      ISET=0
      endIF
      return
      end
*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for river quality -------
*     --------------------------------------------------------------------------
      function GAS204(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET = 0
      if ( ISET .eq. 0 ) then
    1 continue 
      V1=2.*RAN204(IDUM)-1.
      V2=2.*RAN204(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1010,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS204=V2*FAC
      ISET=1
      else
      GAS204=GSET
      ISET=0
      endIF
      return
      end
*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for river quality -------
*     --------------------------------------------------------------------------
      function GAS205(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET = 0
      if ( ISET .eq. 0 ) then
    1 continue 
      V1=2.*RAN205(IDUM)-1.
      V2=2.*RAN205(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1010,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS205=V2*FAC
      ISET=1
      else
      GAS205=GSET
      ISET=0
      endIF
      return
      end
*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for river quality -------
*     --------------------------------------------------------------------------
      function GAS206(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET = 0
      if ( ISET .eq. 0 ) then
    1 continue 
      V1=2.*RAN206(IDUM)-1.
      V2=2.*RAN206(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1010,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS206=V2*FAC
      ISET=1
      else
      GAS206=GSET
      ISET=0
      endIF
      return
      end
*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for river quality -------
*     --------------------------------------------------------------------------
      function GAS207(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET = 0
      if ( ISET .eq. 0 ) then
    1 continue 
      V1=2.*RAN207(IDUM)-1.
      V2=2.*RAN207(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1010,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS207=V2*FAC
      ISET=1
      else
      GAS207=GSET
      ISET=0
      endIF
      return
      end
*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for river quality -------
*     --------------------------------------------------------------------------
      function GAS208(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET = 0
      if ( ISET .eq. 0 ) then
    1 continue 
      V1=2.*RAN208(IDUM)-1.
      V2=2.*RAN208(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1010,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS208=V2*FAC
      ISET=1
      else
      GAS208=GSET
      ISET=0
      endIF
      return
      end
*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for river quality -------
*     --------------------------------------------------------------------------
      function GAS209(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET = 0
      if ( ISET .eq. 0 ) then
    1 continue 
      V1=2.*RAN209(IDUM)-1.
      V2=2.*RAN209(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1010,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS209=V2*FAC
      ISET=1
      else
      GAS209=GSET
      ISET=0
      endIF
      return
      end
*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for river quality -------
*     --------------------------------------------------------------------------
      function GAS210(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET = 0
      if ( ISET .eq. 0 ) then
    1 continue 
      V1=2.*RAN210(IDUM)-1.
      V2=2.*RAN210(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1010,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS210=V2*FAC
      ISET=1
      else
      GAS210=GSET
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

      
      subroutine gas4dets (IDUM,idet,GAS)
      goto (1,2,3,4,5,6,7,8,9,10),idet
    1 GAS = GAS401 (IDUM)
      return
    2 GAS = GAS402 (IDUM)
      return
    3 GAS = GAS403 (IDUM)
      return
    4 GAS = GAS404 (IDUM)
      return
    5 GAS = GAS405 (IDUM)
      return
    6 GAS = GAS406 (IDUM)
      return
    7 GAS = GAS407 (IDUM)
      return
    8 GAS = GAS408 (IDUM)
      return
    9 GAS = GAS409 (IDUM)
      return
   10 GAS = GAS410 (IDUM)
      return
      end


*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for discharge quality ---
*     --------------------------------------------------------------------------
      function GAS4444(IDUM)
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
      GAS4444=V2*FAC
      ISET=1
      else
      GAS4444=GSET
      ISET=0
      endIF
      return
      end
*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for discharge quality ---
*     --------------------------------------------------------------------------
      function GAS401(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET=0
      if ( ISET .eq. 0 ) then
    1 continue     
      V1=2.*RAN401(IDUM)-1.
      V2=2.*RAN401(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1013,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS401=V2*FAC
      ISET=1
      else
      GAS401=GSET
      ISET=0
      endIF
      return
      end
*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for discharge quality ---
*     --------------------------------------------------------------------------
      function GAS402(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET=0
      if ( ISET .eq. 0 ) then
    1 continue     
      V1=2.*RAN402(IDUM)-1.
      V2=2.*RAN402(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1013,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS402=V2*FAC
      ISET=1
      else
      GAS402=GSET
      ISET=0
      endIF
      return
      end
*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for discharge quality ---
*     --------------------------------------------------------------------------
      function GAS403(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET=0
      if ( ISET .eq. 0 ) then
    1 continue     
      V1=2.*RAN403(IDUM)-1.
      V2=2.*RAN403(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1013,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS403=V2*FAC
      ISET=1
      else
      GAS403=GSET
      ISET=0
      endIF
      return
      end
*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for discharge quality ---
*     --------------------------------------------------------------------------
      function GAS404(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET=0
      if ( ISET .eq. 0 ) then
    1 continue     
      V1=2.*RAN404(IDUM)-1.
      V2=2.*RAN404(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1013,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS404=V2*FAC
      ISET=1
      else
      GAS404=GSET
      ISET=0
      endIF
      return
      end
*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for discharge quality ---
*     --------------------------------------------------------------------------
      function GAS405(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET=0
      if ( ISET .eq. 0 ) then
    1 continue     
      V1=2.*RAN405(IDUM)-1.
      V2=2.*RAN405(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1013,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS405=V2*FAC
      ISET=1
      else
      GAS405=GSET
      ISET=0
      endIF
      return
      end
*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for discharge quality ---
*     --------------------------------------------------------------------------
      function GAS406(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET=0
      if ( ISET .eq. 0 ) then
    1 continue     
      V1=2.*RAN406(IDUM)-1.
      V2=2.*RAN406(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1013,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS406=V2*FAC
      ISET=1
      else
      GAS406=GSET
      ISET=0
      endIF
      return
      end
*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for discharge quality ---
*     --------------------------------------------------------------------------
      function GAS407(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET=0
      if ( ISET .eq. 0 ) then
    1 continue     
      V1=2.*RAN407(IDUM)-1.
      V2=2.*RAN407(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1013,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS407=V2*FAC
      ISET=1
      else
      GAS407=GSET
      ISET=0
      endIF
      return
      end
*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for discharge quality ---
*     --------------------------------------------------------------------------
      function GAS408(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET=0
      if ( ISET .eq. 0 ) then
    1 continue     
      V1=2.*RAN408(IDUM)-1.
      V2=2.*RAN408(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1013,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS408=V2*FAC
      ISET=1
      else
      GAS408=GSET
      ISET=0
      endIF
      return
      end
*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for discharge quality ---
*     --------------------------------------------------------------------------
      function GAS409(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET=0
      if ( ISET .eq. 0 ) then
    1 continue     
      V1=2.*RAN409(IDUM)-1.
      V2=2.*RAN409(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1013,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS409=V2*FAC
      ISET=1
      else
      GAS409=GSET
      ISET=0
      endIF
      return
      end
*     --------------------------------------------------------------------------
*     generate random samples from Normal Distribution for discharge quality ---
*     --------------------------------------------------------------------------
      function GAS410(IDUM)
      save iset,gset
      data ISET /0/
      if ( IDUM .lt. 0 ) ISET=0
      if ( ISET .eq. 0 ) then
    1 continue     
      V1=2.*RAN410(IDUM)-1.
      V2=2.*RAN410(IDUM)-1.
      R=V1**2+V2**2
      if (R .ge. 1.) goto 1
      FAC=SQRoot(1013,-2.*LOG(R)/R)
      GSET=V1*FAC
      GAS410=V2*FAC
      ISET=1
      else
      GAS410=GSET
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