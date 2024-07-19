		MODULE FFT_COM
		
		IMPLICIT NONE
		
        INTEGER MFFT,MFFT2
		!INTEGER,PARAMETER :: FFTNUM=64
		!INTEGER,PARAMETER :: FFTNUM=128
		INTEGER,PARAMETER :: FFTNUM=256
!		INTEGER,PARAMETER :: FFTNUM=512
		!INTEGER,PARAMETER :: FFTNUM=1024                                                  !uemura
		!INTEGER,PARAMETER :: FFTNUM=1                                                              !ariyama
		!INTEGER,PARAMETER :: FFTNUM=16
		!INTEGER,PARAMETER :: FFTNUM=183
		INTEGER,PARAMETER :: IRR=0
		!DOUBLE PRECISION,PARAMETER :: DX=0.1d0,DY=DX,DZ=DX
		!DOUBLE PRECISION,PARAMETER :: DX=3.23d-3,DY=DX,DZ=DX                                         !uemura
		
		!DOUBLE PRECISION,PARAMETER :: DX=3.8d-4,DY=DX,DZ=DX
		!DOUBLE PRECISION,PARAMETER :: DX=2.0d-4,DY=DX,DZ=DX                      !ariyamaM1
!		DOUBLE PRECISION,PARAMETER :: DX=1.0d-3,DY=DX,DZ=DX
        DOUBLE PRECISION,PARAMETER :: DX=5.0d-4,DY=DX,DZ=DX
        !COMPLEX V00(0:1024*4*4)                                                                      !uemura      !timestep増やしてうまくいかないときはこれを増やす
		COMPLEX V00(0:2048*4*4) 
		!COMPLEX V00(0:512*4*4)                                                                     !ariyama
		!COMPLEX V00(0:256*4*4) 
		!COMPLEX V00(0:1*4*4)
		!COMPLEX V00(0:8*4*4)
		!COMPLEX V00(0:16*4*4)
		!COMPLEX V00(0:12*4*4)
		!COMPLEX FFT1DATA(0:FFTNUM,0:50000)
		!COMPLEX FFT1DATA(0:FFTNUM,0:80000)
		COMPLEX FFT1DATA(0:FFTNUM,0:30000)                           !aaa
		!COMPLEX FFT1DATA(0:FFTNUM,0:15000)
        COMPLEX,PARAMETER::IJ=(0.0,1.0)
		INTEGER DATANUM,FFT2DATA
		!CHARACTER*2 WAVECHA
		!CHARACTER*3 WAVECHA
		CHARACTER*4 WAVECHA
		!CHARACTER*3 WAVECHA2
		!CHARACTER*4 WAVECHA2
		CHARACTER*5 WAVECHA2
		CHARACTER*64 FILENAME,FFTFILE,FFTFILE2
		!CHARACTER*6 GPLFILE
		!CHARACTER*7 GPLFILE
		CHARACTER*8 GPLFILE
        DOUBLE PRECISION,PARAMETER ::PI=3.14159265359
		DOUBLE PRECISION,PARAMETER ::FC=2.99792458d8
		INTEGER A00,I,J,M,N,AT,AAA,AAA2
		double precision DT,F,DF,DT2,F2,DF2
		END MODULE


!MFFT 8e9
USE FFT_COM
     DO M=0,FFTNUM-1
		V00(:)=0
		!WRITE(WAVECHA,"(I2.2)")M
		!WRITE(WAVECHA,"(I3.3)")M
		WRITE(WAVECHA,"(I4.4)")M
		!FILENAME=WAVECHA//"/BEATEZ"                       !IFsignal
		FILENAME=WAVECHA//"/ezR"                       !ezr
		WRITE(*,*)FILENAME
		AAA=M+20

	  	OPEN(AAA,FILE=FILENAME)
		DATANUM=0
		DO
		   READ(AAA,*,END=100)A00,V00(DATANUM)
		   DATANUM=DATANUM+1
		END DO

100     CLOSE(AAA)
		
		IF(M.EQ.0)THEN
		   MFFT=1
		   DO
			  MFFT=MFFT*2                                                          !MFFT(1024+1)*2=2050:1-FFTのstep数       uemura
			  
			  !MFFT=MFFT*16                                          !ariyama
			  
		      IF(MFFT.GE.DATANUM) exit
		   END DO
		END IF
		
		!DT=0.99d0/(dSQRT((1.0d0/DX)**2d0+(1.0d0/DY)**2d0+(1.0d0/DZ)**2d0)*FC)!*6.0                !syoki
		
		
		dt=0.99999/(fc*sqrt(1.0/(dx*dx)+1.0/(dy*dy)+1.0/(dz*dz)))                     !ariyama
		
		
		
		!DT=0.99d0/(dSQRT((1.0d0/DX)**2d0+(1.0d0/DY)**2d0+(1.0d0/DZ)**2d0)*FC)*10                 !サンプリングレート
		
		DF=1./(MFFT*DT)                                                                       !サンプリング周波数/FFTサイズ     syoki
		
		!DF=1./(3500*DT)                                        !ariyama 3500はデータ数
		
		CALL FFT1

	END DO
!-------一回目のFFT終了--------

		MFFT2=FFTNUM                             !1024
		FFT2DATA=AT
		AAA2=300
		!DT2=0.25d0
		!DT2=0.5d0
		!DT2=0.1d0
		!DT2=0.001d0
		!DT2=0.0005d0                                                                       !uemura syoki          -1000~1000
		!DT2=0.005d0    
		!DT2=0.00005d0                                   !ariyama_-10000~10000
		!DT2=0.000125d0                                                                       !
!		DT2=0.0001d0                                         !27.9GHz -5000~1000
		DT2=0.00025d0
		
		DF2=1./(MFFT2*DT2)
	    OPEN(2002,FILE="2-FFT/FFT.gpl")                                           !uemura
		!OPEN(2002,FILE="2-FFT/FFD.gpl")                                            !ariyama    FMCW hako
		!OPEN(2002,FILE="2-FFT/FFE.gpl")   
		!OPEN(2002,FILE="2-FFT/FFBB.gpl")
		!OPEN(2002,FILE="2-FFT/FFEE.gpl")                                         !74GHz FMCW  
		!OPEN(2002,FILE="2-FFT/FFFF.gpl")
		!OPEN(2002,FILE="2-FFT/FFG.gpl")
		!OPEN(2002,FILE="2-FFT/FFH.gpl")
		!OPEN(2002,FILE="2-FFT/FFI.gpl")                                        !8.4
		!OPEN(2002,FILE="2-FFT/FFJ.gpl")                                       !8.23               
		!OPEN(2002,FILE="2-FFT/FFL.gpl")     
		
		WRITE(2002,*)"set xrange [*:*]"

		!平滑化
		!write(2002,*)"nearint(x)=(x-floor(x)<=0.5 ? floor(x):floor(x)+1)"
		!write(2002,*)"filter(x,y)=nearint(x/y)*y"

		!WRITE(2002,*)"#set xtics 0.1"
		
		DO I=0,FFT2DATA
		WRITE(*,*)I
		CALL FFT2
	   
		END DO
		END

!-----------------------------------------------------------------------
      SUBROUTINE FFT1
!-----------------------------------------------------------------------
    USE FFT_COM
	
	COMPLEX DATA1(0:MFFT)
	
	DATA1(:)=0
	
	!窓関数
	DO I=0,MFFT               !2050
	   DATA1(I)=V00(I)*(0.54d0-0.46d0*dcos(2*PI*I/MFFT))       !データに窓関数をかける
	   IF(M.EQ.0)THEN
	      WRITE(132,*)I,(0.54d0-0.46d0*dcos(2*PI*I/MFFT))
	   END IF
	END DO

	CALL CEFFT(MFFT,DATA1,1,IRR)

		FFTFILE="1-FFT/FFT"//WAVECHA
		!FFTFILE="1-FFT/FFFF"//WAVECHA
		!FFTFILE="1-FFT/FFG"//WAVECHA
		!FFTFILE="1-FFT/FFH"//WAVECHA
		!FFTFILE="1-FFT/FFI"//WAVECHA
		!FFTFILE="1-FFT/FFJ"//WAVECHA
		
		WRITE(*,*)FFTFILE
		OPEN(AAA+FFTNUM+1,FILE=FFTFILE)
		
		F=0
		AT=0
		DO N=0,MFFT/2                          !uemura
		
		!DO N=0,MFFT                                       !ariyama	
		
!		   IF(F.LE.1.20e7)THEN
		      FFT1DATA(M,AT)=DATA1(AT)
!			    IF(F.LE.7.1e6)THEN
!				   FFT1DATA(M,AT)=0
!				END IF
			  !WRITE(AAA+FFTNUM+1,*)F,REAL(FFT1DATA(M,N)),AIMAG(FFT1DATA(M,N))   !1回目のFFTの実数部,虚数部                      uemura syoki
			  WRITE(AAA+FFTNUM+1,*)F,REAL(FFT1DATA(M,N)),AIMAG(FFT1DATA(M,N)),sqrt(REAL(FFT1DATA(M,N))**2+AIMAG(FFT1DATA(M,N))**2)                             !ariyama                          
			  F=F+DF
		      AT=AT+1
!		   END IF
		END DO
		CLOSE(AAA+FFTNUM+1)
	
	RETURN
	END SUBROUTINE

!-----------------------------------------------------------------------
      SUBROUTINE FFT2
!-----------------------------------------------------------------------
      USE FFT_COM
	  
	  COMPLEX FFT1SORT(0:MFFT2)
	  
	  FFT1SORT(:)=0
		AAA2=AAA2+I
	  !WRITE(WAVECHA2,"(I3.3)")I
	  !WRITE(WAVECHA2,"(I4.4)")I
	  WRITE(WAVECHA2,"(I5.5)")I
	  
	  FFTFILE2="2-FFT/FFT"//WAVECHA2                                                              !uemura
	  !FFTFILE2="2-FFT/FFA"//WAVECHA2                                                                                       !ariyama  antena tooi
	  !FFTFILE2="2-FFT/FFB"//WAVECHA2                                                                !ariyama antena 3cel
	  !FFTFILE2="2-FFT/FFD"//WAVECHA2
	  !FFTFILE2="2-FFT/FFE"//WAVECHA2
	  !FFTFILE2="2-FFT/FFBB"//WAVECHA2
	  !FFTFILE2="2-FFT/FFEE"//WAVECHA2
	  !FFTFILE2="2-FFT/FFFF"//WAVECHA2                                                             !79GHz 4GHz souin
	   !FFTFILE2="2-FFT/FFG"//WAVECHA2            
	  !FFTFILE2="2-FFT/FFH"//WAVECHA2 
	  !FFTFILE2="2-FFT/FFH"//WAVECHA2
	  !FFTFILE2="2-FFT/FFI"//WAVECHA2
	  !FFTFILE2="2-FFT/FFJ"//WAVECHA2
	  !FFTFILE2="2-FFT/FFL"//WAVECHA2
	  
	  GPLFILE="FFT"//WAVECHA2                                                               !uemura
	  !GPLFILE="FFD"//WAVECHA2                                                                       !ariyama
	  !GPLFILE="FFE"//WAVECHA2                                                                   !ariyama
	  !GPLFILE="FFBB"//WAVECHA2
      !GPLFILE="FFEE"//WAVECHA2
	  !GPLFILE="FFFF"//WAVECHA2
	   !GPLFILE="FFH"//WAVECHA2  
	   !GPLFILE="FFI"//WAVECHA2 
	   !GPLFILE="FFJ"//WAVECHA2
	  ! GPLFILE="FFL"//WAVECHA2  
	   


	!平滑化
	  !write(2002,*)'plot "',GPLFILE,'"u(filter($1,5.0)):2 smooth unique'


      write(2002,*) 'plot "',GPLFILE,'" with lines'

      write(2002,*) "pause 0.5"
!--------データの並び替え----------------------
		   DO M=0,MFFT2-1
		      FFT1SORT(M)=FFT1DATA(M,I)
			  WRITE(12122,*)M,I,REAL(FFT1SORT(M)),AIMAG(FFT1DATA(M,I))
		   END DO

!--------二回目FFT------------------------	
		   CALL CEFFT(MFFT2,FFT1SORT,1,IRR)
		   OPEN(AAA2,FILE=FFTFILE2)

		   !周波数を反転して出力する
		   F2=-DF2*(MFFT2/2)
			DO N=MFFT2/2,0,-1	!降順ループ
           	  WRITE(AAA2,*)F2,sqrt(REAL((FFT1SORT(N))*CONJG(FFT1SORT(N))))                                   !N:t=0000~1024までの距離FFTデータの実部と複素共役
			  !WRITE(AAA2,*)F2,10*log10(sqrt(REAL((FFT1SORT(N))*CONJG(FFT1SORT(N)))))
              F2=DF2+F2
			END DO

           F2=DF2
		   DO N=MFFT2/2-1,1,-1	!降順ループ
		      WRITE(AAA2,*)F2,sqrt(REAL((FFT1SORT(MFFT2/2+N))*CONJG(FFT1SORT(MFFT2/2+N))))
			  !WRITE(AAA2,*)F2,10*log10(sqrt(REAL((FFT1SORT(MFFT2/2+N))*CONJG(FFT1SORT(MFFT2/2+N)))))
		      F2=DF2+F2
		   END DO
		   
		  ! F2=-DF2*(MFFT2/2.-1)
		  ! DO N=1,MFFT2/2-1
		  !    WRITE(AAA2,*)F2,sqrt(REAL((FFT1SORT(MFFT2/2+N))*CONJG(FFT1SORT(MFFT2/2+N))))
		  !    F2=DF2+F2
		  ! END DO
		   
          ! F2=0
		  ! DO N=0,MFFT2/2
          ! 	  WRITE(AAA2,*)F2,sqrt(REAL((FFT1SORT(N))*CONJG(FFT1SORT(N))))
          !    F2=DF2+F2
          ! END DO
           CLOSE(AAA2)
	RETURN
	END SUBROUTINE
!-----------------------------------------------------------------------
      SUBROUTINE CEFFT(N,CX,ICON,IRR)
!-----------------------------------------------------------------------
!高速フーリエ変換
      IMPLICIT REAL(A-B,D-H,O-Z)
      IMPLICIT COMPLEX(C)
	  INTEGER(kind=8) N,JL,JL2,JJ,J1,J2,JLB,JB2,JB4,JJB,JF1,JF2
      DIMENSION CX(N) 
      DATA PAI/3.1415926535/
!
      IF(N.LT.2) GO TO 900
      FN=N
      M=NINT(LOG10(FN)/LOG10(2.0))
      II=2**M-N
	  
      IF(II.NE.0) GO TO 910
!
      DO 50 L=1,M
      KL=2**(L-1)
      FKL=KL
      FJL=0.5*FN/FKL
      JL=FJL
      JL2=2*JL
      TH=2.0*PAI*FKL/FN
      TH2=0.5*TH
      ST=SIN(TH)
      TT=-2.0*SIN(TH2)*SIN(TH2)
      CT=CMPLX(TT,ST)
!
      DO 40 K=1,KL
      JJ=JL2*(K-1)
      CU=(1.0,0.0)
!
      DO 30 J=1,JL
      J1=J+JJ
      J2=J1+JL
      CA=CX(J1)-CX(J2)
      CX(J1)=CX(J1)+CX(J2)
!
      IF(ICON.LT.0) GO TO 10
!
      CUC=CONJG(CU)
      CX(J2)=CA*CUC
      GO TO 20
!
   10 CONTINUE
      CX(J2)=CA*CU
   20 CONTINUE
      CU=CU+CU*CT
   30 CONTINUE
   40 CONTINUE
   50 CONTINUE
!
!
!    ビット変換
!     ------------
      FM=M
      FFM=0.5*FM
      LLB=FFM
!
      DO 80 LB=1,LLB
      KLB=2**(LB-1)
      FKLB=KLB
      FJLB=0.25*FN/FKLB
      JLB=FJLB
      JB2=2*JLB
      JB4=4*JLB
!
      DO 70 KB=1,KLB
      JJB=JB4*(KB-1)
      JF1=JJB+KLB
      JF2=JJB+JB2
!
      DO 60 JB=1,JLB
      FF=JB-1
      FF=FF/FKLB
      JFF=FF
      JF=JB+JFF*KLB
      J1=JF+JF1
      J2=JF+JF2
      CT=CX(J1)
      CX(J1)=CX(J2)
      CX(J2)=CT
   60 CONTINUE
   70 CONTINUE
   80 CONTINUE
!
!     ERROR CONDITION CODE
!     --------------------
      IRR=0
      RETURN
  900 CONTINUE
      IRR=-1
      RETURN
  910 CONTINUE
      IRR=-2
      RETURN
      END
!
