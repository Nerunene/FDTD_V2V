!***************************************************************************
!        3D FDTD COM
!***************************************************************************
      MODULE FDTD_COM
! *** ANALYSIS FIELD ***
!
!      PARAMETER (NX=732,NY=412,NZ=92)                                                                          !nxとセルの対応関係:nx>dx*解析領域セル数
      !PARAMETER (NX=181,NY=181,NZ=181)
!      PARAMETER (NX=51,NY=51,NZ=51)
	  !PARAMETER (NX=((3158+1)*5),NZ=732)	!EPX2-6                                                   !採用
	 !PARAMETER (NX=((2000)*5),NZ=732)	!EPX2-6
	  !PARAMETER (NX=((416+1)*5),NZ=96+1)	!EPX2-6       !syoki
      !PARAMETER (NX=316*5,NZ=(3158+1)*5)
	 !PARAMETER (NX=(3158+1)*30),NZ=96+1)	!EPX2-6
	 !PARAMETER (NX=(3158+1)*5,NY=412,NZ=412)
	 !PARAMETER (NX=(800)*5,NY=412,NZ=412)                                                       !8.4
	  !PARAMETER (NX=(500)*5,NY=412,NZ=(500)*5)          !dekasugi 
	  PARAMETER (NX=(300)*5,NY=512,NZ=(300)*5) 
	  !PARAMETER (NX=((100+1)*5),NZ=96+1)	!EPX2-6
!	  PARAMETER (disX=1.17d2)
!
      PARAMETER (PI=3.1415926535)
	  
	 double precision :: rnd
     integer :: seedsize, w
     integer, allocatable :: seed(:)	  
	  
!
! *** CELL SIZE ***
!
      !PARAMETER (DX=1.0d-1,DY=1.0d-1,DZ=1.0d-1)
      !PARAMETER (DX=3.23d-3,DY=DX,DZ=DX)                                          !uemura3.23d-3      2.15d-3
	  !PARAMETER (DX=1.0e-4,DY=DX,DZ=DX)
	  !PARAMETER (DX=1.0e-3,DY=DX,DZ=DX)                 !27.9GHz
	  PARAMETER (DX=5.0d-4,DY=DX,DZ=DX)    
!
      !PARAMETER (NY=24*4+1)!nint(((disX+1*(DX*2)+(NX/5)*DX)*tan(7.5d0/180d0*PI)/DY)*2)+1-2)
	  !PARAMETER (NY=16+1)!nint(((disX+1*(DX*2)+(NX/5)*DX)*tan(7.5d0/180d0*PI)/DY)*2)+1-2)
	  !PARAMETER (NY=26+1)!nint(((disX+1*(DX*2)+(NX/5)*DX)*tan(7.5d0/180d0*PI)/DY)*2)+1-2)
	  !PARAMETER (NY=732)                                                                                    !採用
      PARAMETER (FC=2.99792458E+8)
! *** PARAMETER ***
	  !U^(n+1),U^(n),U^(n-1)
	  REAL ROAD(NX,NY),ROADN1(NX,NY),ROADN2(NX,NY)
	  !流速？
	  !PARAMETER(mitiv=0.0)
	  !PARAMETER (mitiv=3.0)
	  real,PARAMETER::mitiv=8.333333!2.52E+00)                          uemura
	  !real,PARAMETER::mitiv=16.666666
	  !real,PARAMETER::mitiv=5.55555
	  !real,PARAMETER::mitiv=-2.777777                                   !ariyama  10km/h
	   
!      real,PARAMETER::mitiv=30!2.52E+00)                              ariyama1

	  !PARAMETER (mitiv=12.5)
	  !PARAMETER (mitiv=16.666666)
	  !PARAMETER (mitiv=21)
	  !PARAMETER (mitiv=25.000)
	  !PARAMETER (mitiv=28.0)

	  !振幅？
!	  PARAMETER (AMP=4.0d-2)                                                  !小領域sin路面振幅
	  !PARAMETER (AMP=1.0d-1)                                                  !uemura                     !27.9GHz
	  !PARAMETER (AMP=2.0d-1)                                                   !ariyama     kihon
!	  PARAMETER (AMP=4.0d-2)                                                  !小領域random路面振幅
!	  PARAMETER (AMP=2.0d-2)                                                   !ariyama                         !振幅
	  !PARAMETER (AMP=3.0d-2)                                                      !ariyama
	  PARAMETER (AMP=2.0d-3)                                                      !ariyama
	  !PARAMETER (AMP=1.0d-3)
	  !PARAMETER (AMP=0.005)                                      !1.0d-4
	  !PARAMETER (AMP=0.0) 
	  !PARAMETER (AMP=3.0/4.0)
	  !電波の周波数、波長
      !PARAMETER (HIKARIfreq=4.64d9,HIKARIRAMUDA=FC/HIKARIfreq)
	  !PARAMETER (HIKARIfreq=79d9,HIKARIRAMUDA=FC/HIKARIfreq)
	  PARAMETER (HIKARIfreq=27.9d9,HIKARIRAMUDA=FC/HIKARIfreq)
	  !地面の波長
	  !PARAMETER (ROADRAMUDA=HIKARIRAMUDA/2.0)                         !周期小
	  PARAMETER (ROADRAMUDA=HIKARIRAMUDA)                              !uemura初期                        !27.9GHz_山の数1/2
	  !PARAMETER (ROADRAMUDA=HIKARIRAMUDA/3.0)
	  !PARAMETER (ROADRAMUDA=HIKARIRAMUDA/4)
!	  PARAMETER (ROADRAMUDA=HIKARIRAMUDA*2)                           !周期大           !8.24距離測定、9.22         !27.9GHz_基本
	  !PARAMETER (ROADRAMUDA=HIKARIRAMUDA*2)
	  !PARAMETER (ROADRAMUDA=HIKARIRAMUDA*20)
	  !PARAMETER (ROADRAMUDA=0.05)

	  !地面の位相速度
	  !PARAMETER (ROADv=SQRT(9.80*ROADRAMUDA/(2.0*PI)))
	  PARAMETER (ROADv=0)
	  !
	  PARAMETER (ROADFREQ=roadv/ROADRAMUDA)
	  REAL DT
	  INTEGER I,J,N

	  END MODULE

!-----------------------------------------------------------------------
      USE FDTD_COM
	  IMPLICIT NONE
!

  	  INTEGER WAL,AA
	  WRITE(*,*)"NY=",NY
      CALL MAKEMITI
	  
	  CALL ROADSHOKI
	  !DT=0.5E-1/2.0
	  !DT=0.5E-1
	  !DT=0.1E-1
	  !DT=0.001E-1
	  !DT=0.0005E-1                           !syoki
	  !DT=(0.0005d0)/2.0                                     !kyorisokudo_saiyou
!	  DT=(0.0000125d0)/2.0
!	  DT=(0.00001d0)/2.0                                        !27.9GHz saiyou(-5000~5000)
	  DT=(0.000025d0)/2.0                                        !小領域sin路面
	  !DT=0.005d0
	
	  
	  WRITE(*,*)DT,DX
	  print *,"jimen sokudo = ",mitiv
	  print *,"jimen sinpuku = ",AMP
	  print *,"jimen hatyou = ",ROADRAMUDA
	  print *,"jimen syuhasu = ",ROADFREQ


	  AA=0
	  WAL=10
	  !DO N=0,WAL*64
		!DO N=0,WAL*128
		DO N=0,WAL*256
!		DO N=0,WAL*512
!	    DO N=0,WAL*1024
		!DO N=0,WAL*16
		!N=0
		
			
	     IF(MOD(N,WAL).EQ.0)THEN
	       CALL OUTPUT(AA)
		   AA=AA+1
		   !OPEN(123,FILE="../roadFFT/fort.123")
		   !WRITE(123,*)real(ROADN2(2,10)/DZ)
		 END IF
	     !CALL MOVEROAD
	  END DO
	  CLOSE(123)
	  END
!
!-----------------------------------------------------------------------
      subroutine OUTPUT(ASA)
!-----------------------------------------------------------------------
      USE FDTD_COM
	  IMPLICIT NONE
	  !CHARACTER*7 NAMEXZ
	  CHARACTER*8 NAMEXZ
	  CHARACTER*1 a,b
	  INTEGER ASA
		WRITE(NAMEXZ,300)ASA
 !300  FORMAT(4Hrod-,I2.2)
 !300  FORMAT(4Hrod-,I3.3)
 300  FORMAT(4Hrod-,I4.4)
      a=""""
      b=","
	  IF(ASA.EQ.0)THEN
	  WRITE(*,*)ASA
      OPEN(UNIT=1,FILE="anime2.gpl")
 	  !WRITE(1,*) "set xrange [400:800]"
 	  !WRITE(1,*) "set yrange [1:27]"
 	  WRITE(1,*) "set zrange [-0.02:0.02]"
	  !WRITE(1,*) "set zrange [-0.005:0.0-05]"
 	  !WRITE(1,*) "set cbrange [-0.005:0.005]"                                                         !AMPに合わせて変更すべき
	  WRITE(1,*) "set cbrange [-0.02:0.02]"                                                         !ariyama
 	  WRITE(1,*) "set pm3d"
 	  WRITE(1,*)"set palette defined(-1.0",a,"#FFFFFF",a,b,"-0.5",a,&
     &"#D9D9D9",a,b,"0.5",a,"#969696",a,b,"1.0",a,"#525252",a,")"
	  WRITE(1,*) "set term gif animate"
	  WRITE(1,*) "set output ",a,"aroad.gif",a
	  END IF
      WRITE(1,*) "splot",a,NAMEXZ,a," with pm3d"
      WRITE(1,*) "pause 0.1"
	  
	  OPEN(86,FILE=NAMEXZ)

	  DO I=NX/5,NX/5*2
	     DO J=1,NY
		 !WRITE(86,*)I,J,ROADN2(I-int(N*DT*mitiv/DX),J)
		 WRITE(86,*)I,J,ROADN2(I+int(N*DT*mitiv/DX),J)
	  END DO
	     WRITE(86,*)
	  END DO
	  
	  
	  RETURN
	  END
!-----------------------------------------------------------------------
      subroutine ROADSHOKI
!-----------------------------------------------------------------------
       USE FDTD_COM
	   IMPLICIT NONE

	   DO I=2,NX-1
	      DO J=1,NY
			!波動方程式の差分近似
			ROADN1(I,J)=ROADN2(I,J)+0.*DT+(roadv**2*DT**2)/(2.*DX**2)&
			&*(ROADN2(I-1,J)+ROADN2(I+1,J)+&
			&-2.*ROADN2(I,J))                            
	   	  END DO
	   END DO
	   DO J=1,NY
		ROADN1(1,J)=0
		ROADN1(NX,J)=ROADN1(NX-1,J)
	   END DO
	   
	   RETURN
	   END
!-----------------------------------------------------------------------
      subroutine MOVEROAD
!-----------------------------------------------------------------------
       USE FDTD_COM
	   IMPLICIT NONE
       
	   DO I=2,NX-1
	     DO J=1,NY
			ROAD(I,J)=roadv**2*(DT**2/DX**2)*(ROADN1(I+1,J)+ROADN1(I-1,J)&
			&-2.*roadN1(I,J))&
			&-roadN2(I,J)+2.*roadN1(I,J)
	     END DO
	   END DO
	   
	   DO J=1,NY
		ROAD(1,J)=0
		ROAD(NX,J)=ROAD(NX-1,J)
	   END DO

	   DO I=1,NX
	     DO J=1,NY
	      roadN2(I,J)=roadN1(I,J)
	      roadN1(I,J)=road(I,J)
	   	 END DO
	   END DO
	   
	   RETURN
	   END
!-----------------------------------------------------------------------
      subroutine MAKEMITI
!-----------------------------------------------------------------------
      USE FDTD_COM
	  
	  REAL RFxn(1:NX)
	  WRITE(*,*)'dx = ',DX


    call random_seed(size=seedsize) !初期値のサイズを取得
    allocate(seed(seedsize)) !配列の割り当て
    do w = 1, seedsize
    call system_clock(count=seed(w)) !時間を取得
     end do
     call random_seed(put=seed(:)) !初期値を与える







	    DO I=1,NX,10
		call random_number(rnd)               !itumo
		
		RFxn(I)=AMP*(rnd)
		RFxn(I+1)=AMP*(rnd)
		RFxn(I+2)=AMP*(rnd)
		RFxn(I+3)=AMP*(rnd)
		RFxn(I+4)=AMP*(rnd)
		RFxn(I+5)=AMP*(rnd)
		RFxn(I+6)=AMP*(rnd)
		RFxn(I+7)=AMP*(rnd)
		RFxn(I+8)=AMP*(rnd)
		RFxn(I+9)=AMP*(rnd)

				
		
	  END DO
	  
	  
	  DO I=1,NX
	     DO J=1,NY
	      ROADN2(I,J)=RFxn(I)
		 END DO
	  END DO
	  
	  RETURN
	  END