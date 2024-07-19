!-----------------------------------------------------------------------
!　　　ダイポールアンテナ(送信)の初期設定
!-----------------------------------------------------------------------
      subroutine setup_dip()
      use fdtd_variable

!タイポール長の設定
      ktop=kfed+1+half_an
      kbtm=kfed-half_an
!係数の設定
      !am_d=1.0
      !bm_d=dt/mu0
      !s_dip=dx*dy-pi*a_dip*a_dip


      !do k=kbtm,ktop-1
        !idex(ifed,jfed,k)=3
        !idex(ifed,jfed-1,k)=3

        !idey(ifed,jfed,k)=3
        !idey(ifed-1,jfed,k)=3
      !end do
      !do i=ifed-1,ifed
         !do j=jfed-1,jfed
            do k=kbtm,ktop
               idez(ifed,jfed,k)=3 !線状ダイポール
            end do
         !end do
      !end do


     
      !do k=kbtm,ktop-1
        !idhx(ifed,jfed,k)=3
        !idhx(ifed,jfed-1,k)=3

        !idhy(ifed,jfed,k)=3
        !idhy(ifed-1,jfed,k)=3
      !end do
      !do i=ifed-1,ifed
         !do j=jfed-1,jfed
            do k=kbtm,ktop
               idhz(ifed,jfed,k)=3
            end do
         !end do
      !end do

      !idex(ifed,jfed,kfed)=0  !給電点は真空
      !idey(ifed,jfed,kfed)=0
      idez(ifed,jfed,kfed)=0

      !idhx(ifed,jfed,kfed)=0 
      !idhy(ifed,jfed,kfed)=0
      idhz(ifed,jfed,kfed)=0



      if(ifed.le.lpml)then
            print*,"error! check anntena_sousin x"
            print*,ifed,lpml
            stop
      end if

      if(jfed.ge.ny-lpml)then
            print*,"error! check antena_sousin y"
            print*,jfed,ny-lpml
            stop

      else if(jfed.le.lpml)then
            print*,"error! check antena_sousin y"
            print*,jfed,ny-lpml
            stop
      end if


      if(kfed+half_an.ge.nz-lpml)then
            print*,"error! check anntena top"
            print*,kfed+half_an,nz-lpml
            stop

      else if(kfed-half_an.le.lpml)then
            print*,"error! check anntena bottom"
            print*,kfed-half_an,lpml
            stop
      end if

 

      end subroutine setup_dip

!-----------------------------------------------------------------------
!　　　ダイポールアンテナ(受信)の初期設定
!-----------------------------------------------------------------------
      subroutine setup_dip_rece()
      use fdtd_variable

!タイポール長の設定
      ktop=kfed_rece+1+half_an
      kbtm=kfed_rece-half_an
!係数の設定
      !am_d=1.0
      !bm_d=dt/mu0
      !s_dip=dx*dy-pi*a_dip*a_dip


      !do k=kbtm,ktop-1
        !idex(ifed_rece,jfed_rece,k)=3
       !idex(ifed_rece,jfed_rece-1,k)=3

        !idey(ifed_rece,jfed_rece,k)=3
        !idey(ifed_rece-1,jfed_rece,k)=3
      !end do
      !do i=ifed_rece-1,ifed_rece
        !do j=jfed_rece-1,jfed_rece
            do k=kbtm,ktop
               idez(ifed_rece,jfed_rece,k)=3 !線状ダイポール
            end do
         !end do
      !end do
     
      !do k=kbtm,ktop-1
        !idhx(ifed_rece,jfed_rece,k)=3
        !idhx(ifed_rece,jfed_rece-1,k)=3

        !idhy(ifed_rece,jfed_rece,k)=3
        !idhy(ifed_rece-1,jfed_rece,k)=3
      !end do
      !do i=ifed_rece-1,ifed_rece
         !do j=jfed_rece-1,jfed_rece
            do k=kbtm,ktop
               idhz(ifed_rece,jfed_rece,k)=3
            end do
         !end do
      !end do



      !idex(ifed_rece,jfed_rece,kfed_rece)=0  !給電点は真空
      !idey(ifed_rece,jfed_rece,kfed_rece)=0
      idez(ifed_rece,jfed_rece,kfed_rece)=0

      !idhx(ifed_rece,jfed_rece,kfed_rece)=0  
      !idhy(ifed_rece,jfed_rece,kfed_rece)=0
      idhz(ifed_rece,jfed_rece,kfed_rece)=0


      if(ifed_rece.ge.nx-lpml)then
            print*,"error! check anntena_jyusin x"
            print*,ifed_rece,nx-lpml
            stop
      end if

      if(jfed_rece.ge.ny-lpml)then
            print*,"error! check antena_jyusin y"
            print*,jfed_rece,ny-lpml
            stop

      else if(jfed.le.lpml)then
            print*,"error! check antena_jyusin y"
            print*,jfed_rece,lpml
            stop
      end if

      if(kfed_rece+half_an.ge.nz-lpml)then
            print*,"error! check anntena_rece top"
            print*,kfed_rece+half_an,nz-lpml
            stop

      else if(kfed_rece-half_an.le.lpml)then
            print*,"error! check anntena_rece bottom"
            print*,kfed_rece-half_an,lpml
            stop
      end if



      end subroutine setup_dip_rece
!-----------------------------------------------------------------------
!　　ダイポール中心軸上の電界（送信）
!-----------------------------------------------------------------------
      subroutine e_dip()
      use fdtd_variable

      do k=kbtm, ktop
         if(k .ne. kfed) then
!導体部
            ez(ifed,jfed,k)=0.0
         else
!励振部
            call feed_sin(n)
         end if
      end do
      end subroutine e_dip
!-----------------------------------------------------------------------
!       励振波源（パルス波）
!-----------------------------------------------------------------------
      subroutine feed_pulse(n)
      use fdtd_variable
      real::iz

      if(lfeed==lsoft) then                              !ソフト給電
      iz=exp(-((t-0.5*dt-t0)/duration)**2)               !ガウスパルス
      ez(ifed,jfed,kfed)=ez(ifed,jfed,kfed)-befed*iz/(dx*dy)
      else
      ez(ifed,jfed,kfed)=exp(-((t-t0)/duration)**2)/dl   !ハード給電
      endif
      end subroutine feed_pulse
!-----------------------------------------------------------------------
!       励振波源（sin波）(FMCW)
!-----------------------------------------------------------------------
      subroutine feed_sin()
      use fdtd_variable
      
      if(n.EQ.1)then
            sweepf=0.0
            sweepn=0
            NNN=0
        end if
!   if (nnn<20000) then

!     if  (nnn<20000) then
        sweepv=sweep_haba/(sweep_step*dt)
        sweepf=sweepv*(sweepn-sweep_step/2)*dt
        sweepn=1+sweepn		
 !     if ( nnn<20000) then  
 !       sweepv=sweep_haba/(sweep_step*dt)
 !       sweepf=sweepv*(sweepn-sweep_step/2)*dt
 !       sweepn=1+sweepn
		
!		else if ( nnn.ge.20000.and.nnn<40000) then 
!        sweepv=sweep_haba/(sweep_step*dt)
!        sweepf=sweepv*(sweepn-sweep_step/2)*dt-4.0e9
!        sweepn=1+sweepn
		
		
 !       else if ( nnn.ge.40000.and.nnn<60000) then 
 !       sweepv=sweep_haba/(sweep_step*dt)
 !       sweepf=sweepv*(sweepn-sweep_step/2)*dt-8.0e9
 !       sweepn=1+sweepn
		
 !       else
  !      sweepv=sweep_haba/(sweep_step*dt)
  !      sweepf=sweepv*(sweepn-sweep_step/2)*dt-12e9
  !      sweepn=1+sweepn
		
!		end if



      !if(n.LE.gensui1)then
      !   vs=AMPV*sin(2.*PI*(freq_an+sweepf)*T)*1.0/2.0
      !&*(1.-cos(PI*t/(gensui1*dt)))

      !else if(n.GT.gensui.AND.n.LE.gensui2)then
      !  vs=AMPV*sin(2.*PI*(freq_an+sweepf)*T)

      !else if(n.GT.gensui2.AND.n.LE.sweep_step)then
      !  vs=AMPV*sin(2.*PI*(freq_an+sweepf)*T)*1./2.
      !&*(1.+cos(PI*(T-gensui2*dt)/((sweep_step-gensui2)*dt)))
      !else
      !  vs=0
      !end if

      !vsend = AMPV*sin(2*PI*(freq_an+sweepf)*t)                                    !souin FMCW
      vsend = AMPV*sin(2*PI*(freq_an)*t)                                           !uemura

      ez(ifed,jfed,kfed) = -vsend/dz

      
      end subroutine feed_sin
!-----------------------------------------------------------------------
!　　導体近傍の磁界
!-----------------------------------------------------------------------
      subroutine h_dip()
      use fdtd_variable

! Hx, Hy に対するCP法
      do k=kbtm,ktop-1
         hy(ifed-1,jfed,k)=am_d*hy(ifed-1,jfed,k)+bm_d*
     &       ((ez(ifed,jfed,k)*d_fed/dz-ez(ifed-1,jfed,k))/(dx-a_dip)
     &          -(ex(ifed-1,jfed,k+1)-ex(ifed-1,jfed,k))/dz)
         hy(ifed,jfed,k)=am_d*hy(ifed,jfed,k)+bm_d*
     &       ((ez(ifed+1,jfed,k)-ez(ifed,jfed,k)*d_fed/dz)/(dx-a_dip)
     &          -(ex(ifed,jfed,k+1)-ex(ifed,jfed,k))/dz)

         hx(ifed,jfed-1,k)=am_d*hx(ifed,jfed-1,k)-bm_d*
     &       ((ez(ifed,jfed,k)*d_fed/dz-ez(ifed,jfed-1,k))/(dy-a_dip)
     &          -(ey(ifed,jfed-1,k+1)-ey(ifed,jfed-1,k))/dz)
         hx(ifed,jfed,k)=am_d*hx(ifed,jfed,k)-bm_d*
     &       ((ez(ifed,jfed+1,k)-ez(ifed,jfed,k)*d_fed/dz)/(dy-a_dip)
     &          -(ey(ifed,jfed,k+1)-ey(ifed,jfed,k))/dz)
      end do
! Hzに対するCP法
      do k=kbtm,ktop
         hz(ifed-1,jfed-1,k)=am_d*hz(ifed-1,jfed-1,k)+bm_d/s_dip*
     &          ( (ex(ifed-1,jfed,k)*(dx-a_dip)-ex(ifed-1,jfed-1,k)*dx)
     &           -(ey(ifed,jfed-1,k)*(dy-a_dip)-ey(ifed-1,jfed-1,k)*dy))
         hz(ifed-1,jfed,k)=am_d*hz(ifed-1,jfed,k)+bm_d/s_dip*
     &          ( (ex(ifed-1,jfed+1,k)*dx-ex(ifed-1,jfed,k)*(dx-a_dip))
     &           -(ey(ifed,jfed,k)*(dy-a_dip)-ey(ifed-1,jfed,k)*dy) )
         hz(ifed,jfed-1,k)=am_d*hz(ifed,jfed-1,k)+bm_d/s_dip*
     &          ( (ex(ifed,jfed,k)*(dx-a_dip)-ex(ifed,jfed-1,k)*dx)
     &           -(ey(ifed+1,jfed-1,k)*dy-ey(ifed,jfed-1,k)*(dy-a_dip)))
         hz(ifed,jfed,k)=am_d*hz(ifed,jfed,k)+bm_d/s_dip*
     &          ( (ex(ifed,jfed+1,k)*dx-ex(ifed,jfed,k)*(dx-a_dip))
     &           -(ey(ifed+1,jfed,k)*dy-ey(ifed,jfed,k)*(dy-a_dip)) )
      end do
      end subroutine h_dip
!-----------------------------------------------------------------------
!　　　給電点電圧，電流の出力
!-----------------------------------------------------------------------
      subroutine out_vi(n)
      use fdtd_variable
      real v,i
c
      if(n == 1) then
         open(33,file='out_vi.txt')
      end if
!      v=-ez(ifed,jfed,kfed)*d_fed
      v=-ez(ifed,jfed,kfed)*dz
      i=(hy(ifed,jfed,kfed)-hy(ifed-1,jfed,kfed))*dy
     &  +(hx(ifed,jfed-1,kfed)-hx(ifed,jfed,kfed))*dx
      write(33,330)t,v,0.5*(i+ip)
      ip=i
      if(n == nstep) then
         close(33)
      end if
  330 format(3e15.6)
      end subroutine 

!-----------------------------------------------------------------------
!　　　給電点電圧，電流の出力（受信）
!-----------------------------------------------------------------------
      subroutine out_vi_rece(n)
      use fdtd_variable
      real v,i
      character filename*128

      filename = "result/out_vi_rece/out_vi_reve.txt"
c
      if(n == 1) then
         open(34,file=filename,status="replace")
      end if
!      v=-ez(ifed_rece,jfed_rece,kfed_rece)*d_fed
      v=-ez(ifed_rece,jfed_rece,kfed_rece)*dz
      i=(hy(ifed_rece,jfed_rece,kfed_rece)
     &-hy(ifed_rece-1,jfed_rece,kfed_rece))*dy
     &  +(hx(ifed_rece,jfed_rece-1,kfed_rece)
     &   -hx(ifed_rece,jfed_rece,kfed_rece))*dx
      write(34,330)t,v,0.5*(i+ip)
      ip=i
      if(n == nstep) then
         close(34)
      end if
  330 format(3e15.6)
      end subroutine out_vi_rece

!---------------------------------------------------------------------------
!      FFT
!---------------------------------------------------------------------------
      subroutine CV_save(n)
      use fdtd_variable

      integer N
      !character*2 WAVECHA
      !character*3 WAVECHA
      character*4 WAVECHA
      character*64 NAMEOUT,NAMEOUT2,NAMEOUT3,NAMEOUT4,NAMEOUT5
      
      !WRITE(WAVECHA,"(I2.2)")WAVE
      !WRITE(WAVECHA,"(I3.3)")WAVE
      WRITE(WAVECHA,"(I4.4)")WAVE

      EBEATZ(N)=-vsend*(ez(ifed_rece,jfed_rece,kfed_rece)*dz)

        NAMEOUT=WAVECHA//"/ezR"
        NAMEOUT2=WAVECHA//"/esend"
        NAMEOUT3=WAVECHA//"/BEATEZ"
        NAMEOUT4=WAVECHA//"/kakunin"
        NAMEOUT5=WAVECHA//"/ezT"

        
        OPEN(120,FILE=NAMEOUT)
        OPEN(121,FILE=NAMEOUT2)
        OPEN(124,FILE=NAMEOUT3)
        OPEN(125,FILE=NAMEOUT4)
        OPEN(126,FILE=NAMEOUT5)

        WRITE(121,*)N,sweepf
        WRITE(125,*)NNN,sweepn,sweepv,sweepf
        NNN=NNN+1
        WRITE(120,*)NNN,ez(ifed_rece,jfed_rece,kfed_rece)
        WRITE(126,*)NNN,ez(ifed,jfed,kfed)
        WRITE(124,*)NNN,EBEATZ(N)
    
      end subroutine     

