!-----------------------------------------------------------------------
!        FDTDの共通変数
!-----------------------------------------------------------------------
      module fdtd_variable
!解析領域                                
      !integer,parameter::nxx=3142,nyy=10,nzz=263   !解析領域分割数
      !integer,parameter::nxx=400,nyy=10,nzz=80 !デバック                    uemura    初期nxx=400,nyy=10,nzz=80:1/20セル          nxx=500,nyy=10,nzz=90:1/30セル
	  	  
	   !integer,parameter::nxx=400,nyy=10,nzz=225                          !daiのデフォルト     

	   !integer,parameter::nxx=800,nyy=10,nzz=225                            
	   
	   !integer,parameter::nxx=500,nyy=10,nzz=225                           !FMCW79GHz    hei_an 0.4m
	   
	  !integer,parameter::nxx=500,nyy=10,nzz=500                            !FMCW79GHz    hei_an 1m       3.23e-3
	   
	   !integer,parameter::nxx=400,nyy=300,nzz=3100
	   
	   !integer,parameter::nxx=1500,nyy=200,nzz=200                          
	   !integer,parameter::nxx=1200,nyy=200,nzz=500                             !ariyama27.9GHz初期
	   !integer,parameter::nxx=1200,nyy=200,nzz=1000                            !itumono27.9
	   integer,parameter::nxx=600,nyy=200,nzz=500                             !aeiyama_M1
	   

      !integer,parameter::nstep=1024*3              !計算ステップ数
      !integer,parameter::nstep=3500              !計算ステップ数   

      integer,parameter::nstep=5000                                           !8.4
	   !integer,parameter::nstep=8000
	   !integer,parameter::nstep=15000
	   !integer,parameter::nstep=1
	  
      !real,parameter::dx=3.23e-3,dy=dx,dz=dy   !セルサイズ                                  uemura初期:3.23e-3              2.15e-3:1/30セル
	  !real,parameter::dx=2.0e-4,dy=dx,dz=dy                                            !ariyama M1
	  !real,parameter::dx=3.8e-4,dy=dx,dz=dy 
	  !real,parameter::dx=1.0e-3,dy=dx,dz=dy
	  real,parameter::dx=5.0e-4,dy=dx,dz=dy
      real::t                                   !時間
      

!定数
      real,parameter::eps0=8.854188e-12,mu0=1.256637e-6 !真空の誘電率，透磁率
      real,parameter::c=2.9979246e8                     !光速
      real,parameter :: PI=3.14159265359e0               !円周率
      real,parameter ::dt=0.99999/
     &(c*sqrt(1.0/(dx*dx)+1.0/(dy*dy)+1.0/(dz*dz)))
!      real(8),parameter ::dt=0.99e0/
!     &(c*sqrt(1.0/(dx*dx)+1.0/(dy*dy)+1.0/(dz*dz)))

!PML吸収境界
      integer,parameter::lpml=8,order=4            !PMLの次数，層数
      real,parameter::rmax=-120.0                  !要求精度〔dB〕

!全計算領域
      integer,parameter::nx=nxx+2*lpml,ny=nyy+2*lpml,nz=nzz+2*lpml !全解析領域
      integer,parameter::nxb=nx-lpml,nyb=ny-lpml,nzb=nz-lpml  !解析領域の境界

!電界磁界の配列
      real::ex(0:nx,0:ny,0:nz),ey(0:nx,0:ny,0:nz),ez(0:nx,0:ny,0:nz)
      real::hx(0:nx,0:ny,0:nz),hy(0:nx,0:ny,0:nz),hz(0:nx,0:ny,0:nz)

!係数の配列
      real::aex(0:255),aey(0:255),aez(0:255) 
      real::bexy(0:255),bexz(0:255)
      real::beyx(0:255),beyz(0:255)
      real::bezx(0:255),bezy(0:255)
      real::amx(0:255),amy(0:255),amz(0:255) 
      real::bmxy(0:255),bmxz(0:255)
      real::bmyx(0:255),bmyz(0:255)
      real::bmzx(0:255),bmzy(0:255)

!媒質定数の配列
      integer::id_cnt   !媒質の数
      real::epr(0:255),sge(0:255),mur(0:255),sgm(0:255)

!ID配列
      integer(1)::idex(0:nx,0:ny,0:nz),idey(0:nx,0:ny,0:nz),
     &              idez(0:nx,0:ny,0:nz),idhx(0:nx,0:ny,0:nz),    
     &              idhy(0:nx,0:ny,0:nz),idhz(0:nx,0:ny,0:nz)

      real::ic0,ic1,jc0,jc1,kc0,kc1    !IDの境界を決める際に必要


!道路の設定
      !integer,parameter::ic=nx/2, jc=ny/2     !道路の中心(x,y)
      !integer,parameter::lx2=nxx/2, ly2=nyy/2, lz2=KSURF       !(道路の寸法）/2
      !integer,parameter::KSURF=lpml+20              !これより下は道路(pmlより上)                            !uemura
	  
	  !integer,parameter::KSURF=lpml+100                                           !ariyama                           kihon
	   integer,parameter::KSURF=lpml+125         
	  !integer,parameter::KSURF=lpml+25                                           ! 27.9GHz itumono
	  
	  !integer,parameter::KSURF=lpml
	  
      real,parameter::epsr=5.31e0                        !道路の比誘電率
      real,parameter::musr=1.0e0                        !道路の比透磁率
      real,parameter::yu_s=epsr*eps0                   !道路の誘電率
      real,parameter::mu_s=musr*mu0                    !道路の透磁率
      real,parameter::sgse=0.113e0                       !道路の導電率
      


      
!アンテナのパラメータ
      !real::ip=0.0                                   !(n-1/2)時間の電流
      !real::len_an=0.15                                !
      real,parameter::len_an=3.23e-2                  !ダイポールの全長[m]
      !real,parameter :: freq_an = 4.64e9       !入力信号の周波数                             !uemura初期
	  
	  !real,parameter :: freq_an = 79e9                       !teldevise.co.jpのマネ 帯域幅4GHz
	  
	  real,parameter :: freq_an = 27.9e9
	  
	  
      !real(8),parameter :: freq_an = 4.64d9       !入力信号の周波数
       real,parameter::half_an=nint(((len_an-dz)/dz))*0.5       !アンテナ半長[cell],h/2
      !real,parameter::hei_an=0.75               !アンテナの地面からの高さ[m]      !使ってない
      !real,parameter::hei_an=0.1e0                                                         !uemura
	  !real,parameter::hei_an=0.4                                        !ariyama_27.9GHz初期                                                                                               
	  !real,parameter::hei_an=0.004                                                             
	  !real,parameter::hei_an=0.75                          !27.9GHz_itumono
	  real,parameter::hei_an=0.15          !dekasugi
	  !real,parameter::hei_an=0.07                                !ariyama_M1
	  !real,parameter::hei_an=0.06
	  
      real,parameter::h_an=nint(hei_an/dz)      !アンテナの地面からの高さ[cell]
      integer::kbtm, ktop                       !ダイポールの下端，上端
      real::am_d,bm_d                           !係数
      real::s_dip                                             !ダイポールセルの断面積
      real,parameter::a_dip=1e-3,d_fed=0.1*a_dip     !半径，給電ギャップ長

!ダイポール送信点、受信点
      !integer,parameter::ifed=31, jfed=ny/2, kfed=half_an+h_an+KSURF                        !uemura
	  
	  integer,parameter::ifed=50, jfed=ny/2, kfed=half_an+h_an+KSURF                                         !cell3.23
	  
	  !integer,parameter::ifed=100, jfed=ny/2, kfed=half_an+h_an+KSURF     
	  
!      integer,parameter::between=0.1 !アンテナ間の距離[m]
      !integer,parameter::between=1 !デバック
      !integer,parameter::between_cell=between/dx !アンテナ間の距離[cell]                                  uemura
      !integer,parameter::ifed_rece=ifed+between_cell                                    !uemura
	  !integer,parameter::ifed_rece=ifed+3                                 !ariyama:送信アンテナ受信電波         cell3.23
	  !integer,parameter::ifed_rece=550    
	  !integer,parameter::ifed_rece=1100                              !itumono_27.9
	  integer,parameter::ifed_rece=550   
	  
      integer,parameter::jfed_rece=jfed
      integer,parameter::kfed_rece=kfed

      
!励振パルスのパラメーター
      real,parameter::duration=0.1e-9,t0=4.0*duration   !パルス幅，ピーク時刻 



!励振の種類
      integer::lfeed,lsoft,lhard
      parameter(lsoft=1,lhard=2)
      parameter(lfeed=lhard)
      real::befed                                       !ソフト給電の係数
      real,parameter::dl=0.001e0                          !ハード給電の間隙

!FMCW
      !real,parameter :: AMPV = 85.44003745    !信号の振幅
      real,parameter :: AMPV = 10e0    !信号の振幅
      integer sweepn
      !integer,parameter :: sweep_step = 3000              !FMCWの掃引ステップ                                           syoki uemura
	  
	  !integer,parameter :: sweep_step = 3500                                       !ariyama
	  
	  !integer,parameter :: sweep_step = 6000                                          !8.4
	  !integer,parameter :: sweep_step = 8000 
	  !integer,parameter :: sweep_step = 4000 
      !integer,parameter :: sweep_step = 1000              !FMCWの掃引ステップ
      !integer,parameter::sweep_step = 1.00e+6
      !integer,parameter :: gensui1 = sweep_step/10, gensui2 = sweep_step*9/10 !信号の立ち上がり、立ち下がり              !ariy
      !real,parameter :: sweep_haba=3.00e+6     !掃引周波数幅
      !real(8),parameter :: sweep_haba=3.00d+6     !掃引周波数幅
      !real,parameter :: sweep_haba=1.50e+9     !掃引周波数幅                                                                syoki uemura
	  
	  !real,parameter :: sweep_haba=3.00e+8                                      !ariyama  FMCW
	  !real,parameter :: sweep_haba=3.00e+9
	  
	  real,parameter :: sweep_haba=4.00e+9                                       !帯域幅4GHz 
	  
	  !real,parameter :: sweep_haba=4.00e+7
	  
      real sweepf,vsend                        !掃引周波数(変数)、送信電圧                                                            syoki
      !real(8) sweepf,vsend                        !掃引周波数(変数)、送信電圧

      real sweepv                              !掃引速度
      !real(8) sweepv                              !掃引速度
      !real vs  !給電電圧

!FFT(cv_save)
      real EBEATZ(nstep)
      !real(8) EBEATZ(nstep)
      integer WAVE
      integer NNN

!miti.f90に必要(make_yu_sig)
!      real Fxn(nx,ny)
!      real FXn1(nx,ny)
!      integer Fxn2(nx,ny)
!      integer count
      integer,parameter::yux=nx-16
      integer,parameter::yuy=ny-16
      integer,parameter::yuz=nz-16
      integer,parameter::ifeed2=nx/2
      integer,parameter::jfeed2=ny/2
      integer,parameter::kfeed2=nz/2
      real Fxn(nx,ny)
      real FXn1(nx,ny)
      real Fxn2(nx,ny)

      end module fdtd_variable
!-----------------------------------------------------------------------
!       メインプログラム
!-----------------------------------------------------------------------      
      program fdtd_3d
      use fdtd_variable
      
      !do WAVE=0,1024                                            !1024回道を動かす ari
	  do WAVE=176,179,1
!	  do WAVE=260,500,1
!	  WAVE=399
	  
      !do WAVE=,255,-2
      !do WAVE=0,511
	  !do WAVE=117,255
	  !do WAVE=255,0,-1
      !do WAVE=71,511                !10.8測定では実際は256までしか使わない
      !do WAVE=3,255,2
      !do WAVE=11,255,2
	  !do WAVE=80,100,2
	  !do WAVE=199,101,-2
	  
        print*,"WAVE:",WAVE
        NNN=0
      
        call clear_eh
        call clear_dip

        call setup()                    !FDTDの初期設定
        call init_pml()                 !PMLの初期設定
        call setup_dip()                !ダイポールアンテナ(送信)初期設定
        call setup_dip_rece()           !ダイポールアンテナ(受信)の初期設定

       ! if(WAVE==0)then
            print*,"ifed:",ifed
            print*,"jfed:",jfed
            print*,"kfed:",kfed
            print*,"ifed_rece:",ifed_rece
            print*,"jfed_rece:",jfed_rece
            print*,"kfed_rece:",kfed_rece
            print*,"KSURF:",KSURF 
        !  end if
            t=dt
            print*,t

        do n=1,nstep                    !繰り返し計算
          write(*,*)'Time step:',n
          
          call e_cal                   !電界の計算
          call e_pml()                 !電界に対するPML

        !if(WAVE==0)then
          !call out_emf(n)              !計算結果の出力
          !call ef_sou_out(n)           !送信部の出力
          !call ef_rece_out(n)          !受信部の出力
          !call ef_all_out(n)           !全電界の出力
        !end if

          call e_dip(n)                 !ダイポール中心軸上の電界（送信）
          
         if(n==1)then 
          call ide_out_xz
          call ide_out_xy
          call idh_out_xz
          call idh_out_xy
         end if

          call cv_save(n)               
          

          t=t+0.5*dt                   !時間の更新

          call h_cal                   !磁界の計算
          call h_pml()                 !磁界に対するPML

          t=t+0.5*dt                   !時間の更新
          
        end do

      end do

      end program fdtd_3d

!-----------------------------------------------------------------------
!     FDTDの初期設定      
!-----------------------------------------------------------------------
      subroutine setup()
      use fdtd_variable
      real::mu
      integer::i,j,k

      call epsmu()                        !誘電体の設定

!係数の計算
      call cal_ce(aex,bexy,bexz,dy,dz)
      call cal_ce(aey,beyz,beyx,dz,dx)
      call cal_ce(aez,bezx,bezy,dx,dy)
      call cal_cm(amx,bmxy,bmxz,dy,dz)
      call cal_cm(amy,bmyz,bmyx,dz,dx)
      call cal_cm(amz,bmzx,bmzy,dx,dy)
      end subroutine setup
!-----------------------------------------------------------------------
!     電界セルエッジの係数
!-----------------------------------------------------------------------
      subroutine cal_ce(ae,be1,be2,d1,d2)
      use fdtd_variable
      real::ae(0:255),be1(0:255),be2(0:255)
      real::eps,sgme
!      
      do i=0,id_cnt-1
        if(i.eq.3)then !完全導体
          ae(i)=0.0
          be1(i)=0.0
          be2(i)=0.0
          cycle
        end if
         eps = epr(i)*eps0
         sgme = sge(i)
         a=0.5*sgme*dt/eps 
         ae(i)=(1.0-a)/(1.0+a)
         be1(i)=dt/eps/(1.0+a)/d1
         be2(i)=dt/eps/(1.0+a)/d2
      end do
      end subroutine cal_ce
!-----------------------------------------------------------------------
!     磁界セルエッジの計算
!-----------------------------------------------------------------------
      subroutine cal_cm(am,bm1,bm2,d1,d2)
      use fdtd_variable
      real::am(0:255),bm1(0:255),bm2(0:255)
      real::mu,sgmm
!      
      do i=0,id_cnt-1
        if(i.eq.3)then
          am(i)=1.0
          bm1(i)=0.0
          bm2(i)=0.0
          cycle
        end if
         mu=mur(i)*mu0
         sgmm=sgm(i)
         a=0.5*sgmm*dt/mu 
         am(i)=(1.0-a)/(1.0+a)
         bm1(i)=dt/mu/(1.0+a)/d1
         bm2(i)=dt/mu/(1.0+a)/d2
      end do
      end subroutine cal_cm
!-----------------------------------------------------------------
!　　各媒質の媒質定数
!-----------------------------------------------------------------
      subroutine epsmu()   
      use fdtd_variable


      
!ID
!0　真空
!1　誘電体(道路)
!2　1/2誘電体(真空、道路)
!3　アンテナ（完全導体）
!4　1/4道路(真空、道路)
!5　3/4道路(真空、道路)

!周囲真空
      epr(0)=1.0
      sge(0)=0.0
      mur(0)=1.0
      sgm(0)=0.0
!誘電体(道路)内部
      epr(1)=epsr
      sge(1)=sgse
      mur(1)=musr
      sgm(1)=0.0
!誘電体（道路）・真空の境界
      epr(2)=0.5*(epr(1)+epr(0))
      sge(2)=0.5*(sge(1)+sge(0))
      mur(2)=1.0
      sgm(2)=0.0
      
!アンテナ(金属)
      epr(3)=0.0
      sge(3)=0.0
      mur(3)=1.0
      sgm(3)=0.0

!1/4道路
      epr(4)=0.25*epr(1)+0.75*epr(0)
      sge(4)=0.0
      mur(4)=1.0
      sgm(4)=0.0
      
!3/4道路
      epr(5)=0.75*epr(1)+0.25*epr(0)
      sge(5)=0.75*sge(1)+0.25*sge(0)
      mur(5)=1.0
      sgm(5)=0.0

      id_cnt=6


      !ic0=ic-lx2
      !ic1=ic+lx2
      !jc0=jc-ly2
      !jc1=jc+ly2
      !kc0=KSURF-lz2
      !kc1=lz2

      ic0=lpml+3
      ic1=nx-lpml-3
      jc0=lpml+3
      jc1=ny-lpml-3
      kc0=lpml+3
      kc1=KSURF

      !fxn(:,:)=KSURF
      !fxn(ic0:ic1,jc0:jc1)=KSURF
      call miti()

      print*,"lpml:",lpml
      print*,"ic0:",ic0
      print*,"nx-lpml",nx-lpml
      print*,"ic1:",ic1
      print*,"jc0:",jc0
      print*,"kc0:",kc0
      print*,"KSURF:",KSURF


      print*,"fxn1(ic0+1,jc0+1):",FXn1(ic0+1,jc0+1)
      print*,"fxn(ic0+100,jc0+2):",FXn(ic0+100,jc0+2)
      print*,"fxn(ic0+3,jc0+3):",FXn(ic0+3,jc0+3)
      print*,"fxn(ic0+4,jc0+4):",FXn(ic0+4,jc0+4)
      print*,"fxn(ic0+5,jc0+5):",FXn(ic0+5,jc0+5)
      print*,"fxn(ic0+6,jc0+6):",FXn(ic0+6,jc0+6)
      print*,"fxn(ic0+7,jc0+7):",FXn(ic0+7,jc0+7)
      print*,"fxn(ic0-1,jc0+1):",FXn(ic0-1,jc0+1)
      print*,"fxn(ic0-2,jc0+1):",FXn(ic0-2,jc0+1)
      print*,"fxn(ic0-3,jc0+1):",FXn(ic0-3,jc0+1)
      print*,"fxn(ic0-4,jc0+1):",FXn(ic0-4,jc0+1)
      print*,"mur(0)",mur(0)
      print*,"mur(1)",mur(1)
      print*,"mur(2)",mur(2)
      print*,"mur(3)",mur(3)
      print*,"sgm(0),sgm(1),sgm(2),sgm(3)",sgm(0),sgm(1),sgm(2),sgm(3)




      do i=0,nx
        do j=0,ny
          do k=0,nz
            if(i .gt. ic0 .and. i .lt.ic1) then                         !nxx範囲
!			if(i .gt. 4100 .and. i .lt.4500 .and.j.gt.jc0.and.j.lt.jc1                      !3.23d-4
!     &                   .and.k.gt.50 .and.k.lt.450) then                  	 
	 
	 
 !               idex(i,j,k)=3
 !               idey(i,j,k)=3
 !               idez(i,j,k)=3
 !               idhx(i,j,k)=3
 !               idhy(i,j,k)=3
 !               idhz(i,j,k)=3
			
             if(j.gt.jc0.and.j.lt.jc1.and.k.gt.kc0                  
     &                  .and.k.lt.fxn(i,j)) then                                     !地面より下
                !idex(i,j,k)=1
                !idey(i,j,k)=1
                !idez(i,j,k)=1
                !idhx(i,j,k)=1
                !idhy(i,j,k)=1
                !idhz(i,j,k)=1
                idex(i,j,k)=3                                                       !uemura:3
                idey(i,j,k)=3
                idez(i,j,k)=3
                idhx(i,j,k)=3
                idhy(i,j,k)=3
                idhz(i,j,k)=3
                !idex(i,j,k)=0
                !idey(i,j,k)=0
                !idez(i,j,k)=0
                !idhx(i,j,k)=0
                !idhy(i,j,k)=0
                !idhz(i,j,k)=0
				
!			else if(i .gt. 40 .and. i .lt.60 .and.j.gt.jc0.and.j.lt.jc1         !3.23d-4
!     &                   .and.k.gt.300 .and.k.lt.990) then                                         !100-500 27.9初期
!                idex(i,j,k)=3
!                idey(i,j,k)=3
!                idez(i,j,k)=3
!                idhx(i,j,k)=3
!                idhy(i,j,k)=3
!                idhz(i,j,k)=3

!			else if(i .gt. 1140 .and. i .lt.1160 .and.j.gt.jc0.and.j.lt.jc1         !3.23d-4
!     &                   .and.k.gt.300 .and.k.lt.990) then
!                idex(i,j,k)=3
!                idey(i,j,k)=3
!               idez(i,j,k)=3
!                idhx(i,j,k)=3
!                idhy(i,j,k)=3
!                idhz(i,j,k)=3



			
 !           else if(i .gt. 100 .and. i .lt.200 .and.j.gt.jc0.and.j.lt.jc1
!     &                   .and.k.gt.25 .and.k.lt.350) then                                       !普段comment out
	 
!            else if(i .gt. 570 .and. i .lt.580 .and.j.gt.jc0.and.j.lt.jc1                      !3.23d-4
!     &                   .and.k.gt.300 .and.k.lt.600) then                  	 
	 
	 
!                idex(i,j,k)=3
!               idey(i,j,k)=3
!                idez(i,j,k)=3
!                idhx(i,j,k)=3
!                idhy(i,j,k)=3
!                idhz(i,j,k)=3
				
				
				
				
				
				
				
            else if(j.lt.jc0.or.j.gt.jc1.or.k.lt.kc0
     &                   .or.k.gt.fxn(i,j)) then                                       !解析領域の外側,地面より上
                idex(i,j,k)=0
                idey(i,j,k)=0
                idez(i,j,k)=0
                idhx(i,j,k)=0
                idhy(i,j,k)=0
                idhz(i,j,k)=0
              else
                !idex(i,j,k)=2                                                        
                !idey(i,j,k)=2
                !idez(i,j,k)=2
                !idhx(i,j,k)=2
                !idhy(i,j,k)=2
                !idhz(i,j,k)=2
!                idex(i,j,k)=3                                                    !uemura:3 地面との境界
!                idey(i,j,k)=3
!                idez(i,j,k)=3
!                idhx(i,j,k)=3
!                idhy(i,j,k)=3
!                idhz(i,j,k)=3
                idex(i,j,k)=0
                idey(i,j,k)=0
                idez(i,j,k)=0
                idhx(i,j,k)=0
                idhy(i,j,k)=0
                idhz(i,j,k)=0
                  
                                                  
              end if
            else if(i.eq.ic0.and.j.eq.jc0.and.k.eq.kc0) then
!                idex(i,j,k)=4                                                      !uemura:4
!                idey(i,j,k)=4
!                idez(i,j,k)=4
!                idhx(i,j,k)=4
!                idhy(i,j,k)=4
!                idhz(i,j,k)=4
                idex(i,j,k)=0
                idey(i,j,k)=0
                idez(i,j,k)=0
                idhx(i,j,k)=0
                idhy(i,j,k)=0
                idhz(i,j,k)=0
            else if(i.eq.ic0.and.j.eq.jc0.and.k.eq.kc1) then
!!                idex(i,j,k)=4                                                     !uemura:4
 !               idey(i,j,k)=4
!                idez(i,j,k)=4
!!                idhx(i,j,k)=4
 !               idhy(i,j,k)=4
 !!               idhz(i,j,k)=4
                idex(i,j,k)=0
                idey(i,j,k)=0
                idez(i,j,k)=0
                idhx(i,j,k)=0
                idhy(i,j,k)=0
                idhz(i,j,k)=0
            else if(i.eq.ic0.and.j.eq.jc1.and.k.eq.kc0) then
!                idex(i,j,k)=4                                                             !uemura:4
!                idey(i,j,k)=4
!                idez(i,j,k)=4
!                idhx(i,j,k)=4
!                idhy(i,j,k)=4
!                idhz(i,j,k)=4
                idex(i,j,k)=0
                idey(i,j,k)=0
                idez(i,j,k)=0
                idhx(i,j,k)=0
                idhy(i,j,k)=0
                idhz(i,j,k)=0
            else if(i.eq.ic0.and.j.eq.jc1.and.k.eq.kc1) then
!                idex(i,j,k)=4                                                                  !uemura:4
!                idey(i,j,k)=4
!                idez(i,j,k)=4
!                idhx(i,j,k)=4
!                idhy(i,j,k)=4
!                idhz(i,j,k)=4
                idex(i,j,k)=0
                idey(i,j,k)=0
                idez(i,j,k)=0
                idhx(i,j,k)=0
                idhy(i,j,k)=0
                idhz(i,j,k)=0
            else if(i.eq.ic1.and.j.eq.jc0.and.k.eq.kc0) then
!                idex(i,j,k)=4                                                                  !uemura:4
!                idey(i,j,k)=4
!                idez(i,j,k)=4
!                idhx(i,j,k)=4
!                idhy(i,j,k)=4
!                idhz(i,j,k)=4
                idex(i,j,k)=0
                idey(i,j,k)=0
                idez(i,j,k)=0
                idhx(i,j,k)=0
                idhy(i,j,k)=0
                idhz(i,j,k)=0

            else if(i.eq.ic1.and.j.eq.jc0.and.k.eq.kc1) then
!                idex(i,j,k)=4                                                        !uemura:4
!                idey(i,j,k)=4
!                idez(i,j,k)=4
!                idhx(i,j,k)=4
!                idhy(i,j,k)=4
!                idhz(i,j,k)=4
                idex(i,j,k)=0
                idey(i,j,k)=0
                idez(i,j,k)=0
                idhx(i,j,k)=0
                idhy(i,j,k)=0
                idhz(i,j,k)=0
            else if(i.eq.ic1.and.j.eq.jc1.and.k.eq.kc0) then
!                idex(i,j,k)=4                                                       !uemura:4
!                idey(i,j,k)=4
!                idez(i,j,k)=4
!                idhx(i,j,k)=4
!                idhy(i,j,k)=4
!                idhz(i,j,k)=4
                idex(i,j,k)=0
                idey(i,j,k)=0
                idez(i,j,k)=0
                idhx(i,j,k)=0
                idhy(i,j,k)=0
                idhz(i,j,k)=0

            else if(i.eq.ic1.and.j.eq.jc1.and.k.eq.kc1) then
!                idex(i,j,k)=4                                                          !uemura:4
!                idey(i,j,k)=4
!                idez(i,j,k)=4
!                idhx(i,j,k)=4
!                idhy(i,j,k)=4
!                idhz(i,j,k)=4  
                idex(i,j,k)=0
                idey(i,j,k)=0
                idez(i,j,k)=0
                idhx(i,j,k)=0
                idhy(i,j,k)=0
                idhz(i,j,k)=0            
            else
              idex(i,j,k)=0
              idey(i,j,k)=0
              idez(i,j,k)=0
              idhx(i,j,k)=0
              idhy(i,j,k)=0
              idhz(i,j,k)=0
            end if               
          end do
        end do
      end do
      end subroutine epsmu
!-----------------------------------------------------------------------
!     電界の計算
!-----------------------------------------------------------------------
      subroutine e_cal()
      use fdtd_variable
      integer::i,j,k
!Ex
      do k=1,nz-1 
         do j=1,ny-1    
            do i=0,nx-1
               id=idex(i,j,k)       
               ex(i,j,k)=aex(id)*ex(i,j,k)+bexy(id)*(hz(i,j,k)
     &             -hz(i,j-1,k))-bexz(id)*(hy(i,j,k)-hy(i,j,k-1))
            end do
         end do
      end do
!Ey
      do k=1,nz-1
         do j=0,ny-1
            do i=1,nx-1
               id=idey(i,j,k)       
               ey(i,j,k)=aey(id)*ey(i,j,k)+beyz(id)*(hx(i,j,k)
     &             -hx(i,j,k-1))-beyx(id)*(hz(i,j,k)-hz(i-1,j,k))
            end do
         end do
      end do
!Ez
      do k=0,nz-1
         do j=1,ny-1
            do i=1,nx-1
               id=idez(i,j,k)       
               ez(i,j,k)=aez(id)*ez(i,j,k)+bezx(id)*(hy(i,j,k)
     &             -hy(i-1,j,k))-bezy(id)*(hx(i,j,k)-hx(i,j-1,k))
            end do
         end do
      end do
      end subroutine e_cal
!-----------------------------------------------------------------------
!     磁界の計算
!-----------------------------------------------------------------------
      subroutine h_cal()
      use fdtd_variable      
      integer::i,j,k
!Hx
      do k=0,nz-1
         do j=0,ny-1
            do i=1,nx-1
               id=idhx(i,j,k)       
               hx(i,j,k)=amx(id)*hx(i,j,k)-bmxy(id)*(ez(i,j+1,k)
     &                   -ez(i,j,k))+bmxz(id)*(ey(i,j,k+1)-ey(i,j,k))
            end do
         end do
      end do
!Hy
      do k=0,nz-1
         do j=1,ny-1
            do i=0,nx-1
               id=idhy(i,j,k)       
               hy(i,j,k)=amy(id)*hy(i,j,k)-bmyz(id)*(ex(i,j,k+1)
     &                   -ex(i,j,k))+bmyx(id)*(ez(i+1,j,k)-ez(i,j,k))
            end do
         end do
      end do
!Hz
      do k=1,nz-1
         do j=0,ny-1
            do i=0,nx-1
               id=idhz(i,j,k)       
               hz(i,j,k)=amz(id)*hz(i,j,k)-bmzx(id)*(ey(i+1,j,k)
     &                   -ey(i,j,k))+bmzy(id)*(ex(i,j+1,k)-ex(i,j,k))
            end do
         end do
      end do
      end subroutine h_cal


!-----------------------------------------------------------------------
!       路面を乗せる
!-----------------------------------------------------------------------
      subroutine miti()
        use fdtd_variable


        INTEGER III,JJJ
        CHARACTER*64 NUM

      WRITE(NUM,121)WAVE
! 121  FORMAT(4Hrod-,I3.3)
 121  FORMAT(4Hrod-,I4.4)
      WRITE(*,*)NUM
             
        size_x = nint(yux/2.0)
        size_y = nint(yuy/2.0)
        size_z = nint(yuz/2.0)
        
        ife = ifeed2
        jfe = jfeed2
        kfe = kfeed2
        
        
        DO I=1,nx
             Fxn(I,:)=KSURF                                                         !FXN:地面(変動)
             Fxn2(I,:)=REAL(KSURF)*dz
        END DO
             OPEN(30,FILE=NUM)
             !DO I=ife-size_x,ife+size_x
                   !READ(30,*)III,Fxn1(I,1)
                   !Fxn(I,:)=KSURF+int(Fxn1(I,1)/dz)
                   !Fxn2(I,:)=REAL(KSURF)*dz+Fxn1(I,1)
                   !WRITE(1212,*)I,Fxn(I,1)
             !END DO
            !CLOSE(30)
            DO I=1,nx
             DO J=1,ny
               IF(I.GE.ife-size_x.AND.I.LE.
     &                    ife+size_x.AND.J.GE.
     &                      jfe-size_y.AND.J.LE.jfe+size_y)THEN
               READ(30,*)III,JJJ,Fxn1(I,J)
               !write(*,*)iii,i,jjj,j
              Fxn(I,J)=int((Fxn1(I,J))/dz)+KSURF                    !/dzで[cell]    
              Fxn2(i,j)=REAL(KSURF)*dz+Fxn1(I,J)
              ELSE
              Fxn(I,J)=KSURF
              END IF
            END DO
            IF(I.GE.ife-size_x.AND.I.LE.ife+size_x)THEN
               READ(30,*)
             END IF
           END DO
          CLOSE(30) 
             
        end subroutine miti      
!-----------------------------------------------------------------------
!       計算結果の出力
!-----------------------------------------------------------------------
      subroutine out_emf(n)
      use fdtd_variable           
      integer,parameter::io=ifed_rece
      integer,parameter::jo=jfed_rece
      integer,parameter::ko=kfed_rece


!出力空間の設定
      is=lpml
      ie=nx-lpml
      ks=lpml
      ke=nz-lpml
      if(n == 1) open(02,file='eztm2.txt')
      write(02,111)t,ez(io,jo,ko)                   !観測点の過渡電界
      if(n==160)then
         open(03,file="ez160h.txt")
         do k=ks,ke
            do i=is,ie
               write(03,222)i-lpml,k-lpml,ez(i,jfed,k) !電界の空間分布
            end do
         end do
         close(03)
      end if
      if(n == nstep) close(02)
  111 format(2e18.9)
  222 format(2i5,e15.5)
      end subroutine out_emf 

!---------------------------------------------------------------------------
!    　全電界をステップごとに記録
!---------------------------------------------------------------------------
      subroutine ef_all_out(n)
      use fdtd_variable           
      character filename*128,tmp*128

!出力空間の設定

      write(tmp, '(i4.4)')n
      filename = "result/efield_all/romennasi/ez" 
     &// trim( adjustl(tmp) ) // '.txt'

      !print *,filename
      if(n.ge.1)then
         open(03,file=filename, status='replace')
         do i=0,nx
            do k=0,nz
               write(03,222)i,k,ez(i,jfed,k) !電界の空間分布
            end do
            write(03,*)
          end do
         end if
      if(n==nstep) close(03)
  111 format(2e18.9)
  222 format(2i5,e15.6)
      end subroutine ef_all_out 
!---------------------------------------------------------------------------
!    　アンテナ送信部をステップごとに記録
!---------------------------------------------------------------------------
      subroutine ef_sou_out(n)
      use fdtd_variable           
      character filename*128,tmp*128

!出力空間の設定

      write(tmp, '(i4.4)')n
      filename = "result/efield_sou_out/ez" 
     &// trim( adjustl(tmp) ) // '.txt'

      !print *,filename
      if(n.ge.1)then
         open(03,file=filename, status='replace')
         do i=10,1000
            do k=250,279
               write(03,222)i,k,ez(i,jfed,k) !電界の空間分布
            end do
            write(03,*)
          end do
         end if
         if(n==nstep) close(03)
  111 format(2e18.9)
  222 format(2i5,e15.6)
      end subroutine ef_sou_out 
!---------------------------------------------------------------------------
!    　アンテナ受信部をステップごとに記録
!---------------------------------------------------------------------------
      subroutine ef_rece_out(n)
      use fdtd_variable           
      character filename*128,tmp*128

!出力空間の設定

      write(tmp, '(i4.4)')n
      filename = "result/efield_rece_out/ez" 
     &// trim( adjustl(tmp) ) // '.txt'

      !print *,filename
      if(n.ge.1)then
         open(03,file=filename, status='replace')
         do i=2000,3158
            do k=0,279
               write(03,222)i,k,ez(i,jfed_rece,k) !電界の空間分布
            end do
            write(03,*)
          end do
         end if
         if(n==nstep) close(03)
  111 format(2e18.9)
  222 format(2i5,e15.6)
      end subroutine ef_rece_out 
!-------------------------------------------
!     媒質IDの分布出力(ide_xz)
!-------------------------------------------
      subroutine ide_out_xz
      use fdtd_variable
        open(4141,file='fort.4141', status = 'replace')
        do i=0, nx
          do k=0, nz
            write(4141,*)i, k, idez(i, jfed, k)
          end do
          write(4141, *)
        end do
        close(4141)
      end subroutine ide_out_xz
!-------------------------------------------
!     媒質IDの分布出力(ide_xy)
!-------------------------------------------
      subroutine ide_out_xy
      use fdtd_variable
        open(4142,file='fort.4142', status = 'replace')
        do i=0, nx
          do k=0, ny
            write(4142,*)i, k, idey(i, jfed, KSURF-1)
          end do
          write(4142, *)
        end do
        close(4142)
      end subroutine ide_out_xy
!-------------------------------------------
!     媒質IDの分布出力(idh_xz)
!-------------------------------------------
      subroutine idh_out_xz
      use fdtd_variable
        open(4143,file='fort.4143', status = 'replace')
        do i=0, nx
          do k=0, nz
            write(4143,*)i, k, idhz(i, jfed, k)
          end do
          write(4143, *)
        end do
        close(4143)
      end subroutine idh_out_xz

!-------------------------------------------
!     媒質IDの分布出力(idh_xy)
!-------------------------------------------
      subroutine idh_out_xy
        use fdtd_variable
          open(4144,file='fort.4144', status = 'replace')
          do i=0, nx
            do j=0, ny
              write(4144,*)i, j, idhy(i, j, KSURF-1)
            end do
            write(4144, *)
          end do
          close(4144)
        end subroutine idh_out_xy

!PMLのinclude
      !include "pml3d.f"
      !include "pml3d2.f"
      include "pml3d3.f"

!---------- ダイポールアンテナ計算のinclude ----------------------------
	  include "dipole.f"                          !ariyama

!電界磁界の初期化
      include "syokika.f"
