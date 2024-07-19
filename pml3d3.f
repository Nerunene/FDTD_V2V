!-----------------------------------------------------------------------
!     PMLÇÃã§í ïœêî
!-----------------------------------------------------------------------
      module fdtd_pml
        use fdtd_variable 

!*** FOR E FIELD ***
    !-----------------------------------
    !    x0
    !-----------------------------------        
        real::exy_x0(0:lpml,0:ny,0:nz),exz_x0(0:lpml,0:ny,0:nz)
        real::eyz_x0(0:lpml,0:ny,0:nz),eyx_x0(0:lpml,0:ny,0:nz) 
        real::ezx_x0(0:lpml,0:ny,0:nz),ezy_x0(0:lpml,0:ny,0:nz)
        real::aeyx_x0(0:lpml,0:ny,0:nz)
        real::aezx_x0(0:lpml,0:ny,0:nz)
        real::aexy_x0(0:lpml,0:ny,0:nz)
        real::aezy_x0(0:lpml,0:ny,0:nz)
        real::aexz_x0(0:lpml,0:ny,0:nz)
        real::aeyz_x0(0:lpml,0:ny,0:nz)
        real::beyx_x0(0:lpml,0:ny,0:nz)
        real::bezx_x0(0:lpml,0:ny,0:nz)
        real::bexy_x0(0:lpml,0:ny,0:nz)
        real::bezy_x0(0:lpml,0:ny,0:nz)
        real::bexz_x0(0:lpml,0:ny,0:nz)
        real::beyz_x0(0:lpml,0:ny,0:nz)
    !-----------------------------------
    !    x1
    !----------------------------------- 
        real::exy_x1(nx-lpml:nx,0:ny,0:nz),exz_x1(nx-lpml:nx,0:ny,0:nz)
        real::eyz_x1(nx-lpml:nx,0:ny,0:nz),eyx_x1(nx-lpml:nx,0:ny,0:nz)
        real::ezx_x1(nx-lpml:nx,0:ny,0:nz),ezy_x1(nx-lpml:nx,0:ny,0:nz)
        real::aeyx_x1(nx-lpml:nx,0:ny,0:nz)
        real::aezx_x1(nx-lpml:nx,0:ny,0:nz)
        real::aexy_x1(nx-lpml:nx,0:ny,0:nz)
        real::aezy_x1(nx-lpml:nx,0:ny,0:nz)
        real::aexz_x1(nx-lpml:nx,0:ny,0:nz)
        real::aeyz_x1(nx-lpml:nx,0:ny,0:nz)
        real::beyx_x1(nx-lpml:nx,0:ny,0:nz)
        real::bezx_x1(nx-lpml:nx,0:ny,0:nz)
        real::bexy_x1(nx-lpml:nx,0:ny,0:nz)
        real::bezy_x1(nx-lpml:nx,0:ny,0:nz)
        real::bexz_x1(nx-lpml:nx,0:ny,0:nz)
        real::beyz_x1(nx-lpml:nx,0:ny,0:nz)
    !-----------------------------------
    !    y0
    !----------------------------------- 
        real::exy_y0(0:nx,0:lpml,0:nz),exz_y0(0:nx,0:lpml,0:nz)
        real::eyz_y0(0:nx,0:lpml,0:nz),eyx_y0(0:nx,0:lpml,0:nz)
        real::ezx_y0(0:nx,0:lpml,0:nz),ezy_y0(0:nx,0:lpml,0:nz)
        real::aeyx_y0(0:nx,0:lpml,0:nz)
        real::aezx_y0(0:nx,0:lpml,0:nz)
        real::aexy_y0(0:nx,0:lpml,0:nz)
        real::aezy_y0(0:nx,0:lpml,0:nz)
        real::aexz_y0(0:nx,0:lpml,0:nz)
        real::aeyz_y0(0:nx,0:lpml,0:nz)
        real::beyx_y0(0:nx,0:lpml,0:nz)
        real::bezx_y0(0:nx,0:lpml,0:nz)
        real::bexy_y0(0:nx,0:lpml,0:nz)
        real::bezy_y0(0:nx,0:lpml,0:nz)
        real::bexz_y0(0:nx,0:lpml,0:nz)
        real::beyz_y0(0:nx,0:lpml,0:nz)
    !-----------------------------------
    !    y1
    !----------------------------------- 
        real::exy_y1(0:nx,ny-lpml:ny,0:nz),exz_y1(0:nx,ny-lpml:ny,0:nz)
        real::eyz_y1(0:nx,ny-lpml:ny,0:nz),eyx_y1(0:nx,ny-lpml:ny,0:nz)
        real::ezx_y1(0:nx,ny-lpml:ny,0:nz),ezy_y1(0:nx,ny-lpml:ny,0:nz)
        real::aeyx_y1(0:nx,ny-lpml:ny,0:nz)
        real::aezx_y1(0:nx,ny-lpml:ny,0:nz)
        real::aexy_y1(0:nx,ny-lpml:ny,0:nz)
        real::aezy_y1(0:nx,ny-lpml:ny,0:nz)
        real::aexz_y1(0:nx,ny-lpml:ny,0:nz)
        real::aeyz_y1(0:nx,ny-lpml:ny,0:nz)
        real::beyx_y1(0:nx,ny-lpml:ny,0:nz)
        real::bezx_y1(0:nx,ny-lpml:ny,0:nz)
        real::bexy_y1(0:nx,ny-lpml:ny,0:nz)
        real::bezy_y1(0:nx,ny-lpml:ny,0:nz)
        real::bexz_y1(0:nx,ny-lpml:ny,0:nz)
        real::beyz_y1(0:nx,ny-lpml:ny,0:nz)
    !-----------------------------------
    !    z0
    !----------------------------------- 
        real::exy_z0(0:nx,0:ny,0:lpml),exz_z0(0:nx,0:ny,0:lpml)
        real::eyz_z0(0:nx,0:ny,0:lpml),eyx_z0(0:nx,0:ny,0:lpml)
        real::ezx_z0(0:nx,0:ny,0:lpml),ezy_z0(0:nx,0:ny,0:lpml)
        real::aeyx_z0(0:nx,0:ny,0:lpml)
        real::aezx_z0(0:nx,0:ny,0:lpml)
        real::aexy_z0(0:nx,0:ny,0:lpml)
        real::aezy_z0(0:nx,0:ny,0:lpml)
        real::aexz_z0(0:nx,0:ny,0:lpml)
        real::aeyz_z0(0:nx,0:ny,0:lpml)
        real::beyx_z0(0:nx,0:ny,0:lpml)
        real::bezx_z0(0:nx,0:ny,0:lpml)
        real::bexy_z0(0:nx,0:ny,0:lpml)
        real::bezy_z0(0:nx,0:ny,0:lpml)
        real::bexz_z0(0:nx,0:ny,0:lpml)
        real::beyz_z0(0:nx,0:ny,0:lpml)
    !-----------------------------------
    !    z1
    !----------------------------------- 
        real::exy_z1(0:nx,0:ny,nz-lpml:nz),exz_z1(0:nx,0:ny,nz-lpml:nz)
        real::eyz_z1(0:nx,0:ny,nz-lpml:nz),eyx_z1(0:nx,0:ny,nz-lpml:nz)
        real::ezx_z1(0:nx,0:ny,nz-lpml:nz),ezy_z1(0:nx,0:ny,nz-lpml:nz)
        real::aeyx_z1(0:nx,0:ny,nz-lpml:nz)
        real::aezx_z1(0:nx,0:ny,nz-lpml:nz)
        real::aexy_z1(0:nx,0:ny,nz-lpml:nz)
        real::aezy_z1(0:nx,0:ny,nz-lpml:nz)
        real::aexz_z1(0:nx,0:ny,nz-lpml:nz)
        real::aeyz_z1(0:nx,0:ny,nz-lpml:nz)
        real::beyx_z1(0:nx,0:ny,nz-lpml:nz)
        real::bezx_z1(0:nx,0:ny,nz-lpml:nz)
        real::bexy_z1(0:nx,0:ny,nz-lpml:nz)
        real::bezy_z1(0:nx,0:ny,nz-lpml:nz)
        real::bexz_z1(0:nx,0:ny,nz-lpml:nz)
        real::beyz_z1(0:nx,0:ny,nz-lpml:nz)


!*** FOR H FIELD ***
    !-----------------------------------
    !    x0
    !-----------------------------------
        real::hxy_x0(0:lpml,0:ny,0:nz),hxz_x0(0:lpml,0:ny,0:nz)
        real::hyz_x0(0:lpml,0:ny,0:nz),hyx_x0(0:lpml,0:ny,0:nz)
        real::hzx_x0(0:lpml,0:ny,0:nz),hzy_x0(0:lpml,0:ny,0:nz)
        real::amyx_x0(0:lpml,0:ny,0:nz)
        real::amzx_x0(0:lpml,0:ny,0:nz)
        real::amxy_x0(0:lpml,0:ny,0:nz)
        real::amzy_x0(0:lpml,0:ny,0:nz)
        real::amxz_x0(0:lpml,0:ny,0:nz)
        real::amyz_x0(0:lpml,0:ny,0:nz)
        real::bmyx_x0(0:lpml,0:ny,0:nz)
        real::bmzx_x0(0:lpml,0:ny,0:nz)
        real::bmxy_x0(0:lpml,0:ny,0:nz)
        real::bmzy_x0(0:lpml,0:ny,0:nz)
        real::bmxz_x0(0:lpml,0:ny,0:nz)
        real::bmyz_x0(0:lpml,0:ny,0:nz)
    !-----------------------------------
    !    x1
    !-----------------------------------
        real::hxy_x1(nx-lpml:nx,0:ny,0:nz),hxz_x1(nx-lpml:nx,0:ny,0:nz)
        real::hyz_x1(nx-lpml:nx,0:ny,0:nz),hyx_x1(nx-lpml:nx,0:ny,0:nz)
        real::hzx_x1(nx-lpml:nx,0:ny,0:nz),hzy_x1(nx-lpml:nx,0:ny,0:nz)
        real::amyx_x1(nx-lpml:nx,0:ny,0:nz)
        real::amzx_x1(nx-lpml:nx,0:ny,0:nz)
        real::amxy_x1(nx-lpml:nx,0:ny,0:nz)
        real::amzy_x1(nx-lpml:nx,0:ny,0:nz)
        real::amxz_x1(nx-lpml:nx,0:ny,0:nz)
        real::amyz_x1(nx-lpml:nx,0:ny,0:nz)
        real::bmyx_x1(nx-lpml:nx,0:ny,0:nz)
        real::bmzx_x1(nx-lpml:nx,0:ny,0:nz)
        real::bmxy_x1(nx-lpml:nx,0:ny,0:nz)
        real::bmzy_x1(nx-lpml:nx,0:ny,0:nz)
        real::bmxz_x1(nx-lpml:nx,0:ny,0:nz)
        real::bmyz_x1(nx-lpml:nx,0:ny,0:nz)
    !-----------------------------------
    !    y0
    !-----------------------------------
        real::hxy_y0(0:nx,0:lpml,0:nz),hxz_y0(0:nx,0:lpml,0:nz)
        real::hyz_y0(0:nx,0:lpml,0:nz),hyx_y0(0:nx,0:lpml,0:nz)
        real::hzx_y0(0:nx,0:lpml,0:nz),hzy_y0(0:nx,0:lpml,0:nz)
        real::amyx_y0(0:nx,0:lpml,0:nz)
        real::amzx_y0(0:nx,0:lpml,0:nz)
        real::amxy_y0(0:nx,0:lpml,0:nz)
        real::amzy_y0(0:nx,0:lpml,0:nz)
        real::amxz_y0(0:nx,0:lpml,0:nz)
        real::amyz_y0(0:nx,0:lpml,0:nz)
        real::bmyx_y0(0:nx,0:lpml,0:nz)
        real::bmzx_y0(0:nx,0:lpml,0:nz)
        real::bmxy_y0(0:nx,0:lpml,0:nz)
        real::bmzy_y0(0:nx,0:lpml,0:nz)
        real::bmxz_y0(0:nx,0:lpml,0:nz)
        real::bmyz_y0(0:nx,0:lpml,0:nz)
    !-----------------------------------
    !    y1
    !-----------------------------------
        real::hxy_y1(0:nx,ny-lpml:ny,0:nz),hxz_y1(0:nx,ny-lpml:ny,0:nz)
        real::hyz_y1(0:nx,ny-lpml:ny,0:nz),hyx_y1(0:nx,ny-lpml:ny,0:nz)
        real::hzx_y1(0:nx,ny-lpml:ny,0:nz),hzy_y1(0:nx,ny-lpml:ny,0:nz)
        real::amyx_y1(0:nx,ny-lpml:ny,0:nz)
        real::amzx_y1(0:nx,ny-lpml:ny,0:nz)
        real::amxy_y1(0:nx,ny-lpml:ny,0:nz)
        real::amzy_y1(0:nx,ny-lpml:ny,0:nz)
        real::amxz_y1(0:nx,ny-lpml:ny,0:nz)
        real::amyz_y1(0:nx,ny-lpml:ny,0:nz)
        real::bmyx_y1(0:nx,ny-lpml:ny,0:nz)
        real::bmzx_y1(0:nx,ny-lpml:ny,0:nz)
        real::bmxy_y1(0:nx,ny-lpml:ny,0:nz)
        real::bmzy_y1(0:nx,ny-lpml:ny,0:nz)
        real::bmxz_y1(0:nx,ny-lpml:ny,0:nz)
        real::bmyz_y1(0:nx,ny-lpml:ny,0:nz)
    !-----------------------------------
    !    z0
    !-----------------------------------
        real::hxy_z0(0:nx,0:ny,0:lpml),hxz_z0(0:nx,0:ny,0:lpml)
        real::hyz_z0(0:nx,0:ny,0:lpml),hyx_z0(0:nx,0:ny,0:lpml)
        real::hzx_z0(0:nx,0:ny,0:lpml),hzy_z0(0:nx,0:ny,0:lpml)
        real::amyx_z0(0:nx,0:ny,0:lpml)
        real::amzx_z0(0:nx,0:ny,0:lpml)
        real::amxy_z0(0:nx,0:ny,0:lpml)
        real::amzy_z0(0:nx,0:ny,0:lpml)
        real::amxz_z0(0:nx,0:ny,0:lpml)
        real::amyz_z0(0:nx,0:ny,0:lpml)
        real::bmyx_z0(0:nx,0:ny,0:lpml)
        real::bmzx_z0(0:nx,0:ny,0:lpml)
        real::bmxy_z0(0:nx,0:ny,0:lpml)
        real::bmzy_z0(0:nx,0:ny,0:lpml)
        real::bmxz_z0(0:nx,0:ny,0:lpml)
        real::bmyz_z0(0:nx,0:ny,0:lpml)
    !-----------------------------------
    !    z1
    !-----------------------------------
        real::hxy_z1(0:nx,0:ny,nz-lpml:nz),hxz_z1(0:nx,0:ny,nz-lpml:nz)
        real::hyz_z1(0:nx,0:ny,nz-lpml:nz),hyx_z1(0:nx,0:ny,nz-lpml:nz)
        real::hzx_z1(0:nx,0:ny,nz-lpml:nz),hzy_z1(0:nx,0:ny,nz-lpml:nz)
        real::amyx_z1(0:nx,0:ny,nz-lpml:nz)
        real::amzx_z1(0:nx,0:ny,nz-lpml:nz)
        real::amxy_z1(0:nx,0:ny,nz-lpml:nz)
        real::amzy_z1(0:nx,0:ny,nz-lpml:nz)
        real::amxz_z1(0:nx,0:ny,nz-lpml:nz)
        real::amyz_z1(0:nx,0:ny,nz-lpml:nz)
        real::bmyx_z1(0:nx,0:ny,nz-lpml:nz)
        real::bmzx_z1(0:nx,0:ny,nz-lpml:nz)
        real::bmxy_z1(0:nx,0:ny,nz-lpml:nz)
        real::bmzy_z1(0:nx,0:ny,nz-lpml:nz)
        real::bmxz_z1(0:nx,0:ny,nz-lpml:nz)
        real::bmyz_z1(0:nx,0:ny,nz-lpml:nz)
    !-----------------------------------

      real,parameter::copml=-1.5280063e-4
      end module fdtd_pml
!-----------------------------------------------------------------------
!     èâä˙ê›íË
!-----------------------------------------------------------------------
      subroutine init_pml()
        use fdtd_pml
        call addpml_x0()
        call addpml_x1()
        call addpml_y0()
        call addpml_y1()
        call addpml_z0()
        call addpml_z1()
      end subroutine init_pml
!-----------------------------------------------------------------------
!     ìdäEÇ…ëŒÇ∑ÇÈPML
!-----------------------------------------------------------------------
      subroutine e_pml()
        use fdtd_pml
        call epml_x0()
        call epml_x1()
        call epml_y0()
        call epml_y1()
        call epml_z0()
        call epml_z1()
      end subroutine e_pml
!-----------------------------------------------------------------------
!     é•äEÇ…ëŒÇ∑ÇÈPML
!-----------------------------------------------------------------------
      subroutine h_pml()
        use fdtd_pml
        call hpml_x0()
        call hpml_x1()
        call hpml_y0()
        call hpml_y1()
        call hpml_z0()
        call hpml_z1()
      end subroutine h_pml
!-----------------------------------------------------------------------
!     åWêîÇÃåvéZ
!-----------------------------------------------------------------------

!îzóÒÇÃèâä˙âªÇ∆äeñ ÇÃPMLÇÃê›íËÇ∆åWêî

    !-----------------------------------
    !    x0
    !-----------------------------------
      subroutine addpml_x0()
         use fdtd_pml
         real::mupml

      exy_x0=0.0
      exz_x0=0.0
      eyz_x0=0.0
      eyx_x0=0.0
      ezx_x0=0.0
      ezy_x0=0.0
      hxy_x0=0.0
      hxz_x0=0.0
      hyz_x0=0.0
      hyx_x0=0.0
      hzx_x0=0.0
      hzy_x0=0.0

      !îΩéÀåWêîÇÃóvãÅêßìx
      smax0x=copml*rmax*(order+1)/(lpml*dx)
      smax0y=copml*rmax*(order+1)/(lpml*dy)
      smax0z=copml*rmax*(order+1)/(lpml*dz)

      do k=0,nz-1
        do j=0,ny-1
           do i=0,lpml-1


      if(i<lpml) then
         sigmxm=((lpml-i-0.5)/lpml)**order*smax0x
         sigmxe=(float(lpml-i)/lpml)**order*smax0x
      else if(i>=nx-lpml) then
         sigmxm=((i-nx+lpml+0.5)/lpml)**order*smax0x
         sigmxe=(float(i-nx+lpml)/lpml)**order*smax0x
      else                                         
         sigmxm=0.0
         sigmxe=0.0
      end if

      if(j<lpml) then
         sigmym=((lpml-j-0.5)/lpml)**order*smax0y
         sigmye=(float(lpml-j)/lpml)**order*smax0y
      else if(j>=ny-lpml) then
         sigmym=((j-ny+lpml+0.5)/lpml)**order*smax0y
         sigmye=(float(j-ny+lpml)/lpml)**order*smax0y
      else                                         
         sigmym=0.0
         sigmye=0.0
      end if
    
      if(k<lpml) then
         sigmzm=((lpml-k-0.5)/lpml)**order*smax0z
         sigmze=(float(lpml-k)/lpml)**order*smax0z
      else if(k>=nz-lpml) then
         sigmzm=((k-nz+lpml+0.5)/lpml)**order*smax0z
         sigmze=(float(k-nz+lpml)/lpml)**order*smax0z
      else                                         
         sigmzm=0.0
         sigmze=0.0
      end if

    !ExÇÃåvéZ
             epspml=0.25*(epr(idex(i,j,k))+epr(idex(i,j-1,k))
     &              +epr(idex(i,j,k-1))+epr(idex(i,j-1,k-1)))*eps0
             sigmy=sigmye*(epspml/eps0)
             sigmz=sigmze*(epspml/eps0)

             a=0.5*sigmy*dt/epspml
             aexy_x0(i,j,k)=(1.0-a)/(1.0+a)
             bexy_x0(i,j,k)=dt/epspml/(1.0+a)/dy

             a=0.5*sigmz*dt/epspml
             aexz_x0(i,j,k)=(1.0-a)/(1.0+a)
             bexz_x0(i,j,k)=dt/epspml/(1.0+a)/dz

    !EyÇÃåvéZ
             epspml=0.25*(epr(idey(i,j,k))+epr(idey(i-1,j,k))
     &              +epr(idey(i,j,k-1))+epr(idey(i-1,j,k-1)))*eps0
             sigmz=sigmze*(epspml/eps0)
             sigmx=sigmxe*(epspml/eps0)

             a=0.5*sigmz*dt/epspml
             aeyz_x0(i,j,k)=(1.0-a)/(1.0+a)
             beyz_x0(i,j,k)=dt/epspml/(1.0+a)/dz

             a=0.5*sigmx*dt/epspml
             aeyx_x0(i,j,k)=(1.0-a)/(1.0+a)
             beyx_x0(i,j,k)=dt/epspml/(1.0+a)/dx

    !EzÇÃåvéZ
             epspml=0.25*(epr(idez(i,j,k))+epr(idez(i-1,j,k))
     &              +epr(idez(i,j-1,k))+epr(idez(i-1,j-1,k)))*eps0
             sigmx=sigmxe*(epspml/eps0)
             sigmy=sigmye*(epspml/eps0)

             a=0.5*sigmx*dt/epspml
             aezx_x0(i,j,k)=(1.0-a)/(1.0+a)
             bezx_x0(i,j,k)=dt/epspml/(1.0+a)/dx

             a=0.5*sigmy*dt/epspml
             aezy_x0(i,j,k)=(1.0-a)/(1.0+a)
             bezy_x0(i,j,k)=dt/epspml/(1.0+a)/dy

    !HxÇÃåvéZ
             mupml=0.5*(mur(idhx(i,j,k))+mur(idhx(i-1,j,k)))*mu0
             epspml=0.5*(epr(idex(i,j,k))+epr(idex(i-1,j,k)))*eps0
             sigmy=sigmym*(epspml/eps0)
             sigmz=sigmzm*(epspml/eps0)

             a=0.5*sigmy*dt/epspml
             amxy_x0(i,j,k)=(1.0-a)/(1.0+a)
             bmxy_x0(i,j,k)=dt/mupml/(1.0+a)/dy

             a=0.5*sigmz*dt/epspml
             amxz_x0(i,j,k)=(1.0-a)/(1.0+a)
             bmxz_x0(i,j,k)=dt/mupml/(1.0+a)/dz

    !HyÇÃåvéZ
             mupml=0.5*(mur(idhy(i,j,k))+mur(idhy(i,j-1,k)))*mu0
             epspml=0.5*(epr(idey(i,j,k))+epr(idey(i,j-1,k)))*eps0
             sigmz=sigmzm*(epspml/eps0)
             sigmx=sigmxm*(epspml/eps0)

             a=0.5*sigmz*dt/epspml
             amyz_x0(i,j,k)=(1.0-a)/(1.0+a)
             bmyz_x0(i,j,k)=dt/mupml/(1.0+a)/dz

             a=0.5*sigmx*dt/epspml
             amyx_x0(i,j,k)=(1.0-a)/(1.0+a)
             bmyx_x0(i,j,k)=dt/mupml/(1.0+a)/dx

    !HzÇÃåvéZ
             mupml=0.5*(mur(idhz(i,j,k))+mur(idhz(i,j,k-1)))*mu0
             epspml=0.5*(epr(idez(i,j,k))+epr(idez(i,j,k-1)))*eps0
             sigmx=sigmxm*(epspml/eps0)
             sigmy=sigmym*(epspml/eps0)

             a=0.5*sigmx*dt/epspml
             amzx_x0(i,j,k)=(1.0-a)/(1.0+a)
             bmzx_x0(i,j,k)=dt/mupml/(1.0+a)/dx

             a=0.5*sigmy*dt/epspml
             amzy_x0(i,j,k)=(1.0-a)/(1.0+a)
             bmzy_x0(i,j,k)=dt/mupml/(1.0+a)/dy

         end do
        end do
      end do 
      end subroutine addpml_x0


    !-----------------------------------
    !    x1
    !-----------------------------------
      subroutine addpml_x1()
        use fdtd_pml
        real::mupml

      exy_x1=0.0
      exz_x1=0.0
      eyz_x1=0.0
      eyx_x1=0.0
      ezx_x1=0.0
      ezy_x1=0.0
      hxy_x1=0.0
      hxz_x1=0.0
      hyz_x1=0.0
      hyx_x1=0.0
      hzx_x1=0.0
      hzy_x1=0.0

      !îΩéÀåWêîÇÃóvãÅêßìx
      smax0x=copml*rmax*(order+1)/(lpml*dx)
      smax0y=copml*rmax*(order+1)/(lpml*dy)
      smax0z=copml*rmax*(order+1)/(lpml*dz)

      do k=0,nz-1
         do j=0,ny-1
            do i=nx-lpml,nx-1


      if(i<lpml) then
         sigmxm=((lpml-i-0.5)/lpml)**order*smax0x
         sigmxe=(float(lpml-i)/lpml)**order*smax0x
      else if(i>=nx-lpml) then
         sigmxm=((i-nx+lpml+0.5)/lpml)**order*smax0x
         sigmxe=(float(i-nx+lpml)/lpml)**order*smax0x
      else                                         
         sigmxm=0.0
         sigmxe=0.0
      end if

      if(j<lpml) then
         sigmym=((lpml-j-0.5)/lpml)**order*smax0y
         sigmye=(float(lpml-j)/lpml)**order*smax0y
      else if(j>=ny-lpml) then
         sigmym=((j-ny+lpml+0.5)/lpml)**order*smax0y
         sigmye=(float(j-ny+lpml)/lpml)**order*smax0y
      else                                         
         sigmym=0.0
         sigmye=0.0
      end if
   
      if(k<lpml) then
         sigmzm=((lpml-k-0.5)/lpml)**order*smax0z
         sigmze=(float(lpml-k)/lpml)**order*smax0z
      else if(k>=nz-lpml) then
         sigmzm=((k-nz+lpml+0.5)/lpml)**order*smax0z
         sigmze=(float(k-nz+lpml)/lpml)**order*smax0z
      else                                         
         sigmzm=0.0
         sigmze=0.0
      end if
      
    !ExÇÃåvéZ
             epspml=0.25*(epr(idex(i,j,k))+epr(idex(i,j-1,k))
     &              +epr(idex(i,j,k-1))+epr(idex(i,j-1,k-1)))*eps0
             sigmy=sigmye*(epspml/eps0)
             sigmz=sigmze*(epspml/eps0)

             a=0.5*sigmy*dt/epspml
             aexy_x1(i,j,k)=(1.0-a)/(1.0+a)
             bexy_x1(i,j,k)=dt/epspml/(1.0+a)/dy

             a=0.5*sigmz*dt/epspml
             aexz_x1(i,j,k)=(1.0-a)/(1.0+a)
             bexz_x1(i,j,k)=dt/epspml/(1.0+a)/dz

    !EyÇÃåvéZ
             epspml=0.25*(epr(idey(i,j,k))+epr(idey(i-1,j,k))
     &              +epr(idey(i,j,k-1))+epr(idey(i-1,j,k-1)))*eps0
             sigmz=sigmze*(epspml/eps0)
             sigmx=sigmxe*(epspml/eps0)

             a=0.5*sigmz*dt/epspml
             aeyz_x1(i,j,k)=(1.0-a)/(1.0+a)
             beyz_x1(i,j,k)=dt/epspml/(1.0+a)/dz

             a=0.5*sigmx*dt/epspml
             aeyx_x1(i,j,k)=(1.0-a)/(1.0+a)
             beyx_x1(i,j,k)=dt/epspml/(1.0+a)/dx

    !EzÇÃåvéZ
             epspml=0.25*(epr(idez(i,j,k))+epr(idez(i-1,j,k))
     &              +epr(idez(i,j-1,k))+epr(idez(i-1,j-1,k)))*eps0
             sigmx=sigmxe*(epspml/eps0)
             sigmy=sigmye*(epspml/eps0)

             a=0.5*sigmx*dt/epspml
             aezx_x1(i,j,k)=(1.0-a)/(1.0+a)
             bezx_x1(i,j,k)=dt/epspml/(1.0+a)/dx

             a=0.5*sigmy*dt/epspml
             aezy_x1(i,j,k)=(1.0-a)/(1.0+a)
             bezy_x1(i,j,k)=dt/epspml/(1.0+a)/dy

    !HxÇÃåvéZ
             mupml=0.5*(mur(idhx(i,j,k))+mur(idhx(i-1,j,k)))*mu0
             epspml=0.5*(epr(idex(i,j,k))+epr(idex(i-1,j,k)))*eps0
             sigmy=sigmym*(epspml/eps0)
             sigmz=sigmzm*(epspml/eps0)

             a=0.5*sigmy*dt/epspml
             amxy_x1(i,j,k)=(1.0-a)/(1.0+a)
             bmxy_x1(i,j,k)=dt/mupml/(1.0+a)/dy

             a=0.5*sigmz*dt/epspml
             amxz_x1(i,j,k)=(1.0-a)/(1.0+a)
             bmxz_x1(i,j,k)=dt/mupml/(1.0+a)/dz

    !HyÇÃåvéZ
             mupml=0.5*(mur(idhy(i,j,k))+mur(idhy(i,j-1,k)))*mu0
             epspml=0.5*(epr(idey(i,j,k))+epr(idey(i,j-1,k)))*eps0
             sigmz=sigmzm*(epspml/eps0)
             sigmx=sigmxm*(epspml/eps0)

             a=0.5*sigmz*dt/epspml
             amyz_x1(i,j,k)=(1.0-a)/(1.0+a)
             bmyz_x1(i,j,k)=dt/mupml/(1.0+a)/dz

             a=0.5*sigmx*dt/epspml
             amyx_x1(i,j,k)=(1.0-a)/(1.0+a)
             bmyx_x1(i,j,k)=dt/mupml/(1.0+a)/dx

    !HzÇÃåvéZ
             mupml=0.5*(mur(idhz(i,j,k))+mur(idhz(i,j,k-1)))*mu0
             epspml=0.5*(epr(idez(i,j,k))+epr(idez(i,j,k-1)))*eps0
             sigmx=sigmxm*(epspml/eps0)
             sigmy=sigmym*(epspml/eps0)

             a=0.5*sigmx*dt/epspml
             amzx_x1(i,j,k)=(1.0-a)/(1.0+a)
             bmzx_x1(i,j,k)=dt/mupml/(1.0+a)/dx

             a=0.5*sigmy*dt/epspml
             amzy_x1(i,j,k)=(1.0-a)/(1.0+a)
             bmzy_x1(i,j,k)=dt/mupml/(1.0+a)/dy
 
         end do
        end do
      end do 
      end subroutine addpml_x1
    !-----------------------------------
    !    y0
    !-----------------------------------
      subroutine addpml_y0
        use fdtd_pml
        real::mupml

      exy_y0=0.0
      exz_y0=0.0
      eyz_y0=0.0
      eyx_y0=0.0
      ezx_y0=0.0
      ezy_y0=0.0
      hxy_y0=0.0
      hxz_y0=0.0
      hyz_y0=0.0
      hyx_y0=0.0
      hzx_y0=0.0
      hzy_y0=0.0

      !îΩéÀåWêîÇÃóvãÅêßìx
      smax0x=copml*rmax*(order+1)/(lpml*dx)
      smax0y=copml*rmax*(order+1)/(lpml*dy)
      smax0z=copml*rmax*(order+1)/(lpml*dz)


      do k=0,nz-1
         do j=0,lpml-1
            do i=0,nx-1


      if(i<lpml) then
         sigmxm=((lpml-i-0.5)/lpml)**order*smax0x
         sigmxe=(float(lpml-i)/lpml)**order*smax0x
      else if(i>=nx-lpml) then
         sigmxm=((i-nx+lpml+0.5)/lpml)**order*smax0x
         sigmxe=(float(i-nx+lpml)/lpml)**order*smax0x
      else                                         
         sigmxm=0.0
         sigmxe=0.0
      end if
 
      if(j<lpml) then
         sigmym=((lpml-j-0.5)/lpml)**order*smax0y
         sigmye=(float(lpml-j)/lpml)**order*smax0y
      else if(j>=ny-lpml) then
         sigmym=((j-ny+lpml+0.5)/lpml)**order*smax0y
         sigmye=(float(j-ny+lpml)/lpml)**order*smax0y
      else                                         
         sigmym=0.0
         sigmye=0.0
      end if
    
      if(k<lpml) then
         sigmzm=((lpml-k-0.5)/lpml)**order*smax0z
         sigmze=(float(lpml-k)/lpml)**order*smax0z
      else if(k>=nz-lpml) then
         sigmzm=((k-nz+lpml+0.5)/lpml)**order*smax0z
         sigmze=(float(k-nz+lpml)/lpml)**order*smax0z
      else                                         
         sigmzm=0.0
         sigmze=0.0
      end if
      
    !ExÇÃåvéZ
             epspml=0.25*(epr(idex(i,j,k))+epr(idex(i,j-1,k))
     &              +epr(idex(i,j,k-1))+epr(idex(i,j-1,k-1)))*eps0
             sigmy=sigmye*(epspml/eps0)
             sigmz=sigmze*(epspml/eps0)

             a=0.5*sigmy*dt/epspml
             aexy_y0(i,j,k)=(1.0-a)/(1.0+a)
             bexy_y0(i,j,k)=dt/epspml/(1.0+a)/dy

             a=0.5*sigmz*dt/epspml
             aexz_y0(i,j,k)=(1.0-a)/(1.0+a)
             bexz_y0(i,j,k)=dt/epspml/(1.0+a)/dz

    !EyÇÃåvéZ
             epspml=0.25*(epr(idey(i,j,k))+epr(idey(i-1,j,k))
     &              +epr(idey(i,j,k-1))+epr(idey(i-1,j,k-1)))*eps0
             sigmz=sigmze*(epspml/eps0)
             sigmx=sigmxe*(epspml/eps0)

             a=0.5*sigmz*dt/epspml
             aeyz_y0(i,j,k)=(1.0-a)/(1.0+a)
             beyz_y0(i,j,k)=dt/epspml/(1.0+a)/dz

             a=0.5*sigmx*dt/epspml
             aeyx_y0(i,j,k)=(1.0-a)/(1.0+a)
             beyx_y0(i,j,k)=dt/epspml/(1.0+a)/dx

    !EzÇÃåvéZ
             epspml=0.25*(epr(idez(i,j,k))+epr(idez(i-1,j,k))
     &              +epr(idez(i,j-1,k))+epr(idez(i-1,j-1,k)))*eps0
             sigmx=sigmxe*(epspml/eps0)
             sigmy=sigmye*(epspml/eps0)

             a=0.5*sigmx*dt/epspml
             aezx_y0(i,j,k)=(1.0-a)/(1.0+a)
             bezx_y0(i,j,k)=dt/epspml/(1.0+a)/dx

             a=0.5*sigmy*dt/epspml
             aezy_y0(i,j,k)=(1.0-a)/(1.0+a)
             bezy_y0(i,j,k)=dt/epspml/(1.0+a)/dy

    !HxÇÃåvéZ
             mupml=0.5*(mur(idhx(i,j,k))+mur(idhx(i-1,j,k)))*mu0
             epspml=0.5*(epr(idex(i,j,k))+epr(idex(i-1,j,k)))*eps0
             sigmy=sigmym*(epspml/eps0)
             sigmz=sigmzm*(epspml/eps0)

             a=0.5*sigmy*dt/epspml
             amxy_y0(i,j,k)=(1.0-a)/(1.0+a)
             bmxy_y0(i,j,k)=dt/mupml/(1.0+a)/dy

             a=0.5*sigmz*dt/epspml
             amxz_x0(i,j,k)=(1.0-a)/(1.0+a)
             bmxz_x0(i,j,k)=dt/mupml/(1.0+a)/dz

    !HyÇÃåvéZ
             mupml=0.5*(mur(idhy(i,j,k))+mur(idhy(i,j-1,k)))*mu0
             epspml=0.5*(epr(idey(i,j,k))+epr(idey(i,j-1,k)))*eps0
             sigmz=sigmzm*(epspml/eps0)
             sigmx=sigmxm*(epspml/eps0)

             a=0.5*sigmz*dt/epspml
             amyz_y0(i,j,k)=(1.0-a)/(1.0+a)
             bmyz_y0(i,j,k)=dt/mupml/(1.0+a)/dz

             a=0.5*sigmx*dt/epspml
             amyx_y0(i,j,k)=(1.0-a)/(1.0+a)
             bmyx_y0(i,j,k)=dt/mupml/(1.0+a)/dx

    !HzÇÃåvéZ
             mupml=0.5*(mur(idhz(i,j,k))+mur(idhz(i,j,k-1)))*mu0
             epspml=0.5*(epr(idez(i,j,k))+epr(idez(i,j,k-1)))*eps0
             sigmx=sigmxm*(epspml/eps0)
             sigmy=sigmym*(epspml/eps0)

             a=0.5*sigmx*dt/epspml
             amzx_y0(i,j,k)=(1.0-a)/(1.0+a)
             bmzx_y0(i,j,k)=dt/mupml/(1.0+a)/dx

             a=0.5*sigmy*dt/epspml
             amzy_y0(i,j,k)=(1.0-a)/(1.0+a)
             bmzy_y0(i,j,k)=dt/mupml/(1.0+a)/dy
 
         end do
        end do
      end do 
      end subroutine addpml_y0
    !-----------------------------------
    !    y1
    !-----------------------------------
      subroutine addpml_y1
        use fdtd_pml
        real::mupml

      exy_y1=0.0
      exz_y1=0.0
      eyz_y1=0.0
      eyx_y1=0.0
      ezx_y1=0.0
      ezy_y1=0.0
      hxy_y1=0.0
      hxz_y1=0.0
      hyz_y1=0.0
      hyx_y1=0.0
      hzx_y1=0.0
      hzy_y1=0.0

      !îΩéÀåWêîÇÃóvãÅêßìx
      smax0x=copml*rmax*(order+1)/(lpml*dx)
      smax0y=copml*rmax*(order+1)/(lpml*dy)
      smax0z=copml*rmax*(order+1)/(lpml*dz)

 
      do k=0,nz-1
         do j=ny-lpml,ny-1
            do i=0,nx-1
 
 
      if(i<lpml) then
         sigmxm=((lpml-i-0.5)/lpml)**order*smax0x
         sigmxe=(float(lpml-i)/lpml)**order*smax0x
      else if(i>=nx-lpml) then
         sigmxm=((i-nx+lpml+0.5)/lpml)**order*smax0x
         sigmxe=(float(i-nx+lpml)/lpml)**order*smax0x
      else                                         
         sigmxm=0.0
         sigmxe=0.0
      end if
 
      if(j<lpml) then
         sigmym=((lpml-j-0.5)/lpml)**order*smax0y
         sigmye=(float(lpml-j)/lpml)**order*smax0y
      else if(j>=ny-lpml) then
         sigmym=((j-ny+lpml+0.5)/lpml)**order*smax0y
         sigmye=(float(j-ny+lpml)/lpml)**order*smax0y
      else                                         
         sigmym=0.0
         sigmye=0.0
      end if
   
      if(k<lpml) then
         sigmzm=((lpml-k-0.5)/lpml)**order*smax0z
         sigmze=(float(lpml-k)/lpml)**order*smax0z
      else if(k>=nz-lpml) then
         sigmzm=((k-nz+lpml+0.5)/lpml)**order*smax0z
         sigmze=(float(k-nz+lpml)/lpml)**order*smax0z
      else                                         
         sigmzm=0.0
         sigmze=0.0
      end if

    !ExÇÃåvéZ
             epspml=0.25*(epr(idex(i,j,k))+epr(idex(i,j-1,k))
     &              +epr(idex(i,j,k-1))+epr(idex(i,j-1,k-1)))*eps0
             sigmy=sigmye*(epspml/eps0)
             sigmz=sigmze*(epspml/eps0)

             a=0.5*sigmy*dt/epspml
             aexy_y1(i,j,k)=(1.0-a)/(1.0+a)
             bexy_y1(i,j,k)=dt/epspml/(1.0+a)/dy

             a=0.5*sigmz*dt/epspml
             aexz_y1(i,j,k)=(1.0-a)/(1.0+a)
             bexz_y1(i,j,k)=dt/epspml/(1.0+a)/dz

    !EyÇÃåvéZ
             epspml=0.25*(epr(idey(i,j,k))+epr(idey(i-1,j,k))
     &              +epr(idey(i,j,k-1))+epr(idey(i-1,j,k-1)))*eps0
             sigmz=sigmze*(epspml/eps0)
             sigmx=sigmxe*(epspml/eps0)

             a=0.5*sigmz*dt/epspml
             aeyz_y1(i,j,k)=(1.0-a)/(1.0+a)
             beyz_y1(i,j,k)=dt/epspml/(1.0+a)/dz

             a=0.5*sigmx*dt/epspml
             aeyx_y1(i,j,k)=(1.0-a)/(1.0+a)
             beyx_y1(i,j,k)=dt/epspml/(1.0+a)/dx

    !EzÇÃåvéZ
             epspml=0.25*(epr(idez(i,j,k))+epr(idez(i-1,j,k))
     &              +epr(idez(i,j-1,k))+epr(idez(i-1,j-1,k)))*eps0
             sigmx=sigmxe*(epspml/eps0)
             sigmy=sigmye*(epspml/eps0)

             a=0.5*sigmx*dt/epspml
             aezx_y1(i,j,k)=(1.0-a)/(1.0+a)
             bezx_y1(i,j,k)=dt/epspml/(1.0+a)/dx

             a=0.5*sigmy*dt/epspml
             aezy_y1(i,j,k)=(1.0-a)/(1.0+a)
             bezy_y1(i,j,k)=dt/epspml/(1.0+a)/dy

    !HxÇÃåvéZ
             mupml=0.5*(mur(idhx(i,j,k))+mur(idhx(i-1,j,k)))*mu0
             epspml=0.5*(epr(idex(i,j,k))+epr(idex(i-1,j,k)))*eps0
             sigmy=sigmym*(epspml/eps0)
             sigmz=sigmzm*(epspml/eps0)

             a=0.5*sigmy*dt/epspml
             amxy_y1(i,j,k)=(1.0-a)/(1.0+a)
             bmxy_y1(i,j,k)=dt/mupml/(1.0+a)/dy

             a=0.5*sigmz*dt/epspml
             amxz_x1(i,j,k)=(1.0-a)/(1.0+a)
             bmxz_x1(i,j,k)=dt/mupml/(1.0+a)/dz

    !HyÇÃåvéZ
             mupml=0.5*(mur(idhy(i,j,k))+mur(idhy(i,j-1,k)))*mu0
             epspml=0.5*(epr(idey(i,j,k))+epr(idey(i,j-1,k)))*eps0
             sigmz=sigmzm*(epspml/eps0)
             sigmx=sigmxm*(epspml/eps0)

             a=0.5*sigmz*dt/epspml
             amyz_y1(i,j,k)=(1.0-a)/(1.0+a)
             bmyz_y1(i,j,k)=dt/mupml/(1.0+a)/dz

             a=0.5*sigmx*dt/epspml
             amyx_y1(i,j,k)=(1.0-a)/(1.0+a)
             bmyx_y1(i,j,k)=dt/mupml/(1.0+a)/dx

    !HzÇÃåvéZ
             mupml=0.5*(mur(idhz(i,j,k))+mur(idhz(i,j,k-1)))*mu0
             epspml=0.5*(epr(idez(i,j,k))+epr(idez(i,j,k-1)))*eps0
             sigmx=sigmxm*(epspml/eps0)
             sigmy=sigmym*(epspml/eps0)

             a=0.5*sigmx*dt/epspml
             amzx_y1(i,j,k)=(1.0-a)/(1.0+a)
             bmzx_y1(i,j,k)=dt/mupml/(1.0+a)/dx

             a=0.5*sigmy*dt/epspml
             amzy_y1(i,j,k)=(1.0-a)/(1.0+a)
             bmzy_y1(i,j,k)=dt/mupml/(1.0+a)/dy
 
         end do
        end do
      end do 

      end subroutine addpml_y1
    !-----------------------------------
    !    z0
    !-----------------------------------
      subroutine addpml_z0
        use fdtd_pml
        real::mupml

      exy_z0=0.0
      exz_z0=0.0
      eyz_z0=0.0
      eyx_z0=0.0
      ezx_z0=0.0
      ezy_z0=0.0
      hxy_z0=0.0
      hxz_z0=0.0
      hyz_z0=0.0
      hyx_z0=0.0
      hzx_z0=0.0
      hzy_z0=0.0

      !îΩéÀåWêîÇÃóvãÅêßìx
      smax0x=copml*rmax*(order+1)/(lpml*dx)
      smax0y=copml*rmax*(order+1)/(lpml*dy)
      smax0z=copml*rmax*(order+1)/(lpml*dz)

 
      do k=0,lpml-1
         do j=0,ny-1
            do i=0,nx-1
 
 
      if(i<lpml) then
         sigmxm=((lpml-i-0.5)/lpml)**order*smax0x
         sigmxe=(float(lpml-i)/lpml)**order*smax0x
      else if(i>=nx-lpml) then
         sigmxm=((i-nx+lpml+0.5)/lpml)**order*smax0x
         sigmxe=(float(i-nx+lpml)/lpml)**order*smax0x
      else                                         
         sigmxm=0.0
         sigmxe=0.0
      end if
 
      if(j<lpml) then
         sigmym=((lpml-j-0.5)/lpml)**order*smax0y
         sigmye=(float(lpml-j)/lpml)**order*smax0y
      else if(j>=ny-lpml) then
         sigmym=((j-ny+lpml+0.5)/lpml)**order*smax0y
         sigmye=(float(j-ny+lpml)/lpml)**order*smax0y
      else                                         
         sigmym=0.0
         sigmye=0.0
      end if
    
      if(k<lpml) then
         sigmzm=((lpml-k-0.5)/lpml)**order*smax0z
         sigmze=(float(lpml-k)/lpml)**order*smax0z
      else if(k>=nz-lpml) then
         sigmzm=((k-nz+lpml+0.5)/lpml)**order*smax0z
         sigmze=(float(k-nz+lpml)/lpml)**order*smax0z
      else                                         
         sigmzm=0.0
         sigmze=0.0
      end if

    !ExÇÃåvéZ
             epspml=0.25*(epr(idex(i,j,k))+epr(idex(i,j-1,k))
     &              +epr(idex(i,j,k-1))+epr(idex(i,j-1,k-1)))*eps0
             sigmy=sigmye*(epspml/eps0)
             sigmz=sigmze*(epspml/eps0)

             a=0.5*sigmy*dt/epspml
             aexy_z0(i,j,k)=(1.0-a)/(1.0+a)
             bexy_z0(i,j,k)=dt/epspml/(1.0+a)/dy

             a=0.5*sigmz*dt/epspml
             aexz_z0(i,j,k)=(1.0-a)/(1.0+a)
             bexz_z0(i,j,k)=dt/epspml/(1.0+a)/dz

    !EyÇÃåvéZ
             epspml=0.25*(epr(idey(i,j,k))+epr(idey(i-1,j,k))
     &              +epr(idey(i,j,k-1))+epr(idey(i-1,j,k-1)))*eps0
             sigmz=sigmze*(epspml/eps0)
             sigmx=sigmxe*(epspml/eps0)

             a=0.5*sigmz*dt/epspml
             aeyz_z0(i,j,k)=(1.0-a)/(1.0+a)
             beyz_z0(i,j,k)=dt/epspml/(1.0+a)/dz

             a=0.5*sigmx*dt/epspml
             aeyx_z0(i,j,k)=(1.0-a)/(1.0+a)
             beyx_z0(i,j,k)=dt/epspml/(1.0+a)/dx

    !EzÇÃåvéZ
             epspml=0.25*(epr(idez(i,j,k))+epr(idez(i-1,j,k))
     &              +epr(idez(i,j-1,k))+epr(idez(i-1,j-1,k)))*eps0
             sigmx=sigmxe*(epspml/eps0)
             sigmy=sigmye*(epspml/eps0)

             a=0.5*sigmx*dt/epspml
             aezx_z0(i,j,k)=(1.0-a)/(1.0+a)
             bezx_z0(i,j,k)=dt/epspml/(1.0+a)/dx

             a=0.5*sigmy*dt/epspml
             aezy_z0(i,j,k)=(1.0-a)/(1.0+a)
             bezy_z0(i,j,k)=dt/epspml/(1.0+a)/dy

    !HxÇÃåvéZ
             mupml=0.5*(mur(idhx(i,j,k))+mur(idhx(i-1,j,k)))*mu0
             epspml=0.5*(epr(idex(i,j,k))+epr(idex(i-1,j,k)))*eps0
             sigmy=sigmym*(epspml/eps0)
             sigmz=sigmzm*(epspml/eps0)

             a=0.5*sigmy*dt/epspml
             amxy_z0(i,j,k)=(1.0-a)/(1.0+a)
             bmxy_z0(i,j,k)=dt/mupml/(1.0+a)/dy

             a=0.5*sigmz*dt/epspml
             amxz_z0(i,j,k)=(1.0-a)/(1.0+a)
             bmxz_z0(i,j,k)=dt/mupml/(1.0+a)/dz

    !HyÇÃåvéZ
             mupml=0.5*(mur(idhy(i,j,k))+mur(idhy(i,j-1,k)))*mu0
             epspml=0.5*(epr(idey(i,j,k))+epr(idey(i,j-1,k)))*eps0
             sigmz=sigmzm*(epspml/eps0)
             sigmx=sigmxm*(epspml/eps0)

             a=0.5*sigmz*dt/epspml
             amyz_z0(i,j,k)=(1.0-a)/(1.0+a)
             bmyz_z0(i,j,k)=dt/mupml/(1.0+a)/dz

             a=0.5*sigmx*dt/epspml
             amyx_z0(i,j,k)=(1.0-a)/(1.0+a)
             bmyx_z0(i,j,k)=dt/mupml/(1.0+a)/dx

    !HzÇÃåvéZ
             mupml=0.5*(mur(idhz(i,j,k))+mur(idhz(i,j,k-1)))*mu0
             epspml=0.5*(epr(idez(i,j,k))+epr(idez(i,j,k-1)))*eps0
             sigmx=sigmxm*(epspml/eps0)
             sigmy=sigmym*(epspml/eps0)

             a=0.5*sigmx*dt/epspml
             amzx_z0(i,j,k)=(1.0-a)/(1.0+a)
             bmzx_z0(i,j,k)=dt/mupml/(1.0+a)/dx

             a=0.5*sigmy*dt/epspml
             amzy_z0(i,j,k)=(1.0-a)/(1.0+a)
             bmzy_z0(i,j,k)=dt/mupml/(1.0+a)/dy

         end do
        end do
      end do 

      end subroutine addpml_z0
    !-----------------------------------
    !    z1
    !-----------------------------------
      subroutine addpml_z1
        use fdtd_pml
        real::mupml


      exy_z1=0.0
      exz_z1=0.0
      eyz_z1=0.0
      eyx_z1=0.0
      ezx_z1=0.0
      ezy_z1=0.0
      hxy_z1=0.0
      hxz_z1=0.0
      hyz_z1=0.0
      hyx_z1=0.0
      hzx_z1=0.0
      hzy_z1=0.0

      !îΩéÀåWêîÇÃóvãÅêßìx
      smax0x=copml*rmax*(order+1)/(lpml*dx)
      smax0y=copml*rmax*(order+1)/(lpml*dy)
      smax0z=copml*rmax*(order+1)/(lpml*dz)
 
      do k=nz-lpml,nz-1
         do j=0,ny-1
            do i=0,nx-1
 
 
      if(i<lpml) then
         sigmxm=((lpml-i-0.5)/lpml)**order*smax0x
         sigmxe=(float(lpml-i)/lpml)**order*smax0x
      else if(i>=nx-lpml) then
         sigmxm=((i-nx+lpml+0.5)/lpml)**order*smax0x
         sigmxe=(float(i-nx+lpml)/lpml)**order*smax0x
      else                                         
         sigmxm=0.0
         sigmxe=0.0
      end if

      if(j<lpml) then
         sigmym=((lpml-j-0.5)/lpml)**order*smax0y
         sigmye=(float(lpml-j)/lpml)**order*smax0y
      else if(j>=ny-lpml) then
         sigmym=((j-ny+lpml+0.5)/lpml)**order*smax0y
         sigmye=(float(j-ny+lpml)/lpml)**order*smax0y
      else                                         
         sigmym=0.0
         sigmye=0.0
      end if
    
      if(k<lpml) then
         sigmzm=((lpml-k-0.5)/lpml)**order*smax0z
         sigmze=(float(lpml-k)/lpml)**order*smax0z
      else if(k>=nz-lpml) then
         sigmzm=((k-nz+lpml+0.5)/lpml)**order*smax0z
         sigmze=(float(k-nz+lpml)/lpml)**order*smax0z
      else                                         
         sigmzm=0.0
         sigmze=0.0
      end if

    !ExÇÃåvéZ
             epspml=0.25*(epr(idex(i,j,k))+epr(idex(i,j-1,k))
     &              +epr(idex(i,j,k-1))+epr(idex(i,j-1,k-1)))*eps0
             sigmy=sigmye*(epspml/eps0)
             sigmz=sigmze*(epspml/eps0)

             a=0.5*sigmy*dt/epspml
             aexy_z1(i,j,k)=(1.0-a)/(1.0+a)
             bexy_z1(i,j,k)=dt/epspml/(1.0+a)/dy

             a=0.5*sigmz*dt/epspml
             aexz_z1(i,j,k)=(1.0-a)/(1.0+a)
             bexz_z1(i,j,k)=dt/epspml/(1.0+a)/dz

    !EyÇÃåvéZ
             epspml=0.25*(epr(idey(i,j,k))+epr(idey(i-1,j,k))
     &              +epr(idey(i,j,k-1))+epr(idey(i-1,j,k-1)))*eps0
             sigmz=sigmze*(epspml/eps0)
             sigmx=sigmxe*(epspml/eps0)

             a=0.5*sigmz*dt/epspml
             aeyz_z1(i,j,k)=(1.0-a)/(1.0+a)
             beyz_z1(i,j,k)=dt/epspml/(1.0+a)/dz

             a=0.5*sigmx*dt/epspml
             aeyx_z1(i,j,k)=(1.0-a)/(1.0+a)
             beyx_z1(i,j,k)=dt/epspml/(1.0+a)/dx

    !EzÇÃåvéZ
             epspml=0.25*(epr(idez(i,j,k))+epr(idez(i-1,j,k))
     &              +epr(idez(i,j-1,k))+epr(idez(i-1,j-1,k)))*eps0
             sigmx=sigmxe*(epspml/eps0)
             sigmy=sigmye*(epspml/eps0)

             a=0.5*sigmx*dt/epspml
             aezx_z1(i,j,k)=(1.0-a)/(1.0+a)
             bezx_z1(i,j,k)=dt/epspml/(1.0+a)/dx

             a=0.5*sigmy*dt/epspml
             aezy_z1(i,j,k)=(1.0-a)/(1.0+a)
             bezy_z1(i,j,k)=dt/epspml/(1.0+a)/dy

    !HxÇÃåvéZ
             mupml=0.5*(mur(idhx(i,j,k))+mur(idhx(i-1,j,k)))*mu0
             epspml=0.5*(epr(idex(i,j,k))+epr(idex(i-1,j,k)))*eps0
             sigmy=sigmym*(epspml/eps0)
             sigmz=sigmzm*(epspml/eps0)

             a=0.5*sigmy*dt/epspml
             amxy_z1(i,j,k)=(1.0-a)/(1.0+a)
             bmxy_z1(i,j,k)=dt/mupml/(1.0+a)/dy

             a=0.5*sigmz*dt/epspml
             amxz_z1(i,j,k)=(1.0-a)/(1.0+a)
             bmxz_z1(i,j,k)=dt/mupml/(1.0+a)/dz

    !HyÇÃåvéZ
             mupml=0.5*(mur(idhy(i,j,k))+mur(idhy(i,j-1,k)))*mu0
             epspml=0.5*(epr(idey(i,j,k))+epr(idey(i,j-1,k)))*eps0
             sigmz=sigmzm*(epspml/eps0)
             sigmx=sigmxm*(epspml/eps0)

             a=0.5*sigmz*dt/epspml
             amyz_z1(i,j,k)=(1.0-a)/(1.0+a)
             bmyz_z1(i,j,k)=dt/mupml/(1.0+a)/dz

             a=0.5*sigmx*dt/epspml
             amyx_z1(i,j,k)=(1.0-a)/(1.0+a)
             bmyx_z1(i,j,k)=dt/mupml/(1.0+a)/dx

    !HzÇÃåvéZ
             mupml=0.5*(mur(idhz(i,j,k))+mur(idhz(i,j,k-1)))*mu0
             epspml=0.5*(epr(idez(i,j,k))+epr(idez(i,j,k-1)))*eps0
             sigmx=sigmxm*(epspml/eps0)
             sigmy=sigmym*(epspml/eps0)

             a=0.5*sigmx*dt/epspml
             amzx_z1(i,j,k)=(1.0-a)/(1.0+a)
             bmzx_z1(i,j,k)=dt/mupml/(1.0+a)/dx

             a=0.5*sigmy*dt/epspml
             amzy_z1(i,j,k)=(1.0-a)/(1.0+a)
             bmzy_z1(i,j,k)=dt/mupml/(1.0+a)/dy
         end do
        end do
       end do 
    !-----------------------------------
      end subroutine addpml_z1

!-----------------------------------------------------------------------
!     ìdäEÇÃåvéZ
!-----------------------------------------------------------------------
    !-----------------------------------
    !    x0
    !-----------------------------------
      subroutine epml_x0()
        use fdtd_pml
    !Ex
      do k=1,nz-1
        do j=1,ny-1
           do i=0,lpml-1
              exy_x0(i,j,k)=aexy_x0(i,j,k)*exy_x0(i,j,k)
     &               +bexy_x0(i,j,k)*(hz(i,j,k)-hz(i,j-1,k))
              exz_x0(i,j,k)=aexz_x0(i,j,k)*exz_x0(i,j,k)
     &               +bexz_x0(i,j,k)*(hy(i,j,k-1)-hy(i,j,k))
              ex(i,j,k)=exy_x0(i,j,k)+exz_x0(i,j,k)
            end do
         end do
      end do
    !Ey
      do k=1,nz-1
         do j=0,ny-1
            do i=1,lpml-1
              eyz_x0(i,j,k)=aeyz_x0(i,j,k)*eyz_x0(i,j,k)
     &               +beyz_x0(i,j,k)*(hx(i,j,k)-hx(i,j,k-1))
              eyx_x0(i,j,k)=aeyx_x0(i,j,k)*eyx_x0(i,j,k)
     &               +beyx_x0(i,j,k)*(hz(i-1,j,k)-hz(i,j,k))
              ey(i,j,k)=eyz_x0(i,j,k)+eyx_x0(i,j,k)
           end do
         end do
      end do
    !Ez
      do k=0,nz-1
         do j=1,ny-1
            do i=1,lpml-1
               ezx_x0(i,j,k)=aezx_x0(i,j,k)*ezx_x0(i,j,k)
     &               +bezx_x0(i,j,k)*(hy(i,j,k)-hy(i-1,j,k))
               ezy_x0(i,j,k)=aezy_x0(i,j,k)*ezy_x0(i,j,k)
     &               +bezy_x0(i,j,k)*(hx(i,j-1,k)-hx(i,j,k))
               ez(i,j,k)=ezx_x0(i,j,k)+ezy_x0(i,j,k)
            end do
         end do
      end do
      end subroutine epml_x0
    !-----------------------------------
    !    x1
    !-----------------------------------
      subroutine epml_x1()
        use fdtd_pml
    !Ex
      do k=1,nz-1
        do j=1,ny-1
           do i=nx-lpml,nx-1
              exy_x1(i,j,k)=aexy_x1(i,j,k)*exy_x1(i,j,k)
     &               +bexy_x1(i,j,k)*(hz(i,j,k)-hz(i,j-1,k))
              exz_x1(i,j,k)=aexz_x1(i,j,k)*exz_x1(i,j,k)
     &               +bexz_x1(i,j,k)*(hy(i,j,k-1)-hy(i,j,k))
              ex(i,j,k)=exy_x1(i,j,k)+exz_x1(i,j,k)
           end do
        end do
      end do
    !Ey
      do k=1,nz-1
        do j=0,ny-1
           do i=nx-lpml+1,nx-1
              eyz_x1(i,j,k)=aeyz_x1(i,j,k)*eyz_x1(i,j,k)
     &               +beyz_x1(i,j,k)*(hx(i,j,k)-hx(i,j,k-1))
              eyx_x1(i,j,k)=aeyx_x1(i,j,k)*eyx_x1(i,j,k)
     &               +beyx_x1(i,j,k)*(hz(i-1,j,k)-hz(i,j,k))
              ey(i,j,k)=eyz_x1(i,j,k)+eyx_x1(i,j,k)
          end do
        end do
      end do
    !Ez
      do k=0,nz-1
        do j=1,ny-1
           do i=nx-lpml+1,nx-1
              ezx_x1(i,j,k)=aezx_x1(i,j,k)*ezx_x1(i,j,k)
     &               +bezx_x1(i,j,k)*(hy(i,j,k)-hy(i-1,j,k))
              ezy_x1(i,j,k)=aezy_x1(i,j,k)*ezy_x1(i,j,k)
     &               +bezy_x1(i,j,k)*(hx(i,j-1,k)-hx(i,j,k))
              ez(i,j,k)=ezx_x1(i,j,k)+ezy_x1(i,j,k)
           end do
        end do
      end do
      end subroutine epml_x1
    !-----------------------------------
    !    y0
    !-----------------------------------
      subroutine epml_y0()
        use fdtd_pml
    !Ex
      do k=1,nz-1
        do j=1,lpml-1
           do i=0,nx-1
              exy_y0(i,j,k)=aexy_y0(i,j,k)*exy_y0(i,j,k)
     &               +bexy_y0(i,j,k)*(hz(i,j,k)-hz(i,j-1,k))
              exz_y0(i,j,k)=aexz_y0(i,j,k)*exz_y0(i,j,k)
     &               +bexz_y0(i,j,k)*(hy(i,j,k-1)-hy(i,j,k))
              ex(i,j,k)=exy_y0(i,j,k)+exz_y0(i,j,k)
           end do
        end do
      end do
    !Ey
      do k=1,nz-1
        do j=0,lpml-1
           do i=1,nx-1
              eyz_y0(i,j,k)=aeyz_y0(i,j,k)*eyz_y0(i,j,k)
     &               +beyz_y0(i,j,k)*(hx(i,j,k)-hx(i,j,k-1))
              eyx_y0(i,j,k)=aeyx_y0(i,j,k)*eyx_y0(i,j,k)
     &               +beyx_y0(i,j,k)*(hz(i-1,j,k)-hz(i,j,k))
              ey(i,j,k)=eyz_y0(i,j,k)+eyx_y0(i,j,k)
          end do
        end do
      end do
    !Ez
      do k=0,nz-1
        do j=1,lpml-1
           do i=1,nx-1
              ezx_y0(i,j,k)=aezx_y0(i,j,k)*ezx_y0(i,j,k)
     &               +bezx_y0(i,j,k)*(hy(i,j,k)-hy(i-1,j,k))
              ezy_y0(i,j,k)=aezy_y0(i,j,k)*ezy_y0(i,j,k)
     &               +bezy_y0(i,j,k)*(hx(i,j-1,k)-hx(i,j,k))
              ez(i,j,k)=ezx_y0(i,j,k)+ezy_y0(i,j,k)
           end do
        end do
      end do
      end subroutine epml_y0
    !-----------------------------------
    !    y1
    !-----------------------------------     
      subroutine epml_y1()
        use fdtd_pml

    !Ex
      do k=1,nz-1
        do j=ny-lpml+1,ny-1
           do i=0,nx-1
              exy_y1(i,j,k)=aexy_y1(i,j,k)*exy_y1(i,j,k)
     &               +bexy_y1(i,j,k)*(hz(i,j,k)-hz(i,j-1,k))
              exz_y1(i,j,k)=aexz_y1(i,j,k)*exz_y1(i,j,k)
     &               +bexz_y1(i,j,k)*(hy(i,j,k-1)-hy(i,j,k))
              ex(i,j,k)=exy_y1(i,j,k)+exz_y1(i,j,k)
           end do
        end do
      end do
    !Ey
      do k=1,nz-1
        do j=ny-lpml,ny-1
           do i=1,nx-1
              eyz_y1(i,j,k)=aeyz_y1(i,j,k)*eyz_y1(i,j,k)
     &               +beyz_y1(i,j,k)*(hx(i,j,k)-hx(i,j,k-1))
              eyx_y1(i,j,k)=aeyx_y1(i,j,k)*eyx_y1(i,j,k)
     &               +beyx_y1(i,j,k)*(hz(i-1,j,k)-hz(i,j,k))
              ey(i,j,k)=eyz_y1(i,j,k)+eyx_y1(i,j,k)
          end do
       end do
      end do
    !Ez
      do k=0,nz-1
        do j=ny-lpml+1,ny-1
           do i=1,nx-1
              ezx_y1(i,j,k)=aezx_y1(i,j,k)*ezx_y1(i,j,k)
     &               +bezx_y1(i,j,k)*(hy(i,j,k)-hy(i-1,j,k))
              ezy_y1(i,j,k)=aezy_y1(i,j,k)*ezy_y1(i,j,k)
     &               +bezy_y1(i,j,k)*(hx(i,j-1,k)-hx(i,j,k))
              ez(i,j,k)=ezx_y1(i,j,k)+ezy_y1(i,j,k)
           end do
        end do
      end do

      end subroutine epml_y1
    !-----------------------------------
    !    z0
    !-----------------------------------
      subroutine epml_z0()
        use fdtd_pml
    !Ex
      do k=1,lpml-1
        do j=1,ny-1
           do i=0,nx-1
              exy_z0(i,j,k)=aexy_z0(i,j,k)*exy_z0(i,j,k)
     &               +bexy_z0(i,j,k)*(hz(i,j,k)-hz(i,j-1,k))
              exz_z0(i,j,k)=aexz_z0(i,j,k)*exz_z0(i,j,k)
     &               +bexz_z0(i,j,k)*(hy(i,j,k-1)-hy(i,j,k))
              ex(i,j,k)=exy_z0(i,j,k)+exz_z0(i,j,k)
           end do
        end do
      end do
    !Ey
      do k=1,lpml-1
        do j=0,ny-1
           do i=1,nx-1
              eyz_z0(i,j,k)=aeyz_z0(i,j,k)*eyz_z0(i,j,k)
     &               +beyz_z0(i,j,k)*(hx(i,j,k)-hx(i,j,k-1))
              eyx_z0(i,j,k)=aeyx_z0(i,j,k)*eyx_z0(i,j,k)
     &               +beyx_z0(i,j,k)*(hz(i-1,j,k)-hz(i,j,k))
              ey(i,j,k)=eyz_z0(i,j,k)+eyx_z0(i,j,k)
          end do
        end do
      end do
    !Ez
      do k=0,lpml-1
        do j=1,ny-1
           do i=1,nx-1
              ezx_z0(i,j,k)=aezx_z0(i,j,k)*ezx_z0(i,j,k)
     &               +bezx_z0(i,j,k)*(hy(i,j,k)-hy(i-1,j,k))
              ezy_z0(i,j,k)=aezy_z0(i,j,k)*ezy_z0(i,j,k)
     &               +bezy_z0(i,j,k)*(hx(i,j-1,k)-hx(i,j,k))
              ez(i,j,k)=ezx_z0(i,j,k)+ezy_z0(i,j,k)
           end do
        end do
      end do

      end subroutine epml_z0

    !-----------------------------------
    !    z1
    !-----------------------------------
      subroutine epml_z1()
        use fdtd_pml
      
    !Ex
      do k=nz-lpml+1,nz-1
        do j=1,ny-1
           do i=0,nx-1
              exy_z1(i,j,k)=aexy_z1(i,j,k)*exy_z1(i,j,k)
     &               +bexy_z1(i,j,k)*(hz(i,j,k)-hz(i,j-1,k))
              exz_z1(i,j,k)=aexz_z1(i,j,k)*exz_z1(i,j,k)
     &               +bexz_z1(i,j,k)*(hy(i,j,k-1)-hy(i,j,k))
              ex(i,j,k)=exy_z1(i,j,k)+exz_z1(i,j,k)
           end do
        end do
      end do
    !Ey
      do k=nz-lpml+1,nz-1
        do j=0,ny-1
           do i=1,nx-1
              eyz_z1(i,j,k)=aeyz_z1(i,j,k)*eyz_z1(i,j,k)
     &               +beyz_z1(i,j,k)*(hx(i,j,k)-hx(i,j,k-1))
              eyx_z1(i,j,k)=aeyx_z1(i,j,k)*eyx_z1(i,j,k)
     &               +beyx_z1(i,j,k)*(hz(i-1,j,k)-hz(i,j,k))
              ey(i,j,k)=eyz_z1(i,j,k)+eyx_z1(i,j,k)
          end do
        end do
      end do
    !Ez
      do k=nz-lpml,nz-1
        do j=1,ny-1
           do i=1,nx-1
              ezx_z1(i,j,k)=aezx_z1(i,j,k)*ezx_z1(i,j,k)
     &               +bezx_z1(i,j,k)*(hy(i,j,k)-hy(i-1,j,k))
              ezy_z1(i,j,k)=aezy_z1(i,j,k)*ezy_z1(i,j,k)
     &               +bezy_z1(i,j,k)*(hx(i,j-1,k)-hx(i,j,k))
              ez(i,j,k)=ezx_z1(i,j,k)+ezy_z1(i,j,k)
           end do
        end do
      end do
    !-----------------------------------
      end subroutine epml_z1
!-----------------------------------------------------------------------
!    é•äEÇÃåvéZ
!-----------------------------------------------------------------------
    
    !-----------------------------------
    !    x0
    !----------------------------------- 
      subroutine hpml_x0()
        use fdtd_pml
    !Hx
      do k=0,nz-1
        do j=0,ny-1
           do i=1,lpml-1
              hxy_x0(i,j,k)=amxy_x0(i,j,k)*hxy_x0(i,j,k)
     &               +bmxy_x0(i,j,k)*(ez(i,j,k)-ez(i,j+1,k))
              hxz_x0(i,j,k)=amxz_x0(i,j,k)*hxz_x0(i,j,k)
     &               +bmxz_x0(i,j,k)*(ey(i,j,k+1)-ey(i,j,k))
              hx(i,j,k)=hxy_x0(i,j,k)+hxz_x0(i,j,k)
           end do
        end do
      end do
    !Hy
      do k=0,nz-1
        do j=1,ny-1
           do i=0,lpml-1
              hyz_x0(i,j,k)=amyz_x0(i,j,k)*hyz_x0(i,j,k)
     &               +bmyz_x0(i,j,k)*(ex(i,j,k)-ex(i,j,k+1))
              hyx_x0(i,j,k)=amyx_x0(i,j,k)*hyx_x0(i,j,k)
     &               +bmyx_x0(i,j,k)*(ez(i+1,j,k)-ez(i,j,k))
              hy(i,j,k)=hyz_x0(i,j,k)+hyx_x0(i,j,k)
           end do
        end do
      end do
    !Hz
      do k=1,nz-1
        do j=0,ny-1
           do i=0,lpml-1
              hzx_x0(i,j,k)=amzx_x0(i,j,k)*hzx_x0(i,j,k)
     &               +bmzx_x0(i,j,k)*(ey(i,j,k)-ey(i+1,j,k))
              hzy_x0(i,j,k)=amzy_x0(i,j,k)*hzy_x0(i,j,k)
     &               +bmzy_x0(i,j,k)*(ex(i,j+1,k)-ex(i,j,k))
              hz(i,j,k)=hzx_x0(i,j,k)+hzy_x0(i,j,k)
           end do
        end do
      end do 
      end subroutine hpml_x0
    !-----------------------------------
    !    x1
    !----------------------------------- 
      subroutine hpml_x1()
        use fdtd_pml
    !Hx
      do k=0,nz-1
        do j=0,ny-1
           do i=nx-lpml+1,nx-1
              hxy_x1(i,j,k)=amxy_x1(i,j,k)*hxy_x1(i,j,k)
     &               +bmxy_x1(i,j,k)*(ez(i,j,k)-ez(i,j+1,k))
              hxz_x1(i,j,k)=amxz_x1(i,j,k)*hxz_x1(i,j,k)
     &               +bmxz_x1(i,j,k)*(ey(i,j,k+1)-ey(i,j,k))
              hx(i,j,k)=hxy_x1(i,j,k)+hxz_x1(i,j,k)
           end do
        end do
      end do
    !Hy
      do k=0,nz-1
        do j=1,ny-1
           do i=nx-lpml,nx-1
              hyz_x1(i,j,k)=amyz_x1(i,j,k)*hyz_x1(i,j,k)
     &               +bmyz_x1(i,j,k)*(ex(i,j,k)-ex(i,j,k+1))
              hyx_x1(i,j,k)=amyx_x1(i,j,k)*hyx_x1(i,j,k)
     &               +bmyx_x1(i,j,k)*(ez(i+1,j,k)-ez(i,j,k))
              hy(i,j,k)=hyz_x1(i,j,k)+hyx_x1(i,j,k)
           end do
        end do
      end do
    !Hz
      do k=1,nz-1
        do j=0,ny-1
           do i=nx-lpml,nx-1
              hzx_x1(i,j,k)=amzx_x1(i,j,k)*hzx_x1(i,j,k)
     &               +bmzx_x1(i,j,k)*(ey(i,j,k)-ey(i+1,j,k))
              hzy_x1(i,j,k)=amzy_x1(i,j,k)*hzy_x1(i,j,k)
     &               +bmzy_x1(i,j,k)*(ex(i,j+1,k)-ex(i,j,k))
              hz(i,j,k)=hzx_x1(i,j,k)+hzy_x1(i,j,k)
           end do
        end do
      end do  
      end subroutine hpml_x1
    !-----------------------------------
    !    y0
    !----------------------------------- 
      subroutine hpml_y0
        use fdtd_pml
    !Hx
      do k=0,nz-1
        do j=0,lpml-1
           do i=1,nx-1
              hxy_y0(i,j,k)=amxy_y0(i,j,k)*hxy_y0(i,j,k)
     &               +bmxy_y0(i,j,k)*(ez(i,j,k)-ez(i,j+1,k))
              hxz_y0(i,j,k)=amxz_y0(i,j,k)*hxz_y0(i,j,k)
     &               +bmxz_y0(i,j,k)*(ey(i,j,k+1)-ey(i,j,k))
              hx(i,j,k)=hxy_y0(i,j,k)+hxz_y0(i,j,k)
           end do
        end do
      end do
    !Hy
      do k=0,nz-1
        do j=1,lpml-1
           do i=0,nx-1
              hyz_y0(i,j,k)=amyz_y0(i,j,k)*hyz_y0(i,j,k)
     &               +bmyz_y0(i,j,k)*(ex(i,j,k)-ex(i,j,k+1))
              hyx_y0(i,j,k)=amyx_y0(i,j,k)*hyx_y0(i,j,k)
     &               +bmyx_y0(i,j,k)*(ez(i+1,j,k)-ez(i,j,k))
              hy(i,j,k)=hyz_y0(i,j,k)+hyx_y0(i,j,k)
           end do
        end do
      end do
    !Hz
      do k=1,nz-1
        do j=0,lpml-1
           do i=0,nx-1
              hzx_y0(i,j,k)=amzx_y0(i,j,k)*hzx_y0(i,j,k)
     &               +bmzx_y0(i,j,k)*(ey(i,j,k)-ey(i+1,j,k))
              hzy_y0(i,j,k)=amzy_y0(i,j,k)*hzy_y0(i,j,k)
     &               +bmzy_y0(i,j,k)*(ex(i,j+1,k)-ex(i,j,k))
              hz(i,j,k)=hzx_y0(i,j,k)+hzy_y0(i,j,k)
           end do
        end do
      end do  
      end subroutine hpml_y0
    !-----------------------------------
    !    y1
    !-----------------------------------  
      subroutine hpml_y1()
        use fdtd_pml
    !Hx
      do k=0,nz-1
        do j=ny-lpml,ny-1
           do i=1,nx-1
              hxy_y1(i,j,k)=amxy_y1(i,j,k)*hxy_y1(i,j,k)
     &               +bmxy_y1(i,j,k)*(ez(i,j,k)-ez(i,j+1,k))
              hxz_y1(i,j,k)=amxz_y1(i,j,k)*hxz_y1(i,j,k)
     &               +bmxz_y1(i,j,k)*(ey(i,j,k+1)-ey(i,j,k))
              hx(i,j,k)=hxy_y1(i,j,k)+hxz_y1(i,j,k)
           end do
        end do
      end do
    !Hy
      do k=0,nz-1
        do j=ny-lpml+1,ny-1
           do i=0,nx-1
              hyz_y1(i,j,k)=amyz_y1(i,j,k)*hyz_y1(i,j,k)
     &               +bmyz_y1(i,j,k)*(ex(i,j,k)-ex(i,j,k+1))
              hyx_y1(i,j,k)=amyx_y1(i,j,k)*hyx_y1(i,j,k)
     &               +bmyx_y1(i,j,k)*(ez(i+1,j,k)-ez(i,j,k))
              hy(i,j,k)=hyz_y1(i,j,k)+hyx_y1(i,j,k)
           end do
        end do
      end do
    !Hz
      do k=1,nz-1
        do j=ny-lpml,ny-1
           do i=0,nx-1
              hzx_y1(i,j,k)=amzx_y1(i,j,k)*hzx_y1(i,j,k)
     &               +bmzx_y1(i,j,k)*(ey(i,j,k)-ey(i+1,j,k))
              hzy_y1(i,j,k)=amzy_y1(i,j,k)*hzy_y1(i,j,k)
     &               +bmzy_y1(i,j,k)*(ex(i,j+1,k)-ex(i,j,k))
              hz(i,j,k)=hzx_y1(i,j,k)+hzy_y1(i,j,k)
           end do
        end do
      end do 

      end subroutine hpml_y1
    !-----------------------------------
    !    z0
    !----------------------------------- 
      subroutine hpml_z0()
        use fdtd_pml
    !Hx
      do k=0,lpml-1
        do j=0,ny-1
           do i=1,nx-1
              hxy_z0(i,j,k)=amxy_z0(i,j,k)*hxy_z0(i,j,k)
     &               +bmxy_z0(i,j,k)*(ez(i,j,k)-ez(i,j+1,k))
              hxz_z0(i,j,k)=amxz_z0(i,j,k)*hxz_z0(i,j,k)
     &               +bmxz_z0(i,j,k)*(ey(i,j,k+1)-ey(i,j,k))
              hx(i,j,k)=hxy_z0(i,j,k)+hxz_z0(i,j,k)
           end do
        end do
      end do
    !Hy
      do k=0,lpml-1
        do j=1,ny-1
           do i=0,nx-1
              hyz_z0(i,j,k)=amyz_z0(i,j,k)*hyz_z0(i,j,k)
     &               +bmyz_z0(i,j,k)*(ex(i,j,k)-ex(i,j,k+1))
              hyx_z0(i,j,k)=amyx_z0(i,j,k)*hyx_z0(i,j,k)
     &               +bmyx_z0(i,j,k)*(ez(i+1,j,k)-ez(i,j,k))
              hy(i,j,k)=hyz_z0(i,j,k)+hyx_z0(i,j,k)
           end do
        end do
      end do
    !Hz
      do k=1,lpml-1
        do j=0,ny-1
           do i=0,nx-1
              hzx_z0(i,j,k)=amzx_z0(i,j,k)*hzx_z0(i,j,k)
     &               +bmzx_z0(i,j,k)*(ey(i,j,k)-ey(i+1,j,k))
              hzy_z0(i,j,k)=amzy_z0(i,j,k)*hzy_z0(i,j,k)
     &               +bmzy_z0(i,j,k)*(ex(i,j+1,k)-ex(i,j,k))
              hz(i,j,k)=hzx_z0(i,j,k)+hzy_z0(i,j,k)
           end do
        end do
      end do 

      end subroutine hpml_z0
 
    !-----------------------------------
    !    z1
    !-----------------------------------
      subroutine hpml_z1()
        use fdtd_pml

    !Hx
      do k=nz-lpml,nz-1
        do j=0,ny-1
           do i=1,nx-1
              hxy_z1(i,j,k)=amxy_z1(i,j,k)*hxy_z1(i,j,k)
     &               +bmxy_z1(i,j,k)*(ez(i,j,k)-ez(i,j+1,k))
              hxz_z1(i,j,k)=amxz_z1(i,j,k)*hxz_z1(i,j,k)
     &               +bmxz_z1(i,j,k)*(ey(i,j,k+1)-ey(i,j,k))
              hx(i,j,k)=hxy_z1(i,j,k)+hxz_z1(i,j,k)
           end do
        end do
      end do
    !Hy
      do k=nz-lpml,nz-1
        do j=1,ny-1
           do i=0,nx-1
              hyz_z1(i,j,k)=amyz_z1(i,j,k)*hyz_z1(i,j,k)
     &               +bmyz_z1(i,j,k)*(ex(i,j,k)-ex(i,j,k+1))
              hyx_z1(i,j,k)=amyx_z1(i,j,k)*hyx_z1(i,j,k)
     &               +bmyx_z1(i,j,k)*(ez(i+1,j,k)-ez(i,j,k))
              hy(i,j,k)=hyz_z1(i,j,k)+hyx_z1(i,j,k)
           end do
        end do
      end do
    !Hz
      do k=nz-lpml+1,nz-1
        do j=0,ny-1
           do i=0,nx-1
              hzx_z1(i,j,k)=amzx_z1(i,j,k)*hzx_z1(i,j,k)
     &               +bmzx_z1(i,j,k)*(ey(i,j,k)-ey(i+1,j,k))
              hzy_z1(i,j,k)=amzy_z1(i,j,k)*hzy_z1(i,j,k)
     &               +bmzy_z1(i,j,k)*(ex(i,j+1,k)-ex(i,j,k))
              hz(i,j,k)=hzx_z1(i,j,k)+hzy_z1(i,j,k)
           end do
        end do
      end do 
    !-----------------------------------
      end subroutine hpml_z1

      
