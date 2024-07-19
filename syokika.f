      subroutine clear_eh
        use fdtd_variable
        ex(:,:,:)=0.0e0
        ey(:,:,:)=0.0e0
        ez(:,:,:)=0.0e0
        hx(:,:,:)=0.0e0
        hy(:,:,:)=0.0e0
        hz(:,:,:)=0.0e0

        idex(:,:,:)=0
        idey(:,:,:)=0
        idez(:,:,:)=0
        idhx(:,:,:)=0
        idhy(:,:,:)=0
        idhz(:,:,:)=0

        fxn(:,:)=0.0e0
        fxn1(:,:)=0.0e0
      end subroutine clear_eh


      subroutine clear_dip
        use fdtd_variable
        
        sweepf=0.0e0
        v_fp=0.0e0
        sweepn=0.0

      end subroutine clear_dip
