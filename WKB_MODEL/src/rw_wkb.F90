!>@file   rw_wkb.F90
!>@brief Wave outputs
!>@author R. Benshila (CNRS)
!>@version 0.0001

MODULE rw_wkb

   USE netcdf
   USE par_wkb
   USE wkbmod
   USE wkbcdf
    
   IMPLICIT NONE
   PRIVATE
   
   PUBLIC :: wkb_wri, wkb_read_ini, wkb_read_bry
   
   LOGICAL :: file_exist=.FALSE.
   INTEGER :: time_out
   LOGICAL, PUBLIC ::   &
   &  ln_w_epb  ,   &
   &  ln_w_wac  ,   &
   &  ln_w_war  ,   &
   &  ln_w_wkx  ,   &
   &  ln_w_wky  ,   &
   &  ln_w_cg   ,   &
   &  ln_w_zeta ,   &
   &  ln_w_hrm  ,   &
   &  ln_w_wvn  ,   &
   &  ln_w_wcr  ,   &
   &  ln_w_wsr  ,   & 
   &  ln_w_frq  ,   & 
   &  ln_w_wfc  
   

CONTAINS

   !> @brief Write wkb outputs
   
   SUBROUTINE wkb_wri
      INTEGER :: status, ncid, ji
      CHARACTER(len=lc),DIMENSION(3) :: dimnames
      CHARACTER(lc) :: clname
      REAL(kind=8), DIMENSION(jpi,jpj,1) :: ztab
  
      clname=TRIM(cn_dirout)//TRIM(cn_fileout)
      
      IF(.NOT. file_exist)THEN
         status = nf90_create(clname,NF90_WRITE,ncid)
         status = nf90_close(ncid)

         dimnames = (/ 'xi_rho      ','eta_rho     ', 'time_counter' /)
         CALL Write_Ncdf_dim(dimnames(1), clname, jpi)
         CALL Write_Ncdf_dim(dimnames(2), clname, jpj)
         CALL Write_Ncdf_dim(dimnames(3), clname, 0)
         CALL Write_Ncdf_var('h'  , dimnames(1:2), clname, h,'float')

         time_out=1
         file_exist=.true.
       
         IF (ln_read_dx) THEN
            CALL Write_Ncdf_var('xi_rho' , dimnames(1),clname, xi_rho, 'float')
            CALL Write_Ncdf_var('eta_rho', dimnames(2),clname, eta_rho, 'float')
#ifdef DEBUG
            CALL Write_Ncdf_var('om_r', dimnames(1:2),clname, om_r, 'float')
            CALL Write_Ncdf_var('on_r', dimnames(1:2),clname, on_r, 'float')
            CALL Write_Ncdf_var('om_v', dimnames(1:2),clname, om_v, 'float')
            CALL Write_Ncdf_var('on_u', dimnames(1:2),clname, on_u, 'float')
#endif
         ELSE 
            CALL Write_Ncdf_var('xi_rho' , dimnames(1),clname, (/ (ji*rdx , ji=1,jpi)/), 'float')
            CALL Write_Ncdf_var('eta_rho', dimnames(2),clname, (/ (ji*rdx , ji=1,jpj)/), 'float')
         ENDIF
      ENDIF

      CALL Write_Ncdf_var('time_counter', (/dimnames(3)/),clname, (/real(nbstp)/),time_out,'float')
      !CALL Write_Ncdf_var('h'  , dimnames, clname, h  (:,:), 'double')
      IF( ln_w_epb) &
      &   CALL Write_Ncdf_var('epb', dimnames, clname, wsb(:,:,wnew), time_out, 'float')
      IF( ln_w_wac) &
      &   CALL Write_Ncdf_var('wac', dimnames, clname, wac(:,:,wnew), time_out, 'float')
      IF (ln_w_war) &
      &   CALL Write_Ncdf_var('war', dimnames, clname, war(:,:,wnew), time_out, 'float')
      IF( ln_w_wkx) &
      &   CALL Write_Ncdf_var('wkx', dimnames, clname, wkx(:,:,wnew), time_out, 'float')
      IF( ln_w_wky ) &
      &   CALL Write_Ncdf_var('wky', dimnames, clname, wke(:,:,wnew), time_out, 'float')
      IF( ln_w_cg ) &
      &   CALL Write_Ncdf_var('Cg' , dimnames, clname, wcg(:,:,wnew), time_out, 'float')
      IF( ln_w_zeta ) &
      &   CALL Write_Ncdf_var('zeta',dimnames,clname,zeta(:,:), time_out, 'float')
      IF( ln_w_hrm ) &
      &   CALL Write_Ncdf_var('hrm' , dimnames, clname, hrm(:,:,wnew), time_out, 'float')
      IF( ln_w_wvn ) &
      &   CALL Write_Ncdf_var('wvn' , dimnames, clname, wvn(:,:,wnew), time_out, 'float')
      IF( ln_w_wcr ) &
      &   CALL Write_Ncdf_var('wcr' , dimnames, clname, wcr(:,:,wnew), time_out, 'float')
      IF( ln_w_wsr ) &
      &   CALL Write_Ncdf_var('wsr' , dimnames, clname, wsr(:,:,wnew), time_out, 'float')
      IF( ln_w_frq ) &
      &   CALL Write_Ncdf_var('frq' , dimnames, clname, frq(:,:,wnew), time_out, 'float')
      IF( ln_w_wfc ) &
      &   CALL Write_Ncdf_var('wfc' , dimnames, clname, wfc(:,:,wnew), time_out, 'float')

       time_out=time_out+1
   
   END SUBROUTINE wkb_wri

   !> @brief Read wkb outputs

   SUBROUTINE wkb_read_ini
      CHARACTER(lc) :: clname
      
      clname=TRIM(cn_dirin)//TRIM(cn_filein)
      
      CALL Read_Ncdf_var('h', clname, h(:,:))
      IF(ln_read_dx) THEN
         CALL Read_Ncdf_var('xi_rho', clname,  xi_rho  (:))
         CALL Read_Ncdf_var('eta_rho', clname, eta_rho(:))
      ENDIF

      clname=TRIM(cn_dirin)//TRIM(cn_rstin)
      IF ( ln_rst ) THEN
         CALL Read_Ncdf_var('epb', clname, wsb(:,:,wstp))
         CALL Read_Ncdf_var('wac', clname, wac(:,:,wstp))
         CALL Read_Ncdf_var('war', clname, war(:,:,wstp))
         CALL Read_Ncdf_var('wkx', clname, wkx(:,:,wstp))
         CALL Read_Ncdf_var('wky', clname, wke(:,:,wstp))
         CALL Read_Ncdf_var('Cg',  clname, wcg(:,:,wstp))

         CALL Read_Ncdf_var('hrm' , clname, hrm(:,:,wstp) )
         CALL Read_Ncdf_var('wvn' , clname, wvn(:,:,wstp) )
         CALL Read_Ncdf_var('wcr' , clname, wcr(:,:,wstp) )
         CALL Read_Ncdf_var('wsr' , clname, wsr(:,:,wstp) )
         CALL Read_Ncdf_var('frq' , clname, frq(:,:,wstp) )
         CALL Read_Ncdf_var('wfc' , clname, wfc(:,:,wstp))
      ENDIF   

   END SUBROUTINE wkb_read_ini

   SUBROUTINE wkb_read_bry(kread)
   	INTEGER, INTENT(in) :: kread
      CHARACTER(lc) :: clname
      
      clname=TRIM(cn_dirin)//TRIM(cn_bryin)
        
      IF(ln_brywest) THEN
         CALL Read_Ncdf_var('tide_west'  , clname, hbry_west , kread)
         hbry_west_dt(:,1)=hbry_west(:,1)
         CALL Read_Ncdf_var('period_west', clname, perbry_west, kread)
         CALL Read_Ncdf_var('hs_west'    , clname, hsbry_west, kread)
         CALL Read_Ncdf_var('dir_west'   , clname, dirbry_west, kread)
      ENDIF
      
   END SUBROUTINE wkb_read_bry

END MODULE rw_wkb
