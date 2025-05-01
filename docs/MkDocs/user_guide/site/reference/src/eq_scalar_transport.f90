! module eq_scalar_transport_mod ! to check
!   use operations
!   use decomp_2d
!   implicit none

!   private :: Compute_transport_rhs
!   private :: Calculate_transport_fractional_step
!   !public  :: Solve_transport_eq
! contains
! !==========================================================================================================
!   subroutine Calculate_transport_fractional_step(rhs0, rhs1, dm, isub)
!     use parameters_constant_mod
!     use udf_type_mod
!     implicit none
!     type(t_domain), intent(in) :: dm
!     real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)), intent(inout) :: rhs0, rhs1
!     integer,  intent(in) :: isub
!     real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: rhs_dummy


!   ! add explicit terms
!     rhs_dummy(:, :, :) = rhs1(:, :, :)
!     rhs1(:, :, :) = dm%tGamma(isub) * rhs1(:, :, :) + &
!                     dm%tZeta (isub) * rhs0(:, :, :)
!     rhs0(:, :, :) = rhs_dummy(:, :, :)

!   ! times the time step 
!     rhs1(:, :, :) = dm%dt * rhs1(:, :, :)

!     return
!   end subroutine
! !==========================================================================================================
!   subroutine Compute_transport_rhs(fl, tm, dm, isub)
!     use operations
!     use udf_type_mod
!     use thermo_info_mod
!     implicit none
!     type(t_domain), intent(in) :: dm
!     type(t_flow),   intent(in) :: fl
!     type(t_thermo), intent(inout) :: tm
!     integer,        intent(in) :: isub    

!     real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc
!     real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: accc_ypencil
!     real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: accc_zpencil
!     real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: apcc
!     real(WP), dimension( dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3) ) :: accp_ypencil
!     real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: accp_zpencil
!     real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: acpc_ypencil
    
!     real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: gy_ypencil
!     real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: gz_zpencil 

!     real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dppc%xsz(3) ) :: hEnth_pcc
!     real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dppc%xsz(3) ) :: kCond_pcc
!     real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: hEnth_cpc_ypencil
!     real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: hEnth_ccp_zpencil
!     real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: Ttemp_ccc_ypencil
!     real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: ene_rhs_ccc_ypencil
!     real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: kCond_cpc_ypencil
!     real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: kCond_ccp_zpencil
!     real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: Ttemp_ccc_zpencil
!     real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: kCond_zpencil
!     real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: ene_rhs_ccc_zpencil
    
!     real(WP) :: fbcx(2), fbcy(2), fbcz(2)
!     integer  :: ibcx(2), ibcy(2), ibcz(2)
!     integer  :: i
! !==========================================================================================================
! !   preparation
! !==========================================================================================================
!     call transpose_x_to_y(fl%gy,        gy_ypencil,   dm%dcpc)   ! for d(g_y h)/dy
!     call transpose_x_to_y(fl%gz,        accp_ypencil, dm%dccp)   ! intermediate, accp_ypencil = gz_ypencil
!     call transpose_y_to_z(accp_ypencil, gz_zpencil,   dm%dccp)   ! for d(g_z h)/dz
! !----------------------------------------------------------------------------------------------------------
! !    h --> h_pcc
! !      --> h_ypencil --> h_cpc_ypencil
! !                    --> h_zpencil --> h_ccp_zpencil
! !----------------------------------------------------------------------------------------------------------
!     call Get_x_midp_C2P_3D(tm%hEnth,     hEnth_pcc,         dm, dm%iAccuracy, dm%ibcx(:, 5), bm%ftpbcx_4cc(:, :, :)%h) ! for d(g_x h_pcc))/dy
!     call transpose_x_to_y (tm%hEnth,     accc_ypencil, dm%dccc)                     !intermediate, accc_ypencil = hEnth_ypencil
!     call Get_y_midp_C2P_3D(accc_ypencil, hEnth_cpc_ypencil, dm, dm%iAccuracy, dm%ibcy(:, 5), bm%ftpbcy_c4c(:, :, :)%h)! for d(g_y h_cpc)/dy
!     call transpose_y_to_z (accc_ypencil, accc_zpencil, dm%dccc) !intermediate, accc_zpencil = hEnth_zpencil
!     call Get_z_midp_C2P_3D(accc_zpencil, hEnth_ccp_zpencil, dm, dm%iAccuracy, dm%ibcz(:, 5), bm%ftpbcz_cc4(:, :, :)%h) ! for d(g_z h_ccp)/dz
! !----------------------------------------------------------------------------------------------------------
! !    k --> k_pcc
! !      --> k_ypencil --> k_cpc_ypencil
! !                    --> k_zpencil --> k_ccp_zpencil              
! !----------------------------------------------------------------------------------------------------------
!     call Get_x_midp_C2P_3D(tm%kCond,      kCond_pcc,         dm, dm%iAccuracy, dm%ibcx(:, 5), dm%ftpbcx_4cc(:, :, :)%k) ! for d(k_pcc * (dT/dx) )/dx
!     call transpose_x_to_y (tm%kCond,      accc_ypencil, dm%dccc)  ! for k d2(T)/dy^2
!     call Get_y_midp_C2P_3D(accc_ypencil,  kCond_cpc_ypencil, dm, dm%iAccuracy, dm%ibcy(:, 5), dm%ftpbcy_c4c(:, :, :)%k)
!     call transpose_y_to_z (accc_ypencil,  kCond_zpencil, dm%dccc) 
!     call Get_z_midp_C2P_3D(kCond_zpencil, kCond_ccp_zpencil, dm, dm%iAccuracy, dm%ibcz(:, 5), dm%ftpbcz_cc4(:, :, :)%k)
! !----------------------------------------------------------------------------------------------------------
! !    T --> T_ypencil --> T_zpencil
! !----------------------------------------------------------------------------------------------------------
!     call transpose_x_to_y (tm%Ttemp,      Ttemp_ccc_ypencil, dm%dccc)   ! for k d2(T)/dy^2
!     call transpose_y_to_z (Ttemp_ccc_ypencil, Ttemp_ccc_zpencil, dm%dccc)   ! for k d2(T)/dz^2
! !==========================================================================================================
! ! the RHS of energy equation
! ! x-pencil : the RHS terms of energy (derivative) operating in the x direction
! !==========================================================================================================
! !----------------------------------------------------------------------------------------------------------
! ! x-pencil : d (gx * h_pcc) / dx 
! !----------------------------------------------------------------------------------------------------------
!     tm%ene_rhs = ZERO
!     call Get_x_1der_P2C_3D( - fl%gx * hEnth_pcc, accc, dm, dm%iAccuracy, dm%ibcx(:, 1) ) ! accc = -d(gx * h)/dx
!     tm%ene_rhs = tm%ene_rhs + accc
! !----------------------------------------------------------------------------------------------------------
! ! x-pencil : d (T) / dx 
! !----------------------------------------------------------------------------------------------------------
!     do i = 1, 2
!       if (dm%ibcx(i, 5) == IBC_NEUMANN) then
!         ibcx(i) = IBC_INTERIOR
!         fbcx(i) = ZERO
!       else
!         ibcx(i) = dm%ibcx(i, 5)
!         !fbcx(i) = dm%fbcx_var(i, 5)
!       end if
!     end do
!     call Get_x_1der_C2P_3D(tm%tTemp, apcc, dm, ibcx(:), fbcx(:) )
! !----------------------------------------------------------------------------------------------------------
! ! x-pencil : k_pcc * d (T) / dx 
! !----------------------------------------------------------------------------------------------------------
!     apcc = apcc * kCond_pcc
!     if (dm%ibcx_Tm(1) == IBC_NEUMANN) then
!       !apcc(1, :, :) = dm%fbcx_var(1, 5)
!     end if
!     if (dm%ibcx_Tm(2) == IBC_NEUMANN) then
!       !apcc(dm%dpcc%xen(1), :, :) = dm%fbcx_var(2, 5)
!     end if
! !----------------------------------------------------------------------------------------------------------
! ! x-pencil : d ( k_pcc * d (T) / dx ) dx
! !----------------------------------------------------------------------------------------------------------
!     call Get_x_1der_P2C_3D(apcc, accc, dm, dm%iAccuracy, dm%ibcx(:, 5) )

!     tm%ene_rhs = tm%ene_rhs + accc
! !==========================================================================================================
! ! the RHS of energy equation
! ! y-pencil : the RHS terms of energy (derivative) operating in the y direction
! !==========================================================================================================
! !----------------------------------------------------------------------------------------------------------
! ! y-pencil : d (gy * h_cpc) / dy 
! !----------------------------------------------------------------------------------------------------------
!     ene_rhs_ccc_ypencil = ZERO
!     call Get_y_1der_P2C_3D( - gy_ypencil * hEnth_cpc_ypencil, accc_ypencil, dm, dm%iAccuracy, dm%ibcy_qy )
!     ene_rhs_ccc_ypencil = ene_rhs_ccc_ypencil + accc_ypencil
! !----------------------------------------------------------------------------------------------------------
! ! y-pencil : d (T) / dy
! !----------------------------------------------------------------------------------------------------------
!     do i = 1, 2
!       if (dm%ibcy(i, 5) == IBC_NEUMANN) then
!         ibcy(i) = IBC_INTERIOR
!         fbcy(i) = ZERO
!       else
!         ibcy(i) = dm%ibcy(i, 5)
!         !fbcy(i) = dm%fbcy_var(i, 5)
!       end if
!     end do
!     call Get_y_1der_C2P_3D(Ttemp_ccc_ypencil, acpc_ypencil, dm, ibcy(:), fbcy(:) )
! !----------------------------------------------------------------------------------------------------------
! ! y-pencil : k_cpc * d (T) / dy 
! !----------------------------------------------------------------------------------------------------------
!     acpc_ypencil = acpc_ypencil * kCond_cpc_ypencil
!     if (dm%ibcy_Tm(1) == IBC_NEUMANN) then
!       !acpc_ypencil(:, 1, :) = dm%fbcy_var(1, 5)
!     end if
!     if (dm%ibcy_Tm(2) == IBC_NEUMANN) then
!       !acpc_ypencil(:, dm%dcpc%yen(2), :) = dm%fbcy_var(2, 5)
!     end if
! !----------------------------------------------------------------------------------------------------------
! ! y-pencil : d ( k_cpc * d (T) / dy ) dy
! !----------------------------------------------------------------------------------------------------------
!     call Get_y_1der_P2C_3D(acpc_ypencil, accc_ypencil, dm, dm%iAccuracy, dm%ibcy(:, 5) )
!     ene_rhs_ccc_ypencil = ene_rhs_ccc_ypencil + accc_ypencil

!     call transpose_y_to_x(ene_rhs_ccc_ypencil, accc, dm%dccc)
!     tm%ene_rhs = tm%ene_rhs + accc
! !==========================================================================================================
! ! the RHS of energy equation
! ! z-pencil : the RHS terms of energy (derivative) operating in the z direction
! !==========================================================================================================
! !----------------------------------------------------------------------------------------------------------
! ! z-pencil : d (gz * h_ccp) / dz 
! !----------------------------------------------------------------------------------------------------------
!     ene_rhs_ccc_zpencil = ZERO
!     call Get_z_1der_P2C_3D( - gz_zpencil * hEnth_ccp_zpencil, accc_zpencil, dm, dm%iAccuracy, dm%ibcz(:, 3) )
!     ene_rhs_ccc_zpencil = ene_rhs_ccc_zpencil + accc_zpencil
! !----------------------------------------------------------------------------------------------------------
! ! z-pencil : d (T) / dz
! !----------------------------------------------------------------------------------------------------------
!     do i = 1, 2
!       if (dm%ibcz(i, 5) == IBC_NEUMANN) then
!         ibcz(i) = IBC_INTERIOR
!         fbcz(i) = ZERO
!       else
!         ibcz(i) = dm%ibcz(i, 5)
!         !fbcz(i) = dm%fbcz_var(i, 5)
!       end if
!     end do
!     call Get_z_1der_C2P_3D(Ttemp_ccc_zpencil, accp_zpencil, dm, ibcz(:), fbcz(:) )
! !----------------------------------------------------------------------------------------------------------
! ! z-pencil : k_ccp * d (T) / dz 
! !----------------------------------------------------------------------------------------------------------
!     accp_zpencil = accp_zpencil * kCond_ccp_zpencil
!     if (dm%ibcz_Tm(1) == IBC_NEUMANN) then
!       !accp_zpencil(:, 1, :) = dm%fbcz_var(1, 5)
!     end if
!     if (dm%ibcz_Tm(2) == IBC_NEUMANN) then
!       !accp_zpencil(:, :, dm%dccp%zen(3)) = dm%fbcz_var(2, 5)
!     end if
! !----------------------------------------------------------------------------------------------------------
! ! z-pencil : d ( k_ccp * d (T) / dz ) / dz
! !----------------------------------------------------------------------------------------------------------
!     call Get_z_1der_P2C_3D(accp_zpencil, accc_zpencil, dm, dm%iAccuracy, dm%ibcz(:, 5) )
!     ene_rhs_ccc_zpencil = ene_rhs_ccc_zpencil + accc_zpencil

!     call transpose_z_to_y(ene_rhs_ccc_zpencil, ene_rhs_ccc_ypencil, dm%dccc)
!     call transpose_y_to_x(ene_rhs_ccc_ypencil, accc,            dm%dccc)
!     tm%ene_rhs = tm%ene_rhs + accc

! !==========================================================================================================
! ! time approaching
! !==========================================================================================================
!     call Calculate_transport_fractional_step(tm%ene_rhs0, tm%ene_rhs, dm, isub)

!     return
!   end subroutine Compute_transport_rhs
! !==========================================================================================================
! !==========================================================================================================
!   subroutine Solve_transport_eq(fl, tm, dm, isub)
!     use udf_type_mod
!     use thermo_info_mod 
!     use solver_tools_mod
!     implicit none
!     type(t_domain), intent(in)    :: dm
!     type(t_flow),   intent(inout) :: fl
!     type(t_thermo), intent(inout) :: tm
!     integer,        intent(in)    :: isub

! !----------------------------------------------------------------------------------------------------------
! !   calculate rhs of transport equation
! !----------------------------------------------------------------------------------------------------------
!     call Compute_transport_rhs(fl, tm, dm, isub)
! !----------------------------------------------------------------------------------------------------------
! !   update rho * h
! !----------------------------------------------------------------------------------------------------------
!     tm%rhoh = tm%rhoh + tm%ene_rhs
! !----------------------------------------------------------------------------------------------------------
! !   update other properties from rho * h
! !----------------------------------------------------------------------------------------------------------
!     call Update_thermal_properties(fl, tm, dm)
! !----------------------------------------------------------------------------------------------------------
! !   No Need to apply b.c.
! !----------------------------------------------------------------------------------------------------------
!   return
!   end subroutine

! end module eq_energy_mod
