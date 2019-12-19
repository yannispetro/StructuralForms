subroutine static_eigen_analysis(fdat,first,print_out,file_name)

    use datmod
!    use optmod

    implicit none

    type(global_data) ::  fdat
!    type(opt_data)    ::  opt

    integer           :: i,first,print_out, ff
    real*4            :: M_tot, W_ext, sigma_max
    character(len=20) :: file_name

!! ============================================================================
!! =========   CALCULATION OF STIFFNESS AND MASS MATRICES   ===================
!! ============================================================================
    if (first .EQ. 1) then
        if (.not. allocated(fdat%K_total)) allocate (fdat%K_total(fdat%dmsn,fdat%dmsn))
        if (.not. allocated(fdat%M_total)) allocate (fdat%M_total(fdat%dmsn,fdat%dmsn))
    endif
    fdat%K_total = 0.0
    fdat%M_total = 0.0
    call stiffness_matrix(fdat)
!!! ============================================================================
!!! =========   MODIFICATION OF STIFFNESS AND MASS MATRICES   ==================
!!! ============================================================================
    if (first .EQ. 1) then
        if (.not. allocated(fdat%R_sup)) allocate (fdat%R_sup(fdat%dmsn,fdat%dmsn))
        if (.not. allocated(fdat%ind_V)) allocate (fdat%ind_V(fdat%dmsn))
        if (.not. allocated(fdat%V_incident)) allocate (fdat%V_incident(fdat%dmsn,fdat%dmsn))
    endif
    fdat%R_sup = 0.0
    fdat%ind_V = 0
    fdat%V_incident = 0

    call modification_matrices(fdat)

    if (first .EQ. 1) then
        if (.not. allocated(fdat%K_m)) allocate (fdat%K_m(fdat%dmsn,fdat%dmsn))
        if (.not. allocated(fdat%K_mm)) allocate (fdat%K_mm(fdat%dmsn,fdat%dmsn))
        if (.not. allocated(fdat%M_mm)) allocate (fdat%M_mm(fdat%dmsn,fdat%dmsn))
    endif

    if (sum(fdat%BCs(:,5)) .NE. 0) then
       fdat%K_m = matmul(matmul(fdat%R_sup,fdat%K_total),transpose(fdat%R_sup))
    else
       fdat%K_m = fdat%K_total
    end if

    fdat%K_mm = matmul(matmul(real(fdat%V_incident),fdat%K_m),transpose(real(fdat%V_incident)))

    fdat%M_mm = matmul(matmul(real(fdat%V_incident),fdat%M_total),transpose(real(fdat%V_incident)))

!!! ============================================================================
!!! ========================   SOLVE   =========================================
!!! ============================================================================
    if (first .EQ. 1) then
        if (.not. allocated(fdat%D_nodal)) allocate (fdat%D_nodal(fdat%dmsn))
        if (.not. allocated(fdat%P_nodal)) allocate (fdat%P_nodal(fdat%dmsn))
        if (.not. allocated(fdat%Ext_for)) allocate (fdat%Ext_for(fdat%dmsn))
    endif
    ff = 0
    call solver(fdat,ff)
!    write (*,*) fdat%D_nodal
!    write (*,*) fdat%P_nodal
!!! ============================================================================
!!! =================   CALCULATE ELEMENT FORCES   =============================
!!! ============================================================================
    if (first .EQ. 1) then
        if (.not. allocated(fdat%F_all)) allocate (fdat%F_all(fdat%n_elements))
    endif
    call element_forces(fdat)

!    write(*,*) fdat%F_all
!
!    do i = 1, fdat%n_elements
!        write (*,*) fdat%F_all(i,:)
!    enddo
!
!    write (*,*) fdat%K_total
!!! ============================================================================
!!! =================   CALCULATE EIGEN-VALUES/MODES  ==========================
!!! ============================================================================
    if (first .EQ. 1) then
        if (.not. allocated(fdat%eigen_v)) allocate (fdat%eigen_v(fdat%dmsn,fdat%n_modes))
        if (.not. allocated(fdat%eigen_f)) allocate (fdat%eigen_f(fdat%n_modes))
    endif
    call eigen(fdat)

    M_tot = 0.0
    do i = 1,fdat%n_elements
        M_tot = M_tot + fdat%El_Prop(i,2)*fdat%El_Prop(i,4)*fdat%El_Prop(i,5)
    enddo

    sigma_max = 0
    do i = 1,fdat%n_elements
        if (abs(fdat%F_all(i)/fdat%El_Prop(i,4)).GT.sigma_max) then
            sigma_max = abs(fdat%F_all(i)/fdat%El_Prop(i,4))
        endif
    enddo

    W_ext = dot_product(fdat%Ext_for,fdat%D_nodal)

    if (print_out .NE.0) then
        call write_output(fdat,file_name,M_tot, W_ext, sigma_max)
    endif

return
end subroutine static_eigen_analysis
