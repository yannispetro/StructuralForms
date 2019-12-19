subroutine  objective(x,idim,z,c,odim,cdim,fdat,opt)
!
! *** Deterministic fundamental solution (e.g. FEM)
!
!    real*4   :: x(idim)     ! input vectors of dimension "idim"
!    real*4   :: z(odim)     ! result vectors of dimension "odim"
!    integer  :: idim        ! dimension of input vector x
!    integer  :: odim        ! dimension of result vector z
!
! ***
!
    use datmod
    use optmod

    implicit none

    type(global_data) ::  fdat
    type(opt_data)    ::  opt
!
    integer,intent(in)     :: idim,odim,cdim
    real*4,intent(inout)      :: x(idim)
    real*4,intent(out)        :: z(odim)
    real*4,intent(out)        :: c(cdim)

    integer                :: ff, i, k, node_id, elem_id, sec_id, x_id, mode
    real*4                 :: max_val, g_constr(2*opt%n_constraints)
    real*4                 :: W_ext, M_tot, sigma_max, f_N, K_N

    do i = 1,opt%n_area
        elem_id = int(opt%area(i,1))
        x_id = int(opt%area(i,4))
        fdat%El_prop(elem_id,4) = x(x_id)
    enddo

    do i = 1,opt%n_x_node
        node_id = int(opt%x_node(i,1))
        x_id = int(opt%x_node(i,4))
        fdat%nodes(node_id,2) = x(x_id)
    enddo

    do i = 1,opt%n_y_node
        node_id = int(opt%y_node(i,1))
        x_id = int(opt%y_node(i,4))
        fdat%nodes(node_id,3) = x(x_id)
    enddo
!    fdat%nodes(5,2) = x(1)
!    fdat%nodes(5,3) = x(2)

    fdat%K_total = 0.0
    fdat%M_total = 0.0

    call stiffness_matrix(fdat)

!    if (.not. allocated(fdat%R_sup)) allocate (fdat%R_sup(fdat%dmsn,fdat%dmsn))
!    if (.not. allocated(fdat%ind_V)) allocate (fdat%ind_V(fdat%dmsn))
!    if (.not. allocated(fdat%V_incident)) allocate (fdat%V_incident(fdat%dmsn,fdat%dmsn))
    fdat%R_sup = 0.0
    fdat%ind_V = 0
    fdat%V_incident = 0

    call modification_matrices(fdat)

!    if (.not. allocated(fdat%K_m)) allocate (fdat%K_m(fdat%dmsn,fdat%dmsn))
!    if (.not. allocated(fdat%K_mm)) allocate (fdat%K_mm(fdat%dmsn,fdat%dmsn))
!
!    if (.not. allocated(fdat%M_mm)) allocate (fdat%M_mm(fdat%dmsn,fdat%dmsn))
    fdat%K_m = 0.0
    fdat%K_mm = 0.0
    fdat%M_mm = 0.0

    if (sum(fdat%BCs(:,5)) .NE. 0) then
        fdat%K_m = matmul(matmul(fdat%R_sup,fdat%K_total),transpose(fdat%R_sup))
    else
        fdat%K_m = fdat%K_total
    end if

    fdat%K_mm = matmul(matmul(real(fdat%V_incident),fdat%K_m),transpose(real(fdat%V_incident)))

    fdat%M_mm = matmul(matmul(real(fdat%V_incident),fdat%M_total),transpose(real(fdat%V_incident)))
!
!    if (.not. allocated(fdat%D_nodal)) allocate (fdat%D_nodal(fdat%dmsn))
!    if (.not. allocated(fdat%P_nodal)) allocate (fdat%P_nodal(fdat%dmsn))
!
!    if (.not. allocated(fdat%P_ele)) allocate (fdat%P_ele(fdat%n_elements,6))
    fdat%D_nodal = 0.0
    fdat%P_nodal = 0.0

    ff = 0
    call solver(fdat,ff)

!    if (.not. allocated(fdat%F_all)) allocate (fdat%F_all(fdat%n_elements,6))
    fdat%F_all = 0.0

!    if (.not. allocated(fdat%el_diag)) allocate (fdat%el_diag(fdat%acc,5*fdat%n_elements))
!    if (.not. allocated(fdat%eigen_v_diag)) allocate (fdat%eigen_v_diag(fdat%acc*fdat%dmsn,fdat%n_elements))

    M_tot = 0.0
    do i = 1,fdat%n_elements
        M_tot = M_tot + fdat%El_Prop(i,2)*fdat%El_Prop(i,4)*fdat%El_Prop(i,5)
    enddo

    call eigen(fdat)
    f_N = fdat%eigen_f(1)

    call element_forces(fdat)

    sigma_max = 0
    do i = 1,fdat%n_elements
        if (abs(fdat%F_all(i)/fdat%El_Prop(i,4)).GT.sigma_max) then
            sigma_max = abs(fdat%F_all(i)/fdat%El_Prop(i,4))
        endif
    enddo

    W_ext = dot_product(fdat%Ext_for,fdat%D_nodal)
!    sigma_max = 100
!    f_N = 2

    k = 0
    g_constr(:) = 0.0
    do i = 1,opt%n_constraints
        if (opt%constraints(i,1).EQ.1) then
            g_constr(2*k+1) = opt%constraints(i,2) - W_ext
            g_constr(2*k+2) = W_ext - opt%constraints(i,3)
        elseif (opt%constraints(i,1).EQ.2) then
            g_constr(2*k+1) = opt%constraints(i,2) - M_tot
            g_constr(2*k+2) = M_tot - opt%constraints(i,3)
        elseif (opt%constraints(i,1).EQ.3) then
            g_constr(2*k+1) = opt%constraints(i,2) - sigma_max
            g_constr(2*k+2) = sigma_max - opt%constraints(i,3)
        elseif (int(opt%constraints(i,1)/10).EQ.5) then
            mode = nint(mod(opt%constraints(i,1)/10.,real(int(opt%constraints(i,1)/10)))*10)
            f_N = fdat%eigen_f(mode)
            g_constr(2*k+1) = opt%constraints(i,2) - f_N
            g_constr(2*k+2) = f_N - opt%constraints(i,3)
        elseif (int(opt%constraints(i,1)/10).EQ.8) then
            mode = nint(mod(opt%constraints(i,1)/10.,real(int(opt%constraints(i,1)/10)))*10)
            K_N = dot_product(matmul(fdat%eigen_v(:,mode),fdat%K_total(:,:)),fdat%eigen_v(:,mode))
            g_constr(2*k+1) = opt%constraints(i,2) - K_N
            g_constr(2*k+2) = K_N - opt%constraints(i,3)
        endif
        k = k + 1
    enddo

!    write(*,*)dot_product(matmul(fdat%eigen_v(:,1),fdat%K_total(:,:)),fdat%eigen_v(:,1)),K_N

    z(1) = W_ext
    do i = 1,2*opt%n_constraints
        if (g_constr(i).LT.0) then
            g_constr(i) = 0.0
        end if
        z(1) = z(1) + opt%rho_p*g_constr(i)**2
    enddo
    do i = 1,opt%n_constraints
        if (opt%constraints(i,1).EQ.1) then
            c(i) = W_ext
        elseif (opt%constraints(i,1).EQ.2) then
            c(i) = M_tot
        elseif (opt%constraints(i,1).EQ.3) then
            c(i) = sigma_max
        elseif (int(opt%constraints(i,1)/10).EQ.5) then
            mode = nint(mod(opt%constraints(i,1)/10.,real(int(opt%constraints(i,1)/10)))*10)
            c(i) = fdat%eigen_f(mode)
        elseif (int(opt%constraints(i,1)/10).EQ.8) then
            mode = nint(mod(opt%constraints(i,1)/10.,real(int(opt%constraints(i,1)/10)))*10)
            c(i) = dot_product(matmul(fdat%eigen_v(:,mode),fdat%K_total(:,:)),fdat%eigen_v(:,mode))
        endif
    enddo

end subroutine objective
