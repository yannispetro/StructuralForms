program main

    use datmod
    use optmod

    implicit none

    type(global_data) ::  fdat
    type(opt_data)    ::  opt

    real*4,allocatable  :: xbound(:,:), x_opt(:)

    real*4              :: opt_set_r(3), z_opt
    integer             :: opt_set_i(10)

    integer             :: i, x_id, sec, mat, elem_id, node_id
    character(len=20)   :: input_file, initial_out_file, optimum_out_file

    input_file = 'test_input.txt'

    initial_out_file   = 'initial_out.txt'
    optimum_out_file   = 'optimum_out.txt'

! ============================================================================
! ==============   READ INPUT FILE   =========================================
! ============================================================================
    call get_items(input_file,fdat)

    fdat%n_nodes     = fdat%item(2) - fdat%item(1)-1
    fdat%n_elements  = fdat%item(3) - fdat%item(2)-1
    fdat%n_materials = fdat%item(4) - fdat%item(3)-1
    fdat%n_sections  = fdat%item(5) - fdat%item(4)-1
    fdat%n_BCs       = fdat%item(6) - fdat%item(5)-1

    fdat%n_forces_n    = fdat%item(7)  - fdat%item(6) - 1

    opt%n_varopt       = fdat%item(8)  - fdat%item(7) - 1
    opt%n_constraints  = fdat%item(10) - fdat%item(9) - 1

    fdat%dmsn = 2*fdat%n_nodes

    if (.not. allocated(fdat%nodes)) allocate (fdat%nodes(fdat%n_nodes,3))
    if (.not. allocated(fdat%elements)) allocate (fdat%elements(fdat%n_elements,4))
    if (.not. allocated(fdat%sections)) allocate (fdat%sections(fdat%n_sections,3))
    if (.not. allocated(fdat%materials)) allocate (fdat%materials(fdat%n_materials,3))
    if (.not. allocated(fdat%BCs)) allocate (fdat%BCs(fdat%n_BCs,4))

    if (fdat%n_forces_n .NE. 0)then
        if (.not. allocated(fdat%forces_n)) allocate (fdat%forces_n(fdat%n_forces_n,3))
    endif

    if (.not. allocated(opt%constraints_tmp)) allocate (opt%constraints_tmp(opt%n_constraints,3))
    if (.not. allocated(opt%varopt)) allocate (opt%varopt(opt%n_varopt,5))

    if (.not. allocated(fdat%parameters)) allocate (fdat%parameters(12))
    if (.not. allocated(opt%opt_par)) allocate (opt%opt_par(12))

    if (.not. allocated(fdat%El_Prop)) allocate (fdat%El_Prop(fdat%n_elements,5))

    call read_data(input_file, fdat, opt)

!    do i = 1,opt%n_constraints
!        write(*,*)opt%constraints(i,:)
!    enddo

! ============================================================================
! =========   STATIC-EIGEN ANALYSIS   ===================
! ============================================================================
    do i = 1,fdat%n_elements
        sec = fdat%elements(i,4)
        mat = int(fdat%sections(sec,3))

        fdat%El_Prop(i,1) = i
        fdat%El_Prop(i,2) = fdat%materials(mat,2) !ro !7850 # kg/m^3
        fdat%El_Prop(i,3) = fdat%materials(mat,3) !E  !210000000000  # N/m^2
        fdat%El_Prop(i,4) = fdat%sections(sec,2)  !A  !a*b # m^2
    enddo

    call static_eigen_analysis(fdat,1,1,initial_out_file)

!    do i = 1,fdat%n_modes
!        write(*,*) dot_product(matmul(fdat%eigen_v(:,i),fdat%K_total(:,:)),fdat%eigen_v(:,i))
!    enddo
!    write(*,*) '----------'
!    do i = 1,fdat%n_modes
!        write(*,*) fdat%eigen_f(i)
!    enddo

!    i = 78
!    write(*,*) int(i/10),nint(mod(i/10.,real(int(i/10)))*10),'wdf'
!    write(*,*) nint(1.7)

! ============================================================================
! ======================   OPTIMIZE   ========================================
! ============================================================================
    opt_set_i(:) = (/opt%Nvar,           int(opt%opt_par(1)),  int(opt%opt_par(2)), &
                    int(opt%opt_par(6)), int(opt%opt_par(7)),  int(opt%opt_par(8)), &
                    int(opt%opt_par(9)), int(opt%opt_par(10)), int(opt%opt_par(11)), &
                    int(opt%opt_par(12))     /)

    opt_set_r(:) = (/opt%opt_par(3),opt%opt_par(4), opt%opt_par(5)/)

    if (.not. allocated(xbound)) allocate (xbound(opt%Nvar,2))
    if (.not. allocated(x_opt))  allocate (x_opt(opt%Nvar))

    if (opt%n_area.EQ.1)then
        x_id = int(opt%area(1,4))
        xbound(x_id,1) = opt%area(1,2)
        xbound(x_id,2) = opt%area(1,3)
    else if (opt%n_area.GT.1)then
        x_id = int(opt%area(1,4))
        xbound(x_id,1) = opt%area(1,2)
        xbound(x_id,2) = opt%area(1,3)
        do i = 2, opt%n_area
            if (opt%area(i,4).NE.opt%area(i-1,4)) then
                x_id = int(opt%area(i,4))
                xbound(x_id,1) = opt%area(i,2)
                xbound(x_id,2) = opt%area(i,3)
            endif
        enddo
    endif

    if (opt%n_x_node.EQ.1)then
        x_id = int(opt%x_node(1,4))
        xbound(x_id,1) = fdat%nodes(int(opt%x_node(1,1)),2) - opt%x_node(1,2)
        xbound(x_id,2) = fdat%nodes(int(opt%x_node(1,1)),2) + opt%x_node(1,3)
    else if (opt%n_x_node.GT.1)then
        x_id = int(opt%x_node(1,4))
        xbound(x_id,1) = fdat%nodes(int(opt%x_node(1,1)),2) - opt%x_node(1,2)
        xbound(x_id,2) = fdat%nodes(int(opt%x_node(1,1)),2) + opt%x_node(1,3)
        do i = 2, opt%n_x_node
            if (opt%x_node(i,4).NE.opt%x_node(i-1,4)) then
                x_id = int(opt%x_node(i,4))
                xbound(x_id,1) = fdat%nodes(int(opt%x_node(i,1)),2) - opt%x_node(i,2)
                xbound(x_id,2) = fdat%nodes(int(opt%x_node(i,1)),2) + opt%x_node(i,3)
            endif
        enddo
    endif

    if (opt%n_y_node.EQ.1)then
        x_id = int(opt%y_node(1,4))
        xbound(x_id,1) = fdat%nodes(int(opt%y_node(1,1)),3) - opt%y_node(1,2)
        xbound(x_id,2) = fdat%nodes(int(opt%y_node(1,1)),3) + opt%y_node(1,3)
    elseif (opt%n_y_node.GT.1)then
        x_id = int(opt%y_node(1,4))
        xbound(x_id,1) = fdat%nodes(int(opt%y_node(1,1)),3) - opt%y_node(1,2)
        xbound(x_id,2) = fdat%nodes(int(opt%y_node(1,1)),3) + opt%y_node(1,3)
        do i = 2, opt%n_y_node
            if (opt%y_node(i,4).NE.opt%y_node(i-1,4)) then
                x_id = int(opt%y_node(i,4))
                xbound(x_id,1) = fdat%nodes(int(opt%y_node(i,1)),3) - opt%y_node(i,2)
                xbound(x_id,2) = fdat%nodes(int(opt%y_node(i,1)),3) + opt%y_node(i,3)
            endif
        enddo
    endif

    call DE_main(opt,fdat,opt_set_i,opt_set_r,xbound,opt%n_constraints,x_opt,z_opt)
!    call evolu_main(opt,fdat,opt_set_i,opt_set_r,xbound,opt%n_constraints,x_opt,z_opt)

!    write(*,*) 'z',z_opt,'x',x_opt !'c',c_opt,

    do i = 1,opt%n_area
        elem_id = int(opt%area(i,1))
        x_id = int(opt%area(i,4))
        fdat%El_prop(elem_id,4) = x_opt(x_id)
    enddo

    do i = 1,opt%n_x_node
        node_id = int(opt%x_node(i,1))
        x_id = int(opt%x_node(i,4))
        fdat%nodes(node_id,2) = x_opt(x_id)
    enddo

    do i = 1,opt%n_y_node
        node_id = int(opt%y_node(i,1))
        x_id = int(opt%y_node(i,4))
        fdat%nodes(node_id,3) = x_opt(x_id)
    enddo

    write(*,*) 'x_opt:'
!    do i = 1,fdat%dmsn
    do i = 1,opt%Nvar
        write(*,*) int(opt%varopt(i,1)),x_opt(i)
    enddo
    write(*,*) ''
    write(*,*) 'z_opt:'
    write(*,*) z_opt

    call static_eigen_analysis(fdat,0,1,optimum_out_file)
!    do i = 1,fdat%n_modes
!        write(*,*) dot_product(matmul(fdat%eigen_v(:,i),fdat%K_total(:,:)),fdat%eigen_v(:,i))
!    enddo
!    write(*,*) '----------'
!    do i = 1,fdat%n_modes
!        write(*,*) fdat%eigen_f(i)
!    enddo

    open(unit=30, file='optimum_design.txt', ACTION="write", STATUS="replace")
    write (30, *) 'NODES'
    do i=1,fdat%n_nodes
         write (30, *) int(fdat%nodes(i,1)),fdat%nodes(i,2:3)
    end do

    write (30, *) 'ELEMENT CROSS SECTION AREAS'
    do i=1,fdat%n_elements
         write (30, *) int(fdat%EL_prop(i,1)), fdat%EL_prop(i,4)
    end do
    close(30)

    deallocate (fdat%nodes, fdat%sections, fdat%materials)

    if (allocated(fdat%forces_n)) deallocate (fdat%forces_n)

    deallocate (fdat%parameters)
    deallocate (fdat%elements, fdat%BCs, fdat%V_incident, fdat%ind_V)
    deallocate (fdat%K_total, fdat%K_m, fdat%K_mm, fdat%M_mm, fdat%R_sup)
    deallocate (fdat%D_nodal, fdat%P_nodal, fdat%M_total)
    deallocate (fdat%El_Prop, fdat%F_all)
    deallocate (fdat%eigen_K, fdat%eigen_M)
    deallocate (fdat%eigen_v)

    stop
end program main

