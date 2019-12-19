module optmod
!
type :: opt_data

    integer :: item2(7), object, dim_obj, n_area, n_x_node, n_y_node, n_constraints, n_varopt,Nvar
    real*4  :: rho_p

    real*4, allocatable    :: area(:,:), x_node(:,:), y_node(:,:)
    real*4, allocatable    :: opt_par(:), constraints_tmp(:,:), constraints(:,:), varopt(:,:)

end type opt_data

end module optmod
