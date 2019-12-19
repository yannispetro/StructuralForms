module datmod
!
type :: global_data

    integer :: dmsn, item(12), n_nodes, n_elements, n_sections, n_materials, n_BCs, n_uknown_P
    integer :: n_forces_n
    integer :: n_modes, n_modes_to_calc

    real, allocatable    :: nodes(:,:), sections(:,:), materials(:,:)
    integer, allocatable :: elements(:,:), BCs(:,:)

    real, allocatable    :: forces_n(:,:)
    real, allocatable    :: parameters(:), El_Prop(:,:)

    real, allocatable    :: K_total(:,:), M_total(:,:)

    integer, allocatable :: V_incident(:,:), ind_V(:)
    real, allocatable    :: R_sup(:,:)

    real, allocatable    :: K_m(:,:), K_mm(:,:), M_mm(:,:)

    real, allocatable    :: D_nodal(:), P_nodal(:)

    real, allocatable    :: F_all(:), Ext_for(:)

    real, allocatable    :: eigen_K(:,:), eigen_M(:,:), eigen_v(:,:), eigen_f(:)

    real, allocatable    :: el_diag(:,:), eigen_v_diag(:,:)


end type global_data

end module datmod
