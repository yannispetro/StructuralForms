subroutine stiffness_matrix(fdat)
    use datmod
    implicit none

    type(global_data)::  fdat

    integer :: i, j, k
    integer :: n1, n2, dm
    real  :: L
    real  :: K_global(4,4),  M_global(4,4)

    dm = int(fdat%dmsn/fdat%n_nodes)

    do i = 1,fdat%n_elements
!        sec = fdat%elements(i,4)
!        mat = int(fdat%sections(sec,3))
!
!        fdat%El_Prop(i,1) = i
!        fdat%El_Prop(i,2) = fdat%materials(mat,2) !ro !7850 # kg/m^3
!        fdat%El_Prop(i,3) = fdat%materials(mat,3) !E  !210000000000  # N/m^2
!        fdat%El_Prop(i,4) = fdat%sections(sec,2)  !A  !a*b # m^2

        n1 = fdat%elements(i,2)
        n2 = fdat%elements(i,3)

        L = ((fdat%nodes(n2,2)-fdat%nodes(n1,2))**2 + (fdat%nodes(n2,3)-fdat%nodes(n1,3))**2)**0.5
        fdat%El_Prop(i,5) = L

        call element_T1(fdat, i, K_global, M_global)

        do j = 1,dm
            do k = 1,dm
                fdat%K_total(dm*(n1-1)+j,dm*(n1-1)+k) = fdat%K_total(dm*(n1-1)+j,dm*(n1-1)+k) + K_global(j,k)
                fdat%M_total(dm*(n1-1)+j,dm*(n1-1)+k) = fdat%M_total(dm*(n1-1)+j,dm*(n1-1)+k) + M_global(j,k)
            end do
        end do
        do j = dm+1,2*dm
            do k = dm+1,2*dm
                fdat%K_total(dm*(n2-1)+j-dm,dm*(n2-1)+k-dm) = fdat%K_total(dm*(n2-1)+j-dm,dm*(n2-1)+k-dm) + K_global(j,k)
                fdat%M_total(dm*(n2-1)+j-dm,dm*(n2-1)+k-dm) = fdat%M_total(dm*(n2-1)+j-dm,dm*(n2-1)+k-dm) + M_global(j,k)
            end do
        end do
        do j = 1,dm
            do k = dm+1,2*dm
                fdat%K_total(dm*(n1-1)+j,dm*(n2-1)+k-dm) = fdat%K_total(dm*(n1-1)+j,dm*(n2-1)+k-dm) + K_global(j,k)
                fdat%M_total(dm*(n1-1)+j,dm*(n2-1)+k-dm) = fdat%M_total(dm*(n1-1)+j,dm*(n2-1)+k-dm) + M_global(j,k)
            end do
        end do
        do j = dm+1,2*dm
            do k = 1,dm
                fdat%K_total(dm*(n2-1)+j-dm,dm*(n1-1)+k) = fdat%K_total(dm*(n2-1)+j-dm,dm*(n1-1)+k) + K_global(j,k)
                fdat%M_total(dm*(n2-1)+j-dm,dm*(n1-1)+k) = fdat%M_total(dm*(n2-1)+j-dm,dm*(n1-1)+k) + M_global(j,k)
            end do
        end do

    end do
    return
end subroutine stiffness_matrix

subroutine element_T1(fdat, el, K_l, M_l)

    use datmod

    implicit none

    type(global_data)::  fdat

    integer :: el, n1, n2
    real  :: ro, E, A, L, c, s, stA, stM, K_l(4,4), M_l(4,4)

    ro    = fdat%El_Prop(el,2)
    E     = fdat%El_Prop(el,3)
    A     = fdat%El_Prop(el,4)
    L     = fdat%El_Prop(el,5)

    n1 = fdat%elements(el,2)
    n2 = fdat%elements(el,3)

    c = (fdat%nodes(n2,2)-fdat%nodes(n1,2))/L
    s = (fdat%nodes(n2,3)-fdat%nodes(n1,3))/L

    stA = E*A/L
    K_l = reshape((/  c**2,  c*s,  -c**2, -c*s,  &
                      c*s,   s**2, -c*s,  -s**2, &
                     -c**2, -c*s,   c**2,  c*s,  &
                     -c*s,  -s**2,  c*s,   s**2  /), shape(K_l))
    K_l = K_l*stA

    stM = ro*A*L/6.0
    M_l = reshape((/ 2*c**2+2*s**2, 0.0              , 2*c**2+2*s**2, 0.0       ,   &
                     0.0            , 2*c**2+2*s**2    , 0.0            , c**2+s**2 ,   &
                     c**2+s**2    , 0.0                , 2*c**2+2*s**2, 0.0         ,   &
                     0.0            , c**2+s**2        , 0.0            , 2*c**2+2*s**2 /), shape(M_l))
    M_l = M_l*stM
!    write (*,*) fdat%n_elements

    return
end subroutine element_T1
