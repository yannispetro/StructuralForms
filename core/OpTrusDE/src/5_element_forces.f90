subroutine element_forces(fdat)

    use datmod

    implicit none

    type(global_data)::  fdat

    integer :: n1, n2, i
    real ::  E, A, L, c, s, stA, T(4,4), D_e(4), D_e_local(4)

    do i = 1, fdat%n_elements

        E     = fdat%El_Prop(i,3)
        A     = fdat%El_Prop(i,4)
        L     = fdat%El_Prop(i,5)

        n1 = fdat%elements(i,2)
        n2 = fdat%elements(i,3)

        c = (fdat%nodes(n2,2)-fdat%nodes(n1,2))/L
        s = (fdat%nodes(n2,3)-fdat%nodes(n1,3))/L

        stA = E*A/L
        D_e = (/ fdat%D_nodal(2*n1-1), fdat%D_nodal(2*n1), &
                 fdat%D_nodal(2*n2-1), fdat%D_nodal(2*n2)  /)

        T = reshape((/ c  , -s  ,  0.0,  0.0,  &
                       s  ,  c  ,  0.0,  0.0,  &
                       0.0,  0.0,  c  , -s  ,  &
                       0.0,  0.0,  s  ,  c    /), shape(T))


        D_e_local = matmul(T,D_e)

        fdat%F_all(i) = stA*(D_e_local(3) - D_e_local(1))
    end do

return
end subroutine element_forces
