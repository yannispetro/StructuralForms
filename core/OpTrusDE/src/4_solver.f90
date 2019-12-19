subroutine solver(fdat,ff)

    use datmod

    implicit none

    type(global_data)::  fdat

    integer :: i, j, k, n1, n2, ss, ff, nn
    real, allocatable  :: K_ff(:,:), K_sf(:,:), K_fs(:,:), K_ss(:,:), K_ff_inv(:,:), M_ff(:,:)
    real, allocatable  :: P(:), P_m(:), P_mm(:), P_f(:), P_s(:), P_ls(:), D_s(:), D_f(:)
    real, allocatable  :: A(:,:), B(:), D_mm(:), D_m(:)


    ff = fdat%dmsn - fdat%n_uknown_P
    ss = fdat%n_uknown_P

    if (.not. allocated(K_ff)) allocate (K_ff(ff,ff))
    if (.not. allocated(K_sf)) allocate (K_sf(ss,ff))
    if (.not. allocated(K_fs)) allocate (K_fs(ff,ss))
    if (.not. allocated(K_ss)) allocate (K_ss(ss,ss))

    if (.not. allocated(M_ff)) allocate (M_ff(ff,ff))

    if (.not. allocated(P)) allocate (P(fdat%dmsn))
    if (.not. allocated(P_m)) allocate (P_m(fdat%dmsn))
    if (.not. allocated(P_mm)) allocate (P_mm(fdat%dmsn))

    if (.not. allocated(P_f)) allocate (P_f(ff))
    if (.not. allocated(P_ls)) allocate (P_ls(ss))

    if (.not. allocated(K_ff_inv)) allocate (K_ff_inv(ff,ff))
    if (.not. allocated(D_s)) allocate (D_s(ss))
    if (.not. allocated(D_f)) allocate (D_f(ff))
    if (.not. allocated(P_s)) allocate (P_s(ss))

    if (.not. allocated(A)) allocate (A(ff,ff))
    if (.not. allocated(B)) allocate (B(ff))

    if (.not. allocated(D_mm)) allocate (D_mm(fdat%dmsn))
    if (.not. allocated(D_m)) allocate (D_m(fdat%dmsn))

    do i = 1,ff
        do j = 1,ff
            K_ff(i,j) = fdat%K_mm(i,j)
            M_ff(i,j) = fdat%M_mm(i,j)
        end do
    end do
    do i = ff+1, fdat%dmsn
        do j = 1,ff
            K_sf(i-ff,j) = fdat%K_mm(i,j)
        end do
    end do
    do i = 1,ff
        do j = ff+1, fdat%dmsn
            K_fs(i,j-ff) = fdat%K_mm(i,j)
        end do
    end do
    do i = ff+1, fdat%dmsn
        do j = ff+1, fdat%dmsn
            K_ss(i-ff,j-ff) = fdat%K_mm(i,j)
        end do
    end do

    P = 0.0

    do i = 1, fdat%n_forces_n
        P(2*int(fdat%forces_n(i,1))-1) = fdat%forces_n(i,2)
        P(2*int(fdat%forces_n(i,1))  ) = fdat%forces_n(i,3)
    end do

    fdat%Ext_for = P

    if (sum(fdat%BCs(:,4)) .NE. 0) then
        P_m = matmul(fdat%R_sup,P)
    else
        P_m = P
    end if

    P_mm = matmul(fdat%V_incident,P_m)

    P_f = P_mm(1:ff)
    P_ls = P_mm(ff+1:size(P_mm))

    !-------SOLVE---------
    D_s = 0.0

    nn = size(K_ff(1,:))

    A = K_ff
    B = P_f - matmul(K_fs,D_s)

    if (fdat%parameters(1) .EQ. 1.0) then
        call gauss_1(A,B,D_f,nn)
    else
        call gauss_1(A,B,D_f,nn)
!        call lu_solve(A, B)
!        D_f = B
    endif

    P_s = matmul(K_sf,D_f) + matmul(K_ss,D_s) - P_ls
    !---------------------

    D_mm(1:ff) = D_f
    D_mm(ff+1:fdat%dmsn) = D_s
    P_mm(ff+1:fdat%dmsn) = P_s

    do i = 1,fdat%dmsn
        D_m(fdat%ind_V(i)) = D_mm(i)
        P_m(fdat%ind_V(i)) = P_mm(i)
    end do

    if (sum(fdat%BCs(:,5)) .NE. 0) then
        fdat%D_nodal = matmul(transpose(fdat%R_sup),D_m)
        fdat%P_nodal = matmul(transpose(fdat%R_sup),P_m)
    else
        fdat%D_nodal = D_m
        fdat%P_nodal = P_m
    end if


!    write (*,*) fdat%D_nodal*2.1*10**7*0.0016
!    write (*,*) P_m


    fdat%eigen_K = K_ff
    fdat%eigen_M = M_ff
    fdat%n_modes = ff

    deallocate (K_ff, K_sf, K_fs, K_ss, K_ff_inv, M_ff)
    deallocate (P, P_m, P_mm, P_f, P_ls, P_s)
    deallocate (D_s, D_f, D_mm, D_m)
    deallocate (A, B)

return
end subroutine solver
