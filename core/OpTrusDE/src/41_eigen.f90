subroutine eigen(fdat)
    use datmod
    implicit none

    type(global_data)::  fdat

    integer :: n, q, i, j, sorted
    real  :: norm, temp_l, vector(fdat%dmsn)
    real, allocatable  :: K_eigen(:,:), M_eigen(:,:), e_val(:), e_vect(:,:), temp_v(:)

    n = fdat%n_modes
!    if (fdat%parameters(3) .EQ. 0) then
!        q = n
!    else
!        q = int(fdat%parameters(3))
!    endif

    q = n!fdat%n_modes_to_calc

    if (.not. allocated(K_eigen)) allocate (K_eigen(n,n))
    if (.not. allocated(M_eigen)) allocate (M_eigen(n,n))

    if (.not. allocated(e_val)) allocate (e_val(q))
    if (.not. allocated(e_vect)) allocate (e_vect(n,q))

    if (.not. allocated(temp_v)) allocate (temp_v(n))

    K_eigen = fdat%eigen_K
    M_eigen = fdat%eigen_M

    do i = 1,n
        do j = 1,n
            if (abs(K_eigen(i,j)) .LT. 0.01) then
                K_eigen(i,j) = 0.0
            endif
            if (abs(M_eigen(i,j)) .LT. 0.01) then
                M_eigen(i,j) = 0.0
            endif
        enddo
    enddo

    do i = 1,n-1
        do j = i+1,n
            K_eigen(j,i) = K_eigen(i,j)
            M_eigen(j,i) = M_eigen(i,j)
        enddo
    enddo

!    call positive_definite(n,K_eigen,pd)
!    write (*,*) pd

    if (fdat%parameters(2) .EQ. 1.0) then
        call Jacobi(K_eigen, M_eigen, n, e_val, e_vect, fdat%parameters)
    else
        call Subspace_Iteration(K_eigen, M_eigen, n, q, e_val, e_vect, fdat%parameters)
    endif
!    call Householder_QR_InvIter(K_eigen, M_eigen, n, q, e_val, e_vect)

!    call Jacobi(K_eigen, M_eigen, n, e_val, e_vect)

!    call Subspace_Iteration(K_eigen, M_eigen, n, q, e_val, e_vect)

!
!    Normalize eigenvectors
!
    do i = 1,q
        norm = 0.0
        do j = 1,n
            if (abs(e_vect(j,i)) .GT. abs(norm)) then
                norm = abs(e_vect(j,i))
            endif
        enddo
        e_vect(:,i) = e_vect(:,i)/norm
    enddo

!
!    Sort the eigenvalues and eigenvectors from small to large eigenvalues
!
    sorted = 0
    do while (sorted .EQ. 0)
        sorted = 1
        do i = q,2,-1
            if (e_val(i) .LT. e_val(i-1)) then
                sorted = 0
                temp_l = e_val(i-1)
                e_val(i-1) = e_val(i)
                e_val(i) = temp_l
                temp_v = e_vect(:,i-1)
                e_vect(:,i-1) = e_vect(:,i)
                e_vect(:,i) = temp_v
            endif
        enddo
    enddo

    fdat%eigen_f = e_val

!    write (*,*) e_val(1:q)
!    do i = 1,n
!        write (*,*) e_vect(i,1:q)
!    enddo

    do i = 1,q
        vector = 0.0
        vector(1:n) = e_vect(:,i)

        do j = 1,fdat%dmsn
            fdat%eigen_v(fdat%ind_V(j),i) = vector(j)
        end do
    enddo

    deallocate (K_eigen, M_eigen, e_val, e_vect, temp_v)
!    call LDL(K_eigen,n,L,D)

    return
end subroutine eigen
