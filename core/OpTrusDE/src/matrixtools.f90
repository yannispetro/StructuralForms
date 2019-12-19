subroutine gauss_1(a,b,x,n)
!============================================================
! Solutions to a system of linear equations A*x=b
! Method: the basic elimination (simple Gauss elimination)
! Alex G. November 2009
!-----------------------------------------------------------
! input ...
! a(n,n) - array of coefficients for matrix A
! b(n)   - vector of the right hand coefficients b
! n      - number of equations
! output ...
! x(n)   - solutions
! comments ...
! the original arrays a(n,n) and b(n) will be destroyed
! during the calculation
!===========================================================
    implicit none
    integer :: n
    real  :: a(n,n), b(n), x(n)
    real  :: c
    integer :: i, j, k

    !step 1: forward elimination
    do k=1, n-1
       do i=k+1,n
          c=a(i,k)/a(k,k)
          a(i,k) = 0.0
          b(i)=b(i)- c*b(k)
          do j=k+1,n
             a(i,j) = a(i,j)-c*a(k,j)
          end do
       end do
    end do

    !step 2: back substitution
    x(n) = b(n)/a(n,n)
    do i=n-1,1,-1
       c=0.0
       do j=i+1,n
         c= c + a(i,j)*x(j)
       end do
       x(i) = (b(i)- c)/a(i,i)
    end do
    return
end subroutine gauss_1
!=====================================================================
!=====================================================================
!=====================================================================
subroutine Subspace_Iteration(A, B, n, p, Eigv, V, par)
    implicit none
    integer :: n, p, q, i, j, k, ij, sorted
    integer :: ND, ICONV, NITE, NITEM
    real  :: A(n,n), B(n,n), Eigv(p), V(n,p), W(n), A_gauss(n,n), B_gauss(n), X_gauss(n), par(10)
    real, allocatable  :: E_VAL(:), E_VECT(:,:), R(:,:), X_hat(:,:), A_proj(:,:), B_proj(:,:)
    real, allocatable  :: Q_vect(:,:), L_val(:), temp_q(:)
    real  :: RT, L, XX, IX, pi, temp_l, temp_v(n), RTOL, dif, RDIF, EIGV2, VDOT
!    real  ::

    ICONV = 0

    NITEM = int(par(7))
    RTOL = par(6)
    q = min(2*p,p+8)
    ij = 0

    if (q .GE. n) then
!
!    Subspace Iteration is not required. Generalized Jacobi instead.
!
        if (.not. allocated(E_VAL)) allocate (E_VAL(n))
        if (.not. allocated(E_VECT)) allocate (E_VECT(n,n))
        call Jacobi(A, B, n, E_VAL, E_VECT, par)
!
!    Sort the eigenvalues and eigenvectors from small to large eigenvalues
!
        sorted = 0
        do while (sorted .EQ. 0)
            sorted = 1
            do i = n,2,-1
                if (E_VAL(i) .LT. E_VAL(i-1)) then
                    sorted = 0
                    temp_l = E_VAL(i-1)
                    E_VAL(i-1) = E_VAL(i)
                    E_VAL(i) = temp_l
                    temp_v = E_VECT(:,i-1)
                    E_VECT(:,i-1) = E_VECT(:,i)
                    E_VECT(:,i) = temp_v
                endif
            enddo
        enddo
        do i = 1,p
            Eigv(i) = E_VAL(i)
            V(:,i) = E_VECT(:,i)
        enddo
    else
!
!    Subspace Iteration is required.
!

        if (.not. allocated(E_VAL) ) allocate ( E_VAL(q)  )
        if (.not. allocated(E_VECT)) allocate (E_VECT(n,q))
        if (.not. allocated(R)     ) allocate (     R(n,q))
        if (.not. allocated(X_hat) ) allocate ( X_hat(n,q))
        if (.not. allocated(A_proj)) allocate (A_proj(q,q))
        if (.not. allocated(B_proj)) allocate (B_proj(q,q))
        if (.not. allocated(Q_vect)) allocate (Q_vect(q,q))
        if (.not. allocated(L_val) ) allocate ( L_val(q)  )
        if (.not. allocated(temp_q)) allocate (temp_q(q)  )
!
!    Construction of the starting iteration vectors
!
        ND = int(real(n)/real(q))
        E_VECT = 0.0
        do i = 1,n
            E_VECT(i,1) = B(i,i)
            W(i) = B(i,i)/A(i,i)
        enddo
        L = n - ND
        do j = 2,q
            RT = 0.0
            i = 1
            do while((i .LE. L) .AND. (W(i) .GE. RT))
                RT = W(i)
                ij = i
                i = i + 1
            enddo
            i = int(L)
            do while((i .LE. n) .AND. (W(i) .GT. RT))
                RT = W(i)
                ij = i
                i = i + 1
            enddo
            W(ij) = 0.0
            L = L - ND
            E_VECT(ij,j) = 1.0
        enddo
!
!    A random vector is added to the last vector
!
        pi = 3.141592654
        XX = 0.5
        do k = 1,n
            XX = (pi + XX)**5
            IX = int(XX)
            XX = XX - real(IX)
            E_VECT(k,q) = E_VECT(k,q) + XX
        enddo
!
!    Begining of iteration loop
!
        NITE = 0
        R = E_VECT
        do while ((ICONV .EQ. 0) .AND. (NITE .LT. NITEM))
            NITE = NITE + 1
!
!    Solve of the linear system K*X_hat(k+1) = M*X(k) to detrmine X_hat. (k -> NITE)
!
            do k = 1,q
                A_gauss = A
                B_gauss = R(:,k)
                call gauss_1(A_gauss,B_gauss,X_gauss,n)
                X_hat(:,k) = X_gauss
            enddo
!
!    Calculate the projections of A and B: K(k+1) = X_hat^T*K*X_hat, M(k+1) = X_hat^T*M*X_hat
!
            A_proj = matmul(matmul(transpose(X_hat),A),X_hat)
            B_proj = matmul(matmul(transpose(X_hat),B),X_hat)
!
!    Solve the Eigensystem of subspace operators using the generalized Jacobi method
!
            call Jacobi(A_proj, B_proj, q, L_val, Q_vect, par)
            E_VECT = matmul(X_hat,Q_vect)
            R = matmul(B,E_VECT)
!
!    Sort the eigenvalues and eigenvectors from small to large eigenvalues
!
            sorted = 0
            do while (sorted .EQ. 0)
                sorted = 1
                do i = q,2,-1
                    if (L_val(i) .LT. L_val(i-1)) then
                        sorted = 0
                        temp_l = L_val(i-1)
                        L_val(i-1) = L_val(i)
                        L_val(i) = temp_l
                        temp_q = Q_vect(:,i-1)
                        Q_vect(:,i-1) = Q_vect(:,i)
                        Q_vect(:,i) = temp_q
                        temp_v = E_vect(:,i-1)
                        E_vect(:,i-1) = E_vect(:,i)
                        E_vect(:,i) = temp_v
                    endif
                enddo
            enddo
!
!    Check for convergence
!
            ICONV = 1
            do i = 1,p
                EIGV2 = L_val(i)**2
                VDOT = dot_product(Q_vect(:,i),Q_vect(:,i))
                dif = VDOT - EIGV2
                RDIF = max(10**(-24)*EIGV2,dif)/EIGV2
                RDIF = RDIF**0.5
!                write (*,*) NITE,DIF
                if (RDIF .GT. RTOL) then
                    ICONV = 0
                endif
            enddo
        enddo
!
!    Store the required p eigenpairs for output
!
        do i = 1,p
            Eigv(i) = L_val(i)
            V(:,i) = E_vect(:,i)
        enddo
    endif
    if (allocated(E_VAL) ) deallocate (E_VAL)
    if (allocated(E_VECT)) deallocate (E_VECT)
    if (allocated(R)     ) deallocate (R)
    if (allocated(X_hat) ) deallocate (X_hat)
    if (allocated(A_proj)) deallocate (A_proj)
    if (allocated(B_proj)) deallocate (B_proj)
    if (allocated(Q_vect)) deallocate (Q_vect)
    if (allocated(L_val) ) deallocate (L_val)
    if (allocated(temp_q)) deallocate (temp_q)
    return
end subroutine Subspace_Iteration
!=====================================================================
!=====================================================================
!=====================================================================
subroutine Jacobi(A, B, n, Eigv, X, par)
    implicit none
    integer :: n, i, j, k, jj
    integer :: NSWEEP, NR, NSMAX, cond1, cond2
    real  :: A(n,n), B(n,n), X(n,n), Eigv(n), D(n), par(10), EPS, RTOL
    real  :: EPTOLA, EPTOLB, AKK, AJJ, AB, SCL, ABCH, AKKCH, AJJCH, CHECK, SQCH, D1, D2, DEN
    real  :: CA, CG, AK, BK, AJ, BJ, XJ, XK, TOL, DIF, EPSA, EPSB, BB
    integer :: JP1, JM1, KP1, KM1

    RTOL = par(4)
    NSMAX = int(par(5))
    cond2 = 1
!
!   Initialize Eigenvalue and Eigenvector Matrices
!
    X = 0.0
    do i = 1,n
        if ((A(i,i) .LE. 0.0) .OR. (B(i,i) .LE. 0.0)) then
            write (*,*) 'ERROR','c'
        endif
        D(i) = A(i,i)/B(i,i)
        Eigv(i) = D(i)
        X(i,i) = 1.0
    enddo
!
!   Initialize Sweep Counter abd Begin Iteration
!
    NSWEEP = 0
    NR = n - 1
    cond1 = 1
    do while ((NSWEEP .LT. NSMAX) .AND. ((cond1 .EQ. 1) .OR. (cond2 .EQ. 1)))
        NSWEEP = NSWEEP + 1
!
!   Check if present off-diagonal element is large enough to
!   require zeroing
!
        EPS = 0.01**(NSWEEP*2)
        do j = 1,NR
            jj = j + 1
            do k = jj,n
                EPTOLA = (A(j,k)/A(j,j))*(A(j,k)/A(k,k))
                EPTOLB = (B(j,k)/B(j,j))*(B(j,k)/B(k,k))
                if ((EPTOLA .GE. EPS) .OR. (EPTOLB .GE. EPS)) then
!
!   If zeroing is required, calculate the rotation matrix
!   elements CA and CG
!
                    AKK = A(k,k)*B(j,k) - B(k,k)*A(j,k)
                    AJJ = A(j,j)*B(j,k) - B(j,j)*A(j,k)
                    AB  = A(j,j)*B(k,k) - A(k,k)*B(j,j)
                    SCL = A(k,k)*B(k,k)
                    ABCH  = AB/SCL
                    AKKCH = AKK/SCL
                    AJJCH = AJJ/SCL
                    CHECK = (ABCH*ABCH + 4.0*AKKCH*AJJCH)/4.0
                    if (CHECK .LT. 0.0) then
                        write (*,*) 'ERROR','a'
                        stop
                    endif
                    SQCH = SCL*CHECK**0.5
                    D1 = AB/2.0 + SQCH
                    D2 = AB/2.0 - SQCH
                    DEN = D1
                    if (abs(D2) .GT. abs(D1)) then
                        DEN = D2
                    endif
                    if (DEN .EQ. 0.0) then
                        CA = 0.0
                        CG = -A(j,k)/A(k,k)
                    else
                        CA = AKK/DEN
                        CG = -AJJ/DEN
                    endif
!
!   Perform the generalized rotation to zero elements
!
                    if ((n-2) .NE. 0) then
                        JP1 = j + 1
                        JM1 = j - 1
                        KP1 = k + 1
                        KM1 = k - 1
                        if ((JM1 - 1) .GE. 0.0) then
                            do i = 1,JM1
                                AJ = A(i,j)
                                BJ = B(i,j)
                                AK = A(i,k)
                                BK = B(i,k)
                                A(i,j) = AJ + CG*AK
                                B(i,j) = BJ + CG*BK
                                A(i,k) = AK + CA*AJ
                                B(i,k) = BK + CA*BJ
                            enddo
                        endif
                        if ((KP1 - n) .LE. 0.0) then
                            do i = KP1,n
                                AJ = A(j,i)
                                BJ = B(j,i)
                                AK = A(k,i)
                                BK = B(k,i)
                                A(j,i) = AJ + CG*AK
                                B(j,i) = BJ + CG*BK
                                A(k,i) = AK + CA*AJ
                                B(k,i) = BK + CA*BJ
                            enddo
                        endif
                        if ((JP1 - KM1) .LE. 0.0) then
                            do i = JP1,KM1
                                AJ = A(j,i)
                                BJ = B(j,i)
                                AK = A(i,k)
                                BK = B(i,k)
                                A(j,i) = AJ + CG*AK
                                B(j,i) = BJ + CG*BK
                                A(i,k) = AK + CA*AJ
                                B(i,k) = BK + CA*BJ
                            enddo
                        endif
                    endif
                    AK = A(k,k)
                    BK = B(k,k)
                    A(k,k) = AK + 2.0*CA*A(j,k) + CA**2*A(j,j)
                    B(k,k) = BK + 2.0*CA*B(j,k) + CA**2*B(j,j)
                    A(j,j) = A(j,j) + 2.0*CG*A(j,k) + CG**2*AK
                    B(j,j) = B(j,j) + 2.0*CG*B(j,k) + CG**2*BK
                    A(j,k) = 0.0
                    B(j,k) = 0.0
!
!     Update the eigenvector matrix after each rotation
!
                    do i = 1,n
                        XJ = X(i,j)
                        XK = X(i,k)
                        X(i,j) = XJ + CG*XK
                        X(i,k) = XK + CA*XJ
                    enddo
                endif
            enddo
        enddo
!
!     Update the eigenvalues after each sweep
!
        do i =1,n
!            if (B(i,i) .LE. 0.0) then
!            write (*,*) A(i,i), B(i,i)
            if ((A(i,i) .LE. 0.0) .OR. (B(i,i) .LE. 0.0)) then
                write (*,*) 'ERROR','b'
                stop
            endif
            Eigv(i) = A(i,i)/B(i,i)
        enddo
!
!     Check for convergence (using cond1)
!
        i = 0
        cond1 = 0
        do while ( (i .LT. n) .AND. (cond1 .EQ. 0))
            i = i + 1
            TOL = RTOL*D(i)
            DIF = abs(Eigv(i) - D(i))
!            write (*,*) DIF
            if (DIF .GT. TOL) then
                cond1 = 1
            endif
        enddo
!
!     Check off-diagonal elements to see if another sweep is needed  (using cond2)
!
        j = 0
        cond2 = 0
        do while ( (j .LT. NR) .AND. (cond1 .EQ. 0) .AND. (cond2 .EQ. 0))
            EPS = RTOL**2
            j = j + 1
            jj = j + 1
            k = jj - 1
            do while ( (k .LT. n) .AND. (cond2 .EQ. 0))
                k = k + 1
                EPSA = (A(j,k)/A(j,j))*(A(j,k)/A(k,k))
                EPSB = (B(j,k)/B(j,j))*(B(j,k)/B(k,k))
                if ((EPSA .GE. EPS) .OR. (EPSB .GE. EPS)) then
                    cond2 = 1
                endif
            enddo
        enddo
!
!    Update D matrix and start new sweep, if allowed
!    (main do while loop a) cond 1, b) cond2, c) NSMAX.)
!
        if ((cond1 .EQ. 1) .OR. (cond2 .EQ. 1)) then
            do i = 1,n
                D(i) = Eigv(i)
            enddo
        endif
    enddo
!
!    Fill out bottom triangle of resultant matrices, scale eigenvectors
!
    do i = 1,n
        do j = 1,n
            A(j,i) = A(i,j)
            B(j,i) = B(i,j)
        enddo
    enddo
    do j = 1,n
        BB = B(j,j)**0.5
        do k = 1,n
            X(k,j) = X(k,j)/BB
        enddo
    enddo
!    write (*,*) cond1, cond2, NSWEEP
    return
end subroutine Jacobi
!=====================================================================
!=====================================================================
!=====================================================================
