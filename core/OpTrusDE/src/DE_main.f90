subroutine DE_main(opt,fdat,opt_set_i,opt_set_r,xbound,cdim,x_opt,z_opt)

    use datmod
    use optmod

    use data_type
!    use data_Rosen
!    use data_HDE
    implicit none

    type(global_data) ::  fdat
    type(opt_data)    ::  opt

    real*4              :: opt_set_r(3), z_opt, VTR
    integer             :: opt_set_i(9), cdim
    real*4              :: xbound(opt%Nvar,2), x_opt(opt%Nvar), c_opt(opt%n_constraints)

    integer             :: Dim_XC, NP, itermax, strategy, refresh, iwrite, method(3), nfeval
    real*4              :: XCmin(opt%Nvar), XCmax(opt%Nvar), CR_XC, F_XC, F_CR
    real*4              :: bestmem_XC(opt%Nvar), bestval, best_constr(opt%n_constraints)

    integer(kind=IB) :: i
    integer (kind=IB), dimension(8) :: time
    intrinsic date_and_time
    external objective_M, objective_W
    open(iwrite,file='Rosen.txt')
    call date_and_time(values=time)
    write(unit=iwrite, FMT=11) time(1:3), time(5:7)

!  write(*,*) F_XC, F_CR

    VTR=-1.0e+6

    Dim_XC      = opt%Nvar
    XCmin       = xbound(:,1)
    XCmax       = xbound(:,2)
    NP          = opt_set_i(2)
    itermax     = opt_set_i(3)
    F_XC        = opt_set_r(1)
    CR_XC       = opt_set_r(2)
    F_CR        = opt_set_r(3)
    strategy    = opt_set_i(4)
    refresh     = opt_set_i(5)
    iwrite      = opt_set_i(6)
    method(1:3) = opt_set_i(7:9)
    opt%rho_p   = opt_set_i(10)

    if (opt%object .EQ. 1) then
        call DE_Fortran90(opt,fdat,objective_W, Dim_XC, XCmin, XCmax, VTR, NP, itermax, F_XC,&
                          CR_XC, strategy, refresh, iwrite, bestmem_XC, &
                          bestval, best_constr, nfeval, F_CR, method)
    else if (opt%object .EQ. 2) then
        call DE_Fortran90(opt,fdat,objective_M, Dim_XC, XCmin, XCmax, VTR, NP, itermax, F_XC,&
                          CR_XC, strategy, refresh, iwrite, bestmem_XC, &
                          bestval, best_constr, nfeval, F_CR, method)
    end if

!            opt_z = minloc(zerg)
!            opt_z_id = opt_z(1)
            x_opt(:) = bestmem_XC
            z_opt    = bestval
            c_opt(:) = best_constr

              write(iwrite,205) NP, nfeval, method(1:3)
              write(iwrite,FMT=201) F_XC, CR_XC, F_CR
              write(iwrite,FMT=200) bestval
              do i=1,Dim_XC
                 write(iwrite,FMT=202) i,bestmem_XC(i)
              end do
200     format(/2x, 'Bestval=', ES14.7)
201     format(2x, 'F_XC =',F6.3, 2x, 'CR_XC =', F6.3, 2x, 'F_CR =', F6.3)
202     format(2x, 'best_XC(',I3,') =',ES14.7)
205     format(2x, 'NP=', I4, 4x, 'No. function call =', I9, &
               /2x, 'mehtod(1:3) =',3I3)
    call date_and_time(values=time)
    write(unit=iwrite, FMT=10)time(1:3), time(5:7)
10  format(/1x, 'End of Program. Date:', I4, '/', I2,'/', I2, ', Time: ', I2,':',I2,':',I2)
11  format(1x, 'Beginning of Program. Date:', I4, '/', I2,'/', I2, ', Time: ', I2,':',I2,':',I2)

return
end subroutine DE_main

!program Rosen
!  use data_type
!  use data_Rosen
!  use data_HDE
!  implicit none
!  integer(kind=IB) :: i
!  integer (kind=IB), dimension(8) :: time
!  intrinsic date_and_time
!  external FTN
!  open(iwrite,file='Rosen.txt')
!  call date_and_time(values=time)
!  write(unit=iwrite, FMT=11) time(1:3), time(5:7)
!
!!  write(*,*) F_XC, F_CR
!
!  call DE_Fortran90(FTN, Dim_XC, XCmin, XCmax, VTR, NP, itermax, F_XC,&
!                CR_XC, strategy, refresh, iwrite, bestmem_XC, &
!                bestval, nfeval, F_CR, method)
!
!              write(iwrite,205) NP, nfeval, method(1:3)
!              write(iwrite,FMT=201) F_XC, CR_XC, F_CR
!              write(iwrite,FMT=200) bestval
!              do i=1,Dim_XC
!                 write(iwrite,FMT=202) i,bestmem_XC(i)
!              end do
!200     format(/2x, 'Bestval=', ES14.7)
!201     format(2x, 'F_XC =',F6.3, 2x, 'CR_XC =', F6.3, 2x, 'F_CR =', F6.3)
!202     format(2x, 'best_XC(',I3,') =',ES14.7)
!205     format(2x, 'NP=', I4, 4x, 'No. function call =', I9, &
!               /2x, 'mehtod(1:3) =',3I3)
!    call date_and_time(values=time)
!    write(unit=iwrite, FMT=10)time(1:3), time(5:7)
!10  format(/1x, 'End of Program. Date:', I4, '/', I2,'/', I2, ', Time: ', I2,':',I2,':',I2)
!11  format(1x, 'Beginning of Program. Date:', I4, '/', I2,'/', I2, ', Time: ', I2,':',I2,':',I2)
!end program Rosen



!subroutine DE_main(opt,fdat,opt_set_i,opt_set_r,xbound,cdim,x_opt,z_opt)
!
!    use data_type
!    use datmod
!    use optmod
!
!    implicit none
!
!    type(global_data) ::  fdat
!    type(opt_data)    ::  opt
!
!    integer,parameter   ::  q_gen_opt_i = 3             ! general
!    integer             ::  gen_opt_i(3) =(/12,5,0/)    ! general
!
!    integer,intent(in)   ::  cdim,opt_set_i(9)
!    real*4, intent(in)   ::  opt_set_r(2)
!    real*4, intent(in)   ::  xbound(opt_set_i(1),2)
!
!    real*4    ::  error(opt_set_i(4)+1)
!    real*4    ::  xpred(opt_set_i(9),opt_set_i(1)),zpred(opt_set_i(9),opt_set_i(2))
!    real*4    ::  xerg(opt_set_i(4),opt_set_i(1)),zerg(opt_set_i(4),opt_set_i(2))
!    real*4    ::  cerg(opt_set_i(4),cdim)
!    real*4    ::  xsave(opt_set_i(4)*opt_set_i(5)*opt_set_i(6),opt_set_i(1))
!    real*4    ::  zsave(opt_set_i(4)*opt_set_i(5)*opt_set_i(6),opt_set_i(2))
!    real*4    ::  csave(opt_set_i(4)*opt_set_i(5)*opt_set_i(6),cdim)
!    real*4    ::  xstart(opt_set_i(10),opt_set_i(1))
!
!    character*256   :: file
!    integer         :: opt_z(2), opt_z_id
!    integer         :: sim,t,i, k, j, cond !, N_tol1
!    real*4          :: x_opt(opt_set_i(1)),z_opt(opt_set_i(2)),c_opt(cdim)
!    real*4          :: x_opt_0(opt_set_i(1)),z_opt_0(opt_set_i(2))
!    real*4          :: tol_x, tol_z, rho_p_step, tol_x_step, max_rho_p, max_tol_x
!    real*4          :: lim1, lim2, z_max,z_min
!    real*4          :: W_ext, M_tot, sigma_max, f_N
!
!    tol_x      = opt%opt_par(7)
!    tol_z      = opt%opt_par(8)
!
!    rho_p_step = opt%opt_par(10)
!    max_rho_p  = opt%opt_par(11)
!
!    tol_x_step = opt%opt_par(12)
!    max_tol_x  = opt%opt_par(13)
!
!    k = 1
!    if (opt%n_area.EQ.1)then
!        xstart(1,k) = fdat%El_Prop(int(opt%area(1,1)),4) ! Oxi auto!! theloume OPT_X
!    else if (opt%n_area.GT.1)then
!        xstart(1,k) = fdat%El_Prop(int(opt%area(1,1)),4)
!        do j = 2,opt%n_area
!            if (opt%area(j,4).NE.opt%area(j-1,4)) then
!                k = k+1
!                xstart(1,k) = fdat%El_Prop(int(opt%area(j,1)),4)
!            endif
!        enddo
!        k = k+1
!    endif
!    if (opt%n_x_node.EQ.1)then
!        xstart(1,k) = fdat%nodes(int(opt%x_node(1,1)),2)
!    else if (opt%n_x_node.GT.1)then
!        xstart(1,k) = fdat%nodes(int(opt%x_node(1,1)),2)
!        do j = 2,opt%n_x_node
!            if (opt%x_node(j,4).NE.opt%x_node(j-1,4)) then
!                k = k+1
!                xstart(1,k) = fdat%nodes(int(opt%x_node(j,1)),2)
!            endif
!        enddo
!        k = k+1
!    endif
!    if (opt%n_y_node.EQ.1)then
!        xstart(1,k) = fdat%nodes(int(opt%y_node(1,1)),3)
!    else if (opt%n_y_node.GT.1)then
!        xstart(1,k) = fdat%nodes(int(opt%y_node(1,1)),3)
!        do j = 2,opt%n_y_node
!            if (opt%y_node(j,4).NE.opt%y_node(j-1,4)) then
!                k = k+1
!                xstart(1,k) = fdat%nodes(int(opt%y_node(j,1)),3)
!            endif
!        enddo
!    endif
!
!    lim1 = 10e+5
!    lim2 = 10e+5
!
!    z_min = 10e+8
!    z_max = 0.0
!
!    cond = 1
!
!    do while ((lim2.GT.tol_z))
!!        N_tol1 = 1
!        opt%rho_p = opt%opt_par(9)
!        do while (((lim1.GT.tol_x).OR.(lim2.GT.tol_z).OR.(opt%rho_p.EQ.opt%opt_par(9))).AND.(opt%rho_p.LE.max_rho_p))
!            write(*,*) '-------------------------------------------------------------- '
!            write(*,*) 'Core Optimization run: ', cond
!            write(*,*) 'Penalty factor = ', opt%rho_p, 'x_tolerance = ',  tol_x!, N_tol1
!
!            if (cond.NE.1) then
!                x_opt_0(:) = xerg(opt_z_id,:)
!                z_opt_0(:) = zerg(opt_z_id,:)
!
!                xstart(1,:) = x_opt_0(:)
!            endif
!
!            Dim_XC      = opt%Nvar
!            XCmin       = xbound(:,1)
!            XCmax       = xbound(:,2)
!            NP          = opt_set_i(1)
!            itermax     = opt_set_i(2)
!            F_XC        = opt_set_r(1)
!            CR_XC       = opt_set_r(2)
!            strategy    = opt_set_i(3)
!            iwrite      = opt_set_i(4)
!            method(1:3) = opt_set_i(5:7)
!
!            call DE_Fortran90(objectiveDE, Dim_XC, XCmin, XCmax, VTR, NP, itermax, F_XC, &
!                              CR_XC, strategy, refresh, iwrite, bestmem_XC, bestval, nfeval, &
!                              F_CR, method)
!
!!            call evolu(xbound,xpred,zpred,xstart,opt_set_i,opt_set_r, &
!!                        xerg,zerg,cerg,xsave,zsave,csave,sim,error,q_gen_opt_i,  &
!!                        gen_opt_i,opt%n_constraints,fdat,opt)
!
!!            write(*,*)opt_set_i(1)
!
!            opt_z = minloc(zerg)
!            opt_z_id = opt_z(1)
!            x_opt(:) = xerg(opt_z_id,:)
!            z_opt(:) = zerg(opt_z_id,:)
!            c_opt(:) = cerg(opt_z_id,:)
!
!            if (z_opt(1) .GT. z_max)then
!                z_max = z_opt(1)
!            end if
!
!            if (z_opt(1) .LT. z_min)then
!                z_min = z_opt(1)
!            end if
!!            write(*,*)z_opt(1),z_max,z_min
!
!            write(*,*) 'constraints:', c_opt
!
!            if (opt%rho_p.NE.opt%opt_par(9)) then
!                lim1 = 0
!                do i = 1,opt_set_i(1)
!                    lim1 = lim1 + abs(( x_opt(i) - x_opt_0(i) )/(x_opt(i) + x_opt_0(i))/2.0)
!                enddo
!                lim1 = sqrt(lim1)
!!                write(*,*) 'test:', abs(z_opt(1)-z_opt_0(1)), (z_min+z_max)/2.0
!                lim2 = abs((z_opt(1)-z_opt_0(1))/(z_min+z_max)/2.0)
!            endif
!
!            write(*,*) 'X error',lim1
!            write(*,*) 'Z error',lim2
!
!            opt%rho_p = rho_p_step*opt%rho_p
!!            N_tol1 = N_tol1+1
!
!            cond = cond + 1
!        end do
!        tol_x = tol_x_step*tol_x
!        if (tol_x .GT.max_tol_x) then
!            stop 9
!        endif
!    end do
!
!!    if (opt_set_i(3) .eq. 1) then
!!        opt_z = minloc(zerg)
!!    elseif(opt_set_i(3) .eq. 2) then
!!        opt_z = maxloc(zerg)
!!    endif
!!    opt_z_id = opt_z(1)
!!!
!!    x_opt(:) = xerg(opt_z_id,:)
!!    z_opt(:) = zerg(opt_z_id,:)
!
!return
!end subroutine DE_main
