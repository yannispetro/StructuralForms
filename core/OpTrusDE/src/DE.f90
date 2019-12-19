subroutine DE_Fortran90(opt,fdat,obj, Dim_XC, XCmin, XCmax, VTR, NP, itermax, F_XC, &
           CR_XC, strategy, refresh, iwrite, bestmem_XC, bestval, best_constr, nfeval, &
           F_CR, method)
!.......................................................................
!
! Differential Evolution for Optimal Control Problems
!
!.......................................................................
!  This Fortran 90 program translates from the original MATLAB
!  version of differential evolution (DE). This FORTRAN 90 code
!  has been tested on Compaq Visual Fortran v6.1.
!  Any users new to the DE are encouraged to read the article of Storn and Price.
!
!  Refences:
!  Storn, R., and Price, K.V., (1996). Minimizing the real function of the
!    ICEC'96 contest by differential evolution. IEEE conf. on Evolutionary
!    Comutation, 842-844.
!
!  This Fortran 90 program written by Dr. Feng-Sheng Wang
!  Department of Chemical Engineering, National Chung Cheng University,
!  Chia-Yi 621, Taiwan, e-mail: chmfsw@ccunix.ccu.edu.tw
!.........................................................................
!                obj : The user provided file for evlauting the objective function.
!                      subroutine obj(xc,fitness)
!                      where "xc" is the real decision parameter vector.(input)
!                            "fitness" is the fitness value.(output)
!             Dim_XC : Dimension of the real decision parameters.
!      XCmin(Dim_XC) : The lower bound of the real decision parameters.
!      XCmax(Dim_XC) : The upper bound of the real decision parameters.
!                VTR : The expected fitness value to reach.
!                 NP : Population size.
!            itermax : The maximum number of iteration.
!               F_XC : Mutation scaling factor for real decision parameters.
!              CR_XC : Crossover factor for real decision parameters.
!           strategy : The strategy of the mutation operations is used in HDE.
!            refresh : The intermediate output will be produced after "refresh"
!                      iterations. No intermediate output will be produced if
!                      "refresh < 1".
!             iwrite : The unit specfier for writing to an external data file.
! bestmen_XC(Dim_XC) : The best real decision parameters.
!              bestval : The best objective function.
!             nfeval : The number of function call.
!         method(1) = 0, Fixed mutation scaling factors (F_XC)
!                   = 1, Random mutation scaling factors F_XC=[0, 1]
!                   = 2, Random mutation scaling factors F_XC=[-1, 1]
!         method(2) = 1, Random combined factor (F_CR) used for strategy = 6
!                        in the mutation operation
!                   = other, fixed combined factor provided by the user
!         method(3) = 1, Saving results in a data file.
!                   = other, displaying results only.
     use datmod
     use optmod

!     use data_type, only : IB, RP
     implicit none

     type(global_data) ::  fdat
     type(opt_data)    ::  opt


     integer, intent(in) :: NP, Dim_XC, itermax, strategy,   &
                                     iwrite, refresh
     real*4, intent(in) :: VTR, CR_XC
     real*4 :: F_XC, F_CR
     real*4, dimension(Dim_XC), intent(in) :: XCmin, XCmax
     real*4, dimension(Dim_XC), intent(inout) :: bestmem_XC
     real*4, intent(out) :: bestval, best_constr(opt%n_constraints)
     integer, intent(out) :: nfeval
     real*4, dimension(NP,Dim_XC) :: pop_XC, bm_XC, mui_XC, mpo_XC,   &
                                            popold_XC, rand_XC, ui_XC
     integer :: i, ibest, iter
     integer, dimension(NP) :: rot, a1, a2, a3, a4, a5, rt
     integer, dimension(4) :: ind
     real*4 :: tempval, temp_c(opt%n_constraints)
     real*4, dimension(NP) :: val
     real*4, dimension(Dim_XC) :: bestmemit_XC
     real*4, dimension(Dim_XC) :: rand_C1
     integer, dimension(3), intent(in) :: method
     external  obj
     intrinsic max, min, random_number, mod, abs, any, all, maxloc
     interface
        function randperm(num)
!           use data_type, only : IB
           implicit none
           integer, intent(in) :: num
           integer, dimension(num) :: randperm
        end function randperm
     end interface

!     write(*,*)XCmax
!     write(*,*)F_XC,CR_XC,F_CR
!     write(*,*)strategy, refresh, iwrite,method
!     write(*,*) 'constr',opt%constraints

 !!-----Initialize a population --------------------------------------------!!
        pop_XC=0.0
        do i=1,NP
           call random_number(rand_C1)
           pop_XC(i,:)=XCmin+rand_C1*(XCmax-XCmin)
        end do
!!--------------------------------------------------------------------------!!

!!------Evaluate fitness functions and find the best member-----------------!!
     val=0.0
     nfeval=0
     ibest=1
! ==================================================================================
     do i=1,opt%n_constraints
        write (*,*) opt%constraints(i,:)
     enddo
     call obj(opt,fdat,pop_XC(1,:),opt%Nvar,val(1),temp_c,1,opt%n_constraints)
!     call obj(pop_XC(1,:), val(1))
! ==================================================================================
     bestval=val(1)
     nfeval=nfeval+1
     best_constr = temp_c
     do i=2,NP
! ==================================================================================
        call obj(opt,fdat,pop_XC(i,:),opt%Nvar,val(i),temp_c,1,opt%n_constraints)
!        call obj(pop_XC(i,:), val(i))
!        write(*,*) 'asd',val(i),opt%constraints
! ==================================================================================
        nfeval=nfeval+1
        if (val(i) < bestval) then
           ibest=i
           bestval=val(i)
           best_constr = temp_c
        end if
     end do
     bestmemit_XC=pop_XC(ibest,:)
     bestmem_XC=bestmemit_XC
!!--------------------------------------------------------------------------!!

     bm_XC=0.0
     rot=(/(i,i=0,NP-1)/)
     iter=1
!     write(*,*) rot
!!------Perform evolutionary computation------------------------------------!!

     do while (iter <= itermax)
        popold_XC=pop_XC

!!------Mutation operation--------------------------------------------------!!
        ind=randperm(4)
        a1=randperm(NP)
        rt=mod(rot+ind(1),NP)
        a2=a1(rt+1)
        rt=mod(rot+ind(2),NP)
        a3=a2(rt+1)
        rt=mod(rot+ind(3),NP)
        a4=a3(rt+1)
        rt=mod(rot+ind(4),NP)
        a5=a4(rt+1)
        bm_XC=spread(bestmemit_XC, DIM=1, NCOPIES=NP)

!! ===================================================================================================
!        write(*,*)'---------------------------- bm_XC -----------------------------------------'
!        do i=1,NP
!            write(*,*) bm_XC(i,:)
!        end do
!        write(*,*)'----------------------------- a1, a2 ----------------------------------------'
!        write(*,*) a1
!        write(*,*) a2
!! ===================================================================================================

!----- Generating a random sacling factor--------------------------------!
        select case (method(1))
        case (1)
           call random_number(F_XC)
        case(2)
           call random_number(F_XC)
           F_XC=2.0*F_XC-1.0
        end select

!---- select a mutation strategy-----------------------------------------!
        select case (strategy)
        case (1)
           ui_XC=bm_XC+F_XC*(popold_XC(a1,:)-popold_XC(a2,:))

        case default
           ui_XC=popold_XC(a3,:)+F_XC*(popold_XC(a1,:)-popold_XC(a2,:))

        case (3)
           ui_XC=popold_XC+F_XC*(bm_XC-popold_XC+popold_XC(a1,:)-popold_XC(a2,:))

        case (4)
           ui_XC=bm_XC+F_XC*(popold_XC(a1,:)-popold_XC(a2,:)+popold_XC(a3,:)-popold_XC(a4,:))

        case (5)
           ui_XC=popold_XC(a5,:)+F_XC*(popold_XC(a1,:)-popold_XC(a2,:)+popold_XC(a3,:) &
                 -popold_XC(a4,:))
        case (6) ! A linear crossover combination of bm_XC and popold_XC
           if (method(2) == 1) call random_number(F_CR)
           ui_XC=popold_XC+F_CR*(bm_XC-popold_XC)+F_XC*(popold_XC(a1,:)-popold_XC(a2,:))

        end select

!! ===================================================================================================
!        write(*,*)'-------------------------- popold_XC ------------------------------------------'
!        do i=1,NP
!            write(*,*) popold_XC(i,:)
!        end do
!        write(*,*)'--------------------------- ui_XC (mut) -------------------------------------------'
!        do i=1,NP
!            write(*,*) ui_XC(i,:)
!        end do
!! ===================================================================================================

!!--------------------------------------------------------------------------!!
!!------Crossover operation-------------------------------------------------!!
        call random_number(rand_XC)
           mui_XC=0.0
           mpo_XC=0.0
        where (rand_XC < CR_XC)
           mui_XC=1.0
!           mpo_XC=0.0_RP
        elsewhere
!           mui_XC=0.0_RP
           mpo_XC=1.0
        end where

        ui_XC=popold_XC*mpo_XC+ui_XC*mui_XC

!! ===================================================================================================
!        write(*,*)'-------------------------- rand_XC --------------------------------------------'
!        do i=1,NP
!            write(*,*) rand_XC(i,:)
!        end do
!        write(*,*)'-------------------------- ui_XC (mut+cross) --------------------------------------'
!        do i=1,NP
!            write(*,*) ui_XC(i,:)
!        end do
!! ===================================================================================================

!!--------------------------------------------------------------------------!!
!!------Evaluate fitness functions and find the best member-----------------!!
        do i=1,NP
!!------Confine each of feasible individuals in the lower-upper bound-------!!
           ui_XC(i,:)=max(min(ui_XC(i,:),XCmax),XCmin)
! ==================================================================================
           call obj(opt,fdat,ui_XC(i,:),opt%Nvar,tempval,temp_c,1,opt%n_constraints)
!           call obj(ui_XC(i,:), tempval)
! ==================================================================================
           nfeval=nfeval+1
           if (tempval < val(i)) then
              pop_XC(i,:)=ui_XC(i,:)
              val(i)=tempval
              if (tempval < bestval) then
                 bestval=tempval
                 bestmem_XC=ui_XC(i,:)
                 best_constr = temp_c
              end if
           end if
        end do

!! ===================================================================================================
!        write(*,*)'-------------------------- pop_XC (mut+cross) --------------------------------------'
!        do i=1,NP
!            write(*,*) pop_XC(i,:), 'best', val(i)
!        end do
!        write(*,*)'--------------------------------------------------------------------------'
!! ===================================================================================================

        bestmemit_XC=bestmem_XC
        if( (refresh > 0) .and. (mod(iter,refresh)==0)) then
             if (method(3)==1) write(unit=iwrite,FMT=203) iter
             write(unit=*, FMT=203) iter
             do i=1,Dim_XC
                 if (method(3)==1) write(unit=iwrite, FMT=202) i, bestmem_XC(i)
                 write(*,FMT=202) i,bestmem_XC(i)
             end do
             if (method(3)==1) write(unit=iwrite, FMT=201) bestval
             write(unit=*, FMT=204) best_constr
             write(unit=*, FMT=201) bestval
        end if
        iter=iter+1
        if ( bestval <= VTR .and. refresh > 0) then
           write(unit=iwrite, FMT=*) ' The best fitness is smaller than VTR'
           write(unit=*, FMT=*) 'The best fitness is smaller than VTR'
           exit
        endif
     end do
!!------end the evolutionary computation------------------------------!!
201 format(2x, 'bestval =', ES14.7, /)
202 format(5x, 'bestmem_XC(', I3, ') =', ES12.5)
203 format(2x, 'No. of iteration  =', I8)
204 format(2x, 'constraints (values) =', ES14.7)
end subroutine DE_Fortran90


function randperm(num)
!  use data_type, only : IB, RP
  implicit none
  integer, intent(in) :: num
  integer :: number, i, j, k
  integer, dimension(num) :: randperm
  real*4, dimension(num) :: rand2
  intrinsic random_number
  call random_number(rand2)
  do i=1,num
     number=1
     do j=1,num
        if (rand2(i) > rand2(j)) then
           number=number+1
        end if
     end do
     do k=1,i-1
        if (rand2(i) <= rand2(k) .and. rand2(i) >= rand2(k)) then
           number=number+1
        end if
     end do
     randperm(i)=number
  end do
  return
end function randperm











!subroutine DE_Fortran90(opt,fdat,obj, Dim_XC, XCmin, XCmax, VTR, NP, itermax, F_XC, &
!           CR_XC, strategy, refresh, iwrite, bestmem_XC, bestval, nfeval, &
!           F_CR, method)
!!.......................................................................
!!
!! Differential Evolution for Optimal Control Problems
!!
!!.......................................................................
!!  This Fortran 90 program translates from the original MATLAB
!!  version of differential evolution (DE). This FORTRAN 90 code
!!  has been tested on Compaq Visual Fortran v6.1.
!!  Any users new to the DE are encouraged to read the article of Storn and Price.
!!
!!  Refences:
!!  Storn, R., and Price, K.V., (1996). Minimizing the real function of the
!!    ICEC'96 contest by differential evolution. IEEE conf. on Evolutionary
!!    Comutation, 842-844.
!!
!!  This Fortran 90 program written by Dr. Feng-Sheng Wang
!!  Department of Chemical Engineering, National Chung Cheng University,
!!  Chia-Yi 621, Taiwan, e-mail: chmfsw@ccunix.ccu.edu.tw
!!.........................................................................
!!                obj : The user provided file for evlauting the objective function.
!!                      subroutine obj(xc,fitness)
!!                      where "xc" is the real decision parameter vector.(input)
!!                            "fitness" is the fitness value.(output)
!!             Dim_XC : Dimension of the real decision parameters.
!!      XCmin(Dim_XC) : The lower bound of the real decision parameters.
!!      XCmax(Dim_XC) : The upper bound of the real decision parameters.
!!                VTR : The expected fitness value to reach.
!!                 NP : Population size.
!!            itermax : The maximum number of iteration.
!!               F_XC : Mutation scaling factor for real decision parameters.
!!              CR_XC : Crossover factor for real decision parameters.
!!           strategy : The strategy of the mutation operations is used in HDE.
!!            refresh : The intermediate output will be produced after "refresh"
!!                      iterations. No intermediate output will be produced if
!!                      "refresh < 1".
!!             iwrite : The unit specfier for writing to an external data file.
!! bestmen_XC(Dim_XC) : The best real decision parameters.
!!              bestval : The best objective function.
!!             nfeval : The number of function call.
!!         method(1) = 0, Fixed mutation scaling factors (F_XC)
!!                   = 1, Random mutation scaling factors F_XC=[0, 1]
!!                   = 2, Random mutation scaling factors F_XC=[-1, 1]
!!         method(2) = 1, Random combined factor (F_CR) used for strategy = 6
!!                        in the mutation operation
!!                   = other, fixed combined factor provided by the user
!!         method(3) = 1, Saving results in a data file.
!!                   = other, displaying results only.
!     use datmod
!     use optmod
!
!     use data_type, only : IB, RP
!     implicit none
!
!     type(global_data) ::  fdat
!     type(opt_data)    ::  opt
!
!
!     integer(kind=IB), intent(in) :: NP, Dim_XC, itermax, strategy,   &
!                                     iwrite, refresh
!     real(kind=RP), intent(in) :: VTR, CR_XC
!     real(kind=RP) :: F_XC, F_CR
!     real(kind=RP), dimension(Dim_XC), intent(in) :: XCmin, XCmax
!     real(kind=RP), dimension(Dim_XC), intent(inout) :: bestmem_XC
!     real(kind=RP), intent(out) :: bestval
!     integer(kind=IB), intent(out) :: nfeval
!     real(kind=RP), dimension(NP,Dim_XC) :: pop_XC, bm_XC, mui_XC, mpo_XC,   &
!                                            popold_XC, rand_XC, ui_XC
!     integer(kind=IB) :: i, ibest, iter
!     integer(kind=IB), dimension(NP) :: rot, a1, a2, a3, a4, a5, rt
!     integer(kind=IB), dimension(4) :: ind
!     real(kind=RP) :: tempval
!     real(kind=RP), dimension(NP) :: val
!     real(kind=RP), dimension(Dim_XC) :: bestmemit_XC
!     real(kind=RP), dimension(Dim_XC) :: rand_C1
!     integer(kind=IB), dimension(3), intent(in) :: method
!     external  obj
!     intrinsic max, min, random_number, mod, abs, any, all, maxloc
!     interface
!        function randperm(num)
!           use data_type, only : IB
!           implicit none
!           integer(kind=IB), intent(in) :: num
!           integer(kind=IB), dimension(num) :: randperm
!        end function randperm
!     end interface
!
!     write(*,*)XCmin
!     write(*,*)F_XC,CR_XC,F_CR
!     write(*,*)strategy, refresh, iwrite,method
!
! !!-----Initialize a population --------------------------------------------!!
!        pop_XC=0.0_RP
!        do i=1,NP
!           call random_number(rand_C1)
!           pop_XC(i,:)=XCmin+rand_C1*(XCmax-XCmin)
!        end do
!!!--------------------------------------------------------------------------!!
!
!!!------Evaluate fitness functions and find the best member-----------------!!
!     val=0.0_RP
!     nfeval=0
!     ibest=1
!! ==================================================================================
!     call obj(opt,fdat,pop_XC(1,:),opt%Nvar,val(1),opt%constraints,1,opt%n_constraints)
!!     call obj(pop_XC(1,:), val(1))
!! ==================================================================================
!     bestval=val(1)
!     nfeval=nfeval+1
!     do i=2,NP
!! ==================================================================================
!        call obj(opt,fdat,pop_XC(i,:),opt%Nvar,val(1),opt%constraints,1,opt%n_constraints)
!!        call obj(pop_XC(i,:), val(i))
!! ==================================================================================
!        nfeval=nfeval+1
!        if (val(i) < bestval) then
!           ibest=i
!           bestval=val(i)
!        end if
!     end do
!     bestmemit_XC=pop_XC(ibest,:)
!     bestmem_XC=bestmemit_XC
!!!--------------------------------------------------------------------------!!
!
!     bm_XC=0.0_RP
!     rot=(/(i,i=0,NP-1)/)
!     iter=1
!!!------Perform evolutionary computation------------------------------------!!
!
!     do while (iter <= itermax)
!        popold_XC=pop_XC
!
!!!------Mutation operation--------------------------------------------------!!
!        ind=randperm(4)
!        a1=randperm(NP)
!        rt=mod(rot+ind(1),NP)
!        a2=a1(rt+1)
!        rt=mod(rot+ind(2),NP)
!        a3=a2(rt+1)
!        rt=mod(rot+ind(3),NP)
!        a4=a3(rt+1)
!        rt=mod(rot+ind(4),NP)
!        a5=a4(rt+1)
!        bm_XC=spread(bestmemit_XC, DIM=1, NCOPIES=NP)
!
!!----- Generating a random sacling factor--------------------------------!
!        select case (method(1))
!        case (1)
!           call random_number(F_XC)
!        case(2)
!           call random_number(F_XC)
!           F_XC=2.0_RP*F_XC-1.0_RP
!        end select
!
!!---- select a mutation strategy-----------------------------------------!
!        select case (strategy)
!        case (1)
!           ui_XC=bm_XC+F_XC*(popold_XC(a1,:)-popold_XC(a2,:))
!
!        case default
!           ui_XC=popold_XC(a3,:)+F_XC*(popold_XC(a1,:)-popold_XC(a2,:))
!
!        case (3)
!           ui_XC=popold_XC+F_XC*(bm_XC-popold_XC+popold_XC(a1,:)-popold_XC(a2,:))
!
!        case (4)
!           ui_XC=bm_XC+F_XC*(popold_XC(a1,:)-popold_XC(a2,:)+popold_XC(a3,:)-popold_XC(a4,:))
!
!        case (5)
!           ui_XC=popold_XC(a5,:)+F_XC*(popold_XC(a1,:)-popold_XC(a2,:)+popold_XC(a3,:) &
!                 -popold_XC(a4,:))
!        case (6) ! A linear crossover combination of bm_XC and popold_XC
!           if (method(2) == 1) call random_number(F_CR)
!           ui_XC=popold_XC+F_CR*(bm_XC-popold_XC)+F_XC*(popold_XC(a1,:)-popold_XC(a2,:))
!
!        end select
!!!--------------------------------------------------------------------------!!
!!!------Crossover operation-------------------------------------------------!!
!        call random_number(rand_XC)
!           mui_XC=0.0_RP
!           mpo_XC=0.0_RP
!        where (rand_XC < CR_XC)
!           mui_XC=1.0_RP
!!           mpo_XC=0.0_RP
!        elsewhere
!!           mui_XC=0.0_RP
!           mpo_XC=1.0_RP
!        end where
!
!        ui_XC=popold_XC*mpo_XC+ui_XC*mui_XC
!!!--------------------------------------------------------------------------!!
!!!------Evaluate fitness functions and find the best member-----------------!!
!        do i=1,NP
!!!------Confine each of feasible individuals in the lower-upper bound-------!!
!           ui_XC(i,:)=max(min(ui_XC(i,:),XCmax),XCmin)
!! ==================================================================================
!           call obj(opt,fdat,ui_XC(i,:),opt%Nvar,tempval,opt%constraints,1,opt%n_constraints)
!!           call obj(ui_XC(i,:), tempval)
!! ==================================================================================
!           nfeval=nfeval+1
!           if (tempval < val(i)) then
!              pop_XC(i,:)=ui_XC(i,:)
!              val(i)=tempval
!              if (tempval < bestval) then
!                 bestval=tempval
!                 bestmem_XC=ui_XC(i,:)
!              end if
!           end if
!        end do
!        bestmemit_XC=bestmem_XC
!        if( (refresh > 0) .and. (mod(iter,refresh)==0)) then
!             if (method(3)==1) write(unit=iwrite,FMT=203) iter
!             write(unit=*, FMT=203) iter
!             do i=1,Dim_XC
!                 if (method(3)==1) write(unit=iwrite, FMT=202) i, bestmem_XC(i)
!                 write(*,FMT=202) i,bestmem_XC(i)
!             end do
!             if (method(3)==1) write(unit=iwrite, FMT=201) bestval
!             write(unit=*, FMT=201) bestval
!        end if
!        iter=iter+1
!        if ( bestval <= VTR .and. refresh > 0) then
!           write(unit=iwrite, FMT=*) ' The best fitness is smaller than VTR'
!           write(unit=*, FMT=*) 'The best fitness is smaller than VTR'
!           exit
!        endif
!     end do
!!!------end the evolutionary computation------------------------------!!
!201 format(2x, 'bestval =', ES14.7, /)
!202 format(5x, 'bestmem_XC(', I3, ') =', ES12.5)
!203 format(2x, 'No. of iteration  =', I8)
!end subroutine DE_Fortran90
!
!
!function randperm(num)
!  use data_type, only : IB, RP
!  implicit none
!  integer(kind=IB), intent(in) :: num
!  integer(kind=IB) :: number, i, j, k
!  integer(kind=IB), dimension(num) :: randperm
!  real(kind=RP), dimension(num) :: rand2
!  intrinsic random_number
!  call random_number(rand2)
!  do i=1,num
!     number=1
!     do j=1,num
!        if (rand2(i) > rand2(j)) then
!           number=number+1
!        end if
!     end do
!     do k=1,i-1
!        if (rand2(i) <= rand2(k) .and. rand2(i) >= rand2(k)) then
!           number=number+1
!        end if
!     end do
!     randperm(i)=number
!  end do
!  return
!end function randperm
