subroutine  objective_M(opt,fdat,x,idim,z,c,odim, cdim)
!
! *** Deterministic fundamental solution (e.g. FEM)
!
!    real*4   :: x(idim)     ! input vectors of dimension "idim"
!    real*4   :: z(odim)     ! result vectors of dimension "odim"
!    integer  :: idim        ! dimension of input vector x
!    integer  :: odim        ! dimension of result vector z
!
! ***
!
    use datmod
    use optmod

    implicit none

    type(global_data) ::  fdat
    type(opt_data)    ::  opt
!
    integer,intent(in)     :: idim,odim,cdim
    real*4,intent(inout)   :: x(idim)
    real*4,intent(out)     :: z(odim)
    real*4,intent(out)     :: c(cdim)

    integer                :: ff, i, k, node_id, elem_id, sec_id, x_id
    real*4                 :: max_val, g_constr(2*cdim)
    real*4                 :: W_ext, M_tot, sigma_max, f_N

!    opt%rho_p = 1.

    do i = 1,opt%n_area
        elem_id = int(opt%area(i,1))
        x_id = int(opt%area(i,4))
        fdat%El_prop(elem_id,4) = x(x_id)
    enddo

    do i = 1,opt%n_x_node
        node_id = int(opt%x_node(i,1))
        x_id = int(opt%x_node(i,4))
        fdat%nodes(node_id,2) = x(x_id)
    enddo

    do i = 1,opt%n_y_node
        node_id = int(opt%y_node(i,1))
        x_id = int(opt%y_node(i,4))
        fdat%nodes(node_id,3) = x(x_id)
    enddo
!    fdat%nodes(5,2) = x(1)
!    fdat%nodes(5,3) = x(2)

    fdat%K_total = 0.0
    fdat%M_total = 0.0

    call stiffness_matrix(fdat)

    M_tot = 0.0
    do i = 1,fdat%n_elements
        M_tot = M_tot + fdat%El_Prop(i,2)*fdat%El_Prop(i,4)*fdat%El_Prop(i,5)
    enddo

!    if (.not. allocated(fdat%R_sup)) allocate (fdat%R_sup(fdat%dmsn,fdat%dmsn))
!    if (.not. allocated(fdat%ind_V)) allocate (fdat%ind_V(fdat%dmsn))
!    if (.not. allocated(fdat%V_incident)) allocate (fdat%V_incident(fdat%dmsn,fdat%dmsn))
    fdat%R_sup = 0.0
    fdat%ind_V = 0
    fdat%V_incident = 0

    call modification_matrices(fdat)

!    if (.not. allocated(fdat%K_m)) allocate (fdat%K_m(fdat%dmsn,fdat%dmsn))
!    if (.not. allocated(fdat%K_mm)) allocate (fdat%K_mm(fdat%dmsn,fdat%dmsn))
!
!    if (.not. allocated(fdat%M_mm)) allocate (fdat%M_mm(fdat%dmsn,fdat%dmsn))
    fdat%K_m = 0.0
    fdat%K_mm = 0.0
    fdat%M_mm = 0.0

    if (sum(fdat%BCs(:,5)) .NE. 0) then
        fdat%K_m = matmul(matmul(fdat%R_sup,fdat%K_total),transpose(fdat%R_sup))
    else
        fdat%K_m = fdat%K_total
    end if

    fdat%K_mm = matmul(matmul(real(fdat%V_incident),fdat%K_m),transpose(real(fdat%V_incident)))

    fdat%M_mm = matmul(matmul(real(fdat%V_incident),fdat%M_total),transpose(real(fdat%V_incident)))
!
!    if (.not. allocated(fdat%D_nodal)) allocate (fdat%D_nodal(fdat%dmsn))
!    if (.not. allocated(fdat%P_nodal)) allocate (fdat%P_nodal(fdat%dmsn))
!
!    if (.not. allocated(fdat%P_ele)) allocate (fdat%P_ele(fdat%n_elements,6))
    fdat%D_nodal = 0.0
    fdat%P_nodal = 0.0

    ff = 0
    call solver(fdat,ff)

!    if (.not. allocated(fdat%F_all)) allocate (fdat%F_all(fdat%n_elements,6))
    fdat%F_all = 0.0

!    if (.not. allocated(fdat%el_diag)) allocate (fdat%el_diag(fdat%acc,5*fdat%n_elements))
!    if (.not. allocated(fdat%eigen_v_diag)) allocate (fdat%eigen_v_diag(fdat%acc*fdat%dmsn,fdat%n_elements))

    call eigen(fdat)
    f_N = fdat%eigen_f(1)

    call element_forces(fdat)

    sigma_max = 0
    do i = 1,fdat%n_elements
        if (abs(fdat%F_all(i)/fdat%El_Prop(i,4)).GT.sigma_max) then
            sigma_max = abs(fdat%F_all(i)/fdat%El_Prop(i,4))
        endif
    enddo

    W_ext = dot_product(fdat%Ext_for,fdat%D_nodal)

    k = 0
    g_constr(:) = 0.0
    do i = 1,opt%n_constraints
        if (opt%constraints(i,1).EQ.1) then
            g_constr(2*k+1) = (opt%constraints(i,2) - W_ext)/(opt%constraints(i,3)-opt%constraints(i,2))
            g_constr(2*k+2) = (W_ext - opt%constraints(i,3))/(opt%constraints(i,3)-opt%constraints(i,2))
        elseif (opt%constraints(i,1).EQ.2) then
            g_constr(2*k+1) = (opt%constraints(i,2) - sigma_max)/(opt%constraints(i,3)-opt%constraints(i,2))
            g_constr(2*k+2) = (sigma_max - opt%constraints(i,3))/(opt%constraints(i,3)-opt%constraints(i,2))
        elseif (opt%constraints(i,1).EQ.3) then
            g_constr(2*k+1) = (opt%constraints(i,2) - f_N)/(opt%constraints(i,3)-opt%constraints(i,2))
            g_constr(2*k+2) = (f_N - opt%constraints(i,3))/(opt%constraints(i,3)-opt%constraints(i,2))
        endif
        k = k + 1
    enddo
!    write(*,*) g_constr, opt%rho_p, sigma_max

    z(1) = M_tot
    do i = 1,2*opt%n_constraints
        if (g_constr(i).LT.0) then
            g_constr(i) = 0.0
        end if
        z(1) = z(1) + opt%rho_p*(g_constr(i))**2
    enddo

    do i = 1,opt%n_constraints
        if (opt%constraints(i,1).EQ.1) then
            c(i) = W_ext
        elseif (opt%constraints(i,1).EQ.2) then
            c(i) = sigma_max
        elseif (opt%constraints(i,1).EQ.3) then
            c(i) = f_N
        endif
    enddo

!-------------------------------------------------------------------------------------------
!
!     1. Quadratische Funktion
!     z(1) = (x(1)-4.0)**2+(x(2)-6.0)**2  ! min bei x1= 4, x2= 6 zopt=0
!     max bei x1=10, x2= 0 zopt=72
!
!
!     2. Zuckerhut
!     z(1)  = 8.0*exp(-(x(1)**2+x(2)**2)) + 2.0*exp(-((x(1)-5.0)**2+(x(2)-4.0)**2)) +x(1)*x(2)/10.0+1.0
!     max bei x(1) = 0.0 x(2) = 0.0 zopt = 9.0
!
!     
!     3. De Jong's funktion
!     z(1) = x(1)**2+x(2)**2 !+x(3)**2+x(4)**2
!     min bei x(i)=0 zopt=0  -5.12<=x(i)<=5.12
!
!     
!     4. Axis parallel hyper-ellipsoid function
!     z(1) = 1.0*x(1)**2 + 2.0*x(2)**2 + 3.0*x(3)**2 + 4.0*x(4)**2 + 5.0*x(5)**2 + 6.0*x(6)**2 + 7.0*x(7)**2 + 8.0*x(8)**2 + 9.0*x(9)**2 + 10.0*x(10)**2
!     min bei x(i)=0 zopt=0  -5.12<=x(i)<=5.12
!
!
!     5. Rotated hyper ellipsoid function
!     z(1) = x(1)**2+(x(1)+x(2))**2 + (x(1)+x(2)+x(3))**2+(x(1)+x(2)+x(3)+x(4))**2+(x(1)+x(2)+x(3)+x(4)+x(5))**2+(x(1)+x(2)+x(3)+x(4)+x(5)+x(6))**2+(x(1)+x(2)+x(3)+x(4)+x(5)+x(6)+x(7))**2+(x(1)+x(2)+x(3)+x(4)+x(5)+x(6)+x(7)+x(8))**2+(x(1)+x(2)+x(3)+x(4)+x(5)+x(6)+x(7)+x(8)+x(9))**2+(x(1)+x(2)+x(3)+x(4)+x(5)+x(6)+x(7)+x(8)+x(9)+x(10))**2
!     min bei x(i)=0  zopt=0  -65<=x(i)<=65
!
! 
!     6. Moved axis parallel hyper-ellipsoid function
!     z(1) = 5.0*x(1)**2+10.0*x(2)**2 !+15.0*x(3)**2+20.0*x(4)**2
!     min bei x(i)=0 zopt=0  -5.12<=x(i)<=5.12
!
!
!     7. Rosenbrock's valley (De Jong's function 2)
!     z(1) = 100.0*(x(2)-x(1)**2)**2+(1.0-x(1))**2 +100.0*(x(3)-x(2)**2)**2+(1.0-x(2))**2+100.0*(x(4)-x(3)**2)**2+(1.0-x(3))**2
!     min bei x(i)=1 zopt=0  -2.048<=x(i)<=2.048
!
!
!     8. Rastrigin's Function n=4
!     z(1) = 2.0*10.0+(x(1)**2-10.0*cos(2.0*3.1415*x(1)))+(x(2)**2-10.0*cos(2.0*3.1415*x(2))) !+(x(3)**2-10.0*cos(2.0*3.1415*x(3)))+(x(4)**2-10.0*cos(2.0*3.1415*x(4)))+(x(5)**2-10.0*cos(2.0*3.1415*x(5)))+(x(6)**2-10.0*cos(2.0*3.1415*x(6)))+(x(7)**2-10.0*cos(2.0*3.1415*x(7)))+(x(8)**2-10.0*cos(2.0*3.1415*x(8)))+(x(9)**2-10.0*cos(2.0*3.1415*x(9)))+(x(10)**2-10.0*cos(2.0*3.1415*x(10)))
!     min bei x(i)=0 zopt=0  -5.12<=x(i)<=5.12
!
!
!     9. Schwefel's function
!     z(1) = -x(1)*sin(sqrt(abs(x(1))))-x(2)*sin(sqrt(abs(x(2))))
!     min bei x(i)=420.9687 zopt=-838.00  -500<=x(i)<=500
!
!
!     10. Griewangk's funtion
!     z(1) = (x(1)**2/4000.0+x(2)**2/4000.0) - cos(x(1)/sqrt(1.0))*cos(x(2)/sqrt(2.0)) + 1
!     min bei x(i)=0 zopt=0 -600<=x(i)<=600
!
!
!     11. Sum of different power function
!     z(1) = abs(x(1))**2 + abs(x(2))**3
!	  min bei x(i)=0 zopt=0 -1<=x(i)<=1
!
!
!     12. Ackley's Funktion n=2 (error!!)
!     z(1) = -20.0*exp(-0.2*sqrt(1.0/2.0*(x(1)**2+x(2)**2)))-exp(1.0/2.0*(cos(2.0*3.1415*x(1))+cos(2.0*3.1415*x(2))))+20.0+exp(1.0)
!     min bei x(i)=0 zopt=0  -1<=x(i)<=1
!
!
!     13. Branins's rcos function
!     z(1) = (x(2)-(5.1/(4.0*3.1415**2))*x(1)**2+5.0/3.1415*x(1)-6.0)**2 + 10.0*(1.0-1.0/(8.0*3.1415))*cos(x(1)) + 10.0
!     min bei (x1,x2) = (-pi,12.275), (pi,2.275), (9.42478,2.475)   zopt=0.397887 -5<=x(i)<=15
!
!
!     14. Easom's Funktion 
!     z(1) = -cos(x(1))*cos(x(2))*exp(-((x(1)-3.1415)**2+(x(2)-3.1415)**2)) 
!     min bei x(1)=pi x(2)=pi  zopt=-1 -3<=x(i)<=5
!
!
!
!     15. Goldstein-Price's Funktion
!     z(1) = (1+(x(1)+x(2)+1)**2*(19-14*x(1)+3*x(1)**2-14*x(2)+6*x(1)*x(2)+3*x(2)**2))*(30+(2*x(1)-3*x(2))**2*(18-32*x(1)+12*x(1)**2+48*x(2)-36*x(1)*x(2)+27*x(2)**2))
!     min bei  (x1,x2)=(0,-1)  zopt=3    -10<=x(i)<=10
!
!
!     16. Six-hump camel back function
!     z(1) = (4.0-2.1*x(1)**2+x(1)**(4.0/3.0))*x(1)**2+x(1)*x(2)+(-4.0+4.0*x(2)**2)*x(2)**2
!     min bei (x1,x2)=(-0.0898,0.7126), (0.0898,-0.7126)  zopt=1.0316 -10<=x(i)<=10
!
!
end subroutine objective_M
