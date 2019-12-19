    subroutine get_items(file_name,fdat) !,nodes,elements,sections,BCs,forces)
    use datmod
    implicit none

    type(global_data)::  fdat

    integer :: i
    character(len=20), intent(in) :: file_name
    character(len=6) :: val1

    val1 = 'something'

    OPEN(UNIT = 10, FILE = file_name, STATUS='UNKNOWN')
    i=0
    do while (val1 .NE. '*END')
        read (10, *) val1
        if (val1 == '*NODES') then
            fdat%item(1) = i+1
        else if (val1 == '*ELEME') then
            fdat%item(2) = i+1
        else if (val1 == '*MATER') then
            fdat%item(3) = i+1
        else if (val1 == '*SECTI') then
            fdat%item(4) = i+1
        else if (val1 == '*BC   ') then
            fdat%item(5) = i+1
        else if (val1 == '*FOR') then
            fdat%item(6) = i+1
        else if (val1 == '*OPT_V') then
            fdat%item(7) = i+1
        else if (val1 == '*OBJEC') then
            fdat%item(8) = i+1
        else if (val1 == '*CONST') then
            fdat%item(9) = i+1
        else if (val1 == '*PARAM') then
            fdat%item(10) = i+1
        else if (val1 == '*OPT_P') then
            fdat%item(11) = i+1
        end if
        i = i+1
    end do
    CLOSE(10)
    fdat%item(12) = i

    return
    end subroutine get_items

    subroutine read_data(file_name,fdat,opt)
    use datmod
    use optmod
    implicit none

    type(global_data) ::  fdat
    type(opt_data)    ::  opt

    integer :: i,j, ia,ix,iy, n_con
    character(len=20), intent(in) :: file_name

    OPEN(UNIT = 10,FILE = file_name, STATUS='UNKNOWN')
    read (10, *)
    j = 1
    do i = fdat%item(1)+1,fdat%item(2)-1
        read (10, *) fdat%nodes(j,:)
        j = j + 1
    end do

    read (10, *)
    j = 1
    do i = fdat%item(2)+1,fdat%item(3)-1
        read (10, *) fdat%elements(j,:)
        j = j + 1
    end do

    read (10, *)
    j = 1
    do i = fdat%item(3)+1,fdat%item(4)-1
        read (10, *) fdat%materials(j,:)
        j = j + 1
    end do

    read (10, *)
    j = 1
    do i = fdat%item(4)+1,fdat%item(5)-1
        read (10, *) fdat%sections(j,:)
        j = j + 1
    end do

    read (10, *)
    j = 1
    do i = fdat%item(5)+1,fdat%item(6)-1
        read (10, *) fdat%BCs(j,:)
        j = j + 1
    end do

    read (10, *)
    j = 1
    do i = fdat%item(6)+1,fdat%item(7)-1
        read (10, *) fdat%forces_n(j,:)
        j = j + 1
    end do

    read (10, *)
    j = 1
    do i = fdat%item(7)+1,fdat%item(8)-1
        read (10, *) opt%varopt(j,:)
        j = j + 1
    end do

    read (10, *)
    read (10, *) opt%object

    read (10, *)
    j = 1
    do i = fdat%item(9)+1,fdat%item(10)-1
        read (10, *) opt%constraints_tmp(j,:)
        j = j + 1
    end do

    read (10, *)
    read (10, *) fdat%parameters(:)

    read (10, *)
    read (10, *) opt%opt_par(:)

    CLOSE(10)

    opt%n_area   = 0
    opt%n_x_node = 0
    opt%n_y_node = 0

    do i = 1,opt%n_varopt
        if (opt%varopt(i,2) .EQ. 0) then
            opt%n_area = opt%n_area + 1
        else if (opt%varopt(i,2) .EQ. 1) then
            opt%n_x_node = opt%n_x_node + 1
        else if (opt%varopt(i,2) .EQ. 2) then
            opt%n_y_node = opt%n_y_node + 1
        endif
    enddo

    if (.not. allocated(opt%area))   allocate (opt%area(opt%n_area,4))
    if (.not. allocated(opt%x_node)) allocate (opt%x_node(opt%n_x_node,4))
    if (.not. allocated(opt%y_node)) allocate (opt%y_node(opt%n_y_node,4))

    opt%Nvar = int(maxval(opt%varopt(:,1)))
    ia = 0
    ix = 0
    iy = 0
    do i = 1,opt%n_varopt
        if (opt%varopt(i,2) .EQ. 0) then
            ia = ia + 1
            opt%area(ia,1) = opt%varopt(i,3)
            opt%area(ia,2) = opt%varopt(i,4)
            opt%area(ia,3) = opt%varopt(i,5)
            opt%area(ia,4) = opt%varopt(i,1)
        else if (opt%varopt(i,2) .EQ. 1) then
            ix = ix + 1
            opt%x_node(ix,1) = opt%varopt(i,3)
            opt%x_node(ix,2) = opt%varopt(i,4)
            opt%x_node(ix,3) = opt%varopt(i,5)
            opt%x_node(ix,4) = opt%varopt(i,1)
        else if (opt%varopt(i,2) .EQ. 2) then
            iy = iy + 1
            opt%y_node(iy,1) = opt%varopt(i,3)
            opt%y_node(iy,2) = opt%varopt(i,4)
            opt%y_node(iy,3) = opt%varopt(i,5)
            opt%y_node(iy,4) = opt%varopt(i,1)
        endif
    enddo

!    opt%constraints = opt%constraints_tmp

    n_con = opt%n_constraints
    if (opt%object.EQ.1) then
        do i = 1,n_con
            if ((opt%constraints_tmp(i,1).EQ.1).OR.(opt%constraints_tmp(i,1).GT.4)) then
                opt%constraints_tmp(i,1) = 0
            else if (opt%constraints_tmp(i,1).EQ.2) then
                opt%constraints_tmp(i,1) = 1
            else if (opt%constraints_tmp(i,1).EQ.3) then
                opt%constraints_tmp(i,1) = 2
            else if (opt%constraints_tmp(i,1).EQ.4) then
                opt%constraints_tmp(i,1) = 3
            end if
        enddo
    else if (opt%object.EQ.2) then
        do i = 1,n_con
            if ((opt%constraints_tmp(i,1).EQ.2).OR.(opt%constraints_tmp(i,1).GT.4)) then
                opt%constraints_tmp(i,1) = 0
            else if (opt%constraints_tmp(i,1).EQ.3) then
                opt%constraints_tmp(i,1) = 2
            else if (opt%constraints_tmp(i,1).EQ.4) then
                opt%constraints_tmp(i,1) = 3
            end if
        enddo
    end if

    do i = 1,n_con
        if (opt%constraints_tmp(i,1).EQ.0) then
            opt%n_constraints = opt%n_constraints - 1
        end if
    enddo

    if (.not. allocated(opt%constraints)) allocate (opt%constraints(opt%n_constraints,3))

    j = 1
    do i = 1,n_con
        if (opt%constraints_tmp(i,1).NE.0) then
            opt%constraints(j,:) = opt%constraints_tmp(i,:)
            j = j + 1
        endif
    enddo

!     do i=1,opt%n_constraints
!        write (*,*) opt%constraints(i,:)
!     enddo

    return
    end subroutine read_data
