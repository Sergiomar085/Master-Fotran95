!-----------------------------------------------------------------------------!
!          Alternative Symmetric sparse matrix manipulation routines          !
!   Authors: Dorival Piedade Neto and Rodrigo Ribeiro Paccola                 !
!   University of Sao Paulo - Sao Carlos Engineering School - October 2014    !
!-----------------------------------------------------------------------------!

module Sparse
    
    implicit none

    type list
        integer(4),dimension(:),allocatable::terms
    end type

    type lists
        type(list),dimension(:),allocatable::terms
    end type

    type sparse_matrix
        integer(4)::nnodes,nelems,nterms,ngl,iflag
        real(8)::ti,tf,tacum
        integer(4),dimension(:),allocatable::len_nodes,len_elems,acum,row,col,vec_,cum
        real(8),dimension(:),allocatable::val
        type(list),dimension(:),allocatable::conec,n_n_incid,n_e_incid
        type(lists),dimension(:),allocatable::conec_new
    end type sparse_matrix

    contains
    
    subroutine prepare_01(sp_matrix, nnodes, nelems, ngl, iflag)
        implicit none

        integer(4)::i
        integer(4),intent(in)::nnodes,nelems,ngl,iflag
        type(sparse_matrix),intent(inout)::sp_matrix

        call cpu_time(sp_matrix%ti)
        
        sp_matrix%nnodes = nnodes
        sp_matrix%nelems = nelems
        sp_matrix%ngl = ngl
        sp_matrix%iflag = iflag

        allocate(sp_matrix%conec(nelems))
        allocate(sp_matrix%len_elems(nelems))
        allocate(sp_matrix%n_n_incid(nnodes))
        allocate(sp_matrix%n_e_incid(nnodes))
        allocate(sp_matrix%len_nodes(nnodes))
        allocate(sp_matrix%acum(nnodes))
        
        allocate(sp_matrix%cum(ngl*nnodes+1))

        allocate(sp_matrix%vec_(ngl+1))
        sp_matrix%vec_ = 0
        do i=2,ngl
            sp_matrix%vec_(i+1) = sp_matrix%vec_(i) + i - 1
        enddo

        call cpu_time(sp_matrix%tf)
        if (sp_matrix%iflag.eq.0) then
            write(*,*)'prepare_01 time: ',sp_matrix%tf - sp_matrix%ti
        end if
    end subroutine prepare_01
    
    subroutine prepare_02(sp_matrix)
        implicit none

        integer(4)::i,j,k,acum,n,ix,ij,jj,counter,accumulated
        integer(4),dimension(:),allocatable::node_aux
        type(sparse_matrix),intent(inout)::sp_matrix

        call cpu_time(sp_matrix%ti)

        do i=1,sp_matrix%nelems
            do j=1,sp_matrix%len_elems(i)
                call add_item(sp_matrix%n_e_incid(sp_matrix%conec(i)%terms(j))%terms,i)
            enddo
        enddo 

        allocate(node_aux(sp_matrix%nnodes))
        acum = 0
        do i=1,sp_matrix%nnodes
            node_aux = 0
            do j=1,size(sp_matrix%n_e_incid(i)%terms)
                do k=1,sp_matrix%len_elems(sp_matrix%n_e_incid(i)%terms(j))
                    if (i.le.sp_matrix%conec(sp_matrix%n_e_incid(i)%terms(j))%terms(k)) then
                        node_aux(sp_matrix%conec(sp_matrix%n_e_incid(i)%terms(j))%terms(k)) = 1
                    endif
                enddo
            enddo
            n = size(pack([(ix,ix=1,sp_matrix%nnodes)],node_aux == 1))
            sp_matrix%len_nodes(i) = n
            acum = acum + n
            sp_matrix%acum(i) = acum
            allocate(sp_matrix%n_n_incid(i)%terms(n))
            sp_matrix%n_n_incid(i)%terms = pack([(ix,ix=1,sp_matrix%nnodes)],node_aux == 1)
        enddo
        deallocate(node_aux)
        
        sp_matrix%nterms = 0
        do i=1,sp_matrix%nnodes
            do ij=1,sp_matrix%ngl
                do j=1,size(sp_matrix%n_n_incid(i)%terms)
                    if (sp_matrix%n_n_incid(i)%terms(j).eq.i) then
                        do jj=ij,sp_matrix%ngl
                            sp_matrix%nterms = sp_matrix%nterms + 1
                        enddo
                    else
                        do jj=1,sp_matrix%ngl
                            sp_matrix%nterms = sp_matrix%nterms + 1
                        enddo
                    endif
                enddo
            enddo
        enddo

        acum = 0
        allocate(sp_matrix%row(sp_matrix%nterms))
        allocate(sp_matrix%col(sp_matrix%nterms))
        allocate(sp_matrix%val(sp_matrix%nterms))
        sp_matrix%val = 0.0d0
        do i=1,sp_matrix%nnodes
            do ij=1,sp_matrix%ngl
                do j=1,size(sp_matrix%n_n_incid(i)%terms)
                    if (sp_matrix%n_n_incid(i)%terms(j).eq.i) then
                        do jj=ij,sp_matrix%ngl
                             acum = acum + 1
                             sp_matrix%row(acum) = (i-1)*sp_matrix%ngl+ij
                             sp_matrix%col(acum) = (sp_matrix%n_n_incid(i)%terms(j)-1)*sp_matrix%ngl+jj
                        enddo
                    else
                        do jj=1,sp_matrix%ngl
                             acum = acum + 1
                             sp_matrix%row(acum) = (i-1)*sp_matrix%ngl+ij
                             sp_matrix%col(acum) = (sp_matrix%n_n_incid(i)%terms(j)-1)*sp_matrix%ngl+jj
                        enddo
                    endif
                enddo
            enddo
        enddo

        allocate(node_aux(1))
        allocate(sp_matrix%conec_new(sp_matrix%nelems))
        do i=1,sp_matrix%nelems
            allocate(sp_matrix%conec_new(i)%terms(sp_matrix%len_elems(i)-1))
            do j=1,sp_matrix%len_elems(i)-1
                allocate(sp_matrix%conec_new(i)%terms(j)%terms(sp_matrix%len_elems(i)-j))
                do k=j+1,sp_matrix%len_elems(i)
                    node_aux = pack([(ix,ix=1,sp_matrix%len_nodes(min(sp_matrix%conec(i)%terms(j),sp_matrix%conec(i)%terms(k))))],sp_matrix%n_n_incid(min(sp_matrix%conec(i)%terms(j),sp_matrix%conec(i)%terms(k)))%terms == max(sp_matrix%conec(i)%terms(j),sp_matrix%conec(i)%terms(k)))
                    sp_matrix%conec_new(i)%terms(j)%terms(k-j) = node_aux(1)
                enddo
            enddo
        enddo
        deallocate(node_aux)
        
        sp_matrix%cum(1) = 1
        counter = 1
        accumulated = 1
        do i=2,sp_matrix%nterms
            accumulated = accumulated + 1
            if (sp_matrix%row(i) > sp_matrix%row(i-1)) then
                counter = counter + 1
                sp_matrix%cum(counter) = accumulated
            end if
        end do
        sp_matrix%cum(sp_matrix%nnodes*sp_matrix%ngl+1) = accumulated + 1
        
        call cpu_time(sp_matrix%tf)
        if (sp_matrix%iflag.eq.0) then
            write(*,*)'prepare_02 time: ',sp_matrix%tf - sp_matrix%ti
        end if
    end subroutine prepare_02
    
    subroutine add_matrix(sp_matrix,id,matrix)
        implicit none
        
        integer(4)::ino,jno,igl,jgl,ia,ib,imin,imax,ngl,acum,pos
        integer(4),intent(in)::id
        real(8),dimension(:,:),allocatable,intent(in)::matrix
        type(sparse_matrix),intent(inout)::sp_matrix

        call cpu_time(sp_matrix%ti)
        
        ngl = sp_matrix%ngl
        do ino=1,sp_matrix%len_elems(id)
            ia = ngl * (ino - 1)
            do jno=1,sp_matrix%len_elems(id)
                ib = ngl * (jno - 1)
                imin = sp_matrix%conec(id)%terms(ino)
                imax = sp_matrix%conec(id)%terms(jno)
                if (imax.ge.imin) then
                    if (imin.eq.1) then
                        acum = 0 
                    else
                        acum = sp_matrix%acum(imin-1) * (ngl ** 2) - (imin - 1) * sp_matrix%vec_(ngl+1) 
                    endif
                    do igl=1,ngl
                        if (imax.eq.imin) then
                            do jgl=igl,ngl
                                pos = acum + sp_matrix%len_nodes(imin) * ngl * (igl - 1) - sp_matrix%vec_(igl) + jgl - igl + 1
                                sp_matrix%val(pos) = sp_matrix%val(pos) + matrix(ia + igl,ib + jgl)
                            enddo
                        else
                            do jgl=1,ngl
                                pos = acum + sp_matrix%len_nodes(imin) * ngl * (igl - 1) - sp_matrix%vec_(igl) + (sp_matrix%conec_new(id)%terms(min(ino,jno))%terms(abs(ino - jno)) - 1) * ngl + jgl - igl + 1
                                sp_matrix%val(pos) = sp_matrix%val(pos) + matrix(ia + igl,ib + jgl)
                            enddo
                        endif
                         !if (imax.eq.imin) then
                         !    do jgl=igl,ngl
                         !        pos = acum + sp_matrix%len_nodes(imin) * ngl * (igl - 1) - sp_matrix%vec_(igl) + jgl - igl + 1
                         !        if (ia + igl .le. ib + jgl) then
                         !            sp_matrix%val(pos) = sp_matrix%val(pos) + matrix(ia + igl,ib + jgl)
                         !        else if (ia + igl .gt. ib + jgl) then
                         !            sp_matrix%val(pos) = sp_matrix%val(pos) + matrix(ib + jgl,ia + igl)
                         !        end if
                         !    enddo
                         !else
                         !    do jgl=1,ngl
                         !        pos = acum + sp_matrix%len_nodes(imin) * ngl * (igl - 1) - sp_matrix%vec_(igl) + &
                         !              (sp_matrix%conec_new(id)%terms(min(ino,jno))%terms(abs(ino - jno)) - 1) * ngl + jgl - igl + 1
                         !        if (ia + igl .le. ib + jgl) then
                         !            sp_matrix%val(pos) = sp_matrix%val(pos) + matrix(ia + igl,ib + jgl)
                         !        else if (ia + igl .gt. ib + jgl) then
                         !            sp_matrix%val(pos) = sp_matrix%val(pos) + matrix(ib + jgl,ia + igl)
                         !        end if
                         !    enddo
                         !endif
                    enddo
                endif
            enddo
        enddo               

        call cpu_time(sp_matrix%tf)
        sp_matrix%tacum = sp_matrix%tacum + sp_matrix%tf - sp_matrix%ti
    end subroutine add_matrix
    
    subroutine add_elem_conec(sp_matrix,id,len,conec)
        implicit none
        
        integer(4),intent(in)::id,len
        integer(4),dimension(:),allocatable,intent(inout)::conec
        type(sparse_matrix),intent(inout)::sp_matrix
        
        call move_alloc(conec,sp_matrix%conec(id)%terms)
        sp_matrix%len_elems(id) = len
        !allocate(sp_matrix%conec(id)%terms(len))
        !sp_matrix%conec(id)%terms = conec
    end subroutine add_elem_conec
    
    subroutine clear_data(sp_matrix)
        implicit none
        
        type(sparse_matrix),intent(inout)::sp_matrix
        
        sp_matrix%val = 0.0d0
        sp_matrix%tacum = 0.0d0
    end subroutine clear_data
    
    subroutine solve_system_of_equation(sp_matrix, vector)
        ! Solves the system of equation A x = b, given by the sp_matrix
        ! (A) and the vector (b), resulting in the solution (x), using the 
        ! direct solver Pardiso from Intel MKL.
        implicit none
        type(sparse_matrix),intent(in)::sp_matrix
        real(8),dimension(sp_matrix%nnodes*sp_matrix%ngl),intent(inout)::vector
        integer(4) :: mtype,maxfct,mnum,phase,n,msglvl,error,nrhs
        integer(8),dimension(:),allocatable :: pt
        integer(4),dimension(:),allocatable :: iparm,perm
        real(8),dimension(:),allocatable :: x
        mtype = -2  !matrix type (-2 <= symmetric indefinite; 2 <= symmetric positive definite)
        maxfct = 1
        mnum = 1
        nrhs = 1
        n = sp_matrix%nnodes*sp_matrix%ngl     !number of equation
        msglvl = 0      !message level information
        allocate(x(n),pt(64),iparm(64),perm(n))
        perm = 0
        call pardisoinit(pt,mtype,iparm)
        phase = 13  !controls the execution of the solver (see MKL reference manual)
        iparm(6) = 1    !The solver stores the solution on 'b' (A.x = b)
        call pardiso(pt,maxfct,mnum,mtype,phase,n,sp_matrix%val,sp_matrix%cum,&
                     sp_matrix%col,perm,nrhs,iparm,msglvl,vector,x,error)
        phase = -1
        call pardiso(pt,maxfct,mnum,mtype,phase,n,sp_matrix%val,sp_matrix%cum,&
                     sp_matrix%col,perm,nrhs,iparm,msglvl,vector,x,error)
    end subroutine solve_system_of_equation  

    subroutine add_item(list, term)
        implicit none
        
        integer(4),dimension(:),allocatable,intent(inout)::list
        integer(4),dimension(:),allocatable::tmp
        integer(4),intent(in) :: term

        if (.not.allocated(list)) then
            allocate(list(1))
            list(1) = term
        else
            call move_alloc(list,tmp)
            allocate(list(size(tmp)+1))
            list(1:size(tmp)) = tmp
            list(size(list)) = term
        end if
    end subroutine add_item
    
    subroutine set_value_to_row(sp_matrix,row,value)
        ! Sets the given value to all sparse matrix terms in the given row.
        implicit none
        type(sparse_matrix), intent(inout)::sp_matrix
        integer(4),intent(in)::row
        real(8),intent(in)::value
        integer(4)::ind, inf, sup
        inf = sp_matrix%cum(row)
        sup = sp_matrix%cum(row+1)-1
        do ind = inf,sup
            sp_matrix%val(ind) = value
        enddo
    end subroutine set_value_to_row

    subroutine set_value_to_col(sp_matrix,col,value)
        ! Sets the given value to all sparse matrix terms in the given column.
        implicit none
        type(sparse_matrix),intent(inout)::sp_matrix
        integer(4),intent(in)::col
        real(8),intent(in)::value
        integer(4)::ind, inf, sup, row_ind
        do row_ind = 1, col
            inf = sp_matrix%cum(row_ind)
            sup = sp_matrix%cum(row_ind+1)-1
            if ((sp_matrix%col(inf).le.col).and.(sp_matrix%col(sup).ge.col)) then
                do ind=inf,sup
                    if (sp_matrix%col(ind).eq.col) then
                        sp_matrix%val(ind) = value
                        exit
                    elseif (sp_matrix%col(ind).gt.col) then
                        exit
                    endif
                end do
            endif
        enddo
    end subroutine set_value_to_col
    
    subroutine set_value_in_term(sp_matrix,row,col,value)
        ! Sets the given value in the sparse matrix term, if it exists
        implicit none
        type(sparse_matrix),intent(inout)::sp_matrix
        integer(4),intent(in)::row, col
        real(8),intent(in)::value
        integer(4)::ind
        do ind=sp_matrix%cum(row),sp_matrix%cum(row+1)-1
            if (sp_matrix%col(ind)==col) then
                sp_matrix%val(ind) = value
                return
            end if
        end do
    end subroutine set_value_in_term
    
end module SPARSE