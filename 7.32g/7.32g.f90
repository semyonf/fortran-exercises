! 7.32g в учебнике
! Упорядочить элементы в СТРОКАХ матрицы B(10,15) так, чтобы |B(i,j)| <= |B(i,(j+1))|

! --------------------------------------------------------------------
! PROGRAM  Sorting:
!    This program can sort a set of numbers.  The method used is 
! usually referred to as "selection" method.
! --------------------------------------------------------------------

program sorting
    implicit none
    integer, dimension(2, 2) :: b
    integer, dimension(2) :: linen
    integer                  :: j = 1, i = 1

    b = reshape((/ 3, -2, 1, 4 /), shape(b))

    write(*,*) 'Original matrix'
    call outputMatrix2D(b, 2, 2)

    do j = 1, 2
        do i = 1, 2
            linen(i) = b(i,j)
        enddo
         
        call  sort(linen, 2)
        do i = 1, 2
            b(i,j) = linen(i)
        enddo
    enddo

    write(*,*) 'Sorted matrix'
    call outputMatrix2D(b, 2, 2)

contains

! --------------------------------------------------------------------
! INTEGER FUNCTION  FindMinimum():
!    This function returns the location of the minimum in the section
! between Start and End.
! --------------------------------------------------------------------

    integer function  findminimum(x, start, end)
        implicit  none
        integer, dimension(1:), intent(in) :: x
        integer, intent(in)                :: start, end
        integer                            :: minimum
        integer                            :: location
        integer                            :: i

        minimum  = x(start)        ! assume the first is the min
        location = start            ! record its position
        do i = start+1, end        ! start with next elements
            if (abs(x(i)) < abs(minimum)) then    !   if x(i) less than the min?
                minimum  = x(i)        !      yes, a new minimum found
                location = i                !      record its position
            end if
        end do
        findminimum = location            ! return the position
    end function findminimum

! --------------------------------------------------------------------
! SUBROUTINE  Swap():
!    This subroutine swaps the values of its two formal arguments.
! --------------------------------------------------------------------

    subroutine swap(a, b)
        implicit  none
        integer, intent(inout) :: a, b
        integer                :: temp

        temp = a
        a    = b
        b    = temp
    end subroutine swap

! --------------------------------------------------------------------
! SUBROUTINE  Sort():
!    This subroutine receives an array x() and sorts it into ascending
! order.
! --------------------------------------------------------------------

    subroutine sort(x, size)
        implicit  none
        integer, dimension(1:), intent(inout) :: x
        integer, intent(in)                   :: size
        integer                               :: i
        integer                               :: location

        do i = 1, size-1            ! except for the last
            location = findminimum(x, i, size)    ! find min from this to last
            call  swap(x(i), x(location))    ! swap this and the minimum
        end do
    end subroutine  sort

end program sorting


subroutine outputMatrix2D(matrix, X, Y)
    implicit none

    integer :: X, Y, i, j
    integer, dimension(X, Y) :: matrix
    character(10) :: convertedInt = ''
    character(1000) :: line = ''

    do j = 1, Y
        do i = 1, X
            write(convertedInt, '(I5)') matrix(i,j)
            write(line, *) trim(line)//trim(convertedInt)
        enddo
        write(*, *) trim(line)
        line = ''
    enddo

    line = ''

end subroutine outputMatrix2D