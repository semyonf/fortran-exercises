! 7.32g в учебнике
! Упорядочить элементы в СТРОКАХ матрицы B(10,15) так, чтобы |B(i,j)| <= |B(i,(j+1))|

! --------------------------------------------------------------------
! PROGRAM  Sorting:
!    This program can sort a set of numbers.  The method used is 
! usually referred to as "selection" method.
! --------------------------------------------------------------------

PROGRAM  Sorting
   IMPLICIT NONE
   INTEGER, DIMENSION(2, 2) :: B
   ! INTEGER, DIMENSION(2)    :: line
   INTEGER, PARAMETER       :: MAX_SIZE = 100
   INTEGER                  :: i = 1, j
   character(5)            :: temp = ' s'
   character(500)            :: line = ''

   B = reshape((/ 3, -2, 1, 4 /), shape(B))

   ! write(temp, *) 'test'
   do i = 1, 5
       write(line, *) trim(line) // '12'
   enddo

   write(*,*) line

   ! do j = 1, 2
   !     do i = 1, 2
   !         write (temp, *) B(i,j) ! converting integer to string using a 'internal file'
   !         line = line // temp
   !     enddo
   !     write(*,*) line
       
   ! enddo

   ! do j = 1, 2
   !     do i = 1, 2
   !         line(i) = B(i,j)
   !     enddo
       
   !     CALL  Sort(line, 2)
   !     do i = 1, 2
   !         B(i,j) = line(i)
   !     enddo
   ! enddo

   ! WRITE(*,*)

CONTAINS

! --------------------------------------------------------------------
! INTEGER FUNCTION  FindMinimum():
!    This function returns the location of the minimum in the section
! between Start and End.
! --------------------------------------------------------------------

   INTEGER FUNCTION  FindMinimum(x, Start, End)
      IMPLICIT  NONE
      INTEGER, DIMENSION(1:), INTENT(IN) :: x
      INTEGER, INTENT(IN)                :: Start, End
      INTEGER                            :: Minimum
      INTEGER                            :: Location
      INTEGER                            :: i

      Minimum  = x(Start)        ! assume the first is the min
      Location = Start            ! record its position
      DO i = Start+1, End        ! start with next elements
         IF (ABS(x(i)) < ABS(Minimum)) THEN    !   if x(i) less than the min?
            Minimum  = x(i)        !      Yes, a new minimum found
            Location = i                !      record its position
         END IF
      END DO
      FindMinimum = Location            ! return the position
   END FUNCTION  FindMinimum

! --------------------------------------------------------------------
! SUBROUTINE  Swap():
!    This subroutine swaps the values of its two formal arguments.
! --------------------------------------------------------------------

   SUBROUTINE  Swap(a, b)
      IMPLICIT  NONE
      INTEGER, INTENT(INOUT) :: a, b
      INTEGER                :: Temp

      Temp = a
      a    = b
      b    = Temp
   END SUBROUTINE  Swap

! --------------------------------------------------------------------
! SUBROUTINE  Sort():
!    This subroutine receives an array x() and sorts it into ascending
! order.
! --------------------------------------------------------------------

   SUBROUTINE  Sort(x, Size)
      IMPLICIT  NONE
      INTEGER, DIMENSION(1:), INTENT(INOUT) :: x
      INTEGER, INTENT(IN)                   :: Size
      INTEGER                               :: i
      INTEGER                               :: Location

      DO i = 1, Size-1            ! except for the last
         Location = FindMinimum(x, i, Size)    ! find min from this to last
         CALL  Swap(x(i), x(Location))    ! swap this and the minimum
      END DO
   END SUBROUTINE  Sort

END PROGRAM  Sorting