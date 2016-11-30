module Emp_Process
   use Environment
   use Emp_IO
   implicit none

contains

! Подпроцесс для поиска и подсчета повторений профессий
   pure subroutine ProcessPositions(employees, unique, occurrences)
      implicit none

      type(employee) :: employees(:), unique(:)
      integer        :: occurrences(:)
      intent(in) employees
      intent(out) unique, occurrences

      integer                     :: nth
      type(employee), allocatable :: filtered(:)
      logical, allocatable        :: repeated(:), newDuplicates(:)

      allocate(repeated(size(employees)))
      repeated = .false.

      nth = 1
      filtered = employees

      do while (.not. all(repeated))
         nth = nth + 1
         unique(nth)%position = filtered(1)%position
         newDuplicates = unique(nth)%position == employees(:)%position
         occurrences(nth) = count(newDuplicates)
         repeated = repeated .or. newDuplicates

         filtered = pack(employees, repeated .neqv. .true.)
      enddo
   end subroutine ProcessPositions

end module Emp_Process