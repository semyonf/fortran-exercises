module Emp_Process
   use Environment
   use Emp_IO
   implicit none

contains

! Подпроцесс для поиска и подсчета повторений профессий
   pure subroutine ProcessPositions(employees, types, occurrences)
      implicit none

      type(employee) :: employees(:), types(:)
      integer        :: occurrences(:)
      intent(in) employees
      intent(out) types, occurrences

      integer                     :: nth, i, duplicates, records
      type(employee), allocatable :: filtered(:)
      logical, allocatable        :: repeated(:)

      records = size(employees(:))
      allocate(repeated(records))

      nth = 1
      duplicates = 0
      repeated = .false.
      filtered = employees

      do while (.not. all(repeated))
         nth = nth + 1
         types(nth)%position = filtered(1)%position
         occurrences(nth) = count(types(nth)%position == employees(:)%position)
         repeated = repeated .or. types(nth)%position == employees(:)%position
         do i = 1, records
            if (repeated(i) .eqv. .true.) then
               duplicates = duplicates + 1
            else
               filtered(i - duplicates)%position = employees(i)%position
            endif
         enddo
         duplicates = 0
      enddo
   end subroutine ProcessPositions

end module Emp_Process