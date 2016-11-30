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

      type(employee), allocatable :: filtered(:)
      logical, allocatable        :: repeated(:)

      allocate(repeated(size(employees)))

      repeated = .false.
      filtered = employees

      call Filter_positions(employees, filtered, repeated, unique, occurrences, 1)
   end subroutine ProcessPositions

   ! Рекурсивная фильтрация
   pure recursive subroutine Filter_positions(original, filtered, repeated, unique, occurrences, nth)
      implicit none
      type(employee) :: filtered(:), unique(:), original(:)
      integer        :: occurrences(:), nth
      logical        :: repeated(:)
      intent(inout) filtered, repeated, unique, occurrences
      intent(in) nth, original

      logical, allocatable :: newDuplicates(:)

      unique(nth)%position = filtered(1)%position
      newDuplicates = unique(nth)%position == original(:)%position
      occurrences(nth) = count(newDuplicates)
      repeated = repeated .or. newDuplicates

      filtered = pack(original, repeated .neqv. .true.)

      if (.not. all(repeated)) then
         call Filter_positions(original, filtered, repeated, unique, occurrences, nth + 1)
      endif
   end subroutine Filter_positions

end module Emp_Process