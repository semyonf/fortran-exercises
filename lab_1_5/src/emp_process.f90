module Emp_Process
   use Environment
   use Emp_IO
   implicit none

contains

   ! Для получения размера списка (количества сотрудников)
   pure recursive subroutine Count_List_Size(Employee_List, Size)
      type(employee) :: Employee_List
      integer        :: Size

      intent(in)  :: Employee_List
      intent(out) :: Size

      Size = Size + 1

      if (Associated(Employee_List%next)) &
         call Count_List_Size(Employee_List%next, Size)
   end subroutine Count_List_Size

   ! Подпроцесс для поиска и подсчета повторений профессий
   pure subroutine ProcessPositions(Employee_List, unique, occurrences)

      type(employee), pointer :: Employee_List, unique(:)
      integer                 :: occurrences(:)

      intent(in)  :: Employee_List
      intent(out) :: unique, occurrences

      type(employee) :: filtered
      logical, allocatable        :: repeated(:)
      integer                     :: Size

      call Count_List_Size(Employee_List, Size)
      allocate(repeated(size))

      repeated = .false.
      filtered = Employee_List

      call Filter_positions(Employee_List, filtered, repeated, unique, occurrences, 1)
   end subroutine ProcessPositions

   ! Рекурсивная фильтрация
   pure recursive subroutine Filter_positions(original, filtered, repeated, unique, occurrences, nth)
      type(employee) :: filtered(:), unique(:), original(:)
      integer        :: occurrences(:), nth
      logical        :: repeated(:)

      intent(inout) :: filtered, repeated, unique, occurrences
      intent(in)    :: nth, original

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
