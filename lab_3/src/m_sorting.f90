module m_sorting
   use Environment
   use m_common
   use m_io

   implicit none

contains

   pure subroutine Process(List)
      intent(inout) List

      type(LineStruct), pointer :: List
      integer, allocatable      :: Lengths(:)
      logical, allocatable      :: Available(:)

      Lengths = Form_Lengths_Of(List)
      allocate(Available(Size(Lengths)))
      Available = .true.

      call Add_Sorted_Ptrs(List, Lengths, Available, List)
   end subroutine Process

   ! Сделать сортировку
   pure recursive subroutine Add_Sorted_Ptrs(List, Lengths, Available, Original)
      intent(inout) List, Available
      intent(in)    Lengths

      type(LineStruct), pointer :: List, Original
      integer                   :: Location
      integer, allocatable      :: Lengths(:)
      logical, allocatable      :: Available(:)

      call Find_Next_Smallest_Location(Lengths, Available, Location)
      call Set_Ptr_To_Nth_Of(List%Next(2)%p, Location, Original)

      if (count(.not. Available) < Size(Lengths) - 1) &
         call Add_Sorted_Ptrs(List%Next(2)%p, Lengths, Available, Original)
   end subroutine Add_Sorted_Ptrs

   ! Найти позицию по проядку следующего минимального элемента
   pure recursive subroutine Find_Next_Smallest_Location(Lengths, Available, Location)
      intent(in)    Lengths
      intent(out)   Location
      intent(inout) Available

      integer, allocatable :: Lengths(:)
      logical, allocatable :: Available(:)
      integer              :: Location

      Location = minloc(Lengths, dim=1, mask=Available)

      if (Location == 1 .and. all(Available)) then
         Available(Location) = .false.
         call Find_Next_Smallest_Location(Lengths, Available, Location)
      endif

      Available(Location) = .false.
   end subroutine Find_Next_Smallest_Location

end module m_sorting
