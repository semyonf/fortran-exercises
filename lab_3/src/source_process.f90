module Source_Process
   use Environment
   use Source_IO

   implicit none

contains

   subroutine Process(List)
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
   recursive subroutine Add_Sorted_Ptrs(List, Lengths, Available, Original)
      intent(inout) List, Available
      intent(in)    Lengths, Original

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
   recursive subroutine Find_Next_Smallest_Location(Lengths, Available, Location)
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

   ! Установить указатель к Н-ному элементу по порядку
   recursive subroutine Set_Ptr_To_Nth_Of(Ptr, Counter, List)
      intent(in)    List
      intent(out)   Ptr
      intent(inout) Counter

      type(LineStruct), pointer :: List
      type(LineStruct), pointer :: Ptr
      integer                   :: Counter

      if (Counter /= 1) then
         Counter = Counter - 1
         call Set_Ptr_To_Nth_Of(Ptr, Counter, List%Next(1)%p)
      else
         Ptr => List
      endif
   end subroutine Set_Ptr_To_Nth_Of

! ---------Формирование массива длин---------

   ! Подсчет строк в тексте List
   pure recursive subroutine Count_Elements_In(List, Size)
      intent(in) List
      intent(inout) Size

      type(LineStruct) :: List
      integer    :: Size

      Size = Size + 1

      if (Associated(List%Next(1)%p)) &
         call Count_Elements_In(List%Next(1)%p, Size)
   end subroutine Count_Elements_In

   ! Составление массива длин строк текста List
   pure function Form_Lengths_Of(List) result(Lengths)
      intent(in) List

      type(LineStruct)     :: List
      integer              :: Size
      integer, allocatable :: Lengths(:)

      Size = 0
      call Count_Elements_In(List, Size)
      allocate(Lengths(Size))

      call Count_Lengths(List, Lengths, Size)
   end function Form_Lengths_Of

   ! Подсчет длин строк в тексте List
   pure recursive subroutine Count_Lengths(List, Lengths, IterationsLeft)
      intent(in) List
      intent(inout) IterationsLeft, Lengths

      type(LineStruct)           :: List
      integer, allocatable :: Lengths(:)
      integer              :: IterationsLeft

      Lengths(1 + Size(Lengths) - IterationsLeft) = len(List%Characters)

      IterationsLeft = IterationsLeft - 1
      if (IterationsLeft /= 0) &
         call Count_Lengths(List%Next(1)%p, Lengths, IterationsLeft)
   end subroutine Count_Lengths

end module Source_process
