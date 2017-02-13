 !
 ! В этом модуле располагаются общие процедуры и типы данных
 !

module m_common
   use Environment

   implicit none

   ! "Костыль" для создания массива указателей
   type NextPtr
      type(LineStruct), pointer :: p => Null()
   end type NextPtr

   ! Структура данных для хранения текста
   type LineStruct
      character(:, CH_), allocatable :: Characters
      type(NextPtr)                  :: Next(2)
   end type LineStruct

contains

   ! Удалить List из памяти
   pure recursive subroutine DeleteFromMemory(List)
      intent(inout) List

      type(LineStruct), pointer :: List

      if (Associated(List%Next(1)%p)) &
         call DeleteFromMemory(List%Next(1)%p)
      deallocate(List)
   end subroutine DeleteFromMemory

   ! Перемотка списка к минимальной строчке
   pure subroutine Rewind_To_Shortest(List)
      type(LineStruct), pointer :: List
      integer                   :: Shortest

      Shortest = minloc(Form_Lengths_Of(List), dim=1)

      call Set_Ptr_To_Nth_Of(List, Shortest, List)

   end subroutine Rewind_To_Shortest

   ! Установить указатель Ptr к Counter элементу списка List
   pure recursive subroutine Set_Ptr_To_Nth_Of(Ptr, Counter, List)
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

   ! Подсчет количества Size строк в списке List
   pure recursive subroutine Count_Elements_In(List, Size)
      intent(in) List
      intent(inout) Size

      type(LineStruct) :: List
      integer    :: Size

      Size = Size + 1

      if (Associated(List%Next(1)%p)) &
         call Count_Elements_In(List%Next(1)%p, Size)
   end subroutine Count_Elements_In

   ! Составление массива длин Lengths строк текста List
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

   ! Подсчет длин строк Lengths в тексте List
   pure recursive subroutine Count_Lengths(List, Lengths, IterationsLeft)
      intent(in) List
      intent(inout) IterationsLeft, Lengths

      type(LineStruct)     :: List
      integer, allocatable :: Lengths(:)
      integer              :: IterationsLeft

      Lengths(1 + Size(Lengths) - IterationsLeft) = len(List%Characters)

      IterationsLeft = IterationsLeft - 1
      if (IterationsLeft /= 0) &
         call Count_Lengths(List%Next(1)%p, Lengths, IterationsLeft)
   end subroutine Count_Lengths

end module m_common
