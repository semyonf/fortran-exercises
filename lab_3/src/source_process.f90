module Source_Process
   use Environment
   use Source_IO

   implicit none

contains

   ! Добавление направления с сортировкой к списку List
   pure recursive function Add_Sorting_To(List) result(SortedList)
      intent(in) List

      type(TextLine)          :: List
      type(TextLine), pointer :: SortedList

      allocate(SortedList)
      SortedList%Characters = List%Characters
      if (Associated(List%NormalNext)) &
         SortedList%NormalNext => Add_Sorting_To(List%NormalNext)
   end function Add_Sorting_To

   ! Подсчет элементов в списке
   pure recursive function Count_Elements_In(List, Current) result(Size)
      intent(in) List, Current

      type(TextLine) :: List
      integer        :: Current, Size

      Size = Current
      if (Associated(List%NormalNext)) &
         Size = Count_Elements_In(List%NormalNext, Current + 1)
   end function Count_Elements_In

   ! Составление массива длин
   pure function Form_Lengths_Of(List) result(Lengths)
      intent(in) List

      type(TextLine)          :: List
      integer                 :: SizeOfList, i
      integer, allocatable    :: Lengths(:)

      SizeOfList = Count_Elements_In(List, 1)
      allocate(Lengths(SizeOfList))

      do concurrent (i = 1:SizeOfList)
         Lengths(i) = len(Get_Chars_At(i, List, 1), CH_)
      end do

   end function Form_Lengths_Of

   ! Получение массива символов из строки Position списка List
   pure recursive function Get_Chars_At(Position, List, Current) result(Chars)
      intent(in) Position, List, Current

      character(:, CH_), allocatable :: Chars
      type(TextLine)                 :: List
      integer                        :: Current, Position

      if (Current .eq. Position) then
         Chars = List%Characters
      else
         if (Associated(List%NormalNext)) then
            Chars = Get_Chars_At(Position, List%NormalNext, Current + 1)
         endif
      endif
   end function Get_Chars_At

end module Source_process
