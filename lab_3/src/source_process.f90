module Source_Process
   use Environment
   use Source_IO

   implicit none

contains

   ! Добавление сортировки по длине строк к списку
   pure function Add_Sorting_To(List) result(SortedList)
      intent(in) List

      type(TextLine)          :: List
      type(TextLine), pointer :: SortedList
      integer                 :: SizeOfList
      integer, allocatable    :: LenghtsOfLines(:)

      SizeOfList = Count_Elements_In(List, 0)
      allocate (SortedList, LenghtsOfLines(SizeOfList))
   end function Add_Sorting_To

   ! Подсчет элементов в списке
   pure recursive function Count_Elements_In(List, Current) result(Size)
      intent(in) List, Current

      type(TextLine) :: List
      integer        :: Current, Size

      Size = Current + 1
      if (Associated(List%NormalNext)) &
         Size = Count_Elements_In(List%NormalNext, Size)
   end function Count_Elements_In

   ! Сортировка списка
   pure recursive function Sort_List(List, Current) result(Size)
      intent(in) List, Current

      type(TextLine) :: List
      integer        :: Current, Size

      Size = Current + 1
      if (Associated(List%NormalNext)) &
         Size = Sort_List(List%NormalNext, Size)
   end function Sort_List

end module Source_process
