module Source_Process
   use Environment
   use Source_IO

   implicit none

contains

   pure function Add_Sorting_To(List) result(SizeOfList)

      intent(in) List

      integer                 :: SizeOfList
      type(TextLine)          :: List
      ! type(TextLine), pointer :: SortedList

      SizeOfList = Count_Elements_In(List, 0)

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

end module Source_process
