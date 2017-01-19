module Emp_Process
   use Environment
   use Emp_IO

   implicit none

contains

   ! Функция для вытягивания профессий и количества их вхождений из списка сотрудников
   pure function Process_Occupations_From(EmployeeList) result(OccupationList)

      intent(in) :: EmployeeList

      type(employee)            :: EmployeeList
      type(occupation), pointer :: OccupationList

      allocate(OccupationList)

      OccupationList%name = 'test'
      OccupationList%Occurrences = 5

      ! call Find_Unique_Occupations(EmployeeList, OccupationList)

   end function Process_Occupations_From

   ! Формирование списка уникальных профессий OccupationList в списке сотрудников EmployeeList
   pure recursive subroutine Find_Unique_Occupations(EmployeeList, OccupationList)

      intent(in)    :: EmployeeList
      intent(inout) :: OccupationList

      type(employee)            :: EmployeeList
      type(occupation), pointer :: OccupationList
      integer                   :: Occurrences

      Occurrences = Count_Occupation_Occurrences_In(OccupationList, EmployeeList%Occupation, 0)

      if (Occurrences .eq. 0) then
         allocate(OccupationList)
         OccupationList%Name = EmployeeList%Occupation
      endif

      if (Associated(EmployeeList%Next)) &
         call Find_Unique_Occupations(EmployeeList%Next, OccupationList%Next)

   end subroutine Find_Unique_Occupations

   ! Подсчет вхождений профессии OccupationName в списке профессий OccupationList
   pure recursive function Count_Occupation_Occurrences_In(OccupationList, OccupationName, Current) result(Occurrences)

      intent(in) :: OccupationList, OccupationName, Current

      character(L_OCCUPATION, kind=CH_) :: OccupationName
      type(occupation)                  :: OccupationList
      integer                           :: Current, Occurrences

      Occurrences = Current

      if (Associated(OccupationList%Next)) &
         Occurrences = Count_Occupation_Occurrences_In(OccupationList%Next, OccupationName, Current + 1)
   end function Count_Occupation_Occurrences_In

   ! ! Подсчет количества сотрудников в списке EmployeeList
   ! pure recursive function Count_Elements_In(EmployeeList, Current) result(Size)

   !    intent(in) EmployeeList, Current

   !    type(employee) :: EmployeeList
   !    integer        :: Current, Size

   !    Size = Current
   !    if (Associated(EmployeeList%Next)) &
   !       Size = Count_Elements_In(EmployeeList%Next, Current + 1)
   ! end function Count_Elements_In

   ! ! Собрать OccupationList, содержащий UniqueOccupaions и количество их вхождений в EmployeeList
   ! pure recursive function Get_Unique_Occupations_Occurences_List(EmployeeList, OccupationName, Current) result(Occurrences)

   !    intent(in) EmployeeList, OccupationName, Current

   !    character(L_OCCUPATION, kind=CH_) :: OccupationName
   !    type(employee)                    :: EmployeeList
   !    integer                           :: Current, Occurrences

   !    Occurrences = Current

   !    if (Associated(EmployeeList%Next)) &
   !       Occurrences = Get_Unique_Occupations_Occurences_List(EmployeeList%Next, OccupationName, Current + 1)
   ! end function Get_Unique_Occupations_Occurences_List

end module Emp_Process
