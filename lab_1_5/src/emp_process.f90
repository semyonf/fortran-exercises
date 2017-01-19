module Emp_Process
   use Environment
   use Emp_IO

   implicit none

contains

   ! Функция для вытягивания профессий и их обработки
   pure function Process_Occupations_From(EmployeeList) result(OccupationList)

      intent(in) :: EmployeeList

      type(employee)            :: EmployeeList
      type(occupation), pointer :: OccupationList
      logical, allocatable      :: Repeated(:)
      integer                   :: SizeOfList

      Allocate(OccupationList)
      OccupationList => Get_Occupations(EmployeeList)

      SizeOfList = Count_Occupations_In(OccupationList, 1)
      Allocate(Repeated(SizeOfList))
      Repeated = .false.

      OccupationList => Form_Unique_Occupations(OccupationList, Repeated)

   end function Process_Occupations_From

   ! Функция для формирования списка уникальных профессий с их количеством вхождений
   pure recursive function Form_Unique_Occupations(OccupationList, Repeated) result(Unique_OccupationList)

      intent(in) OccupationList, Repeated

      type(occupation)                  :: OccupationList
      type(occupation), pointer         :: Unique_OccupationList
      logical, allocatable              :: Repeated(:), Duplicates(:), NewRepeated(:)
      character(L_OCCUPATION, kind=CH_) :: NextUniqueName

      if (.not. all(Repeated)) then
         Allocate(Unique_OccupationList)

         NextUniqueName             = Get_Next_Unique_Occupation_Name(OccupationList, Repeated, 1)
         Unique_OccupationList%Name = NextUniqueName

         Allocate(NewRepeated(size(Repeated)))
         NewRepeated = .false.

         Duplicates  = Get_Duplicates_Of(OccupationList, NextUniqueName, 1, NewRepeated)
         Unique_OccupationList%Occurrences = count(Duplicates)
         NewRepeated = Repeated .or. Duplicates

         if (.not. all(NewRepeated)) then
            Unique_OccupationList%Next => Form_Unique_Occupations(OccupationList, NewRepeated)
         endif
      endif
   end function Form_Unique_Occupations

   ! Функция для получения следующего названия неповторявшейся профессии
   pure recursive function Get_Next_Unique_Occupation_Name(OccupationList, Repeated, Current) result(NextUniqueName)

      intent(in) :: OccupationList, Repeated, Current

      type(occupation)                  :: OccupationList
      logical, allocatable              :: Repeated(:)
      integer                           :: Current
      character(L_OCCUPATION, kind=CH_) :: NextUniqueName

      if (.not. Repeated(Current)) then
         NextUniqueName = OccupationList%Name
      else
         if (Associated(OccupationList%Next)) &
            NextUniqueName = Get_Next_Unique_Occupation_Name(OccupationList%Next, Repeated, Current + 1)
      endif
   end function Get_Next_Unique_Occupation_Name

   ! Получение маски, где в списке встретилась указанная профессия
   pure recursive function Get_Duplicates_Of(OccupationList, OccupationName, Current, Repeated) result(NewRepeated)

      intent(in) :: OccupationList, OccupationName, Current, Repeated

      type(occupation)                  :: OccupationList
      logical, allocatable              :: NewRepeated(:), Repeated(:)
      character(L_OCCUPATION, kind=CH_) :: OccupationName
      integer                           :: Current

      NewRepeated = Repeated
      if (OccupationList%Name == OccupationName) &
         NewRepeated(Current) = .true.

      if (Associated(OccupationList%Next)) &
         NewRepeated = Get_Duplicates_Of(OccupationList%Next, OccupationName, Current + 1, NewRepeated)
   end function Get_Duplicates_Of

   ! Получение списка профессий из списка сотрудников EmployeeList
   pure recursive function Get_Occupations(EmployeeList) result(OccupationList)

      intent(in) :: EmployeeList

      type(employee)            :: EmployeeList
      type(occupation), pointer :: OccupationList

      Allocate(OccupationList)
      OccupationList%Name = EmployeeList%Occupation

      if (Associated(EmployeeList%Next)) &
         OccupationList%Next => Get_Occupations(EmployeeList%Next)
   end function Get_Occupations

   ! Подсчет количества записей в списке OccupationList
   pure recursive function Count_Occupations_In(OccupationList, Current) result(Size)

      intent(in) OccupationList, Current

      type(occupation) :: OccupationList
      integer        :: Current, Size

      Size = Current
      if (Associated(OccupationList%Next)) &
         Size = Count_Occupations_In(OccupationList%Next, Current + 1)
   end function Count_Occupations_In

end module Emp_Process
