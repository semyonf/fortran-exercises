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
      logical, allocatable      :: Repeated(:)
      integer                   :: SizeOfList

      Allocate(OccupationList)
      OccupationList => Get_Occupations(EmployeeList)

      SizeOfList = Count_Occupations_In(OccupationList, 1)
      Allocate(Repeated(SizeOfList))
      Repeated = .false.

      OccupationList => Form_Unique_Occupations(OccupationList, 1, Repeated)

   end function Process_Occupations_From


   pure recursive function Form_Unique_Occupations(OccupationList, Current, Repeated) result(Unique_OccupationList)

      intent(in) OccupationList, Current, Repeated

      type(occupation)          :: OccupationList
      type(occupation), pointer :: Unique_OccupationList
      logical, allocatable      :: Repeated(:), Duplicates(:)
      integer                   :: Current

      if (.not. Repeated(Current)) then
         Duplicates = Get_Duplicates_Of(OccupationList, OccupationList%Name, 1, Repeated)
         Allocate(Unique_OccupationList)

         Unique_OccupationList%Name        = OccupationList%Name
         Unique_OccupationList%Occurrences = count(Duplicates)

         Duplicates = Repeated .or. Duplicates
      else
         Duplicates = Repeated
      endif

      if (Associated(OccupationList%Next)) &
         Unique_OccupationList%Next => Form_Unique_Occupations(OccupationList%Next, Current + 1, Duplicates)

   end function Form_Unique_Occupations


   ! Получение логического массива с элементами true там, где в списке указанная профессия встретилась
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


   ! ! Функция для удаления из списка уже повторившихся профессий
   ! pure recursive function Remove_Repeated(OccupationList, Unique_OccupationList, Repeated, Current) result(Unique_OccupationList)

   !    intent(in) :: OccupationList, Repeated, Current, Unique_OccupationList

   !    type(occupation)          :: OccupationList
   !    type(occupation), pointer :: Unique_OccupationList
   !    logical, allocatable      :: Repeated(:), NewRepeated(:)
   !    integer                   :: Current

   !    if (Repeated(Current) .eq. .false.) then
   !       Allocate(Unique_OccupationList)
   !       Unique_OccupationList%Name = OccupationList%Name
   !       if (Associated(OccupationList%Next)) &
   !          OccupationList%Next => Remove_Repeated(OccupationList%Next, Unique_OccupationList%Next, Repeated, Current + 1)
   !    endif
   ! end function Remove_Repeated









































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



























   ! ! Подсчет вхождений профессии OccupationName в списке профессий OccupationList
   ! pure recursive function Count_Occupation_Occurrences_In(OccupationList, OccupationName, Current) result(Occurrences)

   !    intent(in) :: OccupationList, OccupationName, Current

   !    character(L_OCCUPATION, kind=CH_) :: OccupationName
   !    type(occupation)                  :: OccupationList
   !    integer                           :: Current, Occurrences

   !    Occurrences = Current

   !    if (Associated(OccupationList%Next)) &
   !       Occurrences = Count_Occupation_Occurrences_In(OccupationList%Next, OccupationName, Current + 1)
   ! end function Count_Occupation_Occurrences_In

end module Emp_Process
