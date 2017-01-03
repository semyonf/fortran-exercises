! Copyright 2015 Fyodorov S. A.

module Source_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Source_IO

   implicit none

contains

   pure recursive function Move_Lines(Original, First, Last, K) result(Moved)
      type(SourceLine), intent(in) :: Original
      type(SourceLine), pointer    :: Moved
      integer                      :: i

      i = 1
   end function Move_Lines

   ! Старое

   ! Формирование разницы двух кодов в виде новых строк.
   pure recursive function Diff_Codes(InitialCode, ModdedCode) result(DiffCode)
      type(SourceLine), pointer     :: DiffCode
      type(SourceLine), intent(in)  :: InitialCode, ModdedCode

      ! Поиск и запись отличных строк в рамках исходного файла InitialCode.
      ! Если строки равны:
      if (InitialCode%String == ModdedCode%String) then
         ! Если остались ещё строки, то переход к следующей.
         if (Associated(InitialCode%next)) then
            DiffCode => Diff_Codes(InitialCode%next, ModdedCode%Next)
         ! В противном случае если остались строки в модифицированном файле, то добавление их в список.
         else if (Associated(ModdedCode%next)) then
            ! Запись всех строк оставшейся части ModdedCode.
            DiffCode => Add_Recent_Source_Lines(ModdedCode%next)
         end if
      ! Если строки не равны, то добавление её в список.
      else
         allocate (DiffCode)
         DiffCode%String = CH__"++ " // ModdedCode%String
         DiffCode%next => Diff_Codes(InitialCode, ModdedCode%Next)
      end if
   end function Diff_Codes

   pure recursive function Add_Recent_Source_Lines(ModdedCode) result(DiffCode)
      type(SourceLine), pointer     :: DiffCode
      type(SourceLine), intent(in)  :: ModdedCode

      allocate (DiffCode)
      DiffCode%String = CH__"++ " // ModdedCode%String
      if (Associated(ModdedCode%next)) &
         DiffCode%next => Add_Recent_Source_Lines(ModdedCode%Next)
   end function Add_Recent_Source_Lines

end module Source_process
