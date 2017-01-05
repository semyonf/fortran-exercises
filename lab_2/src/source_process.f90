module Source_Process
   use Environment
   use Source_IO

   implicit none

contains

   pure function Move_Lines(Original, First, Last, K) result(Moving)
      integer, intent(in)        :: First, Last, K
      type(TextLine), intent(in) :: Original
      type(TextLine), pointer    :: Moving
      integer :: test

      test = K

      Moving => Form_Block(Original, First, Last, 1)
   end function Move_Lines

   ! Формирование блока переносимых строк
   pure recursive function Form_Block(Original, Start, End, Current) result(Block)
      type(TextLine), pointer     :: Block
      type(TextLine), intent(in)  :: Original
      integer, intent(in)         :: Start, End, Current

      allocate (Block)

      if (Start <= Current) then
         Block%String = Original%String
         if (Current < End .and. Associated(Original%next)) then
               Block%next => Form_Block(Original%next, Start, End, Current + 1)
         endif
      else
         if (Associated(Original%next)) then
            Block => Form_Block(Original%next, Start, End, Current + 1)
         endif
      endif

   end function Form_Block

   ! pure recursive function Add_Recent_Source_Lines(ModdedCode) result(DiffCode)
   !    type(TextLine), pointer     :: DiffCode
   !    type(TextLine), intent(in)  :: ModdedCode

   !    allocate (DiffCode)
   !    DiffCode%String = CH__"++ " // ModdedCode%String
   !    if (Associated(ModdedCode%next)) &
   !       DiffCode%next => Add_Recent_Source_Lines(ModdedCode%Next)
   ! end function Add_Recent_Source_Lines


   ! pure recursive function Add_Recent_Source_Lines(ModdedCode) result(DiffCode)
   !    type(TextLine), pointer     :: DiffCode
   !    type(TextLine), intent(in)  :: ModdedCode

   !    allocate (DiffCode)
   !    DiffCode%String = CH__"++ " // ModdedCode%String
   !    if (Associated(ModdedCode%next)) &
   !       DiffCode%next => Add_Recent_Source_Lines(ModdedCode%Next)
   ! end function Add_Recent_Source_Lines


   ! ! Формирование разницы двух кодов в виде новых строк.
   ! pure recursive function Diff_Codes(InitialCode, ModdedCode) result(DiffCode)
   !    type(TextLine), pointer     :: DiffCode
   !    type(TextLine), intent(in)  :: InitialCode, ModdedCode

   !    ! Поиск и запись отличных строк в рамках исходного файла InitialCode.
   !    ! Если строки равны:
   !    if (InitialCode%String == ModdedCode%String) then
   !       ! Если остались ещё строки, то переход к следующей.
   !       if (Associated(InitialCode%next)) then
   !          DiffCode => Diff_Codes(InitialCode%next, ModdedCode%Next)
   !       ! В противном случае если остались строки в модифицированном файле, то добавление их в список.
   !       else if (Associated(ModdedCode%next)) then
   !          ! Запись всех строк оставшейся части ModdedCode.
   !          DiffCode => Add_Recent_Source_Lines(ModdedCode%next)
   !       end if
   !    ! Если строки не равны, то добавление её в список.
   !    else
   !       allocate (DiffCode)
   !       DiffCode%String = CH__"++ " // ModdedCode%String
   !       DiffCode%next => Diff_Codes(InitialCode, ModdedCode%Next)
   !    end if
   ! end function Diff_Codes

end module Source_process
