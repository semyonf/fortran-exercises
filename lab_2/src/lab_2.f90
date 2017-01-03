program lab_2
   use Environment
   use Source_Process
   use Source_IO

   implicit none
   character(:), allocatable :: SourceFile, ConfigFile, OutputFile
   integer                   :: First, Last, K

   type(SourceLine), pointer :: Original   => Null() ! Оригинальный текст
   type(SourceLine), pointer :: MovedLines => Null() ! Текст с перемещенными строками

   ConfigFile = "../data/config.txt"
   SourceFile = "../data/source.txt"
   OutputFile = "output.txt"

   call Read_Config_File(ConfigFile, First, Last, K)

   Original => Read_Source_Code(SourceFile)

   if (Associated(Original)) then
      MovedLines => Move_Lines(Original, First, Last, K)

      if (Associated(MovedLines)) &
         call Output_Source_Code(OutputFile, MovedLines)

   endif

end program lab_2
