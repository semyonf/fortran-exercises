program lab_2
   use Environment
   use Source_IO
   use Source_Process

   implicit none

   character(:), allocatable :: SourceFile, ConfigFile, OutputFile
   integer                   :: First, Last, K

   type(TextLine), pointer :: Original   => Null() ! Оригинальный текст
   type(TextLine), pointer :: MovedLines => Null() ! Текст с перемещенными строками

   ConfigFile = "../data/config.txt"
   SourceFile = "../data/source.txt"
   OutputFile = "output.txt"

   call Read_Config_File(ConfigFile, First, Last, K)

   Original => Read_Source(SourceFile)

   if (Associated(Original)) then
      MovedLines => Move_Lines(Original, First, Last, K)

      if (Associated(MovedLines)) &
         call Output_Source_Code(OutputFile, MovedLines)

   endif

end program lab_2
