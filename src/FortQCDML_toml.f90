module FortQCDML_toml
  use FortQCDML_base, only: qcdml_config, &
       qcdml_archive_event, qcdml_parameter, qcdml_markov_step, qcdml_record
   use tomlf, only: toml_table, toml_array, toml_error, toml_load, get_value, len
   implicit none(type, external)
   private

   public :: read_qcdml_toml

contains

   subroutine read_qcdml_toml(filename, config)
      character(*),       intent(in)  :: filename
      type(qcdml_config), intent(out) :: config

      type(toml_table), allocatable :: root
      type(toml_error), allocatable :: error

      type(toml_table), pointer :: management
      type(toml_table), pointer :: implementation
      type(toml_table), pointer :: machine
      type(toml_table), pointer :: code
      type(toml_table), pointer :: algorithm
      type(toml_table), pointer :: markov_sequence
      type(toml_table), pointer :: additional_info
      call toml_load(root, filename, error=error)
      if (allocated(error)) then
         error stop error%message
      end if
      !-----------------------------------------------------------
      ! Top-level values
      !-----------------------------------------------------------
      call get_value(root, "data_lfn", config%data_lfn)
      call get_value(root, "precision", config%precision)
      !-----------------------------------------------------------
      ! Management
      !----------------------------------------------------------
      nullify(management)
      call get_value(root, "management", management)
      if (.not. associated(management)) then
         error stop "Missing required [management] table"
      end if
      call get_value( management, "annotation", config%management_annotation, default="")
      call get_value( management, "revisions", config%revisions, default="")
      call get_value( management, "reference", config%reference, default="")
      call read_archive_events( management, config%archive_events)
      !-----------------------------------------------------------
      ! Implementation
      !-----------------------------------------------------------
      nullify(implementation)
      call get_value(root, "implementation", implementation)
      if (.not. associated(implementation)) then
         error stop "Missing required [implementation] table"
      end if
      call get_value( implementation, "annotation", config%implementation_annotation, default="")
      !------------------------------------------------------------
      ! Implementation: machine
      !------------------------------------------------------------
      nullify(machine)
      call get_value(implementation, "machine", machine)
      if (.not. associated(machine)) then
         error stop "Missing required [implementation.machine] table"
      end if
      call get_value( machine, "annotation", config%machine%annotation, default="")
      call get_value( machine, "name", config%machine%name)
      call get_value( machine, "institution", config%machine%institution)
      call get_value( machine, "type", config%machine%machine_type)
      !-------------------------------------------------------------
      ! Implementation: code
      !-------------------------------------------------------------
      nullify(code)
      call get_value(implementation, "code", code)
      if (.not. associated(code)) then
         error stop "Missing required [implementation.code] table"
      end if
      call get_value( code, "annotation", config%code%annotation, default="")
      call get_value( code, "name", config%code%name)
      call get_value( code, "version", config%code%version)
      call get_value( code, "date", config%code%date)
      !------------------------------------------------------------
      ! Algorithm
      !
      ! The algorithm table is optional in the TOML representation. If it is
      ! absent, the XML writer still creates the required empty <algorithm/>
      ! element.
      !------------------------------------------------------------
      nullify(algorithm)
      call get_value(root, "algorithm", algorithm)
      if (associated(algorithm)) then
         call get_value(algorithm, "annotation", config%algorithm_annotation, default="")
         call read_parameters(algorithm,config%parameters)
      end if
      !-------------------------------------------------------------
      ! Markov sequence
      !-------------------------------------------------------------
      nullify(markov_sequence)
      call get_value( root, "markov_sequence", markov_sequence)
      if (.not. associated(markov_sequence)) then
         error stop "Missing required [markov_sequence] table"
      end if
      call get_value( markov_sequence, "annotation", config%markov_annotation, default="")
      call get_value( markov_sequence, "uri", config%markov_chain_uri)
      call get_value( markov_sequence, "series", config%series)
      call read_markov_steps( markov_sequence, config%markov_steps)
      !------------------------------------------------------------
      ! Additional information
      !------------------------------------------------------------
      nullify(additional_info)
      call get_value( root, "additional_info", additional_info)
      if (associated(additional_info)) then
         call get_value(additional_info, "annotation",config%additional_info_annotation,default="")
      end if
   end subroutine read_qcdml_toml

   subroutine read_archive_events(table, events)
      type(toml_table), intent(inout) :: table
      type(qcdml_archive_event), dimension(:), allocatable, intent(out) :: events
      type(toml_array), pointer :: event_array
      type(toml_table), pointer :: event_table
      type(toml_table), pointer :: participant_table
      integer :: i
      integer :: number_of_events
      nullify(event_array)
      call get_value( table, "archive_events", event_array)
      if (.not. associated(event_array)) then
         error stop "Missing required [[management.archive_events]] array"
      end if
      number_of_events = len(event_array)
      if (number_of_events == 0) then
         error stop "At least one [[management.archive_events]] entry is required"
      end if
      allocate(events(number_of_events))
      do i = 1, number_of_events
         nullify(event_table)
         call get_value(event_array, i, event_table)
         if (.not. associated(event_table)) then
            error stop "An entry in management.archive_events is not a table"
         end if
         call get_value(event_table,"annotation",events(i)%annotation,default="")
         call get_value(event_table,"revision",events(i)%revision,default="")
         call get_value(event_table,"revision_action",events(i)%revision_action)
         call get_value(event_table,"date",events(i)%date)
         nullify(participant_table)
         call get_value(event_table,"participant",participant_table)
         if (.not. associated(participant_table)) then
            error stop "An archive event is missing its participant table"
         end if
         call get_value(participant_table,"orcid",events(i)%participant%orcid,default="")
         call get_value(participant_table,"name",events(i)%participant%name,default="")
         call get_value(participant_table,"institution",events(i)%participant%institution, &
              default="")
      end do
   end subroutine read_archive_events

   subroutine read_parameters(table, parameters)
      type(toml_table), intent(inout) :: table
      type(qcdml_parameter), allocatable, intent(out) :: parameters(:)
      type(toml_array), pointer :: parameter_array
      type(toml_table), pointer :: parameter_table
      integer :: i
      integer :: number_of_parameters
      nullify(parameter_array)
      call get_value( table, "parameters", parameter_array)
      ! An algorithm without parameters is valid.
      if (.not. associated(parameter_array)) return
      number_of_parameters = len(parameter_array)
      ! Leave parameters unallocated for an empty TOML array. This causes the
      ! XML writer to emit <algorithm/> rather than <parameters/>.
      if (number_of_parameters == 0) return
      allocate(parameters(number_of_parameters))
      do i = 1, number_of_parameters
         nullify(parameter_table)
         call get_value(parameter_array,i,parameter_table)
         if (.not. associated(parameter_table)) then
            error stop "An entry in algorithm.parameters is not a table"
         end if
         call get_value(parameter_table,"annotation",parameters(i)%annotation, default="")
         call get_value(parameter_table,"name",parameters(i)%name)
         call get_value(parameter_table,"value",parameters(i)%val)
      end do
   end subroutine read_parameters

   subroutine read_markov_steps(table, steps)
      type(toml_table), intent(inout) :: table
      type(qcdml_markov_step), allocatable, intent(out) :: steps(:)
      type(toml_array), pointer :: step_array
      type(toml_table), pointer :: step_table
      integer :: i
      integer :: number_of_steps
      nullify(step_array)
      call get_value( table, "steps", step_array)
      if (.not. associated(step_array)) then
         error stop "Missing required [[markov_sequence.steps]] array"
      end if
      number_of_steps = len(step_array)
      if (number_of_steps == 0) then
         error stop "At least one [[markov_sequence.steps]] entry is required"
      end if
      allocate(steps(number_of_steps))
      do i = 1, number_of_steps
         nullify(step_table)
         call get_value(step_array,i,step_table)
         if (.not. associated(step_table)) then
            error stop "An entry in markov_sequence.steps is not a table"
         end if
         call get_value(step_table,"annotation",steps(i)%annotation,    default="")
         ! update is intentionally a character value. This preserves leading
         ! zeroes such as "000100".
         call get_value(step_table,"update",steps(i)%update)
         call read_records(step_table,steps(i)%records)
      end do
   end subroutine read_markov_steps

   subroutine read_records(table, records)
      type(toml_table), intent(inout) :: table
      type(qcdml_record), allocatable, intent(out) :: records(:)
      type(toml_array), pointer :: record_array
      type(toml_table), pointer :: record_table
      integer :: i
      integer :: number_of_records
      nullify(record_array)
      call get_value( table, "records", record_array)
      if (.not. associated(record_array)) then
         error stop "A Markov step is missing its records array"
      end if
      number_of_records = len(record_array)
      if (number_of_records == 0) then
         error stop "Each Markov step requires at least one record"
      end if
      allocate(records(number_of_records))
      do i = 1, number_of_records
         nullify(record_table)
         call get_value(record_array,i,record_table)
         if (.not. associated(record_table)) then
            error stop "An entry in markov_sequence.steps.records is not a table"
         end if
         call get_value(record_table,"field",records(i)%field)
         ! The checksum is intentionally character data. Valid QCDml files
         ! contain leading zeroes, UNKNOWN, and values above signed int32.
         call get_value(record_table,"crc_checksum",records(i)%crc_checksum)
         call get_value(record_table,"ave_plaquette",records(i)%ave_plaquette)
      end do
   end subroutine read_records

end module FortQCDML_toml
