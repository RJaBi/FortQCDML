module FortQCDML_write
  use FortQCDML_base, only : qcdml_config, real64, &
       qcdml_archive_event, qcdml_parameter, qcdml_markov_step, qcdml_record,  qcdml_namespace
  use foxy, only : xml_file, xml_tag, xml_escape
  implicit none(type, external)
  private
  public :: build_qcdml_xml

contains

  subroutine build_qcdml_xml(config, document)
    type(qcdml_config), intent(in)  :: config
    type(xml_file),   intent(out) :: document

    integer :: root
    integer :: management
    integer :: history
    integer :: implementation
    integer :: machine
    integer :: code
    integer :: algorithm
    integer :: parameters
    integer :: sequence
    integer :: additional_info
    integer :: i
    call check_config(config)
    call document%add_tag(xml_tag(name="gaugeConfiguration", &
         attributes_stream=  'xmlns="' // qcdml_namespace // '"'),id=root)
    call add_text(document, root, "dataLFN", config%data_lfn)
    ! management
    call document%add_tag(xml_tag(name="management"),parent=root,id=management)
    call add_optional_text(document, management, "annotation",config%management_annotation)
    call add_optional_text(document, management, "revisions",config%revisions)
    call add_optional_text(document, management, "reference",config%reference)
    call document%add_tag(xml_tag(name="archiveHistory"),parent=management,id=history)
    do i = 1, size(config%archive_events)
       call add_archive_event( document, history, config%archive_events(i))
    end do
    ! implementation
    call document%add_tag(xml_tag(name="implementation"),parent=root,id=implementation)
    call add_optional_text(document, implementation, "annotation",config%implementation_annotation)
    call document%add_tag(xml_tag(name="machine"),parent=implementation,id=machine)
    call add_optional_text(document, machine, "annotation",config%machine%annotation)
    call add_text(document, machine, "name", config%machine%name)
    call add_text(document, machine, "institution",config%machine%institution)
    call add_text(document, machine, "machineType",config%machine%machine_type)
    call document%add_tag(xml_tag(name="code"),parent=implementation,id=code)
    call add_optional_text(document, code, "annotation",config%code%annotation)
    call add_text(document, code, "name", config%code%name)
    call add_text(document, code, "version", config%code%version)
    call add_text(document, code, "date", config%code%date)
    ! algorithm
    call document%add_tag(xml_tag(name="algorithm"),parent=root,id=algorithm)
    call add_optional_text(document, algorithm, "annotation",config%algorithm_annotation)
    if (allocated(config%parameters)) then
       if (size(config%parameters) > 0) then
          call document%add_tag(  xml_tag(name="parameters"),  parent=algorithm,  id=parameters)
          do i = 1, size(config%parameters)
             call add_parameter( document, parameters, config%parameters(i))
          end do
       end if
    end if
    call add_text(document, root, "precision", config%precision)
    ! markovSequence
    call document%add_tag(xml_tag(name="markovSequence"),parent=root,id=sequence)
    call add_optional_text(document, sequence, "annotation",config%markov_annotation)
    call add_text(document, sequence, "markovChainURI",config%markov_chain_uri)
    call add_text(document, sequence, "series", config%series)
    do i = 1, size(config%markov_steps)
       call add_markov_step( document, sequence, config%markov_steps(i))
    end do
    ! additionalInfo
    if (has_text(config%additional_info_annotation)) then
       call document%add_tag( xml_tag(name="additionalInfo"), parent=root, id=additional_info)
       call add_text( document, additional_info, "annotation", config%additional_info_annotation)
    end if
  end subroutine build_qcdml_xml

  subroutine add_archive_event(document, parent, event)
    type(xml_file), intent(inout) :: document
    integer,  intent(in):: parent
    type(qcdml_archive_event), intent(in) :: event
    integer :: event_id
    integer :: participant_id
    call document%add_tag(xml_tag(name="archiveEvent"),parent=parent,id=event_id)
    call add_optional_text(document, event_id, "annotation", event%annotation)
    call add_optional_text(document, event_id, "revision", event%revision)
    call add_text(document, event_id, "revisionAction",event%revision_action)
    call document%add_tag(xml_tag(name="participant"),parent=event_id,id=participant_id)
    if (has_text(event%participant%orcid)) then
       call add_text( document, participant_id, "orcid", event%participant%orcid)
       call add_optional_text( document, participant_id, "name", event%participant%name)
       call add_optional_text( document, participant_id, "institution", &
            event%participant%institution)
    else
       call add_text( document, participant_id, "name", event%participant%name)
       call add_text( document, participant_id, "institution", event%participant%institution)
    end if
    call add_text(document, event_id, "date", event%date)
  end subroutine add_archive_event

  subroutine add_parameter(document, parent, parameter)
    type(xml_file), intent(inout) :: document
    integer,  intent(in):: parent
    type(qcdml_parameter), intent(in) :: parameter
    integer :: parameter_id
    call document%add_tag(xml_tag(name="parameter"),parent=parent,id=parameter_id)
    call add_optional_text(document, parameter_id, "annotation",parameter%annotation)
    call add_text(document, parameter_id, "name", parameter%name)
    call add_text(document, parameter_id, "value", parameter%val)
  end subroutine add_parameter

  subroutine add_markov_step(document, parent, step)
    type(xml_file), intent(inout) :: document
    integer,intent(in):: parent
    type(qcdml_markov_step), intent(in) :: step
    integer :: step_id
    integer :: i
    call document%add_tag(xml_tag(name="markovStep"),parent=parent,id=step_id)
    call add_optional_text(document, step_id, "annotation", step%annotation)
    call add_text(document, step_id, "update", step%update)
    do i = 1, size(step%records)
       call add_record(document, step_id, step%records(i))
    end do
  end subroutine add_markov_step

  subroutine add_record(document, parent, record)
    type(xml_file),intent(inout) :: document
    integer, intent(in):: parent
    type(qcdml_record), intent(in) :: record
    integer :: record_id
    call document%add_tag(xml_tag(name="record"),parent=parent,id=record_id)
    call add_text(document, record_id, "field", record%field)
    call add_text(document, record_id, "crcCheckSum",record%crc_checksum)
    call add_real(document, record_id, "avePlaquette",record%ave_plaquette)
  end subroutine add_record

  subroutine add_text(document, parent, name, val)
    type(xml_file), intent(inout) :: document
    integer,intent(in):: parent
    character(*), intent(in):: name
    character(*), intent(in):: val
    call document%add_tag(xml_tag( name=name, content=xml_escape(val)),parent=parent)
  end subroutine add_text

  subroutine add_optional_text(document, parent, name, val)
    type(xml_file),intent(inout) :: document
    integer, intent(in):: parent
    character(*),  intent(in):: name
    character(:), allocatable, intent(in):: val
    if (has_text(val)) then
       call add_text(document, parent, name, val)
    end if
  end subroutine add_optional_text

  subroutine add_real(document, parent, name, val)
    type(xml_file), intent(inout) :: document
    integer,intent(in):: parent
    character(*), intent(in):: name
    real(real64), intent(in):: val
    integer :: id
    call document%add_tag(xml_tag(name=name),parent=parent,id=id)
    call document%set_content(id=id, value=val)
  end subroutine add_real

  subroutine check_config(config)
    type(qcdml_config), intent(in) :: config
    integer :: i
    integer :: j
    call require_text(config%data_lfn, "dataLFN")
    if (.not. allocated(config%archive_events)) then
       error stop "archiveHistory requires an archiveEvent"
    end if
    if (size(config%archive_events) == 0) then
       error stop "archiveHistory requires an archiveEvent"
    end if
    do i = 1, size(config%archive_events)
       call check_archive_event(config%archive_events(i))
    end do
    call require_text(config%machine%name, "machine/name")
    call require_text(config%machine%institution, "machine/institution")
    call require_text(config%machine%machine_type, "machine/machineType")
    call require_text(config%code%name, "code/name")
    call require_text(config%code%version, "code/version")
    call require_text(config%code%date, "code/date")
    call require_text(config%precision, "precision")
    select case (config%precision)
    case ("single", "double", "mixed")
       continue
    case default
       error stop "precision must be single, double, or mixed"
    end select
    if (allocated(config%parameters)) then
       do i = 1, size(config%parameters)
          call require_text(  config%parameters(i)%name, "parameter/name")
          call require_text(  config%parameters(i)%val, "parameter/value")
       end do
    end if
    call require_text(config%markov_chain_uri, "markovSequence/markovChainURI")
    call require_text(config%series, "markovSequence/series")
    if (.not. allocated(config%markov_steps)) then
       error stop "markovSequence requires a markovStep"
    end if
    if (size(config%markov_steps) == 0) then
       error stop "markovSequence requires a markovStep"
    end if
    do i = 1, size(config%markov_steps)
       call require_text( config%markov_steps(i)%update, "markovStep/update")
       if (.not. allocated(config%markov_steps(i)%records)) then
          error stop "Each markovStep requires a record"
       end if
       if (size(config%markov_steps(i)%records) == 0) then
          error stop "Each markovStep requires a record"
       end if
       do j = 1, size(config%markov_steps(i)%records)
          call require_text(  config%markov_steps(i)%records(j)%field,  "record/field")
          call require_text(  config%markov_steps(i)%records(j)%crc_checksum,"record/crcCheckSum")
       end do
    end do
  end subroutine check_config

  subroutine check_archive_event(event)
    type(qcdml_archive_event), intent(in) :: event
    call require_text(event%revision_action,"archiveEvent/revisionAction")
    select case (event%revision_action)
    case ("generate", "add", "replace", "remove")
       continue
    case default
       error stop "revisionAction must be generate, add, replace, or remove"
    end select
    call require_text(event%date, "archiveEvent/date")
    if (.not. has_text(event%participant%orcid)) then
       call require_text( event%participant%name, "participant/name")
       call require_text( event%participant%institution, "participant/institution")
    end if
  end subroutine check_archive_event

  subroutine require_text(val, description)
    character(:), allocatable, intent(in) :: val
    character(*),  intent(in) :: description
    if (.not. has_text(val)) then
       error stop "Missing required value: " // description
    end if
  end subroutine require_text

  pure logical function has_text(val)
    character(:), allocatable, intent(in) :: val
    if (allocated(val)) then
       has_text = len_trim(val) > 0
    else
       has_text = .false.
    end if
  end function has_text

end module FortQCDML_write
