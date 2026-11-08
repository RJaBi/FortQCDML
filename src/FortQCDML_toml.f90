module FortQCDML_toml
  use FortQCDML_base, only: qcdml_config, real64
  use tomlf, only: toml_table, toml_load, get_value, toml_error
  implicit none(type, external)
  private

  public :: read_qcdml_toml

contains

  subroutine read_qcdml_toml(filename, config)
    character(*),       intent(in)  :: filename
    type(qcdml_config), intent(out) :: config
    type(toml_table), allocatable :: table
    type(toml_table), pointer     :: management
    type(toml_table), pointer     :: participant
    type(toml_table), pointer     :: machine
    type(toml_table), pointer     :: code
    type(toml_table), pointer     :: algorithm
    type(toml_table), pointer     :: markov
    type(toml_error), allocatable :: error
    call toml_load(table, filename, error=error)
    if (allocated(error)) then
       error stop error%message
    end if
    call get_value(table, "data_lfn", config%data_lfn)
    call get_value(table, "precision", config%precision)
    call get_value(table, "management", management)
    call get_value(management, "annotation", config%annotation, default="")
    call get_value(management, "revision", config%revision, default=0)
    call get_value(management, "revision_action", config%revision_action, default="generate")
    call get_value(management, "date", config%date)
    call get_value(management, "participant", participant)
    call get_value(participant, "name", config%participant_name)
    call get_value(participant, "institution", config%participant_institution)
    call get_value(participant, "orcid", config%participant_orcid)
    call get_value(table, "machine", machine)
    call get_value(machine, "annotation", config%machine_annotation, default="")
    call get_value(machine, "name", config%machine_name)
    call get_value(machine, "institution", config%machine_institution)
    call get_value(machine, "type", config%machine_type)
    call get_value(table, "code", code)
    call get_value(code, "annotation", config%code_annotation, default="")
    call get_value(code, "name", config%code_name)
    call get_value(code, "version", config%code_version)
    call get_value(code, "date", config%code_date)
    call get_value(table, "algorithm", algorithm)
    call get_value(algorithm, "name", config%algorithm, default="")
    call get_value(algorithm, "annotation", config%algorithm_annotation, default="")
    allocate(config%parameters(2))
    config%parameters(1)%name = "beta"
    call get_value(algorithm, "beta", config%parameters(1)%val)
    config%parameters(2)%name = "trajectory_length"
    call get_value(algorithm, "trajectory_length",config%parameters(2)%val)
    call get_value(table, "markov_sequence", markov)
    call get_value(markov, "annotation", config%markov_annotation, default="")
    call get_value(markov, "uri", config%markov_chain_uri)
    call get_value(markov, "step_annotation", config%step_annotation, default="")
    call get_value(markov, "series", config%series)
    call get_value(markov, "update", config%update)
    call get_value( table, "additional_info", config%additional_info, default="")
  end subroutine read_qcdml_toml


end module FortQCDML_toml
