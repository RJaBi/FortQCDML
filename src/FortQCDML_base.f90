module FortQCDML_base
  use, intrinsic iso_fortran_env, only: real64
  implicit none(type, external)
  private

  character(*), parameter :: qcdml_namespace = &
       "http://www.lqcd.org/ildg/QCDml/config2.0"

  type :: qcdml_record
     character(:), allocatable :: field
     character(:), allocatable :: crc_checksum
     real(real64)              :: ave_plaquette
  end type qcdml_record

  type :: qcdml_parameter
     character(:), allocatable :: annotation
     character(:), allocatable :: name
     character(:), allocatable :: val
  end type qcdml_parameter

  type :: qcdml_config
     character(:), allocatable :: data_lfn
     character(:), allocatable :: annotation
     integer                   :: revision = 0
     character(:), allocatable :: revision_action
     character(:), allocatable :: participant_name
     character(:), allocatable :: participant_institution
     character(:), allocatable :: participant_orcid
     character(:), allocatable :: date
     character(:), allocatable :: machine_annotation
     character(:), allocatable :: machine_name
     character(:), allocatable :: machine_institution
     character(:), allocatable :: machine_type
     character(:), allocatable :: code_annotation
     character(:), allocatable :: code_name
     character(:), allocatable :: code_version
     character(:), allocatable :: code_date
     character(:), allocatable :: algorithm
     character(:), allocatable :: algorithm_annotation
     type(qcdml_parameter), allocatable :: parameters(:)
     character(:), allocatable :: precision
     character(:), allocatable :: markov_annotation
     character(:), allocatable :: markov_chain_uri
     character(:), allocatable :: series
     character(:), allocatable :: step_annotation
     integer                   :: update
     type(qcdml_record), allocatable :: records(:)
     character(:), allocatable :: additional_info
  end type qcdml_config

  public :: qcdml_config
  public :: qcdml_record
  public :: qcdml_parameter
  public :: real64
  public :: qcdml_namespace

end module FortQCDML_base

