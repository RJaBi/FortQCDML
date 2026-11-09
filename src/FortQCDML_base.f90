module FortQCDML_base
   use, intrinsic :: iso_fortran_env, only : real64

   implicit none(type, external)
   private

   character(len=*), parameter :: qcdml_namespace = &
      "http://www.lqcd.org/ildg/QCDml/config2.0"

   type :: qcdml_participant
      character(:), allocatable :: orcid
      character(:), allocatable :: name
      character(:), allocatable :: institution
   end type qcdml_participant

   type :: qcdml_archive_event
      character(:), allocatable :: annotation
      character(:), allocatable :: revision
      character(:), allocatable :: revision_action
      type(qcdml_participant)    :: participant
      character(:), allocatable :: date
   end type qcdml_archive_event

   type :: qcdml_machine
      character(:), allocatable :: annotation
      character(:), allocatable :: name
      character(:), allocatable :: institution
      character(:), allocatable :: machine_type
   end type qcdml_machine

   type :: qcdml_code
      character(:), allocatable :: annotation
      character(:), allocatable :: name
      character(:), allocatable :: version
      character(:), allocatable :: date
   end type qcdml_code

   type :: qcdml_parameter
      character(:), allocatable :: annotation
      character(:), allocatable :: name
      character(:), allocatable :: val
   end type qcdml_parameter

   type :: qcdml_record
      character(:), allocatable :: field
      character(:), allocatable :: crc_checksum
      real(real64)              :: ave_plaquette = 0.0_real64
   end type qcdml_record

   type :: qcdml_markov_step
      character(:), allocatable :: annotation
      character(:), allocatable :: update
      type(qcdml_record), allocatable :: records(:)
   end type qcdml_markov_step

   type :: qcdml_config
      character(:), allocatable :: data_lfn

      character(:), allocatable :: management_annotation
      character(:), allocatable :: revisions
      character(:), allocatable :: reference
      type(qcdml_archive_event), allocatable :: archive_events(:)

      character(:), allocatable :: implementation_annotation
      type(qcdml_machine)       :: machine
      type(qcdml_code)          :: code

      character(:), allocatable :: algorithm_annotation
      type(qcdml_parameter), allocatable :: parameters(:)

      character(:), allocatable :: precision

      character(:), allocatable :: markov_annotation
      character(:), allocatable :: markov_chain_uri
      character(:), allocatable :: series
      type(qcdml_markov_step), allocatable :: markov_steps(:)

      character(:), allocatable :: additional_info_annotation
   end type qcdml_config

   public :: qcdml_config
   public :: qcdml_archive_event
   public :: qcdml_parameter
   public :: qcdml_record
   public :: qcdml_markov_step
   public :: qcdml_namespace
   public :: real64

end module FortQCDML_base
