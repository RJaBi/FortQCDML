module FortQCDML_write
  use FortQCDML_base, only: qcdml_config, qcdml_record, qcdml_parameter, real64, qcdml_namespace
  use foxy, only: xml_file, xml_tag, xml_escape
  implicit none(type, external)
  private

  public :: build_qcdml_xml

contains

   subroutine build_qcdml_xml(config, document)
      type(qcdml_config), intent(in)  :: config
      type(xml_file),     intent(out) :: document

      integer :: root
      integer :: management
      integer :: history
      integer :: event
      integer :: participant
      integer :: implementation
      integer :: machine
      integer :: code
      integer :: algorithm
      integer :: parameters
      integer :: parameter
      integer :: sequence
      integer :: step
      integer :: record

      integer :: i

      call check_config(config)

      !-----------------------------------------------------------------------
      ! gaugeConfiguration
      !-----------------------------------------------------------------------

      call document%add_tag( &
         xml_tag( &
            name="gaugeConfiguration", &
            attributes_stream='xmlns="'//qcdml_namespace//'"' &
         ), &
         id=root &
      )

      call add_text( &
         document, root, "dataLFN", config%data_lfn &
      )

      !-----------------------------------------------------------------------
      ! management
      !-----------------------------------------------------------------------

      call document%add_tag( &
         xml_tag(name="management"), &
         parent=root, &
         id=management &
      )

      call add_optional_text( &
         document, management, "annotation", &
         config%annotation &
      )

      call add_integer( &
         document, management, "revisions", &
         config%revision &
      )

      call document%add_tag( &
         xml_tag(name="archiveHistory"), &
         parent=management, &
         id=history &
      )

      call document%add_tag( &
         xml_tag(name="archiveEvent"), &
         parent=history, &
         id=event &
      )

      call add_integer( &
         document, event, "revision", config%revision &
      )

      call add_text( &
         document, event, "revisionAction", &
         config%revision_action &
      )

      call document%add_tag( &
         xml_tag(name="participant"), &
         parent=event, &
         id=participant &
      )

      if (allocated(config%participant_orcid)) then
         if (len_trim(config%participant_orcid) > 0) then
            call add_text( &
               document, participant, "orcid", &
               config%participant_orcid &
            )

            call add_optional_text( &
               document, participant, "name", &
               config%participant_name &
            )

            call add_optional_text( &
               document, participant, "institution", &
               config%participant_institution &
            )
         else
            call add_text( &
               document, participant, "name", &
               config%participant_name &
            )

            call add_text( &
               document, participant, "institution", &
               config%participant_institution &
            )
         end if
      else
         call add_text( &
            document, participant, "name", &
            config%participant_name &
         )

         call add_text( &
            document, participant, "institution", &
            config%participant_institution &
         )
      end if

      call add_text( &
         document, event, "date", config%date &
      )

      !-----------------------------------------------------------------------
      ! implementation
      !-----------------------------------------------------------------------

      call document%add_tag( &
         xml_tag(name="implementation"), &
         parent=root, &
         id=implementation &
      )

      call document%add_tag( &
         xml_tag(name="machine"), &
         parent=implementation, &
         id=machine &
      )

      call add_optional_text( &
         document, machine, "annotation", &
         config%machine_annotation &
      )

      call add_text( &
         document, machine, "name", &
         config%machine_name &
      )

      call add_text( &
         document, machine, "institution", &
         config%machine_institution &
      )

      call add_text( &
         document, machine, "machineType", &
         config%machine_type &
      )

      call document%add_tag( &
         xml_tag(name="code"), &
         parent=implementation, &
         id=code &
      )

      call add_optional_text( &
         document, code, "annotation", &
         config%code_annotation &
      )

      call add_text( &
         document, code, "name", config%code_name &
      )

      call add_text( &
         document, code, "version", config%code_version &
      )

      call add_text( &
         document, code, "date", config%code_date &
      )

      !-----------------------------------------------------------------------
      ! algorithm
      !-----------------------------------------------------------------------

      call document%add_tag( &
         xml_tag(name="algorithm"), &
         parent=root, &
         id=algorithm &
      )

      call add_optional_text( &
         document, algorithm, "annotation", &
         config%algorithm_annotation &
      )

      if (allocated(config%parameters)) then
         call document%add_tag( &
            xml_tag(name="parameters"), &
            parent=algorithm, &
            id=parameters &
         )

         do i = 1, size(config%parameters)
            call document%add_tag( &
               xml_tag(name="parameter"), &
               parent=parameters, &
               id=parameter &
            )

            call add_optional_text( &
               document, parameter, "annotation", &
               config%parameters(i)%annotation &
            )

            call add_text( &
               document, parameter, "name", &
               config%parameters(i)%name &
            )

            call add_text( &
               document, parameter, "value", &
               config%parameters(i)%val &
            )
         end do
      end if

      !-----------------------------------------------------------------------
      ! precision
      !-----------------------------------------------------------------------

      call add_text( &
         document, root, "precision", config%precision &
      )

      !-----------------------------------------------------------------------
      ! markovSequence
      !-----------------------------------------------------------------------

      call document%add_tag( &
         xml_tag(name="markovSequence"), &
         parent=root, &
         id=sequence &
      )

      call add_optional_text( &
         document, sequence, "annotation", &
         config%markov_annotation &
      )

      call add_text( &
         document, sequence, "markovChainURI", &
         config%markov_chain_uri &
      )

      call add_text( &
         document, sequence, "series", config%series &
      )

      call document%add_tag( &
         xml_tag(name="markovStep"), &
         parent=sequence, &
         id=step &
      )

      call add_optional_text( &
         document, step, "annotation", &
         config%step_annotation &
      )

      call add_integer( &
         document, step, "update", config%update &
      )

      do i = 1, size(config%records)
         call document%add_tag( &
            xml_tag(name="record"), &
            parent=step, &
            id=record &
         )

         call add_text( &
            document, record, "field", &
            config%records(i)%field &
         )

         call add_text( &
            document, record, "crcCheckSum", &
            config%records(i)%crc_checksum &
         )

         call add_real( &
            document, record, "avePlaquette", &
            config%records(i)%ave_plaquette &
         )
      end do

      !-----------------------------------------------------------------------
      ! additionalInfo
      !-----------------------------------------------------------------------

      if (allocated(config%additional_info)) then
         if (len_trim(config%additional_info) > 0) then
            call add_additional_info( &
               document, root, config%additional_info &
            )
         end if
      end if
   end subroutine build_qcdml_xml


   !===========================================================================
   ! XML construction helpers
   !===========================================================================

   subroutine add_text(document, parent, name, value)
      type(xml_file), intent(inout) :: document
      integer, intent(in)    :: parent
      character(*),   intent(in)    :: name
      character(*),   intent(in)    :: value

      call document%add_tag( &
         xml_tag( &
            name=name, &
            content=xml_escape(value) &
         ), &
         parent=parent &
      )
   end subroutine add_text


   subroutine add_optional_text(document, parent, name, value)
      type(xml_file),            intent(inout) :: document
      integer,            intent(in)    :: parent
      character(*),              intent(in)    :: name
      character(:), allocatable, intent(in)    :: value

      if (.not. allocated(value)) return
      if (len_trim(value) == 0) return

      call add_text(document, parent, name, value)
   end subroutine add_optional_text


   subroutine add_integer(document, parent, name, value)
      type(xml_file), intent(inout) :: document
      integer, intent(in)    :: parent
      character(*),   intent(in)    :: name
      integer, intent(in)    :: value

      integer :: id

      call document%add_tag( &
         xml_tag(name=name), &
         parent=parent, &
         id=id &
      )

      call document%set_content(id=id, value=value)
   end subroutine add_integer


   subroutine add_real(document, parent, name, value)
      type(xml_file), intent(inout) :: document
      integer, intent(in)    :: parent
      character(*),   intent(in)    :: name
      real(real64),   intent(in)    :: value

      integer :: id

      call document%add_tag( &
         xml_tag(name=name), &
         parent=parent, &
         id=id &
      )

      call document%set_content(id=id, value=value)
   end subroutine add_real


   subroutine add_additional_info(document, parent, annotation)
      type(xml_file), intent(inout) :: document
      integer, intent(in)    :: parent
      character(*),   intent(in)    :: annotation

      integer :: additional_info

      call document%add_tag( &
         xml_tag(name="additionalInfo"), &
         parent=parent, &
         id=additional_info &
      )

      call add_text( &
         document, additional_info, "annotation", annotation &
      )
   end subroutine add_additional_info


   !===========================================================================
   ! Basic validation
   !
   ! FoXy constructs well-formed XML but does not enforce this XSD. These
   ! checks catch missing values before the document is constructed.
   !===========================================================================

   subroutine check_config(config)
      type(qcdml_config), intent(in) :: config

      integer :: i

      call require_text(config%data_lfn, "data_lfn")
      call require_text(config%revision_action, "revision_action")
      call require_text(config%date, "management date")

      if (config%revision < 0) then
         error stop "revision must be non-negative"
      end if

      select case (config%revision_action)
      case ("generate", "add", "replace", "remove")
         continue
      case default
         error stop &
            "revision_action must be generate, add, replace, or remove"
      end select

      if (allocated(config%participant_orcid)) then
         if (len_trim(config%participant_orcid) == 0) then
            call require_text( &
               config%participant_name, "participant name" &
            )
            call require_text( &
               config%participant_institution, &
               "participant institution" &
            )
         end if
      else
         call require_text( &
            config%participant_name, "participant name" &
         )
         call require_text( &
            config%participant_institution, &
            "participant institution" &
         )
      end if

      call require_text(config%machine_name, "machine name")
      call require_text( &
         config%machine_institution, "machine institution" &
      )
      call require_text(config%machine_type, "machine type")

      call require_text(config%code_name, "code name")
      call require_text(config%code_version, "code version")
      call require_text(config%code_date, "code date")

      call require_text(config%precision, "precision")

      select case (config%precision)
      case ("single", "double", "mixed")
         continue
      case default
         error stop "precision must be single, double, or mixed"
      end select

      call require_text( &
         config%markov_chain_uri, "markov_chain_uri" &
      )

      call require_text(config%series, "series")

      if (config%update < 0) then
         error stop "update must be non-negative"
      end if

      if (.not. allocated(config%records)) then
         error stop "At least one QCDml record is required"
      end if

      if (size(config%records) == 0) then
         error stop "At least one QCDml record is required"
      end if

      do i = 1, size(config%records)
         call require_text( &
            config%records(i)%field, "record field" &
         )

         call require_text( &
            config%records(i)%crc_checksum, &
            "record CRC checksum" &
         )
      end do

      if (allocated(config%parameters)) then
         do i = 1, size(config%parameters)
            call require_text( &
               config%parameters(i)%name, "parameter name" &
            )

            call require_text( &
               config%parameters(i)%val, "parameter value" &
            )
         end do
      end if
   end subroutine check_config


   subroutine require_text(value, description)
      character(:), allocatable, intent(in) :: value
      character(*),              intent(in) :: description

      if (.not. allocated(value)) then
         error stop "Missing required value: "//description
      end if

      if (len_trim(value) == 0) then
         error stop "Empty required value: "//description
      end if
    end subroutine require_text

end module FortQCDML_write
