program make_config_xml
   use FortQCDML, only: read_qcdmL_toml, real64, build_qcdml_xml, qcdml_config
   use foxy, only : xml_file
   implicit none(type, external)

   type(qcdml_config) :: config
   type(xml_file)     :: document

   call read_qcdml_toml("t1.toml", config)

   allocate(config%records(1))

   config%records(1)%field = "su3gauge"
   config%records(1)%crc_checksum = "1234567890"
   config%records(1)%ave_plaquette = 0.593421_real64

   call build_qcdml_xml(config, document)

   call document%save( &
      filename="qcdml-config.xml", &
      is_declared=.true. &
   )
end program make_config_xml
